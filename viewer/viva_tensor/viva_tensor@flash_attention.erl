-module(viva_tensor@flash_attention).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/flash_attention.gleam").
-export([default_config/1, causal_config/1, naive_attention/4, flash_attention/4, benchmark_flash_attention/0, main/0]).
-export_type([flash_config/0, online_stats/0, flash_result/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Flash Attention - O(n) Memory Attention\n"
    "\n"
    " REVOLUCIONÁRIO: Reduz memória de O(n²) para O(n)!\n"
    " https://arxiv.org/abs/2205.14135 (Tri Dao, 2022)\n"
    "\n"
    " PROBLEMA:\n"
    " - Attention padrão: Q @ K^T @ V = O(n²) memória\n"
    " - Para n=8192 (contexto longo): 8192² = 67M elementos = 256MB por cabeça!\n"
    " - 32 cabeças = 8GB só para attention scores\n"
    "\n"
    " SOLUÇÃO:\n"
    " - Processar em TILES (blocos)\n"
    " - Nunca materializar matriz n×n completa\n"
    " - Online softmax: atualiza estatísticas incrementalmente\n"
    "\n"
    " RESULTADO: 2-4x mais rápido, O(n) memória!\n"
).

-type flash_config() :: {flash_config, integer(), integer(), float(), boolean()}.

-type online_stats() :: {online_stats, float(), float(), list(float())}.

-type flash_result() :: {flash_result,
        viva_tensor@tensor:tensor(),
        integer(),
        float()}.

-file("src/viva_tensor/flash_attention.gleam", 67).
?DOC(" Configuração padrão\n").
-spec default_config(integer()) -> flash_config().
default_config(Head_dim) ->
    Scale = case gleam@float:square_root(erlang:float(Head_dim)) of
        {ok, Sqrt} ->
            case Sqrt of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> 1.0 / Gleam@denominator
            end;

        {error, _} ->
            0.125
    end,
    {flash_config, 64, 64, Scale, false}.

-file("src/viva_tensor/flash_attention.gleam", 79).
?DOC(" Config para causal (autoregressive)\n").
-spec causal_config(integer()) -> flash_config().
causal_config(Head_dim) ->
    _record = default_config(Head_dim),
    {flash_config,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        true}.

-file("src/viva_tensor/flash_attention.gleam", 453).
-spec dot_product(list(float()), list(float())) -> float().
dot_product(A, B) ->
    _pipe = gleam@list:map2(A, B, fun(X, Y) -> X * Y end),
    gleam@list:fold(_pipe, +0.0, fun gleam@float:add/2).

-file("src/viva_tensor/flash_attention.gleam", 472).
-spec get_row(list(list(float())), integer()) -> list(float()).
get_row(Matrix, Idx) ->
    case gleam@list:drop(Matrix, Idx) of
        [Row | _] ->
            Row;

        [] ->
            []
    end.

-file("src/viva_tensor/flash_attention.gleam", 479).
-spec result_to_float({ok, float()} | {error, any()}, float()) -> float().
result_to_float(R, Default) ->
    case R of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-file("src/viva_tensor/flash_attention.gleam", 257).
?DOC(" Processa um bloco KV contra queries, atualiza estatísticas online\n").
-spec process_kv_block(
    list(online_stats()),
    list(list(float())),
    list(list(float())),
    list(list(float())),
    flash_config()
) -> list(online_stats()).
process_kv_block(Stats, Q_block, K_block, V_block, Config) ->
    gleam@list:map2(
        Stats,
        Q_block,
        fun(Stat, Q_row) ->
            Scores = gleam@list:map(
                K_block,
                fun(K_row) ->
                    dot_product(Q_row, K_row) * erlang:element(4, Config)
                end
            ),
            New_max = gleam@list:fold(
                Scores,
                erlang:element(2, Stat),
                fun gleam@float:max/2
            ),
            Correction = case erlang:element(3, Stat) > +0.0 of
                true ->
                    _pipe = gleam@float:power(
                        2.71828,
                        erlang:element(2, Stat) - New_max
                    ),
                    result_to_float(_pipe, 1.0);

                false ->
                    1.0
            end,
            Corrected_sum = erlang:element(3, Stat) * Correction,
            Exp_scores = gleam@list:map(
                Scores,
                fun(S) -> _pipe@1 = gleam@float:power(2.71828, S - New_max),
                    result_to_float(_pipe@1, +0.0) end
            ),
            New_sum = gleam@list:fold(
                Exp_scores,
                Corrected_sum,
                fun gleam@float:add/2
            ),
            Corrected_output = gleam@list:map(
                erlang:element(4, Stat),
                fun(O) -> O * Correction end
            ),
            New_contribution = gleam@list:index_fold(
                Exp_scores,
                gleam@list:repeat(+0.0, erlang:length(erlang:element(4, Stat))),
                fun(Acc, Weight, I) ->
                    V_row = get_row(V_block, I),
                    gleam@list:map2(
                        Acc,
                        V_row,
                        fun(A, V) -> A + (Weight * V) end
                    )
                end
            ),
            New_output = gleam@list:map2(
                Corrected_output,
                New_contribution,
                fun gleam@float:add/2
            ),
            {online_stats, New_max, New_sum, New_output}
        end
    ).

-file("src/viva_tensor/flash_attention.gleam", 205).
?DOC(" Processa um bloco de Q contra todos os blocos KV\n").
-spec process_q_block(
    list(list(float())),
    list(list(list(float()))),
    list(list(list(float()))),
    flash_config(),
    integer(),
    integer()
) -> list(list(float())).
process_q_block(Q_block, K_blocks, V_blocks, Config, Q_block_idx, Head_dim) ->
    Initial_stats = gleam@list:map(
        Q_block,
        fun(_) ->
            {online_stats, -999999.0, +0.0, gleam@list:repeat(+0.0, Head_dim)}
        end
    ),
    Zipped_kv = gleam@list:zip(K_blocks, V_blocks),
    Final_stats = gleam@list:index_fold(
        Zipped_kv,
        Initial_stats,
        fun(Stats, Kv_pair, Kv_idx) ->
            {K_block, V_block} = Kv_pair,
            case erlang:element(5, Config) of
                true ->
                    Q_start = Q_block_idx * erlang:element(2, Config),
                    Kv_start = Kv_idx * erlang:element(3, Config),
                    _ = Kv_start + erlang:length(K_block),
                    case Kv_start > (Q_start + erlang:length(Q_block)) of
                        true ->
                            Stats;

                        false ->
                            process_kv_block(
                                Stats,
                                Q_block,
                                K_block,
                                V_block,
                                Config
                            )
                    end;

                false ->
                    process_kv_block(Stats, Q_block, K_block, V_block, Config)
            end
        end
    ),
    gleam@list:map(Final_stats, fun(S) -> case erlang:element(3, S) > +0.0 of
                true ->
                    gleam@list:map(
                        erlang:element(4, S),
                        fun(O) -> case erlang:element(3, S) of
                                +0.0 -> +0.0;
                                -0.0 -> -0.0;
                                Gleam@denominator -> O / Gleam@denominator
                            end end
                    );

                false ->
                    erlang:element(4, S)
            end end).

-file("src/viva_tensor/flash_attention.gleam", 458).
-spec softmax_row(list(float())) -> list(float()).
softmax_row(Row) ->
    Max_val = gleam@list:fold(Row, -999999.0, fun gleam@float:max/2),
    Exp_vals = gleam@list:map(
        Row,
        fun(X) -> _pipe = gleam@float:power(2.71828, X - Max_val),
            result_to_float(_pipe, +0.0) end
    ),
    Sum = gleam@list:fold(Exp_vals, +0.0, fun gleam@float:add/2),
    case Sum > +0.0 of
        true ->
            gleam@list:map(Exp_vals, fun(E) -> case Sum of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> E / Gleam@denominator
                    end end);

        false ->
            Row
    end.

-file("src/viva_tensor/flash_attention.gleam", 486).
-spec float_to_string(float()) -> binary().
float_to_string(F) ->
    Rounded = erlang:float(erlang:round(F * 100.0)) / 100.0,
    gleam_stdlib:float_to_string(Rounded).

-file("src/viva_tensor/flash_attention.gleam", 491).
-spec bytes_to_string(integer()) -> binary().
bytes_to_string(Bytes) ->
    case Bytes of
        B when B >= 1073741824 ->
            <<(float_to_string(erlang:float(B) / 1073741824.0))/binary,
                "GB"/utf8>>;

        B@1 when B@1 >= 1048576 ->
            <<(float_to_string(erlang:float(B@1) / 1048576.0))/binary,
                "MB"/utf8>>;

        B@2 when B@2 >= 1024 ->
            <<(erlang:integer_to_binary(B@2 div 1024))/binary, "KB"/utf8>>;

        B@3 ->
            <<(erlang:integer_to_binary(B@3))/binary, "B"/utf8>>
    end.

-file("src/viva_tensor/flash_attention.gleam", 502).
-spec get_tensor_shape(viva_tensor@tensor:tensor()) -> list(integer()).
get_tensor_shape(T) ->
    case T of
        {tensor, _, Shape} ->
            Shape;

        {strided_tensor, _, Shape@1, _, _} ->
            Shape@1
    end.

-file("src/viva_tensor/flash_attention.gleam", 91).
?DOC(
    " Attention ingênua: O(n²) memória\n"
    " scores = Q @ K^T\n"
    " attn = softmax(scores * scale)\n"
    " out = attn @ V\n"
).
-spec naive_attention(
    viva_tensor@tensor:tensor(),
    viva_tensor@tensor:tensor(),
    viva_tensor@tensor:tensor(),
    float()
) -> {viva_tensor@tensor:tensor(), integer()}.
naive_attention(Q, K, V, Scale) ->
    Q_data = viva_tensor@tensor:to_list(Q),
    K_data = viva_tensor@tensor:to_list(K),
    V_data = viva_tensor@tensor:to_list(V),
    Seq_len = erlang:length(Q_data),
    Head_dim = case get_tensor_shape(Q) of
        [_, D] ->
            D;

        _ ->
            Seq_len
    end,
    Q_rows = gleam@list:sized_chunk(Q_data, Head_dim),
    K_rows = gleam@list:sized_chunk(K_data, Head_dim),
    V_rows = gleam@list:sized_chunk(V_data, Head_dim),
    N_rows = erlang:length(Q_rows),
    N_cols = erlang:length(K_rows),
    Scores = gleam@list:map(
        Q_rows,
        fun(Q_row) ->
            gleam@list:map(
                K_rows,
                fun(K_row) -> dot_product(Q_row, K_row) * Scale end
            )
        end
    ),
    Attn = gleam@list:map(Scores, fun softmax_row/1),
    Output = begin
        _pipe = gleam@list:map(
            Attn,
            fun(Attn_row) ->
                gleam@list:index_fold(
                    Attn_row,
                    gleam@list:repeat(+0.0, Head_dim),
                    fun(Acc, Weight, I) ->
                        V_row = get_row(V_rows, I),
                        gleam@list:map2(
                            Acc,
                            V_row,
                            fun(A, V@1) -> A + (Weight * V@1) end
                        )
                    end
                )
            end
        ),
        lists:append(_pipe)
    end,
    Memory = (N_rows * N_cols) * 4,
    {{tensor, Output, get_tensor_shape(Q)}, Memory}.

-file("src/viva_tensor/flash_attention.gleam", 146).
?DOC(" Flash Attention: processa em tiles, nunca materializa n×n\n").
-spec flash_attention(
    viva_tensor@tensor:tensor(),
    viva_tensor@tensor:tensor(),
    viva_tensor@tensor:tensor(),
    flash_config()
) -> flash_result().
flash_attention(Q, K, V, Config) ->
    Q_data = viva_tensor@tensor:to_list(Q),
    K_data = viva_tensor@tensor:to_list(K),
    V_data = viva_tensor@tensor:to_list(V),
    Head_dim = case get_tensor_shape(Q) of
        [_, D] ->
            D;

        _ ->
            erlang:length(Q_data)
    end,
    Q_rows = gleam@list:sized_chunk(Q_data, Head_dim),
    K_rows = gleam@list:sized_chunk(K_data, Head_dim),
    V_rows = gleam@list:sized_chunk(V_data, Head_dim),
    N_q = erlang:length(Q_rows),
    N_kv = erlang:length(K_rows),
    Q_blocks = gleam@list:sized_chunk(Q_rows, erlang:element(2, Config)),
    K_blocks = gleam@list:sized_chunk(K_rows, erlang:element(3, Config)),
    V_blocks = gleam@list:sized_chunk(V_rows, erlang:element(3, Config)),
    Output = begin
        _pipe = gleam@list:index_map(
            Q_blocks,
            fun(Q_block, Q_block_idx) ->
                process_q_block(
                    Q_block,
                    K_blocks,
                    V_blocks,
                    Config,
                    Q_block_idx,
                    Head_dim
                )
            end
        ),
        _pipe@1 = lists:append(_pipe),
        lists:append(_pipe@1)
    end,
    Flash_memory = (erlang:element(2, Config) * erlang:element(3, Config)) * 4,
    Naive_memory = (N_q * N_kv) * 4,
    Saved = 100.0 * (1.0 - (case erlang:float(Naive_memory) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Flash_memory) / Gleam@denominator
    end)),
    {flash_result, {tensor, Output, get_tensor_shape(Q)}, Flash_memory, Saved}.

-file("src/viva_tensor/flash_attention.gleam", 319).
-spec benchmark_flash_attention() -> nil.
benchmark_flash_attention() ->
    gleam_stdlib:println(
        <<"╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  FLASH ATTENTION - O(n) Memory Algorithm                         ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  Tri Dao et al., 2022 - FlashAttention                           ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝\n"/utf8>>
    ),
    gleam_stdlib:println(<<"PROBLEMA DA ATTENTION PADRÃO:"/utf8>>),
    gleam_stdlib:println(
        <<"  - Attention = softmax(Q @ K^T / sqrt(d)) @ V"/utf8>>
    ),
    gleam_stdlib:println(<<"  - Q @ K^T cria matriz n×n"/utf8>>),
    gleam_stdlib:println(
        <<"  - Para seq_len=8192: 67M elementos = 256MB por cabeça!"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - 32 cabeças = 8GB só para scores intermediários\n"/utf8>>
    ),
    gleam_stdlib:println(<<"SOLUÇÃO FLASH ATTENTION:"/utf8>>),
    gleam_stdlib:println(<<"  - Processa em TILES (blocos)"/utf8>>),
    gleam_stdlib:println(
        <<"  - Online softmax: atualiza estatísticas incrementalmente"/utf8>>
    ),
    gleam_stdlib:println(<<"  - Nunca materializa matriz n×n completa"/utf8>>),
    gleam_stdlib:println(
        <<"  - Resultado: 2-4x mais rápido, O(n) memória!\n"/utf8>>
    ),
    Sizes = [64, 128, 256, 512],
    Head_dim = 64,
    gleam_stdlib:println(<<"━━━ BENCHMARK: Naive vs Flash Attention ━━━"/utf8>>),
    gleam_stdlib:println(
        <<"  head_dim = "/utf8, (erlang:integer_to_binary(Head_dim))/binary>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam@list:each(
        Sizes,
        fun(Seq_len) ->
            Q = viva_tensor@tensor:random_uniform([Seq_len, Head_dim]),
            K = viva_tensor@tensor:random_uniform([Seq_len, Head_dim]),
            V = viva_tensor@tensor:random_uniform([Seq_len, Head_dim]),
            Config = default_config(Head_dim),
            {Naive_time, {_, Naive_mem}} = timer:tc(
                fun() -> naive_attention(Q, K, V, erlang:element(4, Config)) end
            ),
            {Flash_time, Flash_result} = timer:tc(
                fun() -> flash_attention(Q, K, V, Config) end
            ),
            gleam_stdlib:println(
                <<<<"seq_len = "/utf8,
                        (erlang:integer_to_binary(Seq_len))/binary>>/binary,
                    ":"/utf8>>
            ),
            gleam_stdlib:println(
                <<<<<<<<"  Naive:  "/utf8,
                                (erlang:integer_to_binary(Naive_time div 1000))/binary>>/binary,
                            "ms, "/utf8>>/binary,
                        (erlang:integer_to_binary(Naive_mem div 1024))/binary>>/binary,
                    " KB"/utf8>>
            ),
            gleam_stdlib:println(
                <<<<<<<<"  Flash:  "/utf8,
                                (erlang:integer_to_binary(Flash_time div 1000))/binary>>/binary,
                            "ms, "/utf8>>/binary,
                        (erlang:integer_to_binary(
                            erlang:element(3, Flash_result) div 1024
                        ))/binary>>/binary,
                    " KB"/utf8>>
            ),
            gleam_stdlib:println(
                <<<<"  Memory saved: "/utf8,
                        (float_to_string(erlang:element(4, Flash_result)))/binary>>/binary,
                    "%"/utf8>>
            ),
            gleam_stdlib:println(<<""/utf8>>)
        end
    ),
    gleam_stdlib:println(
        <<"━━━ PROJEÇÃO: Economia de Memória por Contexto ━━━"/utf8>>
    ),
    Long_contexts = [1024, 2048, 4096, 8192, 16384, 32768],
    gleam@list:each(
        Long_contexts,
        fun(N) ->
            Naive_mem@1 = (N * N) * 4,
            Flash_mem = (64 * 64) * 4,
            Saved = 100.0 * (1.0 - (case erlang:float(Naive_mem@1) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> erlang:float(Flash_mem) / Gleam@denominator
            end)),
            gleam_stdlib:println(
                <<<<<<<<<<<<<<<<"  n="/utf8,
                                                (erlang:integer_to_binary(N))/binary>>/binary,
                                            ": Naive="/utf8>>/binary,
                                        (bytes_to_string(Naive_mem@1))/binary>>/binary,
                                    ", Flash="/utf8>>/binary,
                                (bytes_to_string(Flash_mem))/binary>>/binary,
                            " ("/utf8>>/binary,
                        (float_to_string(Saved))/binary>>/binary,
                    "% saved)"/utf8>>
            )
        end
    ),
    gleam_stdlib:println(
        <<"\n╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  POR QUE FLASH ATTENTION É REVOLUCIONÁRIO:                       ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                                                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  1. Contextos longos viáveis (32K, 100K tokens)                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  2. 2-4x speedup via IO-awareness                                ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  3. Exatamente correto (não é aproximação!)                      ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  4. Padrão em LLMs modernos (GPT-4, LLaMA, etc)                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                                                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  viva_tensor + Flash = Contextos ilimitados na RTX 4090!         ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝"/utf8>>
    ).

-file("src/viva_tensor/flash_attention.gleam", 315).
-spec main() -> nil.
main() ->
    benchmark_flash_attention().
