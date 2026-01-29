-module(viva_tensor@sparsity).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/sparsity.gleam").
-export([decompress/1, compute_metrics/2, prune_24_magnitude/1, prune_24_gradient/2, sparse_matmul/2, benchmark_sparsity/0, main/0]).
-export_type([sparse24_block/0, sparse24_tensor/0, prune_metrics/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " 2:4 Structured Sparsity\n"
    "\n"
    " NVIDIA Tensor Cores Structured Sparsity\n"
    " Ampere+ Architecture (RTX 3000/4000, A100, H100)\n"
    "\n"
    " CONCEITO:\n"
    " Em cada grupo de 4 elementos, apenas 2 são não-zero\n"
    " = 50% dos elementos são zero, mas em padrão ESTRUTURADO\n"
    "\n"
    " POR QUE ESTRUTURADO > ALEATÓRIO:\n"
    " - Sparsity aleatória: difícil de acelerar em hardware\n"
    " - Sparsity estruturada: hardware pode pular zeros eficientemente\n"
    "\n"
    " FORMATO DE ARMAZENAMENTO:\n"
    " - 2 valores FP16 (32 bits)\n"
    " - 2-bit máscara indicando posições (4 bits para 4 posições)\n"
    " - Total: 36 bits para 4 elementos = 9 bits/elemento vs 16 bits/elemento\n"
    " - Compressão: ~1.8x\n"
    "\n"
    " PERFORMANCE:\n"
    " - 2x throughput em Tensor Cores (pula multiplicações por zero)\n"
    " - Combinado com INT8: 4x speedup total!\n"
).

-type sparse24_block() :: {sparse24_block,
        {float(), float()},
        {integer(), integer()}}.

-type sparse24_tensor() :: {sparse24_tensor,
        list(sparse24_block()),
        list(integer()),
        integer(),
        integer(),
        float()}.

-type prune_metrics() :: {prune_metrics,
        integer(),
        integer(),
        float(),
        float(),
        float()}.

-file("src/viva_tensor/sparsity.gleam", 112).
?DOC(" Poda um grupo de 4 elementos, retornando Sparse24Block\n").
-spec prune_group_magnitude(list(float())) -> sparse24_block().
prune_group_magnitude(Group) ->
    Indexed = gleam@list:index_map(
        Group,
        fun(Val, Idx) -> {Idx, Val, gleam@float:absolute_value(Val)} end
    ),
    Sorted = gleam@list:sort(
        Indexed,
        fun(A, B) ->
            gleam@float:compare(erlang:element(3, B), erlang:element(3, A))
        end
    ),
    case Sorted of
        [First, Second | _] ->
            {Pos1, Val1, _} = First,
            {Pos2, Val2, _} = Second,
            {P1, V1, P2, V2} = case Pos1 < Pos2 of
                true ->
                    {Pos1, Val1, Pos2, Val2};

                false ->
                    {Pos2, Val2, Pos1, Val1}
            end,
            {sparse24_block, {V1, V2}, {P1, P2}};

        _ ->
            {sparse24_block, {+0.0, +0.0}, {0, 1}}
    end.

-file("src/viva_tensor/sparsity.gleam", 143).
?DOC(" Pad grupo para ter exatamente 4 elementos\n").
-spec pad_group(list(float())) -> list(float()).
pad_group(Group) ->
    Len = erlang:length(Group),
    case Len < 4 of
        true ->
            lists:append(Group, gleam@list:repeat(+0.0, 4 - Len));

        false ->
            gleam@list:take(Group, 4)
    end.

-file("src/viva_tensor/sparsity.gleam", 188).
-spec prune_group_by_importance(list(float()), list(float())) -> sparse24_block().
prune_group_by_importance(Weights, Importance) ->
    Indexed = begin
        _pipe = gleam@list:zip(
            gleam@list:range(0, 3),
            gleam@list:zip(Weights, Importance)
        ),
        gleam@list:map(
            _pipe,
            fun(X) ->
                {Idx, {W, I}} = X,
                {Idx, W, I}
            end
        )
    end,
    Sorted = gleam@list:sort(
        Indexed,
        fun(A, B) ->
            gleam@float:compare(erlang:element(3, B), erlang:element(3, A))
        end
    ),
    case Sorted of
        [First, Second | _] ->
            {Pos1, Val1, _} = First,
            {Pos2, Val2, _} = Second,
            {P1, V1, P2, V2} = case Pos1 < Pos2 of
                true ->
                    {Pos1, Val1, Pos2, Val2};

                false ->
                    {Pos2, Val2, Pos1, Val1}
            end,
            {sparse24_block, {V1, V2}, {P1, P2}};

        _ ->
            {sparse24_block, {+0.0, +0.0}, {0, 1}}
    end.

-file("src/viva_tensor/sparsity.gleam", 220).
?DOC(" Reconstrói tensor denso a partir de 2:4 sparse\n").
-spec decompress(sparse24_tensor()) -> viva_tensor@tensor:tensor().
decompress(Sparse) ->
    Data = gleam@list:flat_map(
        erlang:element(2, Sparse),
        fun(Block) ->
            {V1, V2} = erlang:element(2, Block),
            {P1, P2} = erlang:element(3, Block),
            _pipe = gleam@list:range(0, 3),
            gleam@list:map(_pipe, fun(I) -> case I =:= P1 of
                        true ->
                            V1;

                        false ->
                            case I =:= P2 of
                                true ->
                                    V2;

                                false ->
                                    +0.0
                            end
                    end end)
        end
    ),
    Truncated = gleam@list:take(Data, erlang:element(4, Sparse)),
    {tensor, Truncated, erlang:element(3, Sparse)}.

-file("src/viva_tensor/sparsity.gleam", 298).
-spec transpose_matrix(list(list(float()))) -> list(list(float())).
transpose_matrix(M) ->
    case M of
        [] ->
            [];

        [First | _] ->
            N_cols = erlang:length(First),
            _pipe = gleam@list:range(0, N_cols - 1),
            gleam@list:map(
                _pipe,
                fun(Col_idx) ->
                    gleam@list:filter_map(
                        M,
                        fun(Row) -> case gleam@list:drop(Row, Col_idx) of
                                [X | _] ->
                                    {ok, X};

                                [] ->
                                    {error, nil}
                            end end
                    )
                end
            )
    end.

-file("src/viva_tensor/sparsity.gleam", 321).
?DOC(" Calcula métricas de poda\n").
-spec compute_metrics(viva_tensor@tensor:tensor(), sparse24_tensor()) -> prune_metrics().
compute_metrics(Original, Sparse) ->
    Orig_data = viva_tensor@tensor:to_list(Original),
    Decomp_data = viva_tensor@tensor:to_list(decompress(Sparse)),
    Errors = gleam@list:map2(
        Orig_data,
        Decomp_data,
        fun(O, D) -> gleam@float:absolute_value(O - D) end
    ),
    Mean_error = case Errors of
        [] ->
            +0.0;

        _ ->
            case erlang:float(erlang:length(Errors)) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> gleam@list:fold(
                    Errors,
                    +0.0,
                    fun gleam@float:add/2
                )
                / Gleam@denominator
            end
    end,
    Kept = gleam@list:filter_map(
        gleam@list:zip(Orig_data, Decomp_data),
        fun(Pair) ->
            {O@1, D@1} = Pair,
            case gleam@float:absolute_value(D@1) > +0.0 of
                true ->
                    {ok, gleam@float:absolute_value(O@1)};

                false ->
                    {error, nil}
            end
        end
    ),
    Pruned = gleam@list:filter_map(
        gleam@list:zip(Orig_data, Decomp_data),
        fun(Pair@1) ->
            {O@2, D@2} = Pair@1,
            case gleam@float:absolute_value(D@2) > +0.0 of
                false ->
                    {ok, gleam@float:absolute_value(O@2)};

                true ->
                    {error, nil}
            end
        end
    ),
    Kept_mean = case Kept of
        [] ->
            +0.0;

        _ ->
            case erlang:float(erlang:length(Kept)) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@1 -> gleam@list:fold(
                    Kept,
                    +0.0,
                    fun gleam@float:add/2
                )
                / Gleam@denominator@1
            end
    end,
    Pruned_mean = case Pruned of
        [] ->
            +0.0;

        _ ->
            case erlang:float(erlang:length(Pruned)) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@2 -> gleam@list:fold(
                    Pruned,
                    +0.0,
                    fun gleam@float:add/2
                )
                / Gleam@denominator@2
            end
    end,
    {prune_metrics,
        erlang:length(Pruned),
        erlang:length(Orig_data),
        Mean_error,
        Kept_mean,
        Pruned_mean}.

-file("src/viva_tensor/sparsity.gleam", 529).
-spec float_to_string(float()) -> binary().
float_to_string(F) ->
    Rounded = erlang:float(erlang:round(F * 10000.0)) / 10000.0,
    gleam_stdlib:float_to_string(Rounded).

-file("src/viva_tensor/sparsity.gleam", 534).
-spec get_tensor_shape(viva_tensor@tensor:tensor()) -> list(integer()).
get_tensor_shape(T) ->
    case T of
        {tensor, _, Shape} ->
            Shape;

        {strided_tensor, _, Shape@1, _, _} ->
            Shape@1
    end.

-file("src/viva_tensor/sparsity.gleam", 82).
?DOC(
    " Aplica poda 2:4: mantém os 2 maiores em cada grupo de 4\n"
    " Estratégia: magnitude (abs) - padrão da NVIDIA\n"
).
-spec prune_24_magnitude(viva_tensor@tensor:tensor()) -> sparse24_tensor().
prune_24_magnitude(T) ->
    Data = viva_tensor@tensor:to_list(T),
    Shape = get_tensor_shape(T),
    Num_elements = erlang:length(Data),
    Groups = gleam@list:sized_chunk(Data, 4),
    Blocks = gleam@list:map(
        Groups,
        fun(Group) -> prune_group_magnitude(pad_group(Group)) end
    ),
    Num_blocks = erlang:length(Blocks),
    Memory = Num_blocks * 5,
    {sparse24_tensor, Blocks, Shape, Num_elements, Memory, 50.0}.

-file("src/viva_tensor/sparsity.gleam", 157).
?DOC(
    " Poda baseada em gradiente (para treinamento)\n"
    " Mantém elementos com maior |valor × gradiente|\n"
).
-spec prune_24_gradient(
    viva_tensor@tensor:tensor(),
    viva_tensor@tensor:tensor()
) -> sparse24_tensor().
prune_24_gradient(Weights, Gradients) ->
    W_data = viva_tensor@tensor:to_list(Weights),
    G_data = viva_tensor@tensor:to_list(Gradients),
    Shape = get_tensor_shape(Weights),
    Num_elements = erlang:length(W_data),
    Importance = gleam@list:map2(
        W_data,
        G_data,
        fun(W, G) -> gleam@float:absolute_value(W * G) end
    ),
    W_groups = gleam@list:sized_chunk(W_data, 4),
    I_groups = gleam@list:sized_chunk(Importance, 4),
    Blocks = gleam@list:map2(
        W_groups,
        I_groups,
        fun(W_group, I_group) ->
            prune_group_by_importance(pad_group(W_group), I_group)
        end
    ),
    Num_blocks = erlang:length(Blocks),
    {sparse24_tensor, Blocks, Shape, Num_elements, Num_blocks * 5, 50.0}.

-file("src/viva_tensor/sparsity.gleam", 267).
?DOC(" Matmul básico para tensors\n").
-spec tensor_matmul(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> viva_tensor@tensor:tensor().
tensor_matmul(A, B) ->
    A_data = viva_tensor@tensor:to_list(A),
    B_data = viva_tensor@tensor:to_list(B),
    A_shape = get_tensor_shape(A),
    B_shape = get_tensor_shape(B),
    {M, K} = case A_shape of
        [Rows, Cols] ->
            {Rows, Cols};

        _ ->
            {1, erlang:length(A_data)}
    end,
    {_, N} = case B_shape of
        [Rows@1, Cols@1] ->
            {Rows@1, Cols@1};

        _ ->
            {erlang:length(B_data), 1}
    end,
    A_rows = gleam@list:sized_chunk(A_data, K),
    B_cols = transpose_matrix(gleam@list:sized_chunk(B_data, N)),
    Result = gleam@list:flat_map(
        A_rows,
        fun(A_row) ->
            gleam@list:map(
                B_cols,
                fun(B_col) ->
                    _pipe = gleam@list:map2(
                        A_row,
                        B_col,
                        fun(X, Y) -> X * Y end
                    ),
                    gleam@list:fold(_pipe, +0.0, fun gleam@float:add/2)
                end
            )
        end
    ),
    {tensor, Result, [M, N]}.

-file("src/viva_tensor/sparsity.gleam", 250).
?DOC(
    " Matmul com matriz esparsa 2:4\n"
    " Em hardware real (Tensor Cores), isso é 2x mais rápido!\n"
).
-spec sparse_matmul(sparse24_tensor(), viva_tensor@tensor:tensor()) -> {viva_tensor@tensor:tensor(),
    float()}.
sparse_matmul(Sparse_a, Dense_b) ->
    Dense_a = decompress(Sparse_a),
    Result = tensor_matmul(Dense_a, Dense_b),
    Theoretical_speedup = 2.0,
    {Result, Theoretical_speedup}.

-file("src/viva_tensor/sparsity.gleam", 379).
-spec benchmark_sparsity() -> nil.
benchmark_sparsity() ->
    gleam_stdlib:println(
        <<"╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  2:4 STRUCTURED SPARSITY - NVIDIA Tensor Cores                   ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  Ampere+ Architecture (RTX 3000/4000, A100, H100)                ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝\n"/utf8>>
    ),
    gleam_stdlib:println(<<"CONCEITO:"/utf8>>),
    gleam_stdlib:println(
        <<"  - Em cada 4 elementos, mantém apenas 2 (50% sparsity)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - Padrão ESTRUTURADO permite aceleração em hardware"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - Tensor Cores pulam multiplicações por zero"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - Resultado: 2x throughput com ~1% perda de accuracy!\n"/utf8>>
    ),
    gleam_stdlib:println(<<"FORMATO DE ARMAZENAMENTO:"/utf8>>),
    gleam_stdlib:println(<<"  - Original: 4 × FP16 = 64 bits"/utf8>>),
    gleam_stdlib:println(<<"  - Sparse: 2 × FP16 + 4-bit mask = 36 bits"/utf8>>),
    gleam_stdlib:println(<<"  - Compressão: 1.78x\n"/utf8>>),
    T = viva_tensor@tensor:random_uniform([1024, 512]),
    gleam_stdlib:println(<<"━━━ BENCHMARK: Tensor [1024, 512] ━━━"/utf8>>),
    {Time_prune, Sparse} = timer:tc(fun() -> prune_24_magnitude(T) end),
    Metrics = compute_metrics(T, Sparse),
    Original_bytes = (1024 * 512) * 4,
    Compression = case erlang:float(erlang:element(5, Sparse)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Original_bytes) / Gleam@denominator
    end,
    gleam_stdlib:println(
        <<<<"  Tempo de poda:      "/utf8,
                (erlang:integer_to_binary(Time_prune div 1000))/binary>>/binary,
            "ms"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Memória original:   "/utf8,
                (erlang:integer_to_binary(Original_bytes div 1024))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Memória sparse:     "/utf8,
                (erlang:integer_to_binary(erlang:element(5, Sparse) div 1024))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Compressão:         "/utf8,
                (float_to_string(Compression))/binary>>/binary,
            "x"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Sparsity:           "/utf8,
                (float_to_string(erlang:element(6, Sparse)))/binary>>/binary,
            "%"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<<<<<"  Elementos podados:  "/utf8,
                    (erlang:integer_to_binary(erlang:element(2, Metrics)))/binary>>/binary,
                "/"/utf8>>/binary,
            (erlang:integer_to_binary(erlang:element(3, Metrics)))/binary>>
    ),
    gleam_stdlib:println(
        <<"  Erro aproximação:   "/utf8,
            (float_to_string(erlang:element(4, Metrics)))/binary>>
    ),
    gleam_stdlib:println(
        <<"  Magnitude mantida:  "/utf8,
            (float_to_string(erlang:element(5, Metrics)))/binary>>
    ),
    gleam_stdlib:println(
        <<"  Magnitude podada:   "/utf8,
            (float_to_string(erlang:element(6, Metrics)))/binary>>
    ),
    gleam_stdlib:println(<<"\n━━━ SPARSE MATMUL SIMULATION ━━━"/utf8>>),
    B = viva_tensor@tensor:random_uniform([512, 256]),
    {Time_dense, Dense_result} = timer:tc(
        fun() -> tensor_matmul(decompress(Sparse), B) end
    ),
    {Time_sparse, {Sparse_result, Speedup}} = timer:tc(
        fun() -> sparse_matmul(Sparse, B) end
    ),
    gleam_stdlib:println(
        <<<<"  Dense matmul:       "/utf8,
                (erlang:integer_to_binary(Time_dense div 1000))/binary>>/binary,
            "ms"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Sparse matmul:      "/utf8,
                (erlang:integer_to_binary(Time_sparse div 1000))/binary>>/binary,
            "ms (simulado)"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Speedup teórico:    "/utf8, (float_to_string(Speedup))/binary>>/binary,
            "x (hardware real)"/utf8>>
    ),
    Dense_data = viva_tensor@tensor:to_list(Dense_result),
    Sparse_data = viva_tensor@tensor:to_list(Sparse_result),
    Diff = begin
        _pipe = gleam@list:map2(
            Dense_data,
            Sparse_data,
            fun(D, S) -> gleam@float:absolute_value(D - S) end
        ),
        gleam@list:fold(_pipe, +0.0, fun gleam@float:max/2)
    end,
    gleam_stdlib:println(
        <<<<"  Diferença máxima:   "/utf8, (float_to_string(Diff))/binary>>/binary,
            " (deveria ser ~0)"/utf8>>
    ),
    gleam_stdlib:println(<<"\n━━━ COMPARAÇÃO: COMBINANDO TÉCNICAS ━━━"/utf8>>),
    gleam_stdlib:println(<<"  FP16:               2x compressão"/utf8>>),
    gleam_stdlib:println(<<"  INT8:               4x compressão"/utf8>>),
    gleam_stdlib:println(
        <<"  2:4 Sparsity:       2x speedup (+ 1.78x compressão)"/utf8>>
    ),
    gleam_stdlib:println(<<"  NF4:                8x compressão"/utf8>>),
    gleam_stdlib:println(<<"  "/utf8>>),
    gleam_stdlib:println(
        <<"  INT8 + 2:4:         4x × 1.78x = 7.12x compressão, 8x speedup!"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  NF4 + 2:4:          8x × 1.78x = 14.24x compressão!"/utf8>>
    ),
    gleam_stdlib:println(
        <<"\n╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  POR QUE 2:4 SPARSITY É ESSENCIAL:                               ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                                                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  1. Hardware nativo em RTX 3000/4000/A100/H100                   ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  2. 2x throughput com ~1% perda de accuracy                      ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  3. Combina com quantização para 4x+ total                       ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  4. Padrão em modelos NVIDIA (Megatron-LM, etc)                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                                                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  viva_tensor + 2:4 = Máximo uso dos Tensor Cores!                ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝"/utf8>>
    ).

-file("src/viva_tensor/sparsity.gleam", 375).
-spec main() -> nil.
main() ->
    benchmark_sparsity().
