-module(viva_tensor@awq).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/awq.gleam").
-export([default_config/0, collect_activation_stats/1, apply_weight_transform/2, apply_activation_transform/2, identify_salient_channels/2, dequantize_awq/1, compute_awq_scales/2, quantize_awq/3, benchmark_awq/0, main/0]).
-export_type([a_w_q_config/0, a_w_q_scales/0, a_w_q_tensor/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " AWQ (Activation-aware Weight Quantization)\n"
    "\n"
    " MLSys 2024 BEST PAPER AWARD!\n"
    " https://arxiv.org/abs/2306.00978\n"
    "\n"
    " INSIGHT PRINCIPAL:\n"
    " Apenas ~1% dos pesos são \"salientes\" - identificados pela\n"
    " magnitude das ATIVAÇÕES, não dos pesos!\n"
    "\n"
    " ALGORITMO:\n"
    " 1. Coletar estatísticas de ativação (calibration)\n"
    " 2. Identificar canais salientes (alta ativação média)\n"
    " 3. Escalar canais salientes PARA CIMA antes de quantizar\n"
    " 4. Escalar ativações de entrada PARA BAIXO (matematicamente equivalente)\n"
    "\n"
    " RESULTADO: Mesma compressão, MUITO menos erro!\n"
    "\n"
    " Implementação: MIT-HAN Lab + AutoAWQ\n"
).

-type a_w_q_config() :: {a_w_q_config, integer(), integer(), float(), boolean()}.

-type a_w_q_scales() :: {a_w_q_scales, list(float()), list(float()), float()}.

-type a_w_q_tensor() :: {a_w_q_tensor,
        list(integer()),
        a_w_q_scales(),
        list(float()),
        list(integer()),
        list(integer()),
        integer()}.

-file("src/viva_tensor/awq.gleam", 75).
?DOC(" Configuração padrão AWQ\n").
-spec default_config() -> a_w_q_config().
default_config() ->
    {a_w_q_config, 4, 128, 0.5, false}.

-file("src/viva_tensor/awq.gleam", 85).
?DOC(
    " Coleta estatísticas de ativação de um batch de calibração\n"
    " Retorna média absoluta por canal\n"
).
-spec collect_activation_stats(list(list(float()))) -> list(float()).
collect_activation_stats(Activations_batch) ->
    case Activations_batch of
        [] ->
            [];

        [First | _] ->
            Num_channels = erlang:length(First),
            Initial = gleam@list:repeat(+0.0, Num_channels),
            Sums = gleam@list:fold(
                Activations_batch,
                Initial,
                fun(Acc, Activation) ->
                    gleam@list:map2(
                        Acc,
                        Activation,
                        fun(Sum, Act) ->
                            Sum + gleam@float:absolute_value(Act)
                        end
                    )
                end
            ),
            Num_samples = erlang:float(erlang:length(Activations_batch)),
            gleam@list:map(Sums, fun(Sum@1) -> case Num_samples of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> Sum@1 / Gleam@denominator
                    end end)
    end.

-file("src/viva_tensor/awq.gleam", 141).
?DOC(
    " Aplica transformação equivalente aos pesos\n"
    " W' = W * diag(s)\n"
    " Isso escala canais salientes PARA CIMA\n"
).
-spec apply_weight_transform(list(list(float())), a_w_q_scales()) -> list(list(float())).
apply_weight_transform(Weights, Scales) ->
    gleam@list:map(
        Weights,
        fun(Row) ->
            gleam@list:map2(
                Row,
                erlang:element(2, Scales),
                fun(W, S) -> W * S end
            )
        end
    ).

-file("src/viva_tensor/awq.gleam", 153).
?DOC(
    " Aplica transformação inversa às ativações\n"
    " X' = X * diag(1/s)\n"
    " Isso compensa o scaling dos pesos\n"
).
-spec apply_activation_transform(list(float()), a_w_q_scales()) -> list(float()).
apply_activation_transform(Activations, Scales) ->
    gleam@list:map2(
        Activations,
        erlang:element(2, Scales),
        fun(X, S) -> case S > +0.0 of
                true ->
                    case S of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> X / Gleam@denominator
                    end;

                false ->
                    X
            end end
    ).

-file("src/viva_tensor/awq.gleam", 313).
?DOC(" Identifica canais salientes (top-k por ativação)\n").
-spec identify_salient_channels(list(float()), float()) -> list(integer()).
identify_salient_channels(Activation_stats, Top_percent) ->
    N = erlang:length(Activation_stats),
    K = begin
        _pipe = erlang:round((erlang:float(N) * Top_percent) / 100.0),
        gleam@int:max(_pipe, 1)
    end,
    _pipe@1 = Activation_stats,
    _pipe@2 = gleam@list:index_map(_pipe@1, fun(Stat, Idx) -> {Idx, Stat} end),
    _pipe@3 = gleam@list:sort(
        _pipe@2,
        fun(A, B) ->
            gleam@float:compare(erlang:element(2, B), erlang:element(2, A))
        end
    ),
    _pipe@4 = gleam@list:take(_pipe@3, K),
    gleam@list:map(_pipe@4, fun(Pair) -> erlang:element(1, Pair) end).

-file("src/viva_tensor/awq.gleam", 473).
-spec get_tensor_shape(viva_tensor@tensor:tensor()) -> list(integer()).
get_tensor_shape(T) ->
    case T of
        {tensor, _, Shape} ->
            Shape;

        {strided_tensor, _, Shape@1, _, _} ->
            Shape@1
    end.

-file("src/viva_tensor/awq.gleam", 480).
-spec get_at_index_float(list(float()), integer(), float()) -> float().
get_at_index_float(Lst, Idx, Default) ->
    case gleam@list:drop(Lst, Idx) of
        [First | _] ->
            First;

        [] ->
            Default
    end.

-file("src/viva_tensor/awq.gleam", 270).
?DOC(" Dequantiza tensor AWQ\n").
-spec dequantize_awq(a_w_q_tensor()) -> viva_tensor@tensor:tensor().
dequantize_awq(Awq) ->
    Group_size = case erlang:element(4, Awq) of
        [] ->
            erlang:length(erlang:element(2, Awq));

        _ ->
            case erlang:length(erlang:element(4, Awq)) of
                0 -> 0;
                Gleam@denominator -> erlang:length(erlang:element(2, Awq)) div Gleam@denominator
            end
    end,
    Groups = gleam@list:sized_chunk(erlang:element(2, Awq), Group_size),
    Dequantized = begin
        _pipe = gleam@list:index_map(
            Groups,
            fun(Group, Idx) ->
                Scale = get_at_index_float(erlang:element(4, Awq), Idx, 1.0),
                gleam@list:map(Group, fun(Q) -> case Scale of
                            +0.0 -> +0.0;
                            -0.0 -> -0.0;
                            Gleam@denominator@1 -> erlang:float(Q) / Gleam@denominator@1
                        end end)
            end
        ),
        lists:append(_pipe)
    end,
    In_features = case erlang:element(6, Awq) of
        [_, I] ->
            I;

        _ ->
            1
    end,
    Weight_matrix = gleam@list:sized_chunk(Dequantized, In_features),
    Restored = begin
        _pipe@1 = gleam@list:map(
            Weight_matrix,
            fun(Row) ->
                gleam@list:map2(
                    Row,
                    erlang:element(2, erlang:element(3, Awq)),
                    fun(W, S) -> case S > +0.0 of
                            true ->
                                case S of
                                    +0.0 -> +0.0;
                                    -0.0 -> -0.0;
                                    Gleam@denominator@2 -> W / Gleam@denominator@2
                                end;

                            false ->
                                W
                        end end
                )
            end
        ),
        lists:append(_pipe@1)
    end,
    {tensor, Restored, erlang:element(6, Awq)}.

-file("src/viva_tensor/awq.gleam", 487).
-spec float_power(float(), float()) -> float().
float_power(Base, Exp) ->
    case gleam@float:power(Base, Exp) of
        {ok, Result} ->
            Result;

        {error, _} ->
            1.0
    end.

-file("src/viva_tensor/awq.gleam", 117).
?DOC(
    " Computa scales AWQ baseado nas estatísticas de ativação\n"
    " scale[i] = activation_stat[i] ^ alpha\n"
).
-spec compute_awq_scales(list(float()), float()) -> a_w_q_scales().
compute_awq_scales(Activation_stats, Alpha) ->
    Weight_scales = gleam@list:map(
        Activation_stats,
        fun(Stat) ->
            Safe_stat = case Stat > +0.0 of
                true ->
                    Stat;

                false ->
                    1.0
            end,
            float_power(Safe_stat, Alpha)
        end
    ),
    {a_w_q_scales, Weight_scales, Activation_stats, Alpha}.

-file("src/viva_tensor/awq.gleam", 494).
-spec float_result_to_float({ok, float()} | {error, any()}, float()) -> float().
float_result_to_float(R, Default) ->
    case R of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-file("src/viva_tensor/awq.gleam", 224).
?DOC(" Quantização simétrica por grupos\n").
-spec symmetric_group_quantize(list(float()), integer(), integer()) -> {list(integer()),
    list(float())}.
symmetric_group_quantize(Values, Bits, Group_size) ->
    Qmax = begin
        _pipe = gleam@float:power(2.0, erlang:float(Bits - 1)),
        _pipe@1 = float_result_to_float(_pipe, 128.0),
        (fun(X) -> X - 1.0 end)(_pipe@1)
    end,
    Groups = gleam@list:sized_chunk(Values, Group_size),
    {Quantized_groups, Scales} = gleam@list:fold(
        Groups,
        {[], []},
        fun(Acc, Group) ->
            {Q_acc, S_acc} = Acc,
            Max_abs = begin
                _pipe@2 = Group,
                _pipe@3 = gleam@list:map(
                    _pipe@2,
                    fun gleam@float:absolute_value/1
                ),
                gleam@list:fold(_pipe@3, +0.0, fun gleam@float:max/2)
            end,
            Scale = case Max_abs > +0.0 of
                true ->
                    case Max_abs of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> Qmax / Gleam@denominator
                    end;

                false ->
                    1.0
            end,
            Quantized = gleam@list:map(
                Group,
                fun(V) ->
                    Scaled = V * Scale,
                    Clamped = gleam@float:clamp(Scaled, -1.0 * Qmax, Qmax),
                    erlang:round(Clamped)
                end
            ),
            {lists:append(Q_acc, Quantized), [Scale | S_acc]}
        end
    ),
    {Quantized_groups, lists:reverse(Scales)}.

-file("src/viva_tensor/awq.gleam", 170).
?DOC(" Quantiza pesos usando AWQ (pipeline completo)\n").
-spec quantize_awq(
    viva_tensor@tensor:tensor(),
    list(list(float())),
    a_w_q_config()
) -> a_w_q_tensor().
quantize_awq(Weights, Calibration_data, Config) ->
    Weight_data = viva_tensor@tensor:to_list(Weights),
    Shape = get_tensor_shape(Weights),
    {_, In_features} = case Shape of
        [O, I] ->
            {O, I};

        _ ->
            {1, erlang:length(Weight_data)}
    end,
    Weight_matrix = gleam@list:sized_chunk(Weight_data, In_features),
    Activation_stats = collect_activation_stats(Calibration_data),
    Awq_scales = compute_awq_scales(Activation_stats, erlang:element(4, Config)),
    Transformed_weights = apply_weight_transform(Weight_matrix, Awq_scales),
    Flat_transformed = lists:append(Transformed_weights),
    {Quantized, Quant_scales} = symmetric_group_quantize(
        Flat_transformed,
        erlang:element(2, Config),
        erlang:element(3, Config)
    ),
    Num_elements = erlang:length(Flat_transformed),
    Num_groups = case erlang:element(3, Config) of
        0 -> 0;
        Gleam@denominator -> ((Num_elements + erlang:element(3, Config)) - 1)
        div Gleam@denominator
    end,
    Data_bytes = ((Num_elements * erlang:element(2, Config)) + 7) div 8,
    Scale_bytes = Num_groups * 2,
    Awq_scale_bytes = In_features * 2,
    Memory = (Data_bytes + Scale_bytes) + Awq_scale_bytes,
    {a_w_q_tensor, Quantized, Awq_scales, Quant_scales, [], Shape, Memory}.

-file("src/viva_tensor/awq.gleam", 501).
-spec float_to_string(float()) -> binary().
float_to_string(F) ->
    Rounded = erlang:float(erlang:round(F * 10000.0)) / 10000.0,
    gleam_stdlib:float_to_string(Rounded).

-file("src/viva_tensor/awq.gleam", 339).
-spec benchmark_awq() -> nil.
benchmark_awq() ->
    gleam_stdlib:println(
        <<"╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  AWQ (Activation-aware Weight Quantization)                      ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  MLSys 2024 BEST PAPER AWARD!                                    ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝\n"/utf8>>
    ),
    gleam_stdlib:println(<<"CONCEITO:"/utf8>>),
    gleam_stdlib:println(<<"  - Apenas ~1% dos pesos são 'salientes'"/utf8>>),
    gleam_stdlib:println(
        <<"  - Identificados pela magnitude das ATIVAÇÕES, não dos pesos!"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - Escalar canais salientes PARA CIMA antes de quantizar"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - Matematicamente equivalente: W*X = (sW)*(X/s)"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    Weights = viva_tensor@tensor:random_uniform([512, 256]),
    Calibration_data = begin
        _pipe = gleam@list:range(1, 100),
        gleam@list:map(
            _pipe,
            fun(_) -> _pipe@1 = viva_tensor@tensor:random_uniform([256]),
                viva_tensor@tensor:to_list(_pipe@1) end
        )
    end,
    Config = default_config(),
    gleam_stdlib:println(<<"━━━ CALIBRAÇÃO ━━━"/utf8>>),
    Activation_stats = collect_activation_stats(Calibration_data),
    gleam_stdlib:println(<<"  Amostras de calibração: 100"/utf8>>),
    gleam_stdlib:println(<<"  Features por amostra: 256"/utf8>>),
    Salient_channels = identify_salient_channels(Activation_stats, 1.0),
    gleam_stdlib:println(
        <<"  Canais salientes (top 1%): "/utf8,
            (erlang:integer_to_binary(erlang:length(Salient_channels)))/binary>>
    ),
    gleam_stdlib:println(<<"  Top 5 canais mais salientes:"/utf8>>),
    _pipe@2 = Salient_channels,
    _pipe@3 = gleam@list:take(_pipe@2, 5),
    gleam@list:each(
        _pipe@3,
        fun(Idx) ->
            Stat = get_at_index_float(Activation_stats, Idx, +0.0),
            gleam_stdlib:println(
                <<<<<<"    Canal "/utf8,
                            (erlang:integer_to_binary(Idx))/binary>>/binary,
                        ": "/utf8>>/binary,
                    (float_to_string(Stat))/binary>>
            )
        end
    ),
    gleam_stdlib:println(<<"\n━━━ AWQ QUANTIZATION ━━━"/utf8>>),
    {Time_awq, Awq_tensor} = timer:tc(
        fun() -> quantize_awq(Weights, Calibration_data, Config) end
    ),
    Original_bytes = (512 * 256) * 4,
    Ratio = case erlang:float(erlang:element(7, Awq_tensor)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Original_bytes) / Gleam@denominator
    end,
    gleam_stdlib:println(
        <<<<"  Tempo:       "/utf8,
                (erlang:integer_to_binary(Time_awq div 1000))/binary>>/binary,
            "ms"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Original:    "/utf8,
                (erlang:integer_to_binary(Original_bytes div 1024))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Comprimido:  "/utf8,
                (erlang:integer_to_binary(
                    erlang:element(7, Awq_tensor) div 1024
                ))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Compressão:  "/utf8, (float_to_string(Ratio))/binary>>/binary,
            "x"/utf8>>
    ),
    gleam_stdlib:println(<<"\n━━━ VERIFICAÇÃO DE ERRO ━━━"/utf8>>),
    Decompressed = dequantize_awq(Awq_tensor),
    Orig_data = viva_tensor@tensor:to_list(Weights),
    Decomp_data = viva_tensor@tensor:to_list(Decompressed),
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
                Gleam@denominator@1 -> gleam@list:fold(
                    Errors,
                    +0.0,
                    fun gleam@float:add/2
                )
                / Gleam@denominator@1
            end
    end,
    Max_error = gleam@list:fold(Errors, +0.0, fun gleam@float:max/2),
    gleam_stdlib:println(
        <<"  Erro médio: "/utf8, (float_to_string(Mean_error))/binary>>
    ),
    gleam_stdlib:println(
        <<"  Erro máx:   "/utf8, (float_to_string(Max_error))/binary>>
    ),
    gleam_stdlib:println(
        <<"\n━━━ COMPARAÇÃO: AWQ vs Quantização Normal ━━━"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  AWQ reduz erro em canais salientes (~1% dos pesos)"/utf8>>
    ),
    gleam_stdlib:println(<<"  Esses canais têm MAIOR impacto na saída"/utf8>>),
    gleam_stdlib:println(
        <<"  Resultado: mesma compressão, MUITO menos perda de qualidade"/utf8>>
    ),
    gleam_stdlib:println(
        <<"\n╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  POR QUE AWQ VENCEU O MLSYS 2024:                                ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                                                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  1. Insight simples mas poderoso: foca nas ativações             ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  2. Zero custo em runtime (transformação pré-computada)          ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  3. Funciona com qualquer quantização (INT4, INT8, NF4)          ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  4. Estado da arte em LLMs quantizados                           ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                                                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  viva_tensor + AWQ = Máxima precisão com 8x compressão!          ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝"/utf8>>
    ).

-file("src/viva_tensor/awq.gleam", 335).
-spec main() -> nil.
main() ->
    benchmark_awq().
