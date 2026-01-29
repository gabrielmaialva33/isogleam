-module(viva_tensor@metrics).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/metrics.gleam").
-export([mse/2, mae/2, rmse/2, cosine_similarity/2, theoretical_sqnr/1, max_error/2, error_percentile/3, outlier_percentage/3, find_salient_weights/2, snr_db/2, compute_all/2, benchmark_metrics/0, main/0, compute_saliency/2]).
-export_type([quant_metrics/0, layer_metrics/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Métricas Avançadas para Quantização\n"
    "\n"
    " Baseado na análise do Qwen3-235B sobre algoritmos state-of-the-art:\n"
    " - MSE (Mean Squared Error)\n"
    " - MAE (Mean Absolute Error)\n"
    " - Cosine Similarity\n"
    " - SNR (Signal-to-Noise Ratio)\n"
    " - SQNR (Signal-to-Quantization-Noise Ratio)\n"
    " - Perplexity Delta (para LLMs)\n"
    "\n"
    " INSIGHTS DO QWEN3:\n"
    " 1. AWQ: Proteger 1% dos pesos salientes reduz erro drasticamente\n"
    " 2. NF4: Quantis não-uniformes (distribuição normal) > uniformes\n"
    " 3. GPTQ: Ponderar erro pelo Hessian melhora precisão\n"
    " 4. Flash Attention: Online softmax com shifting evita overflow\n"
).

-type quant_metrics() :: {quant_metrics,
        float(),
        float(),
        float(),
        float(),
        float(),
        float(),
        float(),
        float(),
        float()}.

-type layer_metrics() :: {layer_metrics, binary(), quant_metrics(), float()}.

-file("src/viva_tensor/metrics.gleam", 67).
?DOC(" MSE - Mean Squared Error\n").
-spec mse(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> float().
mse(Original, Quantized) ->
    Orig = viva_tensor@tensor:to_list(Original),
    Quant = viva_tensor@tensor:to_list(Quantized),
    Squared_errors = gleam@list:map2(
        Orig,
        Quant,
        fun(O, Q) ->
            Diff = O - Q,
            Diff * Diff
        end
    ),
    case erlang:float(erlang:length(Squared_errors)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> gleam@list:fold(
            Squared_errors,
            +0.0,
            fun(Acc, X) -> Acc + X end
        )
        / Gleam@denominator
    end.

-file("src/viva_tensor/metrics.gleam", 82).
?DOC(" MAE - Mean Absolute Error\n").
-spec mae(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> float().
mae(Original, Quantized) ->
    Orig = viva_tensor@tensor:to_list(Original),
    Quant = viva_tensor@tensor:to_list(Quantized),
    Abs_errors = gleam@list:map2(
        Orig,
        Quant,
        fun(O, Q) -> gleam@float:absolute_value(O - Q) end
    ),
    case erlang:float(erlang:length(Abs_errors)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> gleam@list:fold(
            Abs_errors,
            +0.0,
            fun(Acc, X) -> Acc + X end
        )
        / Gleam@denominator
    end.

-file("src/viva_tensor/metrics.gleam", 94).
?DOC(" RMSE - Root Mean Squared Error\n").
-spec rmse(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> float().
rmse(Original, Quantized) ->
    Mse_val = mse(Original, Quantized),
    case gleam@float:square_root(Mse_val) of
        {ok, Sqrt} ->
            Sqrt;

        {error, _} ->
            +0.0
    end.

-file("src/viva_tensor/metrics.gleam", 104).
?DOC(
    " Cosine Similarity - mede direção, não magnitude\n"
    " 1.0 = vetores idênticos, 0.0 = ortogonais, -1.0 = opostos\n"
).
-spec cosine_similarity(
    viva_tensor@tensor:tensor(),
    viva_tensor@tensor:tensor()
) -> float().
cosine_similarity(Original, Quantized) ->
    Orig = viva_tensor@tensor:to_list(Original),
    Quant = viva_tensor@tensor:to_list(Quantized),
    Dot = begin
        _pipe = gleam@list:map2(Orig, Quant, fun(O, Q) -> O * Q end),
        gleam@list:fold(_pipe, +0.0, fun(Acc, X) -> Acc + X end)
    end,
    Norm_orig = begin
        _pipe@1 = Orig,
        _pipe@2 = gleam@list:map(_pipe@1, fun(X@1) -> X@1 * X@1 end),
        _pipe@3 = gleam@list:fold(
            _pipe@2,
            +0.0,
            fun(Acc@1, X@2) -> Acc@1 + X@2 end
        ),
        gleam@float:square_root(_pipe@3)
    end,
    Norm_quant = begin
        _pipe@4 = Quant,
        _pipe@5 = gleam@list:map(_pipe@4, fun(X@3) -> X@3 * X@3 end),
        _pipe@6 = gleam@list:fold(
            _pipe@5,
            +0.0,
            fun(Acc@2, X@4) -> Acc@2 + X@4 end
        ),
        gleam@float:square_root(_pipe@6)
    end,
    case {Norm_orig, Norm_quant} of
        {{ok, No}, {ok, Nq}} when (No > +0.0) andalso (Nq > +0.0) ->
            case (No * Nq) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> Dot / Gleam@denominator
            end;

        {_, _} ->
            +0.0
    end.

-file("src/viva_tensor/metrics.gleam", 164).
?DOC(
    " SQNR - Signal-to-Quantization-Noise Ratio\n"
    " Teórico para N bits: SQNR = 6.02 * N + 1.76 dB\n"
).
-spec theoretical_sqnr(integer()) -> float().
theoretical_sqnr(Bits) ->
    (6.02 * erlang:float(Bits)) + 1.76.

-file("src/viva_tensor/metrics.gleam", 169).
?DOC(" Max Error - pior caso\n").
-spec max_error(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> float().
max_error(Original, Quantized) ->
    Orig = viva_tensor@tensor:to_list(Original),
    Quant = viva_tensor@tensor:to_list(Quantized),
    _pipe = gleam@list:map2(
        Orig,
        Quant,
        fun(O, Q) -> gleam@float:absolute_value(O - Q) end
    ),
    gleam@list:fold(_pipe, +0.0, fun gleam@float:max/2).

-file("src/viva_tensor/metrics.gleam", 182).
?DOC(" Percentil do erro (aproximado via sorting)\n").
-spec error_percentile(
    viva_tensor@tensor:tensor(),
    viva_tensor@tensor:tensor(),
    float()
) -> float().
error_percentile(Original, Quantized, Percentile) ->
    Orig = viva_tensor@tensor:to_list(Original),
    Quant = viva_tensor@tensor:to_list(Quantized),
    Errors = begin
        _pipe = gleam@list:map2(
            Orig,
            Quant,
            fun(O, Q) -> gleam@float:absolute_value(O - Q) end
        ),
        gleam@list:sort(_pipe, fun gleam@float:compare/2)
    end,
    N = erlang:length(Errors),
    Idx = erlang:round((erlang:float(N) * Percentile) / 100.0),
    Safe_idx = begin
        _pipe@1 = gleam@int:min(Idx, N - 1),
        gleam@int:max(_pipe@1, 0)
    end,
    _pipe@2 = gleam@list:drop(Errors, Safe_idx),
    _pipe@3 = gleam@list:first(_pipe@2),
    (fun(R) -> case R of
            {ok, V} ->
                V;

            {error, _} ->
                +0.0
        end end)(_pipe@3).

-file("src/viva_tensor/metrics.gleam", 209).
?DOC(" Porcentagem de outliers (erro > threshold)\n").
-spec outlier_percentage(
    viva_tensor@tensor:tensor(),
    viva_tensor@tensor:tensor(),
    float()
) -> float().
outlier_percentage(Original, Quantized, Threshold) ->
    Orig = viva_tensor@tensor:to_list(Original),
    Quant = viva_tensor@tensor:to_list(Quantized),
    Errors = gleam@list:map2(
        Orig,
        Quant,
        fun(O, Q) -> gleam@float:absolute_value(O - Q) end
    ),
    Outliers = gleam@list:filter(Errors, fun(E) -> E > Threshold end),
    N = erlang:length(Errors),
    case erlang:float(N) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> 100.0 * erlang:float(erlang:length(Outliers)) / Gleam@denominator
    end.

-file("src/viva_tensor/metrics.gleam", 310).
?DOC(" Identifica top K% de pesos salientes\n").
-spec find_salient_weights(list(float()), float()) -> list(integer()).
find_salient_weights(Saliency, Top_pct) ->
    Indexed = gleam@list:index_map(Saliency, fun(S, I) -> {I, S} end),
    Sorted = gleam@list:sort(
        Indexed,
        fun(A, B) ->
            gleam@float:compare(erlang:element(2, B), erlang:element(2, A))
        end
    ),
    N = erlang:length(Saliency),
    K = begin
        _pipe = erlang:round((erlang:float(N) * Top_pct) / 100.0),
        gleam@int:max(_pipe, 1)
    end,
    _pipe@1 = gleam@list:take(Sorted, K),
    gleam@list:map(_pipe@1, fun(Pair) -> erlang:element(1, Pair) end).

-file("src/viva_tensor/metrics.gleam", 460).
-spec get_tensor_shape(viva_tensor@tensor:tensor()) -> list(integer()).
get_tensor_shape(T) ->
    case T of
        {tensor, _, Shape} ->
            Shape;

        {strided_tensor, _, Shape@1, _, _} ->
            Shape@1
    end.

-file("src/viva_tensor/metrics.gleam", 449).
-spec add_noise(viva_tensor@tensor:tensor(), float()) -> viva_tensor@tensor:tensor().
add_noise(T, Noise_level) ->
    Data = viva_tensor@tensor:to_list(T),
    Noisy = gleam@list:index_map(
        Data,
        fun(X, I) ->
            Noise = (erlang:float((I rem 100) - 50) / 50.0) * Noise_level,
            X + Noise
        end
    ),
    {tensor, Noisy, get_tensor_shape(T)}.

-file("src/viva_tensor/metrics.gleam", 479).
-spec approximate_ln(float()) -> float().
approximate_ln(X) ->
    case X of
        V when V < 0.001 ->
            -7.0;

        V@1 when V@1 < 0.01 ->
            -4.6;

        V@2 when V@2 < 0.1 ->
            -2.3;

        V@3 when V@3 < 1.0 ->
            V@3 - 1.0;

        V@4 when V@4 < 10.0 ->
            (case V@4 of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> (V@4 - 1.0) / Gleam@denominator
            end) * 2.0;

        V@5 when V@5 < 100.0 ->
            2.3 + (case (V@5 / 10.0) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@1 -> ((V@5 / 10.0) - 1.0) / Gleam@denominator@1
            end);

        _ ->
            4.6
    end.

-file("src/viva_tensor/metrics.gleam", 467).
-spec log10(float()) -> float().
log10(X) ->
    case X > +0.0 of
        true ->
            Ln_10 = 2.302585093,
            case Ln_10 of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> approximate_ln(X) / Gleam@denominator
            end;

        false ->
            +0.0
    end.

-file("src/viva_tensor/metrics.gleam", 134).
?DOC(
    " SNR - Signal-to-Noise Ratio em dB\n"
    " SNR = 10 * log10(signal_power / noise_power)\n"
).
-spec snr_db(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> float().
snr_db(Original, Quantized) ->
    Orig = viva_tensor@tensor:to_list(Original),
    Quant = viva_tensor@tensor:to_list(Quantized),
    Signal_power = begin
        _pipe = Orig,
        _pipe@1 = gleam@list:map(_pipe, fun(X) -> X * X end),
        _pipe@2 = gleam@list:fold(_pipe@1, +0.0, fun(Acc, X@1) -> Acc + X@1 end),
        (fun(Sum) -> case erlang:float(erlang:length(Orig)) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> Sum / Gleam@denominator
            end end)(_pipe@2)
    end,
    Noise_power = begin
        _pipe@3 = gleam@list:map2(
            Orig,
            Quant,
            fun(O, Q) ->
                Diff = O - Q,
                Diff * Diff
            end
        ),
        _pipe@4 = gleam@list:fold(
            _pipe@3,
            +0.0,
            fun(Acc@1, X@2) -> Acc@1 + X@2 end
        ),
        (fun(Sum@1) -> case erlang:float(erlang:length(Orig)) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@1 -> Sum@1 / Gleam@denominator@1
            end end)(_pipe@4)
    end,
    case Noise_power > +0.0 of
        true ->
            10.0 * log10(case Noise_power of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator@2 -> Signal_power / Gleam@denominator@2
                end);

        false ->
            100.0
    end.

-file("src/viva_tensor/metrics.gleam", 230).
?DOC(" Computa todas as métricas de uma vez\n").
-spec compute_all(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> quant_metrics().
compute_all(Original, Quantized) ->
    Mse_val = mse(Original, Quantized),
    Mae_val = mae(Original, Quantized),
    Rmse_val = case gleam@float:square_root(Mse_val) of
        {ok, Sqrt} ->
            Sqrt;

        {error, _} ->
            +0.0
    end,
    Cosine_val = cosine_similarity(Original, Quantized),
    Snr_val = snr_db(Original, Quantized),
    Sqnr_val = Snr_val,
    Max_err = max_error(Original, Quantized),
    P99 = error_percentile(Original, Quantized, 99.0),
    Outliers = outlier_percentage(Original, Quantized, 0.01),
    {quant_metrics,
        Mse_val,
        Mae_val,
        Rmse_val,
        Cosine_val,
        Snr_val,
        Sqnr_val,
        Max_err,
        P99,
        Outliers}.

-file("src/viva_tensor/metrics.gleam", 495).
-spec float_to_str(float()) -> binary().
float_to_str(F) ->
    Rounded = erlang:float(erlang:round(F * 100.0)) / 100.0,
    gleam_stdlib:float_to_string(Rounded).

-file("src/viva_tensor/metrics.gleam", 500).
-spec pad_float(float()) -> binary().
pad_float(F) ->
    S = float_to_str(F),
    Len = string:length(S),
    Padding = 10 - Len,
    case Padding > 0 of
        true ->
            <<S/binary, (gleam@string:repeat(<<" "/utf8>>, Padding))/binary>>;

        false ->
            gleam@string:slice(S, 0, 10)
    end.

-file("src/viva_tensor/metrics.gleam", 339).
-spec benchmark_metrics() -> nil.
benchmark_metrics() ->
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"╔═══════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║          MÉTRICAS DE QUANTIZAÇÃO - BENCHMARK                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚═══════════════════════════════════════════════════════════════╝"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    Original = viva_tensor@tensor:random_normal([1024], +0.0, 1.0),
    Small_noise = add_noise(Original, 0.01),
    Medium_noise = add_noise(Original, 0.05),
    Large_noise = add_noise(Original, 0.1),
    gleam_stdlib:println(<<"Original: 1024 floats, mean=0, std=1"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"┌────────────────┬────────────┬────────────┬────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"│ Métrica        │ Ruído 1%   │ Ruído 5%   │ Ruído 10%  │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"├────────────────┼────────────┼────────────┼────────────┤"/utf8>>
    ),
    M1 = compute_all(Original, Small_noise),
    M2 = compute_all(Original, Medium_noise),
    M3 = compute_all(Original, Large_noise),
    gleam_stdlib:println(
        <<<<<<<<<<<<"│ MSE            │ "/utf8,
                                (pad_float(erlang:element(2, M1)))/binary>>/binary,
                            " │ "/utf8>>/binary,
                        (pad_float(erlang:element(2, M2)))/binary>>/binary,
                    " │ "/utf8>>/binary,
                (pad_float(erlang:element(2, M3)))/binary>>/binary,
            " │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<<<<<"│ MAE            │ "/utf8,
                                (pad_float(erlang:element(3, M1)))/binary>>/binary,
                            " │ "/utf8>>/binary,
                        (pad_float(erlang:element(3, M2)))/binary>>/binary,
                    " │ "/utf8>>/binary,
                (pad_float(erlang:element(3, M3)))/binary>>/binary,
            " │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<<<<<"│ RMSE           │ "/utf8,
                                (pad_float(erlang:element(4, M1)))/binary>>/binary,
                            " │ "/utf8>>/binary,
                        (pad_float(erlang:element(4, M2)))/binary>>/binary,
                    " │ "/utf8>>/binary,
                (pad_float(erlang:element(4, M3)))/binary>>/binary,
            " │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<<<<<"│ Cosine Sim     │ "/utf8,
                                (pad_float(erlang:element(5, M1)))/binary>>/binary,
                            " │ "/utf8>>/binary,
                        (pad_float(erlang:element(5, M2)))/binary>>/binary,
                    " │ "/utf8>>/binary,
                (pad_float(erlang:element(5, M3)))/binary>>/binary,
            " │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<<<<<"│ SNR (dB)       │ "/utf8,
                                (pad_float(erlang:element(6, M1)))/binary>>/binary,
                            " │ "/utf8>>/binary,
                        (pad_float(erlang:element(6, M2)))/binary>>/binary,
                    " │ "/utf8>>/binary,
                (pad_float(erlang:element(6, M3)))/binary>>/binary,
            " │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<<<<<"│ Max Error      │ "/utf8,
                                (pad_float(erlang:element(8, M1)))/binary>>/binary,
                            " │ "/utf8>>/binary,
                        (pad_float(erlang:element(8, M2)))/binary>>/binary,
                    " │ "/utf8>>/binary,
                (pad_float(erlang:element(8, M3)))/binary>>/binary,
            " │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<<<<<"│ P99 Error      │ "/utf8,
                                (pad_float(erlang:element(9, M1)))/binary>>/binary,
                            " │ "/utf8>>/binary,
                        (pad_float(erlang:element(9, M2)))/binary>>/binary,
                    " │ "/utf8>>/binary,
                (pad_float(erlang:element(9, M3)))/binary>>/binary,
            " │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"└────────────────┴────────────┴────────────┴────────────┘"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"Teórico SQNR:"/utf8>>),
    gleam_stdlib:println(
        <<<<"  INT8 (8 bits): "/utf8,
                (float_to_str(theoretical_sqnr(8)))/binary>>/binary,
            " dB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  INT4 (4 bits): "/utf8,
                (float_to_str(theoretical_sqnr(4)))/binary>>/binary,
            " dB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  INT2 (2 bits): "/utf8,
                (float_to_str(theoretical_sqnr(2)))/binary>>/binary,
            " dB"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>).

-file("src/viva_tensor/metrics.gleam", 335).
-spec main() -> nil.
main() ->
    benchmark_metrics().

-file("src/viva_tensor/metrics.gleam", 510).
-spec get_at(list(GNJ), integer()) -> {ok, GNJ} | {error, nil}.
get_at(List, Index) ->
    _pipe = List,
    _pipe@1 = gleam@list:drop(_pipe, Index),
    gleam@list:first(_pipe@1).

-file("src/viva_tensor/metrics.gleam", 516).
-spec result_or({ok, GNN} | {error, any()}, GNN) -> GNN.
result_or(R, Default) ->
    case R of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-file("src/viva_tensor/metrics.gleam", 523).
-spec pad_or_truncate(list(GNR), integer(), GNR) -> list(GNR).
pad_or_truncate(Lst, Target_len, Default) ->
    Current_len = erlang:length(Lst),
    case Current_len >= Target_len of
        true ->
            gleam@list:take(Lst, Target_len);

        false ->
            _pipe = Lst,
            lists:append(
                _pipe,
                gleam@list:repeat(Default, Target_len - Current_len)
            )
    end.

-file("src/viva_tensor/metrics.gleam", 264).
?DOC(
    " Computa saliência de pesos baseado em ativações\n"
    " Salience(w) = Var(activation) * w²\n"
).
-spec compute_saliency(viva_tensor@tensor:tensor(), list(list(float()))) -> list(float()).
compute_saliency(Weights, Activations) ->
    W_data = viva_tensor@tensor:to_list(Weights),
    Activation_vars = case Activations of
        [] ->
            gleam@list:repeat(1.0, erlang:length(W_data));

        [First | _] ->
            N_channels = erlang:length(First),
            N_samples = erlang:float(erlang:length(Activations)),
            Means = begin
                _pipe = gleam@list:repeat(+0.0, N_channels),
                _pipe@1 = gleam@list:index_fold(
                    Activations,
                    _pipe,
                    fun(Acc, Acts, _) ->
                        gleam@list:map2(Acc, Acts, fun(A, Act) -> A + Act end)
                    end
                ),
                gleam@list:map(_pipe@1, fun(S) -> case N_samples of
                            +0.0 -> +0.0;
                            -0.0 -> -0.0;
                            Gleam@denominator -> S / Gleam@denominator
                        end end)
            end,
            _pipe@4 = gleam@list:index_fold(
                Activations,
                gleam@list:repeat(+0.0, N_channels),
                fun(Acc@1, Acts@1, _) ->
                    gleam@list:index_map(
                        Acc@1,
                        fun(A@1, I) ->
                            Mean = begin
                                _pipe@2 = get_at(Means, I),
                                result_or(_pipe@2, +0.0)
                            end,
                            Act@1 = begin
                                _pipe@3 = get_at(Acts@1, I),
                                result_or(_pipe@3, +0.0)
                            end,
                            Diff = Act@1 - Mean,
                            A@1 + (Diff * Diff)
                        end
                    )
                end
            ),
            gleam@list:map(_pipe@4, fun(V) -> case N_samples of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator@1 -> V / Gleam@denominator@1
                    end end)
    end,
    Padded_vars = pad_or_truncate(Activation_vars, erlang:length(W_data), 1.0),
    gleam@list:map2(Padded_vars, W_data, fun(Var, W) -> (Var * W) * W end).
