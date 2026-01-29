-module(viva_tensor@benchmark_full).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/benchmark_full.gleam").
-export([run_full_benchmark/0, main/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Benchmark Completo com Métricas Avançadas\n"
    "\n"
    " Compara SQNR real vs teórico para INT8, NF4, AWQ\n"
    " Baseado em papers 2024-2026: NVFP4, TWEO, FALQON\n"
).

-file("src/viva_tensor/benchmark_full.gleam", 453).
-spec generate_calibration_data(integer(), integer()) -> list(list(float())).
generate_calibration_data(Num_samples, Features) ->
    _pipe = gleam@list:range(1, Num_samples),
    gleam@list:map(
        _pipe,
        fun(_) -> _pipe@1 = viva_tensor@tensor:random_uniform([Features]),
            viva_tensor@tensor:to_list(_pipe@1) end
    ).

-file("src/viva_tensor/benchmark_full.gleam", 464).
-spec float_to_str(float()) -> binary().
float_to_str(F) ->
    Rounded = erlang:float(erlang:round(F * 100.0)) / 100.0,
    gleam_stdlib:float_to_string(Rounded).

-file("src/viva_tensor/benchmark_full.gleam", 469).
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

-file("src/viva_tensor/benchmark_full.gleam", 81).
-spec benchmark_int8(viva_tensor@tensor:tensor(), integer()) -> nil.
benchmark_int8(Original, Original_bytes) ->
    gleam_stdlib:println(
        <<"┌─────────────────────────────────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"│ INT8 QUANTIZATION - Symmetric Per-Tensor                               │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"└─────────────────────────────────────────────────────────────────────────┘"/utf8>>
    ),
    Quantized = viva_tensor@compression:quantize_int8(Original),
    Recovered = viva_tensor@compression:dequantize(Quantized),
    Quant_metrics = viva_tensor@metrics:compute_all(Original, Recovered),
    Theoretical_sqnr = viva_tensor@metrics:theoretical_sqnr(8),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"  ┌─────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  │ MÉTRICAS DE QUALIDADE                       │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  ├─────────────────────────────────────────────┤"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ MSE:              "/utf8,
                (pad_float(erlang:element(2, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ MAE:              "/utf8,
                (pad_float(erlang:element(3, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ RMSE:             "/utf8,
                (pad_float(erlang:element(4, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ Cosine Sim:       "/utf8,
                (pad_float(erlang:element(5, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ Max Error:        "/utf8,
                (pad_float(erlang:element(8, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ P99 Error:        "/utf8,
                (pad_float(erlang:element(9, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  ├─────────────────────────────────────────────┤"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ SNR (Real):       "/utf8,
                (pad_float(erlang:element(6, Quant_metrics)))/binary>>/binary,
            " dB       │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ SQNR (Teórico):   "/utf8,
                (pad_float(Theoretical_sqnr))/binary>>/binary,
            " dB       │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ Gap:              "/utf8,
                (pad_float(Theoretical_sqnr - erlang:element(6, Quant_metrics)))/binary>>/binary,
            " dB       │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  └─────────────────────────────────────────────┘"/utf8>>
    ),
    Ratio = case erlang:float(erlang:element(5, Quantized)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Original_bytes) / Gleam@denominator
    end,
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<<<"  Compressão: "/utf8, (float_to_str(Ratio))/binary>>/binary,
            "x"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Memória: "/utf8,
                (erlang:integer_to_binary(erlang:element(5, Quantized) div 1024))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>).

-file("src/viva_tensor/benchmark_full.gleam", 157).
-spec benchmark_nf4_full(viva_tensor@tensor:tensor(), integer()) -> nil.
benchmark_nf4_full(Original, Original_bytes) ->
    gleam_stdlib:println(
        <<"┌─────────────────────────────────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"│ NF4 QUANTIZATION - QLoRA Style (Normal Distribution Quantiles)         │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"└─────────────────────────────────────────────────────────────────────────┘"/utf8>>
    ),
    Config = viva_tensor@nf4:default_config(),
    Quantized = viva_tensor@nf4:quantize(Original, Config),
    Recovered = viva_tensor@nf4:dequantize(Quantized),
    Quant_metrics = viva_tensor@metrics:compute_all(Original, Recovered),
    Theoretical_sqnr = viva_tensor@metrics:theoretical_sqnr(4),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"  ┌─────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  │ MÉTRICAS DE QUALIDADE                       │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  ├─────────────────────────────────────────────┤"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ MSE:              "/utf8,
                (pad_float(erlang:element(2, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ MAE:              "/utf8,
                (pad_float(erlang:element(3, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ RMSE:             "/utf8,
                (pad_float(erlang:element(4, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ Cosine Sim:       "/utf8,
                (pad_float(erlang:element(5, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ Max Error:        "/utf8,
                (pad_float(erlang:element(8, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ P99 Error:        "/utf8,
                (pad_float(erlang:element(9, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  ├─────────────────────────────────────────────┤"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ SNR (Real):       "/utf8,
                (pad_float(erlang:element(6, Quant_metrics)))/binary>>/binary,
            " dB       │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ SQNR (Teórico):   "/utf8,
                (pad_float(Theoretical_sqnr))/binary>>/binary,
            " dB       │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ Gap:              "/utf8,
                (pad_float(Theoretical_sqnr - erlang:element(6, Quant_metrics)))/binary>>/binary,
            " dB       │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  └─────────────────────────────────────────────┘"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<<<"  Compressão: "/utf8,
                (float_to_str(erlang:element(6, Quantized)))/binary>>/binary,
            "x"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Memória: "/utf8,
                (erlang:integer_to_binary(erlang:element(5, Quantized) div 1024))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"  ─── Double Quantization (NF4 + DQ) ───"/utf8>>),
    Dq = viva_tensor@nf4:double_quantize(Original, Config),
    Dq_ratio = case erlang:float(erlang:element(7, Dq)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Original_bytes) / Gleam@denominator
    end,
    gleam_stdlib:println(
        <<<<"  Compressão DQ: "/utf8, (float_to_str(Dq_ratio))/binary>>/binary,
            "x"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Memória DQ: "/utf8,
                (erlang:integer_to_binary(erlang:element(7, Dq) div 1024))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>).

-file("src/viva_tensor/benchmark_full.gleam", 242).
-spec benchmark_awq_full(viva_tensor@tensor:tensor(), integer()) -> nil.
benchmark_awq_full(Original, Original_bytes) ->
    gleam_stdlib:println(
        <<"┌─────────────────────────────────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"│ AWQ QUANTIZATION - Activation-aware (MLSys 2024 Best Paper)            │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"└─────────────────────────────────────────────────────────────────────────┘"/utf8>>
    ),
    Calibration_data = generate_calibration_data(64, 512),
    Config = viva_tensor@awq:default_config(),
    Quantized = viva_tensor@awq:quantize_awq(Original, Calibration_data, Config),
    Recovered = viva_tensor@awq:dequantize_awq(Quantized),
    Quant_metrics = viva_tensor@metrics:compute_all(Original, Recovered),
    Theoretical_sqnr = viva_tensor@metrics:theoretical_sqnr(4),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"  ┌─────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  │ MÉTRICAS DE QUALIDADE                       │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  ├─────────────────────────────────────────────┤"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ MSE:              "/utf8,
                (pad_float(erlang:element(2, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ MAE:              "/utf8,
                (pad_float(erlang:element(3, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ RMSE:             "/utf8,
                (pad_float(erlang:element(4, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ Cosine Sim:       "/utf8,
                (pad_float(erlang:element(5, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ Max Error:        "/utf8,
                (pad_float(erlang:element(8, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ P99 Error:        "/utf8,
                (pad_float(erlang:element(9, Quant_metrics)))/binary>>/binary,
            "          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  ├─────────────────────────────────────────────┤"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ SNR (Real):       "/utf8,
                (pad_float(erlang:element(6, Quant_metrics)))/binary>>/binary,
            " dB       │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ SQNR (Teórico):   "/utf8,
                (pad_float(Theoretical_sqnr))/binary>>/binary,
            " dB       │"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  │ Gap:              "/utf8,
                (pad_float(Theoretical_sqnr - erlang:element(6, Quant_metrics)))/binary>>/binary,
            " dB       │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  └─────────────────────────────────────────────┘"/utf8>>
    ),
    Ratio = case erlang:float(erlang:element(7, Quantized)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Original_bytes) / Gleam@denominator
    end,
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<<<"  Compressão: "/utf8, (float_to_str(Ratio))/binary>>/binary,
            "x"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Memória: "/utf8,
                (erlang:integer_to_binary(erlang:element(7, Quantized) div 1024))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"  ─── Análise de Saliência (AWQ Insight) ───"/utf8>>
    ),
    Activation_stats = viva_tensor@awq:collect_activation_stats(
        Calibration_data
    ),
    Salient = viva_tensor@awq:identify_salient_channels(Activation_stats, 1.0),
    gleam_stdlib:println(
        <<"  Canais salientes (top 1%): "/utf8,
            (erlang:integer_to_binary(erlang:length(Salient)))/binary>>
    ),
    gleam_stdlib:println(<<"  Esses ~1% dominam o erro de quantização!"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>).

-file("src/viva_tensor/benchmark_full.gleam", 479).
-spec pad_ratio(float()) -> binary().
pad_ratio(F) ->
    S = float_to_str(F),
    Len = string:length(S),
    case Len < 4 of
        true ->
            <<(gleam@string:repeat(<<" "/utf8>>, 4 - Len))/binary, S/binary>>;

        false ->
            S
    end.

-file("src/viva_tensor/benchmark_full.gleam", 488).
-spec pad_snr(float()) -> binary().
pad_snr(F) ->
    S = float_to_str(F),
    Len = string:length(S),
    case Len < 6 of
        true ->
            <<(gleam@string:repeat(<<" "/utf8>>, 6 - Len))/binary, S/binary>>;

        false ->
            gleam@string:slice(S, 0, 6)
    end.

-file("src/viva_tensor/benchmark_full.gleam", 497).
-spec pad_gap(float()) -> binary().
pad_gap(F) ->
    S = float_to_str(F),
    Len = string:length(S),
    case Len < 6 of
        true ->
            <<(gleam@string:repeat(<<" "/utf8>>, 6 - Len))/binary, S/binary>>;

        false ->
            gleam@string:slice(S, 0, 6)
    end.

-file("src/viva_tensor/benchmark_full.gleam", 506).
-spec pad_cos(float()) -> binary().
pad_cos(F) ->
    S = float_to_str(F),
    Len = string:length(S),
    case Len < 6 of
        true ->
            <<S/binary, (gleam@string:repeat(<<" "/utf8>>, 6 - Len))/binary>>;

        false ->
            gleam@string:slice(S, 0, 6)
    end.

-file("src/viva_tensor/benchmark_full.gleam", 332).
-spec print_comparison_table(viva_tensor@tensor:tensor(), integer()) -> nil.
print_comparison_table(Original, Original_bytes) ->
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"╔═══════════════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                    TABELA COMPARATIVA FINAL                               ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚═══════════════════════════════════════════════════════════════════════════╝"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    Int8_q = viva_tensor@compression:quantize_int8(Original),
    Int8_r = viva_tensor@compression:dequantize(Int8_q),
    Int8_m = viva_tensor@metrics:compute_all(Original, Int8_r),
    Nf4_config = viva_tensor@nf4:default_config(),
    Nf4_q = viva_tensor@nf4:quantize(Original, Nf4_config),
    Nf4_r = viva_tensor@nf4:dequantize(Nf4_q),
    Nf4_m = viva_tensor@metrics:compute_all(Original, Nf4_r),
    Calib = generate_calibration_data(64, 512),
    Awq_config = viva_tensor@awq:default_config(),
    Awq_q = viva_tensor@awq:quantize_awq(Original, Calib, Awq_config),
    Awq_r = viva_tensor@awq:dequantize_awq(Awq_q),
    Awq_m = viva_tensor@metrics:compute_all(Original, Awq_r),
    Sqnr_8 = viva_tensor@metrics:theoretical_sqnr(8),
    Sqnr_4 = viva_tensor@metrics:theoretical_sqnr(4),
    Int8_ratio = case erlang:float(erlang:element(5, Int8_q)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Original_bytes) / Gleam@denominator
    end,
    Nf4_ratio = erlang:element(6, Nf4_q),
    Awq_ratio = case erlang:float(erlang:element(7, Awq_q)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator@1 -> erlang:float(Original_bytes) / Gleam@denominator@1
    end,
    gleam_stdlib:println(
        <<"┌────────────┬───────────┬──────────┬───────────┬───────────┬──────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"│ Método     │ Compres.  │ SNR Real │ SNR Teór. │ Gap       │ Cosine   │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"├────────────┼───────────┼──────────┼───────────┼───────────┼──────────┤"/utf8>>
    ),
    Int8_gap = Sqnr_8 - erlang:element(6, Int8_m),
    gleam_stdlib:println(
        <<<<<<<<<<<<<<<<<<<<"│ INT8       │ "/utf8,
                                                (pad_ratio(Int8_ratio))/binary>>/binary,
                                            "x   │ "/utf8>>/binary,
                                        (pad_snr(erlang:element(6, Int8_m)))/binary>>/binary,
                                    " │ "/utf8>>/binary,
                                (pad_snr(Sqnr_8))/binary>>/binary,
                            "  │ "/utf8>>/binary,
                        (pad_gap(Int8_gap))/binary>>/binary,
                    "  │ "/utf8>>/binary,
                (pad_cos(erlang:element(5, Int8_m)))/binary>>/binary,
            " │"/utf8>>
    ),
    Nf4_gap = Sqnr_4 - erlang:element(6, Nf4_m),
    gleam_stdlib:println(
        <<<<<<<<<<<<<<<<<<<<"│ NF4        │ "/utf8,
                                                (pad_ratio(Nf4_ratio))/binary>>/binary,
                                            "x   │ "/utf8>>/binary,
                                        (pad_snr(erlang:element(6, Nf4_m)))/binary>>/binary,
                                    " │ "/utf8>>/binary,
                                (pad_snr(Sqnr_4))/binary>>/binary,
                            "  │ "/utf8>>/binary,
                        (pad_gap(Nf4_gap))/binary>>/binary,
                    "  │ "/utf8>>/binary,
                (pad_cos(erlang:element(5, Nf4_m)))/binary>>/binary,
            " │"/utf8>>
    ),
    Awq_gap = Sqnr_4 - erlang:element(6, Awq_m),
    gleam_stdlib:println(
        <<<<<<<<<<<<<<<<<<<<"│ AWQ        │ "/utf8,
                                                (pad_ratio(Awq_ratio))/binary>>/binary,
                                            "x   │ "/utf8>>/binary,
                                        (pad_snr(erlang:element(6, Awq_m)))/binary>>/binary,
                                    " │ "/utf8>>/binary,
                                (pad_snr(Sqnr_4))/binary>>/binary,
                            "  │ "/utf8>>/binary,
                        (pad_gap(Awq_gap))/binary>>/binary,
                    "  │ "/utf8>>/binary,
                (pad_cos(erlang:element(5, Awq_m)))/binary>>/binary,
            " │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"└────────────┴───────────┴──────────┴───────────┴───────────┴──────────┘"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"LEGENDA:"/utf8>>),
    gleam_stdlib:println(
        <<"  - Compres.: Taxa de compressão (maior = melhor)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - SNR Real: Signal-to-Noise Ratio medido (maior = melhor)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - SNR Teór.: SQNR teórico = 6.02*N + 1.76 dB"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - Gap: Diferença entre teórico e real (menor = melhor)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - Cosine: Similaridade cosseno (1.0 = perfeito)"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"INSIGHTS:"/utf8>>),
    gleam_stdlib:println(
        <<"  - INT8: Gap pequeno = quantização eficiente"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - NF4: Melhor que Q4 uniforme por usar quantis normais"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  - AWQ: Foca em canais salientes para minimizar erro"/utf8>>
    ).

-file("src/viva_tensor/benchmark_full.gleam", 26).
-spec run_full_benchmark() -> nil.
run_full_benchmark() ->
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"╔═══════════════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║          viva_tensor - BENCHMARK COMPLETO COM MÉTRICAS AVANÇADAS          ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                   SQNR Real vs Teórico | Papers 2024-2026                 ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚═══════════════════════════════════════════════════════════════════════════╝"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    Test_tensor = viva_tensor@tensor:random_normal([512, 512], +0.0, 0.3),
    Original_bytes = (512 * 512) * 4,
    gleam_stdlib:println(<<"━━━ CONFIGURAÇÃO DO TESTE ━━━"/utf8>>),
    gleam_stdlib:println(<<"  Tensor: [512, 512] = 262,144 elementos"/utf8>>),
    gleam_stdlib:println(
        <<"  Distribuição: Normal(0, 0.3) - típico de pesos NN"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Original: "/utf8,
                (erlang:integer_to_binary(Original_bytes div 1024))/binary>>/binary,
            " KB (FP32)"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    benchmark_int8(Test_tensor, Original_bytes),
    benchmark_nf4_full(Test_tensor, Original_bytes),
    benchmark_awq_full(Test_tensor, Original_bytes),
    print_comparison_table(Test_tensor, Original_bytes),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"═══════════════════════════════════════════════════════════════════════════"/utf8>>
    ),
    gleam_stdlib:println(
        <<"                        BENCHMARK CONCLUÍDO!"/utf8>>
    ),
    gleam_stdlib:println(
        <<"═══════════════════════════════════════════════════════════════════════════"/utf8>>
    ).

-file("src/viva_tensor/benchmark_full.gleam", 22).
-spec main() -> nil.
main() ->
    run_full_benchmark().
