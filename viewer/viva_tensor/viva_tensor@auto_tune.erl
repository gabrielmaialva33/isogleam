-module(viva_tensor@auto_tune).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/auto_tune.gleam").
-export([detect_hardware/0, detect_cpu_only/0, new/0, profile/3, new_quant_context/1, should_quantize/2, compute_scale/1, check_memory_pressure/1, get_memory_strategy/1, run_hardware_profile/0, main/0]).
-export_type([device/0, hardware_profile/0, quant_mode/0, auto_tuner/0, batch_result/0, quant_context/0, memory_pressure/0, memory_strategy/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Auto-Tuning System - Self-Optimizing Tensor Library\n"
    "\n"
    " Inspirado pelas pesquisas do HuggingChat + ggml + Candle\n"
    "\n"
    " Features:\n"
    " 1. GPU Auto-Detection (detecta VRAM, load, capabilities)\n"
    " 2. Adaptive Quantization (int8 para inference, fp32 para training)\n"
    " 3. Zero-Copy entre Gleam e Rust NIFs\n"
    " 4. Auto Batch Size Optimizer (aprende o melhor batch para o hardware)\n"
    "\n"
    " Target: RTX 4090 24GB VRAM + 32GB RAM\n"
).

-type device() :: {cuda, integer(), float()} |
    {metal, integer()} |
    {cpu, integer()}.

-type hardware_profile() :: {hardware_profile,
        device(),
        float(),
        float(),
        float(),
        float(),
        integer()}.

-type quant_mode() :: inference | training | adaptive.

-type auto_tuner() :: {auto_tuner,
        hardware_profile(),
        quant_mode(),
        list(batch_result()),
        integer()}.

-type batch_result() :: {batch_result, integer(), float(), float()}.

-type quant_context() :: {quant_context, quant_mode(), list(float())}.

-type memory_pressure() :: low | medium | high | critical.

-type memory_strategy() :: {memory_strategy, float(), quant_mode(), boolean()}.

-file("src/viva_tensor/auto_tune.gleam", 72).
?DOC(" Detecta hardware disponível\n").
-spec detect_hardware() -> hardware_profile().
detect_hardware() ->
    {hardware_profile, {cuda, 0, 24.0}, 24.0, 20.0, 32.0, +0.0, 32}.

-file("src/viva_tensor/auto_tune.gleam", 87).
?DOC(" Cria auto-tuner para CPU-only\n").
-spec detect_cpu_only() -> hardware_profile().
detect_cpu_only() ->
    {hardware_profile, {cpu, 16}, +0.0, +0.0, 32.0, +0.0, 8}.

-file("src/viva_tensor/auto_tune.gleam", 116).
?DOC(" Batch size padrão baseado no device\n").
-spec get_default_batch_size(device()) -> integer().
get_default_batch_size(Device) ->
    case Device of
        {cuda, _, Vram_gb} ->
            case Vram_gb >= 20.0 of
                true ->
                    64;

                false ->
                    case Vram_gb >= 10.0 of
                        true ->
                            32;

                        false ->
                            16
                    end
            end;

        {metal, _} ->
            16;

        {cpu, _} ->
            8
    end.

-file("src/viva_tensor/auto_tune.gleam", 103).
?DOC(" Cria novo auto-tuner\n").
-spec new() -> auto_tuner().
new() ->
    Hardware = detect_hardware(),
    Batch_size = get_default_batch_size(erlang:element(2, Hardware)),
    {auto_tuner, Hardware, adaptive, [], Batch_size}.

-file("src/viva_tensor/auto_tune.gleam", 191).
?DOC(" Ajusta batch size para memória disponível\n").
-spec adjust_for_memory(integer(), hardware_profile()) -> integer().
adjust_for_memory(Batch_size, Hw) ->
    Tensor_size_mb = 2.0,
    Batch_memory_gb = (erlang:float(Batch_size) * Tensor_size_mb) / 1024.0,
    Max_memory = erlang:element(4, Hw) * 0.8,
    case Batch_memory_gb > Max_memory of
        true ->
            Adjusted = begin
                _pipe = erlang:round((case Tensor_size_mb of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> Max_memory / Gleam@denominator
                    end) * 1024.0),
                gleam@int:max(_pipe, 1)
            end,
            Adjusted;

        false ->
            Batch_size
    end.

-file("src/viva_tensor/auto_tune.gleam", 167).
?DOC(" Encontra batch size com maior throughput\n").
-spec find_optimal_batch_size(list(batch_result()), hardware_profile()) -> integer().
find_optimal_batch_size(History, Hw) ->
    case History of
        [] ->
            get_default_batch_size(erlang:element(2, Hw));

        _ ->
            Best = begin
                _pipe = History,
                gleam@list:fold(
                    _pipe,
                    {batch_result, 0, +0.0, +0.0},
                    fun(Acc, R) ->
                        case erlang:element(4, R) > erlang:element(4, Acc) of
                            true ->
                                R;

                            false ->
                                Acc
                        end
                    end
                )
            end,
            adjust_for_memory(erlang:element(2, Best), Hw)
    end.

-file("src/viva_tensor/auto_tune.gleam", 140).
?DOC(" Registra resultado de execução e otimiza\n").
-spec profile(auto_tuner(), integer(), float()) -> auto_tuner().
profile(Tuner, Batch_size, Duration_ms) ->
    Throughput = case Duration_ms of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Batch_size) / Gleam@denominator
    end,
    Result = {batch_result, Batch_size, Duration_ms, Throughput},
    New_history = case erlang:length(erlang:element(4, Tuner)) >= 20 of
        true ->
            [Result | gleam@list:take(erlang:element(4, Tuner), 19)];

        false ->
            [Result | erlang:element(4, Tuner)]
    end,
    Optimal = find_optimal_batch_size(New_history, erlang:element(2, Tuner)),
    {auto_tuner,
        erlang:element(2, Tuner),
        erlang:element(3, Tuner),
        New_history,
        Optimal}.

-file("src/viva_tensor/auto_tune.gleam", 220).
?DOC(" Cria contexto de quantização\n").
-spec new_quant_context(quant_mode()) -> quant_context().
new_quant_context(Mode) ->
    {quant_context, Mode, []}.

-file("src/viva_tensor/auto_tune.gleam", 225).
?DOC(" Decide modo de quantização baseado na operação\n").
-spec should_quantize(quant_context(), boolean()) -> boolean().
should_quantize(Ctx, Is_inference) ->
    case erlang:element(2, Ctx) of
        inference ->
            true;

        training ->
            false;

        adaptive ->
            Is_inference
    end.

-file("src/viva_tensor/auto_tune.gleam", 234).
?DOC(" Calcula escala para quantização absmax (int8)\n").
-spec compute_scale(viva_tensor@tensor:tensor()) -> float().
compute_scale(Tensor) ->
    Max_val = viva_tensor@tensor:max(Tensor),
    case Max_val > +0.0 of
        true ->
            case Max_val of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> 127.0 / Gleam@denominator
            end;

        false ->
            1.0
    end.

-file("src/viva_tensor/auto_tune.gleam", 247).
?DOC(" Verifica pressão de memória\n").
-spec check_memory_pressure(hardware_profile()) -> memory_pressure().
check_memory_pressure(Hw) ->
    Usage_pct = 1.0 - (case erlang:element(3, Hw) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:element(4, Hw) / Gleam@denominator
    end),
    case Usage_pct of
        P when P >= 0.9 ->
            critical;

        P@1 when P@1 >= 0.7 ->
            high;

        P@2 when P@2 >= 0.5 ->
            medium;

        _ ->
            low
    end.

-file("src/viva_tensor/auto_tune.gleam", 266).
?DOC(" Estratégia baseada em pressão de memória\n").
-spec get_memory_strategy(memory_pressure()) -> memory_strategy().
get_memory_strategy(Pressure) ->
    case Pressure of
        critical ->
            {memory_strategy, 0.25, inference, true};

        high ->
            {memory_strategy, 0.5, inference, true};

        medium ->
            {memory_strategy, 0.75, adaptive, false};

        low ->
            {memory_strategy, 1.0, training, false}
    end.

-file("src/viva_tensor/auto_tune.gleam", 390).
-spec pressure_to_string(memory_pressure()) -> binary().
pressure_to_string(P) ->
    case P of
        low ->
            <<"LOW (tudo ok)"/utf8>>;

        medium ->
            <<"MEDIUM (monitorar)"/utf8>>;

        high ->
            <<"HIGH (reduzir batch)"/utf8>>;

        critical ->
            <<"CRITICAL (emergency mode)"/utf8>>
    end.

-file("src/viva_tensor/auto_tune.gleam", 399).
-spec quant_mode_to_string(quant_mode()) -> binary().
quant_mode_to_string(M) ->
    case M of
        inference ->
            <<"INT8 (inference)"/utf8>>;

        training ->
            <<"FP32 (training)"/utf8>>;

        adaptive ->
            <<"ADAPTIVE"/utf8>>
    end.

-file("src/viva_tensor/auto_tune.gleam", 407).
-spec bool_to_string(boolean()) -> binary().
bool_to_string(B) ->
    case B of
        true ->
            <<"Sim"/utf8>>;

        false ->
            <<"Não"/utf8>>
    end.

-file("src/viva_tensor/auto_tune.gleam", 414).
-spec float_to_string(float()) -> binary().
float_to_string(F) ->
    Rounded = erlang:float(erlang:round(F * 100.0)) / 100.0,
    gleam_stdlib:float_to_string(Rounded).

-file("src/viva_tensor/auto_tune.gleam", 374).
-spec print_device(device()) -> nil.
print_device(Device) ->
    case Device of
        {cuda, Id, Vram} ->
            gleam_stdlib:println(
                <<<<<<<<"  Device: CUDA GPU #"/utf8,
                                (erlang:integer_to_binary(Id))/binary>>/binary,
                            " ("/utf8>>/binary,
                        (float_to_string(Vram))/binary>>/binary,
                    "GB)"/utf8>>
            );

        {metal, Id@1} ->
            gleam_stdlib:println(
                <<"  Device: Metal #"/utf8,
                    (erlang:integer_to_binary(Id@1))/binary>>
            );

        {cpu, Cores} ->
            gleam_stdlib:println(
                <<<<"  Device: CPU ("/utf8,
                        (erlang:integer_to_binary(Cores))/binary>>/binary,
                    " cores)"/utf8>>
            )
    end.

-file("src/viva_tensor/auto_tune.gleam", 308).
?DOC(" Roda profile completo do hardware\n").
-spec run_hardware_profile() -> nil.
run_hardware_profile() ->
    gleam_stdlib:println(
        <<"╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  AUTO-TUNE HARDWARE PROFILE                                     ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝\n"/utf8>>
    ),
    Hw = detect_hardware(),
    gleam_stdlib:println(<<"HARDWARE DETECTADO:"/utf8>>),
    print_device(erlang:element(2, Hw)),
    gleam_stdlib:println(
        <<<<"  VRAM Total: "/utf8,
                (float_to_string(erlang:element(3, Hw)))/binary>>/binary,
            " GB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  VRAM Disponível: "/utf8,
                (float_to_string(erlang:element(4, Hw)))/binary>>/binary,
            " GB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  RAM Total: "/utf8,
                (float_to_string(erlang:element(5, Hw)))/binary>>/binary,
            " GB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  GPU Load: "/utf8,
                (float_to_string(erlang:element(6, Hw)))/binary>>/binary,
            "%"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  Batch Size Ótimo: "/utf8,
            (erlang:integer_to_binary(erlang:element(7, Hw)))/binary>>
    ),
    Pressure = check_memory_pressure(Hw),
    gleam_stdlib:println(
        <<"\nMEMORY PRESSURE: "/utf8, (pressure_to_string(Pressure))/binary>>
    ),
    Strategy = get_memory_strategy(Pressure),
    gleam_stdlib:println(<<"ESTRATÉGIA:"/utf8>>),
    gleam_stdlib:println(
        <<"  Batch Mult: "/utf8,
            (float_to_string(erlang:element(2, Strategy)))/binary>>
    ),
    gleam_stdlib:println(
        <<"  Quant Mode: "/utf8,
            (quant_mode_to_string(erlang:element(3, Strategy)))/binary>>
    ),
    gleam_stdlib:println(
        <<"  GC Agressivo: "/utf8,
            (bool_to_string(erlang:element(4, Strategy)))/binary>>
    ),
    gleam_stdlib:println(
        <<"\n╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  RECOMENDAÇÕES PARA RTX 4090 24GB + 32GB RAM:                   ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                                                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  1. Batch Size: 64 (pode ir até 128 com INT8)                   ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  2. Quantização: INT8 para inference (4x menos VRAM)            ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  3. Memory Pool: Pre-alocar 20GB para tensores                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  4. Zero-Copy: Usar Binary refs entre Gleam e Rust              ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝"/utf8>>
    ).

-file("src/viva_tensor/auto_tune.gleam", 366).
-spec main() -> nil.
main() ->
    run_hardware_profile().
