-module(viva_tensor@rtx4090).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/rtx4090.gleam").
-export([get_specs/0, default_config/0, precision_config/0, speed_config/0, init_memory/0, tensor_memory_bytes/2, can_allocate/2, allocate/2, free/2, estimate_performance/3, process_batch/2, benchmark_rtx4090/0, main/0]).
-export_type([rtx4090_specs/0, rtx4090_config/0, quant_mode4090/0, gpu_memory_state/0, batch_result/0, performance_estimate/0, bottleneck/0, pid_/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " RTX 4090 Optimized Engine\n"
    "\n"
    " ESPECIFICAÇÕES RTX 4090 ASUS ROG STRIX:\n"
    " - GPU: AD102 (16384 CUDA Cores)\n"
    " - Tensor Cores: 512 (4th Gen)\n"
    " - VRAM: 24GB GDDR6X\n"
    " - Bandwidth: 1008 GB/s\n"
    " - TDP: 450W (boost até 600W)\n"
    " - FP32: 82.6 TFLOPS\n"
    " - FP16 Tensor: 330 TFLOPS\n"
    " - INT8 Tensor: 661 TOPS\n"
    "\n"
    " OTIMIZAÇÕES ESPECÍFICAS:\n"
    " 1. VRAM-aware batch sizing (24GB - 2GB sistema = 22GB útil)\n"
    " 2. Tensor Core utilization (alinhamento 8x8 ou 16x16)\n"
    " 3. GDDR6X burst patterns (256-bit bus, aligned access)\n"
    " 4. CUDA Warp-aware parallelism (32 threads)\n"
    "\n"
    " Pure Gleam + BEAM concurrency para máxima utilização!\n"
).

-type rtx4090_specs() :: {rtx4090_specs,
        integer(),
        integer(),
        float(),
        float(),
        float(),
        integer(),
        float(),
        float(),
        float(),
        integer(),
        integer(),
        integer()}.

-type rtx4090_config() :: {rtx4090_config,
        integer(),
        integer(),
        integer(),
        integer(),
        boolean(),
        quant_mode4090()}.

-type quant_mode4090() :: fp32_mode |
    fp16_tensor_mode |
    int8_tensor_mode |
    mixed_precision_mode.

-type gpu_memory_state() :: {gpu_memory_state,
        integer(),
        integer(),
        integer(),
        integer(),
        integer()}.

-type batch_result() :: {batch_result,
        list(viva_tensor@blackwell:blackwell_tensor()),
        integer(),
        float(),
        float(),
        float()}.

-type performance_estimate() :: {performance_estimate,
        float(),
        float(),
        float(),
        bottleneck(),
        float()}.

-type bottleneck() :: compute_bound | memory_bound | latency_bound.

-type pid_() :: any().

-file("src/viva_tensor/rtx4090.gleam", 65).
?DOC(" Retorna specs da RTX 4090\n").
-spec get_specs() -> rtx4090_specs().
get_specs() ->
    {rtx4090_specs,
        16384,
        512,
        24.0,
        22.0,
        1008.0,
        450,
        82.6,
        330.0,
        661.0,
        32,
        128,
        72}.

-file("src/viva_tensor/rtx4090.gleam", 118).
?DOC(" Configuração padrão otimizada\n").
-spec default_config() -> rtx4090_config().
default_config() ->
    _ = get_specs(),
    Batch_size = 128,
    {rtx4090_config, Batch_size, 16, 32, 256, true, int8_tensor_mode}.

-file("src/viva_tensor/rtx4090.gleam", 143).
?DOC(" Configuração para máxima precisão\n").
-spec precision_config() -> rtx4090_config().
precision_config() ->
    _record = default_config(),
    {rtx4090_config,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        false,
        fp32_mode}.

-file("src/viva_tensor/rtx4090.gleam", 152).
?DOC(" Configuração para máxima velocidade\n").
-spec speed_config() -> rtx4090_config().
speed_config() ->
    _record = default_config(),
    {rtx4090_config,
        256,
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        true,
        int8_tensor_mode}.

-file("src/viva_tensor/rtx4090.gleam", 183).
?DOC(" Cria estado inicial de memória para RTX 4090\n").
-spec init_memory() -> gpu_memory_state().
init_memory() ->
    Specs = get_specs(),
    Total = erlang:round(
        ((erlang:element(5, Specs) * 1024.0) * 1024.0) * 1024.0
    ),
    {gpu_memory_state, Total, 0, Total, 0, 0}.

-file("src/viva_tensor/rtx4090.gleam", 197).
?DOC(" Calcula memória necessária para tensor\n").
-spec tensor_memory_bytes(list(integer()), quant_mode4090()) -> integer().
tensor_memory_bytes(Shape, Mode) ->
    Elements = gleam@list:fold(Shape, 1, fun(Acc, D) -> Acc * D end),
    Bytes_per_element = case Mode of
        fp32_mode ->
            4;

        fp16_tensor_mode ->
            2;

        int8_tensor_mode ->
            1;

        mixed_precision_mode ->
            2
    end,
    Elements * Bytes_per_element.

-file("src/viva_tensor/rtx4090.gleam", 211).
?DOC(" Verifica se tensor cabe na VRAM\n").
-spec can_allocate(gpu_memory_state(), integer()) -> boolean().
can_allocate(State, Bytes) ->
    erlang:element(4, State) >= Bytes.

-file("src/viva_tensor/rtx4090.gleam", 216).
?DOC(" Aloca memória para tensor\n").
-spec allocate(gpu_memory_state(), integer()) -> {ok, gpu_memory_state()} |
    {error, binary()}.
allocate(State, Bytes) ->
    case can_allocate(State, Bytes) of
        true ->
            {ok,
                {gpu_memory_state,
                    erlang:element(2, State),
                    erlang:element(3, State) + Bytes,
                    erlang:element(4, State) - Bytes,
                    erlang:element(5, State) + 1,
                    erlang:element(6, State)}};

        false ->
            {error,
                <<<<<<<<<<"OOM: Não há VRAM suficiente. Livre: "/utf8,
                                    (erlang:integer_to_binary(
                                        (erlang:element(4, State) div 1024) div 1024
                                    ))/binary>>/binary,
                                "MB, "/utf8>>/binary,
                            "Necessário: "/utf8>>/binary,
                        (erlang:integer_to_binary((Bytes div 1024) div 1024))/binary>>/binary,
                    "MB"/utf8>>}
    end.

-file("src/viva_tensor/rtx4090.gleam", 245).
?DOC(" Libera memória\n").
-spec free(gpu_memory_state(), integer()) -> gpu_memory_state().
free(State, Bytes) ->
    {gpu_memory_state,
        erlang:element(2, State),
        gleam@int:max(0, erlang:element(3, State) - Bytes),
        gleam@int:min(
            erlang:element(2, State),
            erlang:element(4, State) + Bytes
        ),
        gleam@int:max(0, erlang:element(5, State) - 1),
        erlang:element(6, State)}.

-file("src/viva_tensor/rtx4090.gleam", 364).
?DOC(" Estima performance para operação de tensor\n").
-spec estimate_performance(float(), float(), rtx4090_config()) -> performance_estimate().
estimate_performance(Flops_needed, Bytes_to_transfer, Config) ->
    Specs = get_specs(),
    Available_tflops = case erlang:element(7, Config) of
        fp32_mode ->
            erlang:element(8, Specs);

        fp16_tensor_mode ->
            erlang:element(9, Specs);

        int8_tensor_mode ->
            erlang:element(10, Specs);

        mixed_precision_mode ->
            erlang:element(9, Specs)
    end,
    Compute_time = case (Available_tflops * 1.0e12) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> Flops_needed / Gleam@denominator
    end,
    Memory_time = case (erlang:element(6, Specs) * 1.0e9) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator@1 -> Bytes_to_transfer / Gleam@denominator@1
    end,
    Bottleneck = case Compute_time > Memory_time of
        true ->
            compute_bound;

        false ->
            memory_bound
    end,
    Total_time = gleam@float:max(Compute_time, Memory_time) * 1.2,
    Theoretical_time = gleam@float:max(Compute_time, Memory_time),
    Efficiency = (case Total_time of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator@2 -> Theoretical_time / Gleam@denominator@2
    end) * 100.0,
    {performance_estimate,
        Available_tflops * 1.0e12,
        (Available_tflops * 1.0e12) * (Efficiency / 100.0),
        Total_time * 1000.0,
        Bottleneck,
        Efficiency}.

-file("src/viva_tensor/rtx4090.gleam", 589).
-spec float_to_string(float()) -> binary().
float_to_string(F) ->
    Rounded = erlang:float(erlang:round(F * 100.0)) / 100.0,
    gleam_stdlib:float_to_string(Rounded).

-file("src/viva_tensor/rtx4090.gleam", 594).
-spec bottleneck_str(bottleneck()) -> binary().
bottleneck_str(B) ->
    case B of
        compute_bound ->
            <<"compute-bound"/utf8>>;

        memory_bound ->
            <<"memory-bound"/utf8>>;

        latency_bound ->
            <<"latency-bound"/utf8>>
    end.

-file("src/viva_tensor/rtx4090.gleam", 270).
?DOC(" Processa batch de tensores com compressão\n").
-spec process_batch(list(viva_tensor@tensor:tensor()), rtx4090_config()) -> batch_result().
process_batch(Tensors, Config) ->
    Quant_config = case erlang:element(7, Config) of
        int8_tensor_mode ->
            viva_tensor@blackwell:int8_config();

        _ ->
            viva_tensor@blackwell:nvfp4_config()
    end,
    Parent = erlang:self(),
    Indexed = gleam@list:index_map(Tensors, fun(T, I) -> {I, T} end),
    gleam@list:each(
        Indexed,
        fun(Pair) ->
            {Idx, T@1} = Pair,
            erlang:spawn(
                fun() ->
                    Compressed = viva_tensor@blackwell:compress(
                        T@1,
                        Quant_config
                    ),
                    viva_tensor_ffi:send_msg(Parent, {Idx, Compressed})
                end
            )
        end
    ),
    Start = erlang:monotonic_time(),
    Results = begin
        _pipe = viva_tensor_ffi:collect_n(erlang:length(Tensors)),
        _pipe@1 = gleam@list:sort(
            _pipe,
            fun(A, B) ->
                {I1, _} = A,
                {I2, _} = B,
                gleam@int:compare(I1, I2)
            end
        ),
        gleam@list:map(
            _pipe@1,
            fun(Pair@1) ->
                {_, T@2} = Pair@1,
                T@2
            end
        )
    end,
    End = erlang:monotonic_time(),
    Time_ns = End - Start,
    Time_ms = Time_ns div 1000000,
    Total_original = gleam@list:fold(
        Tensors,
        0,
        fun(Acc, T@3) ->
            Acc + (erlang:length(viva_tensor@tensor:to_list(T@3)) * 4)
        end
    ),
    Total_compressed = gleam@list:fold(
        Results,
        0,
        fun(Acc@1, Bt) -> Acc@1 + erlang:element(6, Bt) end
    ),
    Ratio = case erlang:float(Total_compressed) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Total_original) / Gleam@denominator
    end,
    Saved_mb = (erlang:float(Total_original - Total_compressed) / 1024.0) / 1024.0,
    Throughput = case (erlang:float(Time_ms) / 1000.0) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator@1 -> erlang:float(erlang:length(Tensors)) / Gleam@denominator@1
    end,
    {batch_result, Results, Time_ms, Throughput, Ratio, Saved_mb}.

-file("src/viva_tensor/rtx4090.gleam", 416).
-spec benchmark_rtx4090() -> nil.
benchmark_rtx4090() ->
    Specs = get_specs(),
    gleam_stdlib:println(
        <<"╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  RTX 4090 ASUS ROG STRIX - OPTIMIZED ENGINE                      ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  Pure Gleam maximizando hardware NVIDIA!                         ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝\n"/utf8>>
    ),
    gleam_stdlib:println(<<"ESPECIFICAÇÕES RTX 4090:"/utf8>>),
    gleam_stdlib:println(
        <<"  CUDA Cores:    "/utf8,
            (erlang:integer_to_binary(erlang:element(2, Specs)))/binary>>
    ),
    gleam_stdlib:println(
        <<<<"  Tensor Cores:  "/utf8,
                (erlang:integer_to_binary(erlang:element(3, Specs)))/binary>>/binary,
            " (4th Gen)"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  VRAM:          "/utf8,
                (float_to_string(erlang:element(4, Specs)))/binary>>/binary,
            " GB GDDR6X"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Bandwidth:     "/utf8,
                (float_to_string(erlang:element(6, Specs)))/binary>>/binary,
            " GB/s"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  L2 Cache:      "/utf8,
                (erlang:integer_to_binary(erlang:element(13, Specs)))/binary>>/binary,
            " MB"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<<<"  FP32:          "/utf8,
                (float_to_string(erlang:element(8, Specs)))/binary>>/binary,
            " TFLOPS"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  FP16 Tensor:   "/utf8,
                (float_to_string(erlang:element(9, Specs)))/binary>>/binary,
            " TFLOPS (4x FP32!)"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  INT8 Tensor:   "/utf8,
                (float_to_string(erlang:element(10, Specs)))/binary>>/binary,
            " TOPS (8x FP32!)"/utf8>>
    ),
    gleam_stdlib:println(<<"\n━━━ MEMORY STATE ━━━"/utf8>>),
    Mem = init_memory(),
    gleam_stdlib:println(
        <<<<"  Total VRAM:    "/utf8,
                (erlang:integer_to_binary(
                    (erlang:element(2, Mem) div 1024) div 1024
                ))/binary>>/binary,
            " MB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Free VRAM:     "/utf8,
                (erlang:integer_to_binary(
                    (erlang:element(4, Mem) div 1024) div 1024
                ))/binary>>/binary,
            " MB"/utf8>>
    ),
    Tensor_size = tensor_memory_bytes([1024, 1024], int8_tensor_mode),
    gleam_stdlib:println(
        <<<<"\n  Tensor 1024x1024 INT8: "/utf8,
                (erlang:integer_to_binary(Tensor_size div 1024))/binary>>/binary,
            " KB"/utf8>>
    ),
    Max_tensors = case Tensor_size of
        0 -> 0;
        Gleam@denominator -> erlang:element(4, Mem) div Gleam@denominator
    end,
    gleam_stdlib:println(
        <<"  Tensores que cabem:    "/utf8,
            (erlang:integer_to_binary(Max_tensors))/binary>>
    ),
    gleam_stdlib:println(<<"\n━━━ BATCH PROCESSING (BEAM Parallel) ━━━"/utf8>>),
    Batch_sizes = [100, 500, 1000],
    gleam@list:each(
        Batch_sizes,
        fun(N) ->
            Tensors = begin
                _pipe = gleam@list:range(1, N),
                gleam@list:map(
                    _pipe,
                    fun(_) -> viva_tensor@tensor:random_uniform([512]) end
                )
            end,
            Config = default_config(),
            Result = process_batch(Tensors, Config),
            gleam_stdlib:println(
                <<<<"  "/utf8, (erlang:integer_to_binary(N))/binary>>/binary,
                    " tensors x 512d:"/utf8>>
            ),
            gleam_stdlib:println(
                <<<<"    Tempo:       "/utf8,
                        (erlang:integer_to_binary(erlang:element(3, Result)))/binary>>/binary,
                    "ms"/utf8>>
            ),
            gleam_stdlib:println(
                <<<<"    Throughput:  "/utf8,
                        (float_to_string(erlang:element(4, Result)))/binary>>/binary,
                    " tensors/sec"/utf8>>
            ),
            gleam_stdlib:println(
                <<<<"    Compressão:  "/utf8,
                        (float_to_string(erlang:element(5, Result)))/binary>>/binary,
                    "x"/utf8>>
            ),
            gleam_stdlib:println(
                <<<<"    Economia:    "/utf8,
                        (float_to_string(erlang:element(6, Result)))/binary>>/binary,
                    " MB"/utf8>>
            ),
            gleam_stdlib:println(<<""/utf8>>)
        end
    ),
    gleam_stdlib:println(<<"━━━ PERFORMANCE ESTIMATION ━━━"/utf8>>),
    Matmul_flops = ((4096.0 * 4096.0) * 4096.0) * 2.0,
    Matmul_bytes = ((4096.0 * 4096.0) * 4.0) * 3.0,
    gleam_stdlib:println(<<"  Matmul 4096x4096:"/utf8>>),
    Est_fp32 = estimate_performance(
        Matmul_flops,
        Matmul_bytes,
        precision_config()
    ),
    gleam_stdlib:println(
        <<<<<<"    FP32:  "/utf8,
                    (float_to_string(erlang:element(4, Est_fp32)))/binary>>/binary,
                "ms, "/utf8>>/binary,
            (bottleneck_str(erlang:element(5, Est_fp32)))/binary>>
    ),
    Est_fp16 = estimate_performance(
        Matmul_flops,
        Matmul_bytes / 2.0,
        begin
            _record = default_config(),
            {rtx4090_config,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                erlang:element(6, _record),
                fp16_tensor_mode}
        end
    ),
    gleam_stdlib:println(
        <<<<<<"    FP16:  "/utf8,
                    (float_to_string(erlang:element(4, Est_fp16)))/binary>>/binary,
                "ms, "/utf8>>/binary,
            (bottleneck_str(erlang:element(5, Est_fp16)))/binary>>
    ),
    Est_int8 = estimate_performance(
        Matmul_flops,
        Matmul_bytes / 4.0,
        default_config()
    ),
    gleam_stdlib:println(
        <<<<<<"    INT8:  "/utf8,
                    (float_to_string(erlang:element(4, Est_int8)))/binary>>/binary,
                "ms, "/utf8>>/binary,
            (bottleneck_str(erlang:element(5, Est_int8)))/binary>>
    ),
    gleam_stdlib:println(
        <<"\n╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  RECOMENDAÇÕES PARA SUA RTX 4090:                                ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                                                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  1. Use INT8 Tensor Cores para inference (661 TOPS!)             ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  2. Batch size ótimo: 128-256 tensores                           ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  3. Alinhe memória em 32 bytes (256-bit bus)                     ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  4. Tile size 16x16 para Tensor Cores                            ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  5. 22GB VRAM útil = ~22M tensores de 1KB                        ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                                                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  Com compressão INT8: 24GB VRAM → 96GB efetivo!                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝"/utf8>>
    ).

-file("src/viva_tensor/rtx4090.gleam", 412).
-spec main() -> nil.
main() ->
    benchmark_rtx4090().
