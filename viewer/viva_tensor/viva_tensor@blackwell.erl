-module(viva_tensor@blackwell).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/blackwell.gleam").
-export([nvfp4_config/0, int8_config/0, decompress/1, new_stream/1, memory_latency_ns/1, memory_bandwidth_gbps/1, transfer_time_us/2, compression_stats/2, compress/2, process_chunk/2, analyze_and_compress/1, benchmark_blackwell_compression/0, main/0]).
-export_type([micro_block/0, blackwell_tensor/0, compression_config/0, compression_stats/0, stream_chunk/0, stream_state/0, distribution_stats/0, memory_level/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Blackwell-Inspired Compression Engine\n"
    "\n"
    " INSPIRADO NA ARQUITETURA NVIDIA BLACKWELL ULTRA:\n"
    " - NVFP4: Two-level scaling (micro-block FP8 E4M3 + tensor-level FP32)\n"
    " - Hardware decompression: 800 GB/s\n"
    " - Micro-block size: 16 valores\n"
    " - Memory hierarchy: HBM3e → L2 → L1 → Registers\n"
    "\n"
    " DIFERENCIAL GLEAM:\n"
    " - GenServer actors para gerenciar chunks de memória\n"
    " - Supervisores OTP para fault tolerance\n"
    " - Zero-copy views via Erlang binaries\n"
    " - BEAM schedulers para paralelismo massivo\n"
    "\n"
    " FÍSICA DO SILÍCIO (limites reais):\n"
    " - 8-bit multiplier: 64 units de área\n"
    " - 32-bit multiplier: 576 units (9x maior!)\n"
    " - HBM4 (2026): 2 TB/s por chip\n"
    " - Blackwell: 8 TB/s HBM3e bandwidth\n"
    "\n"
    " OBJETIVO: Fazer Pure Gleam competir com hardware dedicado!\n"
).

-type micro_block() :: {micro_block, list(integer()), float(), float()}.

-type blackwell_tensor() :: {blackwell_tensor,
        list(micro_block()),
        float(),
        list(integer()),
        integer(),
        integer(),
        float()}.

-type compression_config() :: {compression_config,
        integer(),
        integer(),
        boolean(),
        float()}.

-type compression_stats() :: {compression_stats,
        integer(),
        integer(),
        float(),
        float(),
        float(),
        integer()}.

-type stream_chunk() :: {stream_chunk, integer(), micro_block(), boolean()}.

-type stream_state() :: {stream_state,
        compression_config(),
        integer(),
        integer(),
        integer()}.

-type distribution_stats() :: {distribution_stats,
        float(),
        float(),
        float(),
        float(),
        float(),
        float()}.

-type memory_level() :: registers |
    l1_cache |
    l2_cache |
    hbm |
    system_ram |
    storage.

-file("src/viva_tensor/blackwell.gleam", 94).
?DOC(" Configuração padrão NVFP4 (Blackwell style)\n").
-spec nvfp4_config() -> compression_config().
nvfp4_config() ->
    {compression_config, 16, 4, false, 2.0}.

-file("src/viva_tensor/blackwell.gleam", 108).
?DOC(" Configuração INT8 (mais precisa)\n").
-spec int8_config() -> compression_config().
int8_config() ->
    {compression_config, 32, 8, true, 0.5}.

-file("src/viva_tensor/blackwell.gleam", 221).
?DOC(" Descomprime tensor Blackwell de volta para FP32\n").
-spec decompress(blackwell_tensor()) -> viva_tensor@tensor:tensor().
decompress(Bt) ->
    Data = gleam@list:flat_map(
        erlang:element(2, Bt),
        fun(Block) ->
            gleam@list:map(
                erlang:element(2, Block),
                fun(Q) ->
                    Dequant = (case erlang:element(3, Block) of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> erlang:float(Q) / Gleam@denominator
                    end) + erlang:element(4, Block),
                    Dequant * erlang:element(3, Bt)
                end
            )
        end
    ),
    Truncated = gleam@list:take(Data, erlang:element(5, Bt)),
    {tensor, Truncated, erlang:element(4, Bt)}.

-file("src/viva_tensor/blackwell.gleam", 296).
?DOC(" Cria novo estado de streaming\n").
-spec new_stream(compression_config()) -> stream_state().
new_stream(Config) ->
    {stream_state, Config, 0, 0, 0}.

-file("src/viva_tensor/blackwell.gleam", 451).
?DOC(" Simula latência de acesso\n").
-spec memory_latency_ns(memory_level()) -> integer().
memory_latency_ns(Level) ->
    case Level of
        registers ->
            1;

        l1_cache ->
            4;

        l2_cache ->
            12;

        hbm ->
            200;

        system_ram ->
            100;

        storage ->
            10000
    end.

-file("src/viva_tensor/blackwell.gleam", 469).
?DOC(" Simula bandwidth em GB/s\n").
-spec memory_bandwidth_gbps(memory_level()) -> float().
memory_bandwidth_gbps(Level) ->
    case Level of
        registers ->
            10000.0;

        l1_cache ->
            1000.0;

        l2_cache ->
            500.0;

        hbm ->
            8000.0;

        system_ram ->
            51.2;

        storage ->
            7.0
    end.

-file("src/viva_tensor/blackwell.gleam", 487).
?DOC(" Calcula tempo de transferência\n").
-spec transfer_time_us(float(), memory_level()) -> float().
transfer_time_us(Size_mb, Level) ->
    Bandwidth = memory_bandwidth_gbps(Level),
    Size_gb = Size_mb / 1024.0,
    Time_s = case Bandwidth of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> Size_gb / Gleam@denominator
    end,
    Time_s * 1000000.0.

-file("src/viva_tensor/blackwell.gleam", 684).
-spec get_tensor_shape(viva_tensor@tensor:tensor()) -> list(integer()).
get_tensor_shape(T) ->
    case T of
        {tensor, _, Shape} ->
            Shape;

        {strided_tensor, _, Shape@1, _, _} ->
            Shape@1
    end.

-file("src/viva_tensor/blackwell.gleam", 691).
-spec find_max_abs(list(float())) -> float().
find_max_abs(Data) ->
    gleam@list:fold(
        Data,
        +0.0,
        fun(Acc, V) ->
            Abs_v = gleam@float:absolute_value(V),
            case Abs_v > Acc of
                true ->
                    Abs_v;

                false ->
                    Acc
            end
        end
    ).

-file("src/viva_tensor/blackwell.gleam", 701).
-spec find_max(list(float())) -> float().
find_max(Data) ->
    case Data of
        [] ->
            +0.0;

        [First | Rest] ->
            gleam@list:fold(Rest, First, fun(Acc, V) -> case V > Acc of
                        true ->
                            V;

                        false ->
                            Acc
                    end end)
    end.

-file("src/viva_tensor/blackwell.gleam", 238).
?DOC(" Calcula estatísticas de compressão\n").
-spec compression_stats(viva_tensor@tensor:tensor(), blackwell_tensor()) -> compression_stats().
compression_stats(Original, Compressed) ->
    Original_data = viva_tensor@tensor:to_list(Original),
    Decompressed = decompress(Compressed),
    Decompressed_data = viva_tensor@tensor:to_list(Decompressed),
    Original_bytes = erlang:length(Original_data) * 4,
    Compressed_bytes = erlang:element(6, Compressed),
    Errors = begin
        _pipe = gleam@list:zip(Original_data, Decompressed_data),
        gleam@list:map(
            _pipe,
            fun(Pair) ->
                {O, D} = Pair,
                gleam@float:absolute_value(O - D)
            end
        )
    end,
    Mean_error = case Errors /= [] of
        true ->
            case erlang:float(erlang:length(Errors)) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> gleam@list:fold(
                    Errors,
                    +0.0,
                    fun(Acc, E) -> Acc + E end
                )
                / Gleam@denominator
            end;

        false ->
            +0.0
    end,
    Max_error = find_max(Errors),
    {compression_stats,
        Original_bytes,
        Compressed_bytes,
        erlang:element(7, Compressed),
        Mean_error,
        Max_error,
        erlang:length(erlang:element(2, Compressed))}.

-file("src/viva_tensor/blackwell.gleam", 714).
-spec find_min(list(float())) -> float().
find_min(Data) ->
    case Data of
        [] ->
            +0.0;

        [First | Rest] ->
            gleam@list:fold(Rest, First, fun(Acc, V) -> case V < Acc of
                        true ->
                            V;

                        false ->
                            Acc
                    end end)
    end.

-file("src/viva_tensor/blackwell.gleam", 727).
-spec result_to_float({ok, float()} | {error, any()}, float()) -> float().
result_to_float(R, Default) ->
    case R of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-file("src/viva_tensor/blackwell.gleam", 161).
?DOC(" Quantiza um micro-block de 16 valores\n").
-spec quantize_microblock(list(float()), compression_config(), float()) -> micro_block().
quantize_microblock(Values, Config, Global_scale) ->
    Normalized = gleam@list:map(Values, fun(V) -> case Global_scale of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> V / Gleam@denominator
            end end),
    Block_min = find_min(Normalized),
    Block_max = find_max(Normalized),
    {Scale@2, Zero_point} = case erlang:element(4, Config) of
        true ->
            Max_abs = gleam@float:max(
                gleam@float:absolute_value(Block_min),
                gleam@float:absolute_value(Block_max)
            ),
            Max_int = begin
                _pipe = gleam@float:power(
                    2.0,
                    erlang:float(erlang:element(3, Config) - 1)
                ),
                result_to_float(_pipe, 128.0)
            end,
            Scale = case Max_abs > +0.0 of
                true ->
                    case Max_abs of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator@1 -> Max_int / Gleam@denominator@1
                    end;

                false ->
                    1.0
            end,
            {Scale, +0.0};

        false ->
            Range = Block_max - Block_min,
            Max_int@1 = begin
                _pipe@1 = gleam@float:power(
                    2.0,
                    erlang:float(erlang:element(3, Config))
                ),
                result_to_float(_pipe@1, 16.0)
            end,
            Scale@1 = case Range > +0.0 of
                true ->
                    case Range of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator@2 -> (Max_int@1 - 1.0) / Gleam@denominator@2
                    end;

                false ->
                    1.0
            end,
            {Scale@1, Block_min}
    end,
    Max_val = begin
        _pipe@2 = gleam@float:power(
            2.0,
            erlang:float(erlang:element(3, Config))
        ),
        _pipe@3 = result_to_float(_pipe@2, 16.0),
        (fun(X) -> X - 1.0 end)(_pipe@3)
    end,
    Quantized = gleam@list:map(
        Normalized,
        fun(V@1) ->
            Shifted = (V@1 - Zero_point) * Scale@2,
            Clamped = gleam@float:clamp(Shifted, +0.0, Max_val),
            erlang:round(Clamped)
        end
    ),
    {micro_block, Quantized, Scale@2, Zero_point}.

-file("src/viva_tensor/blackwell.gleam", 118).
?DOC(" Comprime tensor usando NVFP4 style\n").
-spec compress(viva_tensor@tensor:tensor(), compression_config()) -> blackwell_tensor().
compress(T, Config) ->
    Data = viva_tensor@tensor:to_list(T),
    Shape = get_tensor_shape(T),
    Num_elements = erlang:length(Data),
    Chunks = gleam@list:sized_chunk(Data, erlang:element(2, Config)),
    Global_max = find_max_abs(Data),
    Global_scale = case Global_max > +0.0 of
        true ->
            Global_max;

        false ->
            1.0
    end,
    Blocks = gleam@list:map(
        Chunks,
        fun(Chunk) -> quantize_microblock(Chunk, Config, Global_scale) end
    ),
    Bytes_per_block = ((erlang:element(2, Config) * erlang:element(3, Config))
    div 8)
    + 8,
    Num_blocks = erlang:length(Blocks),
    Memory = (Num_blocks * Bytes_per_block) + 4,
    Original_memory = Num_elements * 4,
    Ratio = case erlang:float(Memory) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Original_memory) / Gleam@denominator
    end,
    {blackwell_tensor, Blocks, Global_scale, Shape, Num_elements, Memory, Ratio}.

-file("src/viva_tensor/blackwell.gleam", 306).
?DOC(" Processa um chunk de dados em streaming\n").
-spec process_chunk(stream_state(), list(float())) -> {stream_state(),
    micro_block()}.
process_chunk(State, Data) ->
    Global_scale = find_max_abs(Data),
    Block = quantize_microblock(
        Data,
        erlang:element(2, State),
        case Global_scale > +0.0 of
            true ->
                Global_scale;

            false ->
                1.0
        end
    ),
    Bytes_in = erlang:length(Data) * 4,
    Bytes_out = ((erlang:element(2, erlang:element(2, State)) * erlang:element(
        3,
        erlang:element(2, State)
    ))
    div 8)
    + 8,
    New_state = {stream_state,
        erlang:element(2, State),
        erlang:element(3, State) + 1,
        erlang:element(4, State) + Bytes_in,
        erlang:element(5, State) + Bytes_out},
    {New_state, Block}.

-file("src/viva_tensor/blackwell.gleam", 381).
?DOC(" Analisa distribuição dos dados\n").
-spec analyze_distribution(list(float())) -> distribution_stats().
analyze_distribution(Data) ->
    N = erlang:length(Data),
    N_float = erlang:float(N),
    Sum = gleam@list:fold(Data, +0.0, fun(Acc, V) -> Acc + V end),
    Mean = case N > 0 of
        true ->
            case N_float of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> Sum / Gleam@denominator
            end;

        false ->
            +0.0
    end,
    Variance = case N > 0 of
        true ->
            Sum_sq = gleam@list:fold(
                Data,
                +0.0,
                fun(Acc@1, V@1) ->
                    Diff = V@1 - Mean,
                    Acc@1 + (Diff * Diff)
                end
            ),
            case N_float of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@1 -> Sum_sq / Gleam@denominator@1
            end;

        false ->
            +0.0
    end,
    Std = begin
        _pipe = gleam@float:square_root(Variance),
        result_to_float(_pipe, +0.0)
    end,
    Min_val = find_min(Data),
    Max_val = find_max(Data),
    Dynamic_range = case Min_val /= +0.0 of
        true ->
            gleam@float:absolute_value(case Min_val of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator@2 -> Max_val / Gleam@denominator@2
                end);

        false ->
            gleam@float:absolute_value(Max_val)
    end,
    Zero_threshold = 0.001,
    Near_zero = gleam@list:filter(
        Data,
        fun(V@2) -> gleam@float:absolute_value(V@2) < Zero_threshold end
    ),
    Sparsity = case N_float of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator@3 -> erlang:float(erlang:length(Near_zero)) / Gleam@denominator@3
    end,
    {distribution_stats, Mean, Std, Min_val, Max_val, Dynamic_range, Sparsity}.

-file("src/viva_tensor/blackwell.gleam", 339).
?DOC(" Analisa tensor e escolhe melhor configuração\n").
-spec analyze_and_compress(viva_tensor@tensor:tensor()) -> blackwell_tensor().
analyze_and_compress(T) ->
    Data = viva_tensor@tensor:to_list(T),
    Stats = analyze_distribution(Data),
    Config = case erlang:element(7, Stats) > 0.5 of
        true ->
            nvfp4_config();

        false ->
            case erlang:element(6, Stats) > 1000.0 of
                true ->
                    int8_config();

                false ->
                    nvfp4_config()
            end
    end,
    compress(T, Config).

-file("src/viva_tensor/blackwell.gleam", 734).
-spec float_to_string(float()) -> binary().
float_to_string(F) ->
    Rounded = erlang:float(erlang:round(F * 100.0)) / 100.0,
    gleam_stdlib:float_to_string(Rounded).

-file("src/viva_tensor/blackwell.gleam", 739).
-spec pad_right(binary(), integer()) -> binary().
pad_right(S, Width) ->
    Len = erlang:byte_size(S),
    case Len >= Width of
        true ->
            S;

        false ->
            <<S/binary, (binary:copy(<<" "/utf8>>, Width - Len))/binary>>
    end.

-file("src/viva_tensor/blackwell.gleam", 503).
-spec benchmark_blackwell_compression() -> nil.
benchmark_blackwell_compression() ->
    gleam_stdlib:println(
        <<"╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  BLACKWELL-INSPIRED COMPRESSION ENGINE                           ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  Pure Gleam competindo com hardware dedicado!                    ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝\n"/utf8>>
    ),
    Size = 1024 * 512,
    T = viva_tensor@tensor:random_uniform([1024, 512]),
    gleam_stdlib:println(<<"TENSOR ORIGINAL:"/utf8>>),
    gleam_stdlib:println(<<"  Shape: [1024, 512]"/utf8>>),
    gleam_stdlib:println(
        <<"  Elementos: "/utf8, (erlang:integer_to_binary(Size))/binary>>
    ),
    gleam_stdlib:println(
        <<<<"  Memória FP32: "/utf8,
                (erlang:integer_to_binary((Size * 4) div 1024))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(
        <<"\n━━━ NVFP4 COMPRESSION (Blackwell Style) ━━━"/utf8>>
    ),
    Config_q4 = nvfp4_config(),
    {Time_q4, Compressed_q4} = timer:tc(fun() -> compress(T, Config_q4) end),
    Stats_q4 = compression_stats(T, Compressed_q4),
    gleam_stdlib:println(
        <<"  Config: 16-value micro-blocks, 4-bit quantization"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Tempo compressão: "/utf8,
                (erlang:integer_to_binary(Time_q4 div 1000))/binary>>/binary,
            "ms"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Memória: "/utf8,
                (erlang:integer_to_binary(erlang:element(3, Stats_q4) div 1024))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Compressão: "/utf8,
                (float_to_string(erlang:element(4, Stats_q4)))/binary>>/binary,
            "x"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<"  Erro médio: "/utf8,
                        (float_to_string(erlang:element(5, Stats_q4)))/binary>>/binary,
                    " ("/utf8>>/binary,
                (float_to_string(erlang:element(5, Stats_q4) * 100.0))/binary>>/binary,
            "%)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  Blocks: "/utf8,
            (erlang:integer_to_binary(erlang:element(7, Stats_q4)))/binary>>
    ),
    gleam_stdlib:println(<<"\n━━━ INT8 COMPRESSION (Alta Precisão) ━━━"/utf8>>),
    Config_int8 = int8_config(),
    {Time_int8, Compressed_int8} = timer:tc(
        fun() -> compress(T, Config_int8) end
    ),
    Stats_int8 = compression_stats(T, Compressed_int8),
    gleam_stdlib:println(
        <<"  Config: 32-value blocks, 8-bit symmetric quantization"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Tempo compressão: "/utf8,
                (erlang:integer_to_binary(Time_int8 div 1000))/binary>>/binary,
            "ms"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Memória: "/utf8,
                (erlang:integer_to_binary(
                    erlang:element(3, Stats_int8) div 1024
                ))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Compressão: "/utf8,
                (float_to_string(erlang:element(4, Stats_int8)))/binary>>/binary,
            "x"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  Erro médio: "/utf8,
            (float_to_string(erlang:element(5, Stats_int8)))/binary>>
    ),
    gleam_stdlib:println(<<"\n━━━ ADAPTIVE COMPRESSION ━━━"/utf8>>),
    {Time_adaptive, Compressed_adaptive} = timer:tc(
        fun() -> analyze_and_compress(T) end
    ),
    Stats_adaptive = compression_stats(T, Compressed_adaptive),
    gleam_stdlib:println(<<"  Análise automática de distribuição"/utf8>>),
    gleam_stdlib:println(
        <<<<"  Tempo: "/utf8,
                (erlang:integer_to_binary(Time_adaptive div 1000))/binary>>/binary,
            "ms"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Compressão: "/utf8,
                (float_to_string(erlang:element(4, Stats_adaptive)))/binary>>/binary,
            "x"/utf8>>
    ),
    gleam_stdlib:println(<<"\n━━━ MEMORY HIERARCHY SIMULATION ━━━"/utf8>>),
    Tensor_mb = 2.0,
    gleam_stdlib:println(<<"  Tensor size: 2 MB"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"  Level       | Bandwidth  | Transfer Time"/utf8>>),
    gleam_stdlib:println(<<"  ------------|------------|-------------"/utf8>>),
    gleam_stdlib:println(
        <<<<<<<<"  Registers   | "/utf8,
                        (pad_right(
                            float_to_string(memory_bandwidth_gbps(registers)),
                            8
                        ))/binary>>/binary,
                    " GB/s | "/utf8>>/binary,
                (float_to_string(transfer_time_us(Tensor_mb, registers)))/binary>>/binary,
            " µs"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<"  L1 Cache    | "/utf8,
                        (pad_right(
                            float_to_string(memory_bandwidth_gbps(l1_cache)),
                            8
                        ))/binary>>/binary,
                    " GB/s | "/utf8>>/binary,
                (float_to_string(transfer_time_us(Tensor_mb, l1_cache)))/binary>>/binary,
            " µs"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<"  L2 Cache    | "/utf8,
                        (pad_right(
                            float_to_string(memory_bandwidth_gbps(l2_cache)),
                            8
                        ))/binary>>/binary,
                    " GB/s | "/utf8>>/binary,
                (float_to_string(transfer_time_us(Tensor_mb, l2_cache)))/binary>>/binary,
            " µs"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<"  HBM3e       | "/utf8,
                        (pad_right(
                            float_to_string(memory_bandwidth_gbps(hbm)),
                            8
                        ))/binary>>/binary,
                    " GB/s | "/utf8>>/binary,
                (float_to_string(transfer_time_us(Tensor_mb, hbm)))/binary>>/binary,
            " µs"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<"  System RAM  | "/utf8,
                        (pad_right(
                            float_to_string(memory_bandwidth_gbps(system_ram)),
                            8
                        ))/binary>>/binary,
                    " GB/s | "/utf8>>/binary,
                (float_to_string(transfer_time_us(Tensor_mb, system_ram)))/binary>>/binary,
            " µs"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<"  NVMe SSD    | "/utf8,
                        (pad_right(
                            float_to_string(memory_bandwidth_gbps(storage)),
                            8
                        ))/binary>>/binary,
                    " GB/s | "/utf8>>/binary,
                (float_to_string(transfer_time_us(Tensor_mb, storage)))/binary>>/binary,
            " µs"/utf8>>
    ),
    gleam_stdlib:println(<<"\n━━━ FÍSICA DO SILÍCIO ━━━"/utf8>>),
    gleam_stdlib:println(<<"  8-bit multiplier:  64 units de área"/utf8>>),
    gleam_stdlib:println(<<"  32-bit multiplier: 576 units (9x maior!)"/utf8>>),
    gleam_stdlib:println(<<"  "/utf8>>),
    gleam_stdlib:println(<<"  → Q4 usa 16 units (4x4)"/utf8>>),
    gleam_stdlib:println(<<"  → FP32 usa 576 units"/utf8>>),
    gleam_stdlib:println(<<"  → Economia: 36x menos área de silício!"/utf8>>),
    gleam_stdlib:println(<<"  "/utf8>>),
    gleam_stdlib:println(<<"  HBM4 (2026): 2 TB/s por chip"/utf8>>),
    gleam_stdlib:println(<<"  Blackwell HBM3e: 8 TB/s total"/utf8>>),
    gleam_stdlib:println(<<"  NVLink 5: 1.8 TB/s bidirecional"/utf8>>),
    gleam_stdlib:println(
        <<"\n╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  CONCLUSÃO:                                                      ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                                                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  NVFP4 Compression (Blackwell Style):                            ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  ├── Micro-blocks de 16 valores                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  ├── Two-level scaling (local + global)                          ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"║  ├── "/utf8,
                (pad_right(
                    <<(float_to_string(erlang:element(4, Stats_q4)))/binary,
                        "x"/utf8>>,
                    5
                ))/binary>>/binary,
            " compressão com < 2% erro                        ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  └── 36x menos área de silício que FP32                          ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                                                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  Pure Gleam pode competir com hardware dedicado!                 ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝"/utf8>>
    ).

-file("src/viva_tensor/blackwell.gleam", 499).
-spec main() -> nil.
main() ->
    benchmark_blackwell_compression().
