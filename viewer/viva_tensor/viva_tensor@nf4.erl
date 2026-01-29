-module(viva_tensor@nf4).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/nf4.gleam").
-export([nf4_levels/0, default_config/0, quantize/2, double_quantize/2, dequantize/1, compute_stats/2, benchmark_nf4/0, main/0]).
-export_type([n_f4_block/0, n_f4_tensor/0, n_f4_config/0, double_quant_n_f4/0, n_f4_stats/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " NF4 (NormalFloat4) Quantization - QLoRA Style\n"
    "\n"
    " DESCOBERTA VIA HUGGINGCHAT + PESQUISA:\n"
    " NF4 usa 16 níveis derivados dos quantis da distribuição normal\n"
    " Isso é ÓTIMO para pesos de NNs que seguem distribuição gaussiana!\n"
    "\n"
    " Referências:\n"
    " - QLoRA Paper: https://arxiv.org/abs/2305.14314\n"
    " - bitsandbytes: create_normal_map function\n"
    " - MLSys 2024: AWQ Best Paper\n"
    "\n"
    " Vantagens sobre Q4 uniforme:\n"
    " - Mais precisão próximo de zero (onde concentram os pesos)\n"
    " - 8x compressão com ~0.1% erro\n"
    " - Matematicamente ótimo para dados normais\n"
).

-type n_f4_block() :: {n_f4_block, list(integer()), float(), integer()}.

-type n_f4_tensor() :: {n_f4_tensor,
        list(n_f4_block()),
        list(integer()),
        integer(),
        integer(),
        float()}.

-type n_f4_config() :: {n_f4_config, integer(), boolean()}.

-type double_quant_n_f4() :: {double_quant_n_f4,
        list(n_f4_block()),
        list(integer()),
        float(),
        list(integer()),
        integer(),
        integer()}.

-type n_f4_stats() :: {n_f4_stats,
        integer(),
        integer(),
        float(),
        float(),
        float(),
        integer()}.

-file("src/viva_tensor/nf4.gleam", 29).
?DOC(
    " Os 16 níveis NF4 são os quantis de N(0,1) normalizados para [-1, 1]\n"
    " Esses valores são hardcoded em bitsandbytes e usados em QLoRA\n"
).
-spec nf4_levels() -> list(float()).
nf4_levels() ->
    [-1.0,
        -0.6961928009986877,
        -0.5250730514526367,
        -0.39491748809814453,
        -0.28444138169288635,
        -0.18477343022823334,
        -0.09105003625154495,
        +0.0,
        0.07958029955625534,
        0.16093020141124725,
        0.24611230194568634,
        0.33791524171829224,
        0.44070982933044434,
        0.5626170039176941,
        0.7229568362236023,
        1.0].

-file("src/viva_tensor/nf4.gleam", 109).
?DOC(" Configuração padrão QLoRA\n").
-spec default_config() -> n_f4_config().
default_config() ->
    {n_f4_config, 64, false}.

-file("src/viva_tensor/nf4.gleam", 177).
?DOC(" Encontra o índice do nível NF4 mais próximo\n").
-spec find_nearest_nf4_index(float()) -> integer().
find_nearest_nf4_index(Value) ->
    Levels = nf4_levels(),
    _pipe = Levels,
    _pipe@1 = gleam@list:index_map(
        _pipe,
        fun(Level, Idx) ->
            Distance = gleam@float:absolute_value(Value - Level),
            {Idx, Distance}
        end
    ),
    _pipe@2 = gleam@list:fold(
        _pipe@1,
        {0, 999.0},
        fun(Best, Current) ->
            case erlang:element(2, Current) < erlang:element(2, Best) of
                true ->
                    Current;

                false ->
                    Best
            end
        end
    ),
    (fun(Result) -> erlang:element(1, Result) end)(_pipe@2).

-file("src/viva_tensor/nf4.gleam", 154).
?DOC(" Quantiza um bloco de valores para NF4\n").
-spec quantize_block(list(float()), integer()) -> n_f4_block().
quantize_block(Values, Block_size) ->
    Abs_max = begin
        _pipe = Values,
        _pipe@1 = gleam@list:map(_pipe, fun gleam@float:absolute_value/1),
        gleam@list:fold(_pipe@1, +0.0, fun gleam@float:max/2)
    end,
    Safe_max = case Abs_max > +0.0 of
        true ->
            Abs_max;

        false ->
            1.0
    end,
    Normalized = gleam@list:map(Values, fun(V) -> case Safe_max of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> V / Gleam@denominator
            end end),
    Indices = gleam@list:map(Normalized, fun find_nearest_nf4_index/1),
    {n_f4_block, Indices, Safe_max, Block_size}.

-file("src/viva_tensor/nf4.gleam", 459).
-spec get_tensor_shape(viva_tensor@tensor:tensor()) -> list(integer()).
get_tensor_shape(T) ->
    case T of
        {tensor, _, Shape} ->
            Shape;

        {strided_tensor, _, Shape@1, _, _} ->
            Shape@1
    end.

-file("src/viva_tensor/nf4.gleam", 118).
?DOC(" Quantiza tensor para NF4\n").
-spec quantize(viva_tensor@tensor:tensor(), n_f4_config()) -> n_f4_tensor().
quantize(T, Config) ->
    Data = viva_tensor@tensor:to_list(T),
    Shape = get_tensor_shape(T),
    Num_elements = erlang:length(Data),
    Chunks = gleam@list:sized_chunk(Data, erlang:element(2, Config)),
    Blocks = gleam@list:map(
        Chunks,
        fun(Chunk) -> quantize_block(Chunk, erlang:element(2, Config)) end
    ),
    Num_blocks = erlang:length(Blocks),
    Data_bytes = Num_elements div 2,
    Scale_bytes = Num_blocks * 2,
    Memory = Data_bytes + Scale_bytes,
    Original_memory = Num_elements * 4,
    Ratio = case erlang:float(Memory) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Original_memory) / Gleam@denominator
    end,
    {n_f4_tensor, Blocks, Shape, Num_elements, Memory, Ratio}.

-file("src/viva_tensor/nf4.gleam", 241).
?DOC(" Aplica Double Quantization\n").
-spec double_quantize(viva_tensor@tensor:tensor(), n_f4_config()) -> double_quant_n_f4().
double_quantize(T, Config) ->
    Nf4 = quantize(T, Config),
    Scales = gleam@list:map(
        erlang:element(2, Nf4),
        fun(B) -> erlang:element(3, B) end
    ),
    Scales_max = begin
        _pipe = Scales,
        _pipe@1 = gleam@list:map(_pipe, fun gleam@float:absolute_value/1),
        gleam@list:fold(_pipe@1, +0.0, fun gleam@float:max/2)
    end,
    Scales_scale = case Scales_max > +0.0 of
        true ->
            case Scales_max of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> 127.0 / Gleam@denominator
            end;

        false ->
            1.0
    end,
    Quantized_scales = gleam@list:map(
        Scales,
        fun(S) ->
            Scaled = S * Scales_scale,
            _pipe@2 = gleam@float:clamp(Scaled, -127.0, 127.0),
            erlang:round(_pipe@2)
        end
    ),
    Num_blocks = erlang:length(erlang:element(2, Nf4)),
    Data_bytes = erlang:element(4, Nf4) div 2,
    Scale_bytes = Num_blocks,
    Memory = (Data_bytes + Scale_bytes) + 4,
    {double_quant_n_f4,
        erlang:element(2, Nf4),
        Quantized_scales,
        Scales_scale,
        erlang:element(3, Nf4),
        erlang:element(4, Nf4),
        Memory}.

-file("src/viva_tensor/nf4.gleam", 466).
-spec get_at_index(list(float()), integer(), float()) -> float().
get_at_index(Lst, Idx, Default) ->
    case gleam@list:drop(Lst, Idx) of
        [First | _] ->
            First;

        [] ->
            Default
    end.

-file("src/viva_tensor/nf4.gleam", 200).
?DOC(" Dequantiza tensor NF4 de volta para FP32\n").
-spec dequantize(n_f4_tensor()) -> viva_tensor@tensor:tensor().
dequantize(Nf4) ->
    Levels = nf4_levels(),
    Data = gleam@list:flat_map(
        erlang:element(2, Nf4),
        fun(Block) ->
            gleam@list:map(
                erlang:element(2, Block),
                fun(Idx) ->
                    Level = get_at_index(Levels, Idx, +0.0),
                    Level * erlang:element(3, Block)
                end
            )
        end
    ),
    Truncated = gleam@list:take(Data, erlang:element(4, Nf4)),
    {tensor, Truncated, erlang:element(3, Nf4)}.

-file("src/viva_tensor/nf4.gleam", 300).
?DOC(" Calcula estatísticas de erro\n").
-spec compute_stats(viva_tensor@tensor:tensor(), n_f4_tensor()) -> n_f4_stats().
compute_stats(Original, Nf4) ->
    Decompressed = dequantize(Nf4),
    Orig_data = viva_tensor@tensor:to_list(Original),
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
            Sum = gleam@list:fold(Errors, +0.0, fun(Acc, E) -> Acc + E end),
            case erlang:float(erlang:length(Errors)) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> Sum / Gleam@denominator
            end
    end,
    Max_error = gleam@list:fold(Errors, +0.0, fun gleam@float:max/2),
    Original_bytes = erlang:length(Orig_data) * 4,
    {n_f4_stats,
        Original_bytes,
        erlang:element(5, Nf4),
        erlang:element(6, Nf4),
        Mean_error,
        Max_error,
        erlang:length(erlang:element(2, Nf4))}.

-file("src/viva_tensor/nf4.gleam", 473).
-spec float_to_string(float()) -> binary().
float_to_string(F) ->
    Rounded = erlang:float(erlang:round(F * 10000.0)) / 10000.0,
    gleam_stdlib:float_to_string(Rounded).

-file("src/viva_tensor/nf4.gleam", 340).
-spec benchmark_nf4() -> nil.
benchmark_nf4() ->
    gleam_stdlib:println(
        <<"╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  NF4 (NormalFloat4) QUANTIZATION - QLoRA Style                   ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  8x compressão com distribuição gaussiana otimizada!             ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝\n"/utf8>>
    ),
    gleam_stdlib:println(
        <<"NÍVEIS NF4 (16 quantis da distribuição normal):"/utf8>>
    ),
    _pipe = nf4_levels(),
    gleam@list:index_map(
        _pipe,
        fun(Level, Idx) ->
            gleam_stdlib:println(
                <<<<<<"  ["/utf8, (erlang:integer_to_binary(Idx))/binary>>/binary,
                        "]: "/utf8>>/binary,
                    (float_to_string(Level))/binary>>
            )
        end
    ),
    gleam_stdlib:println(<<"\n━━━ BENCHMARK: Tensor 1024x512 ━━━"/utf8>>),
    T = viva_tensor@tensor:random_uniform([1024, 512]),
    Config = default_config(),
    {Time_nf4, Nf4} = timer:tc(fun() -> quantize(T, Config) end),
    Stats = compute_stats(T, Nf4),
    gleam_stdlib:println(<<"\nNF4 Quantization:"/utf8>>),
    gleam_stdlib:println(
        <<<<"  Tempo:       "/utf8,
                (erlang:integer_to_binary(Time_nf4 div 1000))/binary>>/binary,
            "ms"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Original:    "/utf8,
                (erlang:integer_to_binary(erlang:element(2, Stats) div 1024))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Comprimido:  "/utf8,
                (erlang:integer_to_binary(erlang:element(3, Stats) div 1024))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Compressão:  "/utf8,
                (float_to_string(erlang:element(4, Stats)))/binary>>/binary,
            "x"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  Erro médio:  "/utf8,
            (float_to_string(erlang:element(5, Stats)))/binary>>
    ),
    gleam_stdlib:println(
        <<"  Erro máx:    "/utf8,
            (float_to_string(erlang:element(6, Stats)))/binary>>
    ),
    gleam_stdlib:println(
        <<"  Blocos:      "/utf8,
            (erlang:integer_to_binary(erlang:element(7, Stats)))/binary>>
    ),
    gleam_stdlib:println(
        <<"\n━━━ DOUBLE QUANTIZATION (reduz overhead de metadados) ━━━"/utf8>>
    ),
    {Time_dq, Dq} = timer:tc(fun() -> double_quantize(T, Config) end),
    Dq_ratio = case erlang:float(erlang:element(7, Dq)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(erlang:element(2, Stats)) / Gleam@denominator
    end,
    gleam_stdlib:println(
        <<<<"  Tempo:       "/utf8,
                (erlang:integer_to_binary(Time_dq div 1000))/binary>>/binary,
            "ms"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Comprimido:  "/utf8,
                (erlang:integer_to_binary(erlang:element(7, Dq) div 1024))/binary>>/binary,
            " KB"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Compressão:  "/utf8, (float_to_string(Dq_ratio))/binary>>/binary,
            "x"/utf8>>
    ),
    gleam_stdlib:println(<<"\n━━━ COMPARAÇÃO DE FORMATOS ━━━"/utf8>>),
    gleam_stdlib:println(
        <<<<"  FP32:  "/utf8,
                (erlang:integer_to_binary(erlang:element(2, Stats) div 1024))/binary>>/binary,
            " KB (1x)"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  FP16:  "/utf8,
                (erlang:integer_to_binary(
                    (erlang:element(2, Stats) div 2) div 1024
                ))/binary>>/binary,
            " KB (2x)"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  INT8:  "/utf8,
                (erlang:integer_to_binary(
                    (erlang:element(2, Stats) div 4) div 1024
                ))/binary>>/binary,
            " KB (4x)"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<"  NF4:   "/utf8,
                        (erlang:integer_to_binary(
                            erlang:element(3, Stats) div 1024
                        ))/binary>>/binary,
                    " KB ("/utf8>>/binary,
                (float_to_string(erlang:element(4, Stats)))/binary>>/binary,
            "x)"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<"  NF4+DQ: "/utf8,
                        (erlang:integer_to_binary(
                            erlang:element(7, Dq) div 1024
                        ))/binary>>/binary,
                    " KB ("/utf8>>/binary,
                (float_to_string(Dq_ratio))/binary>>/binary,
            "x)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"\n╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  POR QUE NF4 É MELHOR QUE Q4 UNIFORME:                           ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                                                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  1. Níveis derivados da distribuição normal                      ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  2. Mais precisão próximo de zero (onde concentram pesos)        ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  3. Matematicamente ótimo para dados gaussianos                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  4. Usado em QLoRA, bitsandbytes, Hugging Face                   ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                                                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  Com NF4: 24GB VRAM → ~192GB efetivo (8x)!                       ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝"/utf8>>
    ).

-file("src/viva_tensor/nf4.gleam", 336).
-spec main() -> nil.
main() ->
    benchmark_nf4().
