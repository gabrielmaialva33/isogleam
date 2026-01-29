-module(viva_tensor@demo).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/demo.gleam").
-export([main/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " viva_tensor Demo - Demonstração completa da biblioteca\n"
    "\n"
    " Roda com: gleam run -m viva_tensor/demo\n"
).

-file("src/viva_tensor/demo.gleam", 311).
-spec demo_combined() -> nil.
demo_combined() ->
    gleam_stdlib:println(
        <<"┌─────────────────────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"│ 7. MEMORY MULTIPLICATION - Combining Techniques            │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"└─────────────────────────────────────────────────────────────┘"/utf8>>
    ),
    Params = 7000000000,
    Fp16_size = Params * 2,
    gleam_stdlib:println(<<"  Model: 7B parameters"/utf8>>),
    gleam_stdlib:println(
        <<<<"  FP16 size: "/utf8,
                (erlang:integer_to_binary(Fp16_size div 1000000000))/binary>>/binary,
            "GB"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"  ┌─────────────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  │ Technique         │ Size     │ Fits RTX 4090 24GB  │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  ├───────────────────┼──────────┼─────────────────────┤"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  │ FP16              │ 14GB     │ [x] Tight           │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  │ INT8              │ 7GB      │ [x] + KV Cache      │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  │ NF4               │ 3.5GB    │ [x] + Batch=32      │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  │ NF4 + 2:4 Sparse  │ 1.75GB   │ [x] Multiple models!│"/utf8>>
    ),
    gleam_stdlib:println(
        <<"  └─────────────────────────────────────────────────────┘"/utf8>>
    ),
    Vram = 24,
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"  RTX 4090 24GB VRAM can effectively hold:"/utf8>>),
    gleam_stdlib:println(
        <<<<"    - FP16:           "/utf8,
                (erlang:integer_to_binary((Vram div 14) * 7))/binary>>/binary,
            "B params"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"    - INT8:           "/utf8,
                (erlang:integer_to_binary((Vram div 7) * 7))/binary>>/binary,
            "B params"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"    - NF4:            "/utf8,
                (erlang:integer_to_binary(Vram * 2))/binary>>/binary,
            "B params"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"    - NF4 + Sparsity: "/utf8,
                (erlang:integer_to_binary(Vram * 4))/binary>>/binary,
            "B params"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>).

-file("src/viva_tensor/demo.gleam", 380).
-spec get_shape(viva_tensor@tensor:tensor()) -> list(integer()).
get_shape(T) ->
    case T of
        {tensor, _, Shape} ->
            Shape;

        {strided_tensor, _, Shape@1, _, _} ->
            Shape@1
    end.

-file("src/viva_tensor/demo.gleam", 387).
-spec shape_to_string(list(integer())) -> binary().
shape_to_string(Shape) ->
    <<<<"["/utf8,
            (begin
                _pipe = gleam@list:map(Shape, fun erlang:integer_to_binary/1),
                gleam@string:join(_pipe, <<", "/utf8>>)
            end)/binary>>/binary,
        "]"/utf8>>.

-file("src/viva_tensor/demo.gleam", 373).
-spec result_shape_str(
    {ok, viva_tensor@tensor:tensor()} |
        {error, viva_tensor@tensor:tensor_error()}
) -> binary().
result_shape_str(R) ->
    case R of
        {ok, T} ->
            shape_to_string(get_shape(T));

        {error, _} ->
            <<"Error"/utf8>>
    end.

-file("src/viva_tensor/demo.gleam", 391).
-spec float_to_str(float()) -> binary().
float_to_str(F) ->
    Rounded = erlang:float(erlang:round(F * 100.0)) / 100.0,
    gleam_stdlib:float_to_string(Rounded).

-file("src/viva_tensor/demo.gleam", 357).
-spec tensor_preview(viva_tensor@tensor:tensor()) -> binary().
tensor_preview(T) ->
    Data = viva_tensor@tensor:to_list(T),
    Preview = begin
        _pipe = gleam@list:take(Data, 5),
        _pipe@1 = gleam@list:map(_pipe, fun float_to_str/1),
        gleam@string:join(_pipe@1, <<", "/utf8>>)
    end,
    <<<<"["/utf8, Preview/binary>>/binary, "...]"/utf8>>.

-file("src/viva_tensor/demo.gleam", 366).
-spec result_tensor_preview(
    {ok, viva_tensor@tensor:tensor()} |
        {error, viva_tensor@tensor:tensor_error()}
) -> binary().
result_tensor_preview(R) ->
    case R of
        {ok, T} ->
            tensor_preview(T);

        {error, _} ->
            <<"Error"/utf8>>
    end.

-file("src/viva_tensor/demo.gleam", 69).
-spec demo_basic_ops() -> nil.
demo_basic_ops() ->
    gleam_stdlib:println(
        <<"┌─────────────────────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"│ 1. OPERAÇÕES BÁSICAS DE TENSORS                            │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"└─────────────────────────────────────────────────────────────┘"/utf8>>
    ),
    A = viva_tensor@tensor:zeros([2, 3]),
    B = viva_tensor@tensor:ones([2, 3]),
    C = viva_tensor@tensor:random_uniform([2, 3]),
    gleam_stdlib:println(
        <<"  zeros([2,3]): "/utf8, (tensor_preview(A))/binary>>
    ),
    gleam_stdlib:println(
        <<"  ones([2,3]):  "/utf8, (tensor_preview(B))/binary>>
    ),
    gleam_stdlib:println(
        <<"  random([2,3]): "/utf8, (tensor_preview(C))/binary>>
    ),
    Sum_result = viva_tensor@tensor:add(A, B),
    gleam_stdlib:println(
        <<"  zeros + ones: "/utf8, (result_tensor_preview(Sum_result))/binary>>
    ),
    Scaled = viva_tensor@tensor:scale(B, 5.0),
    gleam_stdlib:println(
        <<"  ones * 5.0:   "/utf8, (tensor_preview(Scaled))/binary>>
    ),
    Mat_a = viva_tensor@tensor:random_uniform([3, 4]),
    Mat_b = viva_tensor@tensor:random_uniform([4, 2]),
    Matmul_result = viva_tensor@tensor:matmul(Mat_a, Mat_b),
    gleam_stdlib:println(
        <<"  matmul([3,4], [4,2]): "/utf8,
            (result_shape_str(Matmul_result))/binary>>
    ),
    Random_data = viva_tensor@tensor:random_normal([100], +0.0, 1.0),
    gleam_stdlib:println(
        <<<<<<"  random_normal: mean="/utf8,
                    (float_to_str(viva_tensor@tensor:mean(Random_data)))/binary>>/binary,
                ", std="/utf8>>/binary,
            (float_to_str(viva_tensor@tensor:std(Random_data)))/binary>>
    ),
    gleam_stdlib:println(<<""/utf8>>).

-file("src/viva_tensor/demo.gleam", 396).
-spec format_bytes(integer()) -> binary().
format_bytes(Bytes) ->
    case Bytes of
        B when B >= 1073741824 ->
            <<(float_to_str(erlang:float(B) / 1073741824.0))/binary, "GB"/utf8>>;

        B@1 when B@1 >= 1048576 ->
            <<(float_to_str(erlang:float(B@1) / 1048576.0))/binary, "MB"/utf8>>;

        B@2 when B@2 >= 1024 ->
            <<(erlang:integer_to_binary(B@2 div 1024))/binary, "KB"/utf8>>;

        B@3 ->
            <<(erlang:integer_to_binary(B@3))/binary, "B"/utf8>>
    end.

-file("src/viva_tensor/demo.gleam", 406).
-spec compute_error(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> float().
compute_error(Original, Recovered) ->
    Orig_data = viva_tensor@tensor:to_list(Original),
    Rec_data = viva_tensor@tensor:to_list(Recovered),
    Diffs = gleam@list:map2(
        Orig_data,
        Rec_data,
        fun(A, B) -> gleam@float:absolute_value(A - B) end
    ),
    Sum = gleam@list:fold(Diffs, +0.0, fun(Acc, X) -> Acc + X end),
    case erlang:float(erlang:length(Diffs)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> Sum / Gleam@denominator
    end.

-file("src/viva_tensor/demo.gleam", 112).
-spec demo_int8_quantization() -> nil.
demo_int8_quantization() ->
    gleam_stdlib:println(
        <<"┌─────────────────────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"│ 2. INT8 QUANTIZATION (4x compression)                      │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"└─────────────────────────────────────────────────────────────┘"/utf8>>
    ),
    Weights = viva_tensor@tensor:random_normal([256, 256], +0.0, 0.5),
    Original_size = (256 * 256) * 4,
    gleam_stdlib:println(
        <<<<<<"  Original: "/utf8,
                    (erlang:integer_to_binary(256 * 256))/binary>>/binary,
                " floats = "/utf8>>/binary,
            (format_bytes(Original_size))/binary>>
    ),
    Quantized = viva_tensor@compression:quantize_int8(Weights),
    gleam_stdlib:println(
        <<<<"  Quantizado: "/utf8,
                (erlang:integer_to_binary(erlang:element(5, Quantized)))/binary>>/binary,
            " bytes"/utf8>>
    ),
    Compression_ratio = case erlang:float(erlang:element(5, Quantized)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Original_size) / Gleam@denominator
    end,
    gleam_stdlib:println(
        <<<<"  Compressão: "/utf8, (float_to_str(Compression_ratio))/binary>>/binary,
            "x"/utf8>>
    ),
    Recovered = viva_tensor@compression:dequantize(Quantized),
    Error = compute_error(Weights, Recovered),
    gleam_stdlib:println(
        <<<<"  Erro médio: "/utf8, (float_to_str(Error * 100.0))/binary>>/binary,
            "%"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>).

-file("src/viva_tensor/demo.gleam", 151).
-spec demo_nf4_quantization() -> nil.
demo_nf4_quantization() ->
    gleam_stdlib:println(
        <<"┌─────────────────────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"│ 3. NF4 QUANTIZATION - QLoRA Style (7.5x compression)       │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"└─────────────────────────────────────────────────────────────┘"/utf8>>
    ),
    Weights = viva_tensor@tensor:random_normal([512, 512], +0.0, 0.3),
    Original_size = (512 * 512) * 4,
    gleam_stdlib:println(
        <<"  Original: "/utf8, (format_bytes(Original_size))/binary>>
    ),
    Config = viva_tensor@nf4:default_config(),
    Quantized = viva_tensor@nf4:quantize(Weights, Config),
    gleam_stdlib:println(
        <<<<"  NF4 quantizado: "/utf8,
                (erlang:integer_to_binary(erlang:element(5, Quantized)))/binary>>/binary,
            " bytes"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"  Compressão: "/utf8,
                (float_to_str(erlang:element(6, Quantized)))/binary>>/binary,
            "x"/utf8>>
    ),
    Recovered = viva_tensor@nf4:dequantize(Quantized),
    Error = compute_error(Weights, Recovered),
    gleam_stdlib:println(
        <<<<"  Erro médio: "/utf8, (float_to_str(Error * 100.0))/binary>>/binary,
            "%"/utf8>>
    ),
    Dq_quantized = viva_tensor@nf4:double_quantize(Weights, Config),
    Dq_ratio = case erlang:float(erlang:element(7, Dq_quantized)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Original_size) / Gleam@denominator
    end,
    gleam_stdlib:println(
        <<<<"  NF4+DQ: "/utf8, (float_to_str(Dq_ratio))/binary>>/binary,
            "x compressão"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>).

-file("src/viva_tensor/demo.gleam", 231).
-spec demo_flash_attention() -> nil.
demo_flash_attention() ->
    gleam_stdlib:println(
        <<"┌─────────────────────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"│ 5. FLASH ATTENTION - O(n) Memory                           │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"└─────────────────────────────────────────────────────────────┘"/utf8>>
    ),
    Seq_len = 128,
    Head_dim = 64,
    Q = viva_tensor@tensor:random_normal([Seq_len, Head_dim], +0.0, 0.1),
    K = viva_tensor@tensor:random_normal([Seq_len, Head_dim], +0.0, 0.1),
    V = viva_tensor@tensor:random_normal([Seq_len, Head_dim], +0.0, 0.1),
    gleam_stdlib:println(
        <<"  Sequence length: "/utf8,
            (erlang:integer_to_binary(Seq_len))/binary>>
    ),
    gleam_stdlib:println(
        <<"  Head dimension: "/utf8,
            (erlang:integer_to_binary(Head_dim))/binary>>
    ),
    {Naive_output, Naive_mem} = viva_tensor@flash_attention:naive_attention(
        Q,
        K,
        V,
        0.125
    ),
    gleam_stdlib:println(
        <<"  Naive attention memory: "/utf8, (format_bytes(Naive_mem))/binary>>
    ),
    Config = viva_tensor@flash_attention:default_config(Head_dim),
    Flash_result = viva_tensor@flash_attention:flash_attention(Q, K, V, Config),
    gleam_stdlib:println(
        <<"  Flash attention memory: "/utf8,
            (format_bytes(erlang:element(3, Flash_result)))/binary>>
    ),
    gleam_stdlib:println(
        <<<<"  Memory saved: "/utf8,
                (float_to_str(erlang:element(4, Flash_result)))/binary>>/binary,
            "%"/utf8>>
    ),
    Diff = compute_error(Naive_output, erlang:element(2, Flash_result)),
    gleam_stdlib:println(
        <<<<"  Output difference: "/utf8, (float_to_str(Diff * 100.0))/binary>>/binary,
            "%"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>).

-file("src/viva_tensor/demo.gleam", 273).
-spec demo_sparsity() -> nil.
demo_sparsity() ->
    gleam_stdlib:println(
        <<"┌─────────────────────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"│ 6. 2:4 STRUCTURED SPARSITY - Tensor Cores                  │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"└─────────────────────────────────────────────────────────────┘"/utf8>>
    ),
    Weights = viva_tensor@tensor:random_normal([256, 256], +0.0, 0.5),
    Original_size = (256 * 256) * 4,
    gleam_stdlib:println(
        <<"  Original: [256, 256] = "/utf8,
            (format_bytes(Original_size))/binary>>
    ),
    Sparse = viva_tensor@sparsity:prune_24_magnitude(Weights),
    gleam_stdlib:println(
        <<"  Sparse memory: "/utf8,
            (format_bytes(erlang:element(5, Sparse)))/binary>>
    ),
    gleam_stdlib:println(
        <<<<"  Sparsity: "/utf8,
                (float_to_str(erlang:element(6, Sparse)))/binary>>/binary,
            "%"/utf8>>
    ),
    Compression_ratio = case erlang:float(erlang:element(5, Sparse)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Original_size) / Gleam@denominator
    end,
    gleam_stdlib:println(
        <<<<"  Compression: "/utf8, (float_to_str(Compression_ratio))/binary>>/binary,
            "x"/utf8>>
    ),
    Recovered = viva_tensor@sparsity:decompress(Sparse),
    Error = compute_error(Weights, Recovered),
    gleam_stdlib:println(
        <<"  Approximation error: "/utf8, (float_to_str(Error))/binary>>
    ),
    Dense_b = viva_tensor@tensor:random_normal([256, 64], +0.0, 0.5),
    {_, Speedup} = viva_tensor@sparsity:sparse_matmul(Sparse, Dense_b),
    gleam_stdlib:println(
        <<<<"  Theoretical speedup: "/utf8, (float_to_str(Speedup))/binary>>/binary,
            "x"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>).

-file("src/viva_tensor/demo.gleam", 418).
?DOC(" Convert tensor to matrix (List of List)\n").
-spec tensor_to_matrix(viva_tensor@tensor:tensor(), integer()) -> list(list(float())).
tensor_to_matrix(T, Cols) ->
    Data = viva_tensor@tensor:to_list(T),
    gleam@list:sized_chunk(Data, Cols).

-file("src/viva_tensor/demo.gleam", 190).
-spec demo_awq_quantization() -> nil.
demo_awq_quantization() ->
    gleam_stdlib:println(
        <<"┌─────────────────────────────────────────────────────────────┐"/utf8>>
    ),
    gleam_stdlib:println(
        <<"│ 4. AWQ - Activation-aware (MLSys 2024 Best Paper)          │"/utf8>>
    ),
    gleam_stdlib:println(
        <<"└─────────────────────────────────────────────────────────────┘"/utf8>>
    ),
    Weights = viva_tensor@tensor:random_normal([256, 256], +0.0, 0.3),
    Activations_tensor = viva_tensor@tensor:random_uniform([64, 256]),
    Calibration_data = tensor_to_matrix(Activations_tensor, 256),
    Original_size = (256 * 256) * 4,
    gleam_stdlib:println(
        <<"  Weights: [256, 256] = "/utf8,
            (format_bytes(Original_size))/binary>>
    ),
    gleam_stdlib:println(
        <<"  Calibration: [64, 256] (activations batch)"/utf8>>
    ),
    Config = viva_tensor@awq:default_config(),
    Quantized = viva_tensor@awq:quantize_awq(Weights, Calibration_data, Config),
    gleam_stdlib:println(
        <<<<"  AWQ quantizado: "/utf8,
                (erlang:integer_to_binary(erlang:element(7, Quantized)))/binary>>/binary,
            " bytes"/utf8>>
    ),
    Compression_ratio = case erlang:float(erlang:element(7, Quantized)) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Original_size) / Gleam@denominator
    end,
    gleam_stdlib:println(
        <<<<"  Compressão: "/utf8, (float_to_str(Compression_ratio))/binary>>/binary,
            "x"/utf8>>
    ),
    Recovered = viva_tensor@awq:dequantize_awq(Quantized),
    Error = compute_error(Weights, Recovered),
    gleam_stdlib:println(
        <<<<"  Erro médio: "/utf8, (float_to_str(Error * 100.0))/binary>>/binary,
            "%"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>).

-file("src/viva_tensor/demo.gleam", 22).
-spec main() -> nil.
main() ->
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"╔═══════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║          viva_tensor - Pure Gleam Tensor Library              ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                       DEMO COMPLETA                           ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚═══════════════════════════════════════════════════════════════╝"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    demo_basic_ops(),
    demo_int8_quantization(),
    demo_nf4_quantization(),
    demo_awq_quantization(),
    demo_flash_attention(),
    demo_sparsity(),
    demo_combined(),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"═══════════════════════════════════════════════════════════════"/utf8>>
    ),
    gleam_stdlib:println(<<"                    DEMO CONCLUÍDA!"/utf8>>),
    gleam_stdlib:println(
        <<"═══════════════════════════════════════════════════════════════"/utf8>>
    ).
