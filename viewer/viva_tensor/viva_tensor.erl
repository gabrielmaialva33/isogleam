-module(viva_tensor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor.gleam").
-export([zeros/1, ones/1, fill/2, from_list/1, from_list2d/1, vector/1, matrix/3, random_uniform/1, random_normal/3, xavier_init/2, he_init/2, add/2, sub/2, mul/2, 'div'/2, scale/2, map/2, sum/1, mean/1, max/1, min/1, argmax/1, argmin/1, variance/1, std/1, dot/2, matmul/2, matmul_vec/2, transpose/1, outer/2, reshape/2, flatten/1, squeeze/1, unsqueeze/2, norm/1, normalize/1, clamp/3, shape/1, size/1, rank/1, to_list/1, can_broadcast/2, add_broadcast/2, mul_broadcast/2, to_strided/1, to_contiguous/1, transpose_strided/1, is_contiguous/1, conv2d_config/0, conv2d_same/2, conv2d/3, pad2d/3, pad4d/3, max_pool2d/5, avg_pool2d/5, global_avg_pool2d/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " viva_tensor - Pure Gleam tensor library\n"
    "\n"
    " N-dimensional arrays with named axes, broadcasting, and zero-copy views.\n"
    "\n"
    " ## Features\n"
    " - NumPy-inspired API\n"
    " - Named tensors (Batch, Seq, Feature axes)\n"
    " - Broadcasting\n"
    " - Zero-copy transpose/reshape via strides\n"
    " - O(1) random access with Erlang arrays\n"
    "\n"
    " ## Quick Start\n"
    " ```gleam\n"
    " import viva_tensor as t\n"
    " import viva_tensor/axis\n"
    "\n"
    " // Create tensors\n"
    " let a = t.zeros([2, 3])\n"
    " let b = t.ones([2, 3])\n"
    "\n"
    " // Operations\n"
    " let c = t.add(a, b)\n"
    " let d = t.matmul(a, t.transpose(b))\n"
    "\n"
    " // Named tensors\n"
    " let named = t.named.zeros([axis.batch(32), axis.feature(128)])\n"
    " let summed = t.named.sum_along(named, axis.Batch)\n"
    " ```\n"
).

-file("src/viva_tensor.gleam", 38).
?DOC(" Create tensor of zeros\n").
-spec zeros(list(integer())) -> viva_tensor@tensor:tensor().
zeros(Shape) ->
    viva_tensor@tensor:zeros(Shape).

-file("src/viva_tensor.gleam", 43).
?DOC(" Create tensor of ones\n").
-spec ones(list(integer())) -> viva_tensor@tensor:tensor().
ones(Shape) ->
    viva_tensor@tensor:ones(Shape).

-file("src/viva_tensor.gleam", 48).
?DOC(" Create tensor filled with value\n").
-spec fill(list(integer()), float()) -> viva_tensor@tensor:tensor().
fill(Shape, Value) ->
    viva_tensor@tensor:fill(Shape, Value).

-file("src/viva_tensor.gleam", 53).
?DOC(" Create tensor from list (1D)\n").
-spec from_list(list(float())) -> viva_tensor@tensor:tensor().
from_list(Data) ->
    viva_tensor@tensor:from_list(Data).

-file("src/viva_tensor.gleam", 58).
?DOC(" Create 2D tensor from list of lists\n").
-spec from_list2d(list(list(float()))) -> {ok, viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
from_list2d(Rows) ->
    viva_tensor@tensor:from_list2d(Rows).

-file("src/viva_tensor.gleam", 65).
?DOC(" Create vector (1D tensor)\n").
-spec vector(list(float())) -> viva_tensor@tensor:tensor().
vector(Data) ->
    viva_tensor@tensor:vector(Data).

-file("src/viva_tensor.gleam", 70).
?DOC(" Create matrix (2D tensor)\n").
-spec matrix(integer(), integer(), list(float())) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
matrix(Rows, Cols, Data) ->
    viva_tensor@tensor:matrix(Rows, Cols, Data).

-file("src/viva_tensor.gleam", 83).
?DOC(" Tensor with uniform random values [0, 1)\n").
-spec random_uniform(list(integer())) -> viva_tensor@tensor:tensor().
random_uniform(Shape) ->
    viva_tensor@tensor:random_uniform(Shape).

-file("src/viva_tensor.gleam", 88).
?DOC(" Tensor with normal random values\n").
-spec random_normal(list(integer()), float(), float()) -> viva_tensor@tensor:tensor().
random_normal(Shape, Mean, Std) ->
    viva_tensor@tensor:random_normal(Shape, Mean, Std).

-file("src/viva_tensor.gleam", 93).
?DOC(" Xavier initialization for neural network weights\n").
-spec xavier_init(integer(), integer()) -> viva_tensor@tensor:tensor().
xavier_init(Fan_in, Fan_out) ->
    viva_tensor@tensor:xavier_init(Fan_in, Fan_out).

-file("src/viva_tensor.gleam", 98).
?DOC(" He initialization (for ReLU networks)\n").
-spec he_init(integer(), integer()) -> viva_tensor@tensor:tensor().
he_init(Fan_in, Fan_out) ->
    viva_tensor@tensor:he_init(Fan_in, Fan_out).

-file("src/viva_tensor.gleam", 107).
?DOC(" Element-wise addition\n").
-spec add(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
add(A, B) ->
    viva_tensor@tensor:add(A, B).

-file("src/viva_tensor.gleam", 115).
?DOC(" Element-wise subtraction\n").
-spec sub(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
sub(A, B) ->
    viva_tensor@tensor:sub(A, B).

-file("src/viva_tensor.gleam", 123).
?DOC(" Element-wise multiplication\n").
-spec mul(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
mul(A, B) ->
    viva_tensor@tensor:mul(A, B).

-file("src/viva_tensor.gleam", 131).
?DOC(" Element-wise division\n").
-spec 'div'(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
'div'(A, B) ->
    viva_tensor@tensor:'div'(A, B).

-file("src/viva_tensor.gleam", 139).
?DOC(" Scale by constant\n").
-spec scale(viva_tensor@tensor:tensor(), float()) -> viva_tensor@tensor:tensor().
scale(T, S) ->
    viva_tensor@tensor:scale(T, S).

-file("src/viva_tensor.gleam", 144).
?DOC(" Apply function to each element\n").
-spec map(viva_tensor@tensor:tensor(), fun((float()) -> float())) -> viva_tensor@tensor:tensor().
map(T, F) ->
    viva_tensor@tensor:map(T, F).

-file("src/viva_tensor.gleam", 153).
?DOC(" Sum all elements\n").
-spec sum(viva_tensor@tensor:tensor()) -> float().
sum(T) ->
    viva_tensor@tensor:sum(T).

-file("src/viva_tensor.gleam", 158).
?DOC(" Mean of all elements\n").
-spec mean(viva_tensor@tensor:tensor()) -> float().
mean(T) ->
    viva_tensor@tensor:mean(T).

-file("src/viva_tensor.gleam", 163).
?DOC(" Maximum value\n").
-spec max(viva_tensor@tensor:tensor()) -> float().
max(T) ->
    viva_tensor@tensor:max(T).

-file("src/viva_tensor.gleam", 168).
?DOC(" Minimum value\n").
-spec min(viva_tensor@tensor:tensor()) -> float().
min(T) ->
    viva_tensor@tensor:min(T).

-file("src/viva_tensor.gleam", 173).
?DOC(" Index of maximum value\n").
-spec argmax(viva_tensor@tensor:tensor()) -> integer().
argmax(T) ->
    viva_tensor@tensor:argmax(T).

-file("src/viva_tensor.gleam", 178).
?DOC(" Index of minimum value\n").
-spec argmin(viva_tensor@tensor:tensor()) -> integer().
argmin(T) ->
    viva_tensor@tensor:argmin(T).

-file("src/viva_tensor.gleam", 183).
?DOC(" Variance\n").
-spec variance(viva_tensor@tensor:tensor()) -> float().
variance(T) ->
    viva_tensor@tensor:variance(T).

-file("src/viva_tensor.gleam", 188).
?DOC(" Standard deviation\n").
-spec std(viva_tensor@tensor:tensor()) -> float().
std(T) ->
    viva_tensor@tensor:std(T).

-file("src/viva_tensor.gleam", 197).
?DOC(" Dot product of two vectors\n").
-spec dot(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> {ok,
        float()} |
    {error, viva_tensor@tensor:tensor_error()}.
dot(A, B) ->
    viva_tensor@tensor:dot(A, B).

-file("src/viva_tensor.gleam", 205).
?DOC(" Matrix-matrix multiplication\n").
-spec matmul(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
matmul(A, B) ->
    viva_tensor@tensor:matmul(A, B).

-file("src/viva_tensor.gleam", 213).
?DOC(" Matrix-vector multiplication\n").
-spec matmul_vec(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
matmul_vec(Mat, Vec) ->
    viva_tensor@tensor:matmul_vec(Mat, Vec).

-file("src/viva_tensor.gleam", 221).
?DOC(" Transpose matrix\n").
-spec transpose(viva_tensor@tensor:tensor()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
transpose(T) ->
    viva_tensor@tensor:transpose(T).

-file("src/viva_tensor.gleam", 226).
?DOC(" Outer product\n").
-spec outer(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
outer(A, B) ->
    viva_tensor@tensor:outer(A, B).

-file("src/viva_tensor.gleam", 238).
?DOC(" Reshape tensor\n").
-spec reshape(viva_tensor@tensor:tensor(), list(integer())) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
reshape(T, New_shape) ->
    viva_tensor@tensor:reshape(T, New_shape).

-file("src/viva_tensor.gleam", 246).
?DOC(" Flatten to 1D\n").
-spec flatten(viva_tensor@tensor:tensor()) -> viva_tensor@tensor:tensor().
flatten(T) ->
    viva_tensor@tensor:flatten(T).

-file("src/viva_tensor.gleam", 251).
?DOC(" Remove dimensions of size 1\n").
-spec squeeze(viva_tensor@tensor:tensor()) -> viva_tensor@tensor:tensor().
squeeze(T) ->
    viva_tensor@tensor:squeeze(T).

-file("src/viva_tensor.gleam", 256).
?DOC(" Add dimension of size 1\n").
-spec unsqueeze(viva_tensor@tensor:tensor(), integer()) -> viva_tensor@tensor:tensor().
unsqueeze(T, Axis_idx) ->
    viva_tensor@tensor:unsqueeze(T, Axis_idx).

-file("src/viva_tensor.gleam", 265).
?DOC(" L2 norm\n").
-spec norm(viva_tensor@tensor:tensor()) -> float().
norm(T) ->
    viva_tensor@tensor:norm(T).

-file("src/viva_tensor.gleam", 270).
?DOC(" Normalize to unit length\n").
-spec normalize(viva_tensor@tensor:tensor()) -> viva_tensor@tensor:tensor().
normalize(T) ->
    viva_tensor@tensor:normalize(T).

-file("src/viva_tensor.gleam", 275).
?DOC(" Clamp values\n").
-spec clamp(viva_tensor@tensor:tensor(), float(), float()) -> viva_tensor@tensor:tensor().
clamp(T, Min_val, Max_val) ->
    viva_tensor@tensor:clamp(T, Min_val, Max_val).

-file("src/viva_tensor.gleam", 280).
?DOC(" Get shape\n").
-spec shape(viva_tensor@tensor:tensor()) -> list(integer()).
shape(T) ->
    erlang:element(3, T).

-file("src/viva_tensor.gleam", 285).
?DOC(" Get total size\n").
-spec size(viva_tensor@tensor:tensor()) -> integer().
size(T) ->
    viva_tensor@tensor:size(T).

-file("src/viva_tensor.gleam", 290).
?DOC(" Get rank (number of dimensions)\n").
-spec rank(viva_tensor@tensor:tensor()) -> integer().
rank(T) ->
    viva_tensor@tensor:rank(T).

-file("src/viva_tensor.gleam", 295).
?DOC(" Convert to list\n").
-spec to_list(viva_tensor@tensor:tensor()) -> list(float()).
to_list(T) ->
    viva_tensor@tensor:to_list(T).

-file("src/viva_tensor.gleam", 304).
?DOC(" Check if shapes can broadcast\n").
-spec can_broadcast(list(integer()), list(integer())) -> boolean().
can_broadcast(A, B) ->
    viva_tensor@tensor:can_broadcast(A, B).

-file("src/viva_tensor.gleam", 309).
?DOC(" Add with broadcasting\n").
-spec add_broadcast(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
add_broadcast(A, B) ->
    viva_tensor@tensor:add_broadcast(A, B).

-file("src/viva_tensor.gleam", 317).
?DOC(" Multiply with broadcasting\n").
-spec mul_broadcast(viva_tensor@tensor:tensor(), viva_tensor@tensor:tensor()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
mul_broadcast(A, B) ->
    viva_tensor@tensor:mul_broadcast(A, B).

-file("src/viva_tensor.gleam", 329).
?DOC(" Convert to strided tensor (O(1) access)\n").
-spec to_strided(viva_tensor@tensor:tensor()) -> viva_tensor@tensor:tensor().
to_strided(T) ->
    viva_tensor@tensor:to_strided(T).

-file("src/viva_tensor.gleam", 334).
?DOC(" Convert to contiguous tensor\n").
-spec to_contiguous(viva_tensor@tensor:tensor()) -> viva_tensor@tensor:tensor().
to_contiguous(T) ->
    viva_tensor@tensor:to_contiguous(T).

-file("src/viva_tensor.gleam", 339).
?DOC(" Zero-copy transpose\n").
-spec transpose_strided(viva_tensor@tensor:tensor()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
transpose_strided(T) ->
    viva_tensor@tensor:transpose_strided(T).

-file("src/viva_tensor.gleam", 346).
?DOC(" Check if contiguous\n").
-spec is_contiguous(viva_tensor@tensor:tensor()) -> boolean().
is_contiguous(T) ->
    viva_tensor@tensor:is_contiguous(T).

-file("src/viva_tensor.gleam", 359).
?DOC(" Default conv2d config (3x3 kernel, stride 1, no padding)\n").
-spec conv2d_config() -> viva_tensor@tensor:conv2d_config().
conv2d_config() ->
    viva_tensor@tensor:conv2d_config().

-file("src/viva_tensor.gleam", 364).
?DOC(" Conv2d config with \"same\" padding (output same size as input)\n").
-spec conv2d_same(integer(), integer()) -> viva_tensor@tensor:conv2d_config().
conv2d_same(Kernel_h, Kernel_w) ->
    viva_tensor@tensor:conv2d_same(Kernel_h, Kernel_w).

-file("src/viva_tensor.gleam", 371).
?DOC(
    " 2D Convolution\n"
    " Input: [H, W] or [C, H, W] or [N, C, H, W]\n"
    " Kernel: [KH, KW] or [C, KH, KW] or [C_out, C_in, KH, KW]\n"
).
-spec conv2d(
    viva_tensor@tensor:tensor(),
    viva_tensor@tensor:tensor(),
    viva_tensor@tensor:conv2d_config()
) -> {ok, viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
conv2d(Input, Kernel, Config) ->
    viva_tensor@tensor:conv2d(Input, Kernel, Config).

-file("src/viva_tensor.gleam", 380).
?DOC(" Pad 2D tensor with zeros\n").
-spec pad2d(viva_tensor@tensor:tensor(), integer(), integer()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
pad2d(T, Pad_h, Pad_w) ->
    viva_tensor@tensor:pad2d(T, Pad_h, Pad_w).

-file("src/viva_tensor.gleam", 389).
?DOC(" Pad 4D tensor (batch) with zeros\n").
-spec pad4d(viva_tensor@tensor:tensor(), integer(), integer()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
pad4d(T, Pad_h, Pad_w) ->
    viva_tensor@tensor:pad4d(T, Pad_h, Pad_w).

-file("src/viva_tensor.gleam", 399).
?DOC(
    " Max pooling 2D\n"
    " Input: [H, W] or [N, C, H, W]\n"
).
-spec max_pool2d(
    viva_tensor@tensor:tensor(),
    integer(),
    integer(),
    integer(),
    integer()
) -> {ok, viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
max_pool2d(Input, Pool_h, Pool_w, Stride_h, Stride_w) ->
    viva_tensor@tensor:max_pool2d(Input, Pool_h, Pool_w, Stride_h, Stride_w).

-file("src/viva_tensor.gleam", 410).
?DOC(" Average pooling 2D\n").
-spec avg_pool2d(
    viva_tensor@tensor:tensor(),
    integer(),
    integer(),
    integer(),
    integer()
) -> {ok, viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
avg_pool2d(Input, Pool_h, Pool_w, Stride_h, Stride_w) ->
    viva_tensor@tensor:avg_pool2d(Input, Pool_h, Pool_w, Stride_h, Stride_w).

-file("src/viva_tensor.gleam", 421).
?DOC(" Global average pooling - reduces [N, C, H, W] to [N, C, 1, 1]\n").
-spec global_avg_pool2d(viva_tensor@tensor:tensor()) -> {ok,
        viva_tensor@tensor:tensor()} |
    {error, viva_tensor@tensor:tensor_error()}.
global_avg_pool2d(Input) ->
    viva_tensor@tensor:global_avg_pool2d(Input).
