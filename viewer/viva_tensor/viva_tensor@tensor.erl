-module(viva_tensor@tensor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/tensor.gleam").
-export([zeros/1, ones/1, fill/2, from_list/1, from_list2d/1, vector/1, matrix/3, shape/1, size/1, rank/1, rows/1, cols/1, can_broadcast/2, broadcast_shape/2, dim/2, is_contiguous/1, conv2d_config/0, conv2d_same/2, random_uniform/1, random_normal/3, xavier_init/2, he_init/2, to_strided/1, transpose_strided/1, get_data/1, get/2, get2d/3, get_row/2, get_col/2, map/2, map_indexed/2, add/2, sub/2, mul/2, 'div'/2, scale/2, add_scalar/2, negate/1, sum/1, product/1, mean/1, max/1, min/1, argmax/1, argmin/1, variance/1, std/1, sum_axis/2, mean_axis/2, dot/2, matmul_vec/2, matmul/2, transpose/1, outer/2, to_list/1, to_list2d/1, clone/1, reshape/2, flatten/1, concat/1, concat_axis/2, take_first/2, take_last/2, slice/3, norm/1, normalize/1, clamp/3, squeeze/1, squeeze_axis/2, unsqueeze/2, stack/2, expand_dims/2, to_contiguous/1, get_fast/2, get2d_fast/3, broadcast_to/2, add_broadcast/2, mul_broadcast/2, pad2d/3, pad4d/3, conv2d/3, max_pool2d/5, avg_pool2d/5, global_avg_pool2d/1]).
-export_type([erlang_array/0, tensor/0, tensor_error/0, conv2d_config/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Tensor - N-dimensional arrays for numerical computing\n"
    "\n"
    " Design: NumPy-inspired with strides for zero-copy views.\n"
    " Uses Erlang :array for O(1) access + strides for efficient transpose/reshape.\n"
).

-type erlang_array() :: any().

-type tensor() :: {tensor, list(float()), list(integer())} |
    {strided_tensor,
        erlang_array(),
        list(integer()),
        list(integer()),
        integer()}.

-type tensor_error() :: {shape_mismatch, list(integer()), list(integer())} |
    {invalid_shape, binary()} |
    {dimension_error, binary()} |
    {broadcast_error, list(integer()), list(integer())}.

-type conv2d_config() :: {conv2d_config,
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer()}.

-file("src/viva_tensor/tensor.gleam", 46).
?DOC(" Create tensor of zeros\n").
-spec zeros(list(integer())) -> tensor().
zeros(Shape) ->
    Size = gleam@list:fold(Shape, 1, fun(Acc, Dim) -> Acc * Dim end),
    {tensor, gleam@list:repeat(+0.0, Size), Shape}.

-file("src/viva_tensor/tensor.gleam", 52).
?DOC(" Create tensor of ones\n").
-spec ones(list(integer())) -> tensor().
ones(Shape) ->
    Size = gleam@list:fold(Shape, 1, fun(Acc, Dim) -> Acc * Dim end),
    {tensor, gleam@list:repeat(1.0, Size), Shape}.

-file("src/viva_tensor/tensor.gleam", 58).
?DOC(" Create tensor filled with value\n").
-spec fill(list(integer()), float()) -> tensor().
fill(Shape, Value) ->
    Size = gleam@list:fold(Shape, 1, fun(Acc, Dim) -> Acc * Dim end),
    {tensor, gleam@list:repeat(Value, Size), Shape}.

-file("src/viva_tensor/tensor.gleam", 64).
?DOC(" Create tensor from list (1D)\n").
-spec from_list(list(float())) -> tensor().
from_list(Data) ->
    {tensor, Data, [erlang:length(Data)]}.

-file("src/viva_tensor/tensor.gleam", 69).
?DOC(" Create 2D tensor (matrix) from list of lists\n").
-spec from_list2d(list(list(float()))) -> {ok, tensor()} |
    {error, tensor_error()}.
from_list2d(Rows) ->
    case Rows of
        [] ->
            {ok, {tensor, [], [0, 0]}};

        [First | Rest] ->
            Cols = erlang:length(First),
            Valid = gleam@list:all(
                Rest,
                fun(Row) -> erlang:length(Row) =:= Cols end
            ),
            case Valid of
                false ->
                    {error,
                        {invalid_shape, <<"Rows have different lengths"/utf8>>}};

                true ->
                    Data = lists:append(Rows),
                    Num_rows = erlang:length(Rows),
                    {ok, {tensor, Data, [Num_rows, Cols]}}
            end
    end.

-file("src/viva_tensor/tensor.gleam", 89).
?DOC(" Create vector (1D tensor)\n").
-spec vector(list(float())) -> tensor().
vector(Data) ->
    from_list(Data).

-file("src/viva_tensor/tensor.gleam", 94).
?DOC(" Create matrix (2D tensor) with explicit dimensions\n").
-spec matrix(integer(), integer(), list(float())) -> {ok, tensor()} |
    {error, tensor_error()}.
matrix(Rows, Cols, Data) ->
    Expected_size = Rows * Cols,
    Actual_size = erlang:length(Data),
    case Expected_size =:= Actual_size of
        true ->
            {ok, {tensor, Data, [Rows, Cols]}};

        false ->
            {error,
                {invalid_shape,
                    <<<<<<"Expected "/utf8,
                                (erlang:integer_to_binary(Expected_size))/binary>>/binary,
                            " elements, got "/utf8>>/binary,
                        (erlang:integer_to_binary(Actual_size))/binary>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 119).
?DOC(" Get tensor shape\n").
-spec shape(tensor()) -> list(integer()).
shape(T) ->
    case T of
        {tensor, _, S} ->
            S;

        {strided_tensor, _, S@1, _, _} ->
            S@1
    end.

-file("src/viva_tensor/tensor.gleam", 148).
?DOC(" Total number of elements\n").
-spec size(tensor()) -> integer().
size(T) ->
    case T of
        {tensor, Data, _} ->
            erlang:length(Data);

        {strided_tensor, _, Shape, _, _} ->
            gleam@list:fold(Shape, 1, fun(Acc, Dim) -> Acc * Dim end)
    end.

-file("src/viva_tensor/tensor.gleam", 157).
?DOC(" Number of dimensions (rank)\n").
-spec rank(tensor()) -> integer().
rank(T) ->
    erlang:length(erlang:element(3, T)).

-file("src/viva_tensor/tensor.gleam", 170).
?DOC(" Return number of rows (for matrices)\n").
-spec rows(tensor()) -> integer().
rows(T) ->
    case erlang:element(3, T) of
        [R | _] ->
            R;

        [] ->
            0
    end.

-file("src/viva_tensor/tensor.gleam", 178).
?DOC(" Return number of columns (for matrices)\n").
-spec cols(tensor()) -> integer().
cols(T) ->
    case erlang:element(3, T) of
        [_, C | _] ->
            C;

        [N] ->
            N;

        [] ->
            0
    end.

-file("src/viva_tensor/tensor.gleam", 511).
?DOC(" Remove element at index from list\n").
-spec remove_at_index(list(DWJ), integer()) -> list(DWJ).
remove_at_index(Lst, Idx) ->
    _pipe = Lst,
    _pipe@1 = gleam@list:index_map(_pipe, fun(Item, I) -> {Item, I} end),
    _pipe@2 = gleam@list:filter(
        _pipe@1,
        fun(Pair) -> erlang:element(2, Pair) /= Idx end
    ),
    gleam@list:map(_pipe@2, fun(Pair@1) -> erlang:element(1, Pair@1) end).

-file("src/viva_tensor/tensor.gleam", 1070).
?DOC(" Check if two shapes can be broadcast together\n").
-spec can_broadcast(list(integer()), list(integer())) -> boolean().
can_broadcast(A, B) ->
    {Longer, Shorter} = case erlang:length(A) >= erlang:length(B) of
        true ->
            {A, B};

        false ->
            {B, A}
    end,
    Diff = erlang:length(Longer) - erlang:length(Shorter),
    Padded = lists:append(gleam@list:repeat(1, Diff), Shorter),
    _pipe = gleam@list:zip(Longer, Padded),
    gleam@list:all(
        _pipe,
        fun(Pair) ->
            {Dim_a, Dim_b} = Pair,
            ((Dim_a =:= Dim_b) orelse (Dim_a =:= 1)) orelse (Dim_b =:= 1)
        end
    ).

-file("src/viva_tensor/tensor.gleam", 1087).
?DOC(" Compute broadcast shape\n").
-spec broadcast_shape(list(integer()), list(integer())) -> {ok, list(integer())} |
    {error, tensor_error()}.
broadcast_shape(A, B) ->
    case can_broadcast(A, B) of
        false ->
            {error, {broadcast_error, A, B}};

        true ->
            Max_rank = gleam@int:max(erlang:length(A), erlang:length(B)),
            Diff_a = Max_rank - erlang:length(A),
            Diff_b = Max_rank - erlang:length(B),
            Padded_a = lists:append(gleam@list:repeat(1, Diff_a), A),
            Padded_b = lists:append(gleam@list:repeat(1, Diff_b), B),
            Result_shape = begin
                _pipe = gleam@list:zip(Padded_a, Padded_b),
                gleam@list:map(
                    _pipe,
                    fun(Pair) ->
                        {Dim_a, Dim_b} = Pair,
                        gleam@int:max(Dim_a, Dim_b)
                    end
                )
            end,
            {ok, Result_shape}
    end.

-file("src/viva_tensor/tensor.gleam", 1309).
-spec list_at_int(list(integer()), integer()) -> {ok, integer()} | {error, nil}.
list_at_int(Lst, Index) ->
    case Index < 0 of
        true ->
            {error, nil};

        false ->
            _pipe = Lst,
            _pipe@1 = gleam@list:drop(_pipe, Index),
            gleam@list:first(_pipe@1)
    end.

-file("src/viva_tensor/tensor.gleam", 162).
?DOC(" Specific dimension\n").
-spec dim(tensor(), integer()) -> {ok, integer()} | {error, tensor_error()}.
dim(T, Axis) ->
    _pipe = list_at_int(erlang:element(3, T), Axis),
    gleam@result:map_error(
        _pipe,
        fun(_) ->
            {dimension_error,
                <<<<"Axis "/utf8, (erlang:integer_to_binary(Axis))/binary>>/binary,
                    " out of bounds"/utf8>>}
        end
    ).

-file("src/viva_tensor/tensor.gleam", 1319).
-spec list_at_float(list(float()), integer()) -> {ok, float()} | {error, nil}.
list_at_float(Lst, Index) ->
    case Index < 0 of
        true ->
            {error, nil};

        false ->
            _pipe = Lst,
            _pipe@1 = gleam@list:drop(_pipe, Index),
            gleam@list:first(_pipe@1)
    end.

-file("src/viva_tensor/tensor.gleam", 1337).
-spec flat_to_multi(integer(), list(integer())) -> list(integer()).
flat_to_multi(Flat, Shape) ->
    Reversed = lists:reverse(Shape),
    {Indices, _} = gleam@list:fold(
        Reversed,
        {[], Flat},
        fun(Acc, Dim) ->
            {Idxs, Remaining} = Acc,
            Idx = case Dim of
                0 -> 0;
                Gleam@denominator -> Remaining rem Gleam@denominator
            end,
            Next = case Dim of
                0 -> 0;
                Gleam@denominator@1 -> Remaining div Gleam@denominator@1
            end,
            {[Idx | Idxs], Next}
        end
    ),
    Indices.

-file("src/viva_tensor/tensor.gleam", 1349).
-spec compute_strides(list(integer())) -> list(integer()).
compute_strides(Shape) ->
    Reversed = lists:reverse(Shape),
    {Strides, _} = gleam@list:fold(
        Reversed,
        {[], 1},
        fun(Acc, Dim) ->
            {S, Running} = Acc,
            {[Running | S], Running * Dim}
        end
    ),
    Strides.

-file("src/viva_tensor/tensor.gleam", 519).
?DOC(" Compute flat index when summing along an axis\n").
-spec compute_index_with_axis(list(integer()), integer(), integer(), integer()) -> integer().
compute_index_with_axis(Shape, Out_idx, Axis_idx, Axis_pos) ->
    Strides = compute_strides(Shape),
    _ = erlang:length(Shape),
    Shape_without_axis = remove_at_index(Shape, Axis_idx),
    Strides_without_axis = compute_strides(Shape_without_axis),
    Out_coords = begin
        _pipe = gleam@list:range(0, erlang:length(Shape_without_axis) - 1),
        gleam@list:map(
            _pipe,
            fun(I) ->
                Stride = case begin
                    _pipe@1 = gleam@list:drop(Strides_without_axis, I),
                    gleam@list:first(_pipe@1)
                end of
                    {ok, S} ->
                        S;

                    {error, _} ->
                        1
                end,
                case (case begin
                    _pipe@2 = gleam@list:drop(Shape_without_axis, I),
                    gleam@list:first(_pipe@2)
                end of
                    {ok, D} ->
                        D;

                    {error, _} ->
                        1
                end) of
                    0 -> 0;
                    Gleam@denominator@1 -> (case Stride of
                        0 -> 0;
                        Gleam@denominator -> Out_idx div Gleam@denominator
                    end) rem Gleam@denominator@1
                end
            end
        )
    end,
    {Before, After} = gleam@list:split(Out_coords, Axis_idx),
    Full_coords = lists:append([Before, [Axis_pos], After]),
    _pipe@3 = gleam@list:zip(Full_coords, Strides),
    gleam@list:fold(
        _pipe@3,
        0,
        fun(Acc, Pair) ->
            {Coord, Stride@1} = Pair,
            Acc + (Coord * Stride@1)
        end
    ).

-file("src/viva_tensor/tensor.gleam", 1258).
?DOC(" Check if tensor is contiguous in memory\n").
-spec is_contiguous(tensor()) -> boolean().
is_contiguous(T) ->
    case T of
        {tensor, _, _} ->
            true;

        {strided_tensor, _, Shape, Strides, _} ->
            Expected_strides = compute_strides(Shape),
            Strides =:= Expected_strides
    end.

-file("src/viva_tensor/tensor.gleam", 1392).
-spec multi_to_flat(list(integer()), list(integer())) -> integer().
multi_to_flat(Indices, Shape) ->
    Strides = compute_strides(Shape),
    _pipe = gleam@list:zip(Indices, Strides),
    gleam@list:fold(
        _pipe,
        0,
        fun(Acc, Pair) ->
            {Idx, Stride} = Pair,
            Acc + (Idx * Stride)
        end
    ).

-file("src/viva_tensor/tensor.gleam", 1419).
?DOC(" Default conv2d config (3x3 kernel, stride 1, no padding)\n").
-spec conv2d_config() -> conv2d_config().
conv2d_config() ->
    {conv2d_config, 3, 3, 1, 1, 0, 0}.

-file("src/viva_tensor/tensor.gleam", 1431).
?DOC(" Conv2d config with \"same\" padding (output same size as input)\n").
-spec conv2d_same(integer(), integer()) -> conv2d_config().
conv2d_same(Kernel_h, Kernel_w) ->
    {conv2d_config, Kernel_h, Kernel_w, 1, 1, Kernel_h div 2, Kernel_w div 2}.

-file("src/viva_tensor/tensor.gleam", 2306).
-spec string_join(list(binary()), binary()) -> binary().
string_join(Strings, Sep) ->
    case Strings of
        [] ->
            <<""/utf8>>;

        [S] ->
            S;

        [S@1 | Rest] ->
            <<<<S@1/binary, Sep/binary>>/binary,
                (string_join(Rest, Sep))/binary>>
    end.

-file("src/viva_tensor/tensor.gleam", 2300).
?DOC(" Helper to convert shape to string for error messages\n").
-spec shape_to_string(list(integer())) -> binary().
shape_to_string(Shp) ->
    <<<<"["/utf8,
            (begin
                _pipe = gleam@list:map(Shp, fun erlang:integer_to_binary/1),
                string_join(_pipe, <<", "/utf8>>)
            end)/binary>>/binary,
        "]"/utf8>>.

-file("src/viva_tensor/tensor.gleam", 1017).
?DOC(" Tensor with uniform random values [0, 1)\n").
-spec random_uniform(list(integer())) -> tensor().
random_uniform(Shape) ->
    Size_val = gleam@list:fold(Shape, 1, fun(Acc, Dim) -> Acc * Dim end),
    Data = begin
        _pipe = gleam@list:range(1, Size_val),
        gleam@list:map(_pipe, fun(_) -> rand:uniform() end)
    end,
    {tensor, Data, Shape}.

-file("src/viva_tensor/tensor.gleam", 1026).
?DOC(" Tensor with normal random values (approx via Box-Muller)\n").
-spec random_normal(list(integer()), float(), float()) -> tensor().
random_normal(Shape, Mean_val, Std_val) ->
    Size_val = gleam@list:fold(Shape, 1, fun(Acc, Dim) -> Acc * Dim end),
    Data = begin
        _pipe = gleam@list:range(1, Size_val),
        gleam@list:map(
            _pipe,
            fun(_) ->
                U1 = gleam@float:max(rand:uniform(), 0.0001),
                U2 = rand:uniform(),
                Z = math:sqrt(-2.0 * math:log(U1)) * math:cos(
                    (2.0 * 3.14159265359) * U2
                ),
                Mean_val + (Z * Std_val)
            end
        )
    end,
    {tensor, Data, Shape}.

-file("src/viva_tensor/tensor.gleam", 1046).
?DOC(" Xavier initialization for weights\n").
-spec xavier_init(integer(), integer()) -> tensor().
xavier_init(Fan_in, Fan_out) ->
    Limit = math:sqrt(case erlang:float(Fan_in + Fan_out) of
            +0.0 -> +0.0;
            -0.0 -> -0.0;
            Gleam@denominator -> 6.0 / Gleam@denominator
        end),
    Data = begin
        _pipe = gleam@list:range(1, Fan_in * Fan_out),
        gleam@list:map(
            _pipe,
            fun(_) ->
                R = rand:uniform(),
                ((R * 2.0) * Limit) - Limit
            end
        )
    end,
    {tensor, Data, [Fan_out, Fan_in]}.

-file("src/viva_tensor/tensor.gleam", 1059).
?DOC(" He initialization (for ReLU)\n").
-spec he_init(integer(), integer()) -> tensor().
he_init(Fan_in, Fan_out) ->
    Std_val = math:sqrt(case erlang:float(Fan_in) of
            +0.0 -> +0.0;
            -0.0 -> -0.0;
            Gleam@denominator -> 2.0 / Gleam@denominator
        end),
    random_normal([Fan_out, Fan_in], +0.0, Std_val).

-file("src/viva_tensor/tensor.gleam", 1333).
-spec list_to_array(list(float())) -> erlang_array().
list_to_array(Lst) ->
    viva_tensor_ffi:list_to_array(Lst).

-file("src/viva_tensor/tensor.gleam", 1207).
?DOC(" Convert regular tensor to strided (O(n) once, then O(1) access)\n").
-spec to_strided(tensor()) -> tensor().
to_strided(T) ->
    case T of
        {strided_tensor, _, _, _, _} ->
            T;

        {tensor, Data, Shape} ->
            Storage = list_to_array(Data),
            Strides = compute_strides(Shape),
            {strided_tensor, Storage, Shape, Strides, 0}
    end.

-file("src/viva_tensor/tensor.gleam", 1230).
?DOC(" ZERO-COPY TRANSPOSE - just swap strides and shape!\n").
-spec transpose_strided(tensor()) -> {ok, tensor()} | {error, tensor_error()}.
transpose_strided(T) ->
    case T of
        {tensor, _, Shape} ->
            case Shape of
                [_, _] ->
                    Strided = to_strided(T),
                    transpose_strided(Strided);

                _ ->
                    {error,
                        {dimension_error,
                            <<"Transpose requires 2D tensor"/utf8>>}}
            end;

        {strided_tensor, Storage, Shape@1, Strides, Offset} ->
            case {Shape@1, Strides} of
                {[M, N], [S0, S1]} ->
                    {ok, {strided_tensor, Storage, [N, M], [S1, S0], Offset}};

                {_, _} ->
                    {error,
                        {dimension_error,
                            <<"Transpose requires 2D tensor"/utf8>>}}
            end
    end.

-file("src/viva_tensor/tensor.gleam", 1329).
-spec array_get(erlang_array(), integer()) -> float().
array_get(Arr, Index) ->
    viva_tensor_ffi:array_get(Arr, Index).

-file("src/viva_tensor/tensor.gleam", 127).
?DOC(" Extract data as list from any tensor variant\n").
-spec get_data(tensor()) -> list(float()).
get_data(T) ->
    case T of
        {tensor, Data, _} ->
            Data;

        {strided_tensor, Storage, Shape, Strides, Offset} ->
            Total_size = gleam@list:fold(
                Shape,
                1,
                fun(Acc, Dim) -> Acc * Dim end
            ),
            _pipe = gleam@list:range(0, Total_size - 1),
            gleam@list:map(
                _pipe,
                fun(Flat_idx) ->
                    Indices = flat_to_multi(Flat_idx, Shape),
                    Idx = begin
                        _pipe@1 = gleam@list:zip(Indices, Strides),
                        gleam@list:fold(
                            _pipe@1,
                            Offset,
                            fun(Acc@1, Pair) ->
                                {I, S} = Pair,
                                Acc@1 + (I * S)
                            end
                        )
                    end,
                    array_get(Storage, Idx)
                end
            )
    end.

-file("src/viva_tensor/tensor.gleam", 191).
?DOC(" Access element by linear index\n").
-spec get(tensor(), integer()) -> {ok, float()} | {error, tensor_error()}.
get(T, Index) ->
    case T of
        {tensor, Data, _} ->
            _pipe = list_at_float(Data, Index),
            gleam@result:map_error(
                _pipe,
                fun(_) ->
                    {dimension_error,
                        <<<<"Index "/utf8,
                                (erlang:integer_to_binary(Index))/binary>>/binary,
                            " out of bounds"/utf8>>}
                end
            );

        {strided_tensor, Storage, Shape, Strides, Offset} ->
            Indices = flat_to_multi(Index, Shape),
            Flat_idx = begin
                _pipe@1 = gleam@list:zip(Indices, Strides),
                gleam@list:fold(
                    _pipe@1,
                    Offset,
                    fun(Acc, Pair) ->
                        {I, S} = Pair,
                        Acc + (I * S)
                    end
                )
            end,
            {ok, array_get(Storage, Flat_idx)}
    end.

-file("src/viva_tensor/tensor.gleam", 212).
?DOC(" Access 2D element\n").
-spec get2d(tensor(), integer(), integer()) -> {ok, float()} |
    {error, tensor_error()}.
get2d(T, Row, Col) ->
    case erlang:element(3, T) of
        [_, Num_cols] ->
            Index = (Row * Num_cols) + Col,
            get(T, Index);

        _ ->
            {error, {dimension_error, <<"Tensor is not 2D"/utf8>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 223).
?DOC(" Get matrix row as vector\n").
-spec get_row(tensor(), integer()) -> {ok, tensor()} | {error, tensor_error()}.
get_row(T, Row_idx) ->
    case erlang:element(3, T) of
        [Num_rows, Num_cols] ->
            case (Row_idx >= 0) andalso (Row_idx < Num_rows) of
                true ->
                    Data = get_data(T),
                    Start = Row_idx * Num_cols,
                    Row_data = begin
                        _pipe = Data,
                        _pipe@1 = gleam@list:drop(_pipe, Start),
                        gleam@list:take(_pipe@1, Num_cols)
                    end,
                    {ok, from_list(Row_data)};

                false ->
                    {error,
                        {dimension_error, <<"Row index out of bounds"/utf8>>}}
            end;

        _ ->
            {error, {dimension_error, <<"Tensor is not 2D"/utf8>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 244).
?DOC(" Get matrix column as vector\n").
-spec get_col(tensor(), integer()) -> {ok, tensor()} | {error, tensor_error()}.
get_col(T, Col_idx) ->
    case erlang:element(3, T) of
        [Num_rows, Num_cols] ->
            case (Col_idx >= 0) andalso (Col_idx < Num_cols) of
                true ->
                    Col_data = begin
                        _pipe = gleam@list:range(0, Num_rows - 1),
                        gleam@list:filter_map(
                            _pipe,
                            fun(Row) -> get2d(T, Row, Col_idx) end
                        )
                    end,
                    {ok, from_list(Col_data)};

                false ->
                    {error,
                        {dimension_error, <<"Column index out of bounds"/utf8>>}}
            end;

        _ ->
            {error, {dimension_error, <<"Tensor is not 2D"/utf8>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 266).
?DOC(" Apply function to each element\n").
-spec map(tensor(), fun((float()) -> float())) -> tensor().
map(T, F) ->
    Data = get_data(T),
    {tensor, gleam@list:map(Data, F), erlang:element(3, T)}.

-file("src/viva_tensor/tensor.gleam", 272).
?DOC(" Apply function with index\n").
-spec map_indexed(tensor(), fun((float(), integer()) -> float())) -> tensor().
map_indexed(T, F) ->
    Data = get_data(T),
    {tensor,
        gleam@list:index_map(Data, fun(X, I) -> F(X, I) end),
        erlang:element(3, T)}.

-file("src/viva_tensor/tensor.gleam", 278).
?DOC(" Element-wise addition\n").
-spec add(tensor(), tensor()) -> {ok, tensor()} | {error, tensor_error()}.
add(A, B) ->
    case erlang:element(3, A) =:= erlang:element(3, B) of
        true ->
            A_data = get_data(A),
            B_data = get_data(B),
            Data = gleam@list:map2(A_data, B_data, fun(X, Y) -> X + Y end),
            {ok, {tensor, Data, erlang:element(3, A)}};

        false ->
            {error,
                {shape_mismatch, erlang:element(3, A), erlang:element(3, B)}}
    end.

-file("src/viva_tensor/tensor.gleam", 291).
?DOC(" Element-wise subtraction\n").
-spec sub(tensor(), tensor()) -> {ok, tensor()} | {error, tensor_error()}.
sub(A, B) ->
    case erlang:element(3, A) =:= erlang:element(3, B) of
        true ->
            A_data = get_data(A),
            B_data = get_data(B),
            Data = gleam@list:map2(A_data, B_data, fun(X, Y) -> X - Y end),
            {ok, {tensor, Data, erlang:element(3, A)}};

        false ->
            {error,
                {shape_mismatch, erlang:element(3, A), erlang:element(3, B)}}
    end.

-file("src/viva_tensor/tensor.gleam", 304).
?DOC(" Element-wise multiplication (Hadamard)\n").
-spec mul(tensor(), tensor()) -> {ok, tensor()} | {error, tensor_error()}.
mul(A, B) ->
    case erlang:element(3, A) =:= erlang:element(3, B) of
        true ->
            A_data = get_data(A),
            B_data = get_data(B),
            Data = gleam@list:map2(A_data, B_data, fun(X, Y) -> X * Y end),
            {ok, {tensor, Data, erlang:element(3, A)}};

        false ->
            {error,
                {shape_mismatch, erlang:element(3, A), erlang:element(3, B)}}
    end.

-file("src/viva_tensor/tensor.gleam", 317).
?DOC(" Element-wise division\n").
-spec 'div'(tensor(), tensor()) -> {ok, tensor()} | {error, tensor_error()}.
'div'(A, B) ->
    case erlang:element(3, A) =:= erlang:element(3, B) of
        true ->
            A_data = get_data(A),
            B_data = get_data(B),
            Data = gleam@list:map2(A_data, B_data, fun(X, Y) -> case Y of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> X / Gleam@denominator
                    end end),
            {ok, {tensor, Data, erlang:element(3, A)}};

        false ->
            {error,
                {shape_mismatch, erlang:element(3, A), erlang:element(3, B)}}
    end.

-file("src/viva_tensor/tensor.gleam", 330).
?DOC(" Scale by constant\n").
-spec scale(tensor(), float()) -> tensor().
scale(T, S) ->
    map(T, fun(X) -> X * S end).

-file("src/viva_tensor/tensor.gleam", 335).
?DOC(" Add constant\n").
-spec add_scalar(tensor(), float()) -> tensor().
add_scalar(T, S) ->
    map(T, fun(X) -> X + S end).

-file("src/viva_tensor/tensor.gleam", 340).
?DOC(" Negation\n").
-spec negate(tensor()) -> tensor().
negate(T) ->
    scale(T, -1.0).

-file("src/viva_tensor/tensor.gleam", 349).
?DOC(" Sum all elements\n").
-spec sum(tensor()) -> float().
sum(T) ->
    Data = get_data(T),
    gleam@list:fold(Data, +0.0, fun(Acc, X) -> Acc + X end).

-file("src/viva_tensor/tensor.gleam", 355).
?DOC(" Product of all elements\n").
-spec product(tensor()) -> float().
product(T) ->
    Data = get_data(T),
    gleam@list:fold(Data, 1.0, fun(Acc, X) -> Acc * X end).

-file("src/viva_tensor/tensor.gleam", 361).
?DOC(" Mean\n").
-spec mean(tensor()) -> float().
mean(T) ->
    S = sum(T),
    N = erlang:float(size(T)),
    case N > +0.0 of
        true ->
            case N of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> S / Gleam@denominator
            end;

        false ->
            +0.0
    end.

-file("src/viva_tensor/tensor.gleam", 371).
?DOC(" Maximum value\n").
-spec max(tensor()) -> float().
max(T) ->
    Data = get_data(T),
    case Data of
        [] ->
            +0.0;

        [First | Rest] ->
            gleam@list:fold(
                Rest,
                First,
                fun(Acc, X) -> gleam@float:max(Acc, X) end
            )
    end.

-file("src/viva_tensor/tensor.gleam", 380).
?DOC(" Minimum value\n").
-spec min(tensor()) -> float().
min(T) ->
    Data = get_data(T),
    case Data of
        [] ->
            +0.0;

        [First | Rest] ->
            gleam@list:fold(
                Rest,
                First,
                fun(Acc, X) -> gleam@float:min(Acc, X) end
            )
    end.

-file("src/viva_tensor/tensor.gleam", 389).
?DOC(" Argmax - index of largest element\n").
-spec argmax(tensor()) -> integer().
argmax(T) ->
    Data = get_data(T),
    case Data of
        [] ->
            0;

        [First | Rest] ->
            {Idx, _, _} = gleam@list:fold(
                Rest,
                {0, First, 1},
                fun(Acc, X) ->
                    {Best_idx, Best_val, Curr_idx} = Acc,
                    case X > Best_val of
                        true ->
                            {Curr_idx, X, Curr_idx + 1};

                        false ->
                            {Best_idx, Best_val, Curr_idx + 1}
                    end
                end
            ),
            Idx
    end.

-file("src/viva_tensor/tensor.gleam", 408).
?DOC(" Argmin - index of smallest element\n").
-spec argmin(tensor()) -> integer().
argmin(T) ->
    Data = get_data(T),
    case Data of
        [] ->
            0;

        [First | Rest] ->
            {Idx, _, _} = gleam@list:fold(
                Rest,
                {0, First, 1},
                fun(Acc, X) ->
                    {Best_idx, Best_val, Curr_idx} = Acc,
                    case X < Best_val of
                        true ->
                            {Curr_idx, X, Curr_idx + 1};

                        false ->
                            {Best_idx, Best_val, Curr_idx + 1}
                    end
                end
            ),
            Idx
    end.

-file("src/viva_tensor/tensor.gleam", 427).
?DOC(" Variance of all elements\n").
-spec variance(tensor()) -> float().
variance(T) ->
    Data = get_data(T),
    M = mean(T),
    Squared_diffs = gleam@list:map(
        Data,
        fun(X) ->
            Diff = X - M,
            Diff * Diff
        end
    ),
    N = erlang:float(size(T)),
    case N > +0.0 of
        true ->
            case N of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> gleam@list:fold(
                    Squared_diffs,
                    +0.0,
                    fun(Acc, X@1) -> Acc + X@1 end
                )
                / Gleam@denominator
            end;

        false ->
            +0.0
    end.

-file("src/viva_tensor/tensor.gleam", 443).
?DOC(" Standard deviation\n").
-spec std(tensor()) -> float().
std(T) ->
    math:sqrt(variance(T)).

-file("src/viva_tensor/tensor.gleam", 449).
?DOC(
    " Sum along a specific axis\n"
    " For a [2, 3] tensor, sum_axis(_, 0) gives [3], sum_axis(_, 1) gives [2]\n"
).
-spec sum_axis(tensor(), integer()) -> {ok, tensor()} | {error, tensor_error()}.
sum_axis(T, Axis_idx) ->
    R = rank(T),
    case (Axis_idx >= 0) andalso (Axis_idx < R) of
        false ->
            {error, {dimension_error, <<"Invalid axis index"/utf8>>}};

        true ->
            case erlang:element(3, T) of
                [] ->
                    {error, {dimension_error, <<"Cannot reduce scalar"/utf8>>}};

                [_] ->
                    {ok, {tensor, [sum(T)], [1]}};

                _ ->
                    Axis_size = case begin
                        _pipe = gleam@list:drop(erlang:element(3, T), Axis_idx),
                        gleam@list:first(_pipe)
                    end of
                        {ok, S} ->
                            S;

                        {error, _} ->
                            1
                    end,
                    New_shape = remove_at_index(erlang:element(3, T), Axis_idx),
                    New_size = gleam@list:fold(
                        New_shape,
                        1,
                        fun(Acc, D) -> Acc * D end
                    ),
                    Data = get_data(T),
                    Result = begin
                        _pipe@1 = gleam@list:range(0, New_size - 1),
                        gleam@list:map(
                            _pipe@1,
                            fun(Out_idx) ->
                                _pipe@2 = gleam@list:range(0, Axis_size - 1),
                                gleam@list:fold(
                                    _pipe@2,
                                    +0.0,
                                    fun(Acc@1, Axis_pos) ->
                                        In_idx = compute_index_with_axis(
                                            erlang:element(3, T),
                                            Out_idx,
                                            Axis_idx,
                                            Axis_pos
                                        ),
                                        Val = case begin
                                            _pipe@3 = gleam@list:drop(
                                                Data,
                                                In_idx
                                            ),
                                            gleam@list:first(_pipe@3)
                                        end of
                                            {ok, V} ->
                                                V;

                                            {error, _} ->
                                                +0.0
                                        end,
                                        Acc@1 + Val
                                    end
                                )
                            end
                        )
                    end,
                    {ok, {tensor, Result, New_shape}}
            end
    end.

-file("src/viva_tensor/tensor.gleam", 493).
?DOC(" Mean along a specific axis\n").
-spec mean_axis(tensor(), integer()) -> {ok, tensor()} | {error, tensor_error()}.
mean_axis(T, Axis_idx) ->
    R = rank(T),
    case (Axis_idx >= 0) andalso (Axis_idx < R) of
        false ->
            {error, {dimension_error, <<"Invalid axis index"/utf8>>}};

        true ->
            Axis_size = case begin
                _pipe = gleam@list:drop(erlang:element(3, T), Axis_idx),
                gleam@list:first(_pipe)
            end of
                {ok, S} ->
                    S;

                {error, _} ->
                    1
            end,
            case sum_axis(T, Axis_idx) of
                {error, E} ->
                    {error, E};

                {ok, Summed} ->
                    {ok, scale(Summed, case erlang:float(Axis_size) of
                                +0.0 -> +0.0;
                                -0.0 -> -0.0;
                                Gleam@denominator -> 1.0 / Gleam@denominator
                            end)}
            end
    end.

-file("src/viva_tensor/tensor.gleam", 565).
?DOC(" Dot product of two vectors\n").
-spec dot(tensor(), tensor()) -> {ok, float()} | {error, tensor_error()}.
dot(A, B) ->
    case ((rank(A) =:= 1) andalso (rank(B) =:= 1)) andalso (size(A) =:= size(B)) of
        true ->
            A_data = get_data(A),
            B_data = get_data(B),
            Products = gleam@list:map2(A_data, B_data, fun(X, Y) -> X * Y end),
            {ok,
                gleam@list:fold(Products, +0.0, fun(Acc, X@1) -> Acc + X@1 end)};

        false ->
            {error,
                {shape_mismatch, erlang:element(3, A), erlang:element(3, B)}}
    end.

-file("src/viva_tensor/tensor.gleam", 578).
?DOC(" Matrix-vector multiplication: [m, n] @ [n] -> [m]\n").
-spec matmul_vec(tensor(), tensor()) -> {ok, tensor()} | {error, tensor_error()}.
matmul_vec(Mat, Vec) ->
    case {erlang:element(3, Mat), erlang:element(3, Vec)} of
        {[M, N], [Vec_n]} when N =:= Vec_n ->
            Mat_data = get_data(Mat),
            Vec_data = get_data(Vec),
            Result_data = begin
                _pipe = gleam@list:range(0, M - 1),
                gleam@list:map(
                    _pipe,
                    fun(Row_idx) ->
                        Start = Row_idx * N,
                        Row = begin
                            _pipe@1 = Mat_data,
                            _pipe@2 = gleam@list:drop(_pipe@1, Start),
                            gleam@list:take(_pipe@2, N)
                        end,
                        _pipe@3 = gleam@list:map2(
                            Row,
                            Vec_data,
                            fun(A, B) -> A * B end
                        ),
                        gleam@list:fold(
                            _pipe@3,
                            +0.0,
                            fun(Acc, X) -> Acc + X end
                        )
                    end
                )
            end,
            {ok, {tensor, Result_data, [M]}};

        {[_, N@1], [Vec_n@1]} ->
            {error, {shape_mismatch, [N@1], [Vec_n@1]}};

        {_, _} ->
            {error, {dimension_error, <<"Expected matrix and vector"/utf8>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 602).
?DOC(" Matrix-matrix multiplication: [m, n] @ [n, p] -> [m, p]\n").
-spec matmul(tensor(), tensor()) -> {ok, tensor()} | {error, tensor_error()}.
matmul(A, B) ->
    case {erlang:element(3, A), erlang:element(3, B)} of
        {[M, N], [N2, P]} when N =:= N2 ->
            Result_data = begin
                _pipe = gleam@list:range(0, M - 1),
                gleam@list:flat_map(
                    _pipe,
                    fun(I) -> _pipe@1 = gleam@list:range(0, P - 1),
                        gleam@list:map(
                            _pipe@1,
                            fun(J) -> _pipe@2 = gleam@list:range(0, N - 1),
                                gleam@list:fold(
                                    _pipe@2,
                                    +0.0,
                                    fun(Acc, K) ->
                                        A_ik = case get2d(A, I, K) of
                                            {ok, V} ->
                                                V;

                                            {error, _} ->
                                                +0.0
                                        end,
                                        B_kj = case get2d(B, K, J) of
                                            {ok, V@1} ->
                                                V@1;

                                            {error, _} ->
                                                +0.0
                                        end,
                                        Acc + (A_ik * B_kj)
                                    end
                                ) end
                        ) end
                )
            end,
            {ok, {tensor, Result_data, [M, P]}};

        {[_, N@1], [N2@1, _]} ->
            {error, {shape_mismatch, [N@1, -1], [N2@1, -1]}};

        {_, _} ->
            {error, {dimension_error, <<"Expected two matrices"/utf8>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 632).
?DOC(" Matrix transpose\n").
-spec transpose(tensor()) -> {ok, tensor()} | {error, tensor_error()}.
transpose(T) ->
    case erlang:element(3, T) of
        [M, N] ->
            Result_data = begin
                _pipe = gleam@list:range(0, N - 1),
                gleam@list:flat_map(
                    _pipe,
                    fun(J) -> _pipe@1 = gleam@list:range(0, M - 1),
                        gleam@list:filter_map(
                            _pipe@1,
                            fun(I) -> get2d(T, I, J) end
                        ) end
                )
            end,
            {ok, {tensor, Result_data, [N, M]}};

        _ ->
            {error, {dimension_error, <<"Transpose requires 2D tensor"/utf8>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 648).
?DOC(" Outer product: [m] @ [n] -> [m, n]\n").
-spec outer(tensor(), tensor()) -> {ok, tensor()} | {error, tensor_error()}.
outer(A, B) ->
    case (rank(A) =:= 1) andalso (rank(B) =:= 1) of
        true ->
            M = size(A),
            N = size(B),
            A_data = get_data(A),
            B_data = get_data(B),
            Result_data = gleam@list:flat_map(
                A_data,
                fun(Ai) -> gleam@list:map(B_data, fun(Bj) -> Ai * Bj end) end
            ),
            {ok, {tensor, Result_data, [M, N]}};

        false ->
            {error,
                {dimension_error, <<"Outer product requires two vectors"/utf8>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 668).
?DOC(" Convert to list\n").
-spec to_list(tensor()) -> list(float()).
to_list(T) ->
    get_data(T).

-file("src/viva_tensor/tensor.gleam", 673).
?DOC(" Convert matrix to list of lists\n").
-spec to_list2d(tensor()) -> {ok, list(list(float()))} | {error, tensor_error()}.
to_list2d(T) ->
    case erlang:element(3, T) of
        [Num_rows, Num_cols] ->
            Data = get_data(T),
            Rows_list = begin
                _pipe = gleam@list:range(0, Num_rows - 1),
                gleam@list:map(
                    _pipe,
                    fun(I) ->
                        Start = I * Num_cols,
                        _pipe@1 = Data,
                        _pipe@2 = gleam@list:drop(_pipe@1, Start),
                        gleam@list:take(_pipe@2, Num_cols)
                    end
                )
            end,
            {ok, Rows_list};

        _ ->
            {error, {dimension_error, <<"Tensor is not 2D"/utf8>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 692).
?DOC(" Clone tensor\n").
-spec clone(tensor()) -> tensor().
clone(T) ->
    Data = get_data(T),
    {tensor, Data, erlang:element(3, T)}.

-file("src/viva_tensor/tensor.gleam", 698).
?DOC(" Reshape tensor\n").
-spec reshape(tensor(), list(integer())) -> {ok, tensor()} |
    {error, tensor_error()}.
reshape(T, New_shape) ->
    Old_size = size(T),
    New_size = gleam@list:fold(New_shape, 1, fun(Acc, Dim) -> Acc * Dim end),
    case Old_size =:= New_size of
        true ->
            Data = get_data(T),
            {ok, {tensor, Data, New_shape}};

        false ->
            {error,
                {invalid_shape,
                    <<<<<<<<"Cannot reshape: size mismatch ("/utf8,
                                    (erlang:integer_to_binary(Old_size))/binary>>/binary,
                                " vs "/utf8>>/binary,
                            (erlang:integer_to_binary(New_size))/binary>>/binary,
                        ")"/utf8>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 719).
?DOC(" Flatten to 1D\n").
-spec flatten(tensor()) -> tensor().
flatten(T) ->
    Data = get_data(T),
    {tensor, Data, [size(T)]}.

-file("src/viva_tensor/tensor.gleam", 725).
?DOC(" Concatenate vectors (1D)\n").
-spec concat(list(tensor())) -> tensor().
concat(Tensors) ->
    Data = gleam@list:flat_map(Tensors, fun(T) -> get_data(T) end),
    from_list(Data).

-file("src/viva_tensor/tensor.gleam", 733).
?DOC(
    " Concatenate tensors along a specific axis\n"
    " For [2,3] and [2,3] tensors: concat_axis([a, b], 0) -> [4,3]\n"
    " For [2,3] and [2,3] tensors: concat_axis([a, b], 1) -> [2,6]\n"
).
-spec concat_axis(list(tensor()), integer()) -> {ok, tensor()} |
    {error, tensor_error()}.
concat_axis(Tensors, Axis) ->
    case Tensors of
        [] ->
            {error, {invalid_shape, <<"Cannot concatenate empty list"/utf8>>}};

        [Single] ->
            {ok, Single};

        [First | Rest] ->
            Base_shape = erlang:element(3, First),
            R = erlang:length(Base_shape),
            case (Axis >= 0) andalso (Axis < R) of
                false ->
                    {error,
                        {dimension_error,
                            <<"Invalid axis for concatenation"/utf8>>}};

                true ->
                    Shapes_ok = gleam@list:all(
                        Rest,
                        fun(T) ->
                            T_shape = erlang:element(3, T),
                            case erlang:length(T_shape) =:= R of
                                false ->
                                    false;

                                true ->
                                    _pipe = gleam@list:zip(Base_shape, T_shape),
                                    _pipe@1 = gleam@list:index_map(
                                        _pipe,
                                        fun(Pair, I) -> {Pair, I} end
                                    ),
                                    gleam@list:all(
                                        _pipe@1,
                                        fun(X) ->
                                            {{Dim_a, Dim_b}, I@1} = X,
                                            (I@1 =:= Axis) orelse (Dim_a =:= Dim_b)
                                        end
                                    )
                            end
                        end
                    ),
                    case Shapes_ok of
                        false ->
                            {error,
                                {invalid_shape,
                                    <<"Shapes must match except on concat axis"/utf8>>}};

                        true ->
                            Concat_dim = gleam@list:fold(
                                Tensors,
                                0,
                                fun(Acc, T@1) ->
                                    case begin
                                        _pipe@2 = gleam@list:drop(
                                            erlang:element(3, T@1),
                                            Axis
                                        ),
                                        gleam@list:first(_pipe@2)
                                    end of
                                        {ok, D} ->
                                            Acc + D;

                                        {error, _} ->
                                            Acc
                                    end
                                end
                            ),
                            New_shape = begin
                                _pipe@3 = Base_shape,
                                gleam@list:index_map(
                                    _pipe@3,
                                    fun(D@1, I@2) -> case I@2 =:= Axis of
                                            true ->
                                                Concat_dim;

                                            false ->
                                                D@1
                                        end end
                                )
                            end,
                            case Axis =:= 0 of
                                true ->
                                    Data = gleam@list:flat_map(
                                        Tensors,
                                        fun(T@2) -> get_data(T@2) end
                                    ),
                                    {ok, {tensor, Data, New_shape}};

                                false ->
                                    Total_size = gleam@list:fold(
                                        New_shape,
                                        1,
                                        fun(Acc@1, D@2) -> Acc@1 * D@2 end
                                    ),
                                    _ = compute_strides(New_shape),
                                    Result = begin
                                        _pipe@4 = gleam@list:range(
                                            0,
                                            Total_size - 1
                                        ),
                                        gleam@list:map(
                                            _pipe@4,
                                            fun(Flat_idx) ->
                                                Indices = flat_to_multi(
                                                    Flat_idx,
                                                    New_shape
                                                ),
                                                Axis_idx = case begin
                                                    _pipe@5 = gleam@list:drop(
                                                        Indices,
                                                        Axis
                                                    ),
                                                    gleam@list:first(_pipe@5)
                                                end of
                                                    {ok, I@3} ->
                                                        I@3;

                                                    {error, _} ->
                                                        0
                                                end,
                                                {Tensor_idx, Local_axis_idx, _} = gleam@list:fold(
                                                    Tensors,
                                                    {-1, Axis_idx, 0},
                                                    fun(Acc@2, T@3) ->
                                                        {Found_t,
                                                            Remaining,
                                                            T_idx} = Acc@2,
                                                        case Found_t >= 0 of
                                                            true ->
                                                                Acc@2;

                                                            false ->
                                                                T_axis_size = case begin
                                                                    _pipe@6 = gleam@list:drop(
                                                                        erlang:element(
                                                                            3,
                                                                            T@3
                                                                        ),
                                                                        Axis
                                                                    ),
                                                                    gleam@list:first(
                                                                        _pipe@6
                                                                    )
                                                                end of
                                                                    {ok, D@3} ->
                                                                        D@3;

                                                                    {error, _} ->
                                                                        0
                                                                end,
                                                                case Remaining < T_axis_size of
                                                                    true ->
                                                                        {T_idx,
                                                                            Remaining,
                                                                            T_idx};

                                                                    false ->
                                                                        {-1,
                                                                            Remaining
                                                                            - T_axis_size,
                                                                            T_idx
                                                                            + 1}
                                                                end
                                                        end
                                                    end
                                                ),
                                                Local_indices = begin
                                                    _pipe@7 = Indices,
                                                    gleam@list:index_map(
                                                        _pipe@7,
                                                        fun(Idx, I@4) ->
                                                            case I@4 =:= Axis of
                                                                true ->
                                                                    Local_axis_idx;

                                                                false ->
                                                                    Idx
                                                            end
                                                        end
                                                    )
                                                end,
                                                case begin
                                                    _pipe@8 = gleam@list:drop(
                                                        Tensors,
                                                        Tensor_idx
                                                    ),
                                                    gleam@list:first(_pipe@8)
                                                end of
                                                    {ok, T@4} ->
                                                        T_strides = compute_strides(
                                                            erlang:element(
                                                                3,
                                                                T@4
                                                            )
                                                        ),
                                                        Local_flat = begin
                                                            _pipe@9 = gleam@list:zip(
                                                                Local_indices,
                                                                T_strides
                                                            ),
                                                            gleam@list:fold(
                                                                _pipe@9,
                                                                0,
                                                                fun(A, P) ->
                                                                    A + (erlang:element(
                                                                        1,
                                                                        P
                                                                    )
                                                                    * erlang:element(
                                                                        2,
                                                                        P
                                                                    ))
                                                                end
                                                            )
                                                        end,
                                                        T_data = get_data(T@4),
                                                        case begin
                                                            _pipe@10 = gleam@list:drop(
                                                                T_data,
                                                                Local_flat
                                                            ),
                                                            gleam@list:first(
                                                                _pipe@10
                                                            )
                                                        end of
                                                            {ok, V} ->
                                                                V;

                                                            {error, _} ->
                                                                +0.0
                                                        end;

                                                    {error, _} ->
                                                        +0.0
                                                end
                                            end
                                        )
                                    end,
                                    {ok, {tensor, Result, New_shape}}
                            end
                    end
            end
    end.

-file("src/viva_tensor/tensor.gleam", 908).
?DOC(" Take first N elements along first axis\n").
-spec take_first(tensor(), integer()) -> tensor().
take_first(T, N) ->
    Data = get_data(T),
    case erlang:element(3, T) of
        [] ->
            T;

        [First_dim | Rest_dims] ->
            Take_n = gleam@int:min(N, First_dim),
            Stride = gleam@list:fold(Rest_dims, 1, fun(Acc, D) -> Acc * D end),
            New_data = gleam@list:take(Data, Take_n * Stride),
            New_shape = [Take_n | Rest_dims],
            {tensor, New_data, New_shape}
    end.

-file("src/viva_tensor/tensor.gleam", 924).
?DOC(" Take last N elements along first axis\n").
-spec take_last(tensor(), integer()) -> tensor().
take_last(T, N) ->
    Data = get_data(T),
    case erlang:element(3, T) of
        [] ->
            T;

        [First_dim | Rest_dims] ->
            Take_n = gleam@int:min(N, First_dim),
            Stride = gleam@list:fold(Rest_dims, 1, fun(Acc, D) -> Acc * D end),
            Skip = (First_dim - Take_n) * Stride,
            New_data = gleam@list:drop(Data, Skip),
            New_shape = [Take_n | Rest_dims],
            {tensor, New_data, New_shape}
    end.

-file("src/viva_tensor/tensor.gleam", 942).
?DOC(
    " Slice tensor: extract sub-tensor from start to start+lengths\n"
    " slice(t, [1], [3]) extracts elements at indices 1, 2, 3\n"
).
-spec slice(tensor(), list(integer()), list(integer())) -> {ok, tensor()} |
    {error, tensor_error()}.
slice(T, Start, Lengths) ->
    Data = get_data(T),
    R = rank(T),
    case (erlang:length(Start) =:= R) andalso (erlang:length(Lengths) =:= R) of
        false ->
            {error,
                {dimension_error,
                    <<"Slice dimensions must match tensor rank"/utf8>>}};

        true ->
            case R of
                1 ->
                    S = case gleam@list:first(Start) of
                        {ok, V} ->
                            V;

                        {error, _} ->
                            0
                    end,
                    Len = case gleam@list:first(Lengths) of
                        {ok, V@1} ->
                            V@1;

                        {error, _} ->
                            0
                    end,
                    Sliced = begin
                        _pipe = Data,
                        _pipe@1 = gleam@list:drop(_pipe, S),
                        gleam@list:take(_pipe@1, Len)
                    end,
                    {ok, {tensor, Sliced, [Len]}};

                _ ->
                    New_size = gleam@list:fold(
                        Lengths,
                        1,
                        fun(Acc, D) -> Acc * D end
                    ),
                    Result = begin
                        _pipe@2 = gleam@list:range(0, New_size - 1),
                        gleam@list:map(
                            _pipe@2,
                            fun(Flat_idx) ->
                                Local_indices = flat_to_multi(Flat_idx, Lengths),
                                Global_indices = gleam@list:map2(
                                    Local_indices,
                                    Start,
                                    fun(L, S@1) -> L + S@1 end
                                ),
                                Global_flat = multi_to_flat(
                                    Global_indices,
                                    erlang:element(3, T)
                                ),
                                case begin
                                    _pipe@3 = gleam@list:drop(Data, Global_flat),
                                    gleam@list:first(_pipe@3)
                                end of
                                    {ok, V@2} ->
                                        V@2;

                                    {error, _} ->
                                        +0.0
                                end
                            end
                        )
                    end,
                    {ok, {tensor, Result, Lengths}}
            end
    end.

-file("src/viva_tensor/tensor.gleam", 992).
?DOC(" L2 norm\n").
-spec norm(tensor()) -> float().
norm(T) ->
    Data = get_data(T),
    Sum_sq = gleam@list:fold(Data, +0.0, fun(Acc, X) -> Acc + (X * X) end),
    math:sqrt(Sum_sq).

-file("src/viva_tensor/tensor.gleam", 999).
?DOC(" Normalize to unit length\n").
-spec normalize(tensor()) -> tensor().
normalize(T) ->
    N = norm(T),
    case N > 0.0001 of
        true ->
            scale(T, case N of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> 1.0 / Gleam@denominator
                end);

        false ->
            T
    end.

-file("src/viva_tensor/tensor.gleam", 1008).
?DOC(" Clamp values\n").
-spec clamp(tensor(), float(), float()) -> tensor().
clamp(T, Min_val, Max_val) ->
    map(T, fun(X) -> gleam@float:min(gleam@float:max(X, Min_val), Max_val) end).

-file("src/viva_tensor/tensor.gleam", 1152).
?DOC(" Remove dimensions of size 1\n").
-spec squeeze(tensor()) -> tensor().
squeeze(T) ->
    Data = get_data(T),
    New_shape = gleam@list:filter(erlang:element(3, T), fun(D) -> D /= 1 end),
    Final_shape = case New_shape of
        [] ->
            [1];

        _ ->
            New_shape
    end,
    {tensor, Data, Final_shape}.

-file("src/viva_tensor/tensor.gleam", 1163).
?DOC(" Remove dimension at specific axis if it's 1\n").
-spec squeeze_axis(tensor(), integer()) -> {ok, tensor()} |
    {error, tensor_error()}.
squeeze_axis(T, Axis) ->
    case list_at_int(erlang:element(3, T), Axis) of
        {error, _} ->
            {error, {dimension_error, <<"Axis out of bounds"/utf8>>}};

        {ok, D} ->
            case D =:= 1 of
                false ->
                    {error,
                        {invalid_shape, <<"Dimension at axis is not 1"/utf8>>}};

                true ->
                    Data = get_data(T),
                    New_shape = begin
                        _pipe = erlang:element(3, T),
                        _pipe@1 = gleam@list:index_map(
                            _pipe,
                            fun(Dim, I) -> {Dim, I} end
                        ),
                        _pipe@2 = gleam@list:filter(
                            _pipe@1,
                            fun(Pair) -> erlang:element(2, Pair) /= Axis end
                        ),
                        gleam@list:map(
                            _pipe@2,
                            fun(Pair@1) -> erlang:element(1, Pair@1) end
                        )
                    end,
                    {ok, {tensor, Data, New_shape}}
            end
    end.

-file("src/viva_tensor/tensor.gleam", 1184).
?DOC(" Add dimension of size 1 at specified axis\n").
-spec unsqueeze(tensor(), integer()) -> tensor().
unsqueeze(T, Axis) ->
    Data = get_data(T),
    Rnk = erlang:length(erlang:element(3, T)),
    Insert_at = case Axis < 0 of
        true ->
            (Rnk + Axis) + 1;

        false ->
            Axis
    end,
    {Before, After} = gleam@list:split(erlang:element(3, T), Insert_at),
    New_shape = lists:append([Before, [1], After]),
    {tensor, Data, New_shape}.

-file("src/viva_tensor/tensor.gleam", 869).
?DOC(
    " Stack tensors along a new axis\n"
    " For [3] and [3] tensors: stack([a, b], 0) -> [2, 3]\n"
    " For [3] and [3] tensors: stack([a, b], 1) -> [3, 2]\n"
).
-spec stack(list(tensor()), integer()) -> {ok, tensor()} |
    {error, tensor_error()}.
stack(Tensors, Axis) ->
    case Tensors of
        [] ->
            {error, {invalid_shape, <<"Cannot stack empty list"/utf8>>}};

        [First | Rest] ->
            Base_shape = erlang:element(3, First),
            Shapes_ok = gleam@list:all(
                Rest,
                fun(T) -> erlang:element(3, T) =:= Base_shape end
            ),
            case Shapes_ok of
                false ->
                    {error, {shape_mismatch, Base_shape, []}};

                true ->
                    N_tensors = erlang:length(Tensors),
                    R = erlang:length(Base_shape),
                    Insert_axis = case Axis < 0 of
                        true ->
                            (R + Axis) + 1;

                        false ->
                            Axis
                    end,
                    case (Insert_axis >= 0) andalso (Insert_axis =< R) of
                        false ->
                            {error,
                                {dimension_error,
                                    <<"Invalid axis for stacking"/utf8>>}};

                        true ->
                            {Before, After} = gleam@list:split(
                                Base_shape,
                                Insert_axis
                            ),
                            _ = lists:append([Before, [N_tensors], After]),
                            Unsqueezed = begin
                                _pipe = Tensors,
                                gleam@list:map(
                                    _pipe,
                                    fun(T@1) -> unsqueeze(T@1, Insert_axis) end
                                )
                            end,
                            concat_axis(Unsqueezed, Insert_axis)
                    end
            end
    end.

-file("src/viva_tensor/tensor.gleam", 1198).
?DOC(" Expand tensor to add batch dimension\n").
-spec expand_dims(tensor(), integer()) -> tensor().
expand_dims(T, Axis) ->
    unsqueeze(T, Axis).

-file("src/viva_tensor/tensor.gleam", 1219).
?DOC(" Convert strided tensor back to regular (materializes the view)\n").
-spec to_contiguous(tensor()) -> tensor().
to_contiguous(T) ->
    case T of
        {tensor, _, _} ->
            T;

        {strided_tensor, _, _, _, _} ->
            Data = get_data(T),
            {tensor, Data, erlang:element(3, T)}
    end.

-file("src/viva_tensor/tensor.gleam", 1269).
?DOC(" Get element with O(1) access for StridedTensor\n").
-spec get_fast(tensor(), integer()) -> {ok, float()} | {error, tensor_error()}.
get_fast(T, Index) ->
    case T of
        {tensor, Data, _} ->
            _pipe = list_at_float(Data, Index),
            gleam@result:map_error(
                _pipe,
                fun(_) ->
                    {dimension_error,
                        <<<<"Index "/utf8,
                                (erlang:integer_to_binary(Index))/binary>>/binary,
                            " out of bounds"/utf8>>}
                end
            );

        {strided_tensor, Storage, Shape, Strides, Offset} ->
            Indices = flat_to_multi(Index, Shape),
            Flat_idx = begin
                _pipe@1 = gleam@list:zip(Indices, Strides),
                gleam@list:fold(
                    _pipe@1,
                    Offset,
                    fun(Acc, Pair) ->
                        {Idx, Stride} = Pair,
                        Acc + (Idx * Stride)
                    end
                )
            end,
            {ok, array_get(Storage, Flat_idx)}
    end.

-file("src/viva_tensor/tensor.gleam", 1290).
?DOC(" Get 2D element with O(1) access\n").
-spec get2d_fast(tensor(), integer(), integer()) -> {ok, float()} |
    {error, tensor_error()}.
get2d_fast(T, Row, Col) ->
    case T of
        {tensor, _, _} ->
            get2d(T, Row, Col);

        {strided_tensor, Storage, Shape, Strides, Offset} ->
            case {Shape, Strides} of
                {[_, _], [S0, S1]} ->
                    Flat_idx = (Offset + (Row * S0)) + (Col * S1),
                    {ok, array_get(Storage, Flat_idx)};

                {_, _} ->
                    {error, {dimension_error, <<"Tensor is not 2D"/utf8>>}}
            end
    end.

-file("src/viva_tensor/tensor.gleam", 1359).
-spec broadcast_data(tensor(), list(integer())) -> list(float()).
broadcast_data(T, Target_shape) ->
    Target_size = gleam@list:fold(
        Target_shape,
        1,
        fun(Acc, Dim) -> Acc * Dim end
    ),
    Src_shape = erlang:element(3, T),
    Src_rank = erlang:length(Src_shape),
    Target_rank = erlang:length(Target_shape),
    Data = get_data(T),
    Diff = Target_rank - Src_rank,
    Padded_shape = lists:append(gleam@list:repeat(1, Diff), Src_shape),
    _pipe = gleam@list:range(0, Target_size - 1),
    gleam@list:map(
        _pipe,
        fun(Flat_idx) ->
            Target_indices = flat_to_multi(Flat_idx, Target_shape),
            Src_indices = begin
                _pipe@1 = gleam@list:zip(Target_indices, Padded_shape),
                _pipe@2 = gleam@list:map(
                    _pipe@1,
                    fun(Pair) ->
                        {Idx, Dim@1} = Pair,
                        case Dim@1 =:= 1 of
                            true ->
                                0;

                            false ->
                                Idx
                        end
                    end
                ),
                gleam@list:drop(_pipe@2, Diff)
            end,
            Src_flat = multi_to_flat(Src_indices, Src_shape),
            case list_at_float(Data, Src_flat) of
                {ok, V} ->
                    V;

                {error, _} ->
                    +0.0
            end
        end
    ).

-file("src/viva_tensor/tensor.gleam", 1113).
?DOC(" Broadcast tensor to target shape\n").
-spec broadcast_to(tensor(), list(integer())) -> {ok, tensor()} |
    {error, tensor_error()}.
broadcast_to(T, Target_shape) ->
    case can_broadcast(erlang:element(3, T), Target_shape) of
        false ->
            {error, {broadcast_error, erlang:element(3, T), Target_shape}};

        true ->
            case erlang:element(3, T) =:= Target_shape of
                true ->
                    {ok, T};

                false ->
                    Data = broadcast_data(T, Target_shape),
                    {ok, {tensor, Data, Target_shape}}
            end
    end.

-file("src/viva_tensor/tensor.gleam", 1132).
?DOC(" Element-wise addition with broadcasting\n").
-spec add_broadcast(tensor(), tensor()) -> {ok, tensor()} |
    {error, tensor_error()}.
add_broadcast(A, B) ->
    gleam@result:'try'(
        broadcast_shape(erlang:element(3, A), erlang:element(3, B)),
        fun(Result_shape) ->
            gleam@result:'try'(
                broadcast_to(A, Result_shape),
                fun(A_bc) ->
                    gleam@result:'try'(
                        broadcast_to(B, Result_shape),
                        fun(B_bc) -> add(A_bc, B_bc) end
                    )
                end
            )
        end
    ).

-file("src/viva_tensor/tensor.gleam", 1140).
?DOC(" Element-wise multiplication with broadcasting\n").
-spec mul_broadcast(tensor(), tensor()) -> {ok, tensor()} |
    {error, tensor_error()}.
mul_broadcast(A, B) ->
    gleam@result:'try'(
        broadcast_shape(erlang:element(3, A), erlang:element(3, B)),
        fun(Result_shape) ->
            gleam@result:'try'(
                broadcast_to(A, Result_shape),
                fun(A_bc) ->
                    gleam@result:'try'(
                        broadcast_to(B, Result_shape),
                        fun(B_bc) -> mul(A_bc, B_bc) end
                    )
                end
            )
        end
    ).

-file("src/viva_tensor/tensor.gleam", 1444).
?DOC(
    " Pad a 2D tensor with zeros\n"
    " Input: [H, W], Output: [H + 2*pad_h, W + 2*pad_w]\n"
).
-spec pad2d(tensor(), integer(), integer()) -> {ok, tensor()} |
    {error, tensor_error()}.
pad2d(T, Pad_h, Pad_w) ->
    Shp = shape(T),
    case Shp of
        [H, W] ->
            New_h = H + (2 * Pad_h),
            New_w = W + (2 * Pad_w),
            Data = get_data(T),
            Padded = begin
                _pipe = gleam@list:range(0, New_h - 1),
                gleam@list:flat_map(
                    _pipe,
                    fun(Row) -> _pipe@1 = gleam@list:range(0, New_w - 1),
                        gleam@list:map(
                            _pipe@1,
                            fun(Col) ->
                                Src_row = Row - Pad_h,
                                Src_col = Col - Pad_w,
                                case (((Src_row >= 0) andalso (Src_row < H))
                                andalso (Src_col >= 0))
                                andalso (Src_col < W) of
                                    true ->
                                        Idx = (Src_row * W) + Src_col,
                                        case list_at_float(Data, Idx) of
                                            {ok, V} ->
                                                V;

                                            {error, _} ->
                                                +0.0
                                        end;

                                    false ->
                                        +0.0
                                end
                            end
                        ) end
                )
            end,
            {ok, {tensor, Padded, [New_h, New_w]}};

        _ ->
            {error, {invalid_shape, <<"pad2d requires 2D tensor [H, W]"/utf8>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 1487).
?DOC(
    " Pad a 4D tensor (batch) with zeros\n"
    " Input: [N, C, H, W], Output: [N, C, H + 2*pad_h, W + 2*pad_w]\n"
).
-spec pad4d(tensor(), integer(), integer()) -> {ok, tensor()} |
    {error, tensor_error()}.
pad4d(T, Pad_h, Pad_w) ->
    Shp = shape(T),
    case Shp of
        [N, C, H, W] ->
            New_h = H + (2 * Pad_h),
            New_w = W + (2 * Pad_w),
            Data = get_data(T),
            Spatial_size = H * W,
            _ = New_h * New_w,
            Padded = begin
                _pipe = gleam@list:range(0, N - 1),
                gleam@list:flat_map(
                    _pipe,
                    fun(Batch) -> _pipe@1 = gleam@list:range(0, C - 1),
                        gleam@list:flat_map(
                            _pipe@1,
                            fun(Channel) ->
                                Base_idx = ((Batch * C) * Spatial_size) + (Channel
                                * Spatial_size),
                                _pipe@2 = gleam@list:range(0, New_h - 1),
                                gleam@list:flat_map(
                                    _pipe@2,
                                    fun(Row) ->
                                        _pipe@3 = gleam@list:range(0, New_w - 1),
                                        gleam@list:map(
                                            _pipe@3,
                                            fun(Col) ->
                                                Src_row = Row - Pad_h,
                                                Src_col = Col - Pad_w,
                                                case (((Src_row >= 0) andalso (Src_row
                                                < H))
                                                andalso (Src_col >= 0))
                                                andalso (Src_col < W) of
                                                    true ->
                                                        Idx = (Base_idx + (Src_row
                                                        * W))
                                                        + Src_col,
                                                        case list_at_float(
                                                            Data,
                                                            Idx
                                                        ) of
                                                            {ok, V} ->
                                                                V;

                                                            {error, _} ->
                                                                +0.0
                                                        end;

                                                    false ->
                                                        +0.0
                                                end
                                            end
                                        )
                                    end
                                )
                            end
                        ) end
                )
            end,
            {ok, {tensor, Padded, [N, C, New_h, New_w]}};

        _ ->
            {error,
                {invalid_shape,
                    <<"pad4d requires 4D tensor [N, C, H, W]"/utf8>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 1662).
?DOC(" Compute dot product of kernel with input patch - O(1) access per element\n").
-spec conv2d_dot_product(
    erlang_array(),
    erlang_array(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    float()
) -> float().
conv2d_dot_product(In_arr, K_arr, In_w, Row, Col, Kh, Kw, Kr, Kc, Acc) ->
    case Kr >= Kh of
        true ->
            Acc;

        false ->
            case Kc >= Kw of
                true ->
                    conv2d_dot_product(
                        In_arr,
                        K_arr,
                        In_w,
                        Row,
                        Col,
                        Kh,
                        Kw,
                        Kr + 1,
                        0,
                        Acc
                    );

                false ->
                    In_idx = ((Row + Kr) * In_w) + (Col + Kc),
                    K_idx = (Kr * Kw) + Kc,
                    In_val = viva_tensor_ffi:array_get(In_arr, In_idx),
                    K_val = viva_tensor_ffi:array_get(K_arr, K_idx),
                    conv2d_dot_product(
                        In_arr,
                        K_arr,
                        In_w,
                        Row,
                        Col,
                        Kh,
                        Kw,
                        Kr,
                        Kc + 1,
                        Acc + (In_val * K_val)
                    )
            end
    end.

-file("src/viva_tensor/tensor.gleam", 1617).
?DOC(" Tail-recursive conv2d loop with O(1) array access\n").
-spec conv2d_simple_loop(
    erlang_array(),
    erlang_array(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    list(float())
) -> list(float()).
conv2d_simple_loop(
    In_arr,
    K_arr,
    In_h,
    In_w,
    Kh,
    Kw,
    Stride_h,
    Stride_w,
    Out_h,
    Out_w,
    Oh,
    Ow,
    Acc
) ->
    case Oh >= Out_h of
        true ->
            Acc;

        false ->
            case Ow >= Out_w of
                true ->
                    conv2d_simple_loop(
                        In_arr,
                        K_arr,
                        In_h,
                        In_w,
                        Kh,
                        Kw,
                        Stride_h,
                        Stride_w,
                        Out_h,
                        Out_w,
                        Oh + 1,
                        0,
                        Acc
                    );

                false ->
                    Row = Oh * Stride_h,
                    Col = Ow * Stride_w,
                    Val = conv2d_dot_product(
                        In_arr,
                        K_arr,
                        In_w,
                        Row,
                        Col,
                        Kh,
                        Kw,
                        0,
                        0,
                        +0.0
                    ),
                    conv2d_simple_loop(
                        In_arr,
                        K_arr,
                        In_h,
                        In_w,
                        Kh,
                        Kw,
                        Stride_h,
                        Stride_w,
                        Out_h,
                        Out_w,
                        Oh,
                        Ow + 1,
                        [Val | Acc]
                    )
            end
    end.

-file("src/viva_tensor/tensor.gleam", 1578).
?DOC(" Simple 2D convolution (single channel) - OPTIMIZED with O(1) array access\n").
-spec conv2d_simple(
    tensor(),
    tensor(),
    integer(),
    integer(),
    integer(),
    integer(),
    conv2d_config()
) -> {ok, tensor()} | {error, tensor_error()}.
conv2d_simple(Input, Kernel, H, W, Kh, Kw, Config) ->
    gleam@result:'try'(
        case (erlang:element(6, Config) > 0) orelse (erlang:element(7, Config) > 0) of
            true ->
                pad2d(
                    Input,
                    erlang:element(6, Config),
                    erlang:element(7, Config)
                );

            false ->
                {ok, Input}
        end,
        fun(Padded) ->
            Padded_shape = shape(Padded),
            {Ph@1, Pw@1} = case Padded_shape of
                [Ph, Pw] ->
                    {Ph, Pw};

                _ ->
                    {H, W}
            end,
            Out_h = (case erlang:element(4, Config) of
                0 -> 0;
                Gleam@denominator -> (Ph@1 - Kh) div Gleam@denominator
            end) + 1,
            Out_w = (case erlang:element(5, Config) of
                0 -> 0;
                Gleam@denominator@1 -> (Pw@1 - Kw) div Gleam@denominator@1
            end) + 1,
            In_arr = viva_tensor_ffi:list_to_array(get_data(Padded)),
            K_arr = viva_tensor_ffi:list_to_array(get_data(Kernel)),
            Output = conv2d_simple_loop(
                In_arr,
                K_arr,
                Ph@1,
                Pw@1,
                Kh,
                Kw,
                erlang:element(4, Config),
                erlang:element(5, Config),
                Out_h,
                Out_w,
                0,
                0,
                []
            ),
            {ok, {tensor, lists:reverse(Output), [Out_h, Out_w]}}
        end
    ).

-file("src/viva_tensor/tensor.gleam", 1814).
?DOC(" Sum over kernel window with bounds checking\n").
-spec conv2d_kernel_sum(
    erlang_array(),
    erlang_array(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    float()
) -> float().
conv2d_kernel_sum(
    In_arr,
    K_arr,
    H,
    W,
    Kh,
    Kw,
    Ch_offset,
    K_offset,
    Row,
    Col,
    Kr,
    Kc,
    Acc
) ->
    case Kr >= Kh of
        true ->
            Acc;

        false ->
            case Kc >= Kw of
                true ->
                    conv2d_kernel_sum(
                        In_arr,
                        K_arr,
                        H,
                        W,
                        Kh,
                        Kw,
                        Ch_offset,
                        K_offset,
                        Row,
                        Col,
                        Kr + 1,
                        0,
                        Acc
                    );

                false ->
                    R = Row + Kr,
                    C_pos = Col + Kc,
                    In_val = case (((R >= 0) andalso (R < H)) andalso (C_pos >= 0))
                    andalso (C_pos < W) of
                        true ->
                            viva_tensor_ffi:array_get(
                                In_arr,
                                (Ch_offset + (R * W)) + C_pos
                            );

                        false ->
                            +0.0
                    end,
                    K_val = viva_tensor_ffi:array_get(
                        K_arr,
                        (K_offset + (Kr * Kw)) + Kc
                    ),
                    conv2d_kernel_sum(
                        In_arr,
                        K_arr,
                        H,
                        W,
                        Kh,
                        Kw,
                        Ch_offset,
                        K_offset,
                        Row,
                        Col,
                        Kr,
                        Kc + 1,
                        Acc + (In_val * K_val)
                    )
            end
    end.

-file("src/viva_tensor/tensor.gleam", 1780).
?DOC(" Sum over input channels\n").
-spec conv2d_mc_channels(
    erlang_array(),
    erlang_array(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    float()
) -> float().
conv2d_mc_channels(
    In_arr,
    K_arr,
    C_in,
    H,
    W,
    Kh,
    Kw,
    Spatial_size,
    K_spatial,
    Row,
    Col,
    C,
    Acc
) ->
    case C >= C_in of
        true ->
            Acc;

        false ->
            Ch_offset = C * Spatial_size,
            K_offset = C * K_spatial,
            Channel_sum = conv2d_kernel_sum(
                In_arr,
                K_arr,
                H,
                W,
                Kh,
                Kw,
                Ch_offset,
                K_offset,
                Row,
                Col,
                0,
                0,
                +0.0
            ),
            conv2d_mc_channels(
                In_arr,
                K_arr,
                C_in,
                H,
                W,
                Kh,
                Kw,
                Spatial_size,
                K_spatial,
                Row,
                Col,
                C + 1,
                Acc + Channel_sum
            )
    end.

-file("src/viva_tensor/tensor.gleam", 1729).
?DOC(" Multi-channel conv loop - tail recursive\n").
-spec conv2d_mc_loop(
    erlang_array(),
    erlang_array(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    list(float())
) -> list(float()).
conv2d_mc_loop(
    In_arr,
    K_arr,
    C_in,
    H,
    W,
    Kh,
    Kw,
    Spatial_size,
    K_spatial,
    Stride_h,
    Stride_w,
    Pad_h,
    Pad_w,
    Out_h,
    Out_w,
    Oh,
    Ow,
    Acc
) ->
    case Oh >= Out_h of
        true ->
            Acc;

        false ->
            case Ow >= Out_w of
                true ->
                    conv2d_mc_loop(
                        In_arr,
                        K_arr,
                        C_in,
                        H,
                        W,
                        Kh,
                        Kw,
                        Spatial_size,
                        K_spatial,
                        Stride_h,
                        Stride_w,
                        Pad_h,
                        Pad_w,
                        Out_h,
                        Out_w,
                        Oh + 1,
                        0,
                        Acc
                    );

                false ->
                    Row = (Oh * Stride_h) - Pad_h,
                    Col = (Ow * Stride_w) - Pad_w,
                    Val = conv2d_mc_channels(
                        In_arr,
                        K_arr,
                        C_in,
                        H,
                        W,
                        Kh,
                        Kw,
                        Spatial_size,
                        K_spatial,
                        Row,
                        Col,
                        0,
                        +0.0
                    ),
                    conv2d_mc_loop(
                        In_arr,
                        K_arr,
                        C_in,
                        H,
                        W,
                        Kh,
                        Kw,
                        Spatial_size,
                        K_spatial,
                        Stride_h,
                        Stride_w,
                        Pad_h,
                        Pad_w,
                        Out_h,
                        Out_w,
                        Oh,
                        Ow + 1,
                        [Val | Acc]
                    )
            end
    end.

-file("src/viva_tensor/tensor.gleam", 1698).
?DOC(" Multi-channel convolution (sum over channels) - OPTIMIZED\n").
-spec conv2d_multichannel(
    tensor(),
    tensor(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    conv2d_config()
) -> {ok, tensor()} | {error, tensor_error()}.
conv2d_multichannel(Input, Kernel, C_in, H, W, Kh, Kw, Config) ->
    Out_h = (case erlang:element(4, Config) of
        0 -> 0;
        Gleam@denominator -> ((H + (2 * erlang:element(6, Config))) - Kh) div Gleam@denominator
    end) + 1,
    Out_w = (case erlang:element(5, Config) of
        0 -> 0;
        Gleam@denominator@1 -> ((W + (2 * erlang:element(7, Config))) - Kw) div Gleam@denominator@1
    end) + 1,
    Spatial_size = H * W,
    K_spatial = Kh * Kw,
    In_arr = viva_tensor_ffi:list_to_array(get_data(Input)),
    K_arr = viva_tensor_ffi:list_to_array(get_data(Kernel)),
    Output = conv2d_mc_loop(
        In_arr,
        K_arr,
        C_in,
        H,
        W,
        Kh,
        Kw,
        Spatial_size,
        K_spatial,
        erlang:element(4, Config),
        erlang:element(5, Config),
        erlang:element(6, Config),
        erlang:element(7, Config),
        Out_h,
        Out_w,
        0,
        0,
        []
    ),
    {ok, {tensor, lists:reverse(Output), [Out_h, Out_w]}}.

-file("src/viva_tensor/tensor.gleam", 1976).
?DOC(" Sum over input channels for full conv\n").
-spec conv2d_full_channels(
    erlang_array(),
    erlang_array(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    float()
) -> float().
conv2d_full_channels(
    In_arr,
    K_arr,
    C_in,
    H,
    W,
    Kh,
    Kw,
    In_spatial,
    K_spatial,
    Batch_offset,
    Filter_offset,
    Row,
    Col,
    Ic,
    Acc
) ->
    case Ic >= C_in of
        true ->
            Acc;

        false ->
            Ch_offset = Batch_offset + (Ic * In_spatial),
            K_ch_offset = Filter_offset + (Ic * K_spatial),
            Sum = conv2d_kernel_sum(
                In_arr,
                K_arr,
                H,
                W,
                Kh,
                Kw,
                Ch_offset,
                K_ch_offset,
                Row,
                Col,
                0,
                0,
                +0.0
            ),
            conv2d_full_channels(
                In_arr,
                K_arr,
                C_in,
                H,
                W,
                Kh,
                Kw,
                In_spatial,
                K_spatial,
                Batch_offset,
                Filter_offset,
                Row,
                Col,
                Ic + 1,
                Acc + Sum
            )
    end.

-file("src/viva_tensor/tensor.gleam", 1894).
?DOC(" Full conv loop: batch -> output_channel -> oh -> ow\n").
-spec conv2d_full_loop(
    erlang_array(),
    erlang_array(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    list(float())
) -> list(float()).
conv2d_full_loop(
    In_arr,
    K_arr,
    N,
    C_in,
    C_out,
    H,
    W,
    Kh,
    Kw,
    In_spatial,
    In_batch_size,
    K_spatial,
    K_filter_size,
    Stride_h,
    Stride_w,
    Pad_h,
    Pad_w,
    Out_h,
    Out_w,
    Batch,
    Oc,
    Oh,
    Ow,
    Acc
) ->
    case Batch >= N of
        true ->
            Acc;

        false ->
            case Oc >= C_out of
                true ->
                    conv2d_full_loop(
                        In_arr,
                        K_arr,
                        N,
                        C_in,
                        C_out,
                        H,
                        W,
                        Kh,
                        Kw,
                        In_spatial,
                        In_batch_size,
                        K_spatial,
                        K_filter_size,
                        Stride_h,
                        Stride_w,
                        Pad_h,
                        Pad_w,
                        Out_h,
                        Out_w,
                        Batch + 1,
                        0,
                        0,
                        0,
                        Acc
                    );

                false ->
                    case Oh >= Out_h of
                        true ->
                            conv2d_full_loop(
                                In_arr,
                                K_arr,
                                N,
                                C_in,
                                C_out,
                                H,
                                W,
                                Kh,
                                Kw,
                                In_spatial,
                                In_batch_size,
                                K_spatial,
                                K_filter_size,
                                Stride_h,
                                Stride_w,
                                Pad_h,
                                Pad_w,
                                Out_h,
                                Out_w,
                                Batch,
                                Oc + 1,
                                0,
                                0,
                                Acc
                            );

                        false ->
                            case Ow >= Out_w of
                                true ->
                                    conv2d_full_loop(
                                        In_arr,
                                        K_arr,
                                        N,
                                        C_in,
                                        C_out,
                                        H,
                                        W,
                                        Kh,
                                        Kw,
                                        In_spatial,
                                        In_batch_size,
                                        K_spatial,
                                        K_filter_size,
                                        Stride_h,
                                        Stride_w,
                                        Pad_h,
                                        Pad_w,
                                        Out_h,
                                        Out_w,
                                        Batch,
                                        Oc,
                                        Oh + 1,
                                        0,
                                        Acc
                                    );

                                false ->
                                    Batch_offset = Batch * In_batch_size,
                                    Filter_offset = Oc * K_filter_size,
                                    Row = (Oh * Stride_h) - Pad_h,
                                    Col = (Ow * Stride_w) - Pad_w,
                                    Val = conv2d_full_channels(
                                        In_arr,
                                        K_arr,
                                        C_in,
                                        H,
                                        W,
                                        Kh,
                                        Kw,
                                        In_spatial,
                                        K_spatial,
                                        Batch_offset,
                                        Filter_offset,
                                        Row,
                                        Col,
                                        0,
                                        +0.0
                                    ),
                                    conv2d_full_loop(
                                        In_arr,
                                        K_arr,
                                        N,
                                        C_in,
                                        C_out,
                                        H,
                                        W,
                                        Kh,
                                        Kw,
                                        In_spatial,
                                        In_batch_size,
                                        K_spatial,
                                        K_filter_size,
                                        Stride_h,
                                        Stride_w,
                                        Pad_h,
                                        Pad_w,
                                        Out_h,
                                        Out_w,
                                        Batch,
                                        Oc,
                                        Oh,
                                        Ow + 1,
                                        [Val | Acc]
                                    )
                            end
                    end
            end
    end.

-file("src/viva_tensor/tensor.gleam", 1860).
?DOC(
    " Full convolution with batches and multiple output channels\n"
    " Full batched convolution - OPTIMIZED with O(1) array access\n"
).
-spec conv2d_full(
    tensor(),
    tensor(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    conv2d_config()
) -> {ok, tensor()} | {error, tensor_error()}.
conv2d_full(Input, Kernel, N, C_in, C_out, H, W, Kh, Kw, Config) ->
    Out_h = (case erlang:element(4, Config) of
        0 -> 0;
        Gleam@denominator -> ((H + (2 * erlang:element(6, Config))) - Kh) div Gleam@denominator
    end) + 1,
    Out_w = (case erlang:element(5, Config) of
        0 -> 0;
        Gleam@denominator@1 -> ((W + (2 * erlang:element(7, Config))) - Kw) div Gleam@denominator@1
    end) + 1,
    In_spatial = H * W,
    In_batch_size = C_in * In_spatial,
    K_spatial = Kh * Kw,
    K_filter_size = C_in * K_spatial,
    In_arr = viva_tensor_ffi:list_to_array(get_data(Input)),
    K_arr = viva_tensor_ffi:list_to_array(get_data(Kernel)),
    Output = conv2d_full_loop(
        In_arr,
        K_arr,
        N,
        C_in,
        C_out,
        H,
        W,
        Kh,
        Kw,
        In_spatial,
        In_batch_size,
        K_spatial,
        K_filter_size,
        erlang:element(4, Config),
        erlang:element(5, Config),
        erlang:element(6, Config),
        erlang:element(7, Config),
        Out_h,
        Out_w,
        0,
        0,
        0,
        0,
        []
    ),
    {ok, {tensor, lists:reverse(Output), [N, C_out, Out_h, Out_w]}}.

-file("src/viva_tensor/tensor.gleam", 1543).
?DOC(
    " Extract a patch from 2D tensor at position (row, col)\n"
    " 2D Convolution using optimized O(1) array access\n"
    " Input: [H, W] or [C, H, W] or [N, C, H, W]\n"
    " Kernel: [K_out, K_in, KH, KW] or [KH, KW] for single channel\n"
    " Output: [H_out, W_out] or [N, K_out, H_out, W_out]\n"
).
-spec conv2d(tensor(), tensor(), conv2d_config()) -> {ok, tensor()} |
    {error, tensor_error()}.
conv2d(Input, Kernel, Config) ->
    In_shape = shape(Input),
    K_shape = shape(Kernel),
    case {In_shape, K_shape} of
        {[H, W], [Kh, Kw]} ->
            conv2d_simple(Input, Kernel, H, W, Kh, Kw, Config);

        {[C_in, H@1, W@1], [C_k, Kh@1, Kw@1]} when C_in =:= C_k ->
            conv2d_multichannel(
                Input,
                Kernel,
                C_in,
                H@1,
                W@1,
                Kh@1,
                Kw@1,
                Config
            );

        {[N, C_in@1, H@2, W@2], [C_out, C_k@1, Kh@2, Kw@2]} when C_in@1 =:= C_k@1 ->
            conv2d_full(
                Input,
                Kernel,
                N,
                C_in@1,
                C_out,
                H@2,
                W@2,
                Kh@2,
                Kw@2,
                Config
            );

        {_, _} ->
            {error,
                {invalid_shape,
                    <<<<<<"conv2d shape mismatch: input="/utf8,
                                (shape_to_string(In_shape))/binary>>/binary,
                            " kernel="/utf8>>/binary,
                        (shape_to_string(K_shape))/binary>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 2182).
?DOC(" Pool over a window - returns max or sum depending on is_max\n").
-spec pool_window(
    erlang_array(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    boolean(),
    float()
) -> float().
pool_window(Arr, W, Row, Col, Pool_h, Pool_w, Base, Pr, Pc, Is_max, Acc) ->
    case Pr >= Pool_h of
        true ->
            Acc;

        false ->
            case Pc >= Pool_w of
                true ->
                    pool_window(
                        Arr,
                        W,
                        Row,
                        Col,
                        Pool_h,
                        Pool_w,
                        Base,
                        Pr + 1,
                        0,
                        Is_max,
                        Acc
                    );

                false ->
                    Idx = (Base + ((Row + Pr) * W)) + (Col + Pc),
                    Val = viva_tensor_ffi:array_get(Arr, Idx),
                    New_acc = case Is_max of
                        true ->
                            case Val > Acc of
                                true ->
                                    Val;

                                false ->
                                    Acc
                            end;

                        false ->
                            Acc + Val
                    end,
                    pool_window(
                        Arr,
                        W,
                        Row,
                        Col,
                        Pool_h,
                        Pool_w,
                        Base,
                        Pr,
                        Pc + 1,
                        Is_max,
                        New_acc
                    )
            end
    end.

-file("src/viva_tensor/tensor.gleam", 2058).
?DOC(" 2D pooling loop (tail recursive)\n").
-spec pool2d_loop(
    erlang_array(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    boolean(),
    list(float())
) -> list(float()).
pool2d_loop(
    Arr,
    H,
    W,
    Pool_h,
    Pool_w,
    Stride_h,
    Stride_w,
    Out_h,
    Out_w,
    Oh,
    Ow,
    Base,
    Is_max,
    Acc
) ->
    case Oh >= Out_h of
        true ->
            Acc;

        false ->
            case Ow >= Out_w of
                true ->
                    pool2d_loop(
                        Arr,
                        H,
                        W,
                        Pool_h,
                        Pool_w,
                        Stride_h,
                        Stride_w,
                        Out_h,
                        Out_w,
                        Oh + 1,
                        0,
                        Base,
                        Is_max,
                        Acc
                    );

                false ->
                    Row = Oh * Stride_h,
                    Col = Ow * Stride_w,
                    Val = pool_window(
                        Arr,
                        W,
                        Row,
                        Col,
                        Pool_h,
                        Pool_w,
                        Base,
                        0,
                        0,
                        Is_max,
                        case Is_max of
                            true ->
                                -1.0e308;

                            false ->
                                +0.0
                        end
                    ),
                    Final_val = case Is_max of
                        true ->
                            Val;

                        false ->
                            case erlang:float(Pool_h * Pool_w) of
                                +0.0 -> +0.0;
                                -0.0 -> -0.0;
                                Gleam@denominator -> Val / Gleam@denominator
                            end
                    end,
                    pool2d_loop(
                        Arr,
                        H,
                        W,
                        Pool_h,
                        Pool_w,
                        Stride_h,
                        Stride_w,
                        Out_h,
                        Out_w,
                        Oh,
                        Ow + 1,
                        Base,
                        Is_max,
                        [Final_val | Acc]
                    )
            end
    end.

-file("src/viva_tensor/tensor.gleam", 2107).
?DOC(" 4D pooling loop: batch -> channel -> oh -> ow\n").
-spec pool4d_loop(
    erlang_array(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    boolean(),
    list(float())
) -> list(float()).
pool4d_loop(
    Arr,
    N,
    C,
    H,
    W,
    Pool_h,
    Pool_w,
    Stride_h,
    Stride_w,
    Spatial_size,
    Batch_size,
    Out_h,
    Out_w,
    Batch,
    Channel,
    Oh,
    Ow,
    Is_max,
    Acc
) ->
    case Batch >= N of
        true ->
            Acc;

        false ->
            case Channel >= C of
                true ->
                    pool4d_loop(
                        Arr,
                        N,
                        C,
                        H,
                        W,
                        Pool_h,
                        Pool_w,
                        Stride_h,
                        Stride_w,
                        Spatial_size,
                        Batch_size,
                        Out_h,
                        Out_w,
                        Batch + 1,
                        0,
                        0,
                        0,
                        Is_max,
                        Acc
                    );

                false ->
                    case Oh >= Out_h of
                        true ->
                            pool4d_loop(
                                Arr,
                                N,
                                C,
                                H,
                                W,
                                Pool_h,
                                Pool_w,
                                Stride_h,
                                Stride_w,
                                Spatial_size,
                                Batch_size,
                                Out_h,
                                Out_w,
                                Batch,
                                Channel + 1,
                                0,
                                0,
                                Is_max,
                                Acc
                            );

                        false ->
                            case Ow >= Out_w of
                                true ->
                                    pool4d_loop(
                                        Arr,
                                        N,
                                        C,
                                        H,
                                        W,
                                        Pool_h,
                                        Pool_w,
                                        Stride_h,
                                        Stride_w,
                                        Spatial_size,
                                        Batch_size,
                                        Out_h,
                                        Out_w,
                                        Batch,
                                        Channel,
                                        Oh + 1,
                                        0,
                                        Is_max,
                                        Acc
                                    );

                                false ->
                                    Base = (Batch * Batch_size) + (Channel * Spatial_size),
                                    Row = Oh * Stride_h,
                                    Col = Ow * Stride_w,
                                    Val = pool_window(
                                        Arr,
                                        W,
                                        Row,
                                        Col,
                                        Pool_h,
                                        Pool_w,
                                        Base,
                                        0,
                                        0,
                                        Is_max,
                                        case Is_max of
                                            true ->
                                                -1.0e308;

                                            false ->
                                                +0.0
                                        end
                                    ),
                                    Final_val = case Is_max of
                                        true ->
                                            Val;

                                        false ->
                                            case erlang:float(Pool_h * Pool_w) of
                                                +0.0 -> +0.0;
                                                -0.0 -> -0.0;
                                                Gleam@denominator -> Val / Gleam@denominator
                                            end
                                    end,
                                    pool4d_loop(
                                        Arr,
                                        N,
                                        C,
                                        H,
                                        W,
                                        Pool_h,
                                        Pool_w,
                                        Stride_h,
                                        Stride_w,
                                        Spatial_size,
                                        Batch_size,
                                        Out_h,
                                        Out_w,
                                        Batch,
                                        Channel,
                                        Oh,
                                        Ow + 1,
                                        Is_max,
                                        [Final_val | Acc]
                                    )
                            end
                    end
            end
    end.

-file("src/viva_tensor/tensor.gleam", 2015).
?DOC(
    " Max pooling 2D - OPTIMIZED with O(1) array access\n"
    " Input: [H, W] or [N, C, H, W]\n"
    " Output: [H_out, W_out] or [N, C, H_out, W_out]\n"
).
-spec max_pool2d(tensor(), integer(), integer(), integer(), integer()) -> {ok,
        tensor()} |
    {error, tensor_error()}.
max_pool2d(Input, Pool_h, Pool_w, Stride_h, Stride_w) ->
    Shp = shape(Input),
    Arr = viva_tensor_ffi:list_to_array(get_data(Input)),
    case Shp of
        [H, W] ->
            Out_h = (case Stride_h of
                0 -> 0;
                Gleam@denominator -> (H - Pool_h) div Gleam@denominator
            end) + 1,
            Out_w = (case Stride_w of
                0 -> 0;
                Gleam@denominator@1 -> (W - Pool_w) div Gleam@denominator@1
            end) + 1,
            Output = pool2d_loop(
                Arr,
                H,
                W,
                Pool_h,
                Pool_w,
                Stride_h,
                Stride_w,
                Out_h,
                Out_w,
                0,
                0,
                0,
                true,
                []
            ),
            {ok, {tensor, lists:reverse(Output), [Out_h, Out_w]}};

        [N, C, H@1, W@1] ->
            Out_h@1 = (case Stride_h of
                0 -> 0;
                Gleam@denominator@2 -> (H@1 - Pool_h) div Gleam@denominator@2
            end) + 1,
            Out_w@1 = (case Stride_w of
                0 -> 0;
                Gleam@denominator@3 -> (W@1 - Pool_w) div Gleam@denominator@3
            end) + 1,
            Spatial_size = H@1 * W@1,
            Batch_size = C * Spatial_size,
            Output@1 = pool4d_loop(
                Arr,
                N,
                C,
                H@1,
                W@1,
                Pool_h,
                Pool_w,
                Stride_h,
                Stride_w,
                Spatial_size,
                Batch_size,
                Out_h@1,
                Out_w@1,
                0,
                0,
                0,
                0,
                true,
                []
            ),
            {ok, {tensor, lists:reverse(Output@1), [N, C, Out_h@1, Out_w@1]}};

        _ ->
            {error,
                {invalid_shape, <<"max_pool2d requires 2D or 4D tensor"/utf8>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 2218).
?DOC(
    " Average pooling 2D\n"
    " Average pooling 2D - OPTIMIZED with O(1) array access\n"
).
-spec avg_pool2d(tensor(), integer(), integer(), integer(), integer()) -> {ok,
        tensor()} |
    {error, tensor_error()}.
avg_pool2d(Input, Pool_h, Pool_w, Stride_h, Stride_w) ->
    Shp = shape(Input),
    Arr = viva_tensor_ffi:list_to_array(get_data(Input)),
    case Shp of
        [H, W] ->
            Out_h = (case Stride_h of
                0 -> 0;
                Gleam@denominator -> (H - Pool_h) div Gleam@denominator
            end) + 1,
            Out_w = (case Stride_w of
                0 -> 0;
                Gleam@denominator@1 -> (W - Pool_w) div Gleam@denominator@1
            end) + 1,
            Output = pool2d_loop(
                Arr,
                H,
                W,
                Pool_h,
                Pool_w,
                Stride_h,
                Stride_w,
                Out_h,
                Out_w,
                0,
                0,
                0,
                false,
                []
            ),
            {ok, {tensor, lists:reverse(Output), [Out_h, Out_w]}};

        [N, C, H@1, W@1] ->
            Out_h@1 = (case Stride_h of
                0 -> 0;
                Gleam@denominator@2 -> (H@1 - Pool_h) div Gleam@denominator@2
            end) + 1,
            Out_w@1 = (case Stride_w of
                0 -> 0;
                Gleam@denominator@3 -> (W@1 - Pool_w) div Gleam@denominator@3
            end) + 1,
            Spatial_size = H@1 * W@1,
            Batch_size = C * Spatial_size,
            Output@1 = pool4d_loop(
                Arr,
                N,
                C,
                H@1,
                W@1,
                Pool_h,
                Pool_w,
                Stride_h,
                Stride_w,
                Spatial_size,
                Batch_size,
                Out_h@1,
                Out_w@1,
                0,
                0,
                0,
                0,
                false,
                []
            ),
            {ok, {tensor, lists:reverse(Output@1), [N, C, Out_h@1, Out_w@1]}};

        _ ->
            {error,
                {invalid_shape, <<"avg_pool2d requires 2D or 4D tensor"/utf8>>}}
    end.

-file("src/viva_tensor/tensor.gleam", 2262).
?DOC(
    " Global average pooling - reduces spatial dimensions to 1x1\n"
    " Input: [N, C, H, W] -> Output: [N, C, 1, 1]\n"
).
-spec global_avg_pool2d(tensor()) -> {ok, tensor()} | {error, tensor_error()}.
global_avg_pool2d(Input) ->
    Shp = shape(Input),
    case Shp of
        [N, C, H, W] ->
            Spatial_size = H * W,
            Pool_size = erlang:float(Spatial_size),
            Batch_size = C * Spatial_size,
            Data = get_data(Input),
            Output = begin
                _pipe = gleam@list:range(0, N - 1),
                gleam@list:flat_map(
                    _pipe,
                    fun(Batch) -> _pipe@1 = gleam@list:range(0, C - 1),
                        gleam@list:map(
                            _pipe@1,
                            fun(Channel) ->
                                Base = (Batch * Batch_size) + (Channel * Spatial_size),
                                _pipe@2 = gleam@list:range(0, Spatial_size - 1),
                                _pipe@3 = gleam@list:fold(
                                    _pipe@2,
                                    +0.0,
                                    fun(Sum, I) ->
                                        case list_at_float(Data, Base + I) of
                                            {ok, V} ->
                                                Sum + V;

                                            {error, _} ->
                                                Sum
                                        end
                                    end
                                ),
                                (fun(S) -> case Pool_size of
                                        +0.0 -> +0.0;
                                        -0.0 -> -0.0;
                                        Gleam@denominator -> S / Gleam@denominator
                                    end end)(_pipe@3)
                            end
                        ) end
                )
            end,
            {ok, {tensor, Output, [N, C, 1, 1]}};

        _ ->
            {error,
                {invalid_shape,
                    <<"global_avg_pool2d requires 4D tensor [N, C, H, W]"/utf8>>}}
    end.
