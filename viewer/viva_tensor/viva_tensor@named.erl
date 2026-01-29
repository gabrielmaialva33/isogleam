-module(viva_tensor@named).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/named.gleam").
-export([from_tensor/1, zeros/1, ones/1, random/1, randn/3, find_axis/2, has_axis/2, axis_names/1, shape/1, rank/1, size/1, rename_axis/3, unsqueeze/3, add/2, mul/2, scale/2, map/2, to_tensor/1, describe/1, new/2, axis_size/2, squeeze/2, sum_along/2, mean_along/2]).
-export_type([named_tensor/0, named_tensor_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Named Tensor - Tensors with semantic axis names\n"
    "\n"
    " Wrap tensors with named axes for clearer, safer operations.\n"
    " Instead of sum(t, axis: 0), write sum(t, along: Batch)\n"
).

-type named_tensor() :: {named_tensor,
        viva_tensor@tensor:tensor(),
        list(viva_tensor@axis:axis_spec())}.

-type named_tensor_error() :: {axis_not_found, viva_tensor@axis:axis()} |
    {duplicate_axis, viva_tensor@axis:axis()} |
    {axis_mismatch, viva_tensor@axis:axis(), viva_tensor@axis:axis()} |
    {size_mismatch, viva_tensor@axis:axis(), integer(), integer()} |
    {broadcast_err, binary()} |
    {tensor_err, viva_tensor@tensor:tensor_error()} |
    {invalid_op, binary()}.

-file("src/viva_tensor/named.gleam", 83).
?DOC(" Create from tensor with inferred anonymous axes\n").
-spec from_tensor(viva_tensor@tensor:tensor()) -> named_tensor().
from_tensor(T) ->
    Axes = gleam@list:map(
        erlang:element(3, T),
        fun(Size) -> {axis_spec, anon, Size} end
    ),
    {named_tensor, T, Axes}.

-file("src/viva_tensor/named.gleam", 89).
?DOC(" Create named tensor of zeros\n").
-spec zeros(list(viva_tensor@axis:axis_spec())) -> named_tensor().
zeros(Axes) ->
    Shape = gleam@list:map(Axes, fun(A) -> erlang:element(3, A) end),
    Data = viva_tensor@tensor:zeros(Shape),
    {named_tensor, Data, Axes}.

-file("src/viva_tensor/named.gleam", 96).
?DOC(" Create named tensor of ones\n").
-spec ones(list(viva_tensor@axis:axis_spec())) -> named_tensor().
ones(Axes) ->
    Shape = gleam@list:map(Axes, fun(A) -> erlang:element(3, A) end),
    Data = viva_tensor@tensor:ones(Shape),
    {named_tensor, Data, Axes}.

-file("src/viva_tensor/named.gleam", 103).
?DOC(" Create named tensor with random values [0, 1)\n").
-spec random(list(viva_tensor@axis:axis_spec())) -> named_tensor().
random(Axes) ->
    Shape = gleam@list:map(Axes, fun(A) -> erlang:element(3, A) end),
    Data = viva_tensor@tensor:random_uniform(Shape),
    {named_tensor, Data, Axes}.

-file("src/viva_tensor/named.gleam", 110).
?DOC(" Create named tensor with normal distribution\n").
-spec randn(list(viva_tensor@axis:axis_spec()), float(), float()) -> named_tensor().
randn(Axes, Mean, Std) ->
    Shape = gleam@list:map(Axes, fun(A) -> erlang:element(3, A) end),
    Data = viva_tensor@tensor:random_normal(Shape, Mean, Std),
    {named_tensor, Data, Axes}.

-file("src/viva_tensor/named.gleam", 125).
-spec find_axis_in_list(
    list(viva_tensor@axis:axis_spec()),
    viva_tensor@axis:axis(),
    integer()
) -> {ok, integer()} | {error, named_tensor_error()}.
find_axis_in_list(Axes, Name, Idx) ->
    case Axes of
        [] ->
            {error, {axis_not_found, Name}};

        [First | Rest] ->
            case viva_tensor@axis:equals(erlang:element(2, First), Name) of
                true ->
                    {ok, Idx};

                false ->
                    find_axis_in_list(Rest, Name, Idx + 1)
            end
    end.

-file("src/viva_tensor/named.gleam", 121).
?DOC(" Find axis index by name\n").
-spec find_axis(named_tensor(), viva_tensor@axis:axis()) -> {ok, integer()} |
    {error, named_tensor_error()}.
find_axis(T, Name) ->
    find_axis_in_list(erlang:element(3, T), Name, 0).

-file("src/viva_tensor/named.gleam", 155).
?DOC(" Check if tensor has axis\n").
-spec has_axis(named_tensor(), viva_tensor@axis:axis()) -> boolean().
has_axis(T, Name) ->
    case find_axis(T, Name) of
        {ok, _} ->
            true;

        {error, _} ->
            false
    end.

-file("src/viva_tensor/named.gleam", 163).
?DOC(" Get all axis names\n").
-spec axis_names(named_tensor()) -> list(viva_tensor@axis:axis()).
axis_names(T) ->
    gleam@list:map(erlang:element(3, T), fun(A) -> erlang:element(2, A) end).

-file("src/viva_tensor/named.gleam", 168).
?DOC(" Get shape as list\n").
-spec shape(named_tensor()) -> list(integer()).
shape(T) ->
    erlang:element(3, erlang:element(2, T)).

-file("src/viva_tensor/named.gleam", 173).
?DOC(" Get rank (number of dimensions)\n").
-spec rank(named_tensor()) -> integer().
rank(T) ->
    erlang:length(erlang:element(3, T)).

-file("src/viva_tensor/named.gleam", 178).
?DOC(" Total number of elements\n").
-spec size(named_tensor()) -> integer().
size(T) ->
    viva_tensor@tensor:size(erlang:element(2, T)).

-file("src/viva_tensor/named.gleam", 183).
?DOC(" Rename an axis\n").
-spec rename_axis(
    named_tensor(),
    viva_tensor@axis:axis(),
    viva_tensor@axis:axis()
) -> {ok, named_tensor()} | {error, named_tensor_error()}.
rename_axis(T, From, To) ->
    case find_axis(T, From) of
        {error, E} ->
            {error, E};

        {ok, Idx} ->
            New_axes = gleam@list:index_map(
                erlang:element(3, T),
                fun(Spec, I) -> case I =:= Idx of
                        true ->
                            {axis_spec, To, erlang:element(3, Spec)};

                        false ->
                            Spec
                    end end
            ),
            {ok, {named_tensor, erlang:element(2, T), New_axes}}
    end.

-file("src/viva_tensor/named.gleam", 204).
?DOC(" Add a new axis of size 1\n").
-spec unsqueeze(named_tensor(), viva_tensor@axis:axis(), integer()) -> named_tensor().
unsqueeze(T, Name, Position) ->
    New_spec = {axis_spec, Name, 1},
    {Before, After} = gleam@list:split(erlang:element(3, T), Position),
    New_axes = lists:append([Before, [New_spec], After]),
    New_data = viva_tensor@tensor:unsqueeze(erlang:element(2, T), Position),
    {named_tensor, New_data, New_axes}.

-file("src/viva_tensor/named.gleam", 284).
?DOC(" Element-wise add (same axes required)\n").
-spec add(named_tensor(), named_tensor()) -> {ok, named_tensor()} |
    {error, named_tensor_error()}.
add(A, B) ->
    case viva_tensor@axis:specs_equal(
        erlang:element(3, A),
        erlang:element(3, B)
    ) of
        false ->
            {error, {invalid_op, <<"Axes don't match"/utf8>>}};

        true ->
            case viva_tensor@tensor:add(
                erlang:element(2, A),
                erlang:element(2, B)
            ) of
                {error, E} ->
                    {error, {tensor_err, E}};

                {ok, Result} ->
                    {ok, {named_tensor, Result, erlang:element(3, A)}}
            end
    end.

-file("src/viva_tensor/named.gleam", 300).
?DOC(" Element-wise mul (same axes required)\n").
-spec mul(named_tensor(), named_tensor()) -> {ok, named_tensor()} |
    {error, named_tensor_error()}.
mul(A, B) ->
    case viva_tensor@axis:specs_equal(
        erlang:element(3, A),
        erlang:element(3, B)
    ) of
        false ->
            {error, {invalid_op, <<"Axes don't match"/utf8>>}};

        true ->
            case viva_tensor@tensor:mul(
                erlang:element(2, A),
                erlang:element(2, B)
            ) of
                {error, E} ->
                    {error, {tensor_err, E}};

                {ok, Result} ->
                    {ok, {named_tensor, Result, erlang:element(3, A)}}
            end
    end.

-file("src/viva_tensor/named.gleam", 316).
?DOC(" Scale by constant\n").
-spec scale(named_tensor(), float()) -> named_tensor().
scale(T, S) ->
    {named_tensor,
        viva_tensor@tensor:scale(erlang:element(2, T), S),
        erlang:element(3, T)}.

-file("src/viva_tensor/named.gleam", 321).
?DOC(" Map function over elements\n").
-spec map(named_tensor(), fun((float()) -> float())) -> named_tensor().
map(T, F) ->
    {named_tensor,
        viva_tensor@tensor:map(erlang:element(2, T), F),
        erlang:element(3, T)}.

-file("src/viva_tensor/named.gleam", 330).
?DOC(" Convert to plain tensor (drop names)\n").
-spec to_tensor(named_tensor()) -> viva_tensor@tensor:tensor().
to_tensor(T) ->
    erlang:element(2, T).

-file("src/viva_tensor/named.gleam", 335).
?DOC(" Pretty print tensor info\n").
-spec describe(named_tensor()) -> binary().
describe(T) ->
    Axes_str = begin
        _pipe = erlang:element(3, T),
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(A) ->
                <<<<(viva_tensor@axis:to_string(erlang:element(2, A)))/binary,
                        ":"/utf8>>/binary,
                    (erlang:integer_to_binary(erlang:element(3, A)))/binary>>
            end
        ),
        gleam@string:join(_pipe@1, <<", "/utf8>>)
    end,
    <<<<"NamedTensor["/utf8, Axes_str/binary>>/binary, "]"/utf8>>.

-file("src/viva_tensor/named.gleam", 348).
-spec validate_sizes(list(integer()), list(viva_tensor@axis:axis_spec())) -> {ok,
        nil} |
    {error, named_tensor_error()}.
validate_sizes(Shape, Axes) ->
    case {Shape, Axes} of
        {[], []} ->
            {ok, nil};

        {[S | S_rest], [A | A_rest]} ->
            case S =:= erlang:element(3, A) of
                true ->
                    validate_sizes(S_rest, A_rest);

                false ->
                    {error,
                        {size_mismatch,
                            erlang:element(2, A),
                            erlang:element(3, A),
                            S}}
            end;

        {_, _} ->
            {error, {invalid_op, <<"Shape and axes length mismatch"/utf8>>}}
    end.

-file("src/viva_tensor/named.gleam", 379).
-spec has_duplicates(list(viva_tensor@axis:axis())) -> boolean().
has_duplicates(Items) ->
    case Items of
        [] ->
            false;

        [First | Rest] ->
            case gleam@list:any(
                Rest,
                fun(X) -> viva_tensor@axis:equals(X, First) end
            ) of
                true ->
                    true;

                false ->
                    has_duplicates(Rest)
            end
    end.

-file("src/viva_tensor/named.gleam", 364).
-spec validate_unique_names(list(viva_tensor@axis:axis_spec())) -> {ok, nil} |
    {error, named_tensor_error()}.
validate_unique_names(Axes) ->
    Named_axes = gleam@list:filter(Axes, fun(A) -> case erlang:element(2, A) of
                anon ->
                    false;

                _ ->
                    true
            end end),
    Names = gleam@list:map(Named_axes, fun(A@1) -> erlang:element(2, A@1) end),
    case has_duplicates(Names) of
        true ->
            {error, {duplicate_axis, anon}};

        false ->
            {ok, nil}
    end.

-file("src/viva_tensor/named.gleam", 52).
?DOC(" Create named tensor from data and axis specs\n").
-spec new(viva_tensor@tensor:tensor(), list(viva_tensor@axis:axis_spec())) -> {ok,
        named_tensor()} |
    {error, named_tensor_error()}.
new(Data, Axes) ->
    Data_rank = viva_tensor@tensor:rank(Data),
    Axes_count = erlang:length(Axes),
    case Data_rank =:= Axes_count of
        false ->
            {error,
                {invalid_op,
                    <<<<<<<<"Axis count ("/utf8,
                                    (erlang:integer_to_binary(Axes_count))/binary>>/binary,
                                ") doesn't match tensor rank ("/utf8>>/binary,
                            (erlang:integer_to_binary(Data_rank))/binary>>/binary,
                        ")"/utf8>>}};

        true ->
            case validate_sizes(erlang:element(3, Data), Axes) of
                {error, E} ->
                    {error, E};

                {ok, _} ->
                    case validate_unique_names(Axes) of
                        {error, E@1} ->
                            {error, E@1};

                        {ok, _} ->
                            {ok, {named_tensor, Data, Axes}}
                    end
            end
    end.

-file("src/viva_tensor/named.gleam", 391).
-spec list_at(list(HUX), integer()) -> {ok, HUX} | {error, nil}.
list_at(Lst, Idx) ->
    _pipe = Lst,
    _pipe@1 = gleam@list:drop(_pipe, Idx),
    gleam@list:first(_pipe@1).

-file("src/viva_tensor/named.gleam", 142).
?DOC(" Get axis size by name\n").
-spec axis_size(named_tensor(), viva_tensor@axis:axis()) -> {ok, integer()} |
    {error, named_tensor_error()}.
axis_size(T, Name) ->
    case find_axis(T, Name) of
        {error, E} ->
            {error, E};

        {ok, Idx} ->
            case list_at(erlang:element(3, T), Idx) of
                {error, _} ->
                    {error, {axis_not_found, Name}};

                {ok, Spec} ->
                    {ok, erlang:element(3, Spec)}
            end
    end.

-file("src/viva_tensor/named.gleam", 397).
-spec remove_at(list(HVB), integer()) -> list(HVB).
remove_at(Lst, Idx) ->
    _pipe = Lst,
    _pipe@1 = gleam@list:index_map(_pipe, fun(Item, I) -> {Item, I} end),
    _pipe@2 = gleam@list:filter(
        _pipe@1,
        fun(Pair) -> erlang:element(2, Pair) /= Idx end
    ),
    gleam@list:map(_pipe@2, fun(Pair@1) -> erlang:element(1, Pair@1) end).

-file("src/viva_tensor/named.gleam", 213).
?DOC(" Remove axis of size 1 by name\n").
-spec squeeze(named_tensor(), viva_tensor@axis:axis()) -> {ok, named_tensor()} |
    {error, named_tensor_error()}.
squeeze(T, Name) ->
    case find_axis(T, Name) of
        {error, E} ->
            {error, E};

        {ok, Idx} ->
            case list_at(erlang:element(3, T), Idx) of
                {error, _} ->
                    {error, {axis_not_found, Name}};

                {ok, Spec} ->
                    case erlang:element(3, Spec) =:= 1 of
                        false ->
                            {error,
                                {invalid_op,
                                    <<"Cannot squeeze axis with size != 1"/utf8>>}};

                        true ->
                            case viva_tensor@tensor:squeeze_axis(
                                erlang:element(2, T),
                                Idx
                            ) of
                                {error, E@1} ->
                                    {error, {tensor_err, E@1}};

                                {ok, Squeezed} ->
                                    New_axes = remove_at(
                                        erlang:element(3, T),
                                        Idx
                                    ),
                                    {ok, {named_tensor, Squeezed, New_axes}}
                            end
                    end
            end
    end.

-file("src/viva_tensor/named.gleam", 246).
?DOC(" Sum along named axis\n").
-spec sum_along(named_tensor(), viva_tensor@axis:axis()) -> {ok, named_tensor()} |
    {error, named_tensor_error()}.
sum_along(T, Axis_name) ->
    case find_axis(T, Axis_name) of
        {error, E} ->
            {error, E};

        {ok, Idx} ->
            case viva_tensor@tensor:sum_axis(erlang:element(2, T), Idx) of
                {error, E@1} ->
                    {error, {tensor_err, E@1}};

                {ok, Summed} ->
                    New_axes = remove_at(erlang:element(3, T), Idx),
                    {ok, {named_tensor, Summed, New_axes}}
            end
    end.

-file("src/viva_tensor/named.gleam", 265).
?DOC(" Mean along named axis\n").
-spec mean_along(named_tensor(), viva_tensor@axis:axis()) -> {ok,
        named_tensor()} |
    {error, named_tensor_error()}.
mean_along(T, Axis_name) ->
    case find_axis(T, Axis_name) of
        {error, E} ->
            {error, E};

        {ok, Idx} ->
            case viva_tensor@tensor:mean_axis(erlang:element(2, T), Idx) of
                {error, E@1} ->
                    {error, {tensor_err, E@1}};

                {ok, Meaned} ->
                    New_axes = remove_at(erlang:element(3, T), Idx),
                    {ok, {named_tensor, Meaned, New_axes}}
            end
    end.
