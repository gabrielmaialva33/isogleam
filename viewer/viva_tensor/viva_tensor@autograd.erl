-module(viva_tensor@autograd).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/autograd.gleam").
-export([new_tape/0, new_variable/2, sequence/2, add/3, sub/3, mul/3, mean/2, matmul/3, transpose/2, relu/2, backward/2]).
-export_type([tape/0, variable/0, traced/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type tape() :: {tape,
        integer(),
        gleam@dict:dict(integer(), fun((viva_tensor@tensor:tensor()) -> list({integer(),
            viva_tensor@tensor:tensor()})))}.

-type variable() :: {variable, integer(), viva_tensor@tensor:tensor()}.

-type traced(FOW) :: {traced, FOW, tape()}.

-file("src/viva_tensor/autograd.gleam", 39).
?DOC(" Creates a new empty tape\n").
-spec new_tape() -> tape().
new_tape() ->
    {tape, 0, maps:new()}.

-file("src/viva_tensor/autograd.gleam", 44).
?DOC(" Registers a new variable (leaf node) in the graph\n").
-spec new_variable(tape(), viva_tensor@tensor:tensor()) -> traced(variable()).
new_variable(Tape, Data) ->
    Id = erlang:element(2, Tape),
    Var = {variable, Id, Data},
    New_tape = {tape, Id + 1, erlang:element(3, Tape)},
    {traced, Var, New_tape}.

-file("src/viva_tensor/autograd.gleam", 57).
?DOC(
    " Operation sequencing (Monadic Pipe)\n"
    " Allows chaining layers: x |> sequence(layer1) |> sequence(layer2)\n"
).
-spec sequence(
    {ok, traced(variable())} | {error, FPA},
    fun((tape(), variable()) -> {ok, traced(variable())} | {error, FPA})
) -> {ok, traced(variable())} | {error, FPA}.
sequence(Input, Layer_fn) ->
    gleam@result:'try'(
        Input,
        fun(_use0) ->
            {traced, Var, Tape} = _use0,
            Layer_fn(Tape, Var)
        end
    ).

-file("src/viva_tensor/autograd.gleam", 66).
?DOC(" Traced addition: c = a + b (supports broadcasting)\n").
-spec add(tape(), variable(), variable()) -> {ok, traced(variable())} |
    {error, viva_tensor@tensor:tensor_error()}.
add(Tape, A, B) ->
    gleam@result:'try'(
        viva_tensor@tensor:add_broadcast(
            erlang:element(3, A),
            erlang:element(3, B)
        ),
        fun(Res_data) ->
            Res_id = erlang:element(2, Tape),
            Backward = fun(Grad) ->
                Grad_a = case erlang:element(3, Grad) =:= erlang:element(
                    3,
                    erlang:element(3, A)
                ) of
                    true ->
                        Grad;

                    false ->
                        Res@1 = case viva_tensor@tensor:sum_axis(Grad, 0) of
                            {ok, Res} -> Res;
                            _assert_fail ->
                                erlang:error(#{gleam_error => let_assert,
                                            message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                            file => <<?FILEPATH/utf8>>,
                                            module => <<"viva_tensor/autograd"/utf8>>,
                                            function => <<"add"/utf8>>,
                                            line => 83,
                                            value => _assert_fail,
                                            start => 2548,
                                            'end' => 2593,
                                            pattern_start => 2559,
                                            pattern_end => 2566})
                        end,
                        Res@1
                end,
                Grad_b = case erlang:element(3, Grad) =:= erlang:element(
                    3,
                    erlang:element(3, B)
                ) of
                    true ->
                        Grad;

                    false ->
                        Res@3 = case viva_tensor@tensor:sum_axis(Grad, 0) of
                            {ok, Res@2} -> Res@2;
                            _assert_fail@1 ->
                                erlang:error(#{gleam_error => let_assert,
                                            message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                            file => <<?FILEPATH/utf8>>,
                                            module => <<"viva_tensor/autograd"/utf8>>,
                                            function => <<"add"/utf8>>,
                                            line => 91,
                                            value => _assert_fail@1,
                                            start => 2716,
                                            'end' => 2761,
                                            pattern_start => 2727,
                                            pattern_end => 2734})
                        end,
                        Res@3
                end,
                [{erlang:element(2, A), Grad_a}, {erlang:element(2, B), Grad_b}]
            end,
            New_ops = gleam@dict:insert(
                erlang:element(3, Tape),
                Res_id,
                Backward
            ),
            New_tape = {tape, Res_id + 1, New_ops},
            {ok, {traced, {variable, Res_id, Res_data}, New_tape}}
        end
    ).

-file("src/viva_tensor/autograd.gleam", 106).
?DOC(" Traced subtraction: c = a - b\n").
-spec sub(tape(), variable(), variable()) -> {ok, traced(variable())} |
    {error, viva_tensor@tensor:tensor_error()}.
sub(Tape, A, B) ->
    gleam@result:'try'(
        viva_tensor@tensor:sub(erlang:element(3, A), erlang:element(3, B)),
        fun(Res_data) ->
            Res_id = erlang:element(2, Tape),
            Backward = fun(Grad) ->
                Neg_grad = viva_tensor@tensor:negate(Grad),
                [{erlang:element(2, A), Grad}, {erlang:element(2, B), Neg_grad}]
            end,
            New_ops = gleam@dict:insert(
                erlang:element(3, Tape),
                Res_id,
                Backward
            ),
            New_tape = {tape, Res_id + 1, New_ops},
            {ok, {traced, {variable, Res_id, Res_data}, New_tape}}
        end
    ).

-file("src/viva_tensor/autograd.gleam", 130).
?DOC(" Traced Element-wise Multiplication: c = a * b\n").
-spec mul(tape(), variable(), variable()) -> {ok, traced(variable())} |
    {error, viva_tensor@tensor:tensor_error()}.
mul(Tape, A, B) ->
    gleam@result:'try'(
        viva_tensor@tensor:mul(erlang:element(3, A), erlang:element(3, B)),
        fun(Res_data) ->
            Res_id = erlang:element(2, Tape),
            Backward = fun(Grad) ->
                Grad_a@1 = case viva_tensor@tensor:mul(
                    Grad,
                    erlang:element(3, B)
                ) of
                    {ok, Grad_a} -> Grad_a;
                    _assert_fail ->
                        erlang:error(#{gleam_error => let_assert,
                                    message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"viva_tensor/autograd"/utf8>>,
                                    function => <<"mul"/utf8>>,
                                    line => 144,
                                    value => _assert_fail,
                                    start => 4043,
                                    'end' => 4091,
                                    pattern_start => 4054,
                                    pattern_end => 4064})
                end,
                Grad_b@1 = case viva_tensor@tensor:mul(
                    Grad,
                    erlang:element(3, A)
                ) of
                    {ok, Grad_b} -> Grad_b;
                    _assert_fail@1 ->
                        erlang:error(#{gleam_error => let_assert,
                                    message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"viva_tensor/autograd"/utf8>>,
                                    function => <<"mul"/utf8>>,
                                    line => 145,
                                    value => _assert_fail@1,
                                    start => 4096,
                                    'end' => 4144,
                                    pattern_start => 4107,
                                    pattern_end => 4117})
                end,
                [{erlang:element(2, A), Grad_a@1},
                    {erlang:element(2, B), Grad_b@1}]
            end,
            New_ops = gleam@dict:insert(
                erlang:element(3, Tape),
                Res_id,
                Backward
            ),
            New_tape = {tape, Res_id + 1, New_ops},
            {ok, {traced, {variable, Res_id, Res_data}, New_tape}}
        end
    ).

-file("src/viva_tensor/autograd.gleam", 157).
?DOC(
    " Traced Mean (Reduce Mean): y = mean(x)\n"
    " Returns a scalar Tensor (rank 0 or 1 depending on base implementation, here we force 1)\n"
).
-spec mean(tape(), variable()) -> traced(variable()).
mean(Tape, A) ->
    Val = viva_tensor@tensor:mean(erlang:element(3, A)),
    Res_data = viva_tensor@tensor:from_list([Val]),
    Res_id = erlang:element(2, Tape),
    Backward = fun(Grad) ->
        N = begin
            _pipe = viva_tensor@tensor:size(erlang:element(3, A)),
            erlang:float(_pipe)
        end,
        Grad_val = begin
            _pipe@1 = viva_tensor@tensor:to_list(Grad),
            _pipe@2 = gleam@list:first(_pipe@1),
            gleam@result:unwrap(_pipe@2, 1.0)
        end,
        Scaled_grad_val = case N of
            +0.0 -> +0.0;
            -0.0 -> -0.0;
            Gleam@denominator -> Grad_val / Gleam@denominator
        end,
        Grad_input = viva_tensor@tensor:fill(
            erlang:element(3, erlang:element(3, A)),
            Scaled_grad_val
        ),
        [{erlang:element(2, A), Grad_input}]
    end,
    New_ops = gleam@dict:insert(erlang:element(3, Tape), Res_id, Backward),
    New_tape = {tape, Res_id + 1, New_ops},
    {traced, {variable, Res_id, Res_data}, New_tape}.

-file("src/viva_tensor/autograd.gleam", 185).
?DOC(" Traced Matrix Multiplication: c = a @ b\n").
-spec matmul(tape(), variable(), variable()) -> {ok, traced(variable())} |
    {error, viva_tensor@tensor:tensor_error()}.
matmul(Tape, A, B) ->
    gleam@result:'try'(
        viva_tensor@tensor:matmul(erlang:element(3, A), erlang:element(3, B)),
        fun(Res_data) ->
            Res_id = erlang:element(2, Tape),
            Backward = fun(Grad) ->
                Bt@1 = case viva_tensor@tensor:transpose(erlang:element(3, B)) of
                    {ok, Bt} -> Bt;
                    _assert_fail ->
                        erlang:error(#{gleam_error => let_assert,
                                    message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"viva_tensor/autograd"/utf8>>,
                                    function => <<"matmul"/utf8>>,
                                    line => 199,
                                    value => _assert_fail,
                                    start => 5862,
                                    'end' => 5906,
                                    pattern_start => 5873,
                                    pattern_end => 5879})
                end,
                At@1 = case viva_tensor@tensor:transpose(erlang:element(3, A)) of
                    {ok, At} -> At;
                    _assert_fail@1 ->
                        erlang:error(#{gleam_error => let_assert,
                                    message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"viva_tensor/autograd"/utf8>>,
                                    function => <<"matmul"/utf8>>,
                                    line => 200,
                                    value => _assert_fail@1,
                                    start => 5911,
                                    'end' => 5955,
                                    pattern_start => 5922,
                                    pattern_end => 5928})
                end,
                Grad_a@1 = case viva_tensor@tensor:matmul(Grad, Bt@1) of
                    {ok, Grad_a} -> Grad_a;
                    _assert_fail@2 ->
                        erlang:error(#{gleam_error => let_assert,
                                    message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"viva_tensor/autograd"/utf8>>,
                                    function => <<"matmul"/utf8>>,
                                    line => 202,
                                    value => _assert_fail@2,
                                    start => 5961,
                                    'end' => 6008,
                                    pattern_start => 5972,
                                    pattern_end => 5982})
                end,
                Grad_b@1 = case viva_tensor@tensor:matmul(At@1, Grad) of
                    {ok, Grad_b} -> Grad_b;
                    _assert_fail@3 ->
                        erlang:error(#{gleam_error => let_assert,
                                    message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"viva_tensor/autograd"/utf8>>,
                                    function => <<"matmul"/utf8>>,
                                    line => 203,
                                    value => _assert_fail@3,
                                    start => 6013,
                                    'end' => 6060,
                                    pattern_start => 6024,
                                    pattern_end => 6034})
                end,
                [{erlang:element(2, A), Grad_a@1},
                    {erlang:element(2, B), Grad_b@1}]
            end,
            New_ops = gleam@dict:insert(
                erlang:element(3, Tape),
                Res_id,
                Backward
            ),
            New_tape = {tape, Res_id + 1, New_ops},
            {ok, {traced, {variable, Res_id, Res_data}, New_tape}}
        end
    ).

-file("src/viva_tensor/autograd.gleam", 215).
?DOC(" Traced Transpose: c = a.T\n").
-spec transpose(tape(), variable()) -> {ok, traced(variable())} |
    {error, viva_tensor@tensor:tensor_error()}.
transpose(Tape, A) ->
    gleam@result:'try'(
        viva_tensor@tensor:transpose(erlang:element(3, A)),
        fun(Res_data) ->
            Res_id = erlang:element(2, Tape),
            Backward = fun(Grad) ->
                Grad_t@1 = case viva_tensor@tensor:transpose(Grad) of
                    {ok, Grad_t} -> Grad_t;
                    _assert_fail ->
                        erlang:error(#{gleam_error => let_assert,
                                    message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"viva_tensor/autograd"/utf8>>,
                                    function => <<"transpose"/utf8>>,
                                    line => 225,
                                    value => _assert_fail,
                                    start => 6606,
                                    'end' => 6652,
                                    pattern_start => 6617,
                                    pattern_end => 6627})
                end,
                [{erlang:element(2, A), Grad_t@1}]
            end,
            New_ops = gleam@dict:insert(
                erlang:element(3, Tape),
                Res_id,
                Backward
            ),
            New_tape = {tape, Res_id + 1, New_ops},
            {ok, {traced, {variable, Res_id, Res_data}, New_tape}}
        end
    ).

-file("src/viva_tensor/autograd.gleam", 236).
?DOC(" Traced ReLU\n").
-spec relu(tape(), variable()) -> traced(variable()).
relu(Tape, A) ->
    Res_data = viva_tensor@tensor:map(
        erlang:element(3, A),
        fun(X) -> case X > +0.0 of
                true ->
                    X;

                false ->
                    +0.0
            end end
    ),
    Res_id = erlang:element(2, Tape),
    Backward = fun(Grad) ->
        Mask = viva_tensor@tensor:map(
            erlang:element(3, A),
            fun(X@1) -> case X@1 > +0.0 of
                    true ->
                        1.0;

                    false ->
                        +0.0
                end end
        ),
        Grad_a@1 = case viva_tensor@tensor:mul(Grad, Mask) of
            {ok, Grad_a} -> Grad_a;
            _assert_fail ->
                erlang:error(#{gleam_error => let_assert,
                            message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"viva_tensor/autograd"/utf8>>,
                            function => <<"relu"/utf8>>,
                            line => 256,
                            value => _assert_fail,
                            start => 7346,
                            'end' => 7392,
                            pattern_start => 7357,
                            pattern_end => 7367})
        end,
        [{erlang:element(2, A), Grad_a@1}]
    end,
    New_ops = gleam@dict:insert(erlang:element(3, Tape), Res_id, Backward),
    New_tape = {tape, Res_id + 1, New_ops},
    {traced, {variable, Res_id, Res_data}, New_tape}.

-file("src/viva_tensor/autograd.gleam", 331).
-spec string_shape(list(integer())) -> binary().
string_shape(Shape) ->
    <<<<"["/utf8,
            (gleam@string:join(
                gleam@list:map(Shape, fun erlang:integer_to_binary/1),
                <<", "/utf8>>
            ))/binary>>/binary,
        "]"/utf8>>.

-file("src/viva_tensor/autograd.gleam", 272).
?DOC(
    " Executes backpropagation starting from a scalar variable (loss).\n"
    " Returns a Map of NodeId -> Gradient (Tensor)\n"
).
-spec backward(tape(), variable()) -> {ok,
        gleam@dict:dict(integer(), viva_tensor@tensor:tensor())} |
    {error, binary()}.
backward(Tape, Loss) ->
    Initial_grad = viva_tensor@tensor:ones(
        erlang:element(3, erlang:element(3, Loss))
    ),
    Initial_grads = maps:from_list([{erlang:element(2, Loss), Initial_grad}]),
    All_ids = gleam@list:range(erlang:element(2, Tape) - 1, 0),
    Final_grads = gleam@list:fold(
        All_ids,
        Initial_grads,
        fun(Grads, Current_id) ->
            case gleam_stdlib:map_get(Grads, Current_id) of
                {error, _} ->
                    Grads;

                {ok, Current_grad} ->
                    case gleam_stdlib:map_get(
                        erlang:element(3, Tape),
                        Current_id
                    ) of
                        {error, _} ->
                            Grads;

                        {ok, Back_fn} ->
                            Parent_grads = Back_fn(Current_grad),
                            gleam@list:fold(
                                Parent_grads,
                                Grads,
                                fun(Acc_grads, Pair) ->
                                    {Pid, Pgrad} = Pair,
                                    case gleam_stdlib:map_get(Acc_grads, Pid) of
                                        {error, _} ->
                                            gleam@dict:insert(
                                                Acc_grads,
                                                Pid,
                                                Pgrad
                                            );

                                        {ok, Existing} ->
                                            case erlang:element(3, Existing) =:= erlang:element(
                                                3,
                                                Pgrad
                                            ) of
                                                true ->
                                                    Sum@1 = case viva_tensor@tensor:add(
                                                        Existing,
                                                        Pgrad
                                                    ) of
                                                        {ok, Sum} -> Sum;
                                                        _assert_fail ->
                                                            erlang:error(
                                                                    #{gleam_error => let_assert,
                                                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                                        file => <<?FILEPATH/utf8>>,
                                                                        module => <<"viva_tensor/autograd"/utf8>>,
                                                                        function => <<"backward"/utf8>>,
                                                                        line => 305,
                                                                        value => _assert_fail,
                                                                        start => 9387,
                                                                        'end' => 9435,
                                                                        pattern_start => 9398,
                                                                        pattern_end => 9405}
                                                                )
                                                    end,
                                                    gleam@dict:insert(
                                                        Acc_grads,
                                                        Pid,
                                                        Sum@1
                                                    );

                                                false ->
                                                    Msg = <<<<<<<<<<"ShapeMismatch at node "/utf8,
                                                                        (erlang:integer_to_binary(
                                                                            Pid
                                                                        ))/binary>>/binary,
                                                                    ": existing="/utf8>>/binary,
                                                                (string_shape(
                                                                    erlang:element(
                                                                        3,
                                                                        Existing
                                                                    )
                                                                ))/binary>>/binary,
                                                            ", new="/utf8>>/binary,
                                                        (string_shape(
                                                            erlang:element(
                                                                3,
                                                                Pgrad
                                                            )
                                                        ))/binary>>,
                                                    erlang:error(
                                                        #{gleam_error => panic,
                                                            message => Msg,
                                                            file => <<?FILEPATH/utf8>>,
                                                            module => <<"viva_tensor/autograd"/utf8>>,
                                                            function => <<"backward"/utf8>>,
                                                            line => 316}
                                                    )
                                            end
                                    end
                                end
                            )
                    end
            end
        end
    ),
    {ok, Final_grads}.
