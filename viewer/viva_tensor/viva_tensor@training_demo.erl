-module(viva_tensor@training_demo).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/training_demo.gleam").
-export([main/0]).
-export_type([training_state/0]).

-type training_state() :: {training_state,
        viva_tensor@autograd:tape(),
        viva_tensor@nn:linear(),
        viva_tensor@nn:linear()}.

-file("src/viva_tensor/training_demo.gleam", 57).
-spec train_step(
    training_state(),
    integer(),
    viva_tensor@tensor:tensor(),
    viva_tensor@tensor:tensor()
) -> training_state().
train_step(State, Epoch, X_data, Y_data) ->
    {traced, X, Tape1} = viva_tensor@autograd:new_variable(
        erlang:element(2, State),
        X_data
    ),
    {traced, Target, Tape2} = viva_tensor@autograd:new_variable(Tape1, Y_data),
    {L1_out@1, Tape3@1} = case viva_tensor@nn:linear_forward(
        Tape2,
        erlang:element(3, State),
        X
    ) of
        {ok, {traced, L1_out, Tape3}} -> {L1_out, Tape3};
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"train_step"/utf8>>,
                        line => 68,
                        value => _assert_fail,
                        start => 1801,
                        'end' => 1885,
                        pattern_start => 1812,
                        pattern_end => 1837})
    end,
    {traced, Hidden_act, Tape4} = viva_tensor@nn:relu(Tape3@1, L1_out@1),
    {Output@1, Tape5@1} = case viva_tensor@nn:linear_forward(
        Tape4,
        erlang:element(4, State),
        Hidden_act
    ) of
        {ok, {traced, Output, Tape5}} -> {Output, Tape5};
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"train_step"/utf8>>,
                        line => 71,
                        value => _assert_fail@1,
                        start => 1945,
                        'end' => 2038,
                        pattern_start => 1956,
                        pattern_end => 1981})
    end,
    {Loss_var@1, Tape6@1} = case viva_tensor@nn:mse_loss(
        Tape5@1,
        Output@1,
        Target
    ) of
        {ok, {traced, Loss_var, Tape6}} -> {Loss_var, Tape6};
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"train_step"/utf8>>,
                        line => 75,
                        value => _assert_fail@2,
                        start => 2062,
                        'end' => 2137,
                        pattern_start => 2073,
                        pattern_end => 2100})
    end,
    Grads@1 = case viva_tensor@autograd:backward(Tape6@1, Loss_var@1) of
        {ok, Grads} -> Grads;
        _assert_fail@3 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"train_step"/utf8>>,
                        line => 78,
                        value => _assert_fail@3,
                        start => 2160,
                        'end' => 2217,
                        pattern_start => 2171,
                        pattern_end => 2180})
    end,
    case (Epoch rem 100) =:= 0 of
        true ->
            Loss_val = begin
                _pipe = viva_tensor@tensor:to_list(
                    erlang:element(3, Loss_var@1)
                ),
                _pipe@1 = gleam@list:first(_pipe),
                gleam@result:unwrap(_pipe@1, +0.0)
            end,
            gleam_stdlib:println(
                <<<<<<"Epoch "/utf8, (erlang:integer_to_binary(Epoch))/binary>>/binary,
                        " | Loss: "/utf8>>/binary,
                    (gleam_stdlib:float_to_string(Loss_val))/binary>>
            );

        false ->
            nil
    end,
    Gw1@1 = case gleam_stdlib:map_get(
        Grads@1,
        erlang:element(2, erlang:element(2, erlang:element(3, State)))
    ) of
        {ok, Gw1} -> Gw1;
        _assert_fail@4 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"train_step"/utf8>>,
                        line => 96,
                        value => _assert_fail@4,
                        start => 2587,
                        'end' => 2642,
                        pattern_start => 2598,
                        pattern_end => 2605})
    end,
    Gb1@1 = case gleam_stdlib:map_get(
        Grads@1,
        erlang:element(2, erlang:element(3, erlang:element(3, State)))
    ) of
        {ok, Gb1} -> Gb1;
        _assert_fail@5 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"train_step"/utf8>>,
                        line => 97,
                        value => _assert_fail@5,
                        start => 2645,
                        'end' => 2700,
                        pattern_start => 2656,
                        pattern_end => 2663})
    end,
    New_w1_data@1 = case viva_tensor@tensor:sub(
        erlang:element(3, erlang:element(2, erlang:element(3, State))),
        viva_tensor@tensor:scale(Gw1@1, 0.01)
    ) of
        {ok, New_w1_data} -> New_w1_data;
        _assert_fail@6 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"train_step"/utf8>>,
                        line => 98,
                        value => _assert_fail@6,
                        start => 2703,
                        'end' => 2801,
                        pattern_start => 2714,
                        pattern_end => 2729})
    end,
    New_b1_data@1 = case viva_tensor@tensor:sub(
        erlang:element(3, erlang:element(3, erlang:element(3, State))),
        viva_tensor@tensor:scale(Gb1@1, 0.01)
    ) of
        {ok, New_b1_data} -> New_b1_data;
        _assert_fail@7 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"train_step"/utf8>>,
                        line => 100,
                        value => _assert_fail@7,
                        start => 2804,
                        'end' => 2902,
                        pattern_start => 2815,
                        pattern_end => 2830})
    end,
    Gw2@1 = case gleam_stdlib:map_get(
        Grads@1,
        erlang:element(2, erlang:element(2, erlang:element(4, State)))
    ) of
        {ok, Gw2} -> Gw2;
        _assert_fail@8 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"train_step"/utf8>>,
                        line => 103,
                        value => _assert_fail@8,
                        start => 2906,
                        'end' => 2961,
                        pattern_start => 2917,
                        pattern_end => 2924})
    end,
    Gb2@1 = case gleam_stdlib:map_get(
        Grads@1,
        erlang:element(2, erlang:element(3, erlang:element(4, State)))
    ) of
        {ok, Gb2} -> Gb2;
        _assert_fail@9 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"train_step"/utf8>>,
                        line => 104,
                        value => _assert_fail@9,
                        start => 2964,
                        'end' => 3019,
                        pattern_start => 2975,
                        pattern_end => 2982})
    end,
    New_w2_data@1 = case viva_tensor@tensor:sub(
        erlang:element(3, erlang:element(2, erlang:element(4, State))),
        viva_tensor@tensor:scale(Gw2@1, 0.01)
    ) of
        {ok, New_w2_data} -> New_w2_data;
        _assert_fail@10 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"train_step"/utf8>>,
                        line => 105,
                        value => _assert_fail@10,
                        start => 3022,
                        'end' => 3120,
                        pattern_start => 3033,
                        pattern_end => 3048})
    end,
    New_b2_data@1 = case viva_tensor@tensor:sub(
        erlang:element(3, erlang:element(3, erlang:element(4, State))),
        viva_tensor@tensor:scale(Gb2@1, 0.01)
    ) of
        {ok, New_b2_data} -> New_b2_data;
        _assert_fail@11 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"train_step"/utf8>>,
                        line => 107,
                        value => _assert_fail@11,
                        start => 3123,
                        'end' => 3221,
                        pattern_start => 3134,
                        pattern_end => 3149})
    end,
    Next_tape = viva_tensor@autograd:new_tape(),
    {traced, Nw1, Nt1} = viva_tensor@autograd:new_variable(
        Next_tape,
        New_w1_data@1
    ),
    {traced, Nb1, Nt2} = viva_tensor@autograd:new_variable(Nt1, New_b1_data@1),
    {traced, Nw2, Nt3} = viva_tensor@autograd:new_variable(Nt2, New_w2_data@1),
    {traced, Nb2, Nt4} = viva_tensor@autograd:new_variable(Nt3, New_b2_data@1),
    {training_state, Nt4, {linear, Nw1, Nb1}, {linear, Nw2, Nb2}}.

-file("src/viva_tensor/training_demo.gleam", 26).
-spec main() -> nil.
main() ->
    gleam_stdlib:println(<<"🚀 Starting Mycelial Training Demo..."/utf8>>),
    Tape = viva_tensor@autograd:new_tape(),
    X_data = viva_tensor@tensor:from_list([1.0, 2.0, 3.0, 4.0, 5.0]),
    X_data@2 = case viva_tensor@tensor:reshape(X_data, [5, 1]) of
        {ok, X_data@1} -> X_data@1;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 34,
                        value => _assert_fail,
                        start => 696,
                        'end' => 750,
                        pattern_start => 707,
                        pattern_end => 717})
    end,
    Y_data = viva_tensor@tensor:from_list([2.1, 3.9, 6.2, 8.1, 10.3]),
    Y_data@2 = case viva_tensor@tensor:reshape(Y_data, [5, 1]) of
        {ok, Y_data@1} -> Y_data@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"viva_tensor/training_demo"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 37,
                        value => _assert_fail@1,
                        start => 814,
                        'end' => 868,
                        pattern_start => 825,
                        pattern_end => 835})
    end,
    {traced, _, Tape1} = viva_tensor@autograd:new_variable(Tape, X_data@2),
    {traced, _, Tape2} = viva_tensor@autograd:new_variable(Tape1, Y_data@2),
    {traced, Layer1, Tape3} = viva_tensor@nn:linear(Tape2, 1, 4),
    {traced, Layer2, Tape4} = viva_tensor@nn:linear(Tape3, 4, 1),
    State = {training_state, Tape4, Layer1, Layer2},
    _ = gleam@list:fold(
        gleam@list:range(0, 500 - 1),
        State,
        fun(Acc_state, Epoch) ->
            train_step(Acc_state, Epoch, X_data@2, Y_data@2)
        end
    ),
    gleam_stdlib:println(<<"✅ Training finished!"/utf8>>).
