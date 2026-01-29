-module(viva_tensor@nn).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/nn.gleam").
-export([linear/3, linear_forward/3, relu/2, mse_loss/3]).
-export_type([linear/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type linear() :: {linear,
        viva_tensor@autograd:variable(),
        viva_tensor@autograd:variable()}.

-file("src/viva_tensor/nn.gleam", 12).
?DOC(" Initializes a new Linear layer\n").
-spec linear(viva_tensor@autograd:tape(), integer(), integer()) -> viva_tensor@autograd:traced(linear()).
linear(Tape, In_features, Out_features) ->
    W_data = viva_tensor@tensor:xavier_init(In_features, Out_features),
    B_data = viva_tensor@tensor:zeros([Out_features]),
    {traced, W, Tape1} = viva_tensor@autograd:new_variable(Tape, W_data),
    {traced, B, Tape2} = viva_tensor@autograd:new_variable(Tape1, B_data),
    {traced, {linear, W, B}, Tape2}.

-file("src/viva_tensor/nn.gleam", 24).
?DOC(" Forward pass of the Linear layer\n").
-spec linear_forward(
    viva_tensor@autograd:tape(),
    linear(),
    viva_tensor@autograd:variable()
) -> {ok, viva_tensor@autograd:traced(viva_tensor@autograd:variable())} |
    {error, viva_tensor@tensor:tensor_error()}.
linear_forward(Tape, Layer, X) ->
    gleam@result:'try'(
        viva_tensor@autograd:transpose(Tape, erlang:element(2, Layer)),
        fun(_use0) ->
            {traced, Wt, Tape1} = _use0,
            gleam@result:'try'(
                viva_tensor@autograd:matmul(Tape1, X, Wt),
                fun(_use0@1) ->
                    {traced, Xw, Tape2} = _use0@1,
                    viva_tensor@autograd:add(
                        Tape2,
                        Xw,
                        erlang:element(3, Layer)
                    )
                end
            )
        end
    ).

-file("src/viva_tensor/nn.gleam", 43).
?DOC(" ReLU activation function\n").
-spec relu(viva_tensor@autograd:tape(), viva_tensor@autograd:variable()) -> viva_tensor@autograd:traced(viva_tensor@autograd:variable()).
relu(Tape, X) ->
    viva_tensor@autograd:relu(Tape, X).

-file("src/viva_tensor/nn.gleam", 49).
?DOC(
    " Loss function: Mean Squared Error (MSE)\n"
    " L = mean((pred - target)^2)\n"
).
-spec mse_loss(
    viva_tensor@autograd:tape(),
    viva_tensor@autograd:variable(),
    viva_tensor@autograd:variable()
) -> {ok, viva_tensor@autograd:traced(viva_tensor@autograd:variable())} |
    {error, viva_tensor@tensor:tensor_error()}.
mse_loss(Tape, Pred, Target) ->
    gleam@result:'try'(
        viva_tensor@autograd:sub(Tape, Pred, Target),
        fun(_use0) ->
            {traced, Diff, Tape1} = _use0,
            gleam@result:'try'(
                viva_tensor@autograd:mul(Tape1, Diff, Diff),
                fun(_use0@1) ->
                    {traced, Square, Tape2} = _use0@1,
                    {ok, viva_tensor@autograd:mean(Tape2, Square)}
                end
            )
        end
    ).
