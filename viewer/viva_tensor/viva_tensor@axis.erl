-module(viva_tensor@axis).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/axis.gleam").
-export([axis/2, batch/1, seq/1, feature/1, height/1, width/1, channel/1, input/1, output/1, head/1, embed/1, named/2, equals/2, specs_equal/2, to_string/1]).
-export_type([axis/0, axis_spec/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Axis - Named axis types for semantic tensor dimensions\n"
    "\n"
    " Gives meaning to tensor dimensions: Batch, Seq, Feature, etc.\n"
).

-type axis() :: batch |
    seq |
    feature |
    height |
    width |
    channel |
    input |
    output |
    head |
    embed |
    {named, binary()} |
    anon.

-type axis_spec() :: {axis_spec, axis(), integer()}.

-file("src/viva_tensor/axis.gleam", 47).
?DOC(" Create axis spec\n").
-spec axis(axis(), integer()) -> axis_spec().
axis(Name, Size) ->
    {axis_spec, Name, Size}.

-file("src/viva_tensor/axis.gleam", 52).
?DOC(" Batch dimension\n").
-spec batch(integer()) -> axis_spec().
batch(Size) ->
    {axis_spec, batch, Size}.

-file("src/viva_tensor/axis.gleam", 57).
?DOC(" Sequence dimension\n").
-spec seq(integer()) -> axis_spec().
seq(Size) ->
    {axis_spec, seq, Size}.

-file("src/viva_tensor/axis.gleam", 62).
?DOC(" Feature dimension\n").
-spec feature(integer()) -> axis_spec().
feature(Size) ->
    {axis_spec, feature, Size}.

-file("src/viva_tensor/axis.gleam", 67).
?DOC(" Height dimension\n").
-spec height(integer()) -> axis_spec().
height(Size) ->
    {axis_spec, height, Size}.

-file("src/viva_tensor/axis.gleam", 72).
?DOC(" Width dimension\n").
-spec width(integer()) -> axis_spec().
width(Size) ->
    {axis_spec, width, Size}.

-file("src/viva_tensor/axis.gleam", 77).
?DOC(" Channel dimension\n").
-spec channel(integer()) -> axis_spec().
channel(Size) ->
    {axis_spec, channel, Size}.

-file("src/viva_tensor/axis.gleam", 82).
?DOC(" Input dimension\n").
-spec input(integer()) -> axis_spec().
input(Size) ->
    {axis_spec, input, Size}.

-file("src/viva_tensor/axis.gleam", 87).
?DOC(" Output dimension\n").
-spec output(integer()) -> axis_spec().
output(Size) ->
    {axis_spec, output, Size}.

-file("src/viva_tensor/axis.gleam", 92).
?DOC(" Head dimension\n").
-spec head(integer()) -> axis_spec().
head(Size) ->
    {axis_spec, head, Size}.

-file("src/viva_tensor/axis.gleam", 97).
?DOC(" Embed dimension\n").
-spec embed(integer()) -> axis_spec().
embed(Size) ->
    {axis_spec, embed, Size}.

-file("src/viva_tensor/axis.gleam", 102).
?DOC(" Custom named dimension\n").
-spec named(binary(), integer()) -> axis_spec().
named(Name, Size) ->
    {axis_spec, {named, Name}, Size}.

-file("src/viva_tensor/axis.gleam", 111).
?DOC(" Check if two axes are equal\n").
-spec equals(axis(), axis()) -> boolean().
equals(A, B) ->
    case {A, B} of
        {anon, anon} ->
            true;

        {batch, batch} ->
            true;

        {seq, seq} ->
            true;

        {feature, feature} ->
            true;

        {height, height} ->
            true;

        {width, width} ->
            true;

        {channel, channel} ->
            true;

        {input, input} ->
            true;

        {output, output} ->
            true;

        {head, head} ->
            true;

        {embed, embed} ->
            true;

        {{named, S1}, {named, S2}} ->
            S1 =:= S2;

        {_, _} ->
            false
    end.

-file("src/viva_tensor/axis.gleam", 130).
?DOC(" Check if two axis spec lists are equal\n").
-spec specs_equal(list(axis_spec()), list(axis_spec())) -> boolean().
specs_equal(A, B) ->
    case {A, B} of
        {[], []} ->
            true;

        {[A1 | A_rest], [B1 | B_rest]} ->
            (equals(erlang:element(2, A1), erlang:element(2, B1)) andalso (erlang:element(
                3,
                A1
            )
            =:= erlang:element(3, B1)))
            andalso specs_equal(A_rest, B_rest);

        {_, _} ->
            false
    end.

-file("src/viva_tensor/axis.gleam", 142).
?DOC(" Get human-readable axis name\n").
-spec to_string(axis()) -> binary().
to_string(A) ->
    case A of
        batch ->
            <<"batch"/utf8>>;

        seq ->
            <<"seq"/utf8>>;

        feature ->
            <<"feature"/utf8>>;

        height ->
            <<"height"/utf8>>;

        width ->
            <<"width"/utf8>>;

        channel ->
            <<"channel"/utf8>>;

        input ->
            <<"input"/utf8>>;

        output ->
            <<"output"/utf8>>;

        head ->
            <<"head"/utf8>>;

        embed ->
            <<"embed"/utf8>>;

        {named, S} ->
            S;

        anon ->
            <<"_"/utf8>>
    end.
