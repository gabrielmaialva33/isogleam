-module(ansel@bounding_box).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([ltwh/4, unchecked_ltwh/4, ltrb/4, x1y1x2y2/4, to_ltwh_tuple/1, to_ltrb_tuple/1, to_x1y1x2y2_tuple/1, shrink/2, expand/2, scale/2, cut/2, intersection/2, fit/2, make_relative/2]).
-export_type([bounding_box/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " A module for working with bounding boxes in Ansel. Bounding boxes are a \n"
    " common way to represent rectangular areas in images, and can be used to \n"
    " crop images, fill in images, and highlight areas of images.\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(box) = bounding_box.x1y1x2y2(2, 2, 4, 4)\n"
    " \n"
    " bounding_box.expand(box, by: 2)\n"
    " |> image.extract_area(image, at: _)\n"
    " ```\n"
).

-opaque bounding_box() :: {ltwh, integer(), integer(), integer(), integer()} |
    {ltrb, integer(), integer(), integer(), integer()} |
    {x1y1x2y2, integer(), integer(), integer(), integer()}.

-file("src/ansel/bounding_box.gleam", 27).
?DOC(
    " Creates a new bounding box from the given values in the (Left, Top), (Width, \n"
    " Height) rectangular coordinate format.\n"
).
-spec ltwh(integer(), integer(), integer(), integer()) -> {ok, bounding_box()} |
    {error, snag:snag()}.
ltwh(Left, Top, Width, Height) ->
    case (((Width > 0) andalso (Height > 0)) andalso (Left >= 0)) andalso (Top
    >= 0) of
        true ->
            {ok, {ltwh, Left, Top, Width, Height}};

        false ->
            snag:error(<<"Impossible ltwh bounding box values passed"/utf8>>)
    end.

-file("src/ansel/bounding_box.gleam", 40).
?DOC(false).
-spec unchecked_ltwh(integer(), integer(), integer(), integer()) -> bounding_box().
unchecked_ltwh(Left, Top, Width, Height) ->
    {ltwh, Left, Top, Width, Height}.

-file("src/ansel/bounding_box.gleam", 51).
?DOC(
    " Creates a new bounding box from the given values in the (Left, Top), (Right, \n"
    " Bottom) rectangular coordinate format.\n"
).
-spec ltrb(integer(), integer(), integer(), integer()) -> {ok, bounding_box()} |
    {error, snag:snag()}.
ltrb(Left, Top, Right, Bottom) ->
    case (((Left < Right) andalso (Top < Bottom)) andalso (Left >= 0)) andalso (Top
    >= 0) of
        true ->
            {ok, {ltrb, Left, Top, Right, Bottom}};

        false ->
            snag:error(<<"Impossible ltrb bounding box values passed"/utf8>>)
    end.

-file("src/ansel/bounding_box.gleam", 65).
?DOC(
    " Creates a new bounding box from the given values in the (x1, y1), (x2, y2) \n"
    " rectangular coordinate format.\n"
).
-spec x1y1x2y2(integer(), integer(), integer(), integer()) -> {ok,
        bounding_box()} |
    {error, snag:snag()}.
x1y1x2y2(X1, Y1, X2, Y2) ->
    case (((X1 < X2) andalso (Y1 < Y2)) andalso (X1 >= 0)) andalso (Y1 >= 0) of
        true ->
            {ok, {x1y1x2y2, X1, Y1, X2, Y2}};

        false ->
            snag:error(
                <<"Impossible x1y1x2y2 bounding box values passed"/utf8>>
            )
    end.

-file("src/ansel/bounding_box.gleam", 93).
?DOC(
    " Converts a bounding box to a tuple with the coordinate values left, top, \n"
    " width, height. Useful for working with with custom bounding box operations\n"
    " and getting the width and height of a bounding box.\n"
    " \n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(box) = bounding_box.x1y1x2y2(x1: 2, y1: 2, x2: 6, y2: 6) \n"
    " bounding_box.to_ltwh_tuple(box)\n"
    " // -> #(2, 2, 4, 4)\n"
    " ```\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(box) = bounding_box.x1y1x2y2(x1: 4, y1: 4, x2: 10, y2: 10) \n"
    " let #(_, _, width, height) = bounding_box.to_ltwh_tuple(box)\n"
    " // -> 6, 6\n"
    " ```\n"
).
-spec to_ltwh_tuple(bounding_box()) -> {integer(),
    integer(),
    integer(),
    integer()}.
to_ltwh_tuple(Bounding_box) ->
    case Bounding_box of
        {ltwh, Left, Top, Width, Height} ->
            {Left, Top, Width, Height};

        {ltrb, Left@1, Top@1, Right, Bottom} ->
            {Left@1, Top@1, Right - Left@1, Bottom - Top@1};

        {x1y1x2y2, X1, Y1, X2, Y2} ->
            {X1, Y1, X2 - X1, Y2 - Y1}
    end.

-file("src/ansel/bounding_box.gleam", 110).
?DOC(
    " Converts a bounding box to a tuple with the coordinate values left, top, \n"
    " right, bottom. Useful for working with with custom bounding box operations.\n"
    " \n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(box) = bounding_box.x1y1x2y2(x1: 2, y1: 2, x2: 6, y2: 6) \n"
    " bounding_box.to_ltrb_tuple(box)\n"
    " // -> #(2, 2, 6, 6)\n"
    " ```\n"
).
-spec to_ltrb_tuple(bounding_box()) -> {integer(),
    integer(),
    integer(),
    integer()}.
to_ltrb_tuple(Bounding_box) ->
    case Bounding_box of
        {ltwh, Left, Top, Width, Height} ->
            {Left, Top, Left + Width, Top + Height};

        {ltrb, Left@1, Top@1, Right, Bottom} ->
            {Left@1, Top@1, Right, Bottom};

        {x1y1x2y2, X1, Y1, X2, Y2} ->
            {X1, Y1, X2, Y2}
    end.

-file("src/ansel/bounding_box.gleam", 127).
?DOC(
    " Converts a bounding box to a tuple with the coordinate values x1, y1, x2, \n"
    " y2. Useful for working with with custom bounding box operations.\n"
    " \n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(box) = bounding_box.ltwh(2, 2, 4, 4) \n"
    " bounding_box.to_x1y1x2y2_tuple(box)\n"
    " // -> #(2, 2, 6, 6)\n"
    " ```\n"
).
-spec to_x1y1x2y2_tuple(bounding_box()) -> {integer(),
    integer(),
    integer(),
    integer()}.
to_x1y1x2y2_tuple(Bounding_box) ->
    case Bounding_box of
        {ltwh, Left, Top, Width, Height} ->
            {Left, Top, Left + Width, Top + Height};

        {ltrb, Left@1, Top@1, Right, Bottom} ->
            {Left@1, Top@1, Right, Bottom};

        {x1y1x2y2, X1, Y1, X2, Y2} ->
            {X1, Y1, X2, Y2}
    end.

-file("src/ansel/bounding_box.gleam", 138).
?DOC(
    " Shrinks a bounding box by the given amount in all dimensions. If the amount \n"
    " is negative, the bounding box will not be modified. If the amount to shrink \n"
    " is greater than the size of the bounding box, an error will be returned.\n"
).
-spec shrink(bounding_box(), integer()) -> {ok, bounding_box()} | {error, nil}.
shrink(Bounding_box, Amount) ->
    gleam@bool:guard(
        Amount < 0,
        {ok, Bounding_box},
        fun() ->
            {_, _, Width, Height} = to_ltwh_tuple(Bounding_box),
            gleam@bool:guard(
                ((Amount * 2) >= Width) orelse ((Amount * 2) >= Height),
                {error, nil},
                fun() -> _pipe = case Bounding_box of
                        {ltwh, Left, Top, Width@1, Height@1} ->
                            {ltwh,
                                Left + Amount,
                                Top + Amount,
                                gleam@int:max(Width@1 - (Amount * 2), 0),
                                gleam@int:max(Height@1 - (Amount * 2), 0)};

                        {ltrb, Left@1, Top@1, Right, Bottom} ->
                            {ltrb,
                                Left@1 + Amount,
                                Top@1 + Amount,
                                gleam@int:max(Right - Amount, 0),
                                gleam@int:max(Bottom - Amount, 0)};

                        {x1y1x2y2, X1, Y1, X2, Y2} ->
                            {x1y1x2y2,
                                X1 + Amount,
                                Y1 + Amount,
                                gleam@int:max(X2 - Amount, 0),
                                gleam@int:max(Y2 - Amount, 0)}
                    end,
                    {ok, _pipe} end
            )
        end
    ).

-file("src/ansel/bounding_box.gleam", 179).
?DOC(
    " Expands a bounding box by the given amount in all dimensions. If the amount \n"
    " is negative, the bounding box will not be modified.\n"
).
-spec expand(bounding_box(), integer()) -> bounding_box().
expand(Bounding_box, Amount) ->
    gleam@bool:guard(Amount < 0, Bounding_box, fun() -> case Bounding_box of
                {ltwh, Left, Top, Width, Height} ->
                    {ltwh,
                        gleam@int:max(Left - Amount, 0),
                        gleam@int:max(Top - Amount, 0),
                        Width + (Amount * 2),
                        Height + (Amount * 2)};

                {ltrb, Left@1, Top@1, Right, Bottom} ->
                    {ltrb,
                        gleam@int:max(Left@1 - Amount, 0),
                        gleam@int:max(Top@1 - Amount, 0),
                        Right + Amount,
                        Bottom + Amount};

                {x1y1x2y2, X1, Y1, X2, Y2} ->
                    {x1y1x2y2,
                        gleam@int:max(X1 - Amount, 0),
                        gleam@int:max(Y1 - Amount, 0),
                        X2 + Amount,
                        Y2 + Amount}
            end end).

-file("src/ansel/bounding_box.gleam", 208).
?DOC(" Resizes a bounding box by the given scale.\n").
-spec scale(bounding_box(), float()) -> bounding_box().
scale(Bounding_box, Scale) ->
    {Left, Top, Right, Bottom} = to_ltrb_tuple(Bounding_box),
    {ltrb,
        gleam@float:round(gleam@int:to_float(Left) * Scale),
        gleam@float:round(gleam@int:to_float(Top) * Scale),
        gleam@float:round(gleam@int:to_float(Right) * Scale),
        gleam@float:round(gleam@int:to_float(Bottom) * Scale)}.

-file("src/ansel/bounding_box.gleam", 221).
?DOC(
    " Cuts a bounding box out of another bounding box, returning a list of \n"
    " bounding boxes that represent the area of the original that was not cut out.\n"
).
-spec cut(bounding_box(), bounding_box()) -> list(bounding_box()).
cut(To_cut, Cutter) ->
    {X1, Y1, W1, H1} = to_ltwh_tuple(To_cut),
    {X2, Y2, W2, H2} = to_ltwh_tuple(Cutter),
    Int_left = gleam@int:max(X1, X2),
    Int_top = gleam@int:max(Y1, Y2),
    Int_right = gleam@int:min(X1 + W1, X2 + W2),
    Int_bottom = gleam@int:min(Y1 + H1, Y2 + H2),
    gleam@bool:guard(
        (Int_left >= Int_right) orelse (Int_top >= Int_bottom),
        [To_cut],
        fun() ->
            Cut_pieces = [case Y1 < Int_top of
                    true ->
                        {some, {ltwh, X1, Y1, W1, Int_top - Y1}};

                    false ->
                        none
                end, case X1 < Int_left of
                    true ->
                        {some,
                            {ltwh,
                                X1,
                                Int_top,
                                Int_left - X1,
                                Int_bottom - Int_top}};

                    false ->
                        none
                end, case Int_right < (X1 + W1) of
                    true ->
                        {some,
                            {ltwh,
                                Int_right,
                                Int_top,
                                (X1 + W1) - Int_right,
                                Int_bottom - Int_top}};

                    false ->
                        none
                end, case Int_bottom < (Y1 + H1) of
                    true ->
                        {some,
                            {ltwh, X1, Int_bottom, W1, (Y1 + H1) - Int_bottom}};

                    false ->
                        none
                end],
            gleam@option:values(Cut_pieces)
        end
    ).

-file("src/ansel/bounding_box.gleam", 284).
?DOC(
    " Returns the intersection of two bounding boxes. If they do not intersect,\n"
    " `None` will be returned.\n"
).
-spec intersection(bounding_box(), bounding_box()) -> gleam@option:option(bounding_box()).
intersection(Box1, Box2) ->
    {L1, T1, R1, B1} = to_ltrb_tuple(Box1),
    {L2, T2, R2, B2} = to_ltrb_tuple(Box2),
    gleam@bool:guard(
        (((L1 >= L2) andalso (T1 >= T2)) andalso (R1 =< R2)) andalso (B1 =< B2),
        {some, Box1},
        fun() ->
            gleam@bool:guard(
                (((L1 =< L2) andalso (T1 =< T2)) andalso (R1 >= R2)) andalso (B1
                >= B2),
                {some, Box2},
                fun() ->
                    Left = gleam@int:max(L1, L2),
                    Top = gleam@int:max(T1, T2),
                    Right = gleam@int:min(R1, R2),
                    Bottom = gleam@int:min(B1, B2),
                    gleam@bool:guard(
                        (Left >= Right) orelse (Top >= Bottom),
                        none,
                        fun() -> _pipe = {ltrb, Left, Top, Right, Bottom},
                            {some, _pipe} end
                    )
                end
            )
        end
    ).

-file("src/ansel/bounding_box.gleam", 314).
?DOC(
    " Fits a bounding box into another bounding box, dropping any pixels outside \n"
    " the dimensions of the reference bounding box.\n"
).
-spec fit(bounding_box(), bounding_box()) -> gleam@option:option(bounding_box()).
fit(Box, Reference) ->
    {_, _, Width, Height} = to_ltwh_tuple(Reference),
    {Left, Top, Right, Bottom} = to_ltrb_tuple(Box),
    case {Left < Width, Top < Height} of
        {true, true} ->
            {some,
                {ltrb,
                    Left,
                    Top,
                    gleam@int:min(Right, Width),
                    gleam@int:min(Bottom, Height)}};

        {_, _} ->
            none
    end.

-file("src/ansel/bounding_box.gleam", 355).
?DOC(
    " Makes a bounding box relative to and fit inside another bounding box. \n"
    " Assuming both bounding boxes are on the same image, they are both relative\n"
    " to 0,0 on that image. This adjusts the first bounding box so that the \n"
    " original coordinates are relative to the top left corner of the second \n"
    " bounding box instead, and then fits the adjusted bounding box into the \n"
    " reference bounding box.\n"
    " \n"
    " This is useful when you have two bounding boxes on an image, where one\n"
    " represents an extracted area of the original image and you want to do\n"
    " an operation on that extracted area with the second bounding box, but the \n"
    " second bounding box was calculated with the coordinates of the original \n"
    " image.\n"
    " \n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(box) = bounding_box.ltwh(left: 2, top: 2, width: 4, height: 4) \n"
    " let assert Ok(ref) = bounding_box.ltwh(left: 4, top: 4, width: 6, height: 6) \n"
    " bounding_box.make_relative(box, to: ref)\n"
    " // -> Some(bounding_box.ltwh(left: 0, top: 0, width: 2, height: 2))\n"
    " ```\n"
).
-spec make_relative(bounding_box(), bounding_box()) -> gleam@option:option(bounding_box()).
make_relative(Box, Reference) ->
    {Left, Top, Right, Bottom} = to_ltrb_tuple(Box),
    {Ref_left, Ref_top, _, _} = to_ltwh_tuple(Reference),
    Adj_box = {ltrb,
        gleam@int:max(Left - Ref_left, 0),
        gleam@int:max(Top - Ref_top, 0),
        gleam@int:max(Right - Ref_left, 0),
        gleam@int:max(Bottom - Ref_top, 0)},
    case Adj_box of
        {ltrb, 0, 0, 0, 0} ->
            none;

        _ ->
            fit(Adj_box, Reference)
    end.
