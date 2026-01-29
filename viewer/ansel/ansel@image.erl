-module(ansel@image).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([from_bit_array/1, to_bit_array/2, to_pixel_matrix/1, from_pixel_matrix/1, new/3, extract_area/2, composite_over/4, fill/3, outline/4, blur/2, rotate/2, round/2, get_width/1, get_height/1, fit_bounding_box/2, border/3, to_bounding_box/1, scale/2, scale_width/2, scale_height/2, write/3, read/1, create_thumbnail/2]).
-export_type([image_format/0, format_components/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " This module is primarily a wrapper around the Elixir package Vix, which\n"
    " is a wrapper around the great image processing library vips. A pre-built\n"
    " vips binary comes with Vix, but see the readme for more information on\n"
    " how to bring your own to support more image formats.\n"
    " \n"
    " This module uses the [snag package](https://hexdocs.pm/snag/index.html) for \n"
    " error handling because vix errors just come back as strings and are not \n"
    " enumerated. Make sure to install it as well to work with the error messages.\n"
    " \n"
    " ```gleam\n"
    " import ansel\n"
    " import ansel/image\n"
    " import gleam/result\n"
    " import snag\n"
    "\n"
    " pub fn main() {\n"
    "   let assert Ok(img) = image.read(\"input.jpeg\")\n"
    "\n"
    "   image.scale(img, by: 2.0)\n"
    "   |> result.try(\n"
    "     image.write(_, \"output\", ansel.JPEG(quality: 60, keep_metadata: False)),\n"
    "   )\n"
    "   |> snag.context(\"Unable to process my cool image\")\n"
    " }\n"
    " // -> output.jpeg written to disk as a smaller, rounded, bordered version of the\n"
    " //    original image\n"
    " ```\n"
).

-type image_format() :: {jpeg, integer(), boolean()} |
    {jpeg2000, integer(), boolean()} |
    {jpegxl, integer(), boolean()} |
    png |
    {web_p, integer(), boolean()} |
    {avif, integer(), boolean()} |
    {tiff, integer(), boolean()} |
    {heic, integer(), boolean()} |
    fits |
    matlab |
    pdf |
    svg |
    hdr |
    ppm |
    csv |
    gif |
    analyze |
    n_if_ti |
    deep_zoom |
    {custom, binary(), binary()}.

-type format_components() :: {format_components, binary(), binary()}.

-file("src/ansel/image.gleam", 110).
-spec format_common_options(integer(), boolean()) -> binary().
format_common_options(Quality, Keep_metadata) ->
    <<<<<<<<"[Q="/utf8, (gleam@int:to_string(Quality))/binary>>/binary,
                ",strip="/utf8>>/binary,
            (begin
                _pipe = gleam@bool:to_string(not Keep_metadata),
                gleam@string:lowercase(_pipe)
            end)/binary>>/binary,
        "]"/utf8>>.

-file("src/ansel/image.gleam", 77).
-spec image_format_to_string(image_format()) -> format_components().
image_format_to_string(Format) ->
    case Format of
        {jpeg, Quality, Keep_metadata} ->
            {format_components,
                <<".jpeg"/utf8>>,
                format_common_options(Quality, Keep_metadata)};

        {jpeg2000, Quality@1, Keep_metadata@1} ->
            {format_components,
                <<".jp2"/utf8>>,
                format_common_options(Quality@1, Keep_metadata@1)};

        {jpegxl, Quality@2, Keep_metadata@2} ->
            {format_components,
                <<".jxl"/utf8>>,
                format_common_options(Quality@2, Keep_metadata@2)};

        png ->
            {format_components, <<".png"/utf8>>, <<""/utf8>>};

        {web_p, Quality@3, Keep_metadata@3} ->
            {format_components,
                <<".webp"/utf8>>,
                format_common_options(Quality@3, Keep_metadata@3)};

        {avif, Quality@4, Keep_metadata@4} ->
            {format_components,
                <<".avif"/utf8>>,
                format_common_options(Quality@4, Keep_metadata@4)};

        {tiff, Quality@5, Keep_metadata@5} ->
            {format_components,
                <<".tiff"/utf8>>,
                format_common_options(Quality@5, Keep_metadata@5)};

        {heic, Quality@6, Keep_metadata@6} ->
            {format_components,
                <<".heic"/utf8>>,
                format_common_options(Quality@6, Keep_metadata@6)};

        fits ->
            {format_components, <<".fits"/utf8>>, <<""/utf8>>};

        matlab ->
            {format_components, <<".mat"/utf8>>, <<""/utf8>>};

        pdf ->
            {format_components, <<".pdf"/utf8>>, <<""/utf8>>};

        svg ->
            {format_components, <<".svg"/utf8>>, <<""/utf8>>};

        hdr ->
            {format_components, <<".hdr"/utf8>>, <<""/utf8>>};

        ppm ->
            {format_components, <<".ppm"/utf8>>, <<""/utf8>>};

        csv ->
            {format_components, <<".csv"/utf8>>, <<""/utf8>>};

        gif ->
            {format_components, <<".gif"/utf8>>, <<""/utf8>>};

        analyze ->
            {format_components, <<".analyze"/utf8>>, <<""/utf8>>};

        n_if_ti ->
            {format_components, <<".nii"/utf8>>, <<""/utf8>>};

        deep_zoom ->
            {format_components, <<".dzi"/utf8>>, <<""/utf8>>};

        {custom, Extension, Format@1} ->
            {format_components,
                Extension,
                <<<<"["/utf8, Format@1/binary>>/binary, "]"/utf8>>}
    end.

-file("src/ansel/image.gleam", 160).
?DOC(
    " Reads a vips image from a bit array\n"
    " \n"
    " ## Example\n"
    " ```gleam\n"
    " simplifile.read_bits(\"input.jpeg\")\n"
    " |> result.try(image.from_bit_array)\n"
    " // -> Ok(ansel.Image)\n"
    " ```\n"
).
-spec from_bit_array(bitstring()) -> {ok, ansel:image()} | {error, snag:snag()}.
from_bit_array(Bin) ->
    _pipe = 'Elixir.Vix.Vips.Image':new_from_buffer(Bin),
    _pipe@1 = gleam@result:map_error(_pipe, fun snag:new/1),
    snag:context(_pipe@1, <<"Unable to read image from bit array"/utf8>>).

-file("src/ansel/image.gleam", 178).
?DOC(
    " Saves a vips image to a bit array. Assumes your vips was built with the\n"
    " correct encoder support for the format to save in.\n"
    " \n"
    " ## Example\n"
    " ```gleam\n"
    " image.new(6, 6, color.GleamLucy)\n"
    " |> result.map(to_bit_array(_, ansel.PNG))\n"
    " |> result.try(simplifile.write_bits(\"output.png\"))\n"
    " ```\n"
).
-spec to_bit_array(ansel:image(), image_format()) -> bitstring().
to_bit_array(Img, Format) ->
    'Elixir.Ansel':to_bit_array(Img, image_format_to_string(Format)).

-file("src/ansel/image.gleam", 196).
?DOC(
    " Converts a vips image into a matrix of pixel values.\n"
    "\n"
    " ## Example\n"
    " ```gleam \n"
    " image.new(6, 6, GleamLucy)\n"
    " |> image.to_pixel_matrix\n"
    " // -> Ok([[RGB(255, 175, 243), ...],\n"
    "           [RGB(255, 175, 243), ...],\n"
    "           ...\n"
    "           [RGB(255, 175, 243), ...]])\n"
    " ```\n"
).
-spec to_pixel_matrix(ansel:image()) -> {ok, list(list(ansel@color:color()))} |
    {error, snag:snag()}.
to_pixel_matrix(Img) ->
    _pipe = Img,
    _pipe@1 = 'Elixir.Ansel':to_rgb_list(_pipe),
    _pipe@4 = gleam@result:map(_pipe@1, fun(Pixels) -> _pipe@2 = Pixels,
            gleam@list:map(_pipe@2, fun(Pixel_row) -> _pipe@3 = Pixel_row,
                    gleam@list:map(
                        _pipe@3,
                        fun(Pixel) ->
                            [R, G, B] = case Pixel of
                                [_, _, _] -> Pixel;
                                _assert_fail ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                value => _assert_fail,
                                                module => <<"ansel/image"/utf8>>,
                                                function => <<"to_pixel_matrix"/utf8>>,
                                                line => 206})
                            end,
                            {rgb, R, G, B}
                        end
                    ) end) end),
    _pipe@5 = gleam@result:map_error(_pipe@4, fun snag:new/1),
    snag:context(_pipe@5, <<"Unable to read image into pixel list"/utf8>>).

-file("src/ansel/image.gleam", 227).
?DOC(
    " Converts a matrix of pixel values into a vips image.\n"
    "\n"
    " ## Example\n"
    " ```gleam \n"
    " list.repeat(list.repeat(RGB(255, 175, 243), 6), 6)\n"
    " |> image.from_pixel_matrix\n"
    " // -> Ok(ansel.Image) \n"
    " // Same as image.new(6, 6, GleamLucy)\n"
    " ```\n"
).
-spec from_pixel_matrix(list(list(ansel@color:color()))) -> {ok, ansel:image()} |
    {error, snag:snag()}.
from_pixel_matrix(Pixels) ->
    Height = erlang:length(Pixels),
    Width = begin
        _pipe = Pixels,
        _pipe@1 = gleam@list:first(_pipe),
        _pipe@2 = gleam@result:unwrap(_pipe@1, []),
        erlang:length(_pipe@2)
    end,
    _pipe@3 = Pixels,
    _pipe@4 = gleam@list:flatten(_pipe@3),
    _pipe@5 = gleam@list:map(
        _pipe@4,
        fun(Rgb) ->
            {rgb, R, G, B} = case Rgb of
                {rgb, _, _, _} -> Rgb;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"ansel/image"/utf8>>,
                                function => <<"from_pixel_matrix"/utf8>>,
                                line => 235})
            end,
            <<R, G, B>>
        end
    ),
    _pipe@6 = gleam_stdlib:bit_array_concat(_pipe@5),
    _pipe@7 = 'Elixir.Ansel':from_binary(_pipe@6, Height, Width),
    _pipe@8 = gleam@result:map_error(_pipe@7, fun snag:new/1),
    snag:context(
        _pipe@8,
        <<"Unable to convert rgb list into ansel image"/utf8>>
    ).

-file("src/ansel/image.gleam", 258).
?DOC(
    " Creates a new image with the specified width, height, and color\n"
    " \n"
    " ## Example\n"
    " ```gleam\n"
    " image.new(6, 6, color.Olive)\n"
    " // -> Ok(ansel.Image)\n"
    " ```\n"
).
-spec new(integer(), integer(), ansel@color:color()) -> {ok, ansel:image()} |
    {error, snag:snag()}.
new(Width, Height, Color) ->
    _pipe = 'Elixir.Ansel':new_image(Width, Height, ansel@color:to_bands(Color)),
    _pipe@1 = gleam@result:map_error(_pipe, fun snag:new/1),
    snag:context(_pipe@1, <<"Unable to create new image"/utf8>>).

-file("src/ansel/image.gleam", 277).
?DOC(
    " Extracts an area out of an image, resulting in a new image of the \n"
    " extracted area.\n"
).
-spec extract_area(ansel:image(), ansel@bounding_box:bounding_box()) -> {ok,
        ansel:image()} |
    {error, snag:snag()}.
extract_area(Image, Bounding_box) ->
    {Left, Top, Width, Height} = ansel@bounding_box:to_ltwh_tuple(Bounding_box),
    _pipe = 'Elixir.Vix.Vips.Operation':extract_area(
        Image,
        Left,
        Top,
        Width,
        Height
    ),
    _pipe@1 = gleam@result:map_error(_pipe, fun snag:new/1),
    snag:context(_pipe@1, <<"Unable to extract area from image"/utf8>>).

-file("src/ansel/image.gleam", 299).
?DOC(
    " Places an image over another image, with the top left corner of the\n"
    " overlay image placed at the specified coordinates.\n"
).
-spec composite_over(ansel:image(), ansel:image(), integer(), integer()) -> {ok,
        ansel:image()} |
    {error, snag:snag()}.
composite_over(Base, Overlay, L, T) ->
    _pipe = 'Elixir.Ansel':composite_over(Base, Overlay, L, T),
    _pipe@1 = gleam@result:map_error(_pipe, fun snag:new/1),
    snag:context(
        _pipe@1,
        <<"Unable to composite overlay image over base image"/utf8>>
    ).

-file("src/ansel/image.gleam", 319).
?DOC(" Fills in an area of the passed image with a solid color.\n").
-spec fill(
    ansel:image(),
    ansel@bounding_box:bounding_box(),
    ansel@color:color()
) -> {ok, ansel:image()} | {error, snag:snag()}.
fill(Image, Bounding_box, Color) ->
    {Left, Top, Width, Height} = ansel@bounding_box:to_ltwh_tuple(Bounding_box),
    _pipe = new(Width, Height, Color),
    gleam@result:'try'(
        _pipe,
        fun(_capture) -> composite_over(Image, _capture, Left, Top) end
    ).

-file("src/ansel/image.gleam", 332).
?DOC(
    " Outlines an area in the passed image with a solid color. All\n"
    " outline pixels are written inside the bounding box area.\n"
).
-spec outline(
    ansel:image(),
    ansel@bounding_box:bounding_box(),
    ansel@color:color(),
    integer()
) -> {ok, ansel:image()} | {error, snag:snag()}.
outline(Image, Bounding_box, Color, Thickness) ->
    {Left, Top, Width, Height} = ansel@bounding_box:to_ltwh_tuple(Bounding_box),
    Original_bb = begin
        _pipe = Bounding_box,
        ansel@bounding_box:shrink(_pipe, Thickness)
    end,
    gleam@result:'try'(
        new(Width, Height, Color),
        fun(Outline) ->
            gleam@result:'try'(
                composite_over(Image, Outline, Left, Top),
                fun(Filled) -> case Original_bb of
                        {ok, Original_bb@1} ->
                            {Original_left, Original_top, _, _} = ansel@bounding_box:to_ltwh_tuple(
                                Original_bb@1
                            ),
                            gleam@result:'try'(
                                extract_area(Image, Original_bb@1),
                                fun(Original_area) ->
                                    composite_over(
                                        Filled,
                                        Original_area,
                                        Original_left,
                                        Original_top
                                    )
                                end
                            );

                        {error, nil} ->
                            {ok, Filled}
                    end end
            )
        end
    ).

-file("src/ansel/image.gleam", 393).
?DOC(" Applies a gaussian blur with the given sigma value to an image.\n").
-spec blur(ansel:image(), float()) -> {ok, ansel:image()} | {error, snag:snag()}.
blur(Image, Sigma) ->
    _pipe = 'Elixir.Vix.Vips.Operation':gaussblur(Image, Sigma),
    _pipe@1 = gleam@result:map_error(_pipe, fun snag:new/1),
    snag:context(_pipe@1, <<"Unable to blur image"/utf8>>).

-file("src/ansel/image.gleam", 406).
?DOC(" Rotates an image by the given number of degrees.\n").
-spec rotate(ansel:image(), float()) -> {ok, ansel:image()} |
    {error, snag:snag()}.
rotate(Image, Degrees) ->
    _pipe = case Degrees of
        +0.0 ->
            {ok, Image};

        90.0 ->
            'Elixir.Ansel':rotate90(Image);

        180.0 ->
            'Elixir.Ansel':rotate180(Image);

        270.0 ->
            'Elixir.Ansel':rotate270(Image);

        _ ->
            'Elixir.Vix.Vips.Operation':rotate(Image, Degrees)
    end,
    _pipe@1 = gleam@result:map_error(_pipe, fun snag:new/1),
    snag:context(_pipe@1, <<"Unable to rotate image"/utf8>>).

-file("src/ansel/image.gleam", 440).
?DOC(
    " Rounds the corners of an image by a given radius, leaving transparent \n"
    " pixels where the rounding was applied. A large radius can be given to \n"
    " create circular images. This can be used with the border function to \n"
    " create rounded borders around images. This implmentation is heavily \n"
    " inspired by the extensive elixir library Image.\n"
).
-spec round(ansel:image(), float()) -> {ok, ansel:image()} |
    {error, snag:snag()}.
round(Image, Radius) ->
    _pipe = 'Elixir.Ansel':round(Image, Radius),
    _pipe@1 = gleam@result:map_error(_pipe, fun snag:new/1),
    snag:context(_pipe@1, <<"Unable to round image"/utf8>>).

-file("src/ansel/image.gleam", 454).
?DOC(" Returns the width of an image.\n").
-spec get_width(ansel:image()) -> integer().
get_width(Image) ->
    'Elixir.Vix.Vips.Image':width(Image).

-file("src/ansel/image.gleam", 458).
?DOC(" Returns the height of an image.\n").
-spec get_height(ansel:image()) -> integer().
get_height(Image) ->
    'Elixir.Vix.Vips.Image':height(Image).

-file("src/ansel/image.gleam", 129).
?DOC(
    " Fits a fixed bounding box to an image by dropping any pixels outside the\n"
    " dimensions of the image.\n"
    " \n"
    " ## Example\n"
    " \n"
    " ```gleam\n"
    " let assert Ok(bb) = bounding_box.ltwh(4, 2, 40, 30)\n"
    " let assert Ok(img) = image.new(6, 7, color.GleamLucy)\n"
    " image.fit_bounding_box(bb, in: img)\n"
    " // -> bounding_box.ltwh(left: 4, top: 2, width: 2, height: 5)\n"
    " ```\n"
).
-spec fit_bounding_box(ansel@bounding_box:bounding_box(), ansel:image()) -> {ok,
        ansel@bounding_box:bounding_box()} |
    {error, snag:snag()}.
fit_bounding_box(Bounding_box, Image) ->
    Width = 'Elixir.Vix.Vips.Image':width(Image),
    Height = 'Elixir.Vix.Vips.Image':height(Image),
    {Left, Top, Right, Bottom} = ansel@bounding_box:to_ltrb_tuple(Bounding_box),
    case {Left < Width, Top < Height} of
        {true, true} ->
            ansel@bounding_box:ltrb(
                Left,
                Top,
                gleam@int:min(Right, Width),
                gleam@int:min(Bottom, Height)
            );

        {_, _} ->
            snag:error(
                <<"Passed bounding box is completely outside the image to fit"/utf8>>
            )
    end.

-file("src/ansel/image.gleam", 374).
?DOC(
    " Add a solid border around the passed image, expanding the \n"
    " dimensions of the image by the border thickness. Replaces any transparent\n"
    " pixels with the color of the border. This can be used with the round\n"
    " function to add a rounded border to an image.\n"
).
-spec border(ansel:image(), ansel@color:color(), integer()) -> {ok,
        ansel:image()} |
    {error, snag:snag()}.
border(Image, Color, Thickness) ->
    Height = 'Elixir.Vix.Vips.Image':height(Image),
    Width = 'Elixir.Vix.Vips.Image':width(Image),
    gleam@result:'try'(
        begin
            _pipe = ansel@bounding_box:ltwh(0, 0, Width, Height),
            _pipe@1 = gleam@result:map(
                _pipe,
                fun(_capture) ->
                    ansel@bounding_box:expand(_capture, Thickness)
                end
            ),
            gleam@result:map(_pipe@1, fun ansel@bounding_box:to_ltwh_tuple/1)
        end,
        fun(_use0) ->
            {_, _, Outline_width, Outline_height} = _use0,
            _pipe@2 = new(Outline_width, Outline_height, Color),
            gleam@result:'try'(
                _pipe@2,
                fun(_capture@1) ->
                    composite_over(_capture@1, Image, Thickness, Thickness)
                end
            )
        end
    ).

-file("src/ansel/image.gleam", 462).
?DOC(
    " Returns the dimensions of an image as a bounding box. Useful for bounding\n"
    " box operations.\n"
).
-spec to_bounding_box(ansel:image()) -> ansel@bounding_box:bounding_box().
to_bounding_box(Image) ->
    ansel@bounding_box:unchecked_ltwh(
        0,
        0,
        'Elixir.Vix.Vips.Image':width(Image),
        'Elixir.Vix.Vips.Image':height(Image)
    ).

-file("src/ansel/image.gleam", 488).
?DOC(" Resizes an image by the given scale, preserving the aspect ratio.\n").
-spec scale(ansel:image(), float()) -> {ok, ansel:image()} |
    {error, snag:snag()}.
scale(Img, Scale) ->
    _pipe = 'Elixir.Vix.Vips.Operation':resize(Img, Scale),
    _pipe@1 = gleam@result:map_error(_pipe, fun snag:new/1),
    snag:context(_pipe@1, <<"Unable to resize image"/utf8>>).

-file("src/ansel/image.gleam", 472).
?DOC(" Scales an image to the given width, preserving the aspect ratio.\n").
-spec scale_width(ansel:image(), integer()) -> {ok, ansel:image()} |
    {error, snag:snag()}.
scale_width(Img, Target) ->
    scale(Img, case gleam@int:to_float('Elixir.Vix.Vips.Image':width(Img)) of
            +0.0 -> +0.0;
            -0.0 -> -0.0;
            Gleam@denominator -> gleam@int:to_float(Target) / Gleam@denominator
        end).

-file("src/ansel/image.gleam", 480).
?DOC(" Scales an image to the given height, preserving the aspect ratio.\n").
-spec scale_height(ansel:image(), integer()) -> {ok, ansel:image()} |
    {error, snag:snag()}.
scale_height(Img, Target) ->
    scale(Img, case gleam@int:to_float('Elixir.Vix.Vips.Image':height(Img)) of
            +0.0 -> +0.0;
            -0.0 -> -0.0;
            Gleam@denominator -> gleam@int:to_float(Target) / Gleam@denominator
        end).

-file("src/ansel/image.gleam", 511).
?DOC(
    " Writes an image to the specified path in the specified format.\n"
    " A filename extension is added automatically to the path, based on the format,\n"
    " and therefore should not be provided by the user.\n"
    " Returns the absolute path to the image as it was written to disk, including the extension.\n"
    " \n"
    " ## Example\n"
    " ```gleam\n"
    " image.new(6, 6, color.Olive)\n"
    " |> image.write(to: \"wobble\", in: image.PNG)\n"
    " // -> Ok(\"Users/lucy/myproject/wobble.png\")\n"
    " ```\n"
).
-spec write(ansel:image(), binary(), image_format()) -> {ok, binary()} |
    {error, snag:snag()}.
write(Img, Path, Format) ->
    _pipe = 'Elixir.Ansel':write_to_file(
        Img,
        Path,
        image_format_to_string(Format)
    ),
    _pipe@1 = gleam@result:map_error(_pipe, fun snag:new/1),
    snag:context(_pipe@1, <<"Unable to write image to file"/utf8>>).

-file("src/ansel/image.gleam", 529).
?DOC(" Reads an image from the specified path.\n").
-spec read(binary()) -> {ok, ansel:image()} | {error, snag:snag()}.
read(Path) ->
    _pipe = 'Elixir.Ansel':read(Path),
    _pipe@1 = gleam@result:map_error(_pipe, fun snag:new/1),
    snag:context(_pipe@1, <<"Unable to read image from file"/utf8>>).

-file("src/ansel/image.gleam", 539).
?DOC(" Creates a thumbnail of an image at the specified path with the given width.\n").
-spec create_thumbnail(binary(), integer()) -> {ok, ansel:image()} |
    {error, snag:snag()}.
create_thumbnail(Path, Width) ->
    _pipe = 'Elixir.Vix.Vips.Operation':thumbnail(Path, Width),
    _pipe@1 = gleam@result:map_error(_pipe, fun snag:new/1),
    snag:context(_pipe@1, <<"Unable to create thumbnail from file"/utf8>>).
