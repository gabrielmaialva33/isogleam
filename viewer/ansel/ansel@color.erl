-module(ansel@color).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_rgb_tuple/1, to_bands/1, to_rgba_tuple/1, add_alpha_band/2]).
-export_type([color/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Predefined colors and color functions to be used with Ansel image \n"
    " operations.\n"
).

-type color() :: {rgba, integer(), integer(), integer(), integer()} |
    {rgb, integer(), integer(), integer()} |
    gleam_lucy |
    gleam_navy |
    maroon |
    dark_red |
    brown |
    firebrick |
    crimson |
    red |
    tomato |
    coral |
    indian_red |
    light_coral |
    dark_salmon |
    salmon |
    light_salmon |
    orange_red |
    dark_orange |
    orange |
    gold |
    dark_golden_rod |
    golden_rod |
    pale_golden_rod |
    dark_khaki |
    khaki |
    olive |
    yellow |
    yellow_green |
    dark_olive_green |
    olive_drab |
    lawn_green |
    chartreuse |
    green_yellow |
    dark_green |
    green |
    forest_green |
    lime |
    lime_green |
    light_green |
    pale_green |
    dark_sea_green |
    medium_spring_green |
    spring_green |
    sea_green |
    medium_aqua_marine |
    medium_sea_green |
    light_sea_green |
    dark_slate_gray |
    teal |
    dark_cyan |
    aqua |
    cyan |
    light_cyan |
    dark_turquoise |
    turquoise |
    medium_turquoise |
    pale_turquoise |
    aqua_marine |
    powder_blue |
    cadet_blue |
    steel_blue |
    corn_flower_blue |
    deep_sky_blue |
    dodger_blue |
    light_blue |
    sky_blue |
    light_sky_blue |
    midnight_blue |
    navy |
    dark_blue |
    medium_blue |
    blue |
    royal_blue |
    blue_violet |
    indigo |
    dark_slate_blue |
    slate_blue |
    medium_slate_blue |
    medium_purple |
    dark_magenta |
    dark_violet |
    dark_orchid |
    medium_orchid |
    purple |
    thistle |
    plum |
    violet |
    magenta |
    fuchsia |
    orchid |
    medium_violet_red |
    pale_violet_red |
    deep_pink |
    hot_pink |
    light_pink |
    pink |
    antique_white |
    beige |
    bisque |
    blanched_almond |
    wheat |
    corn_silk |
    lemon_chiffon |
    light_golden_rod_yellow |
    light_yellow |
    saddle_brown |
    sienna |
    chocolate |
    peru |
    sandy_brown |
    burly_wood |
    tan |
    rosy_brown |
    moccasin |
    navajo_white |
    peach_puff |
    misty_rose |
    lavender_blush |
    linen |
    old_lace |
    papaya_whip |
    sea_shell |
    mint_cream |
    slate_gray |
    light_slate_gray |
    light_steel_blue |
    lavender |
    floral_white |
    alice_blue |
    ghost_white |
    honeydew |
    ivory |
    azure |
    snow |
    black |
    dim_gray |
    dim_grey |
    gray |
    grey |
    dark_gray |
    dark_grey |
    silver |
    light_gray |
    light_grey |
    gainsboro |
    white_smoke |
    white.

-file("src/ansel/color.gleam", 182).
?DOC(
    " Returns a color as a tuple of integer rgb bands\n"
    " \n"
    " ## Example\n"
    " ```gleam\n"
    " color.to_rgb_tuple(color.GleamLucy)\n"
    " // -> #(255, 175, 243)\n"
    " ```\n"
).
-spec to_rgb_tuple(color()) -> {integer(), integer(), integer()}.
to_rgb_tuple(Color) ->
    case Color of
        {rgba, R, G, B, _} ->
            {R, G, B};

        {rgb, R@1, G@1, B@1} ->
            {R@1, G@1, B@1};

        gleam_lucy ->
            {255, 175, 243};

        gleam_navy ->
            {41, 45, 62};

        maroon ->
            {128, 0, 0};

        dark_red ->
            {139, 0, 0};

        brown ->
            {165, 42, 42};

        firebrick ->
            {178, 34, 34};

        crimson ->
            {220, 20, 60};

        red ->
            {255, 0, 0};

        tomato ->
            {255, 99, 71};

        coral ->
            {255, 127, 80};

        indian_red ->
            {205, 92, 92};

        light_coral ->
            {240, 128, 128};

        dark_salmon ->
            {233, 150, 122};

        salmon ->
            {250, 128, 114};

        light_salmon ->
            {255, 160, 122};

        orange_red ->
            {255, 69, 0};

        dark_orange ->
            {255, 140, 0};

        orange ->
            {255, 165, 0};

        gold ->
            {255, 215, 0};

        dark_golden_rod ->
            {184, 134, 11};

        golden_rod ->
            {218, 165, 32};

        pale_golden_rod ->
            {238, 232, 170};

        dark_khaki ->
            {189, 183, 107};

        khaki ->
            {240, 230, 140};

        olive ->
            {128, 128, 0};

        yellow ->
            {255, 255, 0};

        yellow_green ->
            {154, 205, 50};

        dark_olive_green ->
            {85, 107, 47};

        olive_drab ->
            {107, 142, 35};

        lawn_green ->
            {124, 252, 0};

        chartreuse ->
            {127, 255, 0};

        green_yellow ->
            {173, 255, 47};

        dark_green ->
            {0, 100, 0};

        green ->
            {0, 128, 0};

        forest_green ->
            {34, 139, 34};

        lime ->
            {0, 255, 0};

        lime_green ->
            {50, 205, 50};

        light_green ->
            {144, 238, 144};

        pale_green ->
            {152, 251, 152};

        dark_sea_green ->
            {143, 188, 143};

        medium_spring_green ->
            {0, 250, 154};

        spring_green ->
            {0, 255, 127};

        sea_green ->
            {46, 139, 87};

        medium_aqua_marine ->
            {102, 205, 170};

        medium_sea_green ->
            {60, 179, 113};

        light_sea_green ->
            {32, 178, 170};

        dark_slate_gray ->
            {47, 79, 79};

        teal ->
            {0, 128, 128};

        dark_cyan ->
            {0, 139, 139};

        aqua ->
            {0, 255, 255};

        cyan ->
            {0, 255, 255};

        light_cyan ->
            {224, 255, 255};

        dark_turquoise ->
            {0, 206, 209};

        turquoise ->
            {64, 224, 208};

        medium_turquoise ->
            {72, 209, 204};

        pale_turquoise ->
            {175, 238, 238};

        aqua_marine ->
            {127, 255, 212};

        powder_blue ->
            {176, 224, 230};

        cadet_blue ->
            {95, 158, 160};

        steel_blue ->
            {70, 130, 180};

        corn_flower_blue ->
            {100, 149, 237};

        deep_sky_blue ->
            {0, 191, 255};

        dodger_blue ->
            {30, 144, 255};

        light_blue ->
            {173, 216, 230};

        sky_blue ->
            {135, 206, 235};

        light_sky_blue ->
            {135, 206, 250};

        midnight_blue ->
            {25, 25, 112};

        navy ->
            {0, 0, 128};

        dark_blue ->
            {0, 0, 139};

        medium_blue ->
            {0, 0, 205};

        blue ->
            {0, 0, 255};

        royal_blue ->
            {65, 105, 225};

        blue_violet ->
            {138, 43, 226};

        indigo ->
            {75, 0, 130};

        dark_slate_blue ->
            {72, 61, 139};

        slate_blue ->
            {106, 90, 205};

        medium_slate_blue ->
            {123, 104, 238};

        medium_purple ->
            {147, 112, 219};

        dark_magenta ->
            {139, 0, 139};

        dark_violet ->
            {148, 0, 211};

        dark_orchid ->
            {153, 50, 204};

        medium_orchid ->
            {186, 85, 211};

        purple ->
            {128, 0, 128};

        thistle ->
            {216, 191, 216};

        plum ->
            {221, 160, 221};

        violet ->
            {238, 130, 238};

        magenta ->
            {255, 0, 255};

        fuchsia ->
            {255, 0, 255};

        orchid ->
            {218, 112, 214};

        medium_violet_red ->
            {199, 21, 133};

        pale_violet_red ->
            {219, 112, 147};

        deep_pink ->
            {255, 20, 147};

        hot_pink ->
            {255, 105, 180};

        light_pink ->
            {255, 182, 193};

        pink ->
            {255, 192, 203};

        antique_white ->
            {250, 235, 215};

        beige ->
            {245, 245, 220};

        bisque ->
            {255, 228, 196};

        blanched_almond ->
            {255, 235, 205};

        wheat ->
            {245, 222, 179};

        corn_silk ->
            {255, 248, 220};

        lemon_chiffon ->
            {255, 250, 205};

        light_golden_rod_yellow ->
            {250, 250, 210};

        light_yellow ->
            {255, 255, 224};

        saddle_brown ->
            {139, 69, 19};

        sienna ->
            {160, 82, 45};

        chocolate ->
            {210, 105, 30};

        peru ->
            {205, 133, 63};

        sandy_brown ->
            {244, 164, 96};

        burly_wood ->
            {222, 184, 135};

        tan ->
            {210, 180, 140};

        rosy_brown ->
            {188, 143, 143};

        moccasin ->
            {255, 228, 181};

        navajo_white ->
            {255, 222, 173};

        peach_puff ->
            {255, 218, 185};

        misty_rose ->
            {255, 228, 225};

        lavender_blush ->
            {255, 240, 245};

        linen ->
            {250, 240, 230};

        old_lace ->
            {253, 245, 230};

        papaya_whip ->
            {255, 239, 213};

        sea_shell ->
            {255, 245, 238};

        mint_cream ->
            {245, 255, 250};

        slate_gray ->
            {112, 128, 144};

        light_slate_gray ->
            {119, 136, 153};

        light_steel_blue ->
            {176, 196, 222};

        lavender ->
            {230, 230, 250};

        floral_white ->
            {255, 250, 240};

        alice_blue ->
            {240, 248, 255};

        ghost_white ->
            {248, 248, 255};

        honeydew ->
            {240, 255, 240};

        ivory ->
            {255, 255, 240};

        azure ->
            {240, 255, 255};

        snow ->
            {255, 250, 250};

        black ->
            {0, 0, 0};

        dim_gray ->
            {105, 105, 105};

        dim_grey ->
            {105, 105, 105};

        gray ->
            {128, 128, 128};

        grey ->
            {128, 128, 128};

        dark_gray ->
            {169, 169, 169};

        dark_grey ->
            {169, 169, 169};

        silver ->
            {192, 192, 192};

        light_gray ->
            {211, 211, 211};

        light_grey ->
            {211, 211, 211};

        gainsboro ->
            {220, 220, 220};

        white_smoke ->
            {245, 245, 245};

        white ->
            {255, 255, 255}
    end.

-file("src/ansel/color.gleam", 165).
?DOC(
    " Returns a color as a list of integer rbg or rbga bands\n"
    " \n"
    " ## Example\n"
    " ```gleam\n"
    " color.to_bands(color.GleamLucy)\n"
    " // -> [255, 175, 243]\n"
    " ```\n"
).
-spec to_bands(color()) -> list(integer()).
to_bands(Color) ->
    case Color of
        {rgba, R, G, B, A} ->
            [R, G, B, A];

        C ->
            {R@1, G@1, B@1} = to_rgb_tuple(C),
            [R@1, G@1, B@1]
    end.

-file("src/ansel/color.gleam", 342).
?DOC(
    " Returns a color as a tuple of integer rgba bands\n"
    " \n"
    " ## Example\n"
    " ```gleam\n"
    " color.to_rgba_tuple(color.GleamLucy)\n"
    " // -> #(255, 175, 243, 255)\n"
    " ```\n"
).
-spec to_rgba_tuple(color()) -> {integer(), integer(), integer(), integer()}.
to_rgba_tuple(Color) ->
    case Color of
        {rgba, R, G, B, A} ->
            {R, G, B, A};

        C ->
            {R@1, G@1, B@1} = to_rgb_tuple(C),
            {R@1, G@1, B@1, 255}
    end.

-file("src/ansel/color.gleam", 361).
?DOC(
    " Adds an alpha band of the given value to a color, possibly making it\n"
    " partially transparent.\n"
    " \n"
    " ## Example\n"
    " ```gleam\n"
    " color.add_alpha_band(color.GleamLucy, of: 128)\n"
    " |> color.to_rgba_tuple\n"
    " // -> #(255, 175, 243, 128)\n"
    " ```\n"
).
-spec add_alpha_band(color(), integer()) -> color().
add_alpha_band(Color, Alpha) ->
    {R, G, B} = to_rgb_tuple(Color),
    {rgba, R, G, B, Alpha}.
