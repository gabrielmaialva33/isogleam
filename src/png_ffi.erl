-module(png_ffi).
-export([decode/1, encode/4]).

%% Decode PNG binary to {Width, Height, ColorType, PixelData}
%% Uses pure Erlang PNG decoding with optimized unfiltering
decode(Bytes) ->
    try
        case parse_png(Bytes) of
            {ok, Width, Height, ColorType, Pixels} ->
                {ok, {Width, Height, ColorType, Pixels}};
            {error, Reason} ->
                {error, atom_to_binary(Reason, utf8)}
        end
    catch
        _:Error ->
            {error, iolist_to_binary(io_lib:format("~p", [Error]))}
    end.

%% Encode pixels to PNG
encode(_Width, _Height, _Pixels, _Options) ->
    %% Simplified - would need zlib compression
    {error, <<"encode not implemented">>}.

%% Parse PNG file
parse_png(<<137, 80, 78, 71, 13, 10, 26, 10, Rest/binary>>) ->
    parse_chunks(Rest, undefined, undefined, undefined, undefined, []);
parse_png(_) ->
    {error, invalid_png_signature}.

%% Parse PNG chunks
%% State: Width, Height, BitDepth, ColorType, IDATs
parse_chunks(<<Length:32/big, Type:4/binary, Rest/binary>>, Width, Height, Depth, ColorType, Data) ->
    DataLen = Length,
    case Rest of
        <<ChunkData:DataLen/binary, _CRC:32/big, Remaining/binary>> ->
            case Type of
                <<"IHDR">> ->
                    <<W:32/big, H:32/big, BitDepth:8, CType:8,
                      _Compression:8, _Filter:8, _Interlace:8>> = ChunkData,
                    %% Only support 8-bit depth for now
                    case BitDepth of
                        8 -> parse_chunks(Remaining, W, H, BitDepth, CType, Data);
                        _ -> {error, unsupported_bit_depth}
                    end;
                <<"IDAT">> ->
                    parse_chunks(Remaining, Width, Height, Depth, ColorType, [ChunkData | Data]);
                <<"IEND">> ->
                    %% Decompress and decode image data
                    Compressed = iolist_to_binary(lists:reverse(Data)),
                    case decompress_idat(Compressed, Width, Height, ColorType) of
                        {ok, Pixels} ->
                            {ok, Width, Height, ColorType, Pixels};
                        Error ->
                            Error
                    end;
                _ ->
                    %% Skip other chunks (PLTE, pHYs, etc.)
                    parse_chunks(Remaining, Width, Height, Depth, ColorType, Data)
            end;
        _ ->
            {error, truncated_chunk}
    end;
parse_chunks(<<>>, Width, Height, Depth, ColorType, Data) when Width =/= undefined ->
    Compressed = iolist_to_binary(lists:reverse(Data)),
    case decompress_idat(Compressed, Width, Height, ColorType) of
        {ok, Pixels} ->
            {ok, Width, Height, ColorType, Pixels};
        Error ->
            Error
    end;
parse_chunks(_, _, _, _, _, _) ->
    {error, invalid_chunks}.

%% Decompress IDAT data using zlib
decompress_idat(Compressed, Width, Height, ColorType) ->
    try
        Z = zlib:open(),
        ok = zlib:inflateInit(Z),
        Decompressed = iolist_to_binary(zlib:inflate(Z, Compressed)),
        zlib:inflateEnd(Z),
        zlib:close(Z),

        %% Calculate Bytes Per Pixel
        BPP = case ColorType of
            0 -> 1; %% Grayscale
            2 -> 3; %% RGB
            4 -> 2; %% Grayscale + Alpha
            6 -> 4; %% RGBA
            _ -> 0
        end,

        case BPP of
            0 -> {error, unsupported_color_type};
            _ ->
                Pixels = unfilter_rows(Decompressed, Width, Height, BPP),
                {ok, Pixels}
        end
    catch
        _:Error ->
            {error, {decompress_failed, Error}}
    end.

%% Remove PNG filter bytes from each row
unfilter_rows(Data, Width, _Height, BPP) ->
    RowBytes = Width * BPP + 1,
    %% Initialize previous row as zeros
    PrevRow = <<0:((Width * BPP) * 8)>>,
    unfilter_rows(Data, Width, BPP, RowBytes, PrevRow, []).

unfilter_rows(<<>>, _, _, _, _, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
unfilter_rows(Data, Width, BPP, RowBytes, PrevRow, Acc) ->
    case Data of
        <<FilterType:8, RowData:(RowBytes-1)/binary, Rest/binary>> ->
            UnfilteredRow = apply_filter(FilterType, RowData, PrevRow, BPP),
            unfilter_rows(Rest, Width, BPP, RowBytes, UnfilteredRow, [UnfilteredRow | Acc]);
        _ ->
            iolist_to_binary(lists:reverse(Acc))
    end.

%% Apply PNG filter
apply_filter(0, Row, _PrevRow, _BPP) -> Row;
apply_filter(1, Row, _PrevRow, BPP) -> unfilter_sub(Row, BPP, <<0:(BPP*8)>>, <<>>);
apply_filter(2, Row, PrevRow, BPP) -> unfilter_up(Row, PrevRow, BPP, <<>>);
apply_filter(3, Row, PrevRow, BPP) -> unfilter_average(Row, PrevRow, BPP, <<0:(BPP*8)>>, <<>>);
apply_filter(4, Row, PrevRow, BPP) -> unfilter_paeth(Row, PrevRow, BPP, <<0:(BPP*8)>>, <<0:(BPP*8)>>, <<>>).

%% Sub: Raw(x) + Raw(x-bpp)
unfilter_sub(<<>>, _BPP, _Left, Acc) -> Acc;
unfilter_sub(<<X:8, Rest/binary>>, BPP, Left, Acc) ->
    %% We need the *reconstructed* left pixel.
    %% But here we process byte by byte.
    %% Left pixel's byte at this position?
    %% Wait, Sub(x) = Raw(x) + Recon(x-bpp).
    %% Since we process in order, Recon(x-bpp) is available in our Acc?
    %% Yes, but we need to fetch it.
    %% Optimized: Maintain a 'Left' buffer of BPP bytes.
    <<L:8, NewLeft/binary>> = Left,
    Recon = (X + L) band 255,
    unfilter_sub(Rest, BPP, <<NewLeft/binary, Recon:8>>, <<Acc/binary, Recon:8>>).

%% Up: Raw(x) + Prior(x)
unfilter_up(<<>>, _, _BPP, Acc) -> Acc;
unfilter_up(<<X:8, Rest/binary>>, <<U:8, RestPrev/binary>>, BPP, Acc) ->
    Recon = (X + U) band 255,
    unfilter_up(Rest, RestPrev, BPP, <<Acc/binary, Recon:8>>).

%% Average: Raw(x) + floor((Recon(x-bpp) + Prior(x))/2)
unfilter_average(<<>>, _, _BPP, _Left, Acc) -> Acc;
unfilter_average(<<X:8, Rest/binary>>, <<U:8, RestPrev/binary>>, BPP, Left, Acc) ->
    <<L:8, NewLeft/binary>> = Left,
    Avg = (L + U) div 2,
    Recon = (X + Avg) band 255,
    unfilter_average(Rest, RestPrev, BPP, <<NewLeft/binary, Recon:8>>, <<Acc/binary, Recon:8>>).

%% Paeth: Raw(x) + PaethPredictor(Recon(x-bpp), Prior(x), Prior(x-bpp))
unfilter_paeth(<<>>, _, _BPP, _Left, _LeftUp, Acc) -> Acc;
unfilter_paeth(<<X:8, Rest/binary>>, <<U:8, RestPrev/binary>>, BPP, Left, LeftUp, Acc) ->
    <<L:8, NewLeft/binary>> = Left,
    <<C:8, NewLeftUp/binary>> = LeftUp,
    Pred = paeth_predictor(L, U, C),
    Recon = (X + Pred) band 255,
    %% Next iteration: Left becomes Recon, LeftUp becomes U
    unfilter_paeth(Rest, RestPrev, BPP, <<NewLeft/binary, Recon:8>>, <<NewLeftUp/binary, U:8>>, <<Acc/binary, Recon:8>>).

paeth_predictor(A, B, C) ->
    P = A + B - C,
    PA = abs(P - A),
    PB = abs(P - B),
    PC = abs(P - C),
    if
        PA =< PB, PA =< PC -> A;
        PB =< PC -> B;
        true -> C
    end.
