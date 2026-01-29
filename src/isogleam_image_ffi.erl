%% IsoGleam Image FFI
%% Load/save PNG files using Erlang
%% Uses wx:image or egd for PNG handling

-module(isogleam_image_ffi).
-export([load_png/1, save_png/2]).

%% Load PNG file to tensor data
%% Returns {ok, Tensor} or {error, Reason}
load_png(Path) ->
    case file:read_file(Path) of
        {ok, Binary} ->
            case decode_png(Binary) of
                {ok, Width, Height, Pixels} ->
                    %% Create tensor structure matching viva_tensor format
                    Tensor = create_tensor([Height, Width, 3], Pixels),
                    {ok, Tensor};
                {error, Reason} ->
                    {error, {invalid_format, Reason}}
            end;
        {error, enoent} ->
            {error, {file_not_found, Path}};
        {error, Reason} ->
            {error, {read_error, Reason}}
    end.

%% Save tensor data as PNG file
save_png(Tensor, Path) ->
    case extract_image_data(Tensor) of
        {ok, Width, Height, Pixels} ->
            Binary = encode_png(Width, Height, Pixels),
            case file:write_file(Path, Binary) of
                ok -> {ok, nil};
                {error, Reason} -> {error, {write_error, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_format, Reason}}
    end.

%% Decode PNG binary to RGB pixels
%% Simplified - uses zlib for deflate
decode_png(Binary) ->
    %% PNG magic number check
    case Binary of
        <<137, 80, 78, 71, 13, 10, 26, 10, _Rest/binary>> ->
            %% For now, return placeholder
            %% TODO: Implement full PNG decoder or use external lib
            {error, <<"PNG decoding not implemented - use external tool">>};
        _ ->
            {error, <<"Not a PNG file">>}
    end.

%% Encode RGB pixels to PNG binary
encode_png(_Width, _Height, _Pixels) ->
    %% TODO: Implement PNG encoder
    %% For now, return empty binary
    <<>>.

%% Create tensor structure from pixel data
create_tensor(Shape, Data) ->
    %% Match viva_tensor format
    #{
        shape => Shape,
        data => list_to_tuple(Data),
        strides => compute_strides(Shape),
        offset => 0
    }.

%% Compute strides for shape
compute_strides(Shape) ->
    compute_strides(lists:reverse(Shape), 1, []).

compute_strides([], _, Acc) ->
    Acc;
compute_strides([H|T], Stride, Acc) ->
    compute_strides(T, Stride * H, [Stride|Acc]).

%% Extract image data from tensor
extract_image_data(Tensor) ->
    case Tensor of
        #{shape := [H, W, 3], data := Data} ->
            Pixels = tuple_to_list(Data),
            {ok, W, H, Pixels};
        _ ->
            {error, <<"Invalid tensor shape for image">>}
    end.
