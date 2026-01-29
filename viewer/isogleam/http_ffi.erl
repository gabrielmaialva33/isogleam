%% IsoGleam HTTP FFI
%% Simple HTTP client using Erlang httpc

-module(http_ffi).
-export([request/4, post_json/3, get/2]).

%% Initialize inets application
init() ->
    case application:ensure_all_started(inets) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    case application:ensure_all_started(ssl) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

%% Generic HTTP request
%% request(Method, URL, Headers, Body) -> {ok, {Status, Headers, Body}} | {error, Reason}
request(Method, URL, Headers, Body) ->
    init(),
    MethodAtom = binary_to_atom(string:lowercase(Method), utf8),
    URLString = binary_to_list(URL),
    HeadersList = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],

    Request = case MethodAtom of
        get -> {URLString, HeadersList};
        head -> {URLString, HeadersList};
        _ -> {URLString, HeadersList, "application/json", Body}
    end,

    HTTPOpts = [{timeout, 30000}, {connect_timeout, 5000}],
    Opts = [{body_format, binary}],

    case httpc:request(MethodAtom, Request, HTTPOpts, Opts) of
        {ok, {{_, Status, _}, RespHeaders, RespBody}} ->
            {ok, {Status, format_headers(RespHeaders), RespBody}};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% POST JSON helper
post_json(URL, Body, Headers) ->
    AllHeaders = [{<<"Content-Type">>, <<"application/json">>} | Headers],
    request(<<"POST">>, URL, AllHeaders, Body).

%% GET helper
get(URL, Headers) ->
    request(<<"GET">>, URL, Headers, <<>>).

%% Format response headers to binary tuples
format_headers(Headers) ->
    [{list_to_binary(K), list_to_binary(V)} || {K, V} <- Headers].
