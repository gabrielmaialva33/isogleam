-module(httpc_ffi).
-export([post_json/2, get/1]).

%% POST JSON request
post_json(Url, Body) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    UrlStr = binary_to_list(Url),
    BodyStr = binary_to_list(Body),
    Request = {UrlStr, [], "application/json", BodyStr},
    case httpc:request(post, Request, [{timeout, 300000}], [{body_format, binary}]) of
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {ok, {StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% GET request
get(Url) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    UrlStr = binary_to_list(Url),
    case httpc:request(get, {UrlStr, []}, [{timeout, 30000}], [{body_format, binary}]) of
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {ok, {StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.
