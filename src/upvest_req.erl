%%% @author Yao Adzaku <yao@rpip>
%%% @copyright (C) 2019, Yao Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2019 by Yao Adzaku <yao@rpip>

-module(upvest_req).
-include("upvest.hrl").
-export([
         request/2,
         get_all/3,
         get_some/4
        ]).


-spec request(request(), client()) -> result().
request(Req, Client) ->
    do_request(Req, Client).

do_request(Req, Client) ->
    #request{method = Method, body = Body} = Req,
    {Headers, RequestUrl} = headers_with_uri(Req, Client),
    EncodedBody = upvest_json:encode(Body),
    Options = Client#client.options,
    ?PRINT(RequestUrl),
                                                %?PRINT(Req),
                                                %?PRINT(Headers),
    case hackney:Method(RequestUrl, Headers, EncodedBody, Options) of
        {ok, Status, _RespHeaders, ClientRef} when Status >= 200 andalso Status < 300 ->
            {ok, Body1} = hackney:body(ClientRef),
            DecodedBody = upvest_json:decode(Body1),
            ?PRINT(maps:get(<<"next">>, DecodedBody)),
            {ok, DecodedBody};
        {ok, 302, RespHeaders, _ClientRef} ->
            RedirectUrl = proplists:get_value(<<"Location">>, RespHeaders),
            do_request(Req#request{uri=RedirectUrl}, Client);
        {ok, 404, _RespHeaders, ClientRef} ->
            {error, hackney:body(ClientRef)};
        {ok, Status, _RespHeaders, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            Error = format_error(Status, RespBody),
            {error, Error};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_some(atom(), pos_integer(), request(), client()) -> {ok, #upvest_list{}} | error().
get_some(Scope, MaxCount, Req, Client) ->
    do_get_some(Scope, MaxCount, 0, Req, Client, true, []).
do_get_some(_Scope, _MaxCount, _TotalCount, _Req, _Client, false, Acc) ->
    {ok, fold_paginated_results(Acc)};
do_get_some(Scope, MaxCount, TotalCount, Req, Client, true, Acc) ->
    case request(Req, Client) of
        {ok, Resp} ->
            HasMore = has_more(Resp),
            case (MaxCount =< TotalCount) and HasMore of
                true ->
                    NextURI = maps:get(<<"next">>, Resp),
                    Req1 = build_paginated_req(Scope, NextURI, Req),
                    Count = length(Resp#upvest_list.results),
                    do_get_some(Scope, MaxCount, TotalCount + Count, Req1, Client, HasMore, [Resp|Acc]);
                _ ->
                    do_get_some(Scope, MaxCount, TotalCount, Req, Client, false, [Resp|Acc])
            end;
        {error, _} = Error ->
            Error
    end.

-spec get_all(atom(), request(), client()) -> {ok, #upvest_list{}} | error().
get_all(Scope, Req, Client) ->
    do_get_all(Scope, Req, Client, true, []).
do_get_all(_Scope, _Req, _Client, false, Acc) ->
    {ok, fold_paginated_results(Acc)};
do_get_all(Scope, Req, Client, true, Acc) ->
    case request(Req, Client) of
        {ok, Resp} ->
            HasMore = has_more(Resp),
            case HasMore of
                true ->
                    NextURI = maps:get(<<"next">>, Resp),
                    Req1 = build_paginated_req(Scope, NextURI, Req),
                    do_get_all(Scope, Req1, Client, HasMore, [Resp|Acc]);
                _ ->
                    do_get_all(Scope, Req, Client, false, [Resp|Acc])
            end;
        {error, _} = Error ->
            Error
    end.

-spec fold_paginated_results([#upvest_list{}]) -> #upvest_list{}.
fold_paginated_results(Objects) ->
    Objects0 = lists:reverse(Objects),
    Accl = lists:foldl(
             fun(X, #upvest_list{results=R} = Acc0) ->
                     N = maps:get(<<"next">>, X),
                     P = maps:get(<<"previous">>, X),
                     R1 = maps:get(<<"results">>, X),
                     Acc0#upvest_list{next=N, previous=P, results=[R1|R]}
             end, #upvest_list{}, Objects0),
    R2 = lists:flatten(Accl#upvest_list.results),
    Accl#upvest_list{results=lists:reverse(R2)}.

-spec build_paginated_req(atom(), url(), request()) -> request().
build_paginated_req(Scope, NextURI, Req) ->
    ParsedURI = upvest_utils:parse_uri(NextURI),
    MQuery = maps:from_list(ParsedURI#uri.query),
    URI = upvest:build_uri(Scope, MQuery),
    Req#request{uri=URI}.

has_more(M) ->
    maps:is_key(<<"next">>, M) andalso (maps:get(<<"next">>, M) =/= null).

-spec versioned_url(string()) -> string().
versioned_url(Path) ->
    ?API_VERSION ++ Path.

headers_with_uri(Req, Client) ->
    AuthHeaders  = authenticate(Req, Client),
    Headers = lists:flatten([AuthHeaders, Client#client.headers | ?DEFAULT_HEADERS]),
    FullUri = ?DEFAULT_BASE_URL ++ versioned_url(Req#request.uri),
    {Headers, FullUri}.

format_error(ErrCode, Body) ->
    ErrCodeMeaning = case ErrCode of
                         400 -> invalid_request_error;
                         401 -> authorization_error;
                         403 -> authentication_error;
                         409 -> duplicate_user;
                         E when E >= 500 -> server_error;
                         E when E =:= 403 orelse E > 404 -> api_error;
                         _ -> unknown_error
                     end,
    format_error(ErrCode, ErrCodeMeaning, Body).

format_error(ErrCode, ErrCodeMeaning, Body) ->
    ?PRINT(Body),
    PreDecoded = upvest_json:decode(Body),
    DecodedResult = proplists:get_value(<<"error">>, PreDecoded),
    #upvest_error{type    = ErrCodeMeaning,
                  http_error_code = ErrCode,
                  message = ?V(message),
                  details   = ?V(details)}.

-spec authenticate(request(), client()) -> proplists:list().
authenticate(#request{} = Req, #client{credentials = #keyauth{} = Cred}) ->
    #request{method = Method, uri = Uri, body = Body} = Req,
    #keyauth{secret = Secret, key = Key, passphrase = Passphrase} = Cred,
    Timestamp = upvest_utils:timestamp(),
    VersionedPath = versioned_url(Uri),
    Method1 = string:uppercase(atom_to_list(Method)),
    EncodedBody = upvest_json:encode(Body),
    Message = io_lib:format("~w~s~s~s", [Timestamp, Method1, VersionedPath, EncodedBody]),
    Message1 = string:join(Message, ""),
    Signature = upvest_utils:generate_signature(Secret, Message1),
    [{<<"Content-Type">>, "application/json"},
     {<<"X-UP-API-Key">>, Key},
     {<<"X-UP-API-Signature">>, Signature},
     {<<"X-UP-API-Timestamp">>,  Timestamp},
     {<<"X-UP-API-Passphrase">>,  Passphrase},
     {<<"X-UP-API-Signed-Path">>, VersionedPath}].
