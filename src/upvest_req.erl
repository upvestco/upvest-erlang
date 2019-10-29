%%% @author Yao Adzaku <yao@rpip>
%%% @copyright (C) 2019, Yao Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2019 by Yao Adzaku <yao@rpip>

-module(upvest_req).
-include("upvest.hrl").
-export([
         request/1,
         get_all/2,
         get_some/3
        ]).


-spec request(request()) -> result().
request(Req) ->
    do_request(Req).

do_request(Req) ->
    #request{method = Method, body = Body} = Req,
    {Headers, RequestUrl} = headers_with_uri(Req),
    EncodedBody = upvest_json:encode(Body),
    ?PRINT(RequestUrl),
    ?PRINT(Headers),
    ?PRINT(Body),
    case hackney:Method(RequestUrl, Headers, EncodedBody, Req#request.options) of
                                                % delete endpoint returns 204 No Content
        {ok, Status, _RespHeaders, _ClientRef} when Status =:= 204 ->
            {ok, no_content};
        {ok, Status, _RespHeaders, ClientRef} when Status >= 200 andalso Status < 300 ->
            {ok, Body1} = hackney:body(ClientRef),
            DecodedBody = upvest_json:decode(Body1),
            {ok, DecodedBody};
        {ok, 302, RespHeaders, _ClientRef} ->
            RedirectUrl = proplists:get_value(<<"Location">>, RespHeaders),
            do_request(Req#request{uri=RedirectUrl});
        {ok, 404, _RespHeaders, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, RespBody};
        {ok, Status, _RespHeaders, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            Error = format_error(Status, RespBody),
            {error, Error};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_some(atom(), pos_integer(), request()) -> {ok, paginated_list()} | error().
get_some(Resource, Limit, Req) ->
    do_get_some(Resource, Limit, 0, Req, true, []).
do_get_some(_Resource, _Limit, _TotalCount, _Req, false, Acc) ->
    {ok, fold_paginated(Acc)};
do_get_some(Resource, Limit, TotalCount, Req, true, Acc) ->
    case request(Req) of
        {ok, Resp} ->
            HasMore = has_more(Resp),
            case (Limit =< TotalCount) and HasMore of
                true ->
                    NextURI = maps:get(<<"next">>, Resp),
                    Req1 = paginated_req(Resource, NextURI, Req),
                    Count = length(Resp#paginated_list.results),
                    do_get_some(Resource, Limit, TotalCount + Count, Req1, HasMore, [Resp|Acc]);
                _ ->
                    do_get_some(Resource, Limit, TotalCount, Req, false, [Resp|Acc])
            end;
        {error, _} = Error ->
            Error
    end.

-spec get_all(atom(), request()) -> {ok, paginated_list() | error()}.
get_all(Resource, Req) ->
    do_get_all(Resource, Req, true, []).
do_get_all(_Resource, _Req, false, Acc) ->
    {ok, fold_paginated(Acc)};
do_get_all(Resource, Req, true, Acc) ->
    case request(Req) of
        {ok, Resp} ->
            HasMore = has_more(Resp),
            case HasMore of
                true ->
                    NextURI = maps:get(<<"next">>, Resp),
                    Req1 = paginated_req(Resource, NextURI, Req),
                    do_get_all(Resource, Req1, HasMore, [Resp|Acc]);
                _ ->
                    do_get_all(Resource, Req, false, [Resp|Acc])
            end;
        {error, _} = Error ->
            Error
    end.

-spec fold_paginated([paginated_list()]) -> paginated_list().
fold_paginated(Objects) ->
    Objects0 = lists:reverse(Objects),
    Accl = lists:foldl(
             fun(X, #paginated_list{results=R} = Acc0) ->
                     N = maps:get(<<"next">>, X),
                     P = maps:get(<<"previous">>, X),
                     R1 = maps:get(<<"results">>, X),
                     Acc0#paginated_list{next=N, previous=P, results=[R1|R]}
             end, #paginated_list{}, Objects0),
    R2 = lists:flatten(Accl#paginated_list.results),
    Accl#paginated_list{results=lists:reverse(R2)}.

-spec paginated_req(atom(), url(), request()) -> request().
paginated_req(Resource, NextURI, Req) ->
    ParsedURI = upvest_utils:parse_uri(NextURI),
    MQuery = maps:from_list(ParsedURI#uri.query),
    URI = upvest:build_uri(Resource, MQuery),
    Req#request{uri=URI}.

has_more(M) ->
    maps:is_key(<<"next">>, M) andalso (maps:get(<<"next">>, M) =/= null).

-spec versioned_url(string()) -> string().
versioned_url(Path) ->
    ?API_VERSION ++ Path.

headers_with_uri(#request{headers=H, uri=U} = Req) ->
    AuthHeaders  = authenticate(Req),
    Headers = lists:flatten([AuthHeaders, H | ?DEFAULT_HEADERS]),
    FullUri = ?DEFAULT_BASE_URL ++ versioned_url(U),
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
    DecodedResult = maybe_get_key(<<"error">>, PreDecoded, <<"details">>),
    #upvest_error{type    = ErrCodeMeaning,
                  http_error_code = ErrCode,
                  message = ?V(message),
                  details   = ?V(details)}.

-spec maybe_get_key(term(), map(), term()) -> term().
maybe_get_key(Key, Map, Alternate) ->
    case catch(maps:get(Key, Map)) of
        {'EXIT', {{badkey, Key}, _}} -> maps:get(Alternate, Map);
        Val -> Val
    end.

-spec authenticate(request()) -> proplists:list().
authenticate(#request{auth = #keyauth{} = Cred} = Req) ->
    #request{method = Method, uri = Uri, body = Body} = Req,
    #keyauth{secret = Secret, key = Key, passphrase = Passphrase} = Cred,
    Timestamp = upvest_utils:timestamp(),
    VersionedPath = versioned_url(Uri),
    Method1 = string:uppercase(atom_to_list(Method)),
    EncodedBody = upvest_json:encode(Body),
    Message = io_lib:format("~w~s~s~s", [Timestamp, Method1, VersionedPath, EncodedBody]),
    Message1 = string:join(Message, ""),
    Signature = upvest_utils:generate_signature(Secret, Message1),
    [{<<"Content-Type">>, <<"application/json">>},
     {<<"X-UP-API-Key">>, Key},
     {<<"X-UP-API-Signature">>, Signature},
     {<<"X-UP-API-Timestamp">>,  Timestamp},
     {<<"X-UP-API-Passphrase">>,  Passphrase},
     {<<"X-UP-API-Signed-Path">>, VersionedPath}].
