%%% @author Yao Adzaku <yao@rpip>
%%% @copyright (C) 2019, Yao Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2019 by Yao Adzaku <yao@rpip>

-module(upvest_req).
-include("upvest.hrl").
-export([
         get_all/2,
         get_some/3,
         run/1
        ]).


-spec run(request()) -> result().
run(Req) ->
    {Headers, RequestUrl} = build_headers(Req),
    Req1 = Req#request{headers=Headers, uri=RequestUrl},
    #request{
       method = Method,
       uri = Uri,
       headers = Headers,
       body = Body
      } = Req1,
    run(Method, Uri, Headers, Body).

run(Method, Uri, Headers, Body) ->
    do_run(Method, Uri, Headers, Body).

do_run(Method, Uri, Headers, Body) ->
    Payload = encode(content_type(Headers), Body),
    Opts = [{recv_timeout, 10000}],
    case hackney:Method(Uri, Headers, Payload, Opts) of
        %% delete endpoint returns 204 No Content
        {ok, Status, _RespHeaders, _ClientRef} when Status =:= 204 ->
            {ok, no_content};
        {ok, Status, _RespHeaders, ClientRef} when Status >= 200 andalso Status < 300 ->
            {ok, Body1} = hackney:body(ClientRef),
            DecodedBody = decode(Body1),
            {ok, DecodedBody};
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
    case run(Req) of
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
    case run(Req) of
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

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
content_type([{<<"Content-Type">>, ?URL_ENCODE_HEADER}|_] = _Headers) ->
    urlencode;

content_type(_Headers) ->
    json.

-spec encode(atom(), term()) -> binary().
encode(urlencode, Params) ->
    hackney_url:qs(Params);

encode(_, Params) ->
    jiffy:encode(Params).

-spec decode(binary()) -> json().
decode(Json) ->
    jiffy:decode(Json, [return_maps]).

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

build_headers(Req) ->
    #request{headers=Headers, uri=U, auth = Auth, base_url = BaseURL} = Req,
    [AuthType|_] = tuple_to_list(Auth),
    AuthHeaders  = authenticate(AuthType, Req),
    Headers1 = lists:flatten([AuthHeaders, Headers | ?DEFAULT_REQUEST_HEADERS]),
    FullURL = BaseURL ++ versioned_url(U),
    {Headers1, FullURL}.

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
    PreDecoded = decode(Body),
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

-spec authenticate(atom(), request()) -> proplists:list().
authenticate(keyauth, #request{auth = Cred} = Req) ->
    #request{method = Method, uri = Uri, body = Body, headers = Headers} = Req,
    #keyauth{secret = Secret, key = Key, passphrase = Passphrase} = Cred,
    Timestamp = upvest_utils:timestamp(),
    VersionedPath = versioned_url(Uri),
    Method1 = string:uppercase(atom_to_list(Method)),
    EncodedBody = encode(content_type(Headers), Body),
    Message = io_lib:format("~w~s~s~s", [Timestamp, Method1, VersionedPath, EncodedBody]),
    Message1 = string:join(Message, ""),
    Signature = upvest_utils:generate_signature(Secret, Message1),
    [{<<"Content-Type">>, <<"application/json">>},
     {<<"X-UP-API-Key">>, Key},
     {<<"X-UP-API-Signature">>, Signature},
     {<<"X-UP-API-Timestamp">>,  Timestamp},
     {<<"X-UP-API-Passphrase">>,  Passphrase},
     {<<"X-UP-API-Signed-Path">>, VersionedPath}];

authenticate(oauth, #request{auth = Cred, base_url = BaseURL} = _Req) ->
    %% preflight request: get access token
    #oauth{
       client_id = ClientID,
       client_secret = ClientSecret,
       username = Username,
       password = Password } = Cred,
    Headers = [{<<"Content-Type">>, ?URL_ENCODE_HEADER},
               {<<"Cache-Control">>, <<"no-cache">>}],
    Payload = [{<<"grant_type">>, ?OAUTH_GRANT_TYPE},
               {<<"scope">>, ?OAUTH_SCOPE},
               {<<"client_id">>, upvest_utils:to_bin(ClientID)},
               {<<"client_secret">>, upvest_utils:to_bin(ClientSecret)},
               {<<"username">>, upvest_utils:to_bin(Username)},
               {<<"password">>, upvest_utils:to_bin(Password)}],
    FullURL = BaseURL ++ versioned_url(?OAUTH_PATH),
    {ok, Resp} = run(post, FullURL, Headers, Payload),
    Bearer = io_lib:format("Bearer ~s", [maps:get(<<"access_token">>, Resp)]),
    %% now return headers with access token for actual
    [{<<"Authorization">>, list_to_binary(Bearer)}].
