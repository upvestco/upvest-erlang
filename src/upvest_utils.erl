-module(upvest_utils).
-include("upvest.hrl").

-export([
         parse_uri/1,
         timestamp/0,
         generate_signature/2
        ]).

%% @doc Returns current timestamp in seconds
-spec timestamp() -> integer().
timestamp() ->
    os:system_time(seconds).

-spec generate_signature(string(), string()) -> string().
generate_signature(Secret, Message) ->
    Bin = crypto:hmac(sha512, Secret, Message),
    string:lowercase(binary_to_list(encode16(Bin))).

-spec encode16(binary()) -> <<_:_*16>>.
encode16(Data) ->
    << <<(hex(N div 16)), (hex(N rem 16))>> || <<N>> <= Data >>.

hex(N) when N < 10 ->
    N + $0;
hex(N) when N < 16 ->
    N - 10 + $a.

%% @doc Parse a URI
%%
%% Returns either the URI or an error term.
%%
%% @end
-spec parse_uri(string()) -> url() | {error, Reason :: term()}.
parse_uri(URI) ->
    case http_uri:parse(URI, [{scheme_defaults, http_uri:scheme_defaults()}]) of
        {ok, {Scheme, UserInfo, Host, Port, Path, Query}} ->
            #uri{
               scheme=Scheme,
               userinfo=UserInfo,
               host=Host,
               port=Port,
               path=parse_path(Path),
               query=parse_query(Query)
              };
        {error, Reason} ->
            {error, Reason}
    end.

-spec parse_path(string()) -> string().
parse_path(Path) ->
    P = string:split(Path, ?API_VERSION),
    lists:nth(2, P).

-spec parse_query(string()) -> proplists:list().
parse_query(Query) ->
    Query1 = string:split(lists:nth(2, string:split(Query, "?")), "&"),
    [list_to_tuple(string:split(X, "=")) || X <- Query1].
