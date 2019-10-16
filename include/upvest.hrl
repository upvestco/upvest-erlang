%%%--------------------------------------------------------------------
%%% Macros
%%%--------------------------------------------------------------------
-define(APP, upvest).
-define(VSN_STR, "0.1.0").

%% MaxPageSize is the maximum page size when retrieving list
-define(DEFAULT_PAGE_SIZE, 100).

%%timeout for HTTP requests
-define(HTTP_TIMEOUT, 10000).

%% default to playground environment
-define(DEFAULT_BASE_URL, "https://api.playground.upvest.co/").

-define(USER_AGENT, "upvest-erlang/" ++ ?VSN_STR).

-define(DEFAULT_HEADERS, [{<<"User-Agent">>, ?USER_AGENT},
                          {<<"Content-Type">>, <<"application/json; charset=utf8">>},
                          {<<"Accept">>, <<"application/json">>}
                         ]).

-define(NRAPI, <<"Not Returned by API">>).
-define(V(X), maps:get(atom_to_binary(X, utf8), DecodedResult, ?NRAPI)).
                                                %-define(PAGINATED(M), maps:update_with("page_size", fun(X) -> X end, ?DEFAULT_PAGE_SIZE, M)).

%% APIVersion is the currently supported API version
-define(API_VERSION, "1.0").

%% Encoding is the text encoding to use
-define(ENCODING, "utf-8").

-define(record_to_list(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).

                                                %Easy to use macros for debugging/development
-define(PRINT(Var),
        case application:get_env(upvest, enable_logging, true) of
            true ->
                io:format(user, "DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var]);
            _ ->
                ok
        end
       ).

%%%--------------------------------------------------------------------
%%% General Types
%%%--------------------------------------------------------------------
-type url() :: httpc:url().
-type method() :: get | post | put | delete.
-type headers() :: [header()].
-type header()  :: {binary(), binary()}.
-type options()  :: map().
-type request() :: request().
-type json() :: jiffy:jiffy_decode_result().

-record(request, {
          method :: atom(),
          uri :: string() | binary(),
          headers = [] ::  proplists:list(),
          body = #{} :: string() | binary() | map()
         }).

%% OAuth (The OAuth2 Key Authentication) is used to authenticate requests on behalf of a user
-record(oauth, {
          client_id:: string(),
          client_secret :: string(),
          username :: string(),
          password :: string()
         }).

%% keyauth (The API Key Authentication) is used to authenticate requests as a tenant.
-record(keyauth, {
          key :: string(),
          secret   ::  string(),
          passphrase :: string()
         }).

-type credentials()  :: #keyauth{} | #oauth{}.

-record(client, {
          credentials :: credentials(),
          base_url = ?DEFAULT_BASE_URL :: string(),
          headers = ?DEFAULT_HEADERS :: headers(),
          options = [] :: proplists:list()
         }).

-type client() :: #client{}.

-record(uri, {
          scheme :: string() | binary(),
          userinfo :: string() | binary(),
          host :: string() | binary(),
          port :: pos_integer(),
          path :: string() | binary(),
          query :: map()
         }).

%%%--------------------------------------------------------------------
%%% Error and Results types
%%%--------------------------------------------------------------------
-record(upvest_error, {
          type    :: api_error | invalid_request_error
                   | authentication_error | connection_error
                   | permission_error,
          message :: string(),
          details :: map(),
          http_error_code :: 400..500
         }).

-type error() :: {error, #upvest_error{} | term()}.

%%%--------------------------------------------------------------------
%%% Records / Upvest Objects
%%%--------------------------------------------------------------------
-type wallet_address() :: string().
-type metadata() :: map().

-record(upvest_asset, {
          id :: string(),
          name :: string(),
          symbol :: string(),
          exponent :: pos_integer(),
          protocol :: pos_integer(),
          metadata :: metadata()
         }).

-record(wallet_balance, {
          amount   :: pos_integer(),
          asset_id :: string(),
          name :: string(),
          symbol :: string(),
          exponent :: pos_integer()
         }).

-record(upvest_wallet, {
          id :: string(),
          path :: string(),
          balances :: [#wallet_balance{}],
          protocol :: string(),
          address :: wallet_address(),
          status :: string(),
          index :: pos_integer()
         }).

-record(upvest_transaction, {
          id :: string(),
          tx_hash :: string(),
          wallet_id :: string(),
          asset_id :: string(),
          asset_name :: string(),
          exponent :: string(),
          sender :: string(),
          recipient :: string(),
          quantity :: string(),
          fee :: string(),
          status :: string()
         }).

-record(upvest_user, {
          username :: string(),
          recovery_kit :: binary(),
          wallets :: [#upvest_wallet{}],
          wallet_ids :: [string()]
         }).


-type upvest_object() :: #upvest_user{} | #upvest_asset{} | #upvest_wallet{} | #upvest_transaction{}.

-record(upvest_list, {previous :: url(), next :: url(), results = [] :: [upvest_object()]}).

-type result() :: {ok, binary() | upvest_object | upvest_list } | error().

-type upvest_object_name() :: user | asset | wallet | transaction.

-export_type([
              result/0,
              client/0,
              error/0,
              options/0
             ]).
