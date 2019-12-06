%%%--------------------------------------------------------------------
%%% Macros
%%%--------------------------------------------------------------------
-define(APP, upvest).
-define(VSN_STR, "0.1.0").

-define(TEST_TIMEOUT, 60).

%% MaxPageSize is the maximum page size when retrieving list
-define(DEFAULT_PAGE_SIZE, 100).

%% timeout for HTTP requests
-define(HTTP_TIMEOUT, 10000).

%% default to playground environment
-define(DEFAULT_BASE_URL, "https://api.playground.upvest.co/").

-define(USER_AGENT, "upvest-erlang/" ++ ?VSN_STR).

-define(DEFAULT_REQUEST_HEADERS,
        [{<<"User-Agent">>, ?USER_AGENT},
         {<<"Content-Type">>, <<"application/json;charset=utf8">>},
         {<<"Accept">>, <<"application/json">>}
        ]).

-define(NRAPI, <<"Not Returned by API">>).
-define(V(X), maps:get(atom_to_binary(X, utf8), DecodedResult, ?NRAPI)).

%% APIVersion is the currently supported API version
-define(API_VERSION, "1.0").

%% Encoding is the text encoding to use
-define(ENCODING, "utf-8").

-define(record_to_list(Name, Record), lists:zip(record_info(fields, Name),tl(tuple_to_list(Record)))).

%% Easy to use macros for debugging/development
-define(PRINT(Var),
        case application:get_env(upvest, enable_logging, true) of
            true ->
                io:format(user, "DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var]);
            _ ->
                ok
        end
       ).

                                                % OAuth
-define(OAUTH_GRANT_TYPE, <<"password">>).
-define(OAUTH_SCOPE, <<"read write echo transaction">>).
-define(OAUTH_PATH, "/clientele/oauth2/token").

                                                % URLEncodeHeader is the content-type header for OuAth2
-define(URL_ENCODE_HEADER, <<"application/x-www-form-urlencoded">>).

%%%--------------------------------------------------------------------
%%% General Types
%%%--------------------------------------------------------------------
-type url() :: httpc:url().
-type method() :: get | post | put | delete.
-type headers() :: [header()].
-type header()  :: {binary(), binary()}.
-type json() :: jiffy:jiffy_decode_result().

-record(request, {
                  method :: atom(),
                  uri :: string() | binary(),
                  headers = [] ::  proplists:list(),
                  body = #{} :: string() | binary() | map(),
                  auth :: credentials(),
                  base_url = ?DEFAULT_BASE_URL :: string()
                 }).

-type request() :: #request{}.

%% historical data transaction filters
-record(txopts, {
          before :: string(),
          fter :: string(),
          confirmations :: pos_integer(),
          cursor :: string(),
          limit :: integer()
         }).

-type txopts() :: #txopts{}.

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

-type auth()  :: #keyauth{} | #oauth{}.

-record(credentials, {
                      auth :: auth(),
                      base_url = ?DEFAULT_BASE_URL :: string()
                     }).

-type credentials() :: #credentials{}.

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

-record(paginated_list, {previous :: url(), next :: url(), results = [] :: [upvest_object()]}).
-type paginated_list() :: #paginated_list{}.

-type result() :: {ok, binary() | upvest_object | paginated_list } | error().

-type upvest_object_name() :: user | asset | wallet | transaction.

-export_type([
              result/0,
              credentials/0,
              error/0
             ]).
