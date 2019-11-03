%%% @author Yao Adzaku <yao.adzaku@gmail.com>
%%% @copyright (C) 2019, Yao Adzaku
%% @doc Main module that implements the functions to interact with the Upvest API.
%%
%% @end
%%% Created : 18 Sep 2019 by Yao Adzaku <yao.adzaku@gmail.com>
%%%-------------------------------------------------------------------
-module(upvest).

-include("upvest.hrl").

-export([
         build_uri/2,
         %% Auth Credentials
         keyauth/3,
         oauth/4,

         %% Tenancy API
         get_user/2,
         get_users/2,
         all_users/1,
         all_users/2,
         create_user/3,
         change_user_password/4,
         delete_user/2,

         get_asset/2,
         get_assets/2,
         get_assets/3,
         all_assets/1,
         all_assets/2,

         %% Clientele API
         get_wallet/2,
         get_wallets/2,
         get_wallets/3,
         all_wallets/1,
         all_wallets/2,
         create_wallet/3,
         create_wallet/4,
         create_wallet/5,
         sign_wallet/4,
         sign_wallet/5,
         sign_wallet/6
        ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Takes an api key, secret and a passphrase.
%%      Returns a value that can be used for API key-based authentication.
%% @end
-spec keyauth(string(), string(), string()) -> credentials().

keyauth(Key, Secret, Passphrase) ->
    #credentials{auth = #keyauth{key = Key, secret = Secret, passphrase = Passphrase}}.

%% @doc Takes a client ID, client secret, username and a password.
%%      Returns a client with credentials that can be used for OAuth authentication.
%% @end
-spec oauth(string(), string(), string(), string()) -> credentials().
oauth(ClientID, ClientSecret, Username, Password) ->
    #credentials{auth = #oauth{client_id = ClientID,
                               client_secret = ClientSecret,
                               username = Username,
                               password = Password}}.

%%%--------------------------------------------------------------------
%%% User Management
%%%--------------------------------------------------------------------
-spec get_users(credentials(), pos_integer()) -> result().
get_users(Cred, Limit) ->
    get_users(Cred, Limit, #{}).

-spec get_users(credentials(), pos_integer(), options()) -> result().
get_users(Cred, Limit, Opts) ->
    Uri = build_uri(users, paginated(Opts)),
    request_all(Cred, users, get, Uri, Limit).

-spec all_users(credentials()) -> result().
all_users(Cred) ->
    all_users(Cred, paginated(#{})).

-spec all_users(credentials(), options()) -> result().
all_users(Cred, Opts) ->
    Uri = build_uri(users, Opts),
    request_all(Cred, users, get, Uri).

-spec get_user(credentials(), string()) -> result().
get_user(Cred, Username) ->
    Uri = build_uri(user, Username),
    request(Cred, get, Uri).

-spec change_user_password(credentials(), string(), string(), string()) -> result().
change_user_password(Cred, Username, OldPassword, NewPassword) ->
    Uri = build_uri(user, Username),
    Body = #{
             "old_password" => upvest_utils:to_bin(OldPassword),
             "new_password" => upvest_utils:to_bin(NewPassword)
            },
    request(Cred, patch, Uri, Body).

-spec create_user(credentials(), string(), string()) -> result().
create_user(Cred, Username, Password) ->
    Uri = "/tenancy/users/",
    Body = #{
             <<"username">> => upvest_utils:to_bin(Username),
             <<"password">> => upvest_utils:to_bin(Password)
            },
    request(Cred, post, Uri, Body).

-spec delete_user(credentials(), string()) -> result().
delete_user(Cred, Username) ->
    Uri = build_uri(user, Username),
    request(Cred, delete, Uri).


%%%--------------------------------------------------------------------
%%% Asset Management
%%%--------------------------------------------------------------------
-spec get_assets(credentials(), pos_integer()) -> result().
get_assets(Cred, Limit) ->
    get_assets(Cred, Limit, #{}).

-spec get_assets(credentials(), pos_integer(), options()) -> result().
get_assets(Cred, Limit, Opts) ->
    Uri = build_uri(assets, paginated(Opts)),
    request_all(Cred, assets, get, Uri, Limit).

-spec all_assets(credentials()) -> result().
all_assets(Cred) ->
    all_assets(Cred, paginated(#{})).

-spec all_assets(credentials(), options()) -> result().
all_assets(Cred, Opts) ->
    Uri = build_uri(assets, Opts),
    request_all(Cred, assets, get, Uri).

-spec get_asset(credentials(), string()) -> result().
get_asset(Cred, AssetID) ->
    Uri = build_uri(asset, AssetID),
    request(Cred, get, Uri).

%%%===================================================================
%%% Wallet Management
%%%===================================================================
-spec get_wallets(credentials(), pos_integer()) -> result().
get_wallets(Cred, Limit) ->
    get_wallets(Cred, Limit, #{}).

-spec get_wallets(credentials(), pos_integer(), options()) -> result().
get_wallets(Cred, Limit, Opts) ->
    Uri = build_uri(wallets, paginated(Opts)),
    request_all(Cred, wallets, get, Uri, Limit).

-spec all_wallets(credentials()) -> result().
all_wallets(Cred) ->
    all_wallets(Cred, paginated(#{})).

-spec all_wallets(credentials(), options()) -> result().
all_wallets(Cred, Opts) ->
    Uri = build_uri(wallets, Opts),
    request_all(Cred, wallets, get, Uri).

-spec get_wallet(credentials(), string()) -> result().
get_wallet(Cred, Username) ->
    Uri = build_uri(wallet, Username),
    request(Cred, get, Uri).

%% @doc Takes valid credentials, a string representing a user's password
%%      and the asset ID.
%%      Returns <code>{ok, Wallet}</code> where <code>Wallet</code> is the
%%      decoded JSON representation of Upvest's response.
%% @end
-spec create_wallet(credentials(), binary(), binary()) -> result().
create_wallet(Cred, Password, AssetID) ->
    Uri = build_uri(wallets, #{}),
    Body = #{
      <<"password">> => upvest_utils:to_bin(Password),
      <<"asset_id">> => upvest_utils:to_bin(AssetID)
     },
    request(Cred, post, Uri, Body).

%% @doc Takes valid credentials, a string representing a user's password,
%%      the asset ID and wallet type.
%%      Returns <code>{ok, Wallet}</code> where <code>Wallet</code> is the
%%      decoded JSON representation of Upvest's response.
%% @end
-spec create_wallet(credentials(), binary(), binary(), binary()) -> result().
create_wallet(Cred, Password, AssetID, Type) ->
    Uri = build_uri(wallets, #{}),
    Body = #{
      <<"password">> => upvest_utils:to_bin(Password),
      <<"asset_id">> => upvest_utils:to_bin(AssetID),
      <<"type">> => upvest_utils:to_bin(Type)
     },
    request(Cred, post, Uri, Body).

%% @doc Takes valid credentials, a string representing a user's password,
%%      the asset ID, wallet type and the index type.
%%      Returns <code>{ok, Wallet}</code> where <code>Wallet</code> is the
%%      decoded JSON representation of Upvest's response.
%% @end
-spec create_wallet(credentials(), binary(), binary(), binary(), binary()) -> result().
create_wallet(Cred, Password, AssetID, Type, Index) ->
    Uri = build_uri(wallets, #{}),
    Body = #{
      <<"password">> => upvest_utils:to_bin(Password),
      <<"asset_id">> => upvest_utils:to_bin(AssetID),
      <<"type">> => upvest_utils:to_bin(Type),
      <<"index">> => upvest_utils:to_bin(Index)
     },
    request(Cred, post, Uri, Body).

-spec sign_wallet(credentials(), binary(), binary(), binary()) -> result().
sign_wallet(Cred, WalletID, Password, ToSign) ->
    Uri = build_uri(sign_wallets, WalletID),
    Body = #{
      <<"wallets">> => upvest_utils:to_bin(WalletID),
      <<"password">> => upvest_utils:to_bin(Password),
      <<"to_sign">> => upvest_utils:to_bin(ToSign)
     },
    request(Cred, post, Uri, Body).

-spec sign_wallet(credentials(), binary(), binary(), binary(), binary()) -> result().
sign_wallet(Cred, WalletID, Password, ToSign, InputFormat) ->
    Uri = build_uri(sign_wallets, WalletID),
    Body = #{
      <<"wallets">> => upvest_utils:to_bin(WalletID),
      <<"password">> => upvest_utils:to_bin(Password),
      <<"to_sign">> => upvest_utils:to_bin(ToSign),
      <<"input_format">> => upvest_utils:to_bin(InputFormat)
     },
    request(Cred, post, Uri, Body).

-spec sign_wallet(credentials(), binary(), binary(), binary(), binary(), binary()) -> result().
sign_wallet(Cred, WalletID, Password, ToSign, InputFormat, OutputFormat) ->
    Uri = build_uri(sign_wallets, WalletID),
    Body = #{
      <<"wallets">> => upvest_utils:to_bin(WalletID),
      <<"password">> => upvest_utils:to_bin(Password),
      <<"to_sign">> => upvest_utils:to_bin(ToSign),
      <<"input_format">> => upvest_utils:to_bin(InputFormat),
      <<"output_format">> => upvest_utils:to_bin(OutputFormat)
     },
    request(Cred, post, Uri, Body).

%%%===================================================================
%%% Internal functions
%%%===================================================================
request(Cred, Method, Uri) ->
    request(Cred, Method, Uri, #{}).
request(Cred, Method, Uri, Body) ->
    Req = #request{method=Method, uri=Uri, body=Body},
    Req1 = merge_req_client(Req, Cred),
    upvest_req:run(Req1).

request_all(Cred, Resource, get, Uri, Limit) ->
    Req = #request{method=get, uri=Uri},
    Req1 = merge_req_client(Req, Cred),
    upvest_req:get_some(Resource, Limit, Req1).

request_all(Cred, Resource, get, Uri) ->
    Req = #request{method=get, uri=Uri},
    Req1 = merge_req_client(Req, Cred),
    upvest_req:get_all(Resource, Req1).

%% merge the user-configured client configs into the request object
merge_req_client(Req, Cred) ->
    Req#request{
      auth = Cred#credentials.auth,
      base_url = Cred#credentials.base_url
     }.

build_uri(user, Username) ->
    Url = "/tenancy/users/~s",
    io_lib:format(Url, [upvest_utils:to_str(Username)]);
build_uri(users, Params) ->
    Url = "/tenancy/users/",
    maybe_append_qs_params(Url, Params);
build_uri(asset, AssetID) ->
    Url = "/assets/~s",
    io_lib:format(Url, [upvest_utils:to_str(AssetID)]);
build_uri(assets, Params) ->
    Url = "/assets/",
    maybe_append_qs_params(Url, Params);
build_uri(transactions, Params) ->
    Url = "/kms/wallets/~s/transactions/",
    maybe_append_qs_params(Url, Params);

build_uri(wallet, WalletID) ->
    Url = "/kms/wallets/~s",
    io_lib:format(Url, [upvest_utils:to_str(WalletID)]);
build_uri(wallets, Params) ->
    Url = "/kms/wallets/",
    maybe_append_qs_params(Url, Params);
build_uri(sign_wallet, WalletID) ->
    Url = "/kms/wallets/%s/sign",
    io_lib:format(Url, [upvest_utils:to_str(WalletID)]).

maybe_append_qs_params(Url, Params) ->
    case maps:size(Params) > 0 of
        false -> io_lib:format(Url, []);
        true ->
            QS = format_qs_params(maps:to_list(Params), []),
            [lists:flatten(io_lib:format("~s?~s", [Url, QS]))]
    end.

format_qs_params([], Acc) ->
    lists:flatten(string:join(lists:reverse(Acc), "&"));
format_qs_params([{_K, ""} | Params], Acc) ->
    format_qs_params(Params, Acc);
format_qs_params([{K, V} | Params], Acc) ->
    format_qs_params(Params, [format_qs_param(K, V) | Acc]).

format_qs_param(K, V) when is_number(V) ->
    io_lib:format("~s=~p", [K, V]);
format_qs_param(K, V) -> io_lib:format("~s=~s", [K, V]).

paginated(M) when is_map(M) ->
    maps:update_with("page_size", fun(X) -> X end, ?DEFAULT_PAGE_SIZE, M);
paginated(Count) ->
    #{"page_size" => Count}.

-spec to_record(upvest_object_name(), proplists:list()) -> upvest_object().
to_record(user, DecodedResult) ->
    #upvest_user{
       username = ?V(username),
       recovery_kit = ?V(recovery_kit),
       wallet_ids = ?V(wallet_ids),
       wallets = ?V(wallets)
      };

to_record(asset, DecodedResult) ->
    #upvest_asset{
       id = ?V(id),
       name = ?V(name),
       symbol = ?V(symbol),
       exponent = ?V(exponent),
       protocol = ?V(protocol),
       metadata = ?V(metadata)
      };
to_record(balance, DecodedResult) ->
    #wallet_balance{
       amount = ?V(amount),
       asset_id= ?V(asset_id),
       name = ?V(name),
       symbol = ?V(symbol),
       exponent = ?V(exponent)
      };
to_record(wallet, DecodedResult) ->
    #upvest_wallet{
       id = ?V(id),
       path = ?V(path),
       balances = ?V(balances),
       protocol = ?V(protocol),
       address  = ?V(address),
       status = ?V(status),
       index = ?V(index)
      };
to_record(transaction, DecodedResult) ->
    #upvest_transaction{
       id = ?V(id),
       tx_hash = ?V(tx_hash),
       wallet_id = ?V(asset_id),
       asset_id = ?V(asset_id),
       asset_name = ?V(asset_name),
       exponent = ?V(exponent),
       sender = ?V(sender),
       recipient = ?V(recipient),
       quantity = ?V(quantity),
       fee = ?V(fee),
       status = ?V(status)
      }.

-spec to_records(paginated_list, upvest_object_name()) -> paginated_list().
to_records(#paginated_list{} = L, Schema) ->
    L#paginated_list{
      results = [upvest_json:to_record(Schema, Object) || Object <- L#paginated_list.results]
     }.
