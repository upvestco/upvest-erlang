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
         all_assets/2
         %% Clientele API
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
             "old_password" => to_bin(OldPassword),
             "new_password" => to_bin(NewPassword)
            },
    request(Cred, patch, Uri, Body).

-spec create_user(credentials(), string(), string()) -> result().
create_user(Cred, Username, Password) ->
    Uri = "/tenancy/users/",
    Body = #{
             <<"username">> => to_bin(Username),
             <<"password">> => to_bin(Password)
            },
    ?PRINT(Body),
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
%%% Internal functions
%%%===================================================================
request(Cred, Method, Uri) ->
    request(Cred, Method, Uri, #{}).
request(Cred, Method, Uri, Body) ->
    Req = #request{method=Method, uri=Uri, body=Body},
    Req1 = merge_req_client(Req, Cred),
    upvest_req:request(Req1).

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
    io_lib:format(Url, [to_str(Username)]);
build_uri(users, Params) ->
    Url = "/tenancy/users/",
    maybe_append_qs_params(Url, Params);
build_uri(asset, AssetID) ->
    Url = "/assets/~s",
    io_lib:format(Url, [to_str(AssetID)]);
build_uri(assets, Params) ->
    Url = "/assets/",
    maybe_append_qs_params(Url, Params);
build_uri(wallets, Params) ->
    Url = "/kms/wallets/",
    maybe_append_qs_params(Url, Params);
build_uri(transactions, Params) ->
    Url = "/kms/wallets/~s/transactions/",
    maybe_append_qs_params(Url, Params).

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

to_str(Arg) when is_binary(Arg) ->
    unicode:characters_to_list(Arg);
to_str(Arg) when is_atom(Arg) ->
    atom_to_list(Arg);
to_str(Arg) when is_integer(Arg) ->
    integer_to_list(Arg);
to_str(Arg) when is_list(Arg) ->
    Arg.

to_bin(Arg) when is_list(Arg) ->
    list_to_binary(Arg);
to_bin(Arg) when is_atom(Arg) ->
    atom_to_binary(Arg, latin1);
to_bin(Arg) when is_binary(Arg) ->
    Arg.
