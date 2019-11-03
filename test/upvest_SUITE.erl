%%% @author Yao Adzaku <yao@rpip>
%%% @copyright (C) 2019, Yao Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 12 Oct 2019 by Yao Adzaku <yao@rpip>

-module(upvest_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("upvest.hrl").
-compile(export_all).

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
all() -> [{group, tenancy_tests}, {group, clientele_tests}].

groups() ->
    [{tenancy_tests,
      [parallel],
      [all_users, list_users, get_user, create_delete_user, all_assets, list_assets, get_asset]
     },
     {clientele_tests,
      [parallel],
      [all_wallets, list_wallets, get_wallet]
     }].

init_per_suite(Config) ->
    application:ensure_all_started(?APP),
    [{keyauth, keyauth_credentials()}, {oauth, oauth_credentials()}|Config].

end_per_suite(_Config) ->
    application:stop(?APP),
    ok.


%%%-------------------------------------------------------------------
%%% users
%%%-------------------------------------------------------------------
all_users(Config) ->
    Cred = ?config(keyauth, Config),
    ?PRINT(Cred),
    {ok, AllUsers} = upvest:all_users(Cred),
    ?assert(is_list(AllUsers#paginated_list.results)).

list_users(Config) ->
    Cred = ?config(keyauth, Config),
    {ok, Users} = upvest:get_users(Cred, 200),
    ?assert(length(Users#paginated_list.results) < 200),
    ?assert(is_list(Users#paginated_list.results)).

get_user(Config) ->
    Cred = ?config(keyauth, Config),
    {ok, Users} = upvest:get_users(Cred, 200),
    [User|_Rest] = Users#paginated_list.results,
    Username = maps:get(<<"username">>, User),
    {ok, User1} = upvest:get_user(Cred, Username),
    ?assertMatch(User, User1).

create_delete_user(Config) ->
    Cred = ?config(keyauth, Config),
    Username = io_lib:format("upvest_test_~s", [random_chrs()]),
    Password = random_chrs(16),
    {ok, User} = upvest:create_user(Cred, Username, Password),
    Username1 = maps:get(<<"username">>, User),
    ?assertEqual(Username1, list_to_binary(Username)),
    upvest:delete_user(Cred, Username1).

%%%-------------------------------------------------------------------
%%% Assets
%%%-------------------------------------------------------------------
all_assets(Config) ->
    Cred = ?config(keyauth, Config),
    {ok, Assets} = upvest:get_assets(Cred, 200),
    ?assert(length(Assets#paginated_list.results) < 200),
    ?assert(is_list(Assets#paginated_list.results)).

list_assets(Config) ->
    Cred = ?config(keyauth, Config),
    {ok, AllAssets} = upvest:all_assets(Cred),
    ?assert(is_list(AllAssets#paginated_list.results)).

get_asset(Config) ->
    Cred = ?config(keyauth, Config),
    {ok, Assets} = upvest:get_assets(Cred, 200),
    [Asset|_Rest] = Assets#paginated_list.results,
    %% use pattern match to assert match
    {ok, Asset} = upvest:get_asset(Cred, maps:get(<<"id">>, Asset)).

%%%-------------------------------------------------------------------
%%% Wallets
%%%-------------------------------------------------------------------
all_wallets(Config) ->
    Cred = ?config(oauth, Config),
    {ok, AllWallets} = upvest:all_wallets(Cred),
    ?assert(is_list(AllWallets#paginated_list.results)).

list_wallets(Config) ->
    Cred = ?config(oauth, Config),
    {ok, Wallets} = upvest:get_wallets(Cred, 200),
    ?assert(length(Wallets#paginated_list.results) < 200),
    ?assert(is_list(Wallets#paginated_list.results)).

get_wallet(Config) ->
    Cred = ?config(oauth, Config),
    {ok, Wallets} = upvest:get_wallets(Cred, 200),
    [Wallet|_Rest] = Wallets#paginated_list.results,
    %% use pattern match to assert match
    {ok, Wallet} = upvest:get_wallet(Cred, maps:get(<<"id">>, Wallet)).

%%%-------------------------------------------------------------------
%%% Miscellaneous
%%%-------------------------------------------------------------------
keyauth_credentials() ->
    Key = os:getenv("API_KEY"),
    Secret = os:getenv("API_SECRET"),
    Passphrase = os:getenv("API_PASSPHRASE"),
    upvest:keyauth(Key, Secret, Passphrase).

oauth_credentials() ->
    ClientID = os:getenv("OAUTH2_CLIENT_ID"),
    ClientSecret = os:getenv("OAUTH2_CLIENT_SECRET"),
    Username = os:getenv("UPVEST_TEST_USERNAME"),
    Password = os:getenv("UPVEST_TEST_PASSWORD"),
    upvest:oauth(ClientID, ClientSecret, Username, Password).

random_chrs() ->
    random_chrs(32).
random_chrs(Bytes) ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(Bytes))).

create_test_user() ->
    Cred = keyauth_credentials(),
    Username = io_lib:format("upvest_test_~s", [random_chrs()]),
    Password = random_chrs(16),
    {ok, User} = upvest:create_user(Cred, Username, Password),
    {ok, User, Password}.
