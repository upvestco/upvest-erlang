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
      [all_wallets, list_wallets, get_wallet, create_wallet, sign_wallet,
       all_transactions, list_transactions, get_transaction,
       all_webhooks, list_webhooks, verify_webhook, crud_webhook
      ]
     }].

init_per_suite(Config) ->
    application:ensure_all_started(?APP),
    [{keyauth, keyauth_credentials()},
     {oauth, oauth_credentials()},
     {eth_ropsten_wallet, <<"8fc19cd0-8f50-4626-becb-c9e284d2315b">>},
     {eth_ropsten_tx, <<"ef48567e-3cb1-47a1-9469-16d38b190c10">>},
     {eth_ropsten_asset, <<"deaaa6bf-d944-57fa-8ec4-2dd45d1f5d3f">>},
     {password, os:getenv("UPVEST_TEST_PASSWORD")},
     %% webhook
     {webhook_url, os:getenv("WEBHOOK_URL")},
     {webhook_verification_url, os:getenv("WEBHOOK_VERIFICATION_URL")}
     |Config].

end_per_suite(_Config) ->
    application:stop(?APP),
    ok.


%%%-------------------------------------------------------------------
%%% users
%%%-------------------------------------------------------------------
all_users(Config) ->
    Cred = ?config(keyauth, Config),
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
    {ok, _Wallet} = upvest:get_wallet(Cred, maps:get(<<"id">>, Wallet)).

create_wallet(Config) ->
    Cred = ?config(oauth, Config),
    AssetID = ?config(eth_ropsten_asset, Config),
    Password = ?config(password, Config),
    {ok, _Wallet} = upvest:create_wallet(Cred, Password, AssetID).

sign_wallet(Config) ->
    Cred = ?config(oauth, Config),
    Password = ?config(password, Config),
    EthWalletID = ?config(eth_ropsten_wallet, Config),
    ToSign = random_chrs(256),
    upvest:sign_wallet(Cred, EthWalletID, Password, ToSign).

%%%-------------------------------------------------------------------
%%% Transactions
%%%-------------------------------------------------------------------
all_transactions(Config) ->
    Cred = ?config(oauth, Config),
    EthWalletID = ?config(eth_ropsten_wallet, Config),
    {ok, AllTxs} = upvest:all_transactions(Cred, EthWalletID),
    ?assert(is_list(AllTxs#paginated_list.results)).

list_transactions(Config) ->
    Cred = ?config(oauth, Config),
    EthWalletID = ?config(eth_ropsten_wallet, Config),
    {ok, AllTxs} = upvest:get_transactions(Cred, EthWalletID, 100),
    ?assert(is_list(AllTxs#paginated_list.results)),
    ?assert(length(AllTxs#paginated_list.results) < 200).

create_transaction(Config) ->
    Cred = ?config(oauth, Config),
    EthWalletID = ?config(eth_ropsten_wallet, Config),
    Password = ?config(password, Config),
    AssetID = ?config(eth_ropsten_asset, Config),
    Qty = 10000000000000000,
    Fee = 41180000000000,
    Recipient = "0xf9b44Ba370CAfc6a7AF77D0BDB0d50106823D91b",
    {ok, _Tx} = upvest:create_transaction(Cred, EthWalletID, Password, AssetID, Qty, Fee, Recipient).

get_transaction(Config) ->
    Cred = ?config(oauth, Config),
    EthWalletID = ?config(eth_ropsten_wallet, Config),
    EthTxID = ?config(eth_ropsten_tx, Config),
    {ok, Tx} = upvest:get_transaction(Cred, EthWalletID, EthTxID),
    %% pattern match TxID and returned TxID
    EthTxID = maps:get(<<"id">>, Tx).

%%%-------------------------------------------------------------------
%%% Webhooks
%%%-------------------------------------------------------------------
all_webhooks(Config) ->
    Cred = ?config(keyauth, Config),
    {ok, AllWebhooks} = upvest:all_webhooks(Cred),
    ?assert(is_list(AllWebhooks#paginated_list.results)).

list_webhooks(Config) ->
    Cred = ?config(keyauth, Config),
    {ok, Webhooks} = upvest:get_webhooks(Cred, 200),
    ?assert(length(Webhooks#paginated_list.results) < 200),
    ?assert(is_list(Webhooks#paginated_list.results)).

verify_webhook(Config) ->
    Cred = ?config(keyauth, Config),
    Url = ?config(webhook_verification_url, Config),
    {ok, _} = upvest:verify_webhook(Cred, Url).

crud_webhook(Config) ->
    Cred = ?config(keyauth, Config),
    Url = ?config(webhook_url, Config),
    Name = io_lib:format("test-webhook-~s", [random_chrs()]),
    Headers = #{<<"X-Test">> => <<"Hello world!">>},
    Version = <<"1.2">>,
    Status = <<"ACTIVE">>,
    EventFilters = [<<"upvest.wallet.created">>, <<"ropsten.block.*">>, <<"upvest.echo.post">>],
    HMACSecretKey = <<"abcdef">>,
    %% create webhook
    {ok, Webhook} = upvest:create_webhook(Cred, Url, Name, Headers, Version, Status, EventFilters, HMACSecretKey),
    WebhookID = maps:get(<<"id">>, Webhook),
    %% retrieve: use pattern match to assert match
    {ok, Webhook} = upvest:get_webhook(Cred, WebhookID),
    %% now delete the webhook
    {ok, _} = upvest:delete_webhook(Cred, WebhookID).

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
