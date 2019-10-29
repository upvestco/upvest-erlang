%%% @author Yao Adzaku <yao@rpip>
%%% @copyright (C) 2019, Yao Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 12 Oct 2019 by Yao Adzaku <yao@rpip>

-module(upvest_tests).
-include_lib("eunit/include/eunit.hrl").
-include("upvest.hrl").

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
upvest_test_() ->
    {spawn,
     {setup,
      fun setup/0,
      fun teardown/1,
      [
       %% {"List users",
       %%  {timeout, 60, fun list_users/0}},
       %% {"Get user", fun get_user/0},
       {"Create user", {timeout, 60, fun create_user/0}}
      ]
     }
    }.

%%%-------------------------------------------------------------------
%%% Setup / Cleanup
%%%-------------------------------------------------------------------
setup() ->
    application:ensure_all_started(?APP),
    ok.

teardown(_) ->
    application:stop(?APP).


%%%-------------------------------------------------------------------
%%% users
%%%-------------------------------------------------------------------
create_user() ->
    Cred = keyauth_credentials(),
    Username = io_lib:format("upvest_test_~s", [random_chrs()]),
    Password = random_chrs(16),
    {ok, User} = upvest:create_user(Cred, Username, Password),
    Username1 = maps:get(<<"username">>, User),
    ?assertEqual(Username1, list_to_binary(Username)),
    upvest:delete_user(Cred, Username1).

list_users() ->
    Cred = keyauth_credentials(),

    {ok, Users} = upvest:get_users(Cred, 200),
    ?assert(length(Users#paginated_list.results) < 200),
    ?assert(is_list(Users#paginated_list.results)),

    {ok, AllUsers} = upvest:all_users(Cred),
    ?assert(is_list(AllUsers#paginated_list.results)).

get_user() ->
    Cred = keyauth_credentials(),
    {ok, Users} = upvest:get_users(Cred, 200),
    User = lists:nth(1, Users#paginated_list.results),
    {ok, User} = upvest:get_user(Cred, maps:get(<<"username">>, User)).

%%%-------------------------------------------------------------------
%%% Miscellaneous
%%%-------------------------------------------------------------------
keyauth_credentials() ->
    Key = os:getenv("API_KEY"),
    Secret = os:getenv("API_SECRET"),
    Passphrase = os:getenv("API_PASSPHRASE"),
    upvest:keyauth(Key, Secret, Passphrase).

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
