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
       {"List users",
        {timeout, 60, fun list_users/0}}
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

list_users() ->
    Key = os:getenv("API_KEY"),
    Secret = os:getenv("API_SECRET"),
    Passphrase = os:getenv("API_PASSPHRASE"),
    Client = upvest:keyauth_client(Key, Secret, Passphrase),
    {ok, Users} = upvest:get_users(Client, 200),
    {ok, AllUsers} = upvest:all_users(Client),
    ?PRINT(Users).
