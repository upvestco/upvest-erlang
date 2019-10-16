%%% @author Yao Adzaku <yao.adzaku@gmail.com>
%%% @copyright (C) 2019, Yao Adzaku
%% @doc Main module that implements the functions to interact with the Upvest API.
%%
%% @end
%%% Created : 18 Sep 2019 by Yao Adzaku <yao.adzaku@gmail.com>
%%%-------------------------------------------------------------------
-module(upvest).

-include("upvest.hrl").

%% Auth Credentials
-export([
         keyauth_client/3,
         oauth_client/4
        ]).

%% Tenancy API
-export([
         get_users/2,
         get_users/3,
         all_users/1,
         all_users/2
        ]).

%% Clientele API

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Credentials

%% @doc Takes an api key, secret and a passphrase.
%%      Returns a value that can be used for API key-based authentication.
%% @end
-spec keyauth_client(string(), string(), string()) -> client().

keyauth_client(Key, Secret, Passphrase) ->
  #client{credentials = #keyauth{key = Key, secret = Secret, passphrase = Passphrase}}.

%% @doc Takes a client ID, client secret, username and a password.
%%      Returns a client with credentials that can be used for OAuth authentication.
%% @end
-spec oauth_client(string(), string(), string(),
                   string()) -> client().

oauth_client(ClientID, ClientSecret, Username, Password) ->
  #client{credentials = #oauth{client_id = ClientID,
                               client_secret = ClientSecret,
                               username = Username,
                               password = Password}}.

%%%--------------------------------------------------------------------
%%% User Management
%%%--------------------------------------------------------------------
get_users(Client, MaxCount) ->
  get_users(Client, MaxCount, #{}).
get_users(Client, MaxCount, Opts) ->
  Uri = build_uri(users, paginated(Opts)),
  Req = #request{method=get, uri=Uri},
  {ok, Users} = upvest_req:get_some(users, MaxCount, Req, Client),
  {ok, zap(Users, user)}.

-spec all_users(client()) -> result().
all_users(Client) ->
  all_users(Client, paginated(#{})).

-spec all_users(client(), options()) -> result().
all_users(Client, Opts) ->
  Uri = build_uri(users, Opts),
  Req = #request{method=get, uri=Uri},
  {ok, Users} = upvest_req:get_all(users, Req, Client),
  {ok, zap(Users, user)}.

%% user_update(UserID, Fields...)
%% user_get(UserID)
%% user_delete(UserID)

%%%--------------------------------------------------------------------
%%% Asset Management
%%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec zap(#upvest_list{}, upvest_object_name()) -> #upvest_list{}.
zap(#upvest_list{} = L, Schema) ->
  L#upvest_list{
    results = [upvest_json:to_record(Schema, Object) || Object <- L#upvest_list.results]
   }.

build_uri(users, Params) ->
  Url = "/tenancy/users/",
  maybe_append_qs_params(Url, Params);
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
