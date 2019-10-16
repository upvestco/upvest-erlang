%%% @author Yao Adzaku <yao@rpip>
%%% @copyright (C) 2019, Yao Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 14 Oct 2019 by Yao Adzaku <yao@rpip>

-module(upvest_json).
-include("upvest.hrl").
-export([
         encode/1,
         decode/1,
         to_record/2
        ]).

-spec encode(map()) -> binary().
encode(Data) ->
    jiffy:encode(Data).

-spec decode(binary()) -> json().
decode(Json) ->
    jiffy:decode(Json, [return_maps]).

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
