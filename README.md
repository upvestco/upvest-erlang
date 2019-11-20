Upvest Erlang Client library
==============================

[![Build Status](https://travis-ci.org/rpip/upvest-erlang.svg?branch=master)](https://travis-ci.org/rpip/upvest-erlang)

In order to retrieve your API credentials for using this Go client, you'll need to [sign up with Upvest](https://login.upvest.co/sign-up).

Usage
-----

### Tenancy API - API Keys Authentication

The Upvest API uses the notion of _tenants_, which represent customers that build their platform upon the Upvest API. The end-users of the tenant (i.e. your customers), are referred to as _clients_. A tenant is able to manage their users directly (CRUD operations for the user instance) and is also able to initiate actions on the user's behalf (create wallets, send transactions).

The authentication via API keys and secret allows you to perform all tenant related operations.

```erlang
Cred = upvest:keyauth(Key, Secret, Passphrase).
> {ok, Users} = upvest:all_users(Cred).
{ok,{paginated_list,<<"http://api.playground.upvest.co/1.0/tenancy/users/?cursor=cj0xJnA9MjAxOS0wNy0zMSsxNiUzQTQwJTNBNDguMj"...>>,
                    null,
                    [#{<<"username">> =>
                           <<"upvest_test_9bfe02c3-b61e-11e9-82b2-42010a1400ca">>,
                       <<"wallets">> => []},
                     #{<<"username">> =>
                           <<"upvest_test_071d31bb-b61f-11e9-8871-42010a1400da">>,
                       <<"wallets">> => []},
                     #{<<"username">> =>
                           <<"upvest_test_c667e46c-b63e-11e9-b2a5-8c8590323ff7">>,
                       <<"wallets">> => []},
                     #{<<"username">> =>
                           <<"upvest_test_2d2272b5-b640-11e9-9f50-42010a140168">>,
                       <<"wallets">> => []},
                     #{<<"username">> =>
                           <<"upvest_test_360b7c6f-b640-11e9-bd10-42010a1400ae">>,
                       <<"wallets">> => []},
                     #{<<"username">> =>
                           <<"upvest_test_ba71f12b-b640-11e9-b4c4-42010a1400b4">>,
                       <<"wallets">> => []},
                     #{<<"username">> =>
                           <<"upvest_test_9ba5c7fa-b642-11e9-80e7-"...>>,
                       <<"wallets">> => []},
                     #{<<"username">> => <<"upvest_test_5da0202b-b645-11e9-8"...>>,
                       <<"wallets">> => []},
                     #{<<"username">> => <<"upvest_test_6d2a0c93-b645-11"...>>,
                       <<"wallets">> => []},
                     #{<<"username">> => <<"upvest_test_ddbf6717-b64"...>>,
                       <<"wallets">> => []},
                     #{<<"username">> => <<"upvest_test_f0112daa"...>>,
                       <<"wallets">> => []},
                     #{<<"username">> => <<"upvest_test_f032"...>>,
                       <<"wallets">> => []},
                     #{<<"username">> => <<"upvest_test_"...>>,<<"wallets">> => []},
                     #{<<"username">> => <<"upvest_t"...>>,<<"wallets">> => []},
                     #{<<"user"...>> => <<"upve"...>>,<<"wall"...>> => []},
                     #{<<...>> => <<...>>,...},
                     #{...}|...]}}


> {ok, User} = upvest:get_user(Cred, Username),
> .... 
```

### Clientele API - OAuth Authentication
The authentication via OAuth allows you to perform operations on behalf of your user.
For more information on the OAuth concept, please refer to our [documentation](https://doc.upvest.co/docs/oauth2-authentication).
Again, please retrieve your client credentials from the [Upvest account management](https://login.upvest.co/).

Next, create an Clientele credentials and your user authentication data in order to authenticate your API calls on behalf of a user:

``` erlang
> Cred = upvest:oauth(ClientID, ClientSecret, Username, Password).
> upvest:all_wallets(Cred).
{ok,{paginated_list,null,null,
                    [#{<<"address">> => null,
                       <<"balances">> =>
                           [#{<<"amount">> => <<"0">>,
                              <<"asset_id">> => <<"51bfa4b5-6499-5fe2-998b-5fb3c9403ac7">>,
                              <<"exponent">> => 12,
                              <<"name">> => <<"Arweave (internal testnet)">>,
                              <<"symbol">> => <<"AR">>}],
                       <<"id">> => <<"ff8f03b6-3d7d-4de5-b286-5a672b01296e">>,
                       <<"index">> => null,<<"protocol">> => <<"arweave_testnet">>,
                       <<"status">> => <<"PENDING">>},
                     #{<<"address">> =>
                           <<"0xC4A284E55Ab2F1C2fEb23A0bfC56FcA31B0c94A3">>,
                       <<"balances">> =>
                           [#{<<"amount">> => <<"0">>,
                              <<"asset_id">> => <<"deaaa6bf-d944-57fa-8ec4-2dd45d1f5d3f">>,
                              <<"exponent">> => 18,<<"name">> => <<"Ether (Ropsten)">>,
                              <<"symbol">> => <<"ETH">>},
                            #{<<"amount">> => <<"0">>,
                              <<"asset_id">> => <<"cfc59efb-3b21-5340-ae96-8cadb4ce31a8">>,
                              <<"exponent">> => 12,<<"name">> => <<"Example coin">>,
                              <<"symbol">> => <<"COIN">>}],
                       <<"id">> => <<"8fc19cd0-8f50-4626-becb-c9e284d2315b">>,
                       <<"index">> => null,
                       <<"protocol">> => <<"ethereum_ropsten">>,
                       <<"status">> => <<"ACTIVE">>},
                     #{<<"address">> =>
                           <<"0xF3e61E5a1FfA1e2f13Abe99A1Dfcd031e7F0b318">>,
                       <<"balances">> =>
                           [#{<<"amount">> => <<"0">>,
                              <<"asset_id">> => <<"cfc59efb-3b21-5340-ae96-8cadb4ce31a8">>,
                              <<"exponent">> => 12,<<"name">> => <<"Example coin">>,
                              <<"symbol">> => <<"COIN">>}],
                       <<"id">> => <<"08402e62-0cc6-4912-9513-0905a97f8cb6">>,
                       <<"index">> => 0,<<"protocol">> => <<"ethereum_ropsten">>,
                       <<"status">> => <<"ACTIVE">>}]}}
```

## Pagination

Paginated/list endpoints return the response wrapped in paginated_list record.

- previous: cursor to previous page/set of results
- next: cursor to next result set
- results = [list of objects]

## Development

1. Code must be well-formatted: `make fmt`
2. All types, structs and funcs should be documented.
3. Ensure that `make test` succeeds.
4. Set up config settings via environment variables:

    ```shell
    # Set your tenancy API key information here.
    export API_KEY=xxxx
    export API_SECRET=xxxx
    export API_PASSPHRASE=xxxx

    # Set your OAuth2 client information here.
    export OAUTH2_CLIENT_ID=xxxx
    export OAUTH2_CLIENT_SECRET=xxxx
    ```


## Test

    make test


The tests require valid Upvest crendetials, so you need to source them in your environment for the tests to complete successfully.

## More

For a comprehensive reference, check out the [Upvest documentation](https://doc.upvest.co).

For details on all the functionality in this library, see the [Generated project docs](./doc/upvest.md).

