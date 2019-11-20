

# Module upvest #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Main module that implements the functions to interact with the Upvest API.

Copyright (c) (C) 2019, Yao Adzaku

__Authors:__ Yao Adzaku ([`yao.adzaku@gmail.com`](mailto:yao.adzaku@gmail.com)).

<a name="description"></a>

## Description ##

<a name="types"></a>

## Data Types ##




### <a name="type-auth">auth()</a> ###


<pre><code>
auth() = #keyauth{key = string(), secret = string(), passphrase = string()} | #oauth{client_id = string(), client_secret = string(), username = string(), password = string()}
</code></pre>




### <a name="type-credentials">credentials()</a> ###


<pre><code>
credentials() = #credentials{auth = <a href="#type-auth">auth()</a>, base_url = string()}
</code></pre>




### <a name="type-error">error()</a> ###


<pre><code>
error() = {error, #upvest_error{type = api_error | invalid_request_error | authentication_error | connection_error | permission_error, message = string(), details = map(), http_error_code = 400..500} | term()}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = map()
</code></pre>




### <a name="type-result">result()</a> ###


<pre><code>
result() = {ok, binary() | upvest_object | paginated_list} | <a href="#type-error">error()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_assets-1">all_assets/1</a></td><td></td></tr><tr><td valign="top"><a href="#all_assets-2">all_assets/2</a></td><td></td></tr><tr><td valign="top"><a href="#all_transactions-2">all_transactions/2</a></td><td></td></tr><tr><td valign="top"><a href="#all_transactions-3">all_transactions/3</a></td><td></td></tr><tr><td valign="top"><a href="#all_users-1">all_users/1</a></td><td></td></tr><tr><td valign="top"><a href="#all_users-2">all_users/2</a></td><td></td></tr><tr><td valign="top"><a href="#all_wallets-1">all_wallets/1</a></td><td></td></tr><tr><td valign="top"><a href="#all_wallets-2">all_wallets/2</a></td><td></td></tr><tr><td valign="top"><a href="#build_uri-2">build_uri/2</a></td><td></td></tr><tr><td valign="top"><a href="#change_user_password-4">change_user_password/4</a></td><td></td></tr><tr><td valign="top"><a href="#create_transaction-7">create_transaction/7</a></td><td></td></tr><tr><td valign="top"><a href="#create_user-3">create_user/3</a></td><td></td></tr><tr><td valign="top"><a href="#create_wallet-3">create_wallet/3</a></td><td>Takes valid credentials, a string representing a user's password
and the asset ID.</td></tr><tr><td valign="top"><a href="#create_wallet-4">create_wallet/4</a></td><td>Takes valid credentials, a string representing a user's password,
the asset ID and wallet type.</td></tr><tr><td valign="top"><a href="#create_wallet-5">create_wallet/5</a></td><td>Takes valid credentials, a string representing a user's password,
the asset ID, wallet type and the index type.</td></tr><tr><td valign="top"><a href="#delete_user-2">delete_user/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_asset-2">get_asset/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_assets-2">get_assets/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_assets-3">get_assets/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_transaction-3">get_transaction/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_transactions-3">get_transactions/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_transactions-4">get_transactions/4</a></td><td></td></tr><tr><td valign="top"><a href="#get_user-2">get_user/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_users-2">get_users/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_wallet-2">get_wallet/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_wallets-2">get_wallets/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_wallets-3">get_wallets/3</a></td><td></td></tr><tr><td valign="top"><a href="#keyauth-3">keyauth/3</a></td><td>Takes an api key, secret and a passphrase.</td></tr><tr><td valign="top"><a href="#oauth-4">oauth/4</a></td><td>Takes a client ID, client secret, username and a password.</td></tr><tr><td valign="top"><a href="#sign_wallet-4">sign_wallet/4</a></td><td></td></tr><tr><td valign="top"><a href="#sign_wallet-5">sign_wallet/5</a></td><td></td></tr><tr><td valign="top"><a href="#sign_wallet-6">sign_wallet/6</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_assets-1"></a>

### all_assets/1 ###

<pre><code>
all_assets(Cred::<a href="#type-credentials">credentials()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="all_assets-2"></a>

### all_assets/2 ###

<pre><code>
all_assets(Cred::<a href="#type-credentials">credentials()</a>, Opts::<a href="#type-options">options()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="all_transactions-2"></a>

### all_transactions/2 ###

<pre><code>
all_transactions(Cred::<a href="#type-credentials">credentials()</a>, WalletID::binary()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="all_transactions-3"></a>

### all_transactions/3 ###

<pre><code>
all_transactions(Cred::<a href="#type-credentials">credentials()</a>, WalletID::binary(), Opts::<a href="#type-options">options()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="all_users-1"></a>

### all_users/1 ###

<pre><code>
all_users(Cred::<a href="#type-credentials">credentials()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="all_users-2"></a>

### all_users/2 ###

<pre><code>
all_users(Cred::<a href="#type-credentials">credentials()</a>, Opts::<a href="#type-options">options()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="all_wallets-1"></a>

### all_wallets/1 ###

<pre><code>
all_wallets(Cred::<a href="#type-credentials">credentials()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="all_wallets-2"></a>

### all_wallets/2 ###

<pre><code>
all_wallets(Cred::<a href="#type-credentials">credentials()</a>, Opts::<a href="#type-options">options()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="build_uri-2"></a>

### build_uri/2 ###

`build_uri(X1, Username) -> any()`

<a name="change_user_password-4"></a>

### change_user_password/4 ###

<pre><code>
change_user_password(Cred::<a href="#type-credentials">credentials()</a>, Username::string(), OldPassword::string(), NewPassword::string()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="create_transaction-7"></a>

### create_transaction/7 ###

<pre><code>
create_transaction(Cred::<a href="#type-credentials">credentials()</a>, WalletID::binary(), Password::binary(), AssetID::binary(), Qty::binary(), Fee::binary(), Recipient::binary()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="create_user-3"></a>

### create_user/3 ###

<pre><code>
create_user(Cred::<a href="#type-credentials">credentials()</a>, Username::string(), Password::string()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="create_wallet-3"></a>

### create_wallet/3 ###

<pre><code>
create_wallet(Cred::<a href="#type-credentials">credentials()</a>, Password::binary(), AssetID::binary()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

Takes valid credentials, a string representing a user's password
and the asset ID.
Returns `{ok, Wallet}` where `Wallet` is the
decoded JSON representation of Upvest's response.

<a name="create_wallet-4"></a>

### create_wallet/4 ###

<pre><code>
create_wallet(Cred::<a href="#type-credentials">credentials()</a>, Password::binary(), AssetID::binary(), Type::binary()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

Takes valid credentials, a string representing a user's password,
the asset ID and wallet type.
Returns `{ok, Wallet}` where `Wallet` is the
decoded JSON representation of Upvest's response.

<a name="create_wallet-5"></a>

### create_wallet/5 ###

<pre><code>
create_wallet(Cred::<a href="#type-credentials">credentials()</a>, Password::binary(), AssetID::binary(), Type::binary(), Index::binary()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

Takes valid credentials, a string representing a user's password,
the asset ID, wallet type and the index type.
Returns `{ok, Wallet}` where `Wallet` is the
decoded JSON representation of Upvest's response.

<a name="delete_user-2"></a>

### delete_user/2 ###

<pre><code>
delete_user(Cred::<a href="#type-credentials">credentials()</a>, Username::string()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="get_asset-2"></a>

### get_asset/2 ###

<pre><code>
get_asset(Cred::<a href="#type-credentials">credentials()</a>, AssetID::string()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="get_assets-2"></a>

### get_assets/2 ###

<pre><code>
get_assets(Cred::<a href="#type-credentials">credentials()</a>, Limit::pos_integer()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="get_assets-3"></a>

### get_assets/3 ###

<pre><code>
get_assets(Cred::<a href="#type-credentials">credentials()</a>, Limit::pos_integer(), Opts::<a href="#type-options">options()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="get_transaction-3"></a>

### get_transaction/3 ###

<pre><code>
get_transaction(Cred::<a href="#type-credentials">credentials()</a>, WalletID::binary(), TxID::binary()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="get_transactions-3"></a>

### get_transactions/3 ###

<pre><code>
get_transactions(Cred::<a href="#type-credentials">credentials()</a>, WalletID::binary(), Limit::pos_integer()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="get_transactions-4"></a>

### get_transactions/4 ###

<pre><code>
get_transactions(Cred::<a href="#type-credentials">credentials()</a>, WalletID::binary(), Limit::pos_integer(), Opts::<a href="#type-options">options()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="get_user-2"></a>

### get_user/2 ###

<pre><code>
get_user(Cred::<a href="#type-credentials">credentials()</a>, Username::string()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="get_users-2"></a>

### get_users/2 ###

<pre><code>
get_users(Cred::<a href="#type-credentials">credentials()</a>, Limit::pos_integer()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="get_wallet-2"></a>

### get_wallet/2 ###

<pre><code>
get_wallet(Cred::<a href="#type-credentials">credentials()</a>, Username::string()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="get_wallets-2"></a>

### get_wallets/2 ###

<pre><code>
get_wallets(Cred::<a href="#type-credentials">credentials()</a>, Limit::pos_integer()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="get_wallets-3"></a>

### get_wallets/3 ###

<pre><code>
get_wallets(Cred::<a href="#type-credentials">credentials()</a>, Limit::pos_integer(), Opts::<a href="#type-options">options()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="keyauth-3"></a>

### keyauth/3 ###

<pre><code>
keyauth(Key::string(), Secret::string(), Passphrase::string()) -&gt; <a href="#type-credentials">credentials()</a>
</code></pre>
<br />

Takes an api key, secret and a passphrase.
Returns a value that can be used for API key-based authentication.

<a name="oauth-4"></a>

### oauth/4 ###

<pre><code>
oauth(ClientID::string(), ClientSecret::string(), Username::string(), Password::string()) -&gt; <a href="#type-credentials">credentials()</a>
</code></pre>
<br />

Takes a client ID, client secret, username and a password.
Returns a client with credentials that can be used for OAuth authentication.

<a name="sign_wallet-4"></a>

### sign_wallet/4 ###

<pre><code>
sign_wallet(Cred::<a href="#type-credentials">credentials()</a>, WalletID::binary(), Password::binary(), ToSign::binary()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="sign_wallet-5"></a>

### sign_wallet/5 ###

<pre><code>
sign_wallet(Cred::<a href="#type-credentials">credentials()</a>, WalletID::binary(), Password::binary(), ToSign::binary(), InputFormat::binary()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

<a name="sign_wallet-6"></a>

### sign_wallet/6 ###

<pre><code>
sign_wallet(Cred::<a href="#type-credentials">credentials()</a>, WalletID::binary(), Password::binary(), ToSign::binary(), InputFormat::binary(), OutputFormat::binary()) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

