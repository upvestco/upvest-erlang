

# Module upvest_req #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2019, Yao Adzaku

__Authors:__ Yao Adzaku ([`yao@rpip`](mailto:yao@rpip)).

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




### <a name="type-metadata">metadata()</a> ###


<pre><code>
metadata() = map()
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = map()
</code></pre>




### <a name="type-paginated_list">paginated_list()</a> ###


<pre><code>
paginated_list() = #paginated_list{previous = <a href="#type-url">url()</a>, next = <a href="#type-url">url()</a>, results = [<a href="#type-upvest_object">upvest_object()</a>]}
</code></pre>




### <a name="type-request">request()</a> ###


<pre><code>
request() = <a href="#type-request">request()</a>
</code></pre>




### <a name="type-result">result()</a> ###


<pre><code>
result() = {ok, binary() | upvest_object | paginated_list} | <a href="#type-error">error()</a>
</code></pre>




### <a name="type-upvest_object">upvest_object()</a> ###


<pre><code>
upvest_object() = #upvest_user{username = string(), recovery_kit = binary(), wallets = [#upvest_wallet{id = string(), path = string(), balances = [#wallet_balance{amount = pos_integer(), asset_id = string(), name = string(), symbol = string(), exponent = pos_integer()}], protocol = string(), address = <a href="#type-wallet_address">wallet_address()</a>, status = string(), index = pos_integer()}], wallet_ids = [string()]} | #upvest_asset{id = string(), name = string(), symbol = string(), exponent = pos_integer(), protocol = pos_integer(), metadata = <a href="#type-metadata">metadata()</a>} | #upvest_wallet{id = string(), path = string(), balances = [#wallet_balance{amount = pos_integer(), asset_id = string(), name = string(), symbol = string(), exponent = pos_integer()}], protocol = string(), address = <a href="#type-wallet_address">wallet_address()</a>, status = string(), index = pos_integer()} | #upvest_transaction{id = string(), tx_hash = string(), wallet_id = string(), asset_id = string(), asset_name = string(), exponent = string(), sender = string(), recipient = string(), quantity = string(), fee = string(), status = string()}
</code></pre>




### <a name="type-url">url()</a> ###


<pre><code>
url() = <a href="httpc.md#type-url">httpc:url()</a>
</code></pre>




### <a name="type-wallet_address">wallet_address()</a> ###


<pre><code>
wallet_address() = string()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_all-2">get_all/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_some-3">get_some/3</a></td><td></td></tr><tr><td valign="top"><a href="#run-1">run/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_all-2"></a>

### get_all/2 ###

<pre><code>
get_all(Resource::atom(), Req::<a href="#type-request">request()</a>) -&gt; {ok, <a href="#type-paginated_list">paginated_list()</a> | <a href="#type-error">error()</a>}
</code></pre>
<br />

<a name="get_some-3"></a>

### get_some/3 ###

<pre><code>
get_some(Resource::atom(), Limit::pos_integer(), Req::<a href="#type-request">request()</a>) -&gt; {ok, <a href="#type-paginated_list">paginated_list()</a>} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="run-1"></a>

### run/1 ###

<pre><code>
run(Req::<a href="#type-request">request()</a>) -&gt; <a href="#type-result">result()</a>
</code></pre>
<br />

