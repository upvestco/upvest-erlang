

# Module upvest_utils #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

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




### <a name="type-url">url()</a> ###


<pre><code>
url() = <a href="httpc.md#type-url">httpc:url()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#generate_signature-2">generate_signature/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_uri-1">parse_uri/1</a></td><td>Parse a URI.</td></tr><tr><td valign="top"><a href="#timestamp-0">timestamp/0</a></td><td>Returns current timestamp in seconds.</td></tr><tr><td valign="top"><a href="#to_bin-1">to_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_str-1">to_str/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="generate_signature-2"></a>

### generate_signature/2 ###

<pre><code>
generate_signature(Secret::string(), Message::string()) -&gt; string()
</code></pre>
<br />

<a name="parse_uri-1"></a>

### parse_uri/1 ###

<pre><code>
parse_uri(URI::string()) -&gt; <a href="#type-url">url()</a> | {error, Reason::term()}
</code></pre>
<br />

Parse a URI

Returns either the URI or an error term.

<a name="timestamp-0"></a>

### timestamp/0 ###

<pre><code>
timestamp() -&gt; integer()
</code></pre>
<br />

Returns current timestamp in seconds

<a name="to_bin-1"></a>

### to_bin/1 ###

`to_bin(Arg) -> any()`

<a name="to_str-1"></a>

### to_str/1 ###

`to_str(Arg) -> any()`

