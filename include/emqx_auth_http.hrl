
-record(http_request, {method = post, url, params, headers, body_type}).

-define(APP, emqx_auth_http).
