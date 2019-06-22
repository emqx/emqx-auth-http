%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_auth_http).

-include("emqx_auth_http.hrl").

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-import(emqx_auth_http_cli,
        [ request/5
        , feedvar/2
        ]).

%% Callbacks
-export([ register_metrics/0
        , check/2
        , description/0
        ]).

register_metrics() ->
    [emqx_metrics:new(MetricName) || MetricName <- ['auth.http.success', 'auth.http.failure', 'auth.http.ignore']].

check(Credentials, #{auth_req := AuthReq,
                     super_req := SuperReq,
                     http_opts := HttpOpts,
                     retry_opts := RetryOpts}) ->
    case authenticate(AuthReq, Credentials, HttpOpts, RetryOpts) of
        {ok, 200, "ignore"} ->
            emqx_metrics:inc('auth.http.ignore'), ok;
        {ok, 200, Body}  ->
            emqx_metrics:inc('auth.http.success'),
            {stop, Credentials#{is_superuser => is_superuser(SuperReq, Credentials, HttpOpts, RetryOpts),
                                auth_result => success,
                                anonymous => false,
                                mountpoint  => mountpoint(Body, Credentials)}};
        {ok, Code, _Body} ->
            emqx_metrics:inc('auth.http.failure'),
            {stop, Credentials#{auth_result => Code, anonymous => false}};
        {error, Error} ->
            ?LOG(error, "[Auth http] check_auth Url: ~p Error: ~p", [AuthReq#http_request.url, Error]),
            emqx_metrics:inc('auth.http.failure'),
            {stop, Credentials#{auth_result => Error, anonymous => false}}
    end.

description() -> "Authentication by HTTP API".

%%--------------------------------------------------------------------
%% Requests
%%--------------------------------------------------------------------

authenticate(#http_request{method = Method, url = Url, params = Params}, Credentials, HttpOpts, RetryOpts) ->
   request(Method, Url, feedvar(Params, Credentials), HttpOpts, RetryOpts).

-spec(is_superuser(undefined | #http_request{}, emqx_types:credetials(), list(), list()) -> boolean()).
is_superuser(undefined, _Credetials, _HttpOpts, _RetryOpts) ->
    false;
is_superuser(#http_request{method = Method, url = Url, params = Params}, Credetials, HttpOpts, RetryOpts) ->
    case request(Method, Url, feedvar(Params, Credetials), HttpOpts, RetryOpts) of
        {ok, 200, _Body}   -> true;
        {ok, _Code, _Body} -> false;
        {error, Error}     -> ?LOG(error, "[Auth HTTP] is_superuser ~s Error: ~p", [Url, Error]),
                              false
    end.

mountpoint(Body, Credetials) when is_list(Body) ->
    mountpoint(list_to_binary(Body), Credetials);

mountpoint(Body, #{mountpoint := Mountpoint}) ->
    case emqx_json:safe_decode(Body, [return_maps]) of
        {error, _} -> Mountpoint;
        {ok, Json} when is_map(Json) ->
            maps:get(<<"mountpoint">>, Json, Mountpoint);
        {ok, _NotMap} ->
            Mountpoint
    end.

