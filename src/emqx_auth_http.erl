%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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
%%--------------------------------------------------------------------

-module(emqx_auth_http).

-include("emqx_auth_http.hrl").

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").
-include_lib("emqx/include/types.hrl").

-import(emqx_auth_http_cli,
        [ request/5
        , feedvar/2
        ]).

%% Callbacks
-export([ register_metrics/0
        , check/3
        , description/0
        ]).

-define(AUTH_METRICS,
        ['auth.http.success',
         'auth.http.failure',
         'auth.http.ignore'
        ]).

-spec(register_metrics() -> ok).
register_metrics() ->
    lists:foreach(fun emqx_metrics:new/1, ?AUTH_METRICS).

check(ClientInfo, AuthResult, #{auth_req   := AuthReq,
                                super_req  := SuperReq,
                                http_opts  := HttpOpts,
                                retry_opts := RetryOpts}) ->
    case authenticate(AuthReq, ClientInfo, HttpOpts, RetryOpts) of
        {ok, 200, "ignore"} ->
            emqx_metrics:inc('auth.http.ignore'), ok;
        {ok, 200, Body}  ->
            emqx_metrics:inc('auth.http.success'),
            IsSuperuser = is_superuser(SuperReq, ClientInfo, HttpOpts, RetryOpts),
            {stop, AuthResult#{is_superuser => IsSuperuser,
                                auth_result => success,
                                anonymous   => false,
                                mountpoint  => mountpoint(Body, ClientInfo)}};
        {ok, Code, _Body} ->
            ?LOG(error, "[Auth http] check_auth Url: ~p login failed. result ~p",
                 [AuthReq#http_request.url, Code]),
            emqx_metrics:inc('auth.http.failure'),
            {stop, AuthResult#{auth_result => http_to_connack_error(Code),
                               anonymous   => false}};
        {error, Error} ->
            ?LOG(error, "[Auth http] check_auth Url: ~p Error: ~p",
                 [AuthReq#http_request.url, Error]),
            emqx_metrics:inc('auth.http.failure'),
            %%FIXME later: server_unavailable is not right.
            {stop, AuthResult#{auth_result => server_unavailable,
                               anonymous   => false}}
    end.

description() -> "Authentication by HTTP API".

%%--------------------------------------------------------------------
%% Requests
%%--------------------------------------------------------------------

authenticate(#http_request{method = Method,
                           url    = Url,
                           params = Params},
             ClientInfo, HttpOpts, RetryOpts) ->
   request(Method, Url, feedvar(Params, ClientInfo), HttpOpts, RetryOpts).

-spec(is_superuser(maybe(#http_request{}), emqx_types:client(), list(), list()) -> boolean()).
is_superuser(undefined, _ClientInfo, _HttpOpts, _RetryOpts) ->
    false;
is_superuser(#http_request{method = Method,
                           url    = Url,
                           params = Params},
             ClientInfo, HttpOpts, RetryOpts) ->
    case request(Method, Url, feedvar(Params, ClientInfo), HttpOpts, RetryOpts) of
        {ok, 200, _Body}   -> true;
        {ok, _Code, _Body} -> false;
        {error, Error}     -> ?LOG(error, "[Auth HTTP] is_superuser ~s Error: ~p", [Url, Error]),
                              false
    end.

mountpoint(Body, #{mountpoint := Mountpoint}) ->
    case emqx_json:safe_decode(iolist_to_binary(Body), [return_maps]) of
        {error, _} -> Mountpoint;
        {ok, Json} when is_map(Json) ->
            maps:get(<<"mountpoint">>, Json, Mountpoint);
        {ok, _NotMap} -> Mountpoint
    end.

http_to_connack_error(400) -> bad_username_or_password;
http_to_connack_error(401) -> bad_username_or_password;
http_to_connack_error(403) -> not_authorized;
http_to_connack_error(429) -> banned;
http_to_connack_error(503) -> server_unavailable;
http_to_connack_error(504) -> server_busy;
http_to_connack_error(_) -> server_unavailable.

