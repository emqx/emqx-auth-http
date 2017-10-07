%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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

-behaviour(emqx_auth_mod).

-include("emqx_auth_http.hrl").

-include_lib("emqx/include/emqx.hrl").

-import(emqx_auth_http_cli, [request/3, feedvar/2, feedvar/3]).

%% Callbacks
-export([init/1, check/3, description/0]).

-define(UNDEFINED(S), (S =:= undefined orelse S =:= <<>>)).

init({AuthReq, SuperReq}) ->
    {ok, {AuthReq, SuperReq}}.

check(#mqtt_client{username = Username}, Password, _Env) when ?UNDEFINED(Username); ?UNDEFINED(Password) ->
    {error, username_or_password_undefined};

check(Client, Password, {#http_request{method = Method, url = Url, params = Params}, SuperReq}) ->
    Params1 = feedvar(feedvar(Params, Client), "%P", Password),
    case request(Method, Url, Params1) of
        {ok, 200, _Body}  -> {ok, is_superuser(SuperReq, Client)};
        {ok, Code, _Body} -> {error, Code};
        {error, Error}    -> lager:error("HTTP ~s Error: ~p", [Url, Error]),
                             {error, Error}
    end.

description() -> "Authentication by HTTP API".

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | #http_request{}, mqtt_client()) -> boolean()).
is_superuser(undefined, _MqttClient) ->
    false;
is_superuser(#http_request{method = Method, url = Url, params = Params}, MqttClient) ->
    case request(Method, Url, feedvar(Params, MqttClient)) of
        {ok, 200, _Body}   -> true;
        {ok, _Code, _Body} -> false;
        {error, Error}     -> lager:error("HTTP ~s Error: ~p", [Url, Error]), false
    end.

