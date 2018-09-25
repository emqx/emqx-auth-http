%%--------------------------------------------------------------------
%% Copyright (c) 2013-2018 EMQ Enterprise, Inc. (http://emqtt.io)
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

-import(emqx_auth_http_cli, [request/5, feedvar/2, feedvar/3]).

%% Callbacks
-export([init/1, check/3, description/0]).

-define(UNDEFINED(S), (S =:= undefined orelse S =:= <<>>)).

init({AuthReq, SuperReq}) ->
    {ok, {AuthReq, SuperReq}}.

check(#mqtt_client{username = Username}, Password, _Env) when ?UNDEFINED(Username); ?UNDEFINED(Password) ->
    {error, username_or_password_undefined};

check(Client, Password, {#http_request{method = Method, url = Url, headers = Headers, body_type = ril}, SuperReq}) ->
    case json_ril(Client, Password) of
        {error, Reason} ->
            lager:error("HTTP auth failed, Url: ~s, Error: ~p", [Url, Reason]),
            {error, Reason};
        Body ->
            IsSuperUser = fun() -> is_superuser(SuperReq, Client) end,
            auth_query(Method, Url, Body, Headers, ril, IsSuperUser)
    end;

check(Client, Password, {#http_request{method = Method, url = Url, params = Params, headers = Headers, body_type = BodyType}, SuperReq}) ->
    Params1 = feedvar(feedvar(Params, Client), <<"%P">>, Password),
    IsSuperUser = fun() -> is_superuser(SuperReq, Client) end,
    auth_query(Method, Url, Params1, Headers, BodyType, IsSuperUser).

description() -> "Authentication by HTTP API".

auth_query(Method, Url, Params, Headers, BodyType, SuperUser) ->
    case request(Method, Url, Params, Headers, #{body_type => BodyType}) of
        {ok, 200, Body}  ->
            lager:debug("HTTP Auth OK, Url: ~p, Msg: ~p", [Url, Body]),
            {ok, SuperUser()};
        {ok, Code, Body} ->
            lager:error("HTTP Auth Failed: ~p, Url: ~p, Msg: ~p", [Code, Url, Body]),
            {error, Code};
        {error, Error} ->
            lager:error("HTTP Auth Error: ~p, Url: ~p", [Error, Url]),
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | #http_request{}, mqtt_client()) -> boolean()).
is_superuser(undefined, _MqttClient) ->
    false;
is_superuser(#http_request{method = Method, url = Url, params = Params, headers = Headers, body_type = BodyType}, MqttClient) ->
    is_superuser_query(Method, Url, feedvar(Params, MqttClient), Headers, BodyType).

is_superuser_query(Method, Url, Params, Headers, BodyType) ->
    case request(Method, Url, Params, Headers, #{body_type => BodyType}) of
        {ok, 200, _Body}   -> true;
        {ok, _Code, _Body} -> false;
        {error, Error}     -> lager:error("HTTP ~s Error: ~p", [Url, Error]), false
    end.

json_ril(Client = #mqtt_client{username = Username}, Password) ->
    try
        [_ClusterID, IMEI] = binary:split(Username, <<"_">>),
        jsx:encode(#{<<"deviceInfo">> => #{
                        <<"info">> => #{
                            <<"type">> => <<"android">>,
                            <<"imei">> => IMEI
                        },
                        <<"consumptionDeviceName">> => <<"JIO_IOT">>,
                        <<"jToken">> => Password
                    }})
    catch
        _E:{badmatch, _} -> {error, {bad_username, {username, Username}}};
        _E:{badarg, _} -> {error, {bad_client, Client}};
        _E:Reason -> {error, Reason}
    end.