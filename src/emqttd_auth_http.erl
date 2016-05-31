%%--------------------------------------------------------------------
%% Copyright (c) 2016 Feng Lee <feng@emqtt.io>.
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

-module(emqttd_auth_http).

-behaviour(emqttd_auth_mod).

-include("emqttd_auth_http.hrl").

-include("../../../include/emqttd.hrl").

-export([is_superuser/2, http_request/3, feedvar/2, feedvar/3]).

-export([init/1, check/3, description/0]).

-define(UNDEFINED(S), (S =:= undefined orelse S =:= <<>>)).

init({SuperReq, AuthReq}) ->
    {ok, {SuperReq, AuthReq}}.

check(#mqtt_client{username = Username}, _Password, _Env) when ?UNDEFINED(Username) ->
    {error, username_undefined};

check(Client, Password, {SuperReq, _}) when ?UNDEFINED(Password) ->
    case is_superuser(SuperReq, Client) of
        true  -> ok;
        false -> {error, password_undefined}
    end;

check(Client, Password, {SuperReq, #http_request{method = Method, url = Url, params = Params}}) ->
    case is_superuser(SuperReq, Client) of
        false -> Params1 = feedvar(feedvar(Params, Client), "%P", Password),
                 case http_request(Method, Url, Params1) of
                    {ok, 200, _Body}  -> ok;
                    {ok, Code, _Body} -> {error, {http_code, Code}};
                    {error, Error}    -> lager:error("HTTP ~s Error: ~p", [Url, Error]),
                                         {error, Error}
                 end;
        true  -> ok
    end.

description() -> "Authentication by HTTP API".

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | #http_request{}, mqtt_client()) -> boolean()).
is_superuser(undefined, _MqttClient) ->
    false;
is_superuser(#http_request{method = Method, url = Url, params = Params}, MqttClient) ->
    case http_request(Method, Url, feedvar(Params, MqttClient)) of
        {ok, 200, _Body}   -> true;
        {ok, _Code, _Body} -> false;
        {error, Error}     -> lager:error("HTTP ~s Error: ~p", [Url, Error]), false
    end.

%%--------------------------------------------------------------------
%% HTTP Request
%%--------------------------------------------------------------------

http_request(get, Url, Params) ->
    Req = {Url ++ "?" ++ mochiweb_util:urlencode(Params), []},
    reply(httpc:request(post, Req, [{autoredirect, true}], []));

http_request(post, Url, Params) ->
    Req = {Url, [], "application/x-www-form-urlencoded", mochiweb_util:urlencode(Params)},
    reply(httpc:request(post, Req, [{autoredirect, true}], [])).

reply({ok, {{_, Code, _}, _Headers, Body}}) ->
    {ok, Code, Body};
reply({ok, Code, Body}) ->
    {ok, Code, Body};
reply({error, Error}) ->
    {error, Error}.

feedvar(Params, #mqtt_client{username = Username, client_id = ClientId, peername = {IpAddr, _}}) ->
    lists:map(fun({Param, "%u"}) -> {Param, Username};
                 ({Param, "%c"}) -> {Param, ClientId};
                 ({Param, "%a"}) -> {Param, inet:ntoa(IpAddr)};
                 (Param)         -> Param
              end, Params).

feedvar(Params, Var, Val) ->
    lists:map(fun({Param, Var0}) when Var0 == Var -> {Param, Val}; (Param) -> Param end, Params).

