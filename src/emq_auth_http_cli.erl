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

-module(emq_auth_http_cli).

-include_lib("emqttd/include/emqttd.hrl").

-export([request/3, feedvar/2, feedvar/3]).

%%--------------------------------------------------------------------
%% HTTP Request
%%--------------------------------------------------------------------

request(get, Url, Params) ->
    Req = {Url ++ "?" ++ mochiweb_util:urlencode(Params), []},
    reply(httpc:request(get, Req, [{autoredirect, true}], []));

request(post, Url, Params) ->
    Req = {Url, [], "application/x-www-form-urlencoded", mochiweb_util:urlencode(Params)},
    reply(httpc:request(post, Req, [{autoredirect, true}], [])).

reply({ok, {{_, Code, _}, _Headers, Body}}) ->
    {ok, Code, Body};
reply({ok, Code, Body}) ->
    {ok, Code, Body};
reply({error, Error}) ->
    {error, Error}.

%%--------------------------------------------------------------------
%% Feed Variables
%%--------------------------------------------------------------------

feedWsHeader(WsInitialHeaders)  when is_list(WsInitialHeaders) ->
    F = fun ({K, V}) ->
                lists:flatten(io_lib:format("~s: ~s", [K, V]))
        end,
    string:join(lists:map(F, WsInitialHeaders), "\r\n");

feedWsHeader(WsInitialHeaders) when not is_list(WsInitialHeaders) ->
    undefined.

feedvar(Params, #mqtt_client{username = Username, client_id = ClientId, ws_initial_headers = WsInitialHeaders, peername = {IpAddr, _}}) ->
    lists:map(fun({Param, "%u"}) -> {Param, Username};
                 ({Param, "%c"}) -> {Param, ClientId};
                 ({Param, "%a"}) -> {Param, inet:ntoa(IpAddr)};
                 ({Param, "%h"}) -> {Param, feedWsHeader(WsInitialHeaders)};
                 (Param)         -> Param
              end, Params).

feedvar(Params, Var, Val) ->
    lists:map(fun({Param, Var0}) when Var0 == Var -> {Param, Val}; (Param) -> Param end, Params).

