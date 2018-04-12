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

-module(emqx_auth_http_cfg).

-include("emqx_auth_http.hrl").

-export ([register/0, unregister/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

register() ->
    clique_config:load_schema([code:priv_dir(?APP)], ?APP),
    register_formatter(),
    register_config().

unregister() ->
    unregister_formatter(),
    unregister_config(),
    clique_config:unload_schema(?APP).

%%--------------------------------------------------------------------
%% Get ENV Register formatter
%%--------------------------------------------------------------------

register_formatter() ->
    [clique:register_formatter(cuttlefish_variable:tokenize(Key), fun formatter_callback/2) || Key <- keys()].

formatter_callback([_, _, _], Params) ->
    proplists:get_value(url, Params);
formatter_callback([_, _, _, "params"], Params) ->
    format(proplists:get_value(params, Params));
formatter_callback([_, _, _, Key], Params) ->
    proplists:get_value(list_to_atom(Key), Params).

%%--------------------------------------------------------------------
%% UnRegister formatter
%%--------------------------------------------------------------------

unregister_formatter() ->
    [clique:unregister_formatter(cuttlefish_variable:tokenize(Key)) || Key <- keys()].

%%--------------------------------------------------------------------
%% Set ENV Register Config
%%--------------------------------------------------------------------

register_config() ->
    Keys = keys(),
    [clique:register_config(Key , fun config_callback/2) || Key <- Keys],
    clique:register_config_whitelist(Keys, ?APP).

config_callback([_, _, Key0], Value) ->
    Key = list_to_atom(Key0),
    {ok, Env} = application:get_env(?APP, Key),
    application:set_env(?APP, Key, lists:keyreplace(url, 1, Env, {url, Value})),
    " successfully\n";
config_callback([_, _, Key0, "params"], Value0) ->
    Key = list_to_atom(Key0),
    {ok, Env} = application:get_env(?APP, Key),
    Value = [list_to_tuple(string:tokens(S, "=")) || S <- string:tokens(Value0, ",")],
    application:set_env(?APP, Key, lists:keyreplace(params, 1, Env, {params, Value})),
    " successfully\n";
config_callback([_, _, Key0, Key1], Value) ->
    Key2 = list_to_atom(Key0),
    Key3 = list_to_atom(Key1),
    {ok, Env} = application:get_env(?APP, Key2),
    application:set_env(?APP, Key2, lists:keyreplace(Key3, 1, Env, {Key3, Value})),
    " successfully\n".

%%--------------------------------------------------------------------
%% UnRegister config
%%--------------------------------------------------------------------

unregister_config() ->
    Keys = keys(),
    [clique:unregister_config(Key) || Key <- Keys],
    clique:unregister_config_whitelist(Keys, ?APP).

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

format(Params) ->
    format(Params, "").
format([{Key, Value}], Acc) ->
    Acc ++ lists:concat([Key, "=", Value]);
format([{Key, Value} | Params], Acc) ->
    format(Params, Acc ++ lists:concat([Key, "=", Value, ","])).

keys() ->
    ["auth.http.auth_req",
     "auth.http.auth_req.method",
     "auth.http.auth_req.params",
     "auth.http.super_req",
     "auth.http.super_req.method",
     "auth.http.super_req.params",
     "auth.http.acl_req",
     "auth.http.acl_req.method",
     "auth.http.acl_req.params"].

