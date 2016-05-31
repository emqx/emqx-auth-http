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

-module(emqttd_auth_http_app).

-behaviour(application).

-include("emqttd_auth_http.hrl").

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1]).

-define(APP, emqttd_auth_http).

%%--------------------------------------------------------------------
%% Application Callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    SuperReq = record(application:get_env(?APP, super_req, undefined)),
    ok = register_auth_mod(SuperReq), ok = register_acl_mod(SuperReq),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

register_auth_mod(SuperReq) ->
    {ok, AuthReq} = application:get_env(?APP, auth_req),
    emqttd_access_control:register_mod(auth, emqttd_auth_http, {SuperReq, record(AuthReq)}).

register_acl_mod(SuperReq) ->
    with_acl_enabled(fun(AclReq) ->
        emqttd_access_control:register_mod(acl, emqttd_acl_http, {SuperReq, record(AclReq)})
    end).

prep_stop(State) ->
    emqttd_access_control:unregister_mod(acl, emqttd_acl_http),
    emqttd_access_control:unregister_mod(auth, emqttd_auth_http),
    State.

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% Dummy Supervisor
%%--------------------------------------------------------------------

init([]) ->
    {ok, { {one_for_all, 10, 100}, []} }.

%%--------------------------------------------------------------------
%% Internel Functions
%%--------------------------------------------------------------------

record(undefined) ->
    undefined;
record(Config) ->
    Method = proplists:get_value(method, Config, post),
    Url    = proplists:get_value(url, Config),
    Params = proplists:get_value(params, Config),
    #http_request{method = Method, url = Url, params = Params}.

with_acl_enabled(Fun) ->
    case application:get_env(?APP, acl_req) of
        {ok, AclReq} -> Fun(AclReq);
        undefined    -> ok
    end.

