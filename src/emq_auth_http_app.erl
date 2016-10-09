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

-module(emq_auth_http_app).

-behaviour(application).

-include("emq_auth_http.hrl").

-export([start/2, stop/1]).

-behaviour(supervisor).

-export([init/1]).

-define(APP, emq_auth_http).

%%--------------------------------------------------------------------
%% Application Callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    with_env(auth_req, fun reg_authmod/1),
    with_env(acl_req,  fun reg_aclmod/1),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

reg_authmod(AuthReq) ->
    SuperReq = r(application:get_env(?APP, super_req, undefined)),
    emqttd_access_control:register_mod(auth, emq_auth_http, {AuthReq, SuperReq}).

reg_aclmod(AclReq) ->
    emqttd_access_control:register_mod(acl, emq_acl_http, AclReq).

stop(_State) ->
    emqttd_access_control:unregister_mod(acl, emq_acl_http),
    emqttd_access_control:unregister_mod(auth, emq_auth_http).

%%--------------------------------------------------------------------
%% Dummy Supervisor
%%--------------------------------------------------------------------

init([]) ->
    {ok, { {one_for_all, 10, 100}, []} }.

%%--------------------------------------------------------------------
%% Internel Functions
%%--------------------------------------------------------------------

with_env(Par, Fun) ->
    case application:get_env(?APP, Par) of
        {ok, Req} -> Fun(r(Req));
        undefined -> ok
    end.

r(undefined) ->
    undefined;
r(Config) ->
    Method = proplists:get_value(method, Config, post),
    Url    = proplists:get_value(url, Config),
    Params = proplists:get_value(params, Config),
    #http_request{method = Method, url = Url, params = Params}.

