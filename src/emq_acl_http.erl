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

-module(emq_acl_http).

-behaviour(emqttd_acl_mod).

-include("emq_auth_http.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-import(emq_auth_http_cli, [request/3, feedvar/2, feedvar/3]).

%% ACL callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

-record(state, {acl_req}).

init(AclReq) ->
	{ok, #state{acl_req = AclReq}}.
 
check_acl({Client, PubSub, Topic}, #state{acl_req = #http_request{method = Method, url = Url, params = Params}}) ->
    Params1 = feedvar(feedvar(feedvar(Params, Client), "%A", access(PubSub)), "%t", Topic),
    case request(Method, Url, Params1) of
        {ok, 200, _Body}   -> allow;
        {ok, _Code, _Body} -> ignore;
        {error, Error}     -> lager:error("HTTP ~s Error: ~p", [Url, Error]), deny
    end.

access(subscribe) -> 1;
access(publish)   -> 2.

reload_acl(_State) -> ok.

description() -> "ACL with HTTP API".

