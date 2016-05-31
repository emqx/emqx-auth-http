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

-module(emqttd_acl_http).

-behaviour(emqttd_acl_mod).

-include("emqttd_auth_http.hrl").

-include("../../../include/emqttd.hrl").

-import(emqttd_auth_http, [http_request/3, feedvar/2, feedvar/3]).

%% ACL callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

init({SuperReq, AclReq}) ->
    {ok, {SuperReq, AclReq}}.

check_acl({#mqtt_client{username = <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    {error, bad_username};

check_acl({Client, PubSub, Topic}, {SuperReq, #http_request{method = Method, url = Url, params = Params}}) ->
    case emqttd_auth_http:is_superuser(SuperReq, Client) of
        false -> Params1 = feedvar(feedvar(feedvar(Params, Client), "%A", access(PubSub)), "%t", Topic),
                 case http_request(Method, Url, Params1) of
                    {ok, 200, _Body}   -> allow;
                    {ok, _Code, _Body} -> deny;
                    {error, Error}     -> lager:error("HTTP ~s Error: ~p", [Url, Error]), deny
                 end;
        true  -> allow
    end.

access(subscribe) -> 1;
access(publish)   -> 2.

reload_acl(_State) -> ok.

description() -> "ACL by HTTP API".

