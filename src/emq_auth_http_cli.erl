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

-module(emq_auth_http_cli).

-export([request/3]).

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

