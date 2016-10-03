%%--------------------------------------------------------------------
%% Copyright (c) 2012-2016 Feng Lee <feng@emqtt.io>.
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

-module(emqttd_auth_http_SUITE).

-compile(export_all).

-include_lib("emqttd/include/emqttd.hrl").

-include_lib("common_test/include/ct.hrl").

-define(SUPERUSER, [[{"username", "superuser"},
                     {"clientid", "superclient"}]]).

-define(ACL, [
              [{"username", "testuser"},
               {"clientid", "client1"},
               {"access", "1"},
               {"topic", "users/testuser/1"},
               {"ipaddr", "127.0.0.1"}],
             [{"username", "xyz"},
              {"clientid", "client2"},
              {"access", "2"},
              {"topic", "a/b/c"},
              {"ipaddr", "192.168.1.3"}]
             ]).

-define(AUTH, [
               [{"clientid","client1"},
                {"username", "testuser1"},
                {"password", "pass1"}],
               [{"clientid","client2"},
                {"username", "testuser2"},
                {"password", "pass2"}]
              ]).

all() -> 
    [{group, emqttd_auth_http}].

groups() -> 
    [{emqttd_auth_http, [sequence],
    [check_auth,
     check_acl,
     superUser]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    application:start(lager),
    application:set_env(emqttd, conf, filename:join([DataDir, "emqttd.conf"])),
    application:ensure_all_started(emqttd),
    application:set_env(emqttd_auth_http, conf, filename:join([DataDir, "emqttd_auth_http.conf"])),
    application:ensure_all_started(emqttd_auth_http),
    start_http_(),
    Config.

end_per_suite(_Config) ->
    mochiweb:stop_http(8080),
    application:stop(emqttd_auth_http),
    application:stop(emqttd),
    emqttd_mnesia:ensure_stopped().

check_acl(_) ->
    SuperUser = #mqtt_client{client_id = <<"superclient">>, username = <<"superuser">>, peername = {{127,0,0,1}, 2982}},
    deny = emqttd_access_control:check_acl(SuperUser, subscribe, <<"users/testuser/1">>),
    deny = emqttd_access_control:check_acl(SuperUser, publish, <<"anytopic">>),
    
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser">>, peername = {{127,0,0,1}, 2981}},
    UnIpUser1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser">>, peername = {{192,168,0,4}, 2981}},
    UnClientIdUser1 = #mqtt_client{client_id = <<"unkonwc">>, username = <<"testuser">>, peername = {{127,0,0,1}, 2981}},
    UnnameUser1= #mqtt_client{client_id = <<"client1">>, username = <<"unuser">>, peername = {{127,0,0,1}, 2981}},
    allow = emqttd_access_control:check_acl(User1, subscribe, <<"users/testuser/1">>),
    deny = emqttd_access_control:check_acl(User1, publish, <<"users/testuser/1">>),
    deny = emqttd_access_control:check_acl(UnIpUser1, subscribe, <<"users/testuser/1">>),
    deny = emqttd_access_control:check_acl(UnClientIdUser1, subscribe, <<"users/testuser/1">>),
    deny  = emqttd_access_control:check_acl(UnnameUser1, subscribe, <<"$SYS/testuser/1">>),
    
    
    User2 = #mqtt_client{client_id = <<"client2">>, username = <<"xyz">>, peername = {{127,0,0,1}, 2982}},
    UserC = #mqtt_client{client_id = <<"client2">>, username = <<"xyz">>, peername = {{192,168,1,3}, 2983}},
    allow = emqttd_access_control:check_acl(UserC, publish, <<"a/b/c">>),
    deny = emqttd_access_control:check_acl(User2, publish, <<"a/b/c">>),
    deny  = emqttd_access_control:check_acl(User2, subscribe, <<"$SYS/testuser/1">>).

check_auth(_) ->
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser1">>, peername = {{127,0,0,1}, 2981}},

    User2 = #mqtt_client{client_id = <<"client2">>, username = <<"testuser2">>, peername = {{127,0,0,1}, 2982}},
    
    User3 = #mqtt_client{client_id = <<"client3">>, peername = {{127,0,0,1}, 2983}},

    {ok, false} = emqttd_access_control:auth(User1, <<"pass1">>),
    {error, {http_code, _Code}} = emqttd_access_control:auth(User1, <<"pass">>),
    {error, username_or_password_undefined} = emqttd_access_control:auth(User1, <<>>),
    
    {ok, false} = emqttd_access_control:auth(User2, <<"pass2">>),
    {error, username_or_password_undefined} = emqttd_access_control:auth(User2, <<>>),
    {error, {http_code, _Code}} = emqttd_access_control:auth(User2, <<"errorpwd">>),
    
    {error, _} = emqttd_access_control:auth(User3, <<"pwd">>).

superuser(Config) ->



%%%%%%%start http listen%%%%%%%%%%%%%%%%%%%%%
start_http_() ->
     mochiweb:start_http(8080, [{max_clients, 1024}, {acceptors, 2}],
                        {?MODULE, handle, []}).

handle(Req) ->
    Path = Req:get(path),
    Params = params(Req),
    ct:log("Path:~p, Params:~p", [Path, Params]),
    handle_(Path, Params, Req).

handle_("/mqtt/superuser", Params, Req) ->
    reply(Req, mapping_(Params, ?SUPERUSER));

handle_("/mqtt/acl", Params, Req) ->
    reply(Req, mapping_(Params, ?ACL));
 
handle_("/mqtt/auth", Params, Req) ->
    reply(Req, mapping_(Params, ?AUTH)).
    
params(Req) ->
    case Req:get(method) of
        'GET'  -> Req:parse_qs();
        'POST' -> Req:parse_post()
    end.

mapping_(_Params, []) ->
    deny;
mapping_(Params, [H|T]) ->
    case mapping_acl_(Params, H) of
        deny ->
            mapping_(Params, T);
        _ ->
            allow
     end.

mapping_acl_([], _Acc) ->
    allow;

mapping_acl_([H|T], Acc) ->
    case lists:member(H, Acc) of
    true ->
        mapping_acl_(T, Acc);   
    false ->
       deny
    end.

reply(Req, Result) ->
    case Result of
    allow ->
        Req:respond({200, [{"Content-Type", "text/plain"}], []});
    deny ->
        Req:respond({404, [{"Content-Type", "text/plain"}], []})
    end.


