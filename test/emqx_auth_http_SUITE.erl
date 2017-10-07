%%--------------------------------------------------------------------
%% Copyright (c) 2016-2017 Feng Lee <feng@emqtt.io>.
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

-module(emqx_auth_http_SUITE).

-compile(export_all).

-include_lib("emqx/include/emqx.hrl").

-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(APP, emqx_auth_http).

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
              {"ipaddr", "192.168.1.3"}],
            [{"username", "testuser1"},
              {"clientid", "client1"},
              {"access", "2"},
              {"topic", "topic"},
              {"ipaddr", "127.0.0.1"}],
            [{"username", "testuser2"},
              {"clientid", "client2"},
              {"access", "1"},
              {"topic", "topic"},
              {"ipaddr", "127.0.0.1"}]
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
    [{group, emqx_auth_http}].

groups() -> 
    [{emqx_auth_http, [sequence],
    [check_acl,
     check_auth,
     restart_httpserver,
     sub_pub,
     server_config,
     comment_config
    ]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    [start_apps(App, DataDir) || App <- [emqx, emqx_auth_http, emqx_retainer]],
    start_http_(),
    Config.

end_per_suite(_Config) ->
    mochiweb:stop_http(8080),
    [application:stop(App) || App <- [emqx_retainer, emqx_auth_http, emqx]].

check_acl(_) ->
    SuperUser = #mqtt_client{client_id = <<"superclient">>, username = <<"superuser">>, peername = {{127,0,0,1}, 2982}},
    deny = emqx_access_control:check_acl(SuperUser, subscribe, <<"users/testuser/1">>),
    deny = emqx_access_control:check_acl(SuperUser, publish, <<"anytopic">>),
    
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser">>, peername = {{127,0,0,1}, 2981}},
    UnIpUser1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser">>, peername = {{192,168,0,4}, 2981}},
    UnClientIdUser1 = #mqtt_client{client_id = <<"unkonwc">>, username = <<"testuser">>, peername = {{127,0,0,1}, 2981}},
    UnnameUser1= #mqtt_client{client_id = <<"client1">>, username = <<"unuser">>, peername = {{127,0,0,1}, 2981}},
    allow = emqx_access_control:check_acl(User1, subscribe, <<"users/testuser/1">>),
    deny = emqx_access_control:check_acl(User1, publish, <<"users/testuser/1">>),
    deny = emqx_access_control:check_acl(UnIpUser1, subscribe, <<"users/testuser/1">>),
    deny = emqx_access_control:check_acl(UnClientIdUser1, subscribe, <<"users/testuser/1">>),
    deny  = emqx_access_control:check_acl(UnnameUser1, subscribe, <<"$SYS/testuser/1">>),
    
    
    User2 = #mqtt_client{client_id = <<"client2">>, username = <<"xyz">>, peername = {{127,0,0,1}, 2982}},
    UserC = #mqtt_client{client_id = <<"client2">>, username = <<"xyz">>, peername = {{192,168,1,3}, 2983}},
    allow = emqx_access_control:check_acl(UserC, publish, <<"a/b/c">>),
    deny = emqx_access_control:check_acl(User2, publish, <<"a/b/c">>),
    deny  = emqx_access_control:check_acl(User2, subscribe, <<"$SYS/testuser/1">>).

check_auth(_) ->
%    {ok, Default} = application:get_env(emqttd, allow_anonymous),
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser1">>, peername = {{127,0,0,1}, 2981}},

    User2 = #mqtt_client{client_id = <<"client2">>, username = <<"testuser2">>, peername = {{127,0,0,1}, 2982}},
    
    User3 = #mqtt_client{client_id = <<"client3">>, peername = {{127,0,0,1}, 2983}},

    {ok, false} = emqx_access_control:auth(User1, <<"pass1">>),
    {error, 404} = emqx_access_control:auth(User1, <<"pass">>),
    {error, username_or_password_undefined} = emqx_access_control:auth(User1, <<>>),
    
    {ok, false} = emqx_access_control:auth(User2, <<"pass2">>),
    {error, username_or_password_undefined} = emqx_access_control:auth(User2, <<>>),
    {error, 404} = emqx_access_control:auth(User2, <<"errorpwd">>),
    
    {error, _} = emqx_access_control:auth(User3, <<"pwd">>).

restart_httpserver(_) ->
%    {ok, Default} = application:get_env(emqttd, acl_nomatch),
    mochiweb:stop_http(8080),
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser">>, peername = {{127,0,0,1}, 2981}},
    deny = emqx_access_control:check_acl(User1, subscribe, <<"users/testuser/1">>),
    start_http_(),
    allow = emqx_access_control:check_acl(User1, subscribe, <<"users/testuser/1">>).

sub_pub(_) ->
    {ok, T1} = emqttc:start_link([{host, "localhost"}, {client_id, <<"client1">>}, {username, <<"testuser1">>}, {password, <<"pass1">>}]),
    emqttc:publish(T1, <<"topic">>, <<"body">>, [{qos, 0}, {retain, true}]),
    timer:sleep(1000),
    {ok, T2} = emqttc:start_link([{host, "localhost"}, {client_id, <<"client2">>}, {username, <<"testuser2">>}, {password, <<"pass2">>}]),
    emqttc:subscribe(T2, <<"topic">>),
    receive
        {publish, Topic, Payload} ->
            ?assertEqual(<<"body">>, Payload)
        after 1000 -> false end,
    emqttc:disconnect(T1),
    emqttc:disconnect(T2).

server_config(_) ->
    Auth = [{url,"http://127.0.0.1:8080/mqtt/auth1"},
            {method,get},
            {params,[{"clientid","%c"},
                     {"username","%u"}
                     ]}],
    Acl = [{url,"http://127.0.0.1:8090/mqtt/acl"},
                           {method,post},
                           {params,[{"access","%A"},
                                    {"username","%u"},
                                    {"clientid","%c"},
                                    {"ipaddr","%a"}
                                    ]}],

    Super = [{url,"http://127.0.0.1:8080/mqtt/superuser1"},
             {method,get},
             {params,[{"clientid","%c"}]}],
    SetConfigKeys = ["auth_req=http://127.0.0.1:8080/mqtt/auth1",
                     "auth_req.method=get",
                     "auth_req.params=clientid=%c,username=%u",
                     "super_req=http://127.0.0.1:8080/mqtt/superuser1",
                     "super_req.method=get",
                     "super_req.params=clientid=%c",
                     "acl_req=http://127.0.0.1:8090/mqtt/acl",
                     "acl_req.method=post",
                     "acl_req.params=access=%A,username=%u,clientid=%c,ipaddr=%a"],
    lists:foreach(fun set_cmd/1, SetConfigKeys),
    {ok, Auth_} = application:get_env(?APP, auth_req),
    {ok, Super_} = application:get_env(?APP, super_req),
    {ok, Acl_} = application:get_env(?APP, acl_req),

    ?assertEqual(lists:sort(Auth), lists:sort(Auth_)),
    ?assertEqual(lists:sort(Acl), lists:sort(Acl_)),
    ?assertEqual(lists:sort(Super), lists:sort(Super_)).

set_cmd(Key) ->
    emqttd_cli_config:run(["config", "set", string:join(["auth.http", Key], "."), "--app=emqx_auth_http"]).

comment_config(_) ->
    application:stop(?APP),
    [application:unset_env(?APP, Par) || Par <- [acl_req, auth_req]],
    application:start(?APP),
    ?assertEqual([], emqttd_access_control:lookup_mods(auth)),
    ?assertEqual([], emqttd_access_control:lookup_mods(acl)).



%%%%%%%start http listen%%%%%%%%%%%%%%%%%%%%%
start_http_() ->
    mochiweb:start_http(8080, [{max_clients, 1024}, {acceptors, 1024}],
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

start_apps(App, DataDir) ->
    Schema = cuttlefish_schema:files([filename:join([DataDir, atom_to_list(App) ++ ".schema"])]),
    Conf = conf_parse:file(filename:join([DataDir, atom_to_list(App) ++ ".conf"])),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals],
    application:ensure_all_started(App).

