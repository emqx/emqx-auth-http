%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_auth_http_SUITE).

-compile(export_all).

-include_lib("emqx/include/emqx.hrl").

-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(APP, emqx_auth_http).

all() ->
    [{group, emqx_auth_http}].

groups() ->
    [{emqx_auth_http, [sequence],
    [check_acl,
     check_auth,
     sub_pub,
     %server_config,
     comment_config
    ]}].

init_per_suite(Config) ->
    http_auth_server:start_http(),
    [start_apps(App, {SchemaFile, ConfigFile}) ||
      {App, SchemaFile, ConfigFile}
        <- [{emqx, local_path("deps/emqx/priv/emqx.schema"),
                   local_path("deps/emqx/etc/emqx.conf")},
            {emqx_auth_http, local_path("priv/emqx_auth_http.schema"),
                             local_path("etc/emqx_auth_http.conf")},
            {emqx_retainer, local_path("deps/emqx_retainer/priv/emqx_retainer.schema"),
                            local_path("deps/emqx_retainer/etc/emqx_retainer.conf")}]],
    Config.

get_base_dir() ->
    {file, Here} = code:is_loaded(?MODULE),
    filename:dirname(filename:dirname(Here)).

local_path(RelativePath) ->
    filename:join([get_base_dir(), RelativePath]).

start_apps(App, {SchemaFile, ConfigFile}) ->
    read_schema_configs(App, {SchemaFile, ConfigFile}),
    set_special_configs(App),
    application:ensure_all_started(App).

read_schema_configs(App, {SchemaFile, ConfigFile}) ->
    ct:pal("Read configs - SchemaFile: ~p, ConfigFile: ~p", [SchemaFile, ConfigFile]),
    Schema = cuttlefish_schema:files([SchemaFile]),
    Conf = conf_parse:file(ConfigFile),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig, []),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals].

set_special_configs(emqx) ->
    application:set_env(emqx, allow_anonymous, false),
    application:set_env(emqx, enable_acl_cache, false),
    application:set_env(emqx, plugins_loaded_file,
                        local_path("deps/emqx/test/emqx_SUITE_data/loaded_plugins"));
set_special_configs(_App) ->
    ok.

end_per_suite(_Config) ->
    http_auth_server:stop_http(),
    [application:stop(App) || App <- [emqx_retainer, emqx_auth_http, emqx]].

init_per_testcase(_) ->
    ok.

end_per_testcase(_) ->
    ok.

check_acl(_) ->
    %ct:pal("all configs: ~p ", [application:get_all_env(?APP)]),
    %ct:pal("emqx all configs: ~p ", [application:get_all_env(emqx)]),
    SuperUser = #{client_id => <<"superclient">>, username => <<"superuser">>,
                  peername => {{127,0,0,1}, 2982}},
    deny = emqx_access_control:check_acl(SuperUser, subscribe, <<"users/testuser/1">>),
    deny = emqx_access_control:check_acl(SuperUser, publish, <<"anytopic">>),

    User1 = #{client_id => <<"client1">>, username => <<"testuser">>, peername => {{127,0,0,1}, 2981}},
    UnIpUser1 = #{client_id => <<"client1">>, username => <<"testuser">>, peername => {{192,168,0,4}, 2981}},
    UnClientIdUser1 = #{client_id => <<"unkonwc">>, username => <<"testuser">>, peername => {{127,0,0,1}, 2981}},
    UnnameUser1= #{client_id => <<"client1">>, username => <<"unuser">>, peername => {{127,0,0,1}, 2981}},
    allow = emqx_access_control:check_acl(User1, subscribe, <<"users/testuser/1">>),
    deny = emqx_access_control:check_acl(User1, publish, <<"users/testuser/1">>),
    deny = emqx_access_control:check_acl(UnIpUser1, subscribe, <<"users/testuser/1">>),
    deny = emqx_access_control:check_acl(UnClientIdUser1, subscribe, <<"users/testuser/1">>),
    deny  = emqx_access_control:check_acl(UnnameUser1, subscribe, <<"$SYS/testuser/1">>),

    User2 = #{client_id => <<"client2">>, username => <<"xyz">>, peername => {{127,0,0,1}, 2982}},
    UserC = #{client_id => <<"client2">>, username => <<"xyz">>, peername => {{192,168,1,3}, 2983}},
    allow = emqx_access_control:check_acl(UserC, publish, <<"a/b/c">>),
    deny = emqx_access_control:check_acl(User2, publish, <<"a/b/c">>),
    deny  = emqx_access_control:check_acl(User2, subscribe, <<"$SYS/testuser/1">>).

check_auth(_) ->
%    {ok, Default} = application:get_env(emqx, allow_anonymous),
    User1 = #{client_id => <<"client1">>, username => <<"testuser1">>, peername => {{127,0,0,1}, 2981}},
    User2 = #{client_id => <<"client2">>, username => <<"testuser2">>, peername => {{127,0,0,1}, 2982}},
    User3 = #{client_id => <<"client3">>, peername => {{127,0,0,1}, 2983}},

    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(User1, <<"pass1">>),
    {error, 404} = emqx_access_control:authenticate(User1, <<"pass">>),
    {error, username_or_password_undefined} = emqx_access_control:authenticate(User1, <<>>),

    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(User2, <<"pass2">>),
    {error, username_or_password_undefined} = emqx_access_control:authenticate(User2, <<>>),
    {error, 404} = emqx_access_control:authenticate(User2, <<"errorpwd">>),

    {error, _} = emqx_access_control:authenticate(User3, <<"pwd">>).

sub_pub(_) ->
    ct:pal("start client"),
    {ok, T1} = emqx_client:start_link([{host, "localhost"},
                                       {client_id, <<"client1">>},
                                       {username, <<"testuser1">>},
                                       {password, <<"pass1">>}]),
    {ok, _} = emqx_client:connect(T1),
    emqx_client:publish(T1, <<"topic">>, <<"body">>, [{qos, 0}, {retain, true}]),
    timer:sleep(1000),
    {ok, T2} = emqx_client:start_link([{host, "localhost"},
                                       {client_id, <<"client2">>},
                                       {username, <<"testuser2">>},
                                       {password, <<"pass2">>}]),
    {ok, _} = emqx_client:connect(T2),
    emqx_client:subscribe(T2, <<"topic">>),
    receive
        {publish, _Topic, Payload} ->
            ?assertEqual(<<"body">>, Payload)
        after 1000 -> false end,
    emqx_client:disconnect(T1),
    emqx_client:disconnect(T2).

server_config(_) ->
    Auth = [{url,"http://127.0.0.1:8991/mqtt/auth1"},
            {method,get},
            {params,[{"clientid","%c"},
                     {"username","%u"}
                     ]}],
    Acl = [{url,"http://127.0.0.1:8991/mqtt/acl"},
                           {method,post},
                           {params,[{"access","%A"},
                                    {"username","%u"},
                                    {"clientid","%c"},
                                    {"ipaddr","%a"}
                                    ]}],

    Super = [{url,"http://127.0.0.1:8991/mqtt/superuser1"},
             {method,get},
             {params,[{"clientid","%c"}]}],
    SetConfigKeys = ["auth_req=http://127.0.0.1:8991/mqtt/auth1",
                     "auth_req.method=get",
                     "auth_req.params=clientid=%c,username=%u",
                     "super_req=http://127.0.0.1:8991/mqtt/superuser1",
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
    emqx_cli_config:run(["config", "set", string:join(["auth.http", Key], "."), "--app=emqx_auth_http"]).

comment_config(_) ->
    application:stop(?APP),
    [application:unset_env(?APP, Par) || Par <- [acl_req, auth_req]],
    application:start(?APP),
    ?assertEqual([], emqx_access_control:lookup_mods(auth)),
    ?assertEqual([], emqx_access_control:lookup_mods(acl)).
