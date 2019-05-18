%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-define(USER(ClientId, Username, Sockname, Peername, Zone),
        #{client_id => ClientId, username => Username, sockname => Sockname, peername => Peername, zone => Zone}).

-define(USER(ClientId, Username, Sockname, Peername, Zone, Mountpoint),
        #{client_id => ClientId, username => Username, sockname => Sockname, peername => Peername, zone => Zone, mountpoint => Mountpoint}).
all() ->
    [{group, emqx_auth_http}].

groups() ->
    [{emqx_auth_http, [sequence],
      [ t_check_acl
      , t_check_auth
      , t_sub_pub
      , t_comment_config]}
    ].

init_per_suite(Config) ->
    http_auth_server:start_http(),
    emqx_ct_helpers:start_apps([emqx_auth_http], fun set_special_configs/1),
    Config.

end_per_suite(_Config) ->
    http_auth_server:stop_http(),
    emqx_ct_helpers:stop_apps([emqx_auth_http, emqx]).


set_special_configs(emqx) ->
    application:set_env(emqx, allow_anonymous, true),
    application:set_env(emqx, enable_acl_cache, false),
    LoadedPluginPath = filename:join(["test", "emqx_SUITE_data", "loaded_plugins"]),
    application:set_env(emqx, plugins_loaded_file,
                        emqx_ct_helpers:deps_path(emqx, LoadedPluginPath));

set_special_configs(emqx_auth_http) ->
    AuthReq = maps:from_list(application:get_env(emqx_auth_http, auth_req, [])),
    SuprReq = maps:from_list(application:get_env(emqx_auth_http, super_req, [])),
    application:set_env(emqx_auth_http, auth_req, maps:to_list(AuthReq#{method := get})),
    application:set_env(emqx_auth_http, super_req, maps:to_list(SuprReq#{method := get}));
set_special_configs(_App) ->
    ok.

%%------------------------------------------------------------------------------
%% Testcases
%%------------------------------------------------------------------------------

t_check_acl(_) ->
    %ct:pal("all configs: ~p ", [application:get_all_env(?APP)]),
    %ct:pal("emqx all configs: ~p ", [application:get_all_env(emqx)]),
    SuperUser = ?USER(<<"superclient">>, <<"superuser">>, {{127,0,0,1}, 1883}, {{127, 0, 0, 1}, 2982}, external),
    deny = emqx_access_control:check_acl(SuperUser, subscribe, <<"users/testuser/1">>),
    deny = emqx_access_control:check_acl(SuperUser, publish, <<"anytopic">>),

    User1 = ?USER(<<"client1">>, <<"testuser">>, {{127,0,0,1}, 1883}, {{127,0,0,1}, 2981}, external),
    UnIpUser1 = ?USER(<<"client1">>, <<"testuser">>, {{127,0,0,1}, 1883}, {{192,168,0,4}, 2981}, external),
    UnClientIdUser1 = ?USER(<<"unkonwc">>, <<"testuser">>, {{127,0,0,1}, 1883}, {{127,0,0,1}, 2981}, external),
    UnnameUser1= ?USER(<<"client1">>, <<"unuser">>, {{127,0,0,1}, 1883}, {{127,0,0,1}, 2981}, external),
    allow = emqx_access_control:check_acl(User1, subscribe, <<"users/testuser/1">>),
    deny = emqx_access_control:check_acl(User1, publish, <<"users/testuser/1">>),
    deny = emqx_access_control:check_acl(UnIpUser1, subscribe, <<"users/testuser/1">>),
    deny = emqx_access_control:check_acl(UnClientIdUser1, subscribe, <<"users/testuser/1">>),
    deny  = emqx_access_control:check_acl(UnnameUser1, subscribe, <<"$SYS/testuser/1">>),

    User2 = ?USER(<<"client2">>, <<"xyz">>, {{127,0,0,1}, 1883}, {{127,0,0,1}, 2982}, external),
    UserC = ?USER(<<"client2">>, <<"xyz">>, {{127,0,0,1}, 1883}, {{192,168,1,3}, 2983}, external),
    allow = emqx_access_control:check_acl(UserC, publish, <<"a/b/c">>),
    deny = emqx_access_control:check_acl(User2, publish, <<"a/b/c">>),
    deny  = emqx_access_control:check_acl(User2, subscribe, <<"$SYS/testuser/1">>).

t_check_auth(_) ->
    User1 = ?USER(<<"client1">>, <<"testuser1">>, {{127,0,0,1}, 1883}, {{127,0,0,1}, 2981}, external, undefined),
    User2 = ?USER(<<"client2">>, <<"testuser2">>, {{127,0,0,1}, 1883}, {{127,0,0,1}, 2982}, exteneral, undefined),
    User3 = ?USER(<<"client3">>, undefined, {{127,0,0,1}, 1883}, {{127,0,0,1}, 2983}, exteneral, undefined),

    {ok, #{auth_result := success,
           anonymous := false,
           is_superuser := false}} = emqx_access_control:authenticate(User1#{password => <<"pass1">>}),
    {error, 404} = emqx_access_control:authenticate(User1#{password => <<"pass">>}),
    {error, 404} = emqx_access_control:authenticate(User1#{password => <<>>}),

    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(User2#{password => <<"pass2">>}),
    {error, 404} = emqx_access_control:authenticate(User2#{password => <<>>}),
    {error, 404} = emqx_access_control:authenticate(User2#{password => <<"errorpwd">>}),

    {error, 404} = emqx_access_control:authenticate(User3#{password => <<"pwd">>}).

t_sub_pub(_) ->
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

t_comment_config(_) ->
    AuthCount = length(emqx_hooks:lookup('client.authenticate')),
    AclCount = length(emqx_hooks:lookup('client.check_acl')),
    application:stop(?APP),
    [application:unset_env(?APP, Par) || Par <- [acl_req, auth_req]],
    application:start(?APP),
    ?assertEqual([], emqx_hooks:lookup('client.authenticate')),
    ?assertEqual(AuthCount - 1, length(emqx_hooks:lookup('client.authenticate'))),
    ?assertEqual(AclCount - 1, length(emqx_hooks:lookup('client.check_acl'))).

