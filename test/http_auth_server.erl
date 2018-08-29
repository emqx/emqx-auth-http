-module(http_auth_server).

-compile(export_all).

-define(SUPERUSER, [[{"username", "superuser"}, {"clientid", "superclient"}]]).

-define(ACL, [[{<<"username">>, <<"testuser">>},
               {<<"clientid">>, <<"client1">>},
               {<<"access">>, <<"1">>},
               {<<"topic">>, <<"users/testuser/1">>},
               {<<"ipaddr">>, <<"127.0.0.1">>}],
              [{<<"username">>, <<"xyz">>},
               {<<"clientid">>, <<"client2">>},
               {<<"access">>, <<"2">>},
               {<<"topic">>, <<"a/b/c">>},
               {<<"ipaddr">>, <<"192.168.1.3">>}],
              [{<<"username">>, <<"testuser1">>},
               {<<"clientid">>, <<"client1">>},
               {<<"access">>, <<"2">>},
               {<<"topic">>, <<"topic">>},
               {<<"ipaddr">>, <<"127.0.0.1">>}],
              [{<<"username">>, <<"testuser2">>},
               {<<"clientid">>, <<"client2">>},
               {<<"access">>, <<"1">>},
               {<<"topic">>, <<"topic">>},
               {<<"ipaddr">>, <<"127.0.0.1">>}]]).

-define(AUTH, [[{<<"clientid">>, <<"client1">>},
                {<<"username">>, <<"testuser1">>},
                {<<"password">>, <<"pass1">>}],
               [{<<"clientid">>, <<"client2">>},
                {<<"username">>, <<"testuser2">>},
                {<<"password">>, <<"pass2">>}]]).

%%%%%%%start http listen%%%%%%%%%%%%%%%%%%%%%
start_http() ->
    %process_flag(trap_exit, true),
    io:format("start http~n", []),
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [
              {"/", ?MODULE, []}
            , {"/mqtt/acl", ?MODULE, []}
            , {"/mqtt/auth", ?MODULE, []}
        ]}
    ]),
    {ok, _Pid} = cowboy:start_clear(http, [{port, 8991}], #{
        env => #{dispatch => Dispatch}
    }).

stop_http() ->
    cowboy:stop_listener(http).

init(Req, Opts) ->
    io:format("init Req: ~p~n", [Req]),
    Req1 = handle_request(Req),
    {ok, Req1, Opts}.

handle_request(Req) ->
    Method =cowboy_req:method(Req),
    Params =
        case Method of
            <<"GET">> -> cowboy_req:parse_qs(Req);
            <<"POST">> ->
                {ok, PostVals, _Req2} = cowboy_req:read_urlencoded_body(Req),
                PostVals
        end,

    AllowDeny = handle_request(cowboy_req:path(Req),
                               Params),
    io:format("Method: ~p, Param: ~p, Result: ~p ~n", [Method, Params, AllowDeny]),
    reply(Req, AllowDeny).

handle_request(<<"/mqtt/superuser">>, Params) ->
    check(Params, ?SUPERUSER);
handle_request(<<"/mqtt/acl">>, Params) ->
    check(Params, ?ACL);
handle_request(<<"/mqtt/auth">>, Params) ->
    check(Params, ?AUTH).

reply(Req, allow) ->
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"hello">>, Req);
reply(Req, deny) ->
    cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>}, <<"deny">>, Req).

check(_Params, []) ->
    ct:pal("check result: deny~n"),
    deny;
check(Params, [ConfRecord|T]) ->
    ct:pal("Params: ~p, ConfRecord:~p ~n", [Params, ConfRecord]),
    case match_config(Params, ConfRecord) of
        not_match ->
            check(Params, T);
        matched -> allow
     end.

match_config([], _ConfigColumn) ->
    %ct:pal("match_config result: matched~n"),
    matched;

match_config([Param|T], ConfigColumn) ->
    %ct:pal("Param: ~p, ConfigColumn:~p ~n", [Param, ConfigColumn]),
    case lists:member(Param, ConfigColumn) of
        true ->
            match_config(T, ConfigColumn);
        false ->
           not_match
    end.
