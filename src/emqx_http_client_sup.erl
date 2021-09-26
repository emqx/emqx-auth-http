-module(emqx_http_client_sup).

-behaviour(supervisor).

-include("emqx_auth_http.hrl").

-export([ start_link/2
        , init/1
        , stop_pool/1
        ]).

start_link(Pool, Opts) ->
    supervisor:start_link(?MODULE, [Pool, Opts]).

init([Pool, Opts]) ->
    PoolSize = pool_size(Opts),
    PoolSpec = ecpool:pool_spec(?APP, Pool, emqx_http_client, Opts ++ [{size, PoolSize}]),
    {ok, {{one_for_one, 10, 100}, [PoolSpec]}}.

pool_size(Opts) ->
    Schedulers = erlang:system_info(schedulers),
    proplists:get_value(pool_size, Opts, Schedulers).

stop_pool(Pool) ->
    ecpool:stop_sup_pool(Pool).
