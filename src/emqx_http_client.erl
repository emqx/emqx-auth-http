-module(emqx_http_client).

-behaviour(gen_server).

-include_lib("emqx/include/logger.hrl").

%% APIs
-export([ start_link/3
        , request/3
        , request/4
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {
          pool      :: ecpool:poo_name(),
          id        :: pos_integer(),
          client    :: pid() | undefined,
          mref      :: reference() | undefined,
          host      :: inet:hostname() | inet:ip_address(),
          port      :: inet:port_number(),
          gun_opts  :: proplists:proplist(),
          gun_state :: down | up
         }).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

start_link(Pool, Id, Opts) ->
    gen_server:start_link(?MODULE, [Pool, Id, Opts], []).

request(Method, Pool, Req) ->
    request(Method, Pool, Req, 5000).

request(get, Pool, {Path, Headers}, Timeout) ->
    call(pick(Pool), {get, {Path, Headers}, Timeout}, Timeout + 1000);
request(Method, Pool, {Path, Headers, Body}, Timeout) ->
    call(pick(Pool), {Method, {Path, Headers, Body}, Timeout}, Timeout + 1000).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Pool, Id, Opts]) ->
    State = #state{pool = Pool,
                   id = Id,
                   client = undefined,
                   mref = undefined,
                   host = proplists:get_value(host, Opts),
                   port = proplists:get_value(port, Opts),
                   gun_opts = gun_opts(Opts),
                   gun_state = down},
    true = gproc_pool:connect_worker(Pool, {Pool, Id}),
    {ok, State}.

handle_call(Req = {_, _, _}, From, State = #state{client = undefined, gun_state = down}) ->
    case open(State) of
        {ok, NewState} ->
            handle_call(Req, From, NewState);
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(Req = {_, _, Timeout}, From, State = #state{client = Client, mref = MRef, gun_state = down}) when is_pid(Client) ->
    case gun:await_up(Client, Timeout, MRef) of
        {ok, _} ->
            handle_call(Req, From, State#state{gun_state = up});
        {error, timeout} ->
            {reply, {error, timeout}, State};
        {error, Reason} ->
            true = erlang:demonitor(MRef, [flush]),
            {reply, {error, Reason}, State#state{client = undefined, mref = undefined}}
    end;

handle_call({Method, Request, Timeout}, _From, State = #state{client = Client, gun_state = up}) when is_pid(Client) ->
    StreamRef = do_request(Client, Method, Request),
    await_response(StreamRef, Timeout, State);

handle_call(Req, _From, State) ->
    ?LOG(error, "Unexpected call: ~p", [Req]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    ?LOG(error, "Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({gun_up, Client, _}, State = #state{client = Client}) ->
    {noreply, State#state{gun_state = up}};

handle_info({gun_down, Client, _, _Reason, _, _}, State = #state{client = Client}) ->
    {noreply, State#state{gun_state = down}};

handle_info({'DOWN', MRef, process, Client, Reason}, State = #state{mref = MRef, client = Client}) ->
    true = erlang:demonitor(MRef, [flush]),
    case open(State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            %% TODO: print warning log
            {noreply, State#state{mref = undefined, client = undefined}}
    end;

handle_info(Info, State) ->
    ?LOG(error, "Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{pool = Pool, id = Id}) ->
    gropc:disconnect_worker(Pool, {Pool, Id}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

open(State = #state{host = Host, port = Port, gun_opts = GunOpts}) ->
    case gun:open(Host, Port, GunOpts) of
        {ok, ConnPid} when is_pid(ConnPid) ->
            MRef = monitor(process, ConnPid),
            {ok, State#state{mref = MRef, client = ConnPid}};
        {error, Reason} ->
            {error, Reason}
    end.

gun_opts(Opts) ->
    gun_opts(Opts, #{retry => 5,
                     retry_timeout => 1000,
                     connect_timeout => 5000,
                     protocols => [http],
                     http_opts => #{keepalive => infinity}}).

gun_opts([], Acc) ->
    Acc;
gun_opts([{retry, Retry} | Opts], Acc) ->
    gun_opts(Opts, Acc#{retry => Retry});
gun_opts([{retry_timeout, RetryTimeout} | Opts], Acc) ->
    gun_opts(Opts, Acc#{retry_timeout => RetryTimeout});
gun_opts([{connect_timeout, ConnectTimeout} | Opts], Acc) ->
    gun_opts(Opts, Acc#{connect_timeout => ConnectTimeout});
gun_opts([{transport, Transport} | Opts], Acc) ->
    gun_opts(Opts, Acc#{transport => Transport});
gun_opts([{transport_opts, TransportOpts} | Opts], Acc) ->
    gun_opts(Opts, Acc#{transport_opts => TransportOpts});
gun_opts([_ | Opts], Acc) ->
    gun_opts(Opts, Acc).

call(ChannPid, Msg, Timeout) ->
    gen_server:call(ChannPid, Msg, Timeout).

pick(Pool) ->
    gproc_pool:pick_worker(Pool).

do_request(Client, get, {Path, Headers}) ->
    gun:get(Client, Path, Headers);
do_request(Client, post, {Path, Headers, Body}) ->
    gun:post(Client, Path, Headers, Body).

await_response(StreamRef, Timeout, State = #state{client = Client, mref = MRef}) ->
    receive
        {gun_response, Client, StreamRef, fin, StatusCode, Headers} ->
            {reply, {ok, StatusCode, Headers}};
        {gun_response, Client, StreamRef, nofin, StatusCode, Headers} ->
            await_body(StreamRef, Timeout, {StatusCode, Headers, <<>>}, State);
        {gun_error, Client, StreamRef, Reason} ->
            {reply, {error, Reason}, State};
        {gun_down, Client, _, Reason, _, _} ->
            {reply, {error, Reason}, State};
        {'DOWN', MRef, process, Client, Reason} ->
            true = erlang:demonitor(MRef, [flush]),
            {reply, {error, Reason}, State#state{client = undefined, mref = undefiend}}
    after Timeout ->
        gun:cancel(Client, StreamRef),
        {reply, {error, timeout}, State}
    end.

await_body(StreamRef, Timeout, {StatusCode, Headers, Acc}, State = #state{client = Client, mref = MRef}) ->
    receive
        {gun_data, Client, StreamRef, fin, Data} ->
            {reply, {ok, StatusCode, Headers, << Acc/binary, Data/binary >>}, State};
        {gun_data, Client, StreamRef, nofin, Data} ->
            await_body(StreamRef, Timeout, {StatusCode, Headers, << Acc/binary, Data/binary >>}, State);
        {gun_error, Client, StreamRef, Reason} ->
            {reply, {error, Reason}, State};
        {gun_down, Client, _, Reason, _, _} ->
            {reply, {error, Reason}, State};
        {'DOWN', MRef, process, Client, Reason} ->
            true = erlang:demonitor(MRef, [flush]),
            {reply, {error, Reason}, State#state{client = undefined, mref = undefiend}}
    after Timeout ->
        gun:cancel(Client, StreamRef),
        {reply, {error, timeout}, State}
    end.