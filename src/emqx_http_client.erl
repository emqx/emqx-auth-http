-module(emqx_http_client).

-behaviour(gen_server).

-behaviour(ecpool_worker).

-include_lib("emqx/include/logger.hrl").

%% APIs
-export([ start_link/1
        , request/3
        , request/4
        , connect/1
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
          client    :: pid() | undefined,
          mref      :: reference() | undefined,
          host      :: inet:hostname() | inet:ip_address(),
          port      :: inet:port_number(),
          enable_pipelining :: boolean(),
          gun_opts  :: proplists:proplist(),
          gun_state :: down | up,
          requests  :: map()
         }).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).

request(Method, PoolName, Req) ->
    request(Method, PoolName, Req, 5000, 3).

request(Method, PoolName, Req, Timeout) ->
    request(Method, PoolName, Req, Timeout, 3).

request(Method, PoolName, Request, Timeout, Retry) ->
    ExpireAt = now_() + Timeout,
    try call(PoolName, {Method, Request, ExpireAt}, Timeout + 1000) of
        %% gun will reply {gun_down, _Client, _, normal, _KilledStreams, _} message
        %% when connection closed by keepalive
        {error, Reason} ->
            case Retry - 1 =:= 0 of
                true ->
                    {error, Reason};
                false ->
                    request(Method, PoolName, Request, Timeout, Retry - 1)
            end;
        Other ->
            Other
    catch
        exit:{timeout, _Details} ->
            {error, timeout}
    end.

connect(Options) ->
    emqx_http_client:start_link(Options).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Opts]) ->
    State = #state{client = undefined,
                   mref = undefined,
                   host = proplists:get_value(host, Opts),
                   port = proplists:get_value(port, Opts),
                   enable_pipelining = proplists:get_value(enable_pipelining, Opts, false),
                   gun_opts = gun_opts(Opts),
                   gun_state = down,
                   requests = #{}},
    {ok, State}.

handle_call(Request = {_, _, _}, From, State = #state{client = undefined, gun_state = down}) ->
    case open(State) of
        {ok, NewState} ->
            handle_call(Request, From, NewState);
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(Request = {_, _, ExpireAt}, From, State = #state{client = Client, mref = MRef, gun_state = down}) when is_pid(Client) ->
    case (Timeout = ExpireAt - now_()) > 0 of
        true ->
            case gun:await_up(Client, Timeout, MRef) of
                {ok, _} ->
                    handle_call(Request, From, State#state{gun_state = up});
                {error, timeout} ->
                    {reply, {error, timeout}, State};
                {error, Reason} ->
                    true = erlang:demonitor(MRef, [flush]),
                    {reply, {error, Reason}, State#state{client = undefined, mref = undefined}}
            end;
        false ->
            {noreply, State}
    end;

handle_call({Method, Request, ExpireAt}, From, State = #state{client = Client,
                                                              requests = Requests,
                                                              enable_pipelining = EnablePipelining,
                                                              gun_state = up}) when is_pid(Client) ->
    StreamRef = do_request(Client, Method, Request),
    case EnablePipelining of
        true ->
            {noreply, State#state{requests = maps:put(StreamRef, {From, ExpireAt, undefined}, Requests)}};
        false ->
            await_response(StreamRef, ExpireAt, State)
    end;

handle_call(Request, _From, State) ->
    ?LOG(error, "Unexpected call: ~p", [Request]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    ?LOG(error, "Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({gun_response, Client, _StreamRef, _IsFin, _StatusCode, _Headers}, State = #state{client = Client, enable_pipelining = false}) ->
    {noreply, State};
handle_info({gun_response, Client, StreamRef, IsFin, StatusCode, Headers}, State = #state{client = Client, requests = Requests, enable_pipelining = true}) ->
    Now = now_(),
    case maps:take(StreamRef, Requests) of
        error ->
            ?LOG(error, "Received 'gun_response' message from unknown stream ref: ~p", [StreamRef]),
            {noreply, State};
        {{_, ExpireAt, _}, NRequests} when Now > ExpireAt ->
            cancel_stream(Client, StreamRef),
            {noreply, State#state{requests = NRequests}};
        {{From, ExpireAt, undefined}, NRequests} ->
            case IsFin of
                fin ->
                    gen_server:reply(From, {ok, StatusCode, Headers}),
                    {noreply, State#state{requests = NRequests}};
                nofin ->
                    {noreply, State#state{requests = NRequests#{StreamRef => {From, ExpireAt, {StatusCode, Headers, <<>>}}}}}
            end;
        _ ->
            ?LOG(error, "Received 'gun_response' message does not match the state", []),
            {noreply, State}
    end;

handle_info({gun_data, Client, _StreamRef, _IsFin, _Data}, State = #state{client = Client, enable_pipelining = false}) ->
    {noreply, State};
handle_info({gun_data, Client, StreamRef, IsFin, Data}, State = #state{client = Client, requests = Requests, enable_pipelining = true}) ->
    Now = now_(),
    case maps:take(StreamRef, Requests) of
        error ->
            ?LOG(error, "Received 'gun_data' message from unknown stream ref: ~p", [StreamRef]),
            {noreply, State};
        {{_, ExpireAt, _}, NRequests} when Now > ExpireAt ->
            cancel_stream(Client, StreamRef),
            {noreply, State#state{requests = NRequests}};
        {{From, ExpireAt, {StatusCode, Headers, Acc}}, NRequests} ->
            case IsFin of
                fin ->
                    gen_server:reply(From, {ok, StatusCode, Headers, <<Acc/binary, Data/binary>>}),
                    {noreply, State#state{requests = NRequests}};
                nofin ->
                    {noreply, State#state{requests = NRequests#{StreamRef => {From, ExpireAt, {StatusCode, Headers, <<Acc/binary, Data/binary>>}}}}}
            end;
        _ ->
            ?LOG(error, "Received 'gun_data' message does not match the state", []),
            {noreply, State}
    end;

handle_info({gun_error, Client, _StreamRef, _Reason}, State = #state{client = Client, enable_pipelining = false}) ->
    {noreply, State};
handle_info({gun_error, Client, StreamRef, Reason}, State = #state{client = Client, requests = Requests, enable_pipelining = true}) ->
    Now = now_(),
    case maps:take(StreamRef, Requests) of
        error ->
            ?LOG(error, "Received 'gun_error' message from unknown stream ref: ~p~n", [StreamRef]),
            {noreply, State};
        {{_, ExpireAt, _}, NRequests} when Now > ExpireAt ->
            {noreply, State#state{requests = NRequests}};
        {{From, _, _}, NRequests} ->
            gen_server:reply(From, {error, Reason}),
            {noreply, State#state{requests = NRequests}}
    end;

handle_info({gun_up, Client, _}, State = #state{client = Client}) ->
    {noreply, State#state{gun_state = up}};

handle_info({gun_down, Client, _, _Reason, _KilledStreams, _}, State = #state{client = Client, enable_pipelining = false}) ->
    {noreply, State#state{gun_state = down, requests = #{}}};
handle_info({gun_down, Client, _, Reason, KilledStreams, _}, State = #state{client = Client, requests = Requests, enable_pipelining = true}) ->
    Reason =/= normal andalso Reason =/= closed andalso ?LOG(warning, "Received 'gun_down' message with reason: ~p", [Reason]),
    Now = now_(),
    NRequests = lists:foldl(fun(StreamRef, Acc) ->
                                case maps:take(StreamRef, Acc) of
                                    error ->
                                        Acc;
                                    {{_, ExpireAt, _}, NAcc} when Now > ExpireAt ->
                                        NAcc;
                                    {{From, _, _}, NAcc} ->
                                        gen_server:reply(From, {error, Reason}),
                                        NAcc
                                end
                            end, Requests, KilledStreams),
    {noreply, State#state{gun_state = down, requests = NRequests}};

handle_info({'DOWN', MRef, process, Client, Reason}, State = #state{mref = MRef, client = Client, requests = Requests, enable_pipelining = EnablePipelining}) ->
    true = erlang:demonitor(MRef, [flush]),
    case EnablePipelining of
        true ->
            Now = now_(),
            lists:foreach(fun({_, {_, ExpireAt, _}}) when Now > ExpireAt ->
                              ok;
                             ({_, {From, _, _}}) ->
                              gen_server:reply(From, {error, Reason})
                          end, maps:to_list(Requests));
        false ->
            ok
    end,
    case open(State#state{requests = #{}}) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, _Reason} ->
            {noreply, State#state{mref = undefined, client = undefined}}
    end;

handle_info(Info, State) ->
    ?LOG(error, "Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
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
                     %% The keepalive mechanism of gun will send "\r\n" for keepalive,
                     %% which may cause misjudgment by some servers, so we disabled it by default
                     http_opts => #{keepalive => infinity},
                     protocols => [http]}).

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

call(PoolName, Msg, Timeout) ->
    ecpool:with_client(PoolName, fun(C) ->
        gen_server:call(C, Msg, Timeout)
    end).

do_request(Client, get, {Path, Headers}) ->
    gun:get(Client, Path, Headers);
do_request(Client, post, {Path, Headers, Body}) ->
    gun:post(Client, Path, Headers, Body);
do_request(Client, put, {Path, Headers, Body}) ->
    gun:put(Client, Path, Headers, Body);
do_request(Client, delete, {Path, Headers}) ->
    gun:delete(Client, Path, Headers).

cancel_stream(Client, StreamRef) ->
    gun:cancel(Client, StreamRef),
    flush_stream(Client, StreamRef).

flush_stream(Client, StreamRef) ->
    receive
        {gun_response, Client, StreamRef, _, _, _} ->
            flush_stream(Client, StreamRef);
        {gun_data, Client, StreamRef, _, _} ->
            flush_stream(Client, StreamRef);
        {gun_error, Client, StreamRef, _} ->
            flush_stream(Client, StreamRef)
	after 0 ->
		ok
	end.

await_response(StreamRef, ExpireAt, State = #state{client = Client}) ->
    case (Timeout = ExpireAt - now_()) > 0 of
        true ->
            receive
                {gun_response, Client, StreamRef, fin, StatusCode, Headers} ->
                    {reply, {ok, StatusCode, Headers}, State};
                {gun_response, Client, StreamRef, nofin, StatusCode, Headers} ->
                    await_remaining_response(StreamRef, ExpireAt, State, {StatusCode, Headers, <<>>});
                {gun_error, Client, StreamRef, Reason} ->
                    {reply, {error, Reason}, State};
                {gun_down, Client, _, Reason, _KilledStreams, _} ->
                    {reply, {error, Reason}, State};
                {'DOWN', MRef, process, Client, Reason} ->
                    true = erlang:demonitor(MRef, [flush]),
                    NState = case open(State) of
                                {ok, State1} -> State1;
                                {error, _Reason} -> State#state{mref = undefined, client = undefined}
                            end,
                    {reply, {error, Reason}, NState}
            after Timeout ->
                cancel_stream(Client, StreamRef),
                {reply, {error, timeout}, State}
            end;
        false ->
            cancel_stream(Client, StreamRef),
            {noreply, State}
    end.

await_remaining_response(StreamRef, ExpireAt, State = #state{client = Client, mref = MRef}, {StatusCode, Headers, Acc}) ->
    case (Timeout = ExpireAt - now_()) > 0 of
        true ->
            receive
                {gun_data, Client, StreamRef, fin, Data} ->
                    {reply, {ok, StatusCode, Headers, <<Acc/binary, Data/binary>>}, State};
                {gun_data, Client, StreamRef, nofin, Data} ->
                    await_remaining_response(StreamRef, ExpireAt, State, {StatusCode, Headers, <<Acc/binary, Data/binary>>});
                {gun_error, Client, StreamRef, Reason} ->
                    {reply, {error, Reason}, State};
                {gun_down, Client, _, Reason, _KilledStreams, _} ->
                    {reply, {error, Reason}, State};
                {'DOWN', MRef, process, Client, Reason} ->
                    true = erlang:demonitor(MRef, [flush]),
                    NState = case open(State) of
                                {ok, State1} -> State1;
                                {error, _Reason} -> State#state{mref = undefined, client = undefined}
                            end,
                    {reply, {error, Reason}, NState}
            after Timeout ->
                cancel_stream(Client, StreamRef),
                {reply, {error, timeout}, State}
            end;
        false ->
            cancel_stream(Client, StreamRef),
            {noreply, State}
    end.

now_() ->
    erlang:system_time(millisecond).
