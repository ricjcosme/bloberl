-module(bloberl_tcp_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([send_to_dispatcher/1, 
        table_limits/6,
        close_table/3]).

-include_lib("kernel/include/logger.hrl").

-define(TIMEOUT, proplists:get_value(tcp_client_timeout, application:get_all_env(bloberl))).
-define(MAX_RECORDS_PER_TABLE, proplists:get_value(max_records_per_table, application:get_all_env(bloberl))).
-define(MAX_MEMORY_PER_TABLE, proplists:get_value(max_memory_per_table, application:get_all_env(bloberl))).

-record(state, {socket}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

handle_cast(accept, State = #state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    bloberl_tcp_sup:start_socket(),
    {noreply, State#state{socket=AcceptSocket}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({tcp, Socket, "quit"++_}, State) ->
    gen_tcp:close(Socket),
    {stop, normal, State};
handle_info({tcp, Socket, Msg}, State) ->
    send_to_dispatcher(Msg),
    TList = pid_to_list(self()),
    TAtom = list_to_atom(TList),
    Info = ets:info(TAtom),
    table_limits(proplists:get_value(size, Info),
                    proplists:get_value(memory, Info),
                    TList,
                    TAtom,
                    ?MAX_RECORDS_PER_TABLE,
                    ?MAX_MEMORY_PER_TABLE),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State, ?TIMEOUT};
handle_info({tcp_closed, _Socket}, State) -> 
    TList = pid_to_list(self()),
    TAtom = list_to_atom(TList),
    close_table(TList, TAtom, "tcp closed"),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) -> {stop, normal, State};
handle_info(timeout, State) -> 
    TList = pid_to_list(self()),
    TAtom = list_to_atom(TList),
    close_table(TList, TAtom, "client timeout"),
    {stop, normal, State};
handle_info(E, State) ->
    io:fwrite("unexpected: ~p~n", [E]),
    {noreply, State}.

handle_call(_E, _From, State) -> {noreply, State}.
terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

send_to_dispatcher(Msg) ->
    bloberl_dispatcher:dispatch(Msg, self()).

table_limits(Records, _, TList, TAtom, MaxRecords, _) when Records == MaxRecords ->
    close_table(TList, TAtom, "record count");
table_limits(_, Mem, TList, TAtom, _, MaxMem) when Mem >= MaxMem ->
    close_table(TList, TAtom, "mem");
table_limits(_, _, _, _, _, _) ->
    ok.

close_table(TList, TAtom, Reason) ->
    TClosed = list_to_atom("closed_t_" ++ TList ++ integer_to_list(os:system_time(nanosecond))),
    bloberl_dispatcher:rename(TAtom, TClosed),
    ?LOG_NOTICE("renamed or closed table ~p to ~p reason: ~p", [TAtom, TClosed, Reason]).    