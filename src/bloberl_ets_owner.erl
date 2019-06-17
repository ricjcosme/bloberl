-module(bloberl_ets_owner).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2,
        terminate/2, 
        code_change/3]).

-export([ship/0, closed_tables/0, fire_it_up/1]).

-include_lib("kernel/include/logger.hrl").

-define(INTERVAL, proplists:get_value(shipping_interval, application:get_all_env(bloberl))).

start_link() ->
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    %% io:format("start_link: ~p~n", [Return]),
    Return.

init([]) ->
    bloberl_upload_server:start_link(),
    State = [],
    erlang:send_after(?INTERVAL, self(), {trigger, start}),
    Return = {ok, State},
    Return.

handle_call(_Request, _From, State) ->
    Reply = ok,
    Return = {reply, Reply, State},
    Return.

handle_cast(_Msg, State) ->
    Return = {noreply, State},
    Return.

handle_info({trigger, _}, State) ->
    Return = {noreply, State},
    % io:format("~p~n", [os:system_time(nanosecond)]),
    % io:format("~p~n", [readram()]),
    ship(),
    erlang:send_after(?INTERVAL, self(), {trigger, rerun}),
    Return;
handle_info(_Info, State) ->
    Return = {noreply, State},
    Return.

terminate(_Reason, _State) ->
    Return = ok,
    io:format("terminate: ~p~n", [Return]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    Return.

ship() ->
    ClosedTables = closed_tables(),
    case ClosedTables of
        [] ->
            do_nothing;
        _ ->
            lists:foreach(fun (T) ->
                    TabName = list_to_atom(T),
                    spawn(?MODULE, fire_it_up, [TabName])
                end, ClosedTables)
    end.

closed_tables() ->
    [L || L <- [string:find(atom_to_list(X),"closed_t_") || X <- ets:all(), is_atom(X)], L /= nomatch].

fire_it_up(TabName) ->
    L = [X || {_, X} <- ets:lookup(TabName, m)],
    CData = zlib:gzip(L),
    gen_server:cast(whereis(bloberl_upload_server), {table, CData}),
    ets:delete(TabName).
