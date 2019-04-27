-module(bloberl_ets_owner).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2,
        terminate/2, 
        code_change/3]).

-export([s3/1, azure_blob/1, ship/0, closed_tables/0]).

-include_lib("kernel/include/logger.hrl").

-define(INTERVAL, proplists:get_value(shipping_interval, application:get_all_env(bloberl))).
-define(S3_BUCKET, proplists:get_value(aws_s3_bucket, application:get_all_env(erlcloud))).
-define(AZ_CONTAINER, proplists:get_value(container, application:get_all_env(erlazure))).

start_link() ->
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    io:format("start_link: ~p~n", [Return]),
    Return.

init([]) ->
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
            ?LOG_NOTICE("nothing to ship", []);
        _ ->
            lists:foreach(fun (L) ->
                    s3(list_to_atom(L)),
                    azure_blob(list_to_atom(L)),
                    ets:delete(list_to_atom(L)) 
                end, ClosedTables)
    end.

closed_tables() ->
    [L || L <- [string:find(atom_to_list(X),"closed_t_") || X <- ets:all(), is_atom(X)], L /= nomatch].

s3(Tab) ->
    case proplists:get_value(aws_access_key_id, application:get_all_env(erlcloud)) of
        undefined ->
            ?LOG_NOTICE("AWS_ACCESS_KEY_ID not defined - not shipping to S3", []);
        _ ->
            L = [X || {_, X} <- ets:lookup(Tab, m)],
            CData = zlib:gzip(L),
            F = integer_to_list(os:system_time(nanosecond)) ++ ".gz",
            erlcloud_s3:put_object(?S3_BUCKET, F, CData, [], []),
            ?LOG_NOTICE("shipped ~p to S3", [F])
    end.

azure_blob(Tab) ->
    case proplists:get_value(account, application:get_all_env(erlazure)) of
        undefined ->
            ?LOG_NOTICE("AZURE_STORAGE_ACCOUNT not defined - not shipping to Azure Blob", []);
        _ ->
            case erlang:whereis(erlazure) of 
                undefined ->
                    {ok, AzureStorageAccount} = application:get_env(erlazure, account),
                    {ok, AzureStorageKey} = application:get_env(erlazure, key),
                    {ok, Pid} = erlazure:start(AzureStorageAccount, AzureStorageKey),
                    register(erlazure, Pid);
                _ ->
                    ok
            end,
            L = [X || {_, X} <- ets:lookup(Tab, m)],
            CData = zlib:gzip(L),
            F = integer_to_list(os:system_time(nanosecond)) ++ ".gz",
            erlazure:put_block_blob(erlang:whereis(erlazure), 
                                        ?AZ_CONTAINER, 
                                        F, 
                                        CData),
            ?LOG_NOTICE("shipped ~p to Azure Blob", [F])
    end.