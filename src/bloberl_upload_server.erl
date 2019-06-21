-module(bloberl_upload_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2,
        terminate/2, 
        code_change/3]).

-export([providers_caller/2,
        aws_s3/2,
        azure_blob/2,
        google_cloud_storage/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("enenra/include/enenra.hrl").

-define(S3_BUCKET, proplists:get_value(aws_s3_bucket, application:get_all_env(erlcloud))).
-define(AZ_CONTAINER, proplists:get_value(container, application:get_all_env(erlazure))).
-define(GCS_BUCKET, proplists:get_value(google_cloud_bucket, application:get_all_env(enenra))).

-define(UPLOAD_RETRY_COUNT, proplists:get_value(upload_retry_count, application:get_all_env(bloberl))).
-define(UPLOAD_RETRY_DELAY, proplists:get_value(upload_retry_delay, application:get_all_env(bloberl))).
-define(UPLOAD_MAX_RETRY_DELAY, proplists:get_value(upload_max_retry_delay, application:get_all_env(bloberl))).

start_link() ->
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    Return.

init([]) ->
    State = [],
    Return = {ok, State},
    Return.

handle_call(_Request, _From, State) ->
    Reply = ok,
    Return = {reply, Reply, State},
    Return.

handle_cast({table, L}, State) ->
    F = integer_to_list(os:system_time(nanosecond)) ++ ".gz",
    spawn(?MODULE, providers_caller, [L,F]),
    Return = {noreply, State},
    Return;
handle_cast(_Msg, State) ->
    Return = {noreply, State},
    Return.

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

providers_caller(L, F) ->
    aws_s3(L, F),
    azure_blob(L, F),
    google_cloud_storage(L, F).                

aws_s3(L, F) ->
    case proplists:get_value(aws_access_key_id, application:get_all_env(erlcloud)) of
        undefined ->
            do_nothing;
        _ ->
            upload(aws_s3, ?S3_BUCKET, F, L, ?UPLOAD_RETRY_COUNT, ?UPLOAD_RETRY_DELAY, ?UPLOAD_MAX_RETRY_DELAY)
    end.

azure_blob(L, F) ->
    case proplists:get_value(account, application:get_all_env(erlazure)) of
        undefined ->
            do_nothing;
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
            upload(azure_blob, ?AZ_CONTAINER, F, L, ?UPLOAD_RETRY_COUNT, ?UPLOAD_RETRY_DELAY, ?UPLOAD_MAX_RETRY_DELAY)
    end.

google_cloud_storage(L, F) ->
    case proplists:get_value(google_cloud_credentials, application:get_all_env(enenra)) of
        undefined ->
            do_nothing;
        _ ->
            upload(google_cloud_storage, ?GCS_BUCKET, F, L, ?UPLOAD_RETRY_COUNT, ?UPLOAD_RETRY_DELAY, ?UPLOAD_MAX_RETRY_DELAY)
    end.

upload(Provider, _, F, _, 0, _, _) ->
    ?LOG_ERROR("final error trying to ship ~p to ~p - giving up", [F, Provider]);
upload(Provider, Bucket, F, L, RetriesLeft, RetryDelayMs, MaxRetryDelayMs) when RetriesLeft > 0 ->
    try
        case Provider of
            aws_s3 -> erlcloud_s3:put_object(Bucket, F, L, [], []);
            azure_blob -> erlazure:put_block_blob(erlang:whereis(erlazure), Bucket, F, L);
            google_cloud_storage ->
                {ok, Credentials} = application:get_env(enenra, google_cloud_credentials),
                {ok, Creds} = enenra:load_credentials(Credentials),
                Md5 = crypto:hash(md5, L),
                enenra:upload_data(L, #object{
                    name = list_to_binary(F),
                    bucket = list_to_binary(Bucket),
                    contentType = <<"application/gzip">>,
                    md5Hash = base64:encode(Md5),
                    size = byte_size(L)}, Creds);
            _ -> do_nothing
        end,
        ?LOG_NOTICE("shipped ~p to ~p", [F, Provider])
    catch
        _:_ ->
            ?LOG_ERROR("error trying to ship ~p to ~p - try #~p", [F, Provider, RetriesLeft]),
            timer:sleep(min(RetryDelayMs, MaxRetryDelayMs)),
            upload(Provider, Bucket, F, L, RetriesLeft - 1, 2 * RetryDelayMs, MaxRetryDelayMs)
    end.