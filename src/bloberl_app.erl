%%%-------------------------------------------------------------------
%% @doc bloberl public API
%% @end
%%%-------------------------------------------------------------------

-module(bloberl_app).

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% Internal functions
-export([set_env/4]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->

    %%====================================================================
    %% S3
    %%====================================================================
    set_env(os:getenv("AWS_ACCESS_KEY_ID"), erlcloud, aws_access_key_id, undefined),
    set_env(os:getenv("AWS_SECRET_ACCESS_KEY"), erlcloud, aws_secret_access_key, undefined),
    set_env(os:getenv("AWS_DEFAULT_REGION"), erlcloud, aws_region, undefined),
    set_env(os:getenv("AWS_S3_BUCKET"), erlcloud, aws_s3_bucket, undefined),
    application:ensure_all_started(erlcloud),

    %%====================================================================
    %% Azure Blob
    %%====================================================================
    set_env(os:getenv("AZURE_STORAGE_ACCOUNT"), erlazure, account, undefined),
    set_env(os:getenv("AZURE_BLOB_STORAGE_KEY"), erlazure, key, undefined),
    set_env(os:getenv("AZURE_BLOB_CONTAINER"), erlazure, container, undefined),
    case application:get_env(erlazure, account) of 
        undefined ->
            ok;
        {ok, AzAcc} ->
            {ok, AzKey} = application:get_env(erlazure, key),
            {ok, Pid} = erlazure:start(AzAcc, AzKey),
            register(erlazure, Pid)
    end,

    case {proplists:get_value(account, application:get_all_env(erlazure)),
            proplists:get_value(aws_access_key_id, application:get_all_env(erlcloud))} of
                {undefined, undefined} ->
                    ?LOG_NOTICE("none of aws S3 or azure blob is defined - shutting down", []),
                    init:stop(0);
                {_, _} ->
                    continue
    end,

    %%====================================================================
    %% Internal settings
    %%====================================================================
    set_env(os:getenv("PORT"), bloberl, port, integer),
    set_env(os:getenv("TCP_CLIENT_TIMEOUT"), bloberl, tcp_client_timeout, integer),
    set_env(os:getenv("MAX_RECORDS_PER_TABLE"), bloberl, max_records_per_table, integer),
    set_env(os:getenv("MAX_MEMORY_PER_TABLE"), bloberl, max_memory_per_table, integer),
    set_env(os:getenv("SHIPPING_INTERVAL"), bloberl, shipping_interval, integer),

    bloberl_sup:start_link(),
    bloberl_tcp_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
set_env(Var, App, Key, Type) when Var /= false, Type == undefined ->
    application:set_env(App, Key, Var);
set_env(Var, App, Key, Type) when Var /= false, Type /= undefined ->
    case Type of
        integer ->
            _Var = list_to_integer(Var);
        _ ->
            _Var = Var
    end,
    application:set_env(App, Key, _Var);
set_env(Var, _, _, _) when Var == false ->
    ok.
