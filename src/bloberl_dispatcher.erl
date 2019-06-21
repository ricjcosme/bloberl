-module(bloberl_dispatcher).

-export([start/0, 
         print_ets/0, 
         dispatch/2, 
         write_message/5,
         insert/2,
         delete/1, 
         rename/2,
         write_temp_file/2]). 

-include_lib("kernel/include/logger.hrl").

start() -> 
   Pid = spawn(?MODULE, dispatch, []), 
   io:format("~p",[Pid]).

print_ets() ->
   [io:format("~p~n", [X]) || X <- bloberl_kv:take(m)].

dispatch(L, From) ->
   % Z=zlib:open(),
   % zlib:deflateInit(Z),
   % CData = zlib:deflate(Z, L, finish),
   % zlib:deflateEnd(Z),
   
   % Z1=zlib:open(),
   % zlib:inflateInit(Z1),
   % Data=zlib:inflate(Z1, CData),
   % zlib:inflateEnd(Z1),
   % io:format("~p",[Data]).
   Table = list_to_atom(pid_to_list(From)),
   write_message(Table, L, 20, 20, 100).

write_message(To, Message, 0, _, _) ->
   insert(To, Message);
write_message(To, Message, RetriesLeft, RetryDelayMs, MaxRetryDelayMs) ->
   try
      insert(To, Message)
   catch
      _:_ ->
         case ets:whereis(To) of 
            undefined -> 
               ets:new(To,[duplicate_bag, 
                           named_table, 
                           compressed,
                           {heir,whereis(bloberl_ets_owner),inherited},
                           public]),
               ?LOG_NOTICE("created table ~p", [To]);
            _ ->
               ok 
         end,
         timer:sleep(min(RetryDelayMs, MaxRetryDelayMs)),
         write_message(To, Message, RetriesLeft - 1, 2 * RetryDelayMs, MaxRetryDelayMs)
   end.

insert(To, Message) ->
      ets:insert(To, {m,Message}).

delete(Tab) ->
   ets:delete(Tab).

rename(Tab, Tab1) ->
   case ets:info(Tab) of 
      undefined ->
         no_rename;
      _ ->
         ets:rename(Tab, Tab1)
   end.

write_temp_file(Tab, File) ->
   ets:foldl(fun({_, V}, Whatever) ->
         file:write_file(File, io_lib:fwrite("~s", [V]), [append]),
         Whatever
      end, notused, Tab),
   ets:delete(Tab).