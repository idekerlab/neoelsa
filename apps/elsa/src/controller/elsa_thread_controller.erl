
-module(elsa_thread_controller).

-export([all/1
       , get/2
       , put/3
       , disable/3
       , format/1
       , format_threads/1]).

-define(TABLE, elsa_services).

all(Instance) ->
  elsa_instance:threads(Instance).

get(Name, Version) ->
  lager:info("Getting thread"),
  case  elsa_store:extract(?TABLE, elsa_hash:sha(Name, Version), fun(S) ->
    elsa_service:get_thread(S)
  end) of
    T  -> info:lager("Stupid: ~p", [T])
  end.

put(Name, Version, Thread) ->
  elsa_store:set(?TABLE, elsa_hash:sha(Name, Version), fun(S) ->
    elsa_service:put_thread(S, Thread)
  end).

disable(Name, Version, Thread) ->
  ok.

find(Instance, ThreadID) ->
  exists([ T || T <- elsa_instance:threads(Instance), elsa_thread:match(T, ThreadID) ]).

exists([]) -> {false, []};
exists(Items) -> {true, Items}.

format(Thread) ->
  elsa_thread:format(Thread).

format_threads(Threads) ->
  [ elsa_thread:format(T) || T <- Threads ].



