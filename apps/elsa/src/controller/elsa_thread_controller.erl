
-module(elsa_thread_controller).

-export([all/1
       , get/2
       , format/1
       , format_threads/1]).

-define(TABLE, elsa_services).

all(Instance) ->
  elsa_instance:threads(Instance).

get(Instance, ThreadID) ->
  exists([ T || T <- elsa_instance:threads(Instance), elsa_thread:match(T, ThreadID) ]).

exists([]) -> {false, []};
exists(Items) -> {true, Items}.

format(Thread) ->
  elsa_instance:format(Thread).

format_threads(Instance) ->
  [ elsa_thread:format(T) || T <- elsa_instance:threads(Instance) ].



