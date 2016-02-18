
-module(elsa_task_controller).

-export([new/1
       , set_resource/3
       , update_status/2
       , format/2]).

-define(TABLE, elsa_tasks).

new(ServiceID) ->
  elsa_store:put(?TABLE, elsa_task:new(ServiceID)).

set_resource(Task, InstanceID, ThreadID)  ->
  elsa_store:set(?TABLE, elsa_task:id(Task), fun(T) ->
    elsa_task:set_resource(T, InstanceID, ThreadID)
  end).

update_status(Task, Status) ->
  elsa_store:set(?TABLE, elsa_task:id(Task), fun(T) ->
    elsa_task:update_status(Task, Status)
  end).

format(Timeout, Task) ->
  [
   {<<"status">>, <<"timer elapsed">>},
   {<<"timeout_microseconds">>, Timeout},
   {<<"task">>, elsa_task:format(Task)}
  ].


