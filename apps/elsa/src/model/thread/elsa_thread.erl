
-module(elsa_thread).

-export([new/2
       , activate/1
       , deactivate/1
       , ref/1
       , match/2
       , format/1]).

-include("elsa_thread.hrl").

new(InstanceID, Num) ->
  ID = elsa_hash:sha(InstanceID, integer_to_binary(Num)),
  #thread{id          = ID
        , instance_id = InstanceID
        , task_id     = none
        , date        = elsa_date:new()
         }.

activate(T = #thread{date=Date, use_count=UC}) ->
  T#thread{date      = elsa_date:update(Date)
         , use_count = UC+1
         , task_id   = assigned
          }.

deactivate(T = #thread{date=Date}) ->
  T#thread{date    = elsa_date:update(Date)
         , task_id = none
          }.

ref(#thread{id=ID}) -> ID.

match(#thread{id=ID}, ThreadID) -> ID == ThreadID.

format(#thread{id=ID, task_id=TaskID, date=Date}) ->
  [
   {<<"id">>, ID},
   {<<"task_id">>, TaskID},
   {<<"date">>, elsa_date:format(Date)}
  ].
