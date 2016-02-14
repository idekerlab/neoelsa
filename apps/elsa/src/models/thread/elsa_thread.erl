
-module(elsa_thread).

-export([new/0
       , update/2
       , update/3]).

new(InstanceID, Num) ->
  #thread{id = elsa_hash:id(InstanceID)
        , instance_id = InstanceID
        , task        = none
        , date        = elsa_date:new()
         }.

update(T, TID) -> update(T, TID, 0).
update(Thread, TaskID, Uses) ->
  Thread#thread{task      = TaskID
              , date      = elsa_date:update(Thread#thread.date)
              , use_count = Thread#thread.use_count+Uses
               }.


