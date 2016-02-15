
-module(elsa_thread).

-export([new/0
       , update/2
       , update/3]).

new(InstanceID, Num) ->
  ID = elsa_hash:id(InstanceID + Num),
  #thread{id          = ID
        , instance_id = InstanceID
        , task        = none
        , date        = elsa_date:new()
         }.

activate(T = #thread{date=Date, use_count=UC}) ->
  T#thread{date      = elsa_date:update(Date)
         , use_count = UC+1
         , task      = assigned
          }.

deactivate(T = #thread{date=Date}) ->
  T#thread{date = elsa_date:update(Date)
         , task = none
          }.
