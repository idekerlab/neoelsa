
-module(elsa_service).

-export([new/2
       , add_instance/3
       , remove_instance/2
       , thread_count/1
       , get_thread/1
       , put_thread/3]).

-include("elsa_service.hrl").

new(Name, Version) -> new(Name, Version, []).
new(Name, Version, Instances) ->
  ID = elsa_hash:sha(Name, Version),
  #service{id        = ID
         , name      = Name
         , version   = Version
         , date      = elsa_date:new()
         , instances = Instances
          }.

add_instance(S = #service{id=ID, instances=Is, date=Date}, Location, ThreadCount) ->
  S#service{instances = [elsa_instance:new(ID, Location, ThreadCount)|Is]
          , date      = elsa_date:update(Date)
           }.

remove_instance(S = #service{instances=Is, date=Date}, InstanceID) ->
S#service{instances = lists:keydelete(InstanceID, 2, Is)
        , date      = elsa_date:update(Date)
         }.

thread_count(#service{instances=Is}) ->
  lists:foldl(fun(I, Sum) -> elsa_instance:thread_count(I) + Sum end, 0, Is).

get_thread(S = #service{instances=Is, date=Date, use_count=UC}) ->
  Instances = [ I || I <- Is, elsa_instance:thread_count(I) > 0 ],
  [Primary|Rest] = lists:sort(fun elsa_instance:rank/2, Instances),
  {Primary2, Refs} = elsa_instance:get_thread(Primary),
  {S#service{instances = [Primary2|Rest]
           , date      = elsa_date:update(Date)
           , use_count = UC+1
            }, Refs}.

put_thread(S = #service{instances=Is, date=Date}, InstanceRef, ThreadRef) ->
  % Get the instance
  I = lists:keysearch(InstanceRef, 2, Is),
  Is2 = lists:keyreplace(InstanceRef, 2, Is, elsa_instance:put_thread(I, ThreadRef)),
  S#service{instances = Is2
          , date      = elsa_date:update(Date)
           }.
