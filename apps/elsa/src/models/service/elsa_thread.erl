
-record(service, {id       :: binary()
               ,  name     :: binary()
               ,  version  :: binary()
               ,  date      = #date{}
               ,  use_count = 0
               ,  instances = []
}).

-module(elsa_instance).

-export([new/2]).


new(Name, Version) ->
  ID = elsa_id:get(Name, Version),
  #service{id        = ID
         , name      = Name
         , version   = Version
         , date      = elsa_date:new()
         , instances = []
          }.

thread_count(#service{instances=Is}) ->
  lists:foldl(fun(I, Sum) -> elsa_instance:thread_count(I) + Sum end, 0, Is).

get_thread(S = #service{instances=Is}) -> 
  Instances = [ I || I <- Is, elsa_instance:thread_count(I) > 0 ],
  [Primary|Rest] = lists:sort(fun elsa_instance:compare/2, Instances),
  {Primary2, Thread} = elsa_instance:thread_out(Primary),
  {S#service{instances=[Primary2|Rest]}, Thread}.

put_thread(T = #thread{instance_id=I_ID}) ->







