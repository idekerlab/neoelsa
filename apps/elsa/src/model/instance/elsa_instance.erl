
-module(elsa_instance).

-export([new/3,
         id/1,
         rank/2,
         thread_count/1,
         get_thread/1,
         put_thread/2]).

-include("elsa_instance.hrl").

new(ServiceID, Location, ThreadCount) ->
  ID = elsa_hash:sha(Location),
  Threads = [ elsa_thread:new(ID, Num) || Num <- lists:seq(1, ThreadCount) ],
  ThreadRefs = [ elsa_thread:ref(T) || T <- Threads ],
  #instance{id                      = ID
          , service_id              = ServiceID
          , location                = Location
          , date                    = elsa_date:new()
          , thread_refs             = ThreadRefs
          , threads                 = Threads
           }.

id(#instance{id=ID}) -> ID.

rank(I1, I2) ->
  threads_out(I1) > threads_out(I2).

thread_count(#instance{thread_refs=ThreadRefs}) ->
  length(ThreadRefs).

threads_out(I = #instance{threads=Threads}) ->
  length(Threads) - thread_count(I).

get_thread(I = #instance{id=ID, thread_refs=ThreadRefs, threads=Threads, date=Date, use_count=UC}) ->
  case ThreadRefs of
    [] -> none_available;
    [Ref|Refs] -> {I#instance{thread_refs = Refs
                            , threads     = activate_thread(Threads, Ref)
                            , date        = elsa_date:update(Date)
                            , use_count   = UC+1
                             }
                 , {ID, Ref}
                  }
  end.

put_thread(Instance = #instance{thread_refs=ThreadRefs, threads=Threads, date=Date}, ThreadRef) ->
  Instance#instance{thread_refs = [ThreadRef|ThreadRefs]
                  , threads     = deactivate_thread(Threads, ThreadRef)
                  , date        = elsa_date:update(Date)
                   }.

activate_thread(Threads, ThreadRef) ->
  {value, T} = lists:keysearch(ThreadRef, 2, Threads),
  lists:keyreplace(ThreadRef, 2, Threads, elsa_thread:activate(T)).

deactivate_thread(Threads, ThreadRef) ->
  {value, T} = lists:keysearch(ThreadRef, 2, Threads),
  lists:keyreplace(ThreadRef, 2, Threads, elsa_thread:deactivate(T)).




