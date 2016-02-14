
-module(elsa_service).

-export([register/3
       , unregister/2
       , checkout/1
       , checkin/2]).

checkout(Name, Version, Location, Threads) ->
  elsa_service_database:update(
    elsa_service_model:id(Name, Version),
    elsa_service, register, [Location, Threads]
  ).

-spec register(binary(), binary(), integer()) -> ok.
register(Service, Location, Threads) when
is_record(Service, service),
is_binary(Location),
is_integer(Threads) ->
  #service{instances = [#instance{id = hash(Location),
                                , service_id = S#service.id,
                                , location   = Location,
                                , threads    = [ create_thread() | _  <- lists:seq(0, Threads)]
                                , date       = #date{created   = elsa_time:get(),
                                                   , modified  = elsa_time:get()}
                        }, S#service.instances]
   }.

-spec unregister(binary(), binary()) -> ok.
unregister(Service, Instance) when
is_record(Service, service),
is_binary(Instance) ->
  #service{instances = [
    I || I <- S#service.instances, I#instance.id != Instance
  ]}.

-spec checkin(binary(), #thread{}) -> ok.
checkin(Service, Thread) when
is_record(Service, service),
is_record(Thread, thread) ->
  #service{instances = [
    I#instance{use_count = I#instnace.use_count+1
             , threads   = [
               Thread#thread{use_count = Thread#thread.use_count+1
                           , date      = elsa_date:update(Thread#thread.date)
               } || I#instance.threads]
    } || I <- S#service.instances]
}.

-spec checkout(binary()) -> #thread{}.
checkout(Service) when
is_record(Service, service) ->
  [Instance|_] = case [ I || I <- S#service.instances, length(I#instance.threads) > 0] of
    [] -> unavailable;
    Is -> rank(Is)
  end,
  remove_thread(S, Instance).

-spec rank([#instance{}] | []) -> [#instnace{}] | [].
rank(Instances) -> Instances.

-spec remove_thread(#service{}, #instance{}) -> #thread{}.
remove_thread(Service, Instance) ->
  [T|Ts] = Instance#instance.threads,
  I = Instances#instance{threads=Ts},
  Service#service{
    instances = lists:keyreplace(Instance#instance.id, 2, Service#service.instances, I)
  }, T.
