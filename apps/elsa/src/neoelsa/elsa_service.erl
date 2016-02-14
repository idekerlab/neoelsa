
-module(elsa_service).

-export([register/3
       , unregister/2
       , checkout/1
       , checkin/2]).

-spec register(binary(), binary(), integer()) -> [any()].
register(ServiceID, Location, ThreadCount) when
is_binary(ServiceID),
is_binary(Location),
is_integer(ThreadCount) ->
  elsa_service:append(ServiceID, elsa_instance:new(Location, ThreadID)).

-spec unregister(binary(), binary()) -> ok.
unregister(ServiceID, InstanceID) when
is_binary(ServiceID),
is_binary(InstanceID) ->
  elsa_service:remove(ServiceID, InstanceID).

-spec checkin(binary(), #thread{}) -> ok.
checkin(Service, Thread) when
is_record(Service, service),
is_record(Thread, thread) ->
  elsa_serice
  elsa_service_model:update(Service, [
    I#instance{use_count = I#instnace.use_count+1
             , threads   = [
               Thread#thread{use_count = Thread#thread.use_count+1
                           , date      = elsa_date:update(Thread#thread.date)
               } || I#instance.threads]
    } || I <- S#service.instances], 1).

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
