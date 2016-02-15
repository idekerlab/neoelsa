
-module(elsa_service_controller).

-export([versions/1
       , register/4
       , unregister/3]).

-define(TABLE, elsa_services).

versions(ServiceName) ->
  [S || S <- elsa_store:get(?TABLE), elsa_service:has_name(S, ServiceName)].

register(Name, Version, Location, ThreadCount) ->
  ServiceID = elsa_hash:sha(Name, Version),
  case elsa_store:get(?TABLE, ServiceID) of
    not_found -> elsa_store:put(?TABLE, elsa_service:new(Name, Version, Location, ThreadCount));
    Service -> case elsa_service:find_instance(Service, elsa_hash:sha(Location)) of
                 not_found -> elsa_store:set(?TABLE, ServiceID, fun(S) ->
                                 elsa_service:add_instance(S, Location, ThreadCount)
                              end);
                 Instance -> unregister(Name, Version, Location),
                             register(Name, Version, Location, ThreadCount)
               end
  end.

unregister(Name, Version, Location) ->
  ServiceID = elsa_hash:sha(Name, Version),
  elsa_store:set(?TABLE, ServiceID, fun(S) ->
    elsa_service:remove_instance(S, elsa_hash:sha(Location))
  end).


