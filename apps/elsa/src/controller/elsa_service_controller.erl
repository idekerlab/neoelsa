
-module(elsa_service_controller).

-export([versions/1
       , register/4]).

-define(TABLE, elsa_service_table).

versions(ServiceName) ->
  [S || S <- elsa_store:all(?TABLE), elsa_service:name(S) == ServiceName].

register(Name, Version, Location, ThreadCount) ->
  ServiceID = elsa_hash:sha(Name, Version),
  case elsa_store:get(?Table, ServiceID) of
    not_found -> elsa_store:put(?Table, elsa_service:new(S, Location, ThreadCount));
    Service -> case elsa_service:find_instance(Service, elsa_hash:id(Location)) of
                 not_found -> elsa_store:set(?TABLE, ServiceID, fun(S) ->
                                 elsa_service:add_instance(S, Location, ThreadCount)
                              end);
                 Instance -> elsa_store:set(?TABLE, ServiceID, fun(S) ->
                               elsa_service:remove_instance(S, elsa_hash:id(Location))
                             end),
                             register(Name, Version, Location, ThreadCount)
               end
  end.



