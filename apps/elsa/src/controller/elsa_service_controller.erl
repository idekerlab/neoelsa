
-module(elsa_service_controller).

-export([versions/1
       , register/4
       , unregister/3]).

-define(TABLE, elsa_services).

versions(ServiceName) ->
  [S || S <- elsa_store:get(?TABLE), elsa_service:has_name(S, ServiceName)].

register(Name, Version) ->
  ServiceID = elsa_hash:sha(Name, Version),
  case elsa_store:get(?TABLE, ServiceID) of
    not_found -> elsa_store:put(?TABLE, elsa_service:new(Name, Version));
    Service ->  already_registered
  end.

register(Body) -> reg(missing, missing, Body).
register(Version, Body) -> reg(missing, Version, Body).
register(N, V, B) ->
  Fields = {extract(<<"name">>, B, N)
          , extract(<<"version">>, B, V)
          , extract(<<"instances">>, B, missing)
           },
  {Name, Version, Instances} =  Fields,
  validate(Fields, [
                     {<<"name">>, Name},
                     {<<"version">>, Version},
                     {<<"instances">>, [remove_status(elsa_instance_controller_register(N, V, I)) ||
                                        I <- Instances]}
                   ]).


