
-module(elsa_service_controller).

-export([all/0
       , versions/1
       , register/2
       , register_body/1
       , register_body/2
       , register_body/3
       , format/1
       , version_format/2]).

-define(TABLE, elsa_services).

all() -> elsa_store:get(?TABLE).

versions(ServiceName) ->
  [S || S <- elsa_store:get(?TABLE), elsa_service:has_name(S, ServiceName)].

register(Name, Version) ->
  ServiceID = elsa_hash:sha(Name, Version),
  case elsa_store:get(?TABLE, ServiceID) of
    not_found -> elsa_store:put(?TABLE, elsa_service:new(Name, Version));
    _Service ->  already_registered
  end.

register_body(Body) -> register_body(missing, missing, Body).
register_body(Version, Body) -> register_body(missing, Version, Body).
register_body(N, V, B) ->
  Fields = {elsa_body:key(<<"name">>, B, N)
          , elsa_body:key(<<"version">>, B, V)
          , elsa_body:key(<<"instances">>, B, missing)
           },
  {Name, Version, Instances} =  Fields,
  elsa_body:validate(Fields, [
    {<<"name">>, Name}
  , {<<"version">>, Version}
  , {<<"instances">>, [ignore(elsa_instance_controller:register_instance(N, V, I)) || I <- Instances]}
  ]).

ignore({_Status, Response}) -> Response.

format(Services) ->
  [
   {<<"services">>, [elsa_service:format(S) || S <- Services]}
  ].

version_format(ServiceName, Versions) ->
  [
   {<<"name">>, ServiceName}
 , {<<"versions">>, [elsa_service:version_format(V) || V <- Versions]}
  ].

