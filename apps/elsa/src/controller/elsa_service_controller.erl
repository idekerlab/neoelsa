
-module(elsa_service_controller).

-export([all/0
       , all/1
       , all/2
       , register/2
       , register_body/1
       , register_body/2
       , register_body/3
       , format/1
       , format/2]).

-define(TABLE, elsa_services).

all() -> elsa_store:get(?TABLE).

all(Name) ->
  exists([S || S <- elsa_store:get(?TABLE), elsa_service:match(S, Name)]).

all(Name, Version) ->
  exists([S || S <- elsa_store:get(?TABLE), elsa_service:match(S, Name, Version)]).

exists([]) -> {false, []};
exists(Items) -> {true, Items}.

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
  lager:info("Instances: ~p, ~p, ~p", [Instances, Name, Version]),
  elsa_body:validate(Fields, [
    {<<"name">>, Name}
  , {<<"version">>, Version}
  , {<<"instances">>, [ignore(elsa_instance_controller:register_instance(Name, Version, I)) || I <- Instances]}
  ]).

ignore({_Status, Response}) -> Response.

format(Services) ->
  [
   {<<"services">>, [elsa_service:format(S) || S <- Services]}
  ].

format(ServiceName, Versions) ->
  [
   {<<"name">>, ServiceName}
 , {<<"versions">>, [elsa_service:version_format(V) || V <- Versions]}
  ].

