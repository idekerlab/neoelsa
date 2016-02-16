
-module(elsa_instance_controller).

-export([get/2
       , all/1
       , register/4
       , unregister/3
       , register_instance/3
       , format/1
       , format/3]).

-define(TABLE, elsa_services).

get(Service, InstanceID) ->
  exists([ I || I <- elsa_service:instances(Service), elsa_instance:match(I, InstanceID) ]).

all(Service) ->
  exists(elsa_service:instances(Service)).

exists([]) -> {false, []};
exists(Items) -> {true, Items}.

register(N, V, _L, _T) when is_atom(N); is_atom(V) -> missing;
register(Name, Version, Location, ThreadCount) ->
  ServiceID = elsa_hash:sha(Name, Version),
  case elsa_store:get(?TABLE, ServiceID) of
    not_found -> elsa_store:put(?TABLE, elsa_service:new(Name, Version, Location, ThreadCount));
    Service -> case elsa_service:find_instance(Service, elsa_hash:sha(Location)) of
                 not_found -> elsa_store:set(?TABLE, ServiceID, fun(S) ->
                                 elsa_service:add_instance(S, Location, ThreadCount)
                              end);
                 _ -> unregister(Name, Version, Location),
                             register(Name, Version, Location, ThreadCount)
               end
  end.

%What happens it doesn't exist
unregister(Name, Version, Location) ->
  ServiceID = elsa_hash:sha(Name, Version),
  elsa_store:set(?TABLE, ServiceID, fun(S) ->
    elsa_service:remove_instance(S, elsa_hash:sha(Location))
  end).

register_instance(N, V, I) ->
  Fields = {elsa_body:key(<<"location">>, I, missing), elsa_body:key(<<"threads">>, I, 32)},
  {Location, ThreadCount} = Fields,
  lager:info("Location is: ~p, TC is: ~p, I: ~p", [Location, ThreadCount, I]),
  register(N, V, Location, ThreadCount),
  elsa_body:validate(Fields, [
   {<<"id">>, elsa_hash:sha(Location)},
   {<<"location">>, Location},
   {<<"threads">>, ThreadCount}
  ]).

format(Instance) ->
  elsa_instance:format(Instance).

format(Name, Version, Instances) ->
  [
   {<<"name">>, Name},
   {<<"version">>, Version},
   {<<"instances">>, [elsa_instance:format(I) || I <- Instances]}
  ].

