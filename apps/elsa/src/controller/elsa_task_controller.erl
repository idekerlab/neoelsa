
-module(elsa_instance_controller).

-export([register/4
       , unregister/3
       , register_instance/3]).

-define(TABLE, elsa_services).

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
  Fields = {extract(<<"location">>, I, missing), extract(<<"threads">>, I, 32)},
  {Location, ThreadCount} = Fields,
  elsa_service_controller:register_instance(N, V, Location, ThreadCount),
  validate(Fields, [
   {<<"id">>, elsa_hash:sha(Location)},
   {<<"location">>, Location},
   {<<"capacity">>, ThreadCount}
  ]).

validate(Tuple, Response) ->
  case lists:member(missing, tuple_to_list(Tuple)) of
    false -> {true, [{<<"registered">>, true}, {<<"object">>, Response}]};
    true -> {false, [{<<"registered">>, false}, {<<"object">>, Response}]}
  end.

extract(Key, Props, Default) ->
  case Default of
    missing -> proplists:get_value(Key, Props, missing);
    Value -> Value
  end.


remove_status({Status, 


