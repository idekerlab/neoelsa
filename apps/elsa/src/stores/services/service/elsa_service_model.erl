
-module(elsa_service_model).

-export([new/2,
         update/3,
         format/1]).

-include("elsa_service_model.hrl").
-include("../capacity/elsa_capacity_model.hrl").
-unclude("../instances/elsa_instance_model.hrl").

new(Service, Version) ->
  #service{
    name={Service, Version},
    created=elsa_time:get()
  }.


-spec update(#service{}, #instance{}, atom()) -> #service{}.
update(S = #service{available=A, capacity=C, instances=I}, #instance{available=IA, reachable=IR}, Action) ->
  {A2, C2, I2} = case Action of
    reg ->
      {elsa_capacity_model:update(C, 1, 1, 1)};
    unreg ->
      {elsa_capacity_model:update(C, -1, if(IA, -1, 0), if(IR, -1, 0))};
    in ->
      {elsa_capacity_model:update(C, 0, if(IA, 1, 0), if(IR, 0, -1)};
    out ->
      {elsa_capacity_model:update(C, 0, -1, if(IR, 0, -1))}
  end,
  S#service{
    available=A2
  , capacity=C2
  , instances=I2
  }.


if(E, V, V2) ->
  case E of
    true -> V;
    false -> V2
  end.

format(S) ->
  {Service, Version} = S#service.name,
  [
    {<<"id">>, #service.id}
  , {<<"Service">>, Service}
  , {<<"version">>, Version}
  , {<<"created">>, elsa_date:format(S#service.created)}
  , {<<"agents">>,  S#service.agents}
  , {<<"available">>, S#service.available}
  ].
