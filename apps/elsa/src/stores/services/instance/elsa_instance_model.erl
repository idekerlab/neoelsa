
-module(elsa_instance_model).

-export([new/4, format/1]).

-include("elsa_instance_model.hrl").
-include("../capacity/elsa_capacity_model.hrl").

new(ID, Location, Capacity, Agent) ->
  #instance{id=ID
          , location=Location
          , agent=Agent
          , registered=calendar:universal_time()
          , last_used=calendar:universal_time()
          , capacity=capacity(Capacity)}.

capacity(C) when is_atom(C) -> C;
capacity(C) -> #capacity{total=C,available=C}.

format(I) ->
  Capacity = case I#instance.capacity of
    infinity -> <<"infinite">>;
    _ -> [
           {<<"total">>, I#instance.capacity#capacity.total}
         , {<<"available">>, I#instance.capacity#capacity.available}
         ]
  end,
  [
    {<<"id">>,                I#instance.id}
  , {<<"agent">>,             I#instance.agent}
  , {<<"location">>,          I#instance.location}
  , {<<"registered">>,        I#instance.registered}
  , {<<"available">>,         I#instance.available}
  , {<<"reachable">>,         I#instance.reachable}
  , {<<"requests_serviced">>, I#instance.serviced}
  , {<<"registered">>,        elsa_time:format(I#instance.registered)}
  , {<<"last_used">>,         elsa_time:format(I#instance.registered)}
  , {<<"capacity">>,          Capacity}
  ].
