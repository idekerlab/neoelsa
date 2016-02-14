
-module(elsa_instance_database).

-export([load/2
       , clear/2
       , store/3
       , register/5
       , checkin/4
       , checkout/2
       , find/3
       , all/2
       , exists/2]).

-export([instances/0, instances/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("elsa_instance_model.hrl").

instances() -> #instance{}.

instances(I) -> I#instance{serviced=I#instance.serviced+1}.

-spec load(binary(), binary()) -> ok.
load(Service, Version) ->
  Name = service(Service, Version),
  case elsa_table:create(Name, instance, record_info(fields, instance)) of
    ok -> lager:info("Service table ~s created.", [Name]);
    exists -> lager:error("Service table ~s already exists.", [Name])
  end,
  ok.

-spec clear(binary(), binary()) -> ok.
clear(Service, Version) ->
  Serv = service(Service, Version),
  case elsa_table:clear(Serv) of
    aborted -> lager:error("Service table ~s could not be cleared", [Serv]);
    ok -> lager:info("Service table ~s cleared.", [Serv])
  end,
  ok.

-spec store(binary(), binary(), #instance{}) -> #instance{}.
store(Service, Version, Instance) when is_record(Instance, instance) ->
  elsa_table:action(service(Service, Version), Instance, write, write).

-spec service(binary(), binary()) -> binary().
service(Service, Version) -> binary_to_atom(<<<<"elsa_service__">>/binary, Service/binary, <<"_">>/binary, Version/binary >>, utf8).

register(Service, Version, Location, Capacity, Agent) ->
  Instance = elsa_instance_model:new(id(Location), Location, Capacity, Agent),
  store(Service, Version, Instance),
  Instance.

-spec(binary(), binary(), binary()) -> #instance{} | not_found
unregister(Service, Version, Location) ->
  Instance = find(Service, Version, Locaiton),
  elsa_table:action(service(Service, Version), id(Location), delete, write),
  Instance.

checkin(Service, Version, Location, Reachable) ->
  I = find(Service, Version, Location),
  {ok, Instance} = store(Service, Version, I#instance{
    available=Reachable,
    reachable=Reachable,
    last_used=elsa_time:get(),
    serviced=I#instance.serviced+1,
    capacity=case I#instance.capacity of
      C when is_atom(C) -> C;
      C -> C#capacity{available=C#capacity.available+1}
    end}),
  Instance.

%TODO Round Robin infinite instances
checkout(Service, Version) ->
  Instances = elsa_table:do(qlc:q([I || I <- mnesia:table(service(Service, Version)), I#instance.available == true, I#instance.reachable == true])),
  Sort = fun(X,Y) -> out(X#instance.capacity) > out(X#instance.capacity) end,
  case lists:reverse(lists:sort(Sort, Instances)) of
    [] -> unavailable;
    [I|_] ->
      Capacity = case I#instance.capacity of
          C when is_atom(C) -> C;
          C -> C#capacity{available=C#capacity.available-1}
      end,
      {ok, _Instance} = store(Service, Version, I#instance{
        capacity  = Capacity
      , available = case Capacity of
          Capacity when is_atom(Capacity) -> true;
          _ -> Capacity#capacity.available =/= 0
        end
      }),
      I#instance.location
  end.
out(C) when is_atom(C) -> C;
out(C) -> C#capacity.available - C#capacity.total.

-spec find(binary(), binary(), binary()) -> not_found | #instance{}.
find(Service, Version, Location) ->
  case elsa_table:action(service(Service, Version), id(Location), read, read) of
    {[Instance], _} -> Instance;
    {[], _} -> not_found
  end.
%
-spec all(binary(), binary()) -> [] | [#instance{}].
all(Service, Version) ->
  elsa_table:do(qlc:q([S || S <- mnesia:table(service(Service, Version))])).

exists(Service, Version) ->
  lists:member(service(Service, Version), mnesia:system_info(tables)).

id(Location) -> base16:encode(crypto:hash(sha256, binary_to_list(Location))).
