-module(elsa_service_database).

-export([load/0
       , clear/0
       , store/1
       , registered/3
       , unregistered/3
       , instance_out/3
       , instance_in/3
       , available/2
       , all/0
       , all/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("elsa_service_model.hrl").
-include("../instances/elsa_instance_model.hrl").

-spec load() -> ok.
load() ->
  case elsa_table:create(elsa_services, service, record_info(fields, service)) of
    ok -> lager:info("Services table created");
    exists -> lager:error("Services table already exists")
  end,
  ok.
%
-spec clear() -> ok.
clear() ->
  case elsa_table:clear(elsa_services) of
    aborted -> lager:info("Service table could not be cleared.");
    ok -> lager:info("Service table cleared.")
  end,
  ok.

registered(Service, Version, Instance) ->
  update(S, fun() -> elsa_service_model:new(Service, Version) end, reg, Instance);

unregistered(Service, Version, Instance) ->
  update(S, unreg, Instance).

checkout_out(Service, Version, Instance) ->
  update(S, in, Instance).

checked_in(Service, Version, Instance) ->
  update(S, out, Instance).

update(S, Action, I) -> update(S, fun() -> not_found end, Action, I).
update(S, Nf, Action, I) when is_record(I, instance) ->
  case find(Service, Verison) of
    not_found -> Nf();
    S -> {ok, S2} = store(elsa_service_model:update(S, I, Action)), S2
  end.
update(_, _, _) -> bad_instance.

store(Service) when is_record(Service, service) ->
  elsa_table:action(elsa_services, Service, write, write).

find(Service, Version) ->
  case elsa_table:action(elsa_services, {Service, Version}, read, read) of
    {[Service], _} -> Service;
    {[], _} -> not_found
  end.

-spec available(binary(), binary()) -> true | false.
available(Service, Version) ->
  case find(Service, Version) of
    not_found -> false;
    S -> S#service.available
  end.

-spec all() -> [] | [#service{}].
all() ->
  elsa_table:do(qlc:q([S || S <- mnesia:table(elsa_services)])).

-spec all(binary()) -> [] | [#service{}].
all(Service) ->
  elsa_table:do(qlc:q([S || S <- mnesia:table(elsa_services), is_service(S#service.name, Service)])).

is_service({S, _}, Service) ->
  S == Service.
