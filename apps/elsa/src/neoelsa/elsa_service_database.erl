-module(elsa_service_database).

-export([load/0
       , clear/0
       , update/1
       , get/1
       , available/2
       , all/0
       , all/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("datatypes.hrl").

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

-spec update(binary(), atom, atom, [any()]) -> ok.
update(ServiceID, Module, Function, Args) ->
  {atomic, ok} = mnesia:transaction(
    fun() ->
      [Service] = mnesia:read(elsa_services, ServiceID, read),
      mnesia:write(elsa_services, apply(Module, Function, [Service]++Args), write)
    end
  ), ok.

-spec get(binary()) -> #service{}
get(ServiceID) ->
  {atomic, Service} = mnesia:transaciton(
    fun() ->
      mnesia:read(elsa_services, ServiceID, read)
    end
  ), Service.

-spec all() -> [] | [#service{}].
all() ->
  elsa_table:do(qlc:q([S || S <- mnesia:table(elsa_services)])).

-spec all(binary()) -> [] | [#service{}].
all(Service) ->
  elsa_table:do(qlc:q([S || S <- mnesia:table(elsa_services), is_service(S#service.name, Service)])).

is_service({S, _}, Service) ->
  S == Service.
