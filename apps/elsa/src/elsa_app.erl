
-module(elsa_app).
-behaviour(application).

-export([start/2
        ,stop/1]).

start(_StartType, _StartArgs) ->
    database_setup([node()]),
    elsa_router:start(),
    {ok, self()}.

stop(_State) ->
  database_teardown([node()]),
  ok.

database_setup(Nodes) ->
  mnesia:stop(),
  case mnesia:create_schema([node()]) of
    ok -> lager:info("Schema created.");
    {error, Reason} -> lager:error("Error while creating schema: ~w", [Reason])
  end,
  mnesia:start(),
  rpc:multicall(Nodes, application, start, [mnesia]),
  lager:info("Mnesia started."),
  elsa_store:load_table(elsa_services, service, [id, name, version, date, use_count, instances]),
  elsa_store:load_table(elsa_tasks, task, [id, thread_info, complete, status, date, result]),
  elsa_store:load_table(elsa_results, result, [status, headers, body]).

database_teardown(Nodes) ->
  elsa_store:clear_table(elsa_services),
  rpc:multicall(Nodes, application, stop, [mnesia]),
  lager:info("Mnesia stopped.").
