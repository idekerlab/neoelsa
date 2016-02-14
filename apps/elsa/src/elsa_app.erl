%%%-------------------------------------------------------------------
%% @doc elsa public API
%% @end
%%%-------------------------------------------------------------------

-module(elsa_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    database_setup([node()]),
    {ok, self()}.

%%--------------------------------------------------------------------
stop(_State) ->
  database_teardown([node()]),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

database_setup(Nodes) ->
  mnesia:stop(),
  case mnesia:create_schema([node()]) of
    ok -> lager:info("Schema created.");
    {error, Reason} -> lager:error("Error while creating schema: ~w", [Reason])
  end,
  mnesia:start(),
  rpc:multicall(Nodes, application, start, [mnesia]),
  lager:info("Mnesia started.").

database_teardown(Nodes) ->
  rpc:multicall(Nodes, application, stop, [mnesia]),
  lager:info("Mnesia stopped.").
