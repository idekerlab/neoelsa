-module(elsa_store).

-export([load_table/3
       , clear_table/1
       , put/2
       , set/3
       , get/2
       , get/1]).

-include_lib("stdlib/include/qlc.hrl").

load_table(Table, Record, Info) ->
  case mnesia:create_table(Table,
    [{attributes, Info},
     {record_name, Record},
     {disc_copies, [node()]}]) of
       {atomic, ok} -> lager:info("~p table created", [Table]);
       {aborted, {already_exists, _}} -> lager:info("~p table alrady exists", [Table])
  end.

clear_table(Table) ->
  case mnesia:clear_table(Table) of
    {atomic, ok} -> lager:info("~p table cleared", [Table]);
    {aborted, _} -> lager:info("~p table could not be cleared.", [Table])
  end.


put(Table, Item) ->
  {atomic, ok} = mnesia:transaction(fun() ->
      mnesia:write(Table, Item, write)
  end), Item.


set(Table, ItemID, UpdateItem) ->
  {atomic, ItemOrNotFound} = mnesia:transaction(fun() ->
    case mnesia:read(Table, ItemID, read) of
      [] -> not_found;
      [Item] -> mnesia:write(Table, UpdateItem(Item), write)
    end
  end), ItemOrNotFound.

get(Table, ItemID) ->
  {atomic, ItemOrNotFound} = mnesia:transaction(fun() ->
    case mnesia:read(Table, ItemID, read) of
      [] -> not_found;
      [Item] -> Item
    end
  end), ItemOrNotFound.

get(Table) ->
  {atomic, Items} = mnesia:transaction(fun() ->
    qlc:e(qlc:q([I || I <- mnesia:table(Table)]))
  end), Items.
