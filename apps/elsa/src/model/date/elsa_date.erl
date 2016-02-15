
-module(elsa_date).

-export([new/0
       , update/1
       , utc/0
       , format/1]).

-include("elsa_date.hrl").

-spec new() -> #date{}.
new() ->
  #date{created  = utc()
      , modified = utc()
  }.

-spec update(#date{}) -> #date{}.
update(Date) ->
  Date#date{modified = utc()}.


utc() ->
    calendar:universal_time().

format({{Y,MO,D}, {H,MI,S}}) ->
    list_to_binary(io_lib:format("~p/~p/~p ~p:~p:~p", [Y, MO, D, H, MI, S])).
