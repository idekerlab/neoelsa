
-module(elsa_time).

-export([get/0, format/1]).

get() ->
  calendar:universal_time().

format({{Y,MO,D}, {H,MI,S}}) ->
  list_to_binary(io_lib:format("~p/~p/~p ~p:~p:~p", [Y, MO, D, H, MI, S])).
