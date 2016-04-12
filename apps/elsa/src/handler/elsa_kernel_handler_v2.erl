
-module(elsa_kernel_handler_v2).

-export([start/0]).

start() ->
  {ok, Listen} = gen_tcp:listen(8080, [binary, {packet, line}, {reuseaddr, true}, {active, true}]),
  accept(Listen).

accept(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> handle_conn(Socket, Listen) end),
  accept(Listen).

handle_conn(Socket, Listen) ->
  receive
    {tcp, Socket, Bin} ->
      io:format("Server said: ~p", [Bin]);
    {tcp_closed, Socket} ->
      io:format("Server socket closed~n")
  end.


