
-module(elsa_kernel_handler_v2).

-export([start/0]).

start() ->
  {ok, Listen} = gen_tcp:listen(8080, [binary, {packet, line}, {reuseaddr, true}, {active, false}]),
  lager:info("Socket opened"),
  accept(Listen).

accept(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  lager:info("Connection made"),
  spawn(fun() -> handle_conn(Socket, Listen) end),
  accept(Listen).

handle_conn(Socket, Listen) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Bin} ->
      lager:info("Server said: ~p", [Bin]),
      parse(Bin);
    {error, closed} ->
      lager:info("Server socket closed~n")
  end.


parse(RequestLine) ->
  [Verb, Rest] = binary:split(RequestLine, <<" ">>),
  [_, Service, Version | Rest2] = binary:split(Rest, <<"/">>, [global]),
  URL = [Verb,<<" /">>,Rest2],
  lager:info("Service: ~p", [Service]),
  lager:info("Version: ~p", [Version]),
  lager:info("Rest: ~p", [URL]).

