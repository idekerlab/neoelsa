
-module(elsa_kernel_handler_v2).

-export([start/0]).

start() ->
  {ok, Listen} = gen_tcp:listen(8080, [binary, {packet, line}, {reuseaddr, true}, {active, false}]),
  accept(Listen).

accept(Listen) ->
  {ok, Client} = gen_tcp:accept(Listen),
  spawn(fun() -> handle_conn(Client) end),
  accept(Listen).

handle_conn(Client) ->
  {Service, Name, Version, Thread} = parse_request_line(Client),
  ContentLength = parse_headers(Client, Service, 0),
  parse_body(Client, Service, ContentLength, 0),
  ContentLength2 = parse_headers(Service, Client, 0),
  parse_body(Service, Client, ContentLength2, 0),
  elsa_kernel_controller:return_thread(Name, Version, Thread).

parse_request_line(Client) ->
  {ok, Line} = gen_tcp:recv(Client, 0),
  {Name, Version} = parse(Line),
  {{Url, Port}, Thread} = elsa_kernel_controller:find_service(Name, Version),
  {ok, Service} = gen_tcp:connect(Url, Port, [binary, {packet, line}]),
  gen_tcp:send(Client, Line),
  {Service, Name, Version, Thread}.

parse_headers(Client, Service, ContentLength) ->
  case gen_tcp:recv(Client, 0) of
    {ok, Line = <<"\r\n">>} ->
      gen_tcp:send(Service, Line),
      inet:setopts(Client, [{packet, raw}]);
    {ok, Line = <<"Content-Length: ", N/binary>>} ->
      gen_tcp:send(Service, Line),
      parse_headers(Client, Service, content_length(N));
    {ok, Line} ->
      gen_tcp:send(Service, Line),
      parse_headers(Client, Service, ContentLength);
    {error, closed} ->
      lager:error("Connection dropped")
  end,
  ContentLength.

parse_body(Client, Service, ContentLength, ReadSoFar) ->
 if ReadSoFar == ContentLength ->
      gen_tcp:close(Client);
    true ->
      case gen_tcp:recv(Client, 0, 100) of
        {ok, Bin} ->
          gen_tcp:send(Service, Bin),
          parse_body(Client, Service, ContentLength, ReadSoFar + size(Bin));
        {error, closed} ->
          lager:info("Connection dropped")
      end
 end.

content_length(Val) ->
  [Num, _] = binary:split(Val, <<"\r">>),
  binary_to_integer(Num).

parse(RequestLine) ->
  [Verb, Rest] = binary:split(RequestLine, <<" ">>),
  [_, Name, Version | Rest2] = binary:split(Rest, <<"/">>, [global]),
  URL = [Verb,<<" /">>,Rest2],
  lager:info("Rest: ~p", [URL]),
  {Name, Version}.
