
-module(elsa_kernel_controller).

-export([parse/1
       , resource_exists/1
       , timeout/1
       , connect/3]).

-record(kernel, {method, name, version, endpoint, headers, body, timeout}).

parse(Req) ->
  {N, Req1} =  cowboy_req:binding(service_name, Req),
  {V, Req2} = cowboy_req:binding(service_version, Req1),
  {E, Req3} = cowboy_req:path(Req2),
  {T, Req4} = cowboy_req:header(<<"x-elsa-timeout">>, Req3, <<"5000">>),
  {H, Req5} = cowboy_req:headers(Req4),
  {M, Req6} = cowboy_req:method(Req5),
  {ok, B, Request} = cowboy_req:body(Req6, [{length, infinity}]),
  {Request, #kernel{method=M, name=N, version=V, endpoint=E, headers=H, body=B, timeout=T}}.

resource_exists(#kernel{name=N, version=V}) ->
  case elsa_service_controller:all(N, V) of
    {false, _} -> false;
    {true, [Service]} -> elsa_service:thread_count(Service) > 0
  end.

timeout(#kernel{timeout=T}) when is_integer(T) -> integer_to_binary(T);
timeout(#kernel{timeout=T}) when is_binary(T) -> T.

connect(Handler, K = #kernel{name=N, version=V, method=M, endpoint=E, headers=H, body=B}, Task) ->
  {L, Thread} = elsa_service_controller:get_thread(N, V),
  case elsa_http_client:call(M, url(L, E), H, B) of
    {ok, Status, Headers, Body} ->
      elsa_service_controller:put_thread(Thread),
      respond(Status, Headers, Body, Task);
    retry ->
      elsa_service_controller:disable_thread(Thread),
      connect(Handler, K, Task)
  end.

respond(S, H, B, Task) -> ok.

url(L, E) -> iolist_to_binary([L, E]).
