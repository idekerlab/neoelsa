
-module(elsa_kernel_controller).

-record(kernel, {method, name, version, endpoint, headers, body, timeout}).

parse(Req) ->
  {Version, Req1} = cowboy_req:binding(version, Req),
  {Service, Req2} =  cowboy_req:binding(service, Req1),
  {Endpoint, Req3} = cowboy_req:path(Req2),
  {Timeout, Req4} = cowboy_req:header(<<"x-elsa-timeout">>, Req3, <<"5000">>),
  {Headers, Req5} = cowboy_req:headers(Req4),
  {Method, Req6} = cowboy_req:method(Req5),
  {ok, Body, Request} = cowboy_req:body(Req6, [{length, infinity}]),
  {Request, #kernel{Method, Service, Version, Endpoint, Headers, Body, Timeout}}.

resource_exists(#kernel{name=N, version=V}) ->
  case elsa_service_controller:all(N, V) of
    {false, _} -> false;
    {true, [Service]} -> elsa_service:thread_count(Service) > 0
  end.

timeout(#kernel{timeout=T}) when is_integer(T) -> integer_to_binary(T);
timeout(#kernel{timeout=T}) when is_binary(T) -> T.

connect(Handler, K = #kernel{name=N, service=S, method=M, endpoint=E, headers=H, body=B}, Task) ->
  {Location, Thread} = elsa_service_controller:get_thread(N, S),
  case elsa_http_client:call(M, url(L, E), H, B) of
    {ok, Status, Headers, Body} ->
      elsa_service_controller:put_thread(Thread),
      respond(Status, Headers, Body);
    retry ->
      elsa_service_controller:disable_thread(Thread),
      connect(Handler, K)
  end.

respond(Status, Headers, Body) ->
  %Make response here.




