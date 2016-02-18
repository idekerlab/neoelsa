
-module(elsa_kernel_controller).

-export([parse/1
       , new_task/1
       , resource_exists/1
       , resource_missing/2
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

new_task(#kernel{name=N, version=V}) ->
  elsa_task_controller:new(elsa_hash:sha(N, V)).

resource_exists(#kernel{name=N, version=V}) ->
  case elsa_service_controller:all(N, V) of
    {false, _} -> false;
    {true, [Service]} -> elsa_service:instance_count(Service) > 0
  end.

timeout(#kernel{timeout=T}) when is_integer(T) -> T;
timeout(#kernel{timeout=T}) when is_binary(T) -> binary_to_integer(T).

create_task(#kernel{name=N, version=V}) ->
  elsa_task_controller:new(elsa_hash:sha(N, V)).

connect(Handler, K = #kernel{name=N, version=V, method=M, endpoint=E, headers=H, body=B}, Task) ->
  {Loc, InstanceID, ThreadID} = find_thread(N, V),
  elsa_task_controller:set_resource(Task, InstanceID, ThreadID),
  elsa_task_controller:update_status(Task, connected),
  case elsa_http_client:call(M, url(Loc, E), H, B) of
    {ok, Status, Headers, Body} ->
      elsa_thread_controller:put(N, V, InstanceID, ThreadID),
      lager:error("connect"),
      respond(Status, Headers, Body, Task);
    retry ->
      lager:error("Could not connect"),
      elsa_task_controller:update_status(Task, retrying_with_new_connection),
      connect(Handler, K, Task)
  end.

find_thread(N, V) ->
  case elsa_thread_controller:get(N, V) of
    none_available -> find_thread(N, V);
    Thread -> Thread
  end.

respond(S, H, B, Task) ->
  %Create result here, add to task, set task status to result ready
  lager:info("~p; ~p; ~p;", [S,H,B]),
  ok.

url(L, E) -> iolist_to_binary([L, E]).

resource_missing(#kernel{name=N, version=V}, Request) ->
  Req = elsa_body:write(Request, [
   {<<"status">>, 404},
   {<<"service_name">>, N},
   {<<"service_version">>, V}
  ]), cowboy_req:reply(404, Req).
