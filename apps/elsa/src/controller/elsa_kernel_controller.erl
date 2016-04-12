
-module(elsa_kernel_controller).

-export([parse/1
       , new_task/1
       , resource_exists/1
       , resource_missing/2
       , timeout/1
       , connect/3
       , create_task/1
       , create_response/2]).

-record(kernel, {method, name, version, endpoint, headers, body, timeout}).

parse(Req) ->
  {N, Req1} =  cowboy_req:binding(service_name, Req),
  {V, Req2} = cowboy_req:binding(service_version, Req1),
  {E, Req3} = cowboy_req:path(Req2),
  {T, Req4} = cowboy_req:header(<<"x-elsa-timeout">>, Req3, <<"5000">>),
  {H, Req5} = cowboy_req:headers(Req4),
  {M, Req6} = cowboy_req:method(Req5),
  {ok, B, Request} = cowboy_req:body(Req6, [{length, infinity}]),
  {Request, #kernel{method=M, name=N, version=V, endpoint=truncate(N, V, E), headers=H, body=B, timeout=T}}.

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
  lager:info("Calling m: ~p, url: ~p, h: ~p, b: ~b", [M, url(Loc, E), H, B]),
  case elsa_http_client:call(M, url(Loc, E), H, B) of
    {ok, Status, Headers, Body} ->
      elsa_thread_controller:put(N, V, InstanceID, ThreadID),
      lager:info("Connection succeeded"),
      respond(Status, Headers, Body, Task, Handler);
    retry ->
      lager:error("Could not connect"),
      elsa_task_controller:update_status(Task, retrying_with_new_connection),
      connect(Handler, K, Task)
  end.

create_response({Status, Headers, Body}, Res) ->
  {ok, Response} = cowboy_req:reply(Status, Headers, Body, Res),
  Response;
create_response(Task, Res) ->
  {ok, Response} = cowboy_req:reply(200, [], elsa_body:to_json(Task), Res),
  Response.

find_thread(N, V) ->
  case elsa_thread_controller:get(N, V) of
    none_available -> find_thread(N, V);
    Thread -> Thread
  end.

respond(S, H, B, Task, Handler) ->
  lager:info("Response: ~p, ~p, ~p", [S, H, B]),
  receive
    timeout ->
      Task
    after 0 ->
     Handler ! {S, H, B}
  end.

url(L, E) -> iolist_to_binary([L, E]).


truncate(V, N, E) ->
  VL = length(binary_to_list(V)),
  NL = length(binary_to_list(N)),
  list_to_binary(string:sub_string(binary_to_list(E), (3 + VL + NL))).

resource_missing(#kernel{name=N, version=V}, Request) ->
  Req = elsa_body:write(Request, [
   {<<"status">>, 404},
   {<<"service_name">>, N},
   {<<"service_version">>, V}
  ]), cowboy_req:reply(404, Req).
