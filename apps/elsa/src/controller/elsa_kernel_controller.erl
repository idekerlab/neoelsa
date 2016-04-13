
-module(elsa_kernel_controller).

-export([resource_exists/2
       , resource_missing/3
       , find_service/2
       , return_thread/3]).

resource_exists(N, V) ->
  case elsa_service_controller:all(N, V) of
    {false, _} -> false;
    {true, [Service]} -> elsa_service:instance_count(Service) > 0
  end.

find_service(N, V) ->
  case elsa_thread_controller:get(N, V) of
    none_available -> find_service(N, V);
    {Loc, InstanceID, ThreadID} = Thread ->
      {split_url(Loc), Thread}
  end.

return_thread(N, V, {_, Ir, Tr}) ->
  elsa_thread_controller:put(N, V, Ir, Tr).

resource_missing(N, V, Request) ->
  Req = elsa_body:write(Request, [
   {<<"status">>, 404},
   {<<"service_name">>, N},
   {<<"service_version">>, V}
  ]), cowboy_req:reply(404, Req).

split_url(Loc) ->
  [Url, Port] = binary:split(Loc, <<":">>),
  {binary_to_list(Url), binary_to_integer(Port)}.
