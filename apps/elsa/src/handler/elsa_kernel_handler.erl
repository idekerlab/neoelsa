
-module(elsa_kernel_handler).

-export([init/3,
         handle/2,
         request/7,
         terminate/3
         ]).

init({tcp, http}, Req, _Opts) ->
  lager:info("Service request received: ~w", [Req]),
  {Request, Kernel} = elsa_kernel_controller:parse(Req),
  case elsa_kernel_controller:resource_exists(Kernel) of
    {true, Service} -> {ok, Request, Kernel};
    {false, Service} -> {shutdown, Request, no_state}
  end.

handle(Req, Kernel) ->
  Task = elsa_task_controller:new(),
  Conn = create_connection(Kernel, Task),
  Response = receive
    R -> R
  after elsa_kernel_controller:timeout(Kernel) ->
    Conn ! timeout,
    elsa_task_controller:format(Task)
  end,
  {ok, Response, done}.

create_connection(Kernel, Task) ->
  spawn_link(elsa_kernel_controller, connect, [self(), Kernel, Task]).

terminate(_Reason, Req, State) ->
  ok.
