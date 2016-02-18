
-module(elsa_kernel_handler).

-export([init/3,
         handle/2,
         terminate/3
         ]).

init({tcp, http}, Req, _Opts) ->
  lager:info("Service request received: ~p", [Req]),
  {Request, Kernel} = elsa_kernel_controller:parse(Req),
  lager:info("Created kernel: ~p", [Kernel]),
  case elsa_kernel_controller:resource_exists(Kernel) of
    true -> {ok, Request, Kernel};
    false ->
      {ok, Req2} = elsa_kernel_controller:resource_missing(Kernel, Request),
      {shutdown, Req2,  no_state}
  end.

handle(Req, Kernel) ->
  Timeout = elsa_kernel_controller:timeout(Kernel),
  {Conn, Task}  = create_connection(Kernel),
  Result = receive
    R -> R
  after Timeout ->
    Conn ! timeout,
    elsa_task_controller:format(Timeout, Task)
  end,
  Response = elsa_kernel_controller:create_response(Result, Req),
  {ok, Response, done}.

create_connection(Kernel) ->
  Task = elsa_kernel_controller:new_task(Kernel),
  {spawn_link(elsa_kernel_controller, connect, [self(), Kernel, Task]), Task}.

terminate(_Reason, Req, State) ->
  ok.

