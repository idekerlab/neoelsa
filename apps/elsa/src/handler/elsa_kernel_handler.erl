
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
    false -> {shutdown, Request, no_state}
  end.

handle(Req, Kernel) ->
  lager:info("Made it to handle with kernel: ~p", [Kernel]),
  {Conn, Task}  = create_connection(Kernel),
  Response = receive
    R -> R
  after elsa_kernel_controller:timeout(Kernel) ->
    lager:info("TImeout"),
    Conn ! timeout,
    elsa_task_controller:format(Task)
  end,
  {ok, Response, done}.

create_connection(Kernel) ->
  Task = elsa_kernel_controller:new_task(Kernel),
  {spawn_link(elsa_kernel_controller, connect, [self(), Kernel, Task]), Task}.

terminate(_Reason, Req, State) ->
  ok.
