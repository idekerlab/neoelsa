
-module(v1_elsa_thread_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         json_response/2]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ServiceName, Request} = cowboy_req:binding(service_name, Req),
  {ServiceVersion, Request2} = cowboy_req:binding(service_version, Request),
  {InstanceID, Request3} = cowboy_req:binding(instance_id, Request2),
  {ThreadID, Request4} = cowboy_req:binding(thread_id, Request3),
  {ok, Request4, {ServiceName, ServiceVersion, InstanceID, ThreadID}}.

allowed_methods(Req, Instance) ->
  {[<<"GET">>], Req, Instance}.

content_types_provided(Req, Instance) ->
  {[{<<"application/json">>, json_response}], Req, Instance}.

resource_exists(Req, {Name, Version, InstanceID, ThreadID}) ->
  {Exists, Thread} = case elsa_service_controller:all(Name, Version) of
    {true, [Service]} ->
      case elsa_instance_controller:get(Service, InstanceID) of
        {true, [I]} -> elsa_thread_controller:find(I, ThreadID);
        {false, _} -> {false, []}
      end;
    {false, _} -> {false, []}
  end,
  {Exists, Req, Thread}.

json_response(Req, [Thread]) ->
  Response = elsa_thread_controller:format(Thread),
  {elsa_body:to_json(Response), Req, Thread}.
