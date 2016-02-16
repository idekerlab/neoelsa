
-module(v1_elsa_instance_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_accepted/2,
         json_request/2,
         content_types_provided/2,
         resource_exists/2,
         json_response/2]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ServiceName, Request} = cowboy_req:binding(service_name, Req),
  {ServiceVersion, Request2} = cowboy_req:binding(service_version, Request),
  {InstanceID, Request3} = cowboy_req:binding(instance_id, Request2),
  {ok, Request3, {ServiceName, ServiceVersion, InstanceID}}.

allowed_methods(Req, Instance) ->
  {[<<"GET">>,<<"POST">>,<<"PUT">>], Req, Instance}.

content_types_accepted(Req, Service) ->
  {[{<<"application/json">>, json_request}], Req, Service}.

json_request(Req, Instance = {ServiceName, ServiceVersion, _}) ->
  {Request, Body} = elsa_body:read(Req),
  {Status, Response} = elsa_instance_controller:register_body(ServiceName, ServiceVersion, Body),
  {Status, elsa_body:write(Request, Response), Instance}.

content_types_provided(Req, Instance) ->
  {[{<<"application/json">>, json_response}], Req, Instance}.

resource_exists(Req, {Name, Version, InstanceID}) ->
  lager:info("S ~p, V ~p", [Name, Version]),
  {Exists, Instance} = case elsa_service_controller:all(Name, Version) of
    {true, [Service]} -> elsa_instance_controller:get(Service, InstanceID);
    {false, _} -> {false, []}
  end,
  {Exists, Req, Instance}.

json_response(Req, [Instance]) ->
  Response = elsa_instance_controller:format(Instance),
  {elsa_body:to_json(Response), Req, Instance}.
