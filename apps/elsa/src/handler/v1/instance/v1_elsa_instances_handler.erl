
-module(v1_elsa_instances_handler).

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
  {ok, Request2, {ServiceName, ServiceVersion}}.

allowed_methods(Req, Service) ->
  {[<<"GET">>], Req, Service}.

content_types_accepted(Req, Service) ->
  {[{<<"application/json">>, json_request}], Req, Service}.

json_request(Req, Service = {ServiceName, ServiceVersion}) ->
  {Request, Body} = elsa_body:read(Req),
  {Status, Response} = elsa_service_controller:register_body(ServiceName, ServiceVersion, Body),
  {Status, elsa_body:write(Request, Response), Service}.

content_types_provided(Req, Instance) ->
  {[{<<"application/json">>, json_response}], Req, Instance}.

resource_exists(Req, {Name, Version}) ->
  lager:info("S ~p, V ~p", [Name, Version]),
  {Exists, Service} = elsa_service_controller:all(Name, Version),
  {Exists, Req, {Name, Version, Service}}.

json_response(Req, {Name, Version, [Service]}) ->
  {_, Instances} = elsa_instance_controller:all(Service),
  Response = elsa_instance_controller:format(Name, Version, Instances),
  {elsa_body:to_json(Response), Req, Service}.
