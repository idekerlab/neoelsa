

-module(v1_elsa_result_handler).

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
  {ok, Request, ServiceName}.

allowed_methods(Req, _State) ->
  {[<<"GET">>,<<"POST">>,<<"PUT">>], Req, _State}.

content_types_accepted(Req, _State) ->
  {[{<<"application/json">>, json_request}], Req, _State}.

json_request(Req, ServiceName) ->
  {Request, Body} = elsa_body:read(Req),
  {Status, Response} = elsa_service_controller:register_body(ServiceName, Body),
  {Status, elsa_body:write(Request, Response), ServiceName}.

content_types_provided(Req, _State) ->
  {[{<<"application/json">>, json_response}], Req, _State}.

resource_exists(Req, ServiceName) ->
  {Exists, Versions} = elsa_service_controller:all(ServiceName),
  {Exists, Req, {ServiceName, Versions}}.

json_response(Req, {ServiceName, Versions}) ->
  Response = elsa_service_controller:format(ServiceName, Versions),
  {elsa_body:to_json(Response), Req, ServiceName}.
