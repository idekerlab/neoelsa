

-module(v1_elsa_services_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_accepted/2,
         json_request/2,
         content_types_provided/2,
         json_response/2]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, _Opts}.

allowed_methods(Req, _State) ->
  {[<<"GET">>,<<"POST">>,<<"PUT">>], Req, _State}.

content_types_accepted(Req, _State) ->
  {[{<<"application/json">>, json_request}], Req, _State}.

json_request(Req, _State) ->
  {Request, Body} = elsa_body:read(Req),
  lager:error("Body was ~p", [Body]),
  {Status, Response} = elsa_service_controller:register_body(Body),
  {Status, elsa_body:write(Request, Response), _State}.

content_types_provided(Req, _State) ->
  {[{<<"application/json">>, json_response}], Req, _State}.

json_response(Req, _State) ->
  {_, Services} = elsa_service_controller:all(),
  Response = elsa_service_controller:format(Services),
  {elsa_body:to_json(Response), Req, _State}.
