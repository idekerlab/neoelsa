
-module(elsa_body).

-export([read/1
       , write/2
       , to_json/1
       , validate/2
       , key/3]).

read(Request) ->
  {ok, JSON, Req} = cowboy_req:body(Request),
  Body = try jsx:decode(JSON)
    catch error:badarg -> invalid_json
  end,
  {Req, Body}.

write(Request, Term) ->
  Body = jsx:prettify(jsx:encode(Term)),
  cowboy_req:set_resp_body(Body, Request).

to_json(Term) ->
  jsx:prettify(jsx:encode(Term)).

validate(Tuple, Response) ->
  case lists:member(missing, tuple_to_list(Tuple)) of
    false -> {true, [{<<"registered">>, true}, {<<"object">>, Response}]};
    true -> {false, [{<<"registered">>, false}, {<<"object">>, Response}]}
  end.

key(Key, Props, Default) ->
  case proplists:get_value(Key, Props, Default) of
    missing -> missing;
    Value when Value =/= Default -> document_and_endpoint_mismatch;
    Value -> Value
  end.
