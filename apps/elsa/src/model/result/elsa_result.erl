
-module(elsa_result).

-export([create/3]).

-include("elsa_result.hrl").

create(Status, Headers, Body) ->
  #result{status  = Status
        , headers = Headers
        , body    = Body
  }.


