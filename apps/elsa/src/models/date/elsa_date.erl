
-module(elsa_date).

-export([new/0
       , update/1]).

-include("elsa_date.hrl").

-spec new() -> #date{}.
new() ->
  #date{created  = elsa_date_lib:get()
      , modified = elsa_date_lib:get()
  }.

-spec update(#date{}) -> #date{}.
update(Date) ->
  Date#date{modified = elsa_date_lib:get()}.
