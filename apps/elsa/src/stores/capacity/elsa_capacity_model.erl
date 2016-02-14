
-module(elsa_capcity_model).

-export([c/4]).

-include("elsa_capacity_model.hrl").

c(C,Tdiff,Adiff, Rdiff) ->
  T = C#capacity.total + Tdiff,
  A = C#capacity.available + Adiff,
  R = C#capacity.reachable + Rdiff,
  C#capacity{
    total=T
  , available=A
  , reachable=R
  }.
