
-module(elsa_hash).

-export([sha/1
       , sha/2]).

sha(Term) ->
    base16:encode(crypto:hash(sha256, binary_to_list(Term))).

sha(Term, Term2) ->
    sha(iolist_to_binary([Term, Term2])).
