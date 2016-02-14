
-ifndef(ELSA_CAPACITY_HRL).

-record(capacity, {total     = 0
                 , available = 0
                 , reachable = 0
}).

-define(TOTAL, #capacity.total).
-define(AVAILABLE, #capacity.available).
-define(REACHABLE, #capacity.reachable).

-define(ELSA_CAPACITY_HRL, true).
-endif.
