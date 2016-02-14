
-ifndef(ELSA_CAPACITY_HRL).
-include("../capacity/elsa_capacity_model.hrl").
-endif.

-record(instance, {id       :: binary()
                 , agent    :: bool()
                 , location :: binary()
                 , available         = true
                 , reachable         = true
                 , registered        = {0, 0, 0}
                 , last_used         = {0, 0, 0}
                 , serviced          = 0
                 , capacity          = #capacity{}
}).
