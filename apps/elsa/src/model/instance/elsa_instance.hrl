
-include("../date/elsa_date.hrl").

-record(instance, {id                     :: binary()
                 , service_id             :: binary()
                 , location               :: binary()
                 , date                    = #date{}
                 , use_count               = 0
                 , thread_refs             = []
                 , threads                 = []
                 }

).
