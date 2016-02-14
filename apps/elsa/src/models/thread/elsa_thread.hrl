
-include("../date/date.hrl").

-record(thread, {id          :: binary()
               , instance_id :: binary()
               , task_id     :: binary()
               , date         = #date{}
               , use_count    = 0
                }).


