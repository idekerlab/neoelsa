
-include("../date/elsa_date.hrl").

-record(thread_info, {service_id  :: binary()
                   ,  instance_id :: binary()
                   ,  thread_id   :: binary()
}).

-record(result, {location       :: binary()
              ,  completed_on   :: tuple()
              ,  status         :: binary()
              ,  body_byte_size :: integer()
}).

-record(task, {id          :: binary()
            ,  thread_info :: binary()
            ,  complete     = false
            ,  status       = created
            ,  date         = #date{}
            ,  result       = #result{}
}).

