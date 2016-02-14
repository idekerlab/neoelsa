
-ifndef(ELSA_CAPACITY_HRL).
-include("../capacity/elsa_capacity_model.hrl").
-endif.

-record(date, {created   :: tuple()
             , modified :: tuple()
}).

-record(service, {id            :: binary()
               ,  name          :: binary()
               ,  version       :: binary()
               ,  date           = #date{}
               ,  use_count      = 0
               ,  instances      = []
}).

-record(instance, {id          :: binary()
                 , service_id  :: binary()
                 , location    :: binary()
                 , date         = #date{}
                 , use_count    = 0
                 , threads      = []
                 }

}).

-record(thread, {id          :: binary()
               , instance_id :: binary()
               , task_id     :: bianry()
               , date         = #date{}
               , use_count    = 0
}).

-record(result, {location       :: binary()
               , body_byte_size :: integer()
               , status_code    :: integer()
               , date_completed :: tuple()
}).

-record(request, {service      :: binary()
                , version      :: binary()
                , method       :: binary()
                , endpoint     :: binary()
                , date_created :: tuple()
}).

-record(task, {id          :: binary()
             , instance_id :: binary()
             , thread_id   :: binary()
             , completed    = false
             , request      = #request{}
             , result       = #result{}
}).
