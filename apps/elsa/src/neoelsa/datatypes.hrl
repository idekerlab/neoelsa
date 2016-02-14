
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

update(S, I) -> update(S, I, 0).
update(Service, Instances, Uses) ->
  Service#service{instances = Instances,
          date = Service#service.date#date{
                                   modified=elsa_date:get()
                                  },
          use_count = Service#service.use_count+Uses
  }.


-record(instance, {id          :: binary()
                 , service_id  :: binary()
                 , location    :: binary()
                 , date         = #date{}
                 , use_count    = 0
                 , threads      = []
                 }

}).

update(I, T) -> update(I, T, 0).
update(Instance, Threads, Uses) ->
  Instance#instance{threads = Threads
                  , date    = elsa_date:update(Instance#instance.date)
                  , use_count = Instance#instance.use_count+Uses
  }.


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
