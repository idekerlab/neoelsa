
-include("../date/elsa_date.hrl").

-record(service, {id       :: binary()
               ,  name     :: binary()
               ,  version  :: binary()
               ,  date      = #date{}
               ,  use_count = 0
               ,  instances = []
}).

