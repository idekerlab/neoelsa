
-module(elsa_task_model).

-export([format/1]).

-include("elsa_task_model.hrl").

format(T) ->
  CompletedAt = case T#task.completed of
    true -> elsa_time:format(T#task.completed_at);
    false -> T#task.completed_at
  end,
  [
    {<<"id">>,           T#task.id}
  , {<<"completed">>,    T#task.completed}
  , {<<"location">>,     T#task.location}
  , {<<"completed_at">>, CompletedAt}
  , {<<"status">>,       T#task.status}
  , {<<"byte_size">>,    T#task.byte_size}
  , {<<"request">>, [
      {<<"service">>, T#task.request#request.service}
    , {<<"version">>, T#task.request#request.version}
    , {<<"method">>,  T#task.request#request.method}
    , {<<"endpoint">>, T#task.request#request.endpoint}
    ]}
  ].
