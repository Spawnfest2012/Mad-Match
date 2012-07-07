-module(pinger_db).

-include("records.hrl").
-include("defaults.hrl").

-export([find/1]).

-spec find(pos_integer()) -> notfound | #pinger{}.
find(Id) ->
  Result = ping_db:find(?PINGER_TABLE,[{where,[{id,Id}]}]),
  [Pinger|[]] = emysql_util:as_record( Result, pinger, record_info(fields, pinger)),
  Pinger.

