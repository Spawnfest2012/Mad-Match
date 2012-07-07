-module(ping_subscription_db).

-include("records.hrl").
-include("defaults.hrl").
-include_lib("deps/emysql/include/emysql.hrl").

-export([find/1]).

-spec find(pos_integer()) -> notfound | #subscription{}.
find(Id) ->
  Result = ping_db:find(?SUBSCRIPTION_TABLE,[{where,[{id,Id}]}]),
  [Subscription|[]] = emysql_util:as_record( Result, subscription, record_info(fields, subscription)),
  Subscription.
 
