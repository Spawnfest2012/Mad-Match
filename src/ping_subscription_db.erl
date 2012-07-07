-module(ping_subscription_db).

-include("records.hrl").
-include("defaults.hrl").
-include_lib("deps/emysql/include/emysql.hrl").

-export([find/1, create/5, delete/1]).

-spec find(pos_integer()) -> notfound | #subscription{}.
find(Id) ->
  Result = ping_db:find(?SUBSCRIPTION_TABLE,[{where,[{id,lists:flatten(io_lib:format("~p",[Id]))}]}]),
  [Subscription|[]] = emysql_util:as_record( Result, subscription, record_info(fields, subscription)),
  Subscription.

create(Type, UserId, PingerId, DownTime, NotifyWhenUp) ->
  ping_db:create(?SUBSCRIPTION_TABLE, [
      {type, Type},
      {user_id, UserId},
      {pinger_id, PingerId},
      {down_time, DownTime},
      {notify_when_up, NotifyWhenUp}]).

delete(Id) ->
  ping_db:delete(?SUBSCRIPTION_TABLE,[{where,[{id,lists:flatten(io_lib:format("~p",[Id]))}]}]).
