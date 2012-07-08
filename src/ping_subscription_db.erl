-module(ping_subscription_db).

-include("records.hrl").
-include("defaults.hrl").
-include_lib("deps/emysql/include/emysql.hrl").

-export([find/1, find_by_user/1, create/6, delete/1,all/1]).

-spec find(pos_integer()) -> notfound | #subscription{}.
find(Id) ->
  Result = ping_db:find(?SUBSCRIPTION_TABLE,[{where,[{id,integer_to_list(Id)}]}]),
  [Subscription|[]] = emysql_util:as_record( Result, subscription, record_info(fields, subscription)),
  Subscription.

-spec find_by_user(pos_integer()|string()) -> not_found | list().
find_by_user(Id) ->
  Result = ping_db:find(?SUBSCRIPTION_TABLE, [{where,[{user_id,integer_to_list(Id)}]}]),
  emysql_util:as_record(Result, subscription, record_info(fields, subscription)).

-spec create(string(),pos_integer(),pos_integer(),pos_integer(),boolean(),pos_integer()) -> {ok,pos_integer()}.
create(Type, UserId, PingerId, DownTime, NotifyWhenUp,NotificationDelay) ->
  ping_db:create(?SUBSCRIPTION_TABLE, [
      {type, Type},
      {user_id, UserId},
      {pinger_id, PingerId},
      {down_time, DownTime},
      {notify_when_up, NotifyWhenUp},
      {notification_delay,NotificationDelay}]).

-spec all(list()) -> [#user{}].
all(Options) -> 
Result = ping_db:find(?SUBSCRIPTION_TABLE,Options),
  emysql_util:as_record(
		Result, subscription, record_info(fields, subscription)).


-spec delete(pos_integer()) -> pos_integer().
delete(Id) ->
  ping_db:delete(?SUBSCRIPTION_TABLE,[{where,[{id,Id}]}]).
