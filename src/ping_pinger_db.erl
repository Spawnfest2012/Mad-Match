-module(ping_pinger_db).

-include("records.hrl").
-include("defaults.hrl").
-include_lib("deps/emysql/include/emysql.hrl").

-export([find/1,create/5,all/1,find_users_emails/3,find_users_emails/2]).

-spec find(pos_integer()) -> notfound | #pinger{}.
find(Id) ->
  Result = ping_db:find(?PINGER_TABLE,[{where,[{id,integer_to_list(Id)}]}]),
  [Pinger|[]] = emysql_util:as_record( Result, pinger, record_info(fields, pinger)),
  Pinger.

-spec create(string(),string(),pos_integer(),string(),pos_integer()) -> integer().
create(Name,Type,UserId,EndPoint,Frequency) -> 
  Result = ping_db:create(?PINGER_TABLE,[{name,Name},{type,Type},{user_id,integer_to_list(UserId)},{end_point,EndPoint},{frequency,Frequency}]).

-spec all(list()) -> [#user{}].
all(Options) -> 
Result = ping_db:find(?PINGER_TABLE,Options),
  emysql_util:as_record(
		Result, pinger, record_info(fields, pinger)).

-spec find_users_emails(pos_integer(),pinger_down,pos_integer()) -> [string()].
find_users_emails(PingerId,pinger_down,DownTime) ->
  Query = "SELECT u.email FROM users u,subscriptions s ON u.id = s.user_id WHERE s.type = 'email' AND s.pinger_id = "++integer_to_list(PingerId)++" AND s.downtime <= "++integer_to_list(DownTime),
  R = ping_db:execute(list_to_binary(Query)),
  lists:flatten(R#result_packet.rows).

-spec find_users_emails(pos_integer(),pinger_up) -> [string()].
find_users_emails(PingerId,pinger_up) ->
  Query = "SELECT u.email FROM users u,subscriptions s ON u.id = s.user_id WHERE s.type = 'email' AND s.pinger_id = "++integer_to_list(PingerId)++ " AND s.notify_when_up = true",
  R = ping_db:execute(list_to_binary(Query)),
  lists:flatten(R#result_packet.rows).
  
