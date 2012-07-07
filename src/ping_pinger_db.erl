-module(ping_pinger_db).

-include("records.hrl").
-include("defaults.hrl").
-include_lib("deps/emysql/include/emysql.hrl").

-export([find/1,find_users_emails/3,find_users_emails/2]).

-spec find(pos_integer()) -> notfound | #pinger{}.
find(Id) ->
  Result = ping_db:find(?PINGER_TABLE,[{where,[{id,Id}]}]),
  [Pinger|[]] = emysql_util:as_record( Result, pinger, record_info(fields, pinger)),
  Pinger.

-spec find_users_emails(pos_integer(),pinger_down|pinger_up,pos_integer()) -> [string()].
find_users_emails(PingerId,pinger_down,DownTime) ->
  Query = "SELECT u.email FROM users u,subscriptions s ON u.id = s.user_id WHERE s.type = 'email' AND s.pinger_id = "++integer_to_list(PingerId)++" AND s.downtime <= "++integer_to_list(DownTime),
  R = ping_db:execute(list_to_binary(Query)),
  lists:flatten(R#result_packet.rows).

find_users_emails(PingerId,pinger_up) ->
  Query = "SELECT u.email FROM users u,subscriptions s ON u.id = s.user_id WHERE s.type = 'email' AND s.pinger_id = "++integer_to_list(PingerId)++ " AND s.notify_when_up = true",
  R = ping_db:execute(list_to_binary(Query)),
  lists:flatten(R#result_packet.rows).
  
