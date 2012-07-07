-module(ping_pinger_db).

-include("records.hrl").
-include("defaults.hrl").
-include_lib("deps/emysql/include/emysql.hrl").

-export([find/1,get_subscriptions/4]).

-spec find(pos_integer()) -> notfound | #pinger{}.
find(Id) ->
  Result = ping_db:find(?PINGER_TABLE,[{where,[{id,Id}]}]),
  [Pinger|[]] = emysql_util:as_record( Result, pinger, record_info(fields, pinger)),
  Pinger.
  
get_subscriptions(Type,PingerId,pinger_down,DownTime) ->
  Query = "SELECT u."++atom_to_list(Type)++" FROM users u,subscriptions s WHERE u.id = s.user_id AND s.type = '"++atom_to_list(Type)++"' AND s.pinger_id = "++integer_to_list(PingerId)++" AND s.down_time <= "++integer_to_list(DownTime),
  lager:info(Query),
  R = ping_db:execute(list_to_binary(Query)),
  lists:flatten(R#result_packet.rows);
get_subscriptions(Type,PingerId,pinger_up,_DownTime) ->
  Query = "SELECT u."++atom_to_list(Type)++" FROM users u,subscriptions s WHERE u.id = s.user_id AND s.type = '"++atom_to_list(Type)++"' AND s.pinger_id = "++integer_to_list(PingerId)++ " AND s.notify_when_up = true",
  R = ping_db:execute(list_to_binary(Query)),
  lists:flatten(R#result_packet.rows).