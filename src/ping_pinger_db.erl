-module(ping_pinger_db).

-include("records.hrl").
-include("defaults.hrl").
-include_lib("deps/emysql/include/emysql.hrl").

-export([find/1,create/6,all/1,get_subscriptions/4,delete/1,firehose/2,update/2]).

-spec find(pos_integer()) -> notfound | #pinger{}.
find(Id) ->
  Result = ping_db:find(?PINGER_TABLE,[{where,[{id,integer_to_list(Id)}]}]),
  [Pinger|[]] = emysql_util:as_record( Result, pinger, record_info(fields, pinger)),
  Pinger.

-spec create(string(),string(),pos_integer(),string(),pos_integer(),string()) -> integer().
create(Name,Type,UserId,EndPoint,Frequency, Data) ->
  JsonData = jsx:encode(Data),
  ping_db:create(?PINGER_TABLE,[{name,Name},{type,Type},{user_id,integer_to_list(UserId)},{end_point,EndPoint},{frequency,Frequency},
    {data,JsonData},{last_status,"down"}]).

delete(Id) ->
  lager:info("Id ~p\n", [Id]),
  ping_db:delete(?PINGER_TABLE,[{where,[{id,Id}]}]).

-spec all(list()) -> [#user{}].
all(Options) -> 
Result = ping_db:find(?PINGER_TABLE,Options),
  emysql_util:as_record(
		Result, pinger, record_info(fields, pinger)).

-spec firehose(pos_integer(),pos_integer()) -> [#user{}].
firehose(Page,PageSize) -> 
  Query = " SELECT p.*, u.name as user_name, u.tagline as user_tagline, count(s.id) as subscription_count FROM " ++ ?PINGER_TABLE ++ " p LEFT OUTER JOIN " ++ ?SUBSCRIPTION_TABLE ++ " s ON s.pinger_id = p.id INNER JOIN " ++ ?USER_TABLE ++ " u ON u.id = p.user_id GROUP BY p.id ORDER BY last_status DESC, subscription_count DESC LIMIT ?, ?",
  emysql:prepare(list_to_atom("firehose"),Query),
  Result = emysql:execute(ping_db,list_to_atom("firehose"),[Page,PageSize]),
  emysql_util:as_record(
		Result, pinger, record_info(fields, pinger)).


-spec get_subscriptions(atom(),pos_integer(),pinger_down|pinger_up,undefined|pos_integer()) -> [string()].
get_subscriptions(Type,PingerId,pinger_down,DownTime) ->
  Now = integer_to_list(ping_utils:now()),
  Query = "SELECT u."++atom_to_list(Type)++
          " FROM users u,subscriptions s WHERE u.id = s.user_id AND s.type = '"++atom_to_list(Type)++
          "' AND s.pinger_id = "++integer_to_list(PingerId)++
          " AND s.down_time <= "++integer_to_list(DownTime)++
          " AND s.last_notification < ("++Now++" - s.notification_delay)" ,
  lager:info(Query),
  R = ping_db:execute(list_to_binary(Query)),
  lists:flatten(R#result_packet.rows);
get_subscriptions(Type,PingerId,pinger_up,_DownTime) ->
  Query = "SELECT u."++atom_to_list(Type)++" FROM users u,subscriptions s WHERE u.id = s.user_id AND s.type = '"++atom_to_list(Type)++"' AND s.pinger_id = "++integer_to_list(PingerId)++ " AND s.notify_when_up = true",
  R = ping_db:execute(list_to_binary(Query)),
  lists:flatten(R#result_packet.rows).

-spec update(pos_integer(),[{atom(),string()}]) -> ok|error.
update(PingerId,Updates) ->
  ping_db:update(?PINGER_TABLE, [{where,[{id,PingerId}]},{update,Updates}]).
