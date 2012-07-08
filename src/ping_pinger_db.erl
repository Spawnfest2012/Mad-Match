-module(ping_pinger_db).

-include("records.hrl").
-include("defaults.hrl").
-include_lib("deps/emysql/include/emysql.hrl").

-export([find/1,create/7,all/1,get_subscriptions/4,delete/1,firehose/2,update/2,find_all_by_user_id/1]).

-spec find(pos_integer()) -> notfound | #pinger{}.
find(Id) ->
  Result = ping_db:find(?PINGER_TABLE,[{where,[{id,integer_to_list(Id)}]}]),
  [Pinger|[]] = emysql_util:as_record( Result, pinger, record_info(fields, pinger), fun ping_utils:as_record/1),
  Pinger.

-spec create(string(),string(),pos_integer(),string(),pos_integer(),string(),string()) -> integer().
create(Name,Type,UserId,EndPoint,Frequency, Data, Location) ->
  JsonData = jsx:encode(Data),
  ping_db:create(?PINGER_TABLE,[{name,Name},{type,Type},{user_id,integer_to_list(UserId)},{end_point,fix_end_point(EndPoint)},{frequency,Frequency},
    {data,JsonData},{last_status,"down"},{location,Location}]).

delete(Id) ->
  ping_db:delete(?PINGER_TABLE,[{where,[{id,Id}]}]).

-spec all(list()) -> [#user{}].
all(Options) -> 
Result = ping_db:find(?PINGER_TABLE,Options),
  emysql_util:as_record(
		Result, pinger, record_info(fields, pinger), fun ping_utils:as_record/1).

-spec firehose(pos_integer() | binary(),pos_integer()) -> [#pinger{}].
firehose(Page,PageSize) when is_binary(Page) ->
  firehose(ping_utils:binary_to_integer(Page),PageSize);
firehose(Page,PageSize) -> 
  Page2 = case Page of
    1 -> 0;
    Val -> Val*PageSize
  end,

  Query = " SELECT p.*, u.name as user_name, u.tagline as user_tagline, count(s.id) as subscription_count FROM " ++ ?PINGER_TABLE ++ " p LEFT OUTER JOIN " ++ ?SUBSCRIPTION_TABLE ++ " s ON s.pinger_id = p.id INNER JOIN " ++ ?USER_TABLE ++ " u ON u.id = p.user_id GROUP BY p.id ORDER BY last_status DESC, subscription_count DESC LIMIT ?, ?",
  emysql:prepare(list_to_atom("firehose"),Query),
  Result = emysql:execute(ping_db,list_to_atom("firehose"),[Page2,PageSize]),
  emysql_util:as_record(
		Result, pinger, record_info(fields, pinger)).


-spec find_all_by_user_id(pos_integer() | binary()) -> [#pinger{}].
find_all_by_user_id(UserId) -> 
  Query = " SELECT p.*, u.name as user_name, u.tagline as user_tagline, count(s.id) as subscription_count FROM " ++ ?PINGER_TABLE ++ " p LEFT OUTER JOIN " ++ ?SUBSCRIPTION_TABLE ++ " s ON s.pinger_id = p.id INNER JOIN " ++ ?USER_TABLE ++ " u ON u.id = p.user_id WHERE p.user_id = ? GROUP BY p.id ORDER BY last_status DESC, subscription_count DESC",
  emysql:prepare(list_to_atom("find_all_by_user_id"),Query),
  Result = emysql:execute(ping_db,list_to_atom("find_all_by_user_id"),[UserId]),
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
  R = ping_db:execute(list_to_binary(Query)),
  lists:flatten(R#result_packet.rows);
get_subscriptions(Type,PingerId,pinger_up,_DownTime) ->
  Query = "SELECT u."++atom_to_list(Type)++" FROM users u,subscriptions s WHERE u.id = s.user_id AND s.type = '"++atom_to_list(Type)++"' AND s.pinger_id = "++integer_to_list(PingerId)++ " AND s.notify_when_up = true",
  R = ping_db:execute(list_to_binary(Query)),
  lists:flatten(R#result_packet.rows).

-spec update(pos_integer(),[{atom(),string()}]) -> ok|error.
update(PingerId,Updates) ->
  ping_db:update(?PINGER_TABLE, [{where,[{id,PingerId}]},{update,Updates}]).

-spec fix_end_point(list()|binary()) -> list().
fix_end_point(Input) when is_binary(Input) ->
  fix_end_point(binary_to_list(Input));
fix_end_point([]) -> [];
fix_end_point(Input) ->
  E = string:strip(Input),
  case string:str(E, "http://") of
    1 -> E;
    _ -> "http://"++E
  end.
