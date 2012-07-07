%% Author: Manuel Gomez
-module(ping_user_db).

-include("records.hrl").
-include("defaults.hrl").
-include_lib("deps/emysql/include/emysql.hrl").


-export([find/1,create/4,all/1]).

-spec find(integer()) -> [#user{}].
find(Id) -> 
Result = ping_db:find(?USER_TABLE,[{where,[{id,integer_to_list(Id)}]}]),
  emysql_util:as_record(
		Result, user, record_info(fields, user)).
  

-spec create(string(),string(),string(),string()) -> {ok,pos_integer()}.
create(Name,Email,Password,Tagline) -> 
  Result = ping_db:create(?USER_TABLE,[{name,Name},{email,Email},{password,Password},{tagline,Tagline}]).

-spec all(list()) -> [#user{}].
all(Options) -> 
Result = ping_db:find(?USER_TABLE,Options),
  emysql_util:as_record(
		Result, user, record_info(fields, user)).
