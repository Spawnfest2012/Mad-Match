%% Author: Manuel Gomez
-module(ping_user_db).

-include("records.hrl").
-include("defaults.hrl").
-include_lib("deps/emysql/include/emysql.hrl").


-export([find/1,create/4]).

-spec find(integer()) -> [#user{}].
find(Id) -> 
Result = ping_db:find(?USER_TABLE,[{where,[{id,lists:flatten(io_lib:format("~p",[Id]))}]}]),
  emysql_util:as_record(
		Result, user, record_info(fields, user)).
  

-spec create(string(),string(),string(),string()) -> {ok,pos_integer()}.
create(Name,Email,Password,Tagline) -> 
  Result = ping_db:create(?USER_TABLE,[{name,Name},{email,Email},{password,Password},{tagline,Tagline}]).
