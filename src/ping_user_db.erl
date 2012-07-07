%% Author: Manuel Gomez
-module(ping_user_db).

-include("records.hrl").
-include("defaults.hrl").
-include_lib("deps/emysql/include/emysql.hrl").


-export([find/1,find/2,create/4]).

-spec find(integer()) -> [#user{}].
find(Id) ->
  find(Id, undefined).

find(Id, Password) ->
  Result = case Password of
    undefined ->
      ping_db:find(?USER_TABLE,[{where,[{id,lists:flatten(io_lib:format("~p",[Id]))}]}]);
    _ ->
      CryptoPass = erlsha2:sha512(list_to_binary(Password)),
      HexPass = hmac:hexlify(CryptoPass),
      io:format("Hex: ~p\n", [HexPass]),
      ping_db:find(?USER_TABLE,[{where,[
              {id,lists:flatten(io_lib:format("~p",[Id]))},
              {password, HexPass}
              ]}])
  end,
  emysql_util:as_record(Result, user, record_info(fields, user)).

-spec create(string(),string(),string(),string()) -> {ok,pos_integer()}.
create(Name,Email,Password,Tagline) -> 
  CryptoPass = erlsha2:sha512(list_to_binary(Password)),
  HexPass = hmac:hexlify(CryptoPass),
  Result = ping_db:create(?USER_TABLE,[{name,Name},{email,Email},{password,HexPass},{tagline,Tagline}]).
