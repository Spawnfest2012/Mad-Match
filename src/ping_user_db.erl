%% Author: Manuel Gomez
-module(ping_user_db).

-include("records.hrl").
-include("defaults.hrl").
-include_lib("deps/emysql/include/emysql.hrl").


-export([find/1,find/2,delete/1,update/6,create/5,all/1,fix_twitter/1]).

-define(CRYPT(P), hmac:hexlify( erlsha2:sha512(P) )).

-spec find(integer()) -> [#user{}].
find(Id) ->
  find(id, Id, undefined).

find(Pk, Password) when is_list(Pk) ->
  find(email, Pk, Password);

find(Pk, Password) when is_integer(Pk) ->
  find(id, Pk, Password).

find(KeyField, Pk, Password) -> 
  Result = case Password of
    undefined ->
      ping_db:find(?USER_TABLE,[{where,[{id,integer_to_list(Pk)}]}]);
    _ ->
      ping_db:find(?USER_TABLE,[{where,[
              {KeyField, (Pk)},
              {password, ?CRYPT(Password)}
              ]}])
  end,
  emysql_util:as_record(Result, user, record_info(fields, user)).

-spec create(string(),string(),string(),string(),string()) -> {ok,pos_integer()}.
create(Name,Email,Password,Tagline,Twitter) ->
  Result = ping_db:create(?USER_TABLE,[{name,Name},{email,Email},{password, ?CRYPT(Password)},{tagline,Tagline},{twitter,fix_twitter(Twitter)}]).

-spec delete(pos_integer()) -> pos_integer().
delete(Id) ->
  ping_db:delete(?USER_TABLE,[{where,[{id,integer_to_list(Id)}]}]).

-spec update(integer(),string(),string(),string(),string(),string()) -> {ok,pos_integer()}.
update(Id,Name,Email,Password,Tagline,Twitter) ->
  case find(Id,Password) of
    [] -> {error, "bad_password"};

    [_] -> Updates = [{name,Name},{email,Email},{tagline,Tagline},{twitter,fix_twitter(Twitter)}],
           ping_db:update(?USER_TABLE, [{where,[{id,Id}]},{update,Updates}])
  end.

-spec all(list()) -> [#user{}].
all(Options) -> 
  Result = ping_db:find(?USER_TABLE,Options),
  emysql_util:as_record(
    Result, user, record_info(fields, user)).

-spec fix_twitter(string()) -> string().
fix_twitter([]) -> [];
fix_twitter(T = [$@,_first|_ ]) -> T;
fix_twitter([$@]) -> [];
fix_twitter(T) -> [$@|T].
