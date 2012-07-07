%% Author: Marcos Almonacid
-module(pingterest).
-behaviour(application).

-export([
   start/0,
   start/2,
   stop/1
        ]).

-export([]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
-spec start() -> ok | {error, term()}.
start() ->
  application:start(cowboy),
  application:start(?MODULE).

-spec start(any(),any()) -> {ok, pid()} | {error, any()}. 
start(_Type, _StartArgs) ->
  case ping_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      {error, Error}
  end.

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

-spec user(integer()) -> list().
user(Id) -> 
  ping_db:user(Id).

-spec create_user(binary(),binary(),binary()) -> list().
create_user(Login, Email, Password) -> 
  ping_db:create_user(Login, Email, Password).

%% ====================================================================
%% Internal functions
%% ====================================================================

