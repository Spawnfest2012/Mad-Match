%% Author: Marcos Almonacid
-module(pingterest).
-behaviour(application).

-export([
   start/0,
   start/2,
   stop/1,
   start_phase/3
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

-spec start_phase(boot,term(),undefined) -> ok.
start_phase(boot,_,undefined) ->
  lists:foreach(fun ping_pinger_sup:start_pinger/1,ping_pinger_db:all([])).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

