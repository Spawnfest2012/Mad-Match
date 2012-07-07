-module(ping_pinger_mgr).

-behaviour(supervisor).

-export([start_link/1, start_pinger/1, init/1]).

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc  Starts the supervisor process
-spec start_link(atom()) -> ignore | {error, term()} | {ok, pid()}.
start_link(Name) ->
  supervisor:start_link({local, Name}, ?MODULE, []).

%% @doc  Starts a new client process
-spec start_pinger([proplists:property()]) -> {ok, pid()} | {error, term()}.
start_pinger(Pinger) ->
  supervisor:start_child(?MODULE, [Pinger]).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init([]) -> {ok, {{simple_one_for_one, 100, 1}, [{ping_pinger, {ping_pinger, start_link, []}, transient, brutal_kill, worker, [ping_pinger]}]}}.
init([]) ->
  {ok, {{simple_one_for_one, 100, 1},
        [{ping_pinger, {ping_pinger, start_link, []},
          transient, brutal_kill, worker, [ping_pinger]}]}}.