%%% Author  : CB DePue III
-module(ping_session_sup).
-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

-define(TIMEOUT,5000).

%% ====================================================================
%% External functions
%% ====================================================================
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ====================================================================
%% Server functions
%% ====================================================================
init([]) ->
    Children = [
              {ping_session,{ping_session_manager,start_link,[]},permanent, ?TIMEOUT, worker,[ping_session_manager]}
						 ],
		lager:info("Initializing Ping Session Supervisor...~n", []),
    {ok,{{one_for_one,5,10}, Children}}.



