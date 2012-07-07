%%% Author  : Marcos Almonacid
-module(ping_sup).
-behaviour(supervisor).

-export([init/1]).

-export([start_link/0]).

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
    Childs = [
							{db,{ping_db,start_link,[]},permanent, brutal_kill, worker,[ping_db]},
							{web,{ping_web,start_link,[ping_utils:get_env(web_host),ping_utils:get_env(web_port)]},permanent, brutal_kill, worker,[ping_web]},
				      %%{ping_pinger_sup, {ping_pinger_sup, start_link,[]}, permanent, brutal_kill, supervisor, [ping_pinger_sup]},
				      {session_sup, {ping_session_sup, start_link,[]}, permanent, brutal_kill, supervisor, [ping_session_sup]},
              {notifier,{ping_notifier,start_link,[]},permanent, brutal_kill, worker,[ping_notifier]}
						 ],
		lager:info("Initializing Ping Supervisor...~n", []),
    {ok,{{one_for_all,5,10}, Childs}}.


