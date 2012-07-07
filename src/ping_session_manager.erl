%%% -------------------------------------------------------------------
%%% Author  : CB DePue III
%%% -------------------------------------------------------------------
-module(ping_session_manager).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE,[],[]).

-spec stop(pid()) -> ok.
stop(Pid) when is_pid(Pid) -> 
  gen_server:cast(Pid, stop).

%% ====================================================================
%% Server functions
%% ====================================================================
-spec init([]) -> {ok,#state{}}.
init([]) ->
  process_flag(trap_exit, true),
  lager:info("Initializing Session Manager...", []),
  {ok,Pid} = ping_session:start_link(),
  TablePid = ets:new(sessions,[ordered_set, protected, {keypos,2}, 
      {heir,self(),hand_back}, {write_concurrency,false}, {read_concurrency,false}]),
  ets:give_away(TablePid,Pid,init_session),
  {ok, #state{}}.

-spec handle_call(term(),{pid(),term()},#state{}) -> {reply,term(),#state{}}.
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

-spec handle_cast(term(),#state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(term(),#state{}) -> {noreply, #state{}}.

handle_info({'ETS-TRANSFER',TablePid,_FromPid,hand_back}, State) ->
  lager:debug("got a session table back from the session manager! ~p ~n ",[TablePid]),
  {ok,Pid} = ping_session:start_link(),
  ets:give_away(TablePid,Pid,init_session),
  {noreply, State};

handle_info({'EXIT', _TablePid, _Reason}, State) ->
  lager:warning("session manager died!",[]),
  {noreply, State};

handle_info(Info, State) ->
  lager:warning("got something : ~p ~n ",[Info]),
  {noreply, State}.

-spec terminate(term(),#state{}) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term()|{down, term()},#state{},term()) -> {ok,#state{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

