%%% -------------------------------------------------------------------
%%% Author  : CB DePue III
%%% -------------------------------------------------------------------
-module(ping_session).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,stop/1,create_session/0,create_session/1,has_session/1,get_session/1,save_session/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { tablepid=undefined :: undefined | integer() }).
-record(sid, { sid=undefined :: undefined | integer(),
               time=undefined :: undefined | integer(),
               proplist=[]    :: list()
  }).

-define(SESSION_SIZE, 20).
-define(SESSION_TIMEOUT, 1000 * 5).
-define(NEXT_SESSION_TIMEOUT, ?SESSION_TIMEOUT + calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(erlang:now()))).

%% ====================================================================
%% External functions
%% ====================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE,[],[]).

-spec stop(pid()) -> ok.
stop(Pid) when is_pid(Pid) -> 
  gen_server:cast(Pid, stop).


-spec has_session(binary()) -> boolean().
has_session(Sid) -> 
  gen_server:call(?MODULE, {has_session,Sid}).
  
-spec create_session() -> {term(), tuple()}.
create_session() -> 
  create_session(none).

-spec create_session(integer() | none) -> {term(), tuple()}.
create_session(Uid) -> 
  gen_server:call(?MODULE, {create_session,Uid}).

save_session(Sid,Proplist) -> 
  gen_server:call(?MODULE, {save_session,Sid,Proplist}).

-spec get_session(binary()) -> {term(), tuple()}.
get_session(Sid) -> 
  gen_server:call(?MODULE, {get_session,Sid}).

%% ====================================================================
%% Server functions
%% ====================================================================
-spec init([]) -> {ok,#state{}}.
init([]) ->
  lager:info("Initializing Session State", []),
  {ok, #state{}}.

-spec handle_call(term(),{pid(),term()},#state{}) -> {reply,term(),#state{}}.
handle_call({has_session, Sid}, _From, State = #state{tablepid=Tid}) ->
  Reply = ets:member(Tid,Sid),
  {reply, Reply, State};

handle_call({create_session, Uid}, _From, State = #state{tablepid=Tid}) ->
  Reply = new_session(Tid,Uid),
  {reply, Reply, State};

handle_call({save_session, Sid, Proplist}, _From, State = #state{tablepid=Tid}) ->
  Reply = case ets:lookup(Tid,Sid) of
    [] -> false;
    [#sid{sid=Sid,proplist=Proplist}] -> {Sid,Proplist}
  end,
  {reply, Reply, State};

handle_call({get_session, Sid}, _From, State = #state{tablepid=Tid}) ->
  Reply = case ets:lookup(Tid,Sid) of
    [] -> false;

    [#sid{sid=Sid,proplist=Proplist}] -> {Sid,Proplist}
  end,
  {reply, Reply, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

-spec handle_cast(term(),#state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(term(),#state{}) -> {noreply, #state{}}.
handle_info({'ETS-TRANSFER',TablePid,_FromPid,_PointlessReason}, State) ->
  lager:debug("got a session table ~p ~n ",[TablePid]),
  NewState = State#state{tablepid=TablePid},
  {noreply, NewState};

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
get_session_id() -> 
  list_to_binary(ping_hex:to_hex(crypto:rand_bytes(?SESSION_SIZE))).

new_session(Tid,Uid) -> 
  Sid = get_session_id(),
  Then = ?NEXT_SESSION_TIMEOUT,
  Proplist = [{uid,Uid}],
  ets:insert(Tid,#sid{sid=Sid,time=Then,proplist=Proplist}),
  {Sid,Proplist}.

