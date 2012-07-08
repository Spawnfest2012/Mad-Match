%%% -------------------------------------------------------------------
%%% Author  : CB DePue III
%%% -------------------------------------------------------------------
-module(ping_session).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,stop/1,create_or_update_cowboy_session_request/1,delete_session/1,create_session/0,create_session/1,is_logged_in/1,get_session/1,save_session/1, save_session/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { tablepid=undefined :: undefined | integer() }).
-record(sid, { sid=undefined :: undefined | integer(),
               time=undefined :: undefined | integer(),
               proplist=[]    :: list()
  }).

-define(SESSION_SIZE, 30). %% size in bytes of random string
-define(SESSION_TIMEOUT, 10000). %% time in seconds
-define(NOW, calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(erlang:now()))).
-define(NEXT_SESSION_TIMEOUT, ?SESSION_TIMEOUT + ?NOW).
-define(PINGTEREST_SESSION, <<"pingterest_session">>).

%% ====================================================================
%% External functions
%% ====================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE,[],[]).

-spec stop(pid()) -> ok.
stop(Pid) when is_pid(Pid) -> 
  gen_server:cast(Pid, stop).


-spec is_logged_in(list()) -> boolean().
is_logged_in(Proplist) -> 
  Sid = proplists:get_value(sid,Proplist),
  gen_server:call(?MODULE, {is_logged_in,Sid}).

-spec create_or_update_cowboy_session_request(any()) -> {ok, any()}.
create_or_update_cowboy_session_request(Req) -> 
  {OldSession,_} = cowboy_http_req:cookie(?PINGTEREST_SESSION, Req),
  {Sid, Proplist} = get_session(OldSession),

  %% special case - insert an 'is_logged_in' property because many controllers need this
  Proplist2 = case proplists:get_value(uid,Proplist) of
    Id when is_integer(Id) -> lists:keystore(is_logged_in,1,Proplist,{is_logged_in,true});
    _ -> Proplist
  end,
  {cowboy_http_req:set_resp_cookie(
   ?PINGTEREST_SESSION , Sid, [{path, "/"}], Req),Proplist2}.
  
-spec create_session() -> {term(), tuple()}.
create_session() -> 
  create_session(none).

-spec create_session(integer() | none) -> {term(), tuple()}.
create_session(Uid) -> 
  gen_server:call(?MODULE, {create_session,Uid}).

-spec delete_session(list() | none) -> ok.
delete_session(Proplist) -> 
  Sid = proplists:get_value(sid,Proplist),
  gen_server:call(?MODULE, {delete_session,Sid}).

-spec save_session(list()) -> {term(), tuple()}.
save_session(Proplist) -> 
  Sid = proplists:get_value(sid,Proplist),
  lager:warning("sid is ~p ~n",[Sid]),
  save_session(Sid,Proplist).

-spec save_session(binary(),list()) -> {term(), tuple()}.
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
handle_call({is_logged_in, Sid}, _From, State = #state{tablepid=Tid}) ->
  Reply = case ets:lookup(Tid,Sid) of
    [] -> false;
    [#sid{sid=Sid,proplist=Proplist}] ->
      case proplists:get_value(uid,Proplist) of
        Id when is_integer(Id) -> true;
        _ -> false
      end
  end,
  {reply, Reply, State};

handle_call({create_session, Uid}, _From, State = #state{tablepid=Tid}) ->
  Reply = new_session(Tid,Uid),
  {reply, Reply, State};

handle_call({delete_session, Sid}, _From, State = #state{tablepid=Tid}) ->
  ets:delete(Tid,Sid),
  {reply, ok, State};

handle_call({save_session, Sid, Proplist}, _From, State = #state{tablepid=Tid}) ->
  lager:warning("ets lookup is  ~p ~n",[ets:lookup(Tid,Sid)]),
  Reply = case ets:lookup(Tid,Sid) of
    [] -> false;
    [Record=#sid{sid=Sid}] -> 
      Then = ?NEXT_SESSION_TIMEOUT,
      NewRecord = Record#sid{time=Then,proplist=Proplist},
      ets:insert(Tid,NewRecord), 
      {Sid,NewRecord}
  end,
  {reply, Reply, State};

handle_call({get_session, Sid}, _From, State = #state{tablepid=Tid}) ->
  Now = ?NOW,
  Reply = case ets:lookup(Tid,Sid) of
    [#sid{sid=Sid,proplist=Proplist,time=Time}] when Time >= Now ->
      lager:debug("session still valid time: ~p now: ~p ~n",[Time,Now]),
      {Sid,Proplist};
    [#sid{sid=Sid,proplist=Proplist,time=Time}] -> 
      lager:debug("session expired. ",[]), 
      ets:delete(Tid,Sid),
      new_session(Tid,none);
    _ -> new_session(Tid,none)
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
  Proplist = [{uid,Uid},{sid,Sid}],
  ets:insert(Tid,#sid{sid=Sid,time=Then,proplist=Proplist}),
  {Sid,Proplist}.

