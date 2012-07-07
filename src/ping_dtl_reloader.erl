%%% -------------------------------------------------------------------
%%% Author  : CB DePue III
%%% -------------------------------------------------------------------
-module(ping_dtl_reloader).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

-define(REFRESH, 1000).
-define(TEMPLATE_DIR, "templates").
-define(HELPER_DIR, "helpers").

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
  lager:info("Initializing DTL Reloader", []),
  {ok, #state{}, ?REFRESH}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.


-spec handle_cast(term(),#state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(term(),#state{}) -> {noreply, #state{}}.
handle_info(timeout, State) ->
  reload_dtls(?TEMPLATE_DIR),
  reload_dtls(?HELPER_DIR),
  {noreply, State, ?REFRESH}.

-spec terminate(term(),#state{}) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term()|{down, term()},#state{},term()) -> {ok,#state{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
reload_dtls(Dir) -> 
 {ok,Files} = file:list_dir(Dir),
  lists:foreach(fun(H) ->
        lager:debug("reloading ~p ~n",[Dir ++ "/" ++ H]),
        erlydtl:compile(Dir ++ "/" ++ H, binary_to_atom(re:replace(H,"\\.","_",[{return,binary}]),utf8)) end, Files).

