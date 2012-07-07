%%% -------------------------------------------------------------------
%%% Author  : Marcos Almonacid
%%% -------------------------------------------------------------------
-module(ping_db).
-behaviour(gen_server).

-include_lib("deps/emysql/include/emysql.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,stop/1,execute/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([find/2,create/2]).

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

-spec execute(binary()) -> term().
execute(Query) ->
  R = emysql:execute(?MODULE, Query),
  R#result_packet.rows.


-spec find(binary(),[{atom,term()}]) -> list().
find(Table,Options) -> 
  gen_server:call(?MODULE, {find,Table,Options}).

-spec create(binary(),[{atom,term()}]) -> list().
create(Table,Fields) -> 
  gen_server:call(?MODULE, {create,Table,Fields}).

%% ====================================================================
%% Server functions
%% ====================================================================
-spec init([]) -> {ok,#state{}}.
init([]) ->
  lager:info("Initializing DB", []),
  emysql:add_pool(?MODULE, 1,
                  ping_utils:get_env(mysql_user), ping_utils:get_env(mysql_pwd), ping_utils:get_env(mysql_host),
                  ping_utils:get_env(mysql_port), ping_utils:get_env(mysql_db), utf8),
  {ok, #state{}}.


-spec handle_call(term(),{pid(),term()},#state{}) -> {reply,term(),#state{}}.
handle_call({create,Table,Fields}, _From, State) ->
  Parameters =  string:join(lists:map(fun({Key,Value})-> atom_to_list(Key) ++ " = ?" end,Fields),", "),
  Values =lists:map(fun({Key,Value})-> Value end,Fields),
  emysql:prepare(list_to_atom("create_" ++Table), list_to_binary("INSERT INTO " ++ Table ++ " SET " ++ Parameters ++ " ")),
  Reply = case emysql:execute(?MODULE,list_to_atom("create_" ++Table),Values) of
    {ok_packet,_,_,Id,_,_,_}          -> {ok,Id};
    {error_packet, _, _, Status, Msg} -> {error, Msg}
  end,
  {reply, Reply, State};
handle_call({find,Table,Options}, _From, State) ->
  {ok, Query} = make_select_query(Table,Options),
  Result = emysql:execute(?MODULE,Query),
  {reply, Result, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.


-spec handle_cast(term(),#state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(term(),#state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
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

-spec make_select_query(string(),[{atom(),term()}]) -> {ok,binary()} | {error,term()}.
make_select_query(Table,Options) ->
  try
    {ok,
     list_to_binary("SELECT * FROM " ++ Table ++ add_options(Options) )}
  catch
    _:Err -> {error,Err}
  end.

-spec add_options([{atom(),term()}]) -> {ok,string()} | {error,term()}.
add_options([]) ->
  "";
add_options([Option|Rest]) ->
  add_option(Option) ++
  add_options(Rest).

-spec add_option({where,[{atom, string()}]}) -> string().
add_option({where,[]}) ->
  [];
add_option({where,Filters}) ->
  " WHERE " ++
  lists:foldl(fun(Filter,Acc) ->
                %% If Filter's not the last, add " AND" to the end
                %% If Filter's a tuple with 3 elements, add " BETWEEN "
                case {lists:last(Filters), Filter} of
                  {Filter,{Key,Value}} ->
                    Acc ++ atom_to_list(Key) ++ " = '" ++ Value ++ "'";
                  {Filter,{Key,From,To}} ->
                    Acc ++ atom_to_list(Key) ++ " BETWEEN '" ++ From ++ "' AND '" ++ To ++ "'";
                  {_,{Key,Value}} ->
                    Acc ++ atom_to_list(Key) ++ " = '" ++ Value ++ "' AND ";
                  {_,{Key,From,To}} ->
                    Acc ++ atom_to_list(Key) ++ " BETWEEN '" ++ From ++ "' AND '" ++ To ++ "' AND "
                end
              end, "", Filters).
