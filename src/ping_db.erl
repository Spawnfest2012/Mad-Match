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

-export([find/2,create/2, delete/2]).

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
  emysql:execute(?MODULE, Query).

-spec find(binary(),[{atom,term()}]) -> list().
find(Table,Options) -> 
  gen_server:call(?MODULE, {find,Table,Options}).

-spec create(binary(),[{atom,term()}]) -> list().
create(Table,Fields) -> 
  gen_server:call(?MODULE, {create,Table,Fields}).

-spec delete(binary(),[{atom,term()}]) -> list().
delete(Table, Options) ->
  gen_server:call(?MODULE, {delete, Table, Options}).

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
  Parameters =  string:join(lists:map(fun({Key,_})-> atom_to_list(Key) ++ " = ?" end,Fields),", "),
  Values =lists:map(fun({_,Value})-> Value end,Fields),
  emysql:prepare(list_to_atom("create_" ++Table), list_to_binary("INSERT INTO " ++ Table ++ " SET " ++ Parameters ++ " ")),
  Reply = case emysql:execute(?MODULE,list_to_atom("create_" ++Table),Values) of
    {ok_packet,_,_,Id,_,_,_}          -> {ok,Id};
    {error_packet, _, _, _, Msg} -> {error, Msg}
  end,
  {reply, Reply, State};
handle_call({find,Table,Options}, _From, State) ->
  Query = list_to_binary("SELECT * FROM " ++ Table ++ add_options(Options)),
  emysql:prepare(list_to_atom("find_" ++Table),Query),
  Values = get_values(Options),
  Result = emysql:execute(?MODULE,list_to_atom("find_" ++Table),Values),
  {reply, Result, State};
handle_call({delete,Table,Options}, _From, State) ->
  Query = list_to_binary("DELETE FROM " ++ Table ++ add_options(Options)),
  lager:info("Query ~p",[Query]),
  emysql:prepare(list_to_atom("delete_" ++Table),Query),
  Values = get_values(Options),
  lager:info("Values ~p",[Values]),
  Reply = case emysql:execute(?MODULE,list_to_atom("delete_" ++Table),Values) of
    {ok_packet,_,Rows,_,_,_,_}          -> Rows;
    {error_packet, _, _, _, Msg} -> {error, Msg}
  end,
  {reply, Reply, State};
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
                  {Filter,{Key,_}} ->
                    Acc ++ atom_to_list(Key) ++ " = ? ";
                  {Filter,{Key,From,To}} ->
                    Acc ++ atom_to_list(Key) ++ " BETWEEN '" ++ From ++ "' AND '" ++ To ++ "'";
                  {_,{Key,_}} ->
                    Acc ++ atom_to_list(Key) ++ " = ? AND ";
                  {_,{Key,From,To}} ->
                    Acc ++ atom_to_list(Key) ++ " BETWEEN '" ++ From ++ "' AND '" ++ To ++ "' AND "
                end
              end, "", Filters).


-spec get_values([{atom(),term()}]) -> {ok,string()} | {error,term()}.
get_values([]) ->
  [];
get_values([Option|Rest]) ->
  get_value(Option) ++
  get_values(Rest).

-spec get_value({where,[{atom, string()}]}) -> string().
get_value({where,[]}) ->
  [];
get_value({where,Filters}) ->
  lists:map(fun({_,Value})-> Value end,Filters).
