-module(ping_pinger_sup).

-define(MANAGERS, 400). %%NOTE: To reduce message_queue_lens on massive user initialization
-include("records.hrl").

-behaviour(supervisor).

-export([start_link/0, start_pinger/1, init/1, count_pingers/0]).

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc  Starts the supervisor process
-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc  Starts a new client process
-spec start_pinger(tuple()) -> {ok, pid()} | {error, term()}.
start_pinger({Id,Name,Type,UserId,EndPoint,Frequency,Data}) ->
  _ = random:seed(erlang:now()),
  
  Pinger = #pinger{id=Id,name=Name,type=Type,user_id=UserId,end_point=EndPoint,frequency=Frequency,data=Data},
  Manager =
    list_to_atom("ping-pinger-manager-" ++ integer_to_list(random:uniform(?MANAGERS))),
  supervisor:start_child(Manager, [Pinger]).

%% @doc  Returns the count of reigstered pinger under the supervision of this process
-spec count_pingers() -> non_neg_integer().
count_pingers() ->
  lists:sum(
    lists:map(
      fun(I) ->
              proplists:get_value(
                active,
                supervisor:count_children(
                  list_to_atom("ping-pinger-manager-" ++ integer_to_list(I))),
                0)
      end, lists:seq(1, ?MANAGERS))).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
  lager:info("Pinger supervisor initialized~n", []),
  Managers =
    [{list_to_atom("ping-pinger-manager-" ++ integer_to_list(I)),
      {ping_pinger_mgr, start_link,
       [list_to_atom("ping-pinger-manager-" ++ integer_to_list(I))]},
      permanent, brutal_kill, supervisor, [ping_pinger_mgr]}
     || I <- lists:seq(1, ?MANAGERS)],
  {ok, {{one_for_one, 5, 10}, Managers}}.
