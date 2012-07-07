%%% Author  : Marcos Almonacid
-module(email_notifier).

-behaviour(gen_event).

-export([]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

init([]) ->
    {ok, #state{}}.

handle_event(Event, State) ->
    {ok, State}.

handle_call(Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(Info, State) ->
    {ok, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

