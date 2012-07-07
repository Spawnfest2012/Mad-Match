-module(ping_pinger).
-behaviour(gen_fsm).
-include("records.hrl").
-callback handle_ping(Pinger::#pinger{}) -> up|down.

-export([start_link/1]).

-export([init/1, up/2, down/2, stop/1, handle_event/3, terminate/3]).

-record(state, {
    pinger :: #pinger{},
    down_since :: term()
    }).

-define(GET_MODULE(S), case S#state.pinger#pinger.type of
    ping -> ping_pinger_ping;
    dns  -> ping_pinger_dns;
    http -> ping_pinger_http
  end).

build_process_name(Id) ->
  list_to_atom(?MODULE_STRING ++ [$-|integer_to_list(Id)]).

start_link(Pinger) ->
  gen_fsm:start_link({local, build_process_name(Pinger#pinger.id)}, ?MODULE, Pinger, []).

init(Pinger) ->
  lager:info("Init"),
  {ok, up, #state{pinger = Pinger}, Pinger#pinger.frequency}.

up(timeout, State) ->
  lager:info("UP"),
  NextState = ?GET_MODULE(State):handle_ping(State#state.pinger),
  case NextState of
    down ->
      {next_state, NextState,
       State#state{down_since = ping_utils:now()},
       erlang:trunc(((State#state.pinger)#pinger.frequency) / 10)};
    up ->
      {next_state, NextState, State, (State#state.pinger)#pinger.frequency}
  end.

down(timeout, State) ->
  lager:info("Down"),
  NextState = ?GET_MODULE(State):handle_ping(State#state.pinger),
  case NextState of
    up ->
      {next_state, NextState,
       State#state{down_since = undefined},
       State#state.pinger#pinger.frequency};
    down ->
      {next_state, NextState, State, erlang:trunc(((State#state.pinger)#pinger.frequency) / 10)}
  end.

stop(Id) ->
  gen_fsm:send_all_state_event(build_process_name(Id), stop).

handle_event(stop, _, State) ->
  {stop, normal, State}.

terminate(_Reason, _StateName, _StateData) ->
  ok.
