-module(ping_pinger).
-behaviour(gen_fsm).
-include("records.hrl").
%-callback handle_ping(Pinger :: #pinger{}) -> up|down.

-define(NOTIFICATION_TIME, 6000). % 1 min

-export([start_link/1]).
-export([behaviour_info/1]).
-export([init/1, up/2, down/2, stop/1, handle_event/3, terminate/3]).

-record(state, {
    pinger :: #pinger{},
    down_since = undefined :: pos_integer(),
    last_notification_time = 0 :: pos_integer(), 
    notify_up = false :: boolean()
    }).

-define(GET_MODULE(S), case S#state.pinger#pinger.type of
    ping -> ping_pinger_ping;
    dns  -> ping_pinger_dns;
    http -> ping_pinger_http
  end).

behaviour_info(callbacks) -> [{handle_ping, 1}];
behaviour_info(_) -> undefined.

build_process_name(Id) ->
  list_to_atom(?MODULE_STRING ++ [$-|integer_to_list(Id)]).

start_link(Pinger) ->
  gen_fsm:start_link({local, build_process_name(Pinger#pinger.id)}, ?MODULE, Pinger, []).

init(Pinger) ->
  ping_pinger_db:update(Pinger#pinger.id,[{last_status,"up"},{last_check,ping_utils:now()}]),
  {ok, up, #state{pinger = Pinger}, Pinger#pinger.frequency}.

up(timeout, State) ->
  NextState = ?GET_MODULE(State):handle_ping(State#state.pinger),
  case NextState of
    down ->
      Now = ping_utils:now(),
      ping_web_handler_ws:notify((State#state.pinger)#pinger.id,<<"down">>,
                                 list_to_binary("Down for " ++ ping_utils:time_diff_now(Now))),
      ping_pinger_db:update((State#state.pinger)#pinger.id,[{last_status,"down"},{last_check,Now}]),
      {next_state, down,
       %% I'm setting the last_notification_time, I know it's odd.
       State#state{down_since = ping_utils:now(),last_notification_time = ping_utils:now()},
       erlang:trunc(((State#state.pinger)#pinger.frequency) / 10)};
    up ->
      {next_state, up, State, (State#state.pinger)#pinger.frequency}
  end.

down(timeout, State) ->
  NextState = ?GET_MODULE(State):handle_ping(State#state.pinger),
  case NextState of
    up ->
      Now = ping_utils:now(),
            ping_web_handler_ws:notify((State#state.pinger)#pinger.id,<<"up">>,
                                       list_to_binary("Responding for " ++ ping_utils:time_diff_now(Now))),
      ping_pinger_db:update((State#state.pinger)#pinger.id,[{last_status,"up"},{last_check,Now}]),
      Event = #event{type = pinger_up,pinger = State#state.pinger},
      case State#state.notify_up of
        true  -> ping_notifier:notify(Event);
        false -> ok
      end,
      {next_state, up,
       State#state{down_since = undefined, notify_up = false},
       State#state.pinger#pinger.frequency};
    down ->
      NewState = case ?NOTIFICATION_TIME =< (ping_utils:now() - State#state.last_notification_time) of
        true  -> 
          Event = #event{type = pinger_down,pinger = State#state.pinger, down_time=(ping_utils:now()-State#state.down_since)},
          ping_notifier:notify(Event),
          State#state{last_notification_time = ping_utils:now(), notify_up = true};
        false -> 
          State
      end,
      
      {next_state, down, NewState, erlang:trunc(((State#state.pinger)#pinger.frequency) / 10)}
  end.

stop(Id) ->
  gen_fsm:send_all_state_event(build_process_name(Id), stop).

handle_event(stop, _, State) ->
  {stop, normal, State}.

terminate(_Reason, _StateName, _StateData) ->
  ok.
