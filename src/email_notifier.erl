%%% Author  : Marcos Almonacid
-module(email_notifier).

-behaviour(gen_event).

-include("records.hrl").

-define(PINGER_DOWN_EMAIL(Name,DownTime),[{subject,"Server Down: "++Name},
                                          {body,"The server "++Name++" fell "++integer_to_list(DownTime)++" minutes ago."}]).
-define(PINGER_UP_EMAIL(Name,DownTime),[{subject,"Server is back: "++Name},
                                          {body,"The server "++Name++" is up again :D."}]).


-export([]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

init([]) ->
  {ok, #state{}}.

-spec handle_event(#event{} | term(),#state{}) -> {ok,#state{}}. 
handle_event({#event{ type = EventType, pinger_id = EventPingerId, down_time = EventDownTime }}, State) ->
  Pinger = pinger_db:find(EventPingerId),
  Emails = user_db:get_emails(EventPingerId,EventType,EventDownTime),
  case EventType of
    pinger_down ->
      lists:foreach(fun(Email) -> send_email(Email,[{subject,?PINGER_DOWN_EMAIL(Pinger#pinger.name,EventDownTime)}]) end, Emails);
    pinger_up ->
      lists:foreach(fun(Email) -> send_email(Email,[{subject,?PINGER_UP_EMAIL(Pinger#pinger.name,EventDownTime)}]) end, Emails)
  end,
  {ok, State};
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
send_email(Email,[{subject,Subject},{body,Body}]) ->
  lager:info("sending email: ~p ~p",[Email,Subject]).
