%%% Author  : Marcos Almonacid
-module(email_notifier).

-behaviour(gen_event).

-include("records.hrl").

-define(PINGER_DOWN_EMAIL(Name,DownTime),[{subject,"Server Down: "++Name},
                                          {body,"The server "++Name++" fell "++integer_to_list(DownTime)++" minutes ago."}]).
-define(PINGER_UP_EMAIL(Name),[{subject,"Server is back: "++Name},
                               {body,"The server "++Name++" is up again :D."}]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

init([]) ->
  {ok, #state{}}.

-spec handle_event(#event{} | term(),#state{}) -> {ok,#state{}}. 
handle_event({#event{ type = Type, pinger = Pinger, down_time = DownTime }}, State) ->
  Emails = ping_pinger_db:get_emails(Pinger#pinger.id,Type,DownTime),
  case Type of
    pinger_down ->
      lists:foreach(fun(Email) -> send(Email,?PINGER_DOWN_EMAIL(Pinger#pinger.name,DownTime)) end, Emails);
    pinger_up ->
      lists:foreach(fun(Email) -> send(Email,?PINGER_UP_EMAIL(Pinger#pinger.name)) end, Emails)
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
send(Email,[{subject,Subject},{body,Body}]) ->
  lager:debug("sending email: ~p ~p",[Email,Subject]),
  mailer:send({ping_utils:get_env(email_host), ping_utils:get_env(email_port)},
              {ping_utils:get_env(email_name),ping_utils:get_env(email_addr),ping_utils:get_env(email_pwd)},
              [Email], Subject, Body).
