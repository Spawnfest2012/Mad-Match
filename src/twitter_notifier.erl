%%% Author  : Marcos Almonacid
-module(twitter_notifier).

-behaviour(gen_event).

-include("records.hrl").

-define(BASE_URL, "https://api.twitter.com/1/statuses/update.xml").
-define(PINGER_DOWN_TWEET(TwitterUser,PingerName,DownTime), TwitterUser ++ " The server "++PingerName++" has been down for "++integer_to_list(DownTime)++" minutes.").
-define(PINGER_UP_TWEET(TwitterUser,PingerName), TwitterUser ++ " The server "++PingerName++" is up again.").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

init([]) ->
  {ok, #state{}}.

-spec handle_event(#event{} | term(),#state{}) -> {ok,#state{}}. 
handle_event(#event{ type = Type, pinger = Pinger, down_time = DownTime }, State) ->
  Twitters = ping_pinger_db:get_subscriptions(twitter,Pinger#pinger.id,Type,DownTime),
  case Type of
    pinger_down ->
      DownTimeMins = trunc((DownTime/1000)/60),
      lists:foreach(fun(TwitterHandle) -> send(?PINGER_DOWN_TWEET(binary_to_list(TwitterHandle),Pinger#pinger.name,DownTimeMins)) end, Twitters);
    pinger_up ->
      lists:foreach(fun(TwitterHandle) -> send(?PINGER_UP_TWEET(binary_to_list(TwitterHandle),Pinger#pinger.name)) end, Twitters)
  end,
  {ok, State};
handle_event(Event, State) ->
  lager:info("twitter notifier, unknown event: ~p",[Event]),
  {ok, State}.

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
-spec send(string()) -> ok.
send(Tweet) ->
  lager:debug("sending tweet: ~p",[Tweet]),
  case oauth:post(?BASE_URL, [{status, Tweet}], {ping_utils:get_env(twitter_consumer),
                                                 ping_utils:get_env(twitter_consumer_secret),
                                                 hmac_sha1},
                                                 ping_utils:get_env(twitter_token),
                                                 ping_utils:get_env(twitter_token_secret)) of
    {ok, _, _, "Failed to validate oauth signature or token"} -> {oauth_error, "Failed to validate oauth signature or token"};
    {ok, "401", _, Body} -> {oauth_error, Body};
    {ok, "500", _, _Body} -> {error, server_error};
    {ok, "502", _, _Body} -> {error, overburdened};
    {ok, [$2, _, _], _Headers, _Body} -> lager:debug("tweet sended");
    {ok, Code, _Headers, Body} -> {error, ["Response code: ", Code, "\n\nResponse Body:\n", Body]};
    Other -> Other
  end.