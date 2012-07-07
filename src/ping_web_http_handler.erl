-module(ping_web_http_handler).
-export([init/3, info/3, terminate/2]).

-define(TIMEOUT, 60000).

init({tcp, http}, Req, Opts) ->
  {loop, Req, undefined_state, ?TIMEOUT, hibernate}.

info({reply, Body}, Req, State) ->
  {ok, Req2} = cowboy_http_req:reply(200, [], Body, Req),
  {ok, Req2, State};
info(Message, Req, State) ->
  {loop, Req, State, hibernate}.

terminate(Req, State) ->
  ok.
