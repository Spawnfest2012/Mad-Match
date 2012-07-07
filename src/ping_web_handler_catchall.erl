-module(ping_web_handler_catchall).
-export([init/3, handle/2, terminate/2]).

%%
%% API Functions
%%

init({tcp, http}, Req, []) ->
  {ok, Req, undefined}.

handle(Req, undefined = State) ->
  {ok, Req2} = cowboy_http_req:reply(404, [], <<"404 dude">>, Req),
  {ok, Req2, State}.

terminate(Req, State) ->
  ok.

