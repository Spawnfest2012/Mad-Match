-module(ping_web_handler_api).
-export([init/3, handle/2, terminate/2]).

%%
%% API Functions
%%

init({tcp, http}, Req, Opts) ->
  {ok, Req, undefined}.

handle(Req, undefined = State) ->
  {ok, Req2} = cowboy_http_req:reply(404, [], <<"ENOENT dude">>, Req),
  {ok, Req2, State}.

terminate(Req, State) ->
  ok. 
