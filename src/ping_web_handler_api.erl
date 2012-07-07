-module(ping_web_handler_api).
-export([init/3, handle/2, terminate/2]).

-define(NOT_FOUND, [404, <<"<body> 404, not found </body>">>]).

%%
%% API Functions
%%

init({tcp, http}, Req, Opts) ->
  {ok, Req, Opts}.

handle(Req, State) ->
  {Method, _}            = cowboy_http_req:method(Req),
  {[_|[Object|Args]], _} = cowboy_http_req:path(Req),
  Body                   = cowboy_http_req:body(Req),

  lager:info("Method: ~p || Object: ~p || Args: ~p\n", [Method, Object, Args]),
  
  [Status, Response] = case Object of
    <<"user">>         -> handle_user(Method, Args, Body);
    <<"pinger">>       -> handle_pinger(Method, Args, Body);
    <<"subscription">> -> handle_subscription(Method, Args, Body);
    <<"alert">>        -> handle_alert(Method, Args, Body);
    <<"firehose">>     -> handle_firehose(Method, Args, Body);
    _                  -> handle_unknown(Method, Args, Body)
  end,
  {ok, Req2} = cowboy_http_req:reply(Status, [], Response, Req),
  {ok, Req2, State}.

terminate(_Req, _State) ->
  ok.

%%
%% Local Functions
%%

handle_user('PUT', Args, Req) ->
  [200, <<"<body>User Created</body>">>];
handle_user(_, _, _) ->
  ?NOT_FOUND.

handle_pinger('PUT', Args, Req) ->
  [200, <<"<body>Pinger Created</body>">>];
handle_pinger('DELETE', Args, Req) ->
  [200, <<"<body>Pinger Deleted</body>">>];
handle_pinger(_, _, _) ->
  ?NOT_FOUND.

handle_subscription('PUT', Args, Req) ->
  [200, <<"<body>Subscription Created</body>">>];
handle_subscription('DELETE', Args, Req) ->
  [200, <<"<body>Subscription Deleted</body>">>];
handle_subscription(_, _, _) ->
  ?NOT_FOUND.

handle_alert('PUT', Args, Req) ->
  [200, <<"<body>Alert Created</body>">>].

handle_firehose('GET', Args, Req) ->
  [200, <<"<body> Here is your f*****g firehose </body>">>].

handle_unknown(Object, Args, Req) ->
  [404, <<"404 dude">>].
