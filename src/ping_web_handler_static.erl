-module(ping_web_handler_static).
-export([init/3, handle/2, terminate/2]).

%%
%% API Functions
%%

init({tcp, http}, Req, []) ->
  {ok, Req, undefined_state};
init({tcp, http}, Req, OnlyFile) ->
  {ok, Req, OnlyFile}.

handle(Req, undefined_state = State) ->
  {[_|Path], Req2} = cowboy_http_req:path(Req),
  send(Req2, Path, State);
handle(Req, OnlyFile = State) ->
  send(Req, OnlyFile, State).

terminate(Req, State) ->
  ok.

%%
%% Local functions
%%

send(Req, PathBins, State) ->
  Path = [ binary_to_list(P) || P <- PathBins ],
  io:format("Path: ~p\n", [Path]),
  case file(filename:join(Path)) of
    {ok, Body} ->
      Headers = [{<<"Content-Type">>, <<"text/html">>}],
      {ok, Req2} = cowboy_http_req:reply(200, Headers, Body, Req),
      {ok, Req2, State};
    _ ->
      {ok, Req2} = cowboy_http_req:reply(404, [], <<"<body>404 Not Found :(</body>">>, Req),
      {ok, Req2, State}
  end.

file(Path) ->
  io:format("FILE: ~p\n", [Path]),
  file:read_file(Path).
