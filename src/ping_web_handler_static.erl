-module(ping_web_handler_static).
-export([init/3, handle/2, terminate/2]).

%%
%% API Functions
%%

-define(BASE_PATH, "ui/public").

init({tcp, http}, Req, []) ->
  {ok, Req, undefined};
init({tcp, http}, Req, File) ->
  {ok, Req, File}.

handle(Req, undefined = State) ->
  {Path, Req2} = cowboy_http_req:path(Req),
  send(Req2, Path, State);
handle(Req, File = State) ->
  send(Req, File, State).

terminate(Req, State) ->
  ok.

%%
%% Local functions
%%

send(Req, PathBins, State) ->
  [Kontroller|_] = PathBins,
  Path = [?BASE_PATH] ++ [ binary_to_list(P) || P <- PathBins ],
  io:format("Path: ~p\n", [Path]),
  Headers = [{<<"Content-Type">>, <<"text/html">>}],
  case {file(filename:join(Path)), catch binary_to_existing_atom(Kontroller,utf8)} of
    {{ok, Body},_} ->
      {ok, Req2} = cowboy_http_req:reply(200, Headers, Body, Req),
      {ok, Req2, State};
    {{error,enoent},Controller} when is_atom(Controller) -> 
      case catch code:which(Controller) of
        non_existing -> 
          {ok, Req2} = cowboy_http_req:reply(404, [], <<"<body>404 Not Found :(</body>">>, Req),
          {ok, Req2, State};
        {'EXIT', _} -> 
          {ok, Req2} = cowboy_http_req:reply(404, [], <<"<body>404 Not Found :(</body>">>, Req),
          {ok, Req2, State};
        _ -> 
          {ok,RenderedBody} = Controller:render([]),
          {ok, Req2} = cowboy_http_req:reply(200, Headers, RenderedBody, Req),
          {ok, Req2, State}
      end;
    _ -> 
      {ok, Req2} = cowboy_http_req:reply(404, [], <<"<body>404 Not Found :(</body>">>, Req),
      {ok, Req2, State}
  end.

file(Path) ->
  io:format("FILE: ~p\n", [Path]),
  file:read_file(Path).
