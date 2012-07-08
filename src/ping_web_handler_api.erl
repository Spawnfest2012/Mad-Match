-module(ping_web_handler_api).
-export([init/3, handle/2, terminate/2]).

-define(NOT_FOUND, [404, <<"<body> 404, not found </body>">>]).
-define(UNAUTHORIZED, [401, <<"<body> 401, not going to happen</body>">>]).
-define(OK, [200, <<"ok">>]).
-define(CREATED, [201, <<"ok">>]).

-include("records.hrl").

%%
%% API Functions
%%

init({tcp, http}, Req, Opts) ->
  {ok, Req, Opts}.

handle(R, State) ->
  {{ok, Req}, Session} = ping_session:create_or_update_cowboy_session_request(R),
  {Method, _}            = cowboy_http_req:method(Req),
  {[_|[Object|Args]], _} = cowboy_http_req:path(Req),

  LoggedIn = ping_session:is_logged_in(Session),
  lager:info("Logged in: ~p Method: ~p || Object: ~p || Args: ~p\n", [LoggedIn, Method, Object, Args]),
  [Status, Response] = case Object of
    <<"user">>         -> handle_user(Method, Args, Req, Session);
    <<"login">>        -> handle_login(Method, Args, Req, Session);
    <<"logout">>       -> handle_logout(Method, Args, Req, Session);
    <<"pinger">>       -> handle_pinger(Method, Args, Req, Session);
    <<"subscription">> -> handle_subscription(Method, Args, Req);
    <<"alert">>        -> handle_alert(Method, Args, Req);
    <<"firehose">>     -> handle_firehose(Method, Args, Req);
    _                  -> handle_unknown(Method, Args, Req)
  end,
  lager:info("Response> Status: ~p, Response: ~p\n", [Status, Response]),
  {ok, Req2} = cowboy_http_req:reply(Status, [{<<"Content-Type">>, <<"application/json">>}], Response, Req),
  {ok, Req2, State}.

terminate(_Req, _State) ->
  ok.

%%
%% Local Functions
%%

get_parameters(Qs, Params) ->
  lists:map(fun(P) -> get_parameter(P, Qs) end, Params).

get_parameter(Key, Qs) ->
  Value = proplists:get_value(Key, Qs),
  to_list(Value).

to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) -> V.

handle_user('PUT', _Args, Req, Session) ->
  {Qs, _} = cowboy_http_req:body_qs(Req),
  [N, E, P, T] = get_parameters(Qs, [<<"name">>, <<"email">>, <<"password">>, <<"tagline">>]),
  lager:warning("creating a guy: ~p ~p ~p ~p ~n", [N, E, P, T]),
  case ping_user_db:create(N, E, P, T) of
    {ok, Uid} ->
      ping_session:save_session(lists:keystore(uid,1,Session,{uid,Uid})),
      Response = "{status: ok}, {response: {id:" ++ integer_to_list(Uid) ++ "}",
      [201, Response];
    {error, Error} ->
      lager:warning("ERROR: ~p\n", [Error]),
      Response = "{status: error}, {response: {msg:" ++ Error ++ "}",
      [400, Response]
  end;
handle_user(_, _, _, _) ->
  ?NOT_FOUND.

handle_login('POST', _Args, Req, Session) ->
  {Qs, _} = cowboy_http_req:body_qs(Req),
  [Email, Pass] = get_parameters(Qs, [<<"email">>, <<"password">>]),
  case ping_user_db:find(Email, Pass) of
    [] -> 
      ?UNAUTHORIZED;
    [User] ->
      lager:info("Storing session with user id: ~p\n", [User#user.id]),
      ping_session:save_session(lists:keystore(uid,1,Session,{uid,User#user.id})),
      ?CREATED
  end;
handle_login(_, _, _, _) ->
  ?NOT_FOUND.

handle_logout('POST', _Args, _Req, Session) ->
  ping_session:delete_session(Session),
  ?OK;
handle_logout(_, _, _, _) ->
  ?NOT_FOUND.

handle_pinger('PUT', _Args, Req, Session) ->
  % TODO: Check for session auth

  {Qs, _} = cowboy_http_req:body_qs(Req),
  lager:info("Qs: ~p\n", [Qs]),
  [Name, Type, Endpoint, Frequency] = get_parameters(Qs, [<<"name">>, <<"type">>, <<"endpoint">>, <<"frequency">>]),
  Data = case Type of
    "http" ->
      [Method, Status] = get_parameters(Qs, [<<"web_method">>, <<"web_status">>]),
      [{method, list_to_binary(Method)}, {status, list_to_binary(Status)}];
    _ -> []
  end,
  lager:info("Data: ~p\n", [Data]),
  UserId = proplists:get_value(uid, Session),
  case ping_pinger_db:create(Name, Type, UserId, Endpoint, list_to_integer(Frequency) * 1000, Data) of
    {ok, _}    -> [201, <<"{status: ok}">>];
    {_, Error} -> [400, list_to_binary(Error)]
  end;
handle_pinger('DELETE', Args, _Req, _Session) ->
  Id = lists:nth(1, Args),
  Rows = ping_pinger_db:delete( binary_to_list(Id) ),
  case Rows of
    0 -> Response = "{status: notfound}",
      [204, Response];
    _ -> Response = "{status: ok}",
      [200, Response]
  end;
handle_pinger(_, _, _, _) ->
  ?NOT_FOUND.

handle_subscription('PUT', _Args, Req) ->
  {Qs, _} = cowboy_http_req:body_qs(Req),
  [T, U, P, D, N] = get_parameters(Qs, [<<"type">>, <<"user_id">>, <<"pinger_id">>, <<"down_time">>, <<"notify_when_up">>]),
  case ping_subscription_db:create(T, U, P, D, N) of
    {ok, Id} ->
      Response = "{status: ok}, {response: {id:" ++ integer_to_list(Id) ++ "}",
      [201, Response];
    {error, Error} ->
      lager:warning("ERROR: ~p\n", [Error]),
      Response = "{status: error}, {response: {msg:" ++ Error ++ "}}",
      [400, Response]
  end;
handle_subscription('DELETE', Args, _Req) ->
  Id = lists:nth(1, Args),
  Rows = ping_subscription_db:delete( binary_to_list(Id) ),
  case Rows of
    0 -> Response = "{status: notfound}",
      [204, Response];
    _ -> Response = "{status: ok}",
      [200, Response]
  end;
handle_subscription(_, _, _) ->
  ?NOT_FOUND.

handle_alert('PUT', _Args, _Req) ->
  [200, <<"<body>Alert Created</body>">>].

handle_firehose('GET', _Args, _Req) ->
  [200, <<"<body> Here is your f*****g firehose </body>">>].

handle_unknown(_Object, _Args, _Req) ->
  [404, <<"404 dude">>].
