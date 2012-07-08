%%% -------------------------------------------------------------------
%%% Author  : C B DePue III
%%% -------------------------------------------------------------------
-module(ping_user_controller).
-behaviour(ping_controller).
-extends(ping_controller).

-export([render/2]).

render(Req,Session) ->
  User = ?BASE_MODULE:param(Req,2),
  Pingers = lists:map(fun(P) -> Pl = ping_utils:record_to_proplist(P), {ok,Html} = pinger_dtl:render(Pl), Html end, ping_pinger_db:firehose(1,20)),
  NewSession = Session ++ [{pingers,Pingers}],
  index_dtl:render(NewSession).


