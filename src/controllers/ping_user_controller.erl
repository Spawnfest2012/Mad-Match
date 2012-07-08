%%% -------------------------------------------------------------------
%%% Author  : C B DePue III
%%% -------------------------------------------------------------------
-module(ping_user_controller).
-behaviour(ping_controller).
-extends(ping_controller).

-export([render/2]).

render(Req,Session) ->
  UserId = ?BASE_MODULE:param(Req,2),
  Pingers = lists:map(fun(P) -> Pl = ping_utils:record_to_proplist(P), {ok,Html} = pinger_dtl:render(Pl), Html end, ping_pinger_db:find_all_by_user_id(UserId)),
  NewSession = Session ++ [{pingers,Pingers}],
  index_dtl:render(NewSession).


