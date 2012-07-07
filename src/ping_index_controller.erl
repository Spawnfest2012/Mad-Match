%%% -------------------------------------------------------------------
%%% Author  : C B DePue III
%%% -------------------------------------------------------------------
-module(ping_index_controller).
-behaviour(ping_controller).

-export([render/2]).

render(Req,Session) ->
  NewSession = Session ++ [{pingers,[]}],
  lager:warning("~p ~n",[NewSession]),
  index_dtl:render(NewSession).

