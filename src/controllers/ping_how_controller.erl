%%% -------------------------------------------------------------------
%%% Author  : C B DePue III
%%% -------------------------------------------------------------------
-module(ping_how_controller).
-behaviour(ping_controller).

-export([render/2]).

render(Req,Session) ->
  how_dtl:render(Session).


