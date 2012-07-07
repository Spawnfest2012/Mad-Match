%%% -------------------------------------------------------------------
%%% -------------------------------------------------------------------
-module(ping_addping_controller).
-behaviour(ping_controller).

-export([render/2]).

render(Req,Session) ->
  addping_dtl:render().

