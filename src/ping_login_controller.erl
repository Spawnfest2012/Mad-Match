%%% -------------------------------------------------------------------
%%% Author  : C B DePue III
%%% -------------------------------------------------------------------
-module(ping_login_controller).
-behaviour(ping_controller).

-export([render/2]).

render(Req,Session) ->
  login_dtl:render().


