%%% -------------------------------------------------------------------
%%% Author  : C B DePue III
%%% -------------------------------------------------------------------
-module(ping_addping_controller).
-behaviour(ping_controller).
-extends(ping_controller).

-export([render/2]).

render(Req,Session) ->
  case ping_session:is_logged_in(Session) of
    true -> addping_dtl:render();
    false -> ?BASE_MODULE:redirect(301, "/", Req)
  end.

