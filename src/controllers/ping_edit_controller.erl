%%% -------------------------------------------------------------------
%%% Author  : C B DePue III
%%% -------------------------------------------------------------------
-module(ping_edit_controller).
-behaviour(ping_controller).
-extends(ping_controller).

-export([render/2]).

render(Req,Session) ->
  case ping_session:is_logged_in(Session) of
    true -> edit_dtl:render(Session ++ [{button_label,"Edit My Account"}]);
    false -> ?BASE_MODULE:redirect(301, "/", Req)
  end.

