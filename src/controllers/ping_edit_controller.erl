%%% -------------------------------------------------------------------
%%% Author  : C B DePue III
%%% -------------------------------------------------------------------
-module(ping_edit_controller).
-behaviour(ping_controller).
-extends(ping_controller).

-export([render/2]).

render(Req,Session) ->
  case ping_session:is_logged_in(Session) of
    true -> 
      [Urecord] = ping_user_db:find(proplists:get_value(uid,Session)),
      edit_dtl:render(Session ++ [{password_label,"Re-enter Password to Save"},{button_label,"Edit My Account"}] ++ ping_utils:record_to_proplist(Urecord));
    false -> ?BASE_MODULE:redirect(301, "/", Req)
  end.

