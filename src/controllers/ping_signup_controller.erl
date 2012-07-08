%%% -------------------------------------------------------------------
%%% Author  : C B DePue III
%%% -------------------------------------------------------------------
-module(ping_signup_controller).
-behaviour(ping_controller).

-export([render/2]).

render(Req,Session) ->
  signup_dtl:render(Session ++ [{name,""},{email,""},{twitter,""},{tagline,""},{password_label,"Password"},{button_label,"I'm pingterested, sign me up!"}]).


