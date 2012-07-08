%%% -------------------------------------------------------------------
%%% Author  : C B DePue III
%%% -------------------------------------------------------------------
-module(ping_reel_controller).
-behaviour(ping_controller).
-extends(ping_controller).

-export([render/2]).

-define(PER_PAGE, 20).

render(Req,Session) ->
  {Qs, _} = cowboy_http_req:body_qs(Req),
  [Page] = ?BASE_MODULE:get_parameters(Qs, [<<"page">>]),

  %% use extends
  Pingers = lists:map(fun(P) -> Pl = ping_utils:record_to_proplist(P), {ok,Html} = pinger_dtl:render(Pl), Html end, ping_pinger_db:firehose(Page,?PER_PAGE)),
  NewSession = Session ++ [{pingers,Pingers}],
  erlydtl:compile(<<" {% for p in pingers %} {{ p }} {% endfor %} ">>, pinglist),
  pinglist:render(NewSession).


