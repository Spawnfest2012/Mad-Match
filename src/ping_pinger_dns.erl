-module(ping_pinger_dns).
-behaviour(ping_pinger).

-export([handle_ping/1]).
-include("records.hrl").

handle_ping(Pinger) ->
  case inet_res:gethostbyname(Pinger#pinger.end_point) of
    {ok, _} -> up;
    _       -> down
  end.
