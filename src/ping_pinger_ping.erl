-module(ping_pinger_ping).
-behaviour(ping_pinger).

-export([handle_ping/1]).

handle_ping(Pinger) ->
  lager:info("ping_pinger_ping: Pinging"),
  case random:uniform(2) of 1 -> up; 2 -> down end.
