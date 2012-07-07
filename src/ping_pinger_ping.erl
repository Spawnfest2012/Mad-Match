-module(ping_pinger_ping).
-behaviour(ping_pinger).

-export([handle_ping/1]).
-include("records.hrl").

handle_ping(Pinger) ->
  lager:info("ping_pinger_ping: Pinging"),

  % XXX: Use gen_icmp
  Host = Pinger#pinger.end_point,
  Status = os:cmd("ping -c 1 "++Host++" &>/dev/null; echo $?"),
  case Status of
    "0\n" -> up;
    _     -> down
  end.
