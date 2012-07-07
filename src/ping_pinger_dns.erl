-module(ping_pinger_dns).
-behaviour(ping_pinger).

-export([handle_ping/1]).
-include("records.hrl").

handle_ping(Pinger) ->
  lager:info("ping_pinger_dns: Pinging"),
  case inet_res:gethostbyname(Pinger#pinger.end_point) of
    {ok, _} -> up;
    _       -> down
  end.
