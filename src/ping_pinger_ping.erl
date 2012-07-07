-module(ping_pinger_ping).
-behaviour(ping_pinger).

-export([handle_ping/1]).
-include("records.hrl").

-spec handle_ping(#pinger{}) -> up | down.
handle_ping(Pinger) ->
  lager:info("ping_pinger_ping: Pinging"),
  Host = Pinger#pinger.end_point,
  try
    case gen_icmp:ping(Host) of
      [{ok,_,_,_,_}|_] -> up;
      {ok,_,_,_,_} -> up;
      [{{error,timeout},_,_}|_] -> down;
      {{error,timeout},_,_} -> down;
      _     -> down
    end
  catch
    _:Err -> lager:debug("~p",[Err]),  down
  end.
