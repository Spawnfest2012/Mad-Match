-module(ping_pinger_http).
-behaviour(ping_pinger).

-export([handle_ping/1]).
-include("records.hrl").

handle_ping(Pinger) ->
  lager:info("ping_pinger_http: Pinging"),
  Data = Pinger#pinger.data,
  Status = case lists:keyfind("status", 1, Data) of
    false -> "200";
    {_, S} -> S
  end,

  Method = case lists:keyfind("method", 1, Data) of
    false -> get;
    {_, M} -> M
  end,

  Response = ibrowse:send_req(Pinger#pinger.end_point, [], Method),
  lager:info("Matching with status: ~p\n", [Status]),
  lager:info("ping_pinger_http :: RESPONSE :: ~p\n", [Response]),
  case Response of
    {ok, Status, _, _} -> up;
    _ -> down
  end.
