%%% -------------------------------------------------------------------
%%% Author  : C B DePue III
%%% -------------------------------------------------------------------
-module(ping_index_controller).
-behaviour(ping_controller).
-extends(ping_controller).

-export([render/2]).

render(Req,Session) ->
  %% use extends
  Pingers = lists:map(fun(P) -> Pl = ping_utils:record_to_proplist(P), 
        T = proplists:get_value(last_check,Pl),
        Status = binary_to_atom(proplists:get_value(last_status,Pl),utf8),
        PropList = case {Status,T} of
          {_,T} when T == 0 -> Pl ++ [{time_in_words, "Unknown status"}];
          {down,T} -> Pl ++ [{time_in_words,"Down for " ++ ping_utils:time_diff_now(T)}];
          {up,T} -> Pl ++ [{time_in_words,"Responding for " ++ ping_utils:time_diff_now(T)}]
        end,
        {ok,Html} = pinger_dtl:render(PropList), Html 
                      end, 
                      ping_pinger_db:firehose(1,20)),
  NewSession = Session ++ [{pingers,Pingers}],
  index_dtl:render(NewSession).

