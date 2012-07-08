-module(ping_controller).
-export([behaviour_info/1]).
-export([redirect/3,get_parameters/2,get_parameter/2]).

behaviour_info(callbacks) -> [{render, 2}];
behaviour_info(_) -> undefined.

redirect(Type,Location, Req) -> 
  cowboy_http_req:reply(
        Type,
        [{<<"Location">>, Location}],
        <<"Redirecting...">>,
        Req
    ).

get_parameters(Qs, Params) ->
  lists:map(fun(P) -> get_parameter(P, Qs) end, Params).

get_parameter(Key, Qs) ->
  Value = proplists:get_value(Key, Qs),
  ping_utils:safe_binary_to_list(Value).

