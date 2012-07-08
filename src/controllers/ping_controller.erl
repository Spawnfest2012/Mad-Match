-module(ping_controller).
-export([behaviour_info/1]).
-export([redirect/3]).
behaviour_info(callbacks) -> [{render, 2}];
behaviour_info(_) -> undefined.

redirect(Type,Location, Req) -> 
  cowboy_http_req:reply(
        Type,
        [{<<"Location">>, Location}],
        <<"Redirecting...">>,
        Req
    ).
  
