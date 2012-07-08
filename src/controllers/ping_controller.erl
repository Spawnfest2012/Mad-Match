-module(ping_controller).
-export([behaviour_info/1]).
behaviour_info(callbacks) -> [{render, 2}];
behaviour_info(_) -> undefined.

