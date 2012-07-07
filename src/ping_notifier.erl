-module(ping_notifier).

-export([start_link/0]).

-spec start_link() -> {ok,pid()}.
start_link() ->
  gen_event:start_link(?MODULE).