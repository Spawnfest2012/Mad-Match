-module(ping_notifier).

-export([start_link/0,notify/1]).

-spec start_link() -> {ok,pid()}.
start_link() ->
  lager:info("Initializing Notifier", []),
  Resp = gen_event:start_link({local,?MODULE}),
  lists:foreach(fun(Notifier) ->
                  gen_event:add_handler(?MODULE, Notifier, [])
                end, ping_utils:get_env(notifiers)),
  Resp.

-spec notify(term()) -> ok.
notify(Event) ->
  gen_event:notify(?MODULE, Event).