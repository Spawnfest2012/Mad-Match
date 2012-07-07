-module(ping_web).


%%
%% Exported Functions
%%
-export([start_link/2]).

%%
%% API Functions
%%
-spec start_link(string(),pos_integer()) -> {ok,pid()}.
start_link(Host,Port) ->
  {ok,self()}.

%%
%% Local Functions
%%

