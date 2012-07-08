-module(ping_web_handler_ws).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, notify/2]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, []) ->
    lager:info("INIT"),
    case cowboy_http_req:header('Upgrade', Req) of
        {undefined, Req2} -> {ok, Req2, undefined};
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

notify(Id, Status) ->
    lists:foreach(fun(Pid) -> Pid ! {notify, Id, Status} end, pg2:get_members("ws")).

websocket_init(_Any, Req, []) ->
%    timer:send_interval(1000, tick),
    pg2:join("ws", self()),
    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, self(), hibernate}.

websocket_handle(_Any, Req, State) ->
    lager:info("_ANY"),
    {ok, Req, State}.

websocket_info({notify, Id, Status}, Req, State) ->
    lager:info("{Notify}"),
    Response = jsx:encode([{id, Id}, {status, Status}]),
    {reply, {text, Response}, Req, State, hibernate};

websocket_info(_Info, Req, State) ->
    lager:info("_INFO"),
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
