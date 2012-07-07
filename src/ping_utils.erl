%% @author Marcos Almonacid 
-module(ping_utils).

-export([rfc2882/0, rfc2882/1, rfc3339/1, iso8601/0, iso8601/1, dateadd/2,
         make_pairs/1, safe_term_to_binary/1, safe_list_to_float/1, binary_to_integer/1, to_lower/1,
         now/0, get_all_env/0, get_env/1, set_env/2, stop_timer/1,
         now_to_gregorian_seconds/1, random_string/1,seed/0]).

-export([pad_to16/1]).
-export([first/3]).
-export([positive/1]).
-export([record_to_proplist/1]). 

-type datetime() :: {{pos_integer(), 1..12, 1..31}, {0..23, 0..59, 0..59}}.
-export_type([datetime/0]).

-include("include/records.hrl").

record_to_proplist(#pinger{} = Rec) ->
  lists:zip(record_info(fields, pinger), tl(tuple_to_list(Rec)));
record_to_proplist(#subscription{} = Rec) ->
  lists:zip(record_info(fields, subscription), tl(tuple_to_list(Rec)));
record_to_proplist(#event{} = Rec) ->
  lists:zip(record_info(fields, event), tl(tuple_to_list(Rec)));
record_to_proplist(#user{} = Rec) ->
  lists:zip(record_info(fields, user), tl(tuple_to_list(Rec))).

-spec binary_to_integer(binary()) -> integer().
binary_to_integer(B) -> 
  list_to_integer(binary_to_list(B)).

-spec safe_list_to_float(string()) -> float().
safe_list_to_float(String) ->
  try erlang:list_to_float(String)
  catch _:badarg -> erlang:list_to_integer(String) * 1.0
  end.

-spec safe_term_to_binary(float() | integer() | tuple() | atom() | binary() | iolist()) -> binary().
safe_term_to_binary(F) when is_float(F) ->
  iolist_to_binary(io_lib:format("~f", [F]));

safe_term_to_binary(I) when is_integer(I) ->
  list_to_binary(integer_to_list(I));

safe_term_to_binary(L) when is_tuple(L) -> 
  list_to_binary([]);

safe_term_to_binary(L) when is_list(L) ->
  unicode:characters_to_binary(L);

safe_term_to_binary(undefined) -> 
  <<>>;

safe_term_to_binary(A) when is_atom(A) -> 
  list_to_binary(atom_to_list(A));

safe_term_to_binary(A) when is_binary(A) -> A.

-spec iso8601() -> string().
iso8601() ->
  [EEEandComma, DD, MMM, YYYY, Time, "GMT"] = string:tokens(httpd_util:rfc1123_date(), " "),
  EEE = lists:reverse(erlang:tl(lists:reverse(EEEandComma))),
  string:join([EEE, MMM, DD, Time, "+0000", YYYY], " ").  

-spec iso8601(datetime()) -> string().
iso8601(T) ->
  [EEEandComma, DD, MMM, YYYY, Time, "GMT"] = string:tokens(httpd_util:rfc1123_date(T), " "),
  EEE = lists:reverse(erlang:tl(lists:reverse(EEEandComma))),
  string:join([EEE, MMM, DD, Time, "+0000", YYYY], " ").  

-spec rfc2882() -> string().
rfc2882() ->
  re:replace(httpd_util:rfc1123_date(),"GMT","+0000",[{return,list}]).

-spec rfc2882(datetime()) -> string().
rfc2882(T) ->
  re:replace(httpd_util:rfc1123_date(T),"GMT","+0000",[{return,list}]).

-spec rfc3339(datetime()) -> string().
rfc3339({{Year, Month, Day}, {Hour, Min, Sec}}) ->
  lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w+0000",
                              [Year,Month,Day, Hour, Min, Sec])).  

-spec make_pairs([X]) -> [{X, X}].
make_pairs(List) -> 
  make_pairs(List,[]).

make_pairs([],Pairs) -> 
  lists:reverse(Pairs);

make_pairs([_Odd],Pairs) -> 
  lists:reverse(Pairs);

make_pairs([L1,L2|Rem],Pairs) -> 
  make_pairs(Rem,[{L1,L2}] ++ Pairs).

-spec pad_to16(binary()) -> binary().
pad_to16(Bin) ->
	Padding_bits = (16 - (size(Bin) rem 16)) * 8,
	<<Bin/binary,0:Padding_bits>>.

-spec now() -> integer().
now() ->
  {_, _, MicroSecs} = erlang:now(),
  Millis = erlang:trunc(MicroSecs/1000),
  calendar:datetime_to_gregorian_seconds(
    calendar:universal_time()) * 1000 + Millis.

-spec now_to_gregorian_seconds(integer()) -> integer().
now_to_gregorian_seconds(Now) ->
	round(Now/1000).
	
-spec dateadd(datetime(), integer()) -> datetime().
dateadd(Date, Seconds) ->
  calendar:gregorian_seconds_to_datetime(
    calendar:datetime_to_gregorian_seconds(Date) + Seconds).

-spec get_all_env() -> [{atom(), term()}].
get_all_env() ->
  application:get_all_env(pingterst).

-spec get_env(atom()) -> term().
get_env(Field) ->
  case application:get_env(pingterest, Field) of
    {ok, Value} ->
      lager:debug("~p := ~p~n", [Field, Value]),
      Value;
    _ ->
      Value = get_env_default(Field),
      lager:debug("~p := ~p~n", [Field, Value]),
      Value
  end.

-spec get_env_default(atom()) -> term().
get_env_default(Field) ->
  case Field of
      _ -> throw({env_undefined, Field})
  end.

-spec set_env(atom(), term()) -> ok.
set_env(Field, Value) ->
  application:set_env(pingterst, Field, Value).

-spec stop_timer(undefined | reference() | timer:tref()) -> ok.
stop_timer(undefined) -> ok;
stop_timer(Ref) when is_reference(Ref) ->
  case erlang:cancel_timer(Ref) of
    false -> ok;
    _Time -> ok
  end;
stop_timer(Timer) ->
  case timer:cancel(Timer) of
    {ok, cancel} -> ok;
    {error, Reason} ->
      lager:warning("Couldn't stop timer ~p: ~p~n", [Timer, Reason]),
      ok
  end.

-spec first(X, X, [X]) -> none | X.
first(_X, _Y, []) -> none;
first(X, _Y, [X|_]) -> X;
first(_X, Y, [Y|_]) -> Y;
first(X, Y, [_Z|Rest]) -> first(X, Y, Rest).

-spec positive(integer()) -> non_neg_integer().
positive(X) when X < 0 -> 0;
positive(X) -> X.

-spec to_lower(binary()) -> binary().
to_lower(Bin) ->
  to_lower(Bin, <<>>).

to_lower(<<>>, Acc) ->
  Acc;
to_lower(<<C, Rest/binary>>, Acc) when $A =< C, C =< $Z ->
  to_lower(Rest, <<Acc/binary, (C+32)>>);
to_lower(<<195, C, Rest/binary>>, Acc) when 128 =< C, C =< 150 -> %% A-0 with tildes plus enye
  to_lower(Rest, <<Acc/binary, 195, (C+32)>>);
to_lower(<<195, C, Rest/binary>>, Acc) when 152 =< C, C =< 158 -> %% U and Y with tilde plus greeks
  to_lower(Rest, <<Acc/binary, 195, (C+32)>>);
to_lower(<<C, Rest/binary>>, Acc) ->
  to_lower(Rest, <<Acc/binary, C>>).



-spec random_string(pos_integer()) -> [char()].
random_string(Len) ->
  Chrs = list_to_tuple("abcdefghijklmnopqrstuvwxyz0123456789"),
  ChrsSize = size(Chrs),
  F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
  lists:foldl(F, "", lists:seq(1, Len)).


-spec seed() -> ok.
seed() ->
  
  {ok,Migration} = file:read_file("src/migration.sql"),
  lager:info("Migration ~p",[Migration]),
  

  emysql:execute(ping_db,Migration),


  {ok,Uid} = ping_user_db:create("Master Pinger","alerts@pingtere.sh","nopingforyou",""),

  Apps = [{"Prod1","ping",Uid,"prod1.whisper.sh",60000},{"Prod3","ping",Uid,"prod3.whisper.sh",60000},{"Prod4","ping",Uid,"prod4.whisper.sh",60000},{"Prod5","ping",Uid,"prod5.whisper.sh",60000}],
  lists:foreach(fun({Name,Type,UserId,EndPoint,Frequency})-> ping_pinger_db:create(Name,Type,UserId,EndPoint,Frequency) end, Apps),
  ok.

