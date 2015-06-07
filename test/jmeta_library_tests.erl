%%% coding: UTF-8
%%%-------------------------------------------------------------------
%%% @author Konstantin Duman <said.dk@gmail.com>
%%% @copyright (C) 2015, FreeGeo
%%% Published under MIT license.
%%%
%%% @doc
%%% Contains unit tests.
%%% @end
%%%-------------------------------------------------------------------

-module(jmeta_library_tests).
-include_lib("eunit/include/eunit.hrl").

each(Meta, List) -> lists:foreach(fun(X) -> true = jmeta:is({Meta, X}) end, List).
each_is_not(Meta, List) -> lists:foreach(fun(X) -> {error, _} = jmeta:is({Meta, X}) end, List).

std() -> [] = [R || {error, _} = R <- [jmeta_declaration:parse(T) || T <- jmeta_library:std()]].

atom() ->
  each(atom, [ok, '', 'hey.hey', 'Hello My Friend', '+', 'a.b.c']),
  each_is_not(atom, [<<>>, ""]).

null() ->
  each(null, [null, 'null']),
  each_is_not(null, ["null", <<"null">>, undefined, nil]).

numeric() ->
  each(numeric, [0, 1.0, -99999999999999999999.0, 20, 2.3e-40, 32#JMETA]),
  each_is_not(numeric, [<<>>, {}, [], <<0>>]).

integer() ->
  each(integer, [0, 1, -20, 100000, 32#ELEPHANT, 999999999999999999999999]),
  each_is_not(integer, [0.0, 1.0]).

bit() ->
  each(bit, [1, 0, 2#1, 2#0]),
  each_is_not(bit, [1.0, 0.0, 10, -0.2]).

float() ->
  each(float, [0.0, 1.0, -2.3e-40]),
  each_is_not(float, [0, 1, 32#FLOAT]).

boolean() ->
  each(boolean, [true, false, 'true', 'false']),
  each_is_not(boolean, [1, 0, "true", "false"]).

list() ->
  each(list, [[], "", [1, 2, 3], "ABC", [[], [], []]]),
  each_is_not(list, [{}, <<>>]).

non_empty_list() ->
  each(non_empty_list, ["ABC", [[], [], []], [1, 2, 3]]),
  each_is_not(non_empty_list, [[], ""]).

set_keys() ->
  each(set_keys, [[], [<<>>], [<<1, 2, 3>>, <<"jmeta">>]]),
  each_is_not(set_keys, [[1, 2, 3], {1, 2, 3}]).

set_refs() ->
  each(set_refs, [[], [1, 2, 3]]),
  each_is_not(set_refs, [{}, [a, b, c], [1, 2, 3.0]]).

list_of_frames() ->
  A = [{id, 1}],
  B = [{a, 1}, {b, 2}],
  each(list_of_frames, [[], [A], [A, A, B, B]]),
  each_is_not(list_of_frames, [A, [[{a, 1}, {a, 2}]]]).

tuple() ->
  each(tuple, [{}, {1, 2, 3}, {{a, 1}, {b, 2}}]),
  each_is_not(tuple, [[], <<>>]).

string() ->
  each(string, [<<>>, <<"Hello">>, <<13, 10>>]),
  each_is_not(string, [[], "", "Hello", [$j, $m, $e, $t, $a]]).

string128() ->
  each(string128, [<<>>, binary:copy(<<$A>>, 128)]),
  each_is_not(string128, ["", binary:copy(<<$A>>, 129)]).

binary() ->
  each(string, [<<>>, <<"Hello">>, <<13, 10>>]),
  each_is_not(string, [[], "", "Hello", [$j, $m, $e, $t, $a]]).

datetime() ->
  GoodDateList =
    [date(),
      {0, 1, 1},
      {2013, 12, 31},
      {2013, 2, 28}],
  each(date, GoodDateList),
  BadDateList =
    [{2013, 2, 29},
      {2013, 2, 0},
      {2013, 0, 1},
      {2013, 13, 1},
      {-1, 1, 1}],
  each_is_not(date, BadDateList),
  GoodTimeAList =
    [time(),
      {0, 0, 0},
      {23, 59, 59},
      {9, 48, 21}],
  each('time.a', GoodTimeAList),
  BadTimeAList =
    [{0, 0, -1},
      {0, 0, 60},
      {0, -1, 0},
      {0, 60, 0},
      {-1, 0, 0},
      {24, 0, 0}],
  each_is_not('time.a', BadTimeAList),
  TimeAToTimeB = fun({H, M, S}) -> {H, M, float(S)} end,
  GoodTimeBList = lists:map(TimeAToTimeB, GoodTimeAList),
  each('time.b', GoodTimeBList),
  BadTimeBList = lists:map(TimeAToTimeB, BadTimeAList),
  each_is_not('time.b', BadTimeBList),
  GoodTimestampAList = [{D, T} || D <- GoodDateList, T <- GoodTimeAList],
  each('timestamp.a', [calendar:local_time() | GoodTimestampAList]),
  BadTimestampAList =
    [{D, T} || D <- GoodDateList, T <- BadTimeAList] ++
    [{D, T} || D <- BadDateList, T <- GoodTimeAList],
  each_is_not('timestamp.a', BadTimestampAList),
  GoodTimestampBList = [{D, T} || D <- GoodDateList, T <- GoodTimeBList],
  each('timestamp.b', GoodTimestampBList),
  BadTimestampBList =
    [{D, T} || D <- GoodDateList, T <- BadTimeBList] ++
    [{D, T} || D <- BadDateList, T <- GoodTimeBList],
  each_is_not('timestamp.b', BadTimestampBList),
  GoodTimeStampA1 = {date(), {0, 0, 0}},
  GoodTimeStampA2 = {date(), {1, 0, 0}},
  GoodTimestampRangeAList =
    [[GoodTimeStampA1, GoodTimeStampA2]] ++ [[T, T] || T <- GoodTimestampAList],
  each('timestamp_range.a', GoodTimestampRangeAList),
  BadTimestampRangeAList =
    [[GoodTimeStampA2, GoodTimeStampA1]] ++ [[T, T] || T <- BadTimestampAList],
  each_is_not('timestamp_range.a', BadTimestampRangeAList),
  GoodTimeStampB1 = {date(), {0, 0, 0.0}},
  GoodTimeStampB2 = {date(), {1, 0, 0.0}},
  GoodTimestampRangeBList =
    [[GoodTimeStampB1, GoodTimeStampB2]] ++ [[T, T] || T <- GoodTimestampBList],
  each('timestamp_range.b', GoodTimestampRangeBList),
  BadTimestampRangeBList =
    [[GoodTimeStampB2, GoodTimeStampB1]] ++ [[T, T] || T <- BadTimestampBList],
  each_is_not('timestamp_range.b', BadTimestampRangeBList).

frame() ->
  A = [{id, 1}],
  B = [{id, 2}, {'a.b.c', <<>>}],
  C = [{<<"a">>, 1}],
  each(frame, [[], A, B]),
  each_is_not(frame, [{}, A ++ B, C]).

new_frame() ->
  A = [],
  B = [{a, 1}, {b, 2}],
  C = [{id, 1}],
  each(new_frame, [A, B]),
  each_is_not(new_frame, [C, C ++ A, C ++ B]).

empty_frame() ->
  each(empty_frame, [[]]),
  each_is_not(empty_frame, [{}, [{id, 1}], [{a, 1}, {b, 2}]]).

base() ->
  each(base, [[], [{id, 1}]]),
  each_is_not(base, [{}, [{a, 1}]]).

main_test_() ->
  {setup,
    fun jmeta:start/0,
    fun(_) -> jmeta:stop() end,
    [?_test(std()),
      ?_test(atom()),
      ?_test(null()),
      ?_test(numeric()),
      ?_test(integer()),
      ?_test(bit()),
      ?_test(float()),
      ?_test(boolean()),
      ?_test(list()),
      ?_test(non_empty_list()),
      ?_test(set_keys()),
      ?_test(set_refs()),
      ?_test(list_of_frames()),
      ?_test(tuple()),
      ?_test(string()),
      ?_test(string128()),
      ?_test(binary()),
      ?_test(datetime()),
      ?_test(frame()),
      ?_test(new_frame()),
      ?_test(empty_frame()),
      ?_test(base())]}.
