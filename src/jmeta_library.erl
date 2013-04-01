%% Author: Said
%% Created: 09.02.2013
%% Description: TODO: Add description to jmeta_library
-module(jmeta_library).

%%
%% Include files
%%

%%
%% Exported Functions
%%

% test
-export([test/0]).

-export([std/0]).

%%
%% API Functions
%%

test() ->
    [] = [R || {error, _} = R <- [jmeta_declaration:parse(T) || T <- std()]],
    test_atom(),
    test_null(),
    test_numeric(),
    test_integer(),
    test_bit(),
    test_float(),
    test_boolean(),
    test_list(),
    test_non_empty_list(),
    test_set_keys(),
    test_set_refs(),
    test_list_of_frames(),
    test_tuple(),
    test_string(),
    test_string128(),
    test_binary(),
    test_datetime(),
    test_frame(),
    test_new_frame(),
    test_empty_frame(),
    test_base(),
    jmeta_test:done().

std() ->
    [%----------------------TYPES---------------------
     atom(),
     null(),
     % numeric
     numeric(),
     integer(),
     bit(),
     float(),
     % boolean
     boolean(),
     % list
     list(),
     non_empty_list(),
     set_keys(),
     set_refs(),
     list_of_frames(),
     % tuple
     tuple(),
     % string and binary
     string(),
     string128(),
     binary(),
     % datetime
     date_(),
     timea(),
     timeb(),
     timestampa(),
     timestampb(),
     timestamp_rangea(),
     timestamp_rangeb(),
     % frame
     frame(),
     new_frame(),
     empty_frame(),
     %---------------------FRAMES---------------------
     base()].

%%
%% Local Functions
%%

each(Meta, List) ->
    lists:foreach(fun(X) -> true = jmeta:is({Meta, X}) end, List).

each_is_not(Meta, List) ->
    lists:foreach(fun(X) -> {error, _} = jmeta:is({Meta, X}) end, List).

%----------------------TYPES---------------------

atom() ->
    {type, atom,
     [{guards, [fun is_atom/1]}
     ]}.

test_atom() ->
    each(atom, [ok, '', 'hey.hey', 'Hello My Friend', '+', a.b.c]),
    each_is_not(atom, [<<>>, ""]).

null() ->
    {type, null,
     [{guards, [fun(null) -> true end]}
     ]}.

test_null() ->
    each(null, [null, 'null']),
    each_is_not(null, ["null", <<"null">>, undefined, nil]).

% numeric

numeric() ->
    {type, numeric,
     [{guards, [fun is_number/1]}
     ]}.

test_numeric() ->
    each(numeric, [0, 1.0, -99999999999999999999.0, 20, 2.3e-40, 32#JMETA]),
    each_is_not(numeric, [<<>>, {}, [], <<0>>]).

integer() ->
    {type, integer,
     [{guards, [fun is_integer/1]}
     ]}.

test_integer() ->
    each(integer, [0, 1, -20, 100000, 32#ELEPHANT, 999999999999999999999999]),
    each_is_not(integer, [0.0, 1.0]).

bit() ->
    {type, bit,
     [{guards, [fun(X) when is_integer(X) -> X >= 0 andalso X =< 1 end]}
     ]}.

test_bit() ->
    each(bit, [1, 0, 2#1, 2#0]),
    each_is_not(bit, [1.0, 0.0, 10, -0.2]).

float() ->
    {type, float,
     [{guards, [fun is_float/1]}
     ]}.

test_float() ->
    each(float, [0.0, 1.0, -2.3e-40]),
    each_is_not(float, [0, 1, 32#FLOAT]).

% boolean

boolean() ->
    {type, boolean,
     [{guards, [fun is_boolean/1]}
     ]}.

test_boolean() ->
    each(boolean, [true, false, 'true', 'false']),
    each_is_not(boolean, [1, 0, "true", "false"]).

% list

list() ->
    {type, list,
     [{guards, [fun is_list/1]}
     ]}.

test_list() ->
    each(list, [[], "", [1, 2, 3], "ABC", [[], [], []]]),
    each_is_not(list, [{}, <<>>]).

non_empty_list() ->
    {type, non_empty_list,
     [{guards, [fun(X) when is_list(X) -> length(X) > 0 end]}
     ]}.

test_non_empty_list() ->
    each(non_empty_list, ["ABC", [[], [], []], [1, 2, 3]]),
    each_is_not(non_empty_list, [[], ""]).

set_keys() ->
    {type, set_keys,
     [{guards, [fun(List) when is_list(List) -> lists:all(fun is_bitstring/1, List) end]}
     ]}.

test_set_keys() ->
    each(set_keys, [[], [<<>>], [<<1, 2, 3>>, <<"jmeta">>]]),
    each_is_not(set_keys, [[1, 2, 3], {1, 2, 3}]).

set_refs() ->
    {type, set_refs,
     [{guards, [fun(List) when is_list(List) -> lists:all(fun is_integer/1, List) end]}
     ]}.

test_set_refs() ->
    each(set_refs, [[], [1, 2, 3]]),
    each_is_not(set_refs, [{}, [a, b, c], [1, 2, 3.0]]).

list_of_frames() ->
    {type, list_of_frames,
     [{guards, [fun(List) when is_list(List) -> lists:all(fun jframe:is_frame/1, List) end]}
     ]}.

test_list_of_frames() ->
    A = [{id, 1}],
    B = [{a, 1}, {b, 2}],
    each(list_of_frames, [[], [A], [A, A, B, B]]),
    each_is_not(list_of_frames, [A, [[{a, 1}, {a, 2}]]]).

% tuple

tuple() ->
    {type, tuple,
     [{guards, [fun is_tuple/1]}
     ]}.

test_tuple() ->
    each(tuple, [{}, {1, 2, 3}, {{a, 1}, {b, 2}}]),
    each_is_not(tuple, [[], <<>>]).

% string and binary

string() ->
    {type, string,
     [{guards, [fun is_bitstring/1]}
     ]}.

test_string() ->
    each(string, [<<>>, <<"Hello">>, <<13, 10>>]),
    each_is_not(string, [[], "", "Hello", [$j, $m, $e, $t, $a]]).

string128() ->
    {type, string128,
     [{guards, [fun(X) when is_bitstring(X) -> size(X) =< 128 end]}
     ]}.

test_string128() ->
    each(string128, [<<>>, binary:copy(<<$A>>, 128)]),
    each_is_not(string128, ["", binary:copy(<<$A>>, 129)]).

% its same as string
binary() ->
    {type, binary,
     [{guards, [fun is_binary/1]}
     ]}.

test_binary() ->
    each(string, [<<>>, <<"Hello">>, <<13, 10>>]),
    each_is_not(string, [[], "", "Hello", [$j, $m, $e, $t, $a]]).

% datetime

date_() ->
    {type, date,
     [{guards, [fun calendar:valid_date/1]}
     ]}.

timea() ->
    {type, time.a,
     [{guards, [fun({H, M, S}) when is_integer(H) andalso is_integer(M) andalso is_integer(S) ->
                        H >= 0 andalso H < 24 andalso
                            M >= 0 andalso M < 60 andalso
                            S >= 0 andalso S < 60
                end]}
     ]}.

timeb() ->
    {type, time.b,
     [{guards, [fun({H, M, S}) when is_float(S) ->
                        true = jmeta:is({time.a, {H, M, trunc(S)}})
                end]}
     ]}.

timestamp_guard(TimeVersion) ->
    fun({Date, Time}) ->
            true = jmeta:is({date, Date}),
            true = jmeta:is({TimeVersion, Time})
    end.

timestampa() ->
    {type, timestamp.a,
     [{guards, [timestamp_guard(time.a)]}
     ]}.

timestampb() ->
    {type, timestamp.b,
     [{guards, [timestamp_guard(time.b)]}
     ]}.

timestamp_range_guard(TimestampVersion) ->
    fun([T1, T2] = Range) ->
            true = jmeta:list_of({TimestampVersion, Range}),
            T1 =< T2
    end.

timestamp_rangea() ->
    {type, timestamp_range.a,
     [{guards, [timestamp_range_guard(timestamp.a)]}
     ]}.

timestamp_rangeb() ->
    {type, timestamp_range.b,
     [{guards, [timestamp_range_guard(timestamp.b)]}
     ]}.

test_datetime() ->
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
    each(time.a, GoodTimeAList),
    BadTimeAList =
        [{0, 0, -1},
         {0, 0, 60},
         {0, -1, 0},
         {0, 60, 0},
         {-1, 0, 0},
         {24, 0, 0}],
    each_is_not(time.a, BadTimeAList),
    TimeAToTimeB = fun({H, M, S}) -> {H, M, float(S)} end,
    GoodTimeBList = lists:map(TimeAToTimeB, GoodTimeAList),
    each(time.b, GoodTimeBList),
    BadTimeBList = lists:map(TimeAToTimeB, BadTimeAList),
    each_is_not(time.b, BadTimeBList),
    GoodTimestampAList = [{D, T} || D <- GoodDateList, T <- GoodTimeAList],
    each(timestamp.a, [calendar:local_time()|GoodTimestampAList]),
    BadTimestampAList =
        [{D, T} || D <- GoodDateList, T <- BadTimeAList] ++
            [{D, T} || D <- BadDateList, T <- GoodTimeAList],
    each_is_not(timestamp.a, BadTimestampAList),
    GoodTimestampBList = [{D, T} || D <- GoodDateList, T <- GoodTimeBList],
    each(timestamp.b, GoodTimestampBList),
    BadTimestampBList =
        [{D, T} || D <- GoodDateList, T <- BadTimeBList] ++
            [{D, T} || D <- BadDateList, T <- GoodTimeBList],
    each_is_not(timestamp.b, BadTimestampBList),
    GoodTimeStampA1 = {date(), {0, 0, 0}},
    GoodTimeStampA2 = {date(), {1, 0, 0}},
    GoodTimestampRangeAList =
        [[GoodTimeStampA1, GoodTimeStampA2]] ++ [[T, T] || T <- GoodTimestampAList],
    each(timestamp_range.a, GoodTimestampRangeAList),
    BadTimestampRangeAList =
        [[GoodTimeStampA2, GoodTimeStampA1]] ++ [[T, T] || T <- BadTimestampAList],
    each_is_not(timestamp_range.a, BadTimestampRangeAList),
    GoodTimeStampB1 = {date(), {0, 0, 0.0}},
    GoodTimeStampB2 = {date(), {1, 0, 0.0}},
    GoodTimestampRangeBList =
        [[GoodTimeStampB1, GoodTimeStampB2]] ++ [[T, T] || T <- GoodTimestampBList],
    each(timestamp_range.b, GoodTimestampRangeBList),
    BadTimestampRangeBList =
        [[GoodTimeStampB2, GoodTimeStampB1]] ++ [[T, T] || T <- BadTimestampBList],
    each_is_not(timestamp_range.b, BadTimestampRangeBList).

% frame

frame() ->
    {type, frame,
     [{guards, [fun jframe:is_frame/1]}
     ]}.

test_frame() ->
    A = [{id, 1}],
    B = [{id, 2}, {a.b.c, <<>>}],
    C = [{<<"a">>, 1}],
    each(frame, [[], A, B]),
    each_is_not(frame, [{}, A ++ B, C]).

new_frame() ->
    {type, new_frame,
     [{mixins, [frame]},
      {guards, [fun jframe:is_new/1]}
     ]}.

test_new_frame() ->
    A = [],
    B = [{a, 1}, {b, 2}],
    C = [{id, 1}],
    each(new_frame, [A, B]),
    each_is_not(new_frame, [C, C ++ A, C ++ B]).

empty_frame() ->
    {type, empty_frame,
     [{mixins, [frame]},
      {guards, [fun jframe:is_empty/1]}
     ]}.

test_empty_frame() ->
    each(empty_frame, [[]]),
    each_is_not(empty_frame, [{}, [{id, 1}], [{a, 1}, {b, 2}]]).

%---------------------FRAMES---------------------

base() ->
    {frame, base,
      [{fields,
        [{id, [{is, integer}, {optional, true}]}
        ]}
      ]}.

test_base() ->
    each(base, [[], [{id, 1}]]),
    each_is_not(base, [{}, [{a, 1}]]).
