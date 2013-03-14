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
    test_iso8601(),
    test_timestamp(),
    test_timestamp_range(),
    test_frame(),
    test_new_frame(),
    test_empty_frame(),
    test_base(),
    jmeta_test:done().

std() ->
    [%----------------------TYPES---------------------
     atom(),
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
     iso8601(),
     timestamp(),
     timestamp_range(),
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
     [{guards, [fun is_atom/1]},
      {default, 0}]}.

test_atom() ->
    each(atom, [ok, '', 'hey.hey', 'Hello My Friend', '+', a.b.c]),
    each_is_not(atom, [<<>>, ""]).

% numeric

numeric() ->
    {type, numeric,
     [{guards, [fun is_number/1]},
      {default, 0}]}.

test_numeric() ->
    each(numeric, [0, 1.0, -99999999999999999999.0, 20, 2.3e-40, 32#JMETA]),
    each_is_not(numeric, [<<>>, {}, [], <<0>>]).

integer() ->
    {type, integer,
     [{guards, [fun is_integer/1]},
      {default, 0}]}.

test_integer() ->
    each(integer, [0, 1, -20, 100000, 32#ELEPHANT, 999999999999999999999999]),
    each_is_not(integer, [0.0, 1.0]).

bit() ->
    {type, bit,
     [{guards, [fun(X) when is_integer(X) -> X >= 0 andalso X =< 1 end]},
      {default, 0}]}.

test_bit() ->
    each(bit, [1, 0, 2#1, 2#0]),
    each_is_not(bit, [1.0, 0.0, 10, -0.2]).

float() ->
    {type, float,
     [{guards, [fun is_float/1]},
      {default, 0.0}]}.

test_float() ->
    each(float, [0.0, 1.0, -2.3e-40]),
    each_is_not(float, [0, 1, 32#FLOAT]).

% boolean

boolean() ->
    {type, boolean,
     [{guards, [fun is_boolean/1]},
      {default, false}]}.

test_boolean() ->
    each(boolean, [true, false, 'true', 'false']),
    each_is_not(boolean, [1, 0, "true", "false"]).

% list

list() ->
    {type, list,
     [{guards, [fun is_list/1]},
      {default, []}]}.

test_list() ->
    each(list, [[], "", [1, 2, 3], "ABC", [[], [], []]]),
    each_is_not(list, [{}, <<>>]).

non_empty_list() ->
    {type, non_empty_list,
     [{guards, [fun(X) when is_list(X) -> length(X) > 0 end]}]}.

test_non_empty_list() ->
    each(non_empty_list, ["ABC", [[], [], []], [1, 2, 3]]),
    each_is_not(non_empty_list, [[], ""]).

set_keys() ->
    {type, set_keys,
     [{guards, [fun(List) when is_list(List) -> lists:all(fun is_bitstring/1, List) end]},
      {default, []}]}.

test_set_keys() ->
    each(set_keys, [[], [<<>>], [<<1, 2, 3>>, <<"jmeta">>]]),
    each_is_not(set_keys, [[1, 2, 3], {1, 2, 3}]).

set_refs() ->
    {type, set_refs,
     [{guards, [fun(List) when is_list(List) -> lists:all(fun is_integer/1, List) end]},
      {default, []}]}.

test_set_refs() ->
    each(set_refs, [[], [1, 2, 3]]),
    each_is_not(set_refs, [{}, [a, b, c], [1, 2, 3.0]]).

list_of_frames() ->
    {type, list_of_frames,
     [{guards, [fun(List) when is_list(List) -> lists:all(fun jframe:is_frame/1, List) end]},
      {default, []}]}.

test_list_of_frames() ->
    A = [{id, 1}],
    B = [{a, 1}, {b, 2}],
    each(list_of_frames, [[], [A], [A, A, B, B]]),
    each_is_not(list_of_frames, [A, [[{a, 1}, {a, 2}]]]).

% tuple

tuple() ->
    {type, tuple,
     [{guards, [fun is_tuple/1]},
      {default, {}}]}.

test_tuple() ->
    each(tuple, [{}, {1, 2, 3}, {{a, 1}, {b, 2}}]),
    each_is_not(tuple, [[], <<>>]).

% string and binary

string() ->
    {type, string,
     [{guards, [fun is_bitstring/1]},
      {default, <<>>}]}.

test_string() ->
    each(string, [<<>>, <<"Hello">>, <<13, 10>>]),
    each_is_not(string, [[], "", "Hello", [$j, $m, $e, $t, $a]]).

string128() ->
    {type, string128,
     [{guards, [fun(X) when is_bitstring(X) -> size(X) =< 128 end]},
      {default, <<>>}]}.

test_string128() ->
    each(string128, [<<>>, binary:copy(<<$A>>, 128)]),
    each_is_not(string128, ["", binary:copy(<<$A>>, 129)]).

% its same as string
binary() ->
    {type, binary,
     [{guards, [fun is_binary/1]},
      {default, <<>>}]}.

test_binary() ->
    each(string, [<<>>, <<"Hello">>, <<13, 10>>]),
    each_is_not(string, [[], "", "Hello", [$j, $m, $e, $t, $a]]).

% datetime

% FIXME
iso8601() ->
    {type, iso8601,
     [{guards, [fun(<<_:64/bitstring, "T", _:48/bitstring>>) -> true;
                   (_) -> false
                end]},
      {default, <<"20000101T000000">>}]}.

test_iso8601() ->
    ok.

% FIXME
timestamp() ->
    {type, timestamp,
     [{guards, [fun({{Y, M, D}, {H, N, S}}) when is_float(S) ->
                        lists:all(fun is_integer/1, [Y, M, D, H, N]);
                   (_) -> false
                end]},
      {default, {{2000, 1, 1}, {0, 0, 0.0}}}]}.

test_timestamp() ->
    ok.

% FIXME
timestamp_range() ->
    {type, timestamp_range,
     [{guards, [fun([T1, T2]) ->
                        Check = fun(T) -> true = jmeta:is({timestamp, T}) end,
                        Check(T1), Check(T2),
                        T1 =< T2
                end]},
      {default, [{{2000, 1, 1}, {0, 0, 0.0}},
                 {{2001, 1, 1}, {0, 0, 0.0}}]}]}.

test_timestamp_range() ->
    ok.

% frame

frame() ->
    {type, frame,
     [{guards, [fun jframe:is_frame/1]},
      {default, []}]}.

test_frame() ->
    A = [{id, 1}],
    B = [{id, 2}, {a.b.c, <<>>}],
    C = [{<<"a">>, 1}],
    each(frame, [[], A, B]),
    each_is_not(frame, [{}, A ++ B, C]).

new_frame() ->
    {type, new_frame,
     [{mixins, [frame]},
      {guards, [fun jframe:is_new/1]},
      {default, []}]}.

test_new_frame() ->
    A = [],
    B = [{a, 1}, {b, 2}],
    C = [{id, 1}],
    each(new_frame, [A, B]),
    each_is_not(new_frame, [C, C ++ A, C ++ B]).

empty_frame() ->
    {type, empty_frame,
     [{mixins, [frame]},
      {guards, [fun jframe:is_empty/1]},
      {default, []}]}.

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
