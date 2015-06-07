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

-module(jframe_tests).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
  [] = jframe:new(),
  {error, wrong_frame} = jframe:new([{id, 1}, {id, 2}]),
  [{a, 1}, {b, 2}, {c, 3}] = jframe:new(jframe:new([a, b, c], [1, 2, 3])).

transform_test() ->
  F10 = [{id, 10}] = jframe:store({id, 10}, jframe:new()),
  10 = jframe:find(id, F10),
  {} = jframe:find([], F10),
  F11 = [{id, 12}] = jframe:store({id, 12}, F10),
  F12 = jframe:store([{name, <<"Kostya">>}, {age, 25}], F11),
  {12, 25} = jframe:find([id, age], F12),
  {<<"Kostya">>, undefined, m} = jframe:find([name, gender, {gender, m}], F12),
  F13 = jframe:store({gender, m}, F12),
  m = jframe:find(gender, F13),
  {F13} = jframe:take([], F13),
  {undefined, F13} = jframe:take(lastname, F13),
  {<<"Duman">>, F13} = jframe:take({lastname, <<"Duman">>}, F13),
  {<<"Kostya">>, m, F14} = jframe:take([name, gender], F13),
  {12, undefined, 25, undefined} = jframe:find([id, name, age, gender], F14),
  F14 = jframe:delete(gender, F14),
  [{id, 12}] = jframe:delete(age, F14),
  [] = jframe:delete([id, age], F14),
  35 = jframe:find(age, jframe:update({age, fun(Age) -> Age + 10 end}, F14)),
  Inc = fun(X) -> X + 1 end,
  {13, 26} = jframe:find([id, age], jframe:update([{id, Inc}, {age, Inc}], F14)),
  F14 = jframe:extend(F14, []),
  F15 = jframe:extend(F10, [F11, F12, F13, F14]),
  {12, <<"Kostya">>, 25, m} = jframe:find([id, name, age, gender], F15),
  SF15 = [{age, 25}, {gender, m}, {id, 12}, {name, <<"Kostya">>}] = jframe:keysort(F15),
  [SF15, SF15, SF15] = jframe:keysort([F15, F15, F15]),
  [] = jframe:valuesmap(Inc, []),
  F16 = jframe:keysort(jframe:delete([gender, name], F15)),
  F17 = [{age, 26}, {id, 13}] = jframe:valuesmap(Inc, F16),
  [F17, F17, F17] = jframe:valuesmap(Inc, [F16, F16, F16]).

base_test() ->
  F10 = jframe:new(),
  F11 = jframe:store({name, <<>>}, F10),
  F12 = jframe:keysort(jframe:store({id, 1}, F11)),
  [true, true, false] = lists:map(fun jframe:is_new/1, [F10, F11, F12]),
  [true, false, false] = lists:map(fun jframe:is_empty/1, [F10, F11, F12]),
  [id, name] = jframe:keys(F12),
  [1, <<>>] = jframe:values(F12),
  true = jframe:has(name, F12),
  true = jframe:has([id, name], F12),
  false = jframe:has(age, F12),
  false = jframe:has([id, name, age], F12),
  [true, true, false, false] = lists:map(fun jframe:is_frame/1, [jframe:new(), F12, F12 ++ F12, [1, 2, 3]]),
  false = jframe:is_key_identical(F10, F12),
  true = jframe:is_key_identical(F12, F12),
  false = jframe:is_key_identical(F12, jframe:store({a, 1}, F12)),
  true = jframe:is_key_identical(F12, jframe:new([{id, 13}, {name, <<"jmeta">>}])).

compare_and_sort_test() ->
  F10 = F50 = jframe:new(),
  false = jframe:compare(F10, F50, []),
  false = jframe:compare(F10, F50, [{a, '>'}, {b, '<'}, {c, [{d, '<'}]}, {e, fun(_, _) -> true end}]),
  F51 = jframe:store({a, 1}, F50),
  F11 = jframe:store({a, 2}, F10),
  true = jframe:compare(F11, F51, [{a, '>'}]),
  true = jframe:compare(F11, F51, [{a, fun(A, B) -> A > B end}]),
  F52 = jframe:store({b, 1}, F51),
  F12 = jframe:store({b, 1}, F11),
  false = jframe:compare(F12, F52, [{b, '<'}]),
  true = jframe:compare(F12, F52, [{b, '<'}, {a, '>'}]),
  false = jframe:compare(F12, jframe:store({b, 0}, F52), [{b, '<'}, {a, '>'}]),
  % warning, do not compare not key identical frames
  % the result will be based on the result of compare 'undefined' atoms
  true = jframe:compare(jframe:delete(a, F12), F52, [{b, '<'}, {a, '>'}]),
  F13 = F53 = jframe:new([{a, 1}]),
  F14 = jframe:store({b, [{c, 1}, {d, 1}]}, F13),
  F54 = jframe:store({b, [{c, 1}, {d, 2}]}, F53),
  true = jframe:compare(F14, F54, [{a, '<'}, {b, [{c, '<'}, {d, '<'}]}]),
  false = jframe:compare(F14, F54, [{a, '<'}, {b, [{c, '<'}, {d, '>'}]}]),
  FA = [{a, 3}, {b, 4}],
  FB = [{a, 3}, {b, 2}],
  FC = [{a, 1}, {b, 5}],
  FD = [{a, 2}, {b, 1}],
  FE = [{a, 4}, {b, 6}],
  [FE, FB, FA, FD, FC] = jframe:sort([{a, '>'}, {b, '<'}], [FA, FB, FC, FD, FE]).

diff_test() ->
  [] = jframe:diff([], []),
  [] = jframe:diff([{a, 1}], [{a, 1}]),
  [{a, non_equal}] = jframe:diff([{a, 1}], [{a, 2}]),
  [{b, [{e, removed}, {d, non_equal}]}, {a, removed}, {a1, added}] =
    jframe:diff([{a, 1}, {b, [{c, 1}, {d, 2}, {e, 3}]}], [{a1, 1}, {b, [{c, 1}, {d, 3}]}]),
  [] = jframe:diff([{a, 1}, {b, [{c, 1}, {d, 2}]}], [{a, 1}, {b, [{c, 1}, {d, 2}]}]).
