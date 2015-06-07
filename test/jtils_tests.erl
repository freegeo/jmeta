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

-module(jtils_tests).
-include_lib("eunit/include/eunit.hrl").

-include("jtils.hrl").

list_test() ->
  A = [],
  B = [1, 2, 3],
  C = [3, 2, 1],
  D = [3, 2, 1, 1, 2, 3],
  E = [[1, 2], [2, 1]],
  [A, B, C, C, E] = lists:map(fun jtils:ulist/1, [A, B, C, D, E]),
  [true, true, true, false, true] = lists:map(fun jtils:is_list_elements_unique/1, [A, B, C, D, E]).

misc_test() ->
  ?assertException(error, function_clause, jtils:random_list(-1, "ABC")),
  ?assertException(error, function_clause, jtils:random_list(10, "")),
  "" = jtils:random_list(0, ""),
  "" = jtils:random_list(0, ?ALLOWED_CHARS),
  "AAAAAAAAAA" = jtils:random_list(10, "A"),
  [[], [], []] = jtils:random_list(3, [[]]),
  "" = jtils:random_token(0),
  10 = length(jtils:random_token(10)),
  T1 = jtils:jtoken(),
  18 = length(T1),
  T2 = jtils:shuffle(T1),
  18 = length(T2),
  true = T1 =/= T2, % not quite safe, but the possible problem might occur very rarely
  <<"616263646566">> = jtils:hex(<<"abcdef">>),
  <<"414243f3010d096f41">> = jtils:hex(<<"ABC", 243, 1, 13, 9, 111, $A>>),
  0 = length([X || X <- [jtils:uid() || _ <- lists:seq(1, 25000)], byte_size(X) =/= 36]).
