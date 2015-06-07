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

-module(jmeta_namespace_tests).
-include_lib("eunit/include/eunit.hrl").

-include("jtils.hrl").

-define(N, 'jmeta.namespace.test'). % test namespace
-define(TN(X), {?N, X}).

api() ->
  Exists = fun(Namespace) -> whereis(?NAMESPACE(Namespace)) =/= undefined end,
  TestTypes =
    [case Meta of
       {type, Name, Data} -> {type, ?TN(Name), Data};
       {frame, Name, Data} -> {frame, ?TN(Name), Data}
     end || Meta <- lists:sublist(jmeta_library:std(), 3)],
  First = lists:nth(1, TestTypes),
  Second = lists:nth(2, TestTypes),
  Third = lists:nth(3, TestTypes),
  false = Exists(?N),
  1 = jmeta_namespace:add(First), true = Exists(?N),
  2 = jmeta_namespace:add(Second), true = Exists(?N),
  2 = jmeta_namespace:add(Second), true = Exists(?N), % the add method replaces old meta
  3 = jmeta_namespace:add(Third), true = Exists(?N),
  F = jmeta_declaration:parse(First),
  S = jmeta_declaration:parse(Second),
  T = jmeta_declaration:parse(Third),
  FK = jmeta_declaration:key(F),
  SK = jmeta_declaration:key(S),
  TK = jmeta_declaration:key(T),
  F = jmeta_namespace:get(FK),
  S = jmeta_namespace:get(SK),
  T = jmeta_namespace:get(TK),
  2 = jmeta_namespace:delete(FK), true = Exists(?N),
  1 = jmeta_namespace:delete(SK), true = Exists(?N),
  1 = jmeta_namespace:delete(SK), true = Exists(?N), % delete by non existing key just ignored
  0 = jmeta_namespace:delete(TK), false = Exists(?N).

behaviour() ->
  {error, {wrong_namespace, 'some.strange.namespace'}} =
    jmeta_namespace:get({'some.strange.namespace', 'some.undefined.meta'}),
  {error, {{std, 'some.undefined.meta'}, is_not_defined}} = jmeta_namespace:get({std, 'some.undefined.meta'}).

main_test_() ->
  {setup,
    fun jmeta:start/0,
    fun(_) -> jmeta:stop() end,
    [?_test(api()),
      ?_test(behaviour())]}.
