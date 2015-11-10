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

-module(jmeta_tests).
-include_lib("eunit/include/eunit.hrl").

-define(N(X), {'freegeo.jmeta.test', X}).
-define(N(X, Params), {'freegeo.jmeta.test', X, Params}).
-define(WMS(X), {'freegeo.jmeta.wms', X}).
-define(WMS(X, Params), {'freegeo.jmeta.wms', X, Params}).

integration() ->
  % types and mixins
  DateISO8601a = <<"20130228">>, % YYYYMMDD
  DateISO8601b = <<"2013-02-28">>, % YYYY-MM-DD
  ISO8601a =
    {type, ?N('iso8601.a'),
      [{guards, [fun(<<Y:(4 * 8)/bitstring, M:(2 * 8)/bitstring, D:(2 * 8)/bitstring>>) ->
        Date = list_to_tuple([list_to_integer(bitstring_to_list(X)) || X <- [Y, M, D]]),
        jmeta:is({date, Date})
      end]}
      ]},
  ISO8601b =
    {type, ?N('iso8601.b'),
      [{guards, [fun(<<Y:(4 * 8)/bitstring, "-", M:(2 * 8)/bitstring, "-", D:(2 * 8)/bitstring>>) ->
        jmeta:is({?N('iso8601.a'), <<Y/bitstring, M/bitstring, D/bitstring>>})
      end]}
      ]},
  ISO8601 =
    {type, ?N(iso8601),
      [{mixins, [?N('iso8601.a'), ?N('iso8601.b')]},
        {mode, [{mixins, any}]}
      ]},
  lists:foreach(fun jmeta:add/1, [ISO8601a, ISO8601b, ISO8601]),
  true = jmeta:is({?N('iso8601.a'), DateISO8601a}),
  {error, {not_a, ?N('iso8601.a', [])}} = jmeta:is({?N('iso8601.a'), DateISO8601b}),
  false = true =:= jmeta:is({?N('iso8601.a'), <<"20130229">>}), % right format, wrong date
  true = jmeta:is({?N('iso8601.b'), DateISO8601b}),
  false = true =:= jmeta:is({?N('iso8601.b'), DateISO8601a}),
  % iso8601 is a variant type and accept a and b format
  true = jmeta:is({?N(iso8601), DateISO8601a}),
  true = jmeta:is({?N(iso8601), DateISO8601b}),
  % more mixins
  ForeignKey =
    {type, ?N(foreign_key),
      [{mixins, [integer, null]},
        {mode, [{mixins, any}]}
      ]},
  jmeta:add(ForeignKey),
  true = jmeta:is({?N(foreign_key), null}),
  true = jmeta:is({?N(foreign_key), 1}),
  false = true =:= jmeta:is({?N(foreign_key), "null"}),
  % types and variative guards
  jmeta:add({type, ?N(str),
    [{guards, [fun is_list/1, fun is_bitstring/1]},
      {mode, [{guards, any}]}
    ]}),
  true = jmeta:is({?N(str), "jmeta"}),
  true = jmeta:is({?N(str), <<"jmeta">>}),
  false = true =:= jmeta:is({?N(str), 32#JMETA}),
  % list of
  TestList = [2#01, <<"2">>, 3, "4", [5], 6, {7}],
  ListOfResult =
    [[{error, {not_a, {std, integer, []}}}, {pos, 2}],
      [_, {pos, 4}],
      [_, {pos, 5}],
      [_, {pos, 7}]] = jmeta:list_of({integer, TestList}),
  [<<"2">>, "4", [5], {7}] = [lists:nth(jframe:find(pos, X), TestList) || X <- ListOfResult],
  % nested list of
  true = jmeta:list_of({{list_of, {list_of, integer}}, []}),
  true = jmeta:list_of({{list_of, {list_of, integer}}, [[], [], []]}),
  true = jmeta:list_of({{list_of, {list_of, integer}}, [[[1, 2], [3, 4]], [[]], []]}),
  [[[[{error, {not_a, {std, integer, []}}}, {pos, 3}]], {pos, 1}],
    [[[_,{pos, 2}],
      [_,{pos, 3}],
      [_,{pos, 4}]], {pos, 2}],
    [{error, data_is_not_a_list}, {pos, 3}]] = jmeta:list_of({{list_of, integer}, [[1, 2, []], [1, a, b, c], abc]}),
  [1 ,2, 4, 5] = jmeta:pick({integer, [1, 2, b, integer, null, 4, 5, a, "6", <<"7">>]}),
  [1, 2, null, 4, 5] = jmeta:pick({?N(foreign_key), [1, 2, b, integer, null, 4, 5, a, "6", <<"7">>]}),
  % frames
  true = jmeta:is({base, []}),
  true = jmeta:is({base, [{id, 1}]}),
  {error, [{not_a, {std, base, []}}, {extra_keys, [name]}]} = jmeta:is({base, [{name, <<>>}]}),
  Arrival =
    {frame, ?WMS(arrival),
      [{extend, [base]},
        {fields,
          [{datetime, [{is, 'timestamp.b'}, {optional, true}]},
            {nr, [{is, string128}]},
            {warehouse_id, [{is, integer}]},
            {status, [{is, integer}, {guards, [fun(X) -> lists:member(X, [1, 2, 3]) end]}]},
            {items, [{list_of, ?WMS(arrival_item)}]}
          ]}
      ]},
  ArrivalItem =
    {frame, ?WMS(arrival_item),
      [{extend, [base]},
        {fields,
          [{arrival_id, [{is, integer}, {optional, true}]},
            {product_id, [{is, integer}, {optional, true}]},
            {expire, [{is, 'timestamp.b'}]},
            {count, [{is, integer}]}
          ]}
      ]},
  lists:foreach(fun jmeta:add/1, [Arrival, ArrivalItem]),
  A1 = jframe:new(),
  {error, [{not_a, ?WMS(arrival, [])},
    {violated, [
      {nr, missed},
      {warehouse_id, missed},
      {status, missed},
      {items, missed}
    ]}]} = jmeta:is({?WMS(arrival), A1}),
  A2 = jframe:store([{nr, <<"123">>},
    {warehouse_id, 1},
    {status, 5},
    {extra1, some_data}, % complex extra keys test
    {extra2, some_data},
    {items, []}], A1),
  {error, [_,
    {violated, [{status, {{is, {std, integer, []}}, but_breaking_a_guard}}]},
    {extra_keys, [extra1, extra2]}]} = jmeta:is({?WMS(arrival), A2}),
  A3 = jframe:delete([extra1, extra2], jframe:store({status, 2}, A2)),
  true = jmeta:is({?WMS(arrival), A3}),
  AI1 = jframe:new([{id, 1},
    {product_id, 2},
    {expire, {date(), {0, 0, 0.0}}},
    {count, 10}]),
  A4 = jframe:store([{id, 1}, {items, [AI1]}], A3),
  true = jmeta:is({?WMS(arrival), A4}),
  A5 = jframe:update({items, fun([Item]) -> lists:duplicate(10, Item) end}, A4),
  true = jmeta:is({?WMS(arrival), A5}),
  % override fields via extend
  jmeta:add({frame, ?WMS('arrival.a'),
    [{extend, [?WMS(arrival)]},
      {fields,
        [{nr, [{is, integer}]}
        ]}
    ]}),
  {error, [_, {violated, [{nr, {not_a, {std, integer, []}}}]}]} = jmeta:is({?WMS('arrival.a'), A5}),
  A6 = jframe:store({nr, 123}, A5),
  true = jmeta:is({?WMS('arrival.a'), A6}),
  % invalidation
  Scenario = fun() ->
    jmeta:add({frame, ?N('test.cache'),
      [{extend, [base]}
      ]}),
    TC1 = [{id, 1}],
    true = jmeta:is({?N('test.cache'), TC1}),
    jmeta:add({frame, ?N('test.cache'),
      [{fields,
        [{id, [{is, string128}]}
        ]}
      ]}),
    true = jmeta:is({?N('test.cache'), TC1}),
    jmeta:cache_reset(),
    {error, [_, {violated, [{id, {not_a, {std, string128, []}}}]}]} = jmeta:is({?N('test.cache'), TC1})
  end,
  jmeta:cache_for(Scenario),
  % pick and mixins tricks
  jmeta:add({type, ?N(range1_7), [{guards, [fun(X) -> lists:member(X, lists:seq(1, 7)) end]}]}),
  jmeta:add({type, ?N(range4_9), [{guards, [fun(X) -> lists:member(X, lists:seq(4, 9)) end]}]}),
  jmeta:add({type, ?N(mix_1_7_and_4_9), [{mixins, [?N(range1_7), ?N(range4_9)]}, {mode, [{mixins, all}]}]}),
  jmeta:add({type, ?N(mix_1_7_or_4_9), [{mixins, [?N(range1_7), ?N(range4_9)]}, {mode, [{mixins, any}]}]}),
  Nums = lists:seq(-20, 20),
  [1, 2, 3, 4, 5, 6, 7] = jmeta:pick({?N(range1_7), Nums}),
  [4, 5, 6, 7, 8, 9] = jmeta:pick({?N(range4_9), Nums}),
  [4, 5, 6, 7] = jmeta:pick({?N(mix_1_7_and_4_9), Nums}),
  [1, 2, 3, 4, 5, 6, 7, 8, 9] = jmeta:pick({?N(mix_1_7_or_4_9), Nums}),
  % just yet another complex test
  Identifier =
    {type, ?N(identifier),
      [{mixins, [null, integer]},
        {mode, [{mixins, any}]}
      ]},
  Entity =
    {frame, ?N(entity),
      [{extend, [base]},
        {fields,
          [{id, {is, ?N(identifier)}}
          ]}
      ]},
  User =
    {frame, ?N(user),
      [{extend, [?N(entity)]},
        {fields,
          [{first_name, {is, string}},
            {second_name, [{is, string}, {optional, true}]},
            {nickname, [{is, string}, {optional, true}]},
            {dob, [{is, ?N(iso8601)}, {optional, true}]},
            {status, [{is, atom}, {guards, [fun(X) -> lists:member(X, [active, inactive]) end]}]},
            {projects, {list_of, ?N(project)}}
          ]}
      ]},
  Project =
    {frame, ?N(project),
      [{extend, [?N(entity)]},
        {fields,
          [{name, {is, string}},
            {created, {is, ?N(iso8601)}},
            {commits, [{is, integer}, {guards, [fun(X) -> X > 0 end]}]}
          ]}
      ]},
  lists:foreach(fun jmeta:add/1, [Identifier, Entity, User, Project]),
  Kostya =
    [{first_name, <<"Kostya">>},
      {second_name, duman},
      {middle_name, <<"V">>},
      {nickname, <<"Said">>},
      {status, destructive},
      {projects, [
        [{name, jobcheck},
          {created, <<"2012-01-09">>},
          {commits, 276}],
        [{id, null},
          {name, sqlex},
          {created, <<"Apr 29, 2013">>},
          {commits, 0}],
        [{id, 1},
          {name, <<"jmeta">>},
          {created, <<20130209>>},
          {subscribers, 2}]
      ]}],
  % OK, here we are!
  {error,
    [{not_a, ?N(user, [])},
      {violated, [
        {id, missed},
        {second_name, {not_a, {std, string, []}}},
        {status, {{is, {std, atom, []}}, but_breaking_a_guard}},
        {projects, [
          [{error,
            [{not_a, ?N(project, [])},
              {violated, [
                {id, missed},
                {name, {not_a, {std, string, []}}}
              ]}
            ]},
            {pos, 1}
          ],
          [{error,
            [{not_a, ?N(project, [])},
              {violated, [
                {name, {not_a, {std, string, []}}},
                {created, {not_a, ?N(iso8601, [])}},
                {commits, {{is, {std, integer, []}}, but_breaking_a_guard}}
              ]}
            ]},
            {pos, 2}
          ],
          [{error,
            [{not_a, ?N(project, [])},
              {violated, [
                {created, {not_a, ?N(iso8601, [])}},
                {commits, missed}
              ]},
              {extra_keys, [subscribers]}
            ]},
            {pos, 3}
          ]
        ]}
      ]},
      {extra_keys, [middle_name]}
    ]
  } = jmeta:is({?N(user), Kostya}).

integration_test_() ->
  {setup,
    fun jmeta:start/0,
    fun(_) -> jmeta:stop() end,
    ?_test(integration())}.
