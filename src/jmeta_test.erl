%% Author: Said
%% Mailto: said.dk@gmail.com
%% Created: 19.02.2013
%% Published under MIT license.
%% Description: provides test system for jetta.
-module(jmeta_test).

%%
%% Include files
%%

-define(DONE_KEY, {ok, done}).
-define(N(X), {freegeo.jmeta.test, X}).
-define(WMS(X), {freegeo.jmeta.wms, X}).

%%
%% Exported Functions
%%

-export([run/0, done/0]).

%%
%% API Functions
%%

run() ->
    Modules =
        [list_to_atom(filename:basename(Name, ".beam")) ||
           Name <- filelib:wildcard([filename:dirname(code:which(?MODULE)), "/*.beam"])],
    ModulesWithTest =
        [Module ||
         Module <- Modules,
         lists:member({test, 0}, Module:module_info(exports)),
         Module =/= jmeta],
    case [R || R <- [check(M) || M <- ModulesWithTest], R =/= ?DONE_KEY] of
        [] -> integration_test();
        Errors -> {error, Errors}
    end.

done() ->
    ?DONE_KEY.

%%
%% Local Functions
%%

check(M) ->
    try
        {ok, done} = M:test()
    catch
        _:Why -> [{module, M}, {reason, Why}, {stack, erlang:get_stacktrace()}]
    end.

integration_test() ->
    % types and mixins
    DateISO8601a = <<"20130228">>, % YYYYMMDD
    DateISO8601b = <<"2013-02-28">>, % YYYY-MM-DD
    ISO8601a =
        {type, ?N(iso8601.a),
         [{guards, [fun(<<Y:(4*8)/bitstring, M:(2*8)/bitstring, D:(2*8)/bitstring>>) ->
                            Date = list_to_tuple([list_to_integer(bitstring_to_list(X)) || X <- [Y, M, D]]),
                            jmeta:is({date, Date})
                    end]}
         ]},
    ISO8601b =
        {type, ?N(iso8601.b),
         [{guards, [fun(<<Y:(4*8)/bitstring, "-", M:(2*8)/bitstring, "-", D:(2*8)/bitstring>>) ->
                            jmeta:is({?N(iso8601.a), <<Y/bitstring, M/bitstring, D/bitstring>>})
                    end]}
         ]},
    ISO8601 =
        {type, ?N(iso8601),
         [{mixins, [?N(iso8601.a), ?N(iso8601.b)]},
          {mode, [{mixins, any}]}
         ]},
    lists:foreach(fun jmeta:add/1, [ISO8601a, ISO8601b, ISO8601]),
    true = jmeta:is({?N(iso8601.a), DateISO8601a}),
    {error, {not_a, ?N(iso8601.a)}} = jmeta:is({?N(iso8601.a), DateISO8601b}),
    false = true =:= jmeta:is({?N(iso8601.a), <<"20130229">>}), % right format, wrong date
    true = jmeta:is({?N(iso8601.b), DateISO8601b}),
    false = true =:= jmeta:is({?N(iso8601.b), DateISO8601a}),
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
    false = true =:= jmeta:is({?N(str), 32#jmeta}),
    % list of
    TestList = [2#01, <<"2">>, 3, "4", [5], 6, {7}],
    ListOfResult =
        [[{error, {not_a, {std, integer}}}, {pos, 7}],
         [_, {pos, 5}],
         [_, {pos, 4}],
         [_, {pos, 2}]] = jmeta:list_of({integer, TestList}),
    [{7}, [5], "4", <<"2">>] = [lists:nth(jframe:find(pos, X), TestList) || X <- ListOfResult],
    % frames
    true = jmeta:is({base, []}),
    true = jmeta:is({base, [{id, 1}]}),
    {error, [{not_a, {std, base}}, {extra_keys, [name]}]} = jmeta:is({base, [{name, <<>>}]}),
    Arrival = 
        {frame, ?WMS(arrival),
         [{extend, [base]},
          {fields,
           [{datetime, [{is, timestamp.b}, {optional, true}]},
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
            {expire, [{is, timestamp.b}]},
            {count, [{is, integer}]}
           ]}
         ]},
    lists:foreach(fun jmeta:add/1, [Arrival, ArrivalItem]),
    A1 = jframe:new(),
    {error, [{not_a, ?WMS(arrival)},
             {violated, [items, status, warehouse_id, nr]}]} = jmeta:is({?WMS(arrival), A1}),
    A2 = jframe:store([{nr, <<"123">>},
                       {warehouse_id, 1},
                       {status, 5},
                       {extra1, some_data}, % complex extra keys test
                       {extra2, some_data},
                       {items, []}], A1),
    {error, [_,
             {violated, [status]},
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
    jmeta:add({frame, ?WMS(arrival.a),
               [{extend, [?WMS(arrival)]},
                {fields,
                 [{nr, [{is, integer}]}
                 ]}
               ]}),
    {error, [_, {violated, [nr]}]} = jmeta:is({?WMS(arrival.a), A5}),
    A6 = jframe:store({nr, 123}, A5),
    true = jmeta:is({?WMS(arrival.a), A6}),
    % invalidation
    Scenario =
        fun() ->
                jmeta:add({frame, ?N(test.cache),
                           [{extend, [base]}
                           ]}),
                TC1 = [{id, 1}],
                true = jmeta:is({?N(test.cache), TC1}),
                jmeta:add({frame, ?N(test.cache),
                           [{fields,
                             [{id, [{is, string128}]}
                             ]}
                           ]}),
                true = jmeta:is({?N(test.cache), TC1}),
                jmeta:cache_reset(),
                {error,[_, {violated, [id]}]} = jmeta:is({?N(test.cache), TC1})
        end,
    jmeta:cache_for(Scenario),
    cleanup(),
    {ok, complete}.

cleanup() ->
    Types =
        [?N(iso8601.a),
         ?N(iso8601.b),
         ?N(iso8601),
         ?N(foreign_key),
         ?N(str),
         ?WMS(arrival),
         ?WMS(arrival_item),
         ?WMS(arrival.a),
         ?N(test.cache)],
    lists:foreach(fun jmeta:delete/1, Types).
