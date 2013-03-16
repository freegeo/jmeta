%% Author: Said
%% Created: 19.02.2013
%% Description: TODO: Add description to jmeta_speed_test
-module(jmeta_speed_test).

%%
%% Include files
%%

-include("jtest.hrl").

%%
%% Exported Functions
%%

-export([run/2]).

%%
%% API Functions
%%

run(Repeats, UseCache) ->
    Scenario =
        fun() ->
                {ok, [{setup, test(fun test_setup/0, Repeats)},
                      {api, test(fun test_api/0, Repeats)}]}
        end,
    case UseCache of
        true -> jmeta:cache_for(Scenario);
        false -> Scenario()
    end.

%%
%% Local Functions
%%

test(F, Repeats) ->
    {Result, ok} = timer:tc(fun() -> lists:foreach(fun(_) -> F() end, lists:seq(1, Repeats)) end),
    {Result / 1000 / 1000, sec}.

test_setup() ->
    TestTypes =
        [case Meta of
             {type, Name, Data} -> {type, ?TN(Name), Data};
             {frame, Name, Data} -> {frame, ?TN(Name), Data}
         end || Meta <- jmeta_library:std()],
    lists:foreach(fun jmeta:add/1, TestTypes),
    lists:foreach(fun jmeta:delete/1,
                  [case Meta of
                       {type, Name, _} -> Name;
                       {frame, Name, _} -> Name
                   end || Meta <- TestTypes]).

% TODO more tests for frames 
test_api() ->
    Tests =
        [{atom, ok},
         {numeric, 10.23},
         {integer, 100},
         {bit, 1},
         {float, 1.234},
         {boolean, true},
         {list, []},
         {non_empty_list, "ABC"},
         {set_keys, [<<"a">>, <<"b">>, <<"c">>]},
         {set_refs, [1, 2, 3]},
         {list_of_frames, [[], [], []]},
         {tuple, {1, 2, 3}},
         {string, <<>>},
         {string128, <<>>},
         {binary, <<>>},
         {date, {2013, 2, 16}},
         {time.a, {0, 0, 0}},
         {time.b, {0, 0, 0.0}},
         {timestamp.a, {{2013, 03, 14}, {3, 48, 55}}},
         {timestamp.b, {{2013, 03, 14}, {3, 48, 55.0}}},
         {timestamp_range.a, [{{2013, 03, 14}, {3, 48, 55}},
                              {{2013, 03, 15}, {0, 0, 0}}]},
         {timestamp_range.b, [{{2013, 03, 14}, {3, 48, 55.0}},
                              {{2013, 03, 15}, {0, 0, 0.0}}]},
         {frame, [{a, 1}, {b, 2}]},
         {new_frame, [{a, 1}, {b, 2}]},
         {empty_frame, []},
         {base, [{id, 1}]}],
    lists:foreach(fun(X) -> true = jmeta:is(X) end, Tests).
