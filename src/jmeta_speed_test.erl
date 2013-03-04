%% Author: Said
%% Created: 19.02.2013
%% Description: TODO: Add description to jmeta_speed_test
-module(jmeta_speed_test).

%%
%% Include files
%%

%%
%% Exported Functions
%%

-export([run/0]).

%%
%% API Functions
%%

run() ->
    Result = {ok, [{load, test_load()}, {api, test_api()}]},
    cleanup(),
    Result.

%%
%% Local Functions
%%

test_load() ->
    ok.

test_api() ->
    ok.

cleanup() ->
    ok.
