%%% coding: UTF-8
%%%-------------------------------------------------------------------
%%% @author Konstantin Duman <said.dk@gmail.com>
%%% @copyright (C) 2015, FreeGeo
%%% Published under MIT license.
%%%
%%% @doc
%%% Provides main API for jmeta.
%%% Using this module you install new types or frames and check raw data via unified API.
%%% @end
%%%-------------------------------------------------------------------

-module(jmeta).

-export([start/0, stop/0]).

%% api
-export([% setup
  add/1, delete/1, get/1,
  % meta api
  is/1, list_of/1, cache_for/1, cache_reset/0,
  % tests
  test/0,
  speed_test/0, speed_test/1, speed_test/2]).

%% API Functions
start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

%% setup
add(Meta) -> jmeta_namespace:add(Meta).
delete(Name) -> jmeta_namespace:delete(Name).
get(Name) -> jmeta_namespace:get(Name).

%% meta api
is(X) -> jmeta_check:is(X).
list_of(X) -> jmeta_check:list_of(X).
cache_for(Scenario) -> jmeta_cache:for(Scenario).
cache_reset() -> jmeta_cache:reset().

%% tests
test() -> jmeta_test:run().
speed_test() -> speed_test(1000).
speed_test(Repeats) -> jmeta_speed_test:run(Repeats, true).
speed_test(Repeats, UseCache) -> jmeta_speed_test:run(Repeats, UseCache).
