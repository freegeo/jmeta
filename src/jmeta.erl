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
  add/1, delete/1, get/1, definitions/1,
  % validation api
  is/1, list_of/1, pick/1, cache_for/1, cache_reset/0]).

%% API Functions
-spec start() -> any().
start() -> application:start(?MODULE).

-spec stop() -> any().
stop() -> application:stop(?MODULE).

%% setup
-spec add(any()) -> any().
add(Meta) -> jmeta_namespace:add(Meta).

-spec delete(any()) -> any().
delete(Name) -> jmeta_namespace:delete(Name).

-spec get(any()) -> any().
get(Name) -> jmeta_namespace:get(Name).

-spec definitions(any()) -> [any()].
definitions(Namespace) -> jmeta_namespace:definitions(Namespace).

%% validation api
-spec is(any()) -> any().
is(X) -> jmeta_check:is(X).

-spec list_of(any()) -> any().
list_of(X) -> jmeta_check:list_of(X).

-spec pick({atom() | {atom(), atom()}, list()}) -> list().
pick({X, ListOfData}) -> [Data || Data <- ListOfData, true =:= jmeta:is({X, Data})].

-spec cache_for(fun(() -> X)) -> X when X :: any().
cache_for(Scenario) -> jmeta_cache:for(Scenario).

-spec cache_reset() -> any().
cache_reset() -> jmeta_cache:reset().
