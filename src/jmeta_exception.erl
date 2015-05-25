%%% coding: UTF-8
%%%-------------------------------------------------------------------
%%% @author Konstantin Duman <said.dk@gmail.com>
%%% @copyright (C) 2015, FreeGeo
%%% Published under MIT license.
%%%
%%% @doc
%%% Some core-specific utils.
%%% @end
%%%-------------------------------------------------------------------
-module(jmeta_exception).

-include("jtest.hrl").

-define(KEY, 'jmeta.exception').

%% test
-export([test/0]).

%% api
-export([new/1, safe_try/2]).

%% API Functions
test() ->
  test_api(),
  jmeta_test:done().

new(Reason) -> error({?KEY, Reason}).

safe_try(Scenario, OnException) ->
  try
    Scenario()
  catch
    error:{?KEY, Reason} -> new(Reason);
    Class:Reason -> OnException({Class, Reason})
  end.

%%
%% Local Functions
%%

test_api() ->
  DontCare = fun(_) -> dont_care end,
  ThrowTest = fun() -> throw(test) end,
  ?EXCEPTION(error, {?KEY, test}, safe_try(fun() -> new(test) end, DontCare)),
  1 = safe_try(fun() -> 1 end, DontCare),
  dont_care = safe_try(ThrowTest, DontCare),
  {throw, test} = safe_try(ThrowTest, fun(X) -> X end).
