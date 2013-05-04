%% Author: Said
%% Created: 14.03.2013
%% Published under MIT license.
%% Description: TODO: Add description to jmeta_exception
-module(jmeta_exception).

%%
%% Include files
%%

-include("jtest.hrl").

-define(KEY, jmeta.exception).

%%
%% Exported Functions
%%

% test
-export([test/0]).

% api
-export([new/1,
         safe_try/2]).

%%
%% API Functions
%%

test() ->
    test_api(),
    jmeta_test:done().

new(Reason) ->
    error({?KEY, Reason}).

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
