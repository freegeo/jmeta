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

-module(jmeta_exception_tests).
-include_lib("eunit/include/eunit.hrl").

api_exception_test() ->
  DontCare = fun(_) -> dont_care end,
  ThrowTest = fun() -> throw(test) end,
  ?assertException(error, {'jmeta.exception', test},
    jmeta_exception:safe_try(fun() -> jmeta_exception:new(test) end, DontCare)),
  1 = jmeta_exception:safe_try(fun() -> 1 end, DontCare),
  dont_care = jmeta_exception:safe_try(ThrowTest, DontCare),
  {throw, test} = jmeta_exception:safe_try(ThrowTest, fun(X) -> X end).
