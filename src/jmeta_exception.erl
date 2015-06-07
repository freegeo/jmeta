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

-define(KEY, 'jmeta.exception').

%% api
-export([new/1, safe_try/2]).

%% API Functions
new(Reason) -> error({?KEY, Reason}).

safe_try(Scenario, OnException) ->
  try
    Scenario()
  catch
    error:{?KEY, Reason} -> new(Reason);
    Class:Reason -> OnException({Class, Reason})
  end.
