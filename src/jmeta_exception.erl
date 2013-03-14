%% Author: Said
%% Created: 14.03.2013
%% Description: TODO: Add description to jmeta_exception
-module(jmeta_exception).

%%
%% Include files
%%

-define(KEY, jmeta.exception).

%%
%% Exported Functions
%%
-export([new/1,
         safe_try/2]).

%%
%% API Functions
%%

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

