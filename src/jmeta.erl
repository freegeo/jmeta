%% Author: Said
%% Created: 11.02.2013
%% Description: TODO: Add description to jmeta
-module(jmeta).

%%
%% Include files
%%

%%
%% Exported Functions
%%

-export([start/0, stop/0]).

% api
-export([% setup
         add/1, delete/1, get/1,
         % meta api
         is/1, list_of/1]).

%%
%% API Functions
%%

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

% setup

add(Meta) ->
    jmeta_namespace:add(Meta).

delete(Name) ->
    jmeta_namespace:delete(Name).

get(Name) ->
    jmeta_namespace:get(Name).

% meta api

is(X) ->
    jmeta_check:is(X).

list_of(X) ->
    jmeta_check:list_of(X).

%%
%% Local Functions
%%

