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
-export([% setup tupes
         add_type/1, delete_type/1, get_type/1,
         % setup frames
         add_frame/1, delete_frame/1, get_frame/1,
         % jmeta api
         is_type/1, is_frame/1,
         list_of_type/1, list_of_frame/1,
         % jmeta simple api (use in on your own risk)
         is/1, list_of/1]).

%%
%% API Functions
%%

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

% setup tupes

add_type(Meta) ->
    jmeta_type_cache:add(Meta).

delete_type(Name) ->
    jmeta_type_cache:delete(Name).

get_type(Name) ->
    jmeta_type_cache:get(Name).

add_frame(Meta) ->
    jmeta_frame_cache:add(Meta).

delete_frame(Name) ->
    jmeta_frame_cache:delete(Name).

get_frame(Name) ->
    jmeta_frame_cache:get(Name).

% jmeta api

is_type(X) ->
    jmeta_check:is_type(X).

is_frame(X) ->
    jmeta_check:is_frame(X).

list_of_type(X) ->
    jmeta_check:list_of_type(X).

list_of_frame(X) ->
    jmeta_check:list_of_frame(X).

% jmeta simple api

is(X) ->
    jmeta_check:is(X).

list_of(X) ->
    jmeta_check:list_of(X).

%%
%% Local Functions
%%

