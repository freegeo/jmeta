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
         % jmeta api
         is_type/1, is_frame/1]).

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

% jmeta api

is_type(X) ->
    jmeta_check:type(X).

is_frame(X) ->
    jmeta_check:frame(X).

%%
%% Local Functions
%%

