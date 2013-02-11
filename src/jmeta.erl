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
-export([is_type/1, is_frame/1]).

%%
%% API Functions
%%

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

is_type(X) ->
    jmeta_check:type(X).

is_frame(X) ->
    jmeta_check:frame(X).

%%
%% Local Functions
%%

