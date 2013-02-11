%% Author: Said
%% Created: 11.02.2013
%% Description: TODO: Add description to jmeta_check
-module(jmeta_check).

%%
%% Include files
%%

%%
%% Exported Functions
%%

% test
-export([test/0]).

% api
-export([type/1, frame/1]).

%%
%% API Functions
%%

test() ->
    {ok, done}.

type({TypeName, _RawData}) when is_atom(TypeName) ->
    {ok, checked};
type(_) ->
    {error, wrong_type_check_format}.

frame({FrameName, _RawData}) when is_atom(FrameName) ->
    {ok, checked};
frame(_) ->
    {error, wrong_frame_check_format}.

%%
%% Local Functions
%%

