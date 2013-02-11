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

-export([is_type/1, is_frame/1]).

%%
%% API Functions
%%

is_type({TypeName, _RawData}) when is_atom(TypeName) ->
    {ok, checked};
is_type(_) ->
    {error, wrong_type_check_format}.

is_frame({FrameName, _RawData}) when is_atom(FrameName) ->
    {ok, checked};
is_frame(_) ->
    {error, wrong_frame_check_format}.

%%
%% Local Functions
%%

