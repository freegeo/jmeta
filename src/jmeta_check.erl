%% Author: Said
%% Created: 11.02.2013
%% Description: TODO: Add description to jmeta_check
-module(jmeta_check).

%%
%% Include files
%%

-include("jmeta.hrl").

%%
%% Exported Functions
%%

% test
-export([test/0]).

% api
-export([type/1, frame/1]).

% used by meta_caches
-export([type_cache_check/2]).

%%
%% API Functions
%%

test() ->
    {ok, done}.

% api

type({TypeName, RawData}) when is_atom(TypeName) ->
    Checker = jmeta_type_cache:checker(TypeName),
    Checker(RawData);
type(_) -> {error, wrong_type_check_format}.

frame({FrameName, _RawData}) when is_atom(FrameName) ->
    {ok, checked};
frame(_) -> {error, wrong_frame_check_format}.

% used by meta_caches

type_cache_check(#type{constraints=Constraints, mixins=Mixins, name=TypeName,
                       mode=#tmode{constraints=MConstraints, mixins=MMixins}},
                 RawData) ->
    try
        true = lists:MMixins(fun(Mixin) -> true =:= type({Mixin, RawData}) end, Mixins),
        true = lists:MConstraints(fun(Constraint) -> true =:= Constraint(RawData) end, Constraints)
    catch
        _:_ -> {error, {not_a, TypeName}}
    end.

%%
%% Local Functions
%%

