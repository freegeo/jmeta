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
-export([is_type/1, is_frame/1, list_of_type/1, list_of_frame/1, is/1, list_of/1]).

%%
%% API Functions
%%

test() ->
    test_type(),
    test_frame(),
    test_list_of(),
    {ok, done}.

% api

is_type({TypeName, RawData}) when is_atom(TypeName) ->
    case prepare_cache({type, TypeName}) of
        {error, _} = E -> E;
        Cache -> is_type(TypeName, RawData, Cache)
    end;
is_type(_) -> {error, wrong_type_check_format}.

is_frame({FrameName, RawData}) when is_atom(FrameName) ->
    case prepare_cache({frame, FrameName}) of
        {error, _} = E -> E;
        Cache -> is_frame(FrameName, RawData, Cache)
    end;
is_frame(_) -> {error, wrong_frame_check_format}.

list_of_type({TypeName, ListOfRawData}) when is_atom(TypeName) ->
    case prepare_cache({type, TypeName}) of
        {error, _} = E -> E;
        Cache -> list_of_type(TypeName, ListOfRawData, Cache)
    end;
list_of_type(_) -> {error, wrong_list_of_type_check_format}.

list_of_frame({FrameName, ListOfRawData}) when is_atom(FrameName) ->
    case prepare_cache({frame, FrameName}) of
        {error, _} = E -> E;
        Cache -> list_of_frame(FrameName, ListOfRawData, Cache)
    end;
list_of_frame(_) -> {error, wrong_list_of_frame_check_format}.

% simple api

is({_, RawData} = X) ->
    case jframe:is_frame(RawData) of
        true -> is_frame(X);
        false -> is_type(X)
    end.

list_of({_, RawData} = X) ->
    case is_type({list_of_frames, RawData}) of
        true -> list_of_frame(X);
        _ -> list_of_type(X)
    end.

%%
%% Local Functions
%%

prepare_cache(X) ->
    prepare_cache([X], {[], []}).

prepare_cache([], Result) -> Result;
prepare_cache([{type, TypeName}|Rest], {Types, Frames}) ->
    case jframe:has(TypeName, Types) of
        true -> prepare_cache(Rest, {Types, Frames});
        false ->
            case jmeta_type_cache:get(TypeName) of
                {error, _} = E -> E;
                Type -> prepare_cache(Rest ++ Type#type.require, {[{TypeName, Type}|Types], Frames})
            end
    end;
prepare_cache([{frame, FrameName}|Rest], {Types, Frames}) ->
    case jframe:has(FrameName, Frames) of
        true -> prepare_cache(Rest, {Types, Frames});
        false ->
            case jmeta_frame_cache:get(FrameName) of
                {error, _} = E -> E;
                Frame -> prepare_cache(Rest ++ Frame#frame.require, {Types, [{FrameName, Frame}|Frames]})
            end
    end.

std_list_of(Checker, ListOfRawData) ->
    Check =
        fun(RawData, {Result, Prev}) ->
                Current = Prev + 1,
                case Checker(RawData) of
                    true -> {Result, Current};
                    Error -> {[[Error, {pos, Current}]|Result], Current}
                end
        end,
    {Result, _} = lists:foldl(Check, {[], 0}, ListOfRawData),
    case Result of
        [] -> true;
        _ -> Result
    end.

list_of_type(TypeName, ListOfRawData, Cache) ->
    std_list_of(fun(RawData) -> is_type(TypeName, RawData, Cache) end, ListOfRawData).

list_of_frame(FrameName, ListOfRawData, Cache) ->
    std_list_of(fun(RawData) -> is_frame(FrameName, RawData, Cache) end, ListOfRawData).

is_type(TypeName, RawData, {Types, _} = Cache) ->
    try
        #type{mode=#tmode{guards=MGuards, mixins=MMixins},
              guards=Guards, mixins=Mixins} = jframe:find(TypeName, Types),
        true = lists:MMixins(fun(Mixin) -> true =:= is_type(Mixin, RawData, Cache) end, Mixins),
        true = lists:MGuards(fun(Guard) -> true =:= Guard(RawData) end, Guards)
    catch
        _:_ -> {error, {not_a, TypeName}}
    end.

is_frame(FrameName, RawData, {_, Frames} = Cache) ->
    case true =:= jframe:is_frame(RawData) of
        false -> {error, not_a_frame};
        true ->
            Fields = extend_fields(FrameName, Frames),
            {Violated, Rest, _} = lists:foldl(fun process_field/2, {[], RawData, Cache}, Fields),
            case Violated of
                [] ->
                    case Rest of
                        [] -> true;
                        _ -> {error, {extra_keys, jframe:keys(Rest)}}
                    end;
                _ -> {error, [{not_a, FrameName}, {violated, Violated}]}
            end
    end.

process_field({FieldName, #field{class=Class, guards=Guards, mode=#fmode{optional=Optional}}},
              {Violated, Frame, Cache}) ->
    {FieldData, Rest} = jframe:take(FieldName, Frame),
    try
        case Optional of
            false -> true = jframe:has(FieldName, Frame);
            _ -> dont_care
        end,
        case Class of
            {type, TypeName} -> true = is_type(TypeName, FieldData, Cache);
            {frame, FrameName} -> true = is_frame(FrameName, FieldData, Cache);
            {list_of, {type, TypeName}} -> true = list_of_type(TypeName, FieldData, Cache);
            {list_of, {frame, FrameName}} -> true = list_of_frame(FrameName, FieldData, Cache)
        end,
        lists:all(fun(Guard) -> true = Guard(FieldData) end, Guards)
    catch
        _:_ -> {[FieldName|Violated], Rest}
    end.

extend_fields(FrameName, Frames) ->
    #frame{extend=Extend, fields=Fields} = jframe:find(FrameName, Frames),
    Base = jframe:extend(jframe:new(), [extend_fields(E, Frames) || E <- Extend]),
    jframe:extend(Base, [Fields]).

% TODO tests
test_type() ->
    ok.

test_frame() ->
    ok.

test_list_of() ->
    ok.
