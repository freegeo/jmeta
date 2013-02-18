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
-export([is/1, list_of/1]).

%%
%% API Functions
%%

test() ->
    test_is(),
    test_list_of(),
    {ok, done}.

% api

is({{_, _} = Key, RawData}) ->
    case prepare_cache(Key) of
        {error, _} = E -> E;
        {Types, _} = Cache ->
            case proplists:is_defined(Key, Types) of
                true -> is_type(Key, RawData, Cache);
                false -> is_frame(Key, RawData, Cache)
            end
    end;
is({Name, RawData}) when is_atom(Name) -> is({{std, Name}, RawData});
is(_) -> {error, wrong_check_format}.

list_of({{_, _} = Key, RawData}) ->
    case prepare_cache(Key) of
        {error, _} = E -> E;
        {Types, _} = Cache ->
            case proplists:is_defined(Key, Types) of
                true -> is_type(Key, RawData, Cache);
                false -> is_frame(Key, RawData, Cache)
            end
    end;
list_of({Name, RawData}) when is_atom(Name) -> list_of({{std, Name}, RawData});
list_of(_) -> {error, wrong_list_check_format}.

%%
%% Local Functions
%%

prepare_cache(X) ->
    prepare_cache([X], {[], []}).

prepare_cache([], {Types, Frames}) ->
    {Types,
     [{Key, F#frame{fields=[update_method(Field, Types) || Field <- F#frame.fields]}}
     || {Key, F} <- Frames]};
prepare_cache([Key|Rest], {Types, Frames}) ->
    case proplists:is_defined(Key, Types) orelse proplists:is_defined(Key, Frames) of
        true -> prepare_cache(Rest, {Types, Frames});
        false ->
            case jmeta_namespace:get(Key) of
                {error, _} = E -> E;
                Data ->
                    case jmeta_declaration:kind(Data) of
                        type -> prepare_cache(Rest ++ Data#type.require, {[{Key, Data}|Types], Frames});
                        frame -> prepare_cache(Rest ++ Data#frame.require, {Types, [{Key, Data}|Frames]})
                    end
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

is_type(TypeKey, RawData, {Types, _} = Cache) ->
    try
        #type{mode=#tmode{guards=MGuards, mixins=MMixins},
              guards=Guards, mixins=Mixins} = proplists:get_value(TypeKey, Types),
        true = lists:MMixins(fun(Mixin) -> true =:= is_type(Mixin, RawData, Cache) end, Mixins),
        true = lists:MGuards(fun(Guard) -> true =:= Guard(RawData) end, Guards)
    catch
        _:_ -> {error, {not_a, TypeKey}}
    end.

is_frame(FrameKey, RawData, {_, Frames} = Cache) ->
    case true =:= jframe:is_frame(RawData) of
        false -> {error, not_a_frame};
        true ->
            Fields = extend_fields(FrameKey, Frames), % TODO optimize
            {Violated, Rest, _} = lists:foldl(fun process_field/2, {[], RawData, Cache}, Fields),
            case Violated of
                [] ->
                    case Rest of
                        [] -> true;
                        _ -> {error, {extra_keys, jframe:keys(Rest)}}
                    end;
                _ -> {error, [{not_a, FrameKey}, {violated, Violated}]}
            end
    end.

process_field({FieldName, #field{method=Method, guards=Guards, mode=#fmode{optional=Optional}}},
              {Violated, Frame, Cache}) ->
    {FieldData, Rest} = jframe:take(FieldName, Frame),
    try
        case jframe:has(FieldName, Frame) of
            false -> Optional = true;
            true ->
                true = Method(FieldData, Cache),
                lists:all(fun(Guard) -> true = Guard(FieldData) end, Guards)
        end,
        {Violated, Rest, Cache}
    catch
        _:_ -> {[FieldName|Violated], Rest, Cache}
    end.

extend_fields(FrameKey, Frames) ->
    #frame{extend=Extend, fields=Fields} = proplists:get_value(FrameKey, Frames),
    Base = jframe:extend(jframe:new(), [extend_fields(E, Frames) || E <- Extend]),
    jframe:extend(Base, [{Field#field.name, Field} || Field <- Fields]).

update_method(#field{class=Class} = Field, Types) ->
    Wrap = fun(Key, Fun) -> fun(Data, Cache) -> Fun(Key, Data, Cache) end end,
    Method =
        case Class of
            {is, Key} ->
                case proplists:is_defined(Key, Types) of
                    true -> Wrap(Key, fun is_type/3);
                    false -> Wrap(Key, fun is_frame/3)
                end;
            {list_of, Key} ->
                case proplists:is_defined(Key, Types) of
                    true -> Wrap(Key, fun list_of_type/3);
                    false -> Wrap(Key, fun list_of_frame/3)
                end
        end,
    Field#field{method=Method}.

% TODO tests
test_is() ->
    ok.

test_list_of() ->
    ok.
