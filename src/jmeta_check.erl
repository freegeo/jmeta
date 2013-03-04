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
    initial(Key,
            fun(type) -> is_type(Key, RawData);
               (frame) -> is_frame(Key, RawData)
            end);
is({Name, RawData}) when is_atom(Name) -> is({{std, Name}, RawData});
is(_) -> {error, wrong_check_format}.

list_of({{_, _} = Key, RawData}) ->
    initial(Key,
            fun(type) -> list_of_type(Key, RawData);
               (frame) -> list_of_frame(Key, RawData)
            end);
list_of({Name, RawData}) when is_atom(Name) -> list_of({{std, Name}, RawData});
list_of(_) -> {error, wrong_list_check_format}.

%%
%% Local Functions
%%

initial(Key, Selector) ->
    Scenario =
        fun() ->
                case jmeta_cache:type_or_frame(Key) of
                    {error, _} = E -> E;
                    Meta -> Selector(jmeta_declaration:kind(Meta))
                end
        end,
    jmeta_cache:for(Scenario).

list_of(Checker, ListOfRawData) ->
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

list_of_type(Key, ListOfRawData) ->
    list_of(fun(RawData) -> is_type(Key, RawData) end, ListOfRawData).

list_of_frame(Key, ListOfRawData) ->
    list_of(fun(RawData) -> is_frame(Key, RawData) end, ListOfRawData).

is_type(Key, RawData) ->
    try
        % Maybe we can make it more informative.
        % Just try rethrow missing type error.
        #type{mode=#tmode{guards=MGuards, mixins=MMixins},
              guards=Guards, mixins=Mixins} = jmeta_cache:type(Key),
        true = lists:MMixins(fun(Mixin) -> true =:= is_type(Mixin, RawData) end, Mixins),
        true = lists:MGuards(fun(Guard) -> true =:= Guard(RawData) end, Guards)
    catch
        _:_ -> {error, {not_a, Key}}
    end.

is_frame(Key, RawData) ->
    case true =:= jframe:is_frame(RawData) of
        false -> {error, not_a_frame};
        true ->
            case jmeta_cache:extended_fields(Key) of
                {error, _} = E -> E;
                Fields ->
                    {Violated, Rest} = lists:foldl(fun process_field/2, {[], RawData}, Fields),
                    case Violated of
                        [] ->
                            case Rest of
                                [] -> true;
                                _ -> {error, {extra_keys, jframe:keys(Rest)}}
                            end;
                        _ -> {error, [{not_a, Key}, {violated, Violated}]}
                    end
            end
    end.

process_field({FieldName, #field{class=Class, guards=Guards, optional=Optional}},
              {Violated, Frame}) ->
    {FieldData, Rest} = jframe:take(FieldName, Frame),
    try
        case jframe:has(FieldName, Frame) of
            false -> Optional = true;
            true ->
                true =
                    case Class of
                        {is, Key} -> is({Key, FieldData});
                        {list_of, Key} -> list_of({Key, FieldData})
                    end,
                lists:all(fun(Guard) -> true = Guard(FieldData) end, Guards)
        end,
        {Violated, Rest}
    catch
        _:_ -> {[FieldName|Violated], Rest}
    end.

% TODO tests
test_is() ->
    ok.

test_list_of() ->
    ok.
