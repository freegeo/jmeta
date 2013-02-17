%% Author: Said
%% Created: 08.02.2013
%% Description: TODO: Add description to jmeta_declaration
-module(jmeta_declaration).

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
-export([parse_type/1,
         parse_frame/1]).

%%
%% API Functions
%%

test() ->
    test_type(),
    test_frame(),
    {ok, done}.

parse_type({type, Name, Meta}) when is_atom(Name) ->
    case jframe:new(Meta) of
        {error, wrong_frame} -> {error, meta_wrong_frame};
        _ ->
            {Mixins, Guards, Default, Mode, Rest} =
                jframe:take([{mixins, []},
                             {guards, []},
                             default,
                             {mode, [{mixins, all},
                                     {guards, all}]}], Meta),
            case jframe:is_empty(Rest) of
                false -> {error, meta_contains_wrong_keys};
                true ->
                    case Mixins =:= [] andalso Guards =:= [] of
                        true -> {error, meta_is_empty};
                        false ->
                            case lists:all(fun is_atom/1, Mixins) of
                                false -> {error, mixins_must_be_atoms};
                                true ->
                                    case is_list_of_unary_funs(Guards) of
                                        false -> {error, guards_must_be_unary_funs};
                                        true ->
                                            case jframe:new(Mode) of
                                                {error, wrong_frame} -> {error, mode_wrong_frame};
                                                _ ->
                                                    {MixinsMode, GuardsMode} =
                                                        jframe:find([{mixins, all},
                                                                     {guards, all}], Mode),
                                                    AllOrAny = fun(all) -> true; (any) -> true; (_) -> false end,
                                                    case lists:all(AllOrAny, [MixinsMode, GuardsMode]) of
                                                        false -> {error, mode_wrong_arguments};
                                                        true ->
                                                            Type = #type{name=Name,
                                                                         mixins=jtils:list_distinct(Mixins),
                                                                         guards=Guards,
                                                                         default = Default,
                                                                         mode=#tmode{mixins=MixinsMode,
                                                                                     guards=GuardsMode}},
                                                            calc_type_require(Type)
                                                    end
                                            end
                                    end
                            end
                    end
            end
    end;
parse_type(_) -> {error, wrong_type_format}.

parse_frame({frame, Name, Meta}) when is_atom(Name) ->
    case jframe:new(Meta) of
        {error, wrong_frame} -> {error, meta_wrong_frame};
        _ ->
            {Extend, Fields, Rest} = jframe:take([{extend, []}, {fields, []}], Meta),
            case jframe:is_empty(Rest) of
                false -> {error, meta_contains_wrong_keys};
                true ->
                    case lists:all(fun is_atom/1, Extend) of
                        false -> {error, extend_must_be_atoms};
                        true ->
                            case jframe:new(Fields) of
                                {error, wrong_frame} -> {error, fields_wrong_frame};
                                _ ->
                                    ParseFields =
                                        fun({FieldName, _} = Field) ->
                                                case parse_field(Field) of
                                                    {error, Reason} ->
                                                        {error, [{field, FieldName},
                                                                 {reason, Reason}]};
                                                    R -> R
                                                end
                                        end,
                                    ParsedFields = lists:map(ParseFields, Fields),
                                    case [Description || {error, Description} <- ParsedFields] of
                                        [] -> calc_frame_require(#frame{name=Name, extend=Extend, fields=ParsedFields});
                                        FieldsErrors -> {error, {incorrect_fields, FieldsErrors}}
                                    end
                            end
                    end
            end
    end;
parse_frame(_) -> {error, wrong_frame_format}.

%%
%% Local Functions
%%

parse_field({Name, Meta}) when is_atom(Name) ->
    case jframe:new(Meta) of
        {error, wrong_frame} -> {error, field_wrong_frame};
        _ ->
            {TypeClass, FrameClass, ListOfClass, Guards, Default, Mode, Rest} =
                jframe:take([{type, []},
                             {frame, []},
                             {list_of, []},
                             {guards, []},
                             default,
                             {mode, [{optional, false}]}], Meta),
            case jframe:is_empty(Rest) of
                false -> {error, field_contains_wrong_keys};
                true ->
                    case parse_class(TypeClass, FrameClass, ListOfClass) of
                        {error, _} = E -> E;
                        Class ->
                            case is_list_of_unary_funs(Guards) of
                                false -> {error, guards_must_be_unary_funs};
                                true ->
                                    case jframe:new(Mode) of
                                        {error, wrong_frame} -> {error, field_mode_wrong_frame};
                                        _ ->
                                            Optional = jframe:find({optional, false}, Mode),
                                            #field{name=Name,
                                                   class=Class,
                                                   guards=Guards,
                                                   default=Default,
                                                   mode=#fmode{optional=Optional}}
                                    end
                            end
                    end
            end
    end;
parse_field(_) -> {error, wrong_field_format}.

parse_class(TypeName, [], []) when is_atom(TypeName) -> {type, TypeName};
parse_class([], FrameName, []) when is_atom(FrameName) -> {frame, FrameName};
parse_class([], [], {type, TypeName} = ListOf) when is_atom(TypeName) -> {list_of, ListOf};
parse_class([], [], {frame, FrameName} = ListOf) when is_atom(FrameName) -> {list_of, ListOf};
parse_class(_, _, _) -> {error, ambiguous_class}.

calc_type_require(#type{mixins=Mixins} = T) ->
    T#type{require=[{type, M} || M <- Mixins]}.

calc_frame_require(#frame{extend=Extend, fields=Fields} = F) ->
    Require = [case Field#field.class of
                   {list_of, Class} -> Class;
                   Other -> Other
               end || Field <- Fields],
    F#frame{require=lists:usort([{frame, Frame} || Frame <- Extend] ++ Require)}.

is_list_of_unary_funs(List) ->
    lists:all(fun(Fun) -> {arity, 1} =:= erlang:fun_info(Fun, arity) end, List).

% TODO update tests
test_type() ->
    {error, wrong_type_format} = parse_type(1),
    {error, wrong_type_format} = parse_type({1, 1, 1}),
    {error, wrong_type_format} = parse_type({type, 1, 1}),
    {error, meta_wrong_frame} = parse_type({type, int, 1}),
    {error, meta_contains_wrong_keys} = parse_type({type, int, [{a, 1}, {b, 2}, {c, 3}]}),
    {error, meta_is_empty} = parse_type({type, int, []}),
    {error, meta_is_empty} = parse_type({type, int, [{mixins, []}]}),
    {error, meta_is_empty} = parse_type({type, int, [{guards, []}]}),
    {error, meta_is_empty} = parse_type({type, int, [{mixins, []}, {guards, []}]}),
    {error, mixins_must_be_atoms} = parse_type({type, int, [{mixins, [1, 2, 3]}]}),
    {error, guards_must_be_unary_funs} = parse_type({type, int, [{guards, [fun(1, 1) -> true end]}]}),
    {error, mode_wrong_arguments} = parse_type({type, int, [{guards, [fun is_integer/1]}, {mode, [{mixins, abc}]}]}),
    {error, mode_wrong_arguments} = parse_type({type, int, [{guards, [fun is_integer/1]}, {mode, [{guards, abc}]}]}),
    T1 = #type{name=int,
               mixins=[number],
               guards=[],
               default=undefined,
               mode=#tmode{guards=all,
                           mixins=all}} = parse_type({type, int, [{mixins, [number, number]}]}),
    T2 = T1#type{default=0},
    T2 = parse_type({type, int, [{mixins, [number]}, {default, 0}]}),
    T3 = T2#type{mode=T2#type.mode#tmode{guards=any}},
    T3 = parse_type({type, int, [{mixins, [number]}, {default, 0}, {mode, [{guards, any}]}]}),
    IsInteger = fun is_integer/1,
    T4 = T3#type{guards=[IsInteger]},
    T4 = parse_type({type, int, [{mixins, [number]}, {guards, [IsInteger]}, {default, 0}, {mode, [{guards, any}]}]}).

test_frame() ->
    % TODO parse_field and parse_frame tests
    ok.
