%% Author: Said
%% Mailto: said.dk@gmail.com
%% Created: 08.02.2013
%% Published under MIT license.
%% Description: provides some API for parse and analyze jmeta types/frames definitions.
%% Can be useful if you want implement some extensions based on jmeta.
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
-export([parse/1, unparse/1,
         namespace/1, name/1, key/1, kind/1]).

%%
%% API Functions
%%

test() ->
    test_parse_behaviour(),
    test_parse_type(),
    test_parse_frame(),
    test_unparse(),
    test_misc(),
    jmeta_test:done().

parse({type, Name, Meta}) when is_atom(Name) -> parse({type, {std, Name}, Meta});
parse({type, {Namespace, Name} = Key, Meta}) when is_atom(Namespace) andalso is_atom(Name) ->
    case jframe:new(Meta) of
        {error, wrong_frame} -> {error, wrong_meta_frame};
        _ ->
            {Mixins, Guards, Mode, Rest} =
                jframe:take([{mixins, []},
                             {guards, []},
                             {mode, [{mixins, all},
                                     {guards, all}]}], Meta),
            case jframe:is_empty(Rest) of
                false -> {error, meta_contains_wrong_keys};
                true ->
                    case Mixins =:= [] andalso Guards =:= [] of
                        true -> {error, meta_is_empty};
                        false ->
                            case is_list_of_class_keys(Mixins) of
                                false -> {error, mixins_should_be_class_keys};
                                true ->
                                    case is_list_of_unary_funs(Guards) of
                                        false -> {error, guards_should_be_unary_funs};
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
                                                            StdMixins = [to_class_key(M) || M <- Mixins],
                                                            #type{name=Key,
                                                                  mixins=jtils:ulist(StdMixins),
                                                                  guards=Guards,
                                                                  mode=#tmode{mixins=MixinsMode,
                                                                              guards=GuardsMode}}
                                                    end
                                            end
                                    end
                            end
                    end
            end
    end;
parse({frame, Name, Meta}) when is_atom(Name) -> parse({frame, {std, Name}, Meta});
parse({frame, {Namespace, Name} = Key, Meta}) when is_atom(Namespace) andalso is_atom(Name) ->
    case jframe:new(Meta) of
        {error, wrong_frame} -> {error, wrong_meta_frame};
        _ ->
            {Extend, Fields, Rest} = jframe:take([{extend, []}, {fields, []}], Meta),
            case jframe:is_empty(Rest) of
                false -> {error, meta_contains_wrong_keys};
                true ->
                    case is_list_of_class_keys(Extend) of
                        false -> {error, extend_should_be_class_keys};
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
                                        [] ->
                                            StdExtend = [to_class_key(E) || E <- Extend],
                                            #frame{name=Key,
                                                   extend=jtils:ulist(StdExtend),
                                                   fields=ParsedFields};
                                        FieldsErrors -> {error, {incorrect_fields, FieldsErrors}}
                                    end
                            end
                    end
            end
    end;
parse(_) -> {error, wrong_meta_format}.

unparse(#type{name=Name, mixins=Mixins, guards=Guards, mode=#tmode{mixins=MMixins, guards=MGuards}}) ->
    {type, Name,
     [{mixins, Mixins},
      {guards, Guards},
      {mode, [{mixins, MMixins},
              {guards, MGuards}]}]};
unparse(#frame{name=Name, extend=Extend, fields=Fields}) ->
    {frame, Name,
     [{extend, Extend},
      {fields, [{FieldName, [Class, {guards, Guards}, {optional, Optional}]} ||
                #field{name=FieldName, class=Class, guards=Guards, optional=Optional} <- Fields ]}]};
unparse(_) -> {error, wrong_unparse_format}.

namespace(#type{name={Namespace, _}}) -> Namespace;
namespace(#frame{name={Namespace, _}}) -> Namespace;
namespace({_, {Namespace, _}, _}) -> Namespace;
namespace(_) -> {error, unknown_format}.

name(#type{name={_, Name}}) -> Name;
name(#frame{name={_, Name}}) -> Name;
name({_, {_, Name}, _}) -> Name;
name(_) -> {error, unknown_format}.

key(#type{name=Key}) -> Key;
key(#frame{name=Key}) -> Key;
key({_, {_, _} = Key, _}) -> Key;
key(_) -> {error, unknown_format}.

kind(#type{}) -> type;
kind(#frame{}) -> frame;
kind(_) -> {error, unknown_record}.

%%
%% Local Functions
%%

parse_field({Name, Meta}) when is_atom(Name) ->
    case jframe:new(Meta) of
        {error, wrong_frame} -> {error, field_wrong_frame};
        _ ->
            {Is, ListOf, Guards, Optional, Rest} =
                jframe:take([{is, []},
                             {list_of, []},
                             {guards, []},
                             {optional, false}], Meta),
            case jframe:is_empty(Rest) of
                false -> {error, field_contains_wrong_keys};
                true ->
                    case parse_class(Is, ListOf) of
                        {error, _} = E -> E;
                        Class ->
                            case is_list_of_unary_funs(Guards) of
                                false -> {error, guards_should_be_unary_funs};
                                true ->
                                    #field{name=Name,
                                           class=Class,
                                           guards=Guards,
                                           optional=Optional}
                            end
                    end
            end
    end;
parse_field(_) -> {error, wrong_field_format}.

parse_class(Is, []) when is_atom(Is) -> parse_class({std, Is}, []);
parse_class({Namespace, Name} = Is, []) when is_atom(Namespace) andalso is_atom(Name) -> {is, Is};
parse_class([], ListOf) when is_atom(ListOf) -> parse_class([], {std, ListOf});
parse_class([], {Namespace, Name} = ListOf) when is_atom(Namespace) andalso is_atom(Name) -> {list_of, ListOf};
parse_class(_, _) -> {error, ambiguous_class}.

is_class_key({Namespace, Name}) when is_atom(Namespace) andalso is_atom(Name) -> true;
is_class_key(Name) when is_atom(Name) -> true;
is_class_key(_) -> false.

is_list_of_class_keys(List) ->
    lists:all(fun is_class_key/1, List).

is_list_of_unary_funs(List) ->
    lists:all(fun(Fun) -> {arity, 1} =:= erlang:fun_info(Fun, arity) end, List).

to_class_key({_, _} = X) -> X;
to_class_key(Name) -> {std, Name}.

test_parse_behaviour() ->
    {error, wrong_meta_format} = parse(1),
    {error, wrong_meta_format} = parse({1, 1, 1}),
    {error, wrong_meta_format} = parse({type, 1, 1}),
    {error, wrong_meta_format} = parse({frame, 1, 1}).

test_parse_type() ->
    {error, wrong_meta_frame} = parse({type, int, 1}),
    {error, meta_contains_wrong_keys} = parse({type, int, [{a, 1}, {b, 2}, {c, 3}]}),
    {error, meta_is_empty} = parse({type, int, []}),
    {error, meta_is_empty} = parse({type, int, [{mixins, []}]}),
    {error, meta_is_empty} = parse({type, int, [{guards, []}]}),
    {error, meta_is_empty} = parse({type, int, [{mixins, []}, {guards, []}]}),
    {error, mixins_should_be_class_keys} = parse({type, int, [{mixins, [1, 2, 3]}]}),
    {error, guards_should_be_unary_funs} = parse({type, int, [{guards, [fun(1, 1) -> true end]}]}),
    {error, mode_wrong_arguments} = parse({type, int, [{guards, [fun is_integer/1]}, {mode, [{mixins, abc}]}]}),
    {error, mode_wrong_arguments} = parse({type, int, [{guards, [fun is_integer/1]}, {mode, [{guards, abc}]}]}),
    T1 = #type{name={std, int},
               mixins=[{std, number}],
               guards=[],
               mode=#tmode{guards=all,
                           mixins=all}} = parse({type, int, [{mixins, [number, number]}]}),
    T2 = T1#type{mode=T1#type.mode#tmode{guards=any}},
    T2 = parse({type, int, [{mixins, [number]}, {mode, [{guards, any}]}]}),
    IsInteger = fun is_integer/1,
    T3 = T2#type{guards=[IsInteger]},
    T3 = parse({type, int, [{mixins, [number]}, {guards, [IsInteger]}, {mode, [{guards, any}]}]}).

test_parse_frame() ->
    % TODO parse_field and parse (frame) tests
    ok.

test_unparse() ->
    % common behaviour
    {error, wrong_unparse_format} = unparse(1),
    % type
    IsInteger = fun is_integer/1,
    T1 = {type, test,
          [{guards, [IsInteger]}
          ]},
    T1P = parse(T1),
    T1U = unparse(T1P),
    true = T1 =/= T1U,
    T1U = unparse(parse(T1U)),
    {type, {std, test}, T1UMeta} = T1U,
    {[], [IsInteger], [{mixins, all}, {guards, all}], []} = jframe:take([mixins, guards, mode], T1UMeta),
    % frame
    More4 = fun(X) -> X > 4 end,
    Less10 = fun(X) -> X < 10 end,
    F1 = {frame, {ns, test},
          [{extend, [base]},
           {fields,
            [{a, [{is, string}]},
             {b, [{list_of, integer}, {optional, true}]},
             {c, [{is, integer}, {guards, [More4, Less10]}]}
            ]}
          ]},
    F1P = parse(F1),
    F1U = unparse(F1P),
    true = F1 =/= F1U,
    F1U = unparse(parse(F1U)),
    {frame, {ns, test}, F1UMeta} = F1U,
    {[{std, base}], F1UFields, []} = jframe:take([extend, fields], F1UMeta),
    {F1Ua, F1Ub, F1Uc, []} = jframe:take([a, b, c], F1UFields),
    Inspect = fun(Field) -> jframe:take([is, list_of, guards, optional], Field) end,
    {{std, string}, undefined, [], false, []} = Inspect(F1Ua),
    {undefined, {std, integer}, [], true, []} = Inspect(F1Ub),
    {{std, integer}, undefined, [More4, Less10], false, []} = Inspect(F1Uc).

test_misc() ->
    NS = std,
    Name = random_name,
    Key = {NS, Name},
    T = #type{name=Key},
    F = #frame{name=Key},
    O = {ignored, Key, ignored},
    NS = namespace(T),
    NS = namespace(F),
    NS = namespace(O),
    {error, unknown_format} = namespace(ignored),
    Name = name(T),
    Name = name(F),
    Name = name(O),
    {error, unknown_format} = name(ignored),    
    Key = key(T),
    Key = key(F),
    Key = key(O),
    {error, unknown_format} = key(ignored),
    type = kind(T),
    frame = kind(F),
    {error, unknown_record} = kind(O).
