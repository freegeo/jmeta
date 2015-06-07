%%% coding: UTF-8
%%%-------------------------------------------------------------------
%%% @author Konstantin Duman <said.dk@gmail.com>
%%% @copyright (C) 2015, FreeGeo
%%% Published under MIT license.
%%%
%%% @doc
%%% Contains unit tests.
%%% @end
%%%-------------------------------------------------------------------

-module(jmeta_declaration_tests).
-include_lib("eunit/include/eunit.hrl").

-include("jmeta.hrl").

parse_behaviour_test() ->
  {error, wrong_meta_format} = jmeta_declaration:parse(1),
  {error, wrong_meta_format} = jmeta_declaration:parse({1, 1, 1}),
  {error, wrong_meta_format} = jmeta_declaration:parse({type, 1, 1}),
  {error, wrong_meta_format} = jmeta_declaration:parse({frame, 1, 1}).

parse_type_test() ->
  {error, wrong_meta_frame} = jmeta_declaration:parse({type, int, 1}),
  {error, meta_contains_wrong_keys} = jmeta_declaration:parse({type, int, [{a, 1}, {b, 2}, {c, 3}]}),
  {error, meta_is_empty} = jmeta_declaration:parse({type, int, []}),
  {error, meta_is_empty} = jmeta_declaration:parse({type, int, [{mixins, []}]}),
  {error, meta_is_empty} = jmeta_declaration:parse({type, int, [{guards, []}]}),
  {error, meta_is_empty} = jmeta_declaration:parse({type, int, [{mixins, []}, {guards, []}]}),
  {error, mixins_should_be_class_keys} = jmeta_declaration:parse({type, int, [{mixins, [1, 2, 3]}]}),
  {error, guards_should_be_unary_funs} = jmeta_declaration:parse({type, int, [{guards, [fun(1, 1) -> true end]}]}),
  {error, mode_wrong_arguments} =
    jmeta_declaration:parse({type, int, [{guards, [fun is_integer/1]}, {mode, [{mixins, abc}]}]}),
  {error, mode_wrong_arguments} =
    jmeta_declaration:parse({type, int, [{guards, [fun is_integer/1]}, {mode, [{guards, abc}]}]}),
  T1 = #type{name = {std, int}, mixins = [{std, number}], guards = [], mode = #tmode{guards = all, mixins = all}} =
    jmeta_declaration:parse({type, int, [{mixins, [number, number]}]}),
  T2 = T1#type{mode = T1#type.mode#tmode{guards = any}},
  T2 = jmeta_declaration:parse({type, int, [{mixins, [number]}, {mode, [{guards, any}]}]}),
  IsInteger = fun is_integer/1,
  T3 = T2#type{guards = [IsInteger]},
  T3 = jmeta_declaration:parse({type, int, [{mixins, [number]}, {guards, [IsInteger]}, {mode, [{guards, any}]}]}).

parse_frame_test() ->
  {error, wrong_field_format} = jmeta_declaration:parse_field(1),
  {error, wrong_field_format} = jmeta_declaration:parse_field({1, 1}),
  {error, field_wrong_frame} = jmeta_declaration:parse_field({correct, 1}),
  {error, field_contains_wrong_keys} = jmeta_declaration:parse_field({correct, [{extra, 1}]}),
  {error, ambiguous_class} = jmeta_declaration:parse_field({correct, []}),
  {error, ambiguous_class} = jmeta_declaration:parse_field({correct, [{is, int}, {list_of, int}]}),
  {error, guards_should_be_unary_funs} =
    jmeta_declaration:parse_field({correct, [{is, int}, {guards, [fun(1, 1) -> true end]}]}),
  FI1D = {correct1, [{is, int}]},
  FI1 = #field{name = correct1, class = {is, {std, int}}, guards = [], optional = false} =
    jmeta_declaration:parse_field(FI1D),
  More4 = fun(X) -> X > 4 end,
  Less10 = fun(X) -> X < 10 end,
  FI2D = {correct2, [{list_of, {ns, test}}, {guards, [More4, Less10]}, {optional, true}]},
  FI2 = #field{name = correct2, class = {list_of, {ns, test}}, guards = [More4, Less10], optional = true} =
    jmeta_declaration:parse_field(FI2D),
  {error, wrong_meta_frame} = jmeta_declaration:parse({frame, test, 1}),
  {error, meta_contains_wrong_keys} = jmeta_declaration:parse({frame, test, [{extra, 1}]}),
  {error, extend_should_be_class_keys} = jmeta_declaration:parse({frame, test, [{extend, [1]}]}),
  {error, extend_should_be_class_keys} = jmeta_declaration:parse({frame, test, [{extend, [a, {ns, b}, 1]}]}),
  {error, fields_wrong_frame} = jmeta_declaration:parse({frame, test, [{fields, [1, 2, 3]}]}),
  {error, {incorrect_fields, [[{field, a}, {reason, field_wrong_frame}], [{field, b}, {reason, ambiguous_class}]]}} =
    jmeta_declaration:parse({frame, test, [{fields, [{a, 1}, {b, []}]}]}),
  #frame{name = {std, test}, extend = [{std, a}, {ns, b}], fields = [FI1, FI2], extended_fields = undefined} =
    jmeta_declaration:parse({frame, test, [{extend, [a, a, {ns, b}]}, {fields, [FI1D, FI2D]}]}).

unparse_test() ->
  % common behaviour
  {error, wrong_unparse_format} = jmeta_declaration:unparse(1),
  % type
  IsInteger = fun is_integer/1,
  T1 = {type, test, [{guards, [IsInteger]}]},
  T1P = jmeta_declaration:parse(T1),
  T1U = jmeta_declaration:unparse(T1P),
  true = T1 =/= T1U,
  T1U = jmeta_declaration:unparse(jmeta_declaration:parse(T1U)),
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
  F1P = jmeta_declaration:parse(F1),
  F1U = jmeta_declaration:unparse(F1P),
  true = F1 =/= F1U,
  F1U = jmeta_declaration:unparse(jmeta_declaration:parse(F1U)),
  {frame, {ns, test}, F1UMeta} = F1U,
  {[{std, base}], F1UFields, []} = jframe:take([extend, fields], F1UMeta),
  {F1Ua, F1Ub, F1Uc, []} = jframe:take([a, b, c], F1UFields),
  Inspect = fun(Field) -> jframe:take([is, list_of, guards, optional], Field) end,
  {{std, string}, undefined, [], false, []} = Inspect(F1Ua),
  {undefined, {std, integer}, [], true, []} = Inspect(F1Ub),
  {{std, integer}, undefined, [More4, Less10], false, []} = Inspect(F1Uc).

misc_test() ->
  NS = std,
  Name = random_name,
  Key = {NS, Name},
  T = #type{name = Key},
  F = #frame{name = Key},
  O = {ignored, Key, ignored},
  NS = jmeta_declaration:namespace(T),
  NS = jmeta_declaration:namespace(F),
  NS = jmeta_declaration:namespace(O),
  {error, unknown_format} = jmeta_declaration:namespace(ignored),
  Name = jmeta_declaration:name(T),
  Name = jmeta_declaration:name(F),
  Name = jmeta_declaration:name(O),
  {error, unknown_format} = jmeta_declaration:name(ignored),
  Key = jmeta_declaration:key(T),
  Key = jmeta_declaration:key(F),
  Key = jmeta_declaration:key(O),
  {error, unknown_format} = jmeta_declaration:key(ignored),
  type = jmeta_declaration:kind(T),
  frame = jmeta_declaration:kind(F),
  {error, unknown_record} = jmeta_declaration:kind(O).
