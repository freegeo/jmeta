%%% coding: UTF-8
%%%-------------------------------------------------------------------
%%% @author Konstantin Duman <said.dk@gmail.com>
%%% @copyright (C) 2015, FreeGeo
%%% Published under MIT license.
%%%
%%% @doc
%%% Provides some API of parse and analyze jmeta types/frames definitions.
%%% Use it if you want to implement some jmeta-based extensions.
%%% @end
%%%-------------------------------------------------------------------

-module(jmeta_declaration).

-include("jmeta.hrl").

%% api
-export([parse/1, unparse/1, namespace/1, name/1, key/1, kind/1]).

%% api internal
-export([parse_field/1, wrap_guards/1]).

%% API Functions
parse({type, Name, Meta}) when is_atom(Name) -> parse({type, {std, Name}, Meta});
parse({type, {Namespace, Name} = Key, Meta}) when is_atom(Namespace) andalso is_atom(Name) ->
  case jframe:new(Meta) of
    {error, wrong_frame} -> {error, wrong_meta_frame};
    _ ->
      {Mixins, RawGuards, ParamsRaw, ModeRaw, Rest} = jframe:take([{mixins, []}, {guards, []},
        {params, []}, {mode, [{mixins, all}, {guards, all}]}], Meta),
      case jframe:is_empty(Rest) of
        false -> {error, meta_contains_wrong_keys};
        true ->
          case jframe:new(ParamsRaw) of
            {error, wrong_frame} -> {error, params_wrong_frame};
            Params ->
              case Mixins =:= [] andalso RawGuards =:= [] andalso Params =:= [] of
                true -> {error, meta_is_empty};
                false ->
                  case is_list_of_class_keys(Mixins) of
                    false -> {error, mixins_should_be_class_keys};
                    true ->
                      case wrap_guards(RawGuards) of
                        {error, _} = E -> E;
                        Guards ->
                          case jframe:new(ModeRaw) of
                            {error, wrong_frame} -> {error, mode_wrong_frame};
                            Mode ->
                              {MixinsMode, GuardsMode} = jframe:find([{mixins, all}, {guards, all}], Mode),
                              AllOrAny = fun(all) -> true; (any) -> true; (_) -> false end,
                              case lists:all(AllOrAny, [MixinsMode, GuardsMode]) of
                                false -> {error, mode_wrong_arguments};
                                true ->
                                  #type{name = Key,
                                    mixins = jtils:ulist([to_class_key(M) || M <- Mixins]),
                                    guards = Guards,
                                    params = Params,
                                    mode = #tmode{mixins = MixinsMode, guards = GuardsMode}}
                              end
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
                  ParseField = fun({FieldName, _} = Field) ->
                    case parse_field(Field) of
                      {error, Reason} ->
                        {error, [{field, FieldName},
                        {reason, Reason}]};
                      R -> R
                    end
                  end,
                  ParsedFields = lists:map(ParseField, Fields),
                  case [Description || {error, Description} <- ParsedFields] of
                    [] ->
                      StdExtend = [to_class_key(E) || E <- Extend],
                      #frame{name = Key, extend = jtils:ulist(StdExtend), fields = ParsedFields};
                    FieldsErrors -> {error, {incorrect_fields, FieldsErrors}}
                  end
              end
          end
      end
  end;
parse(_) -> {error, wrong_meta_format}.

unparse(#type{name = Name, mixins = Mixins, guards = Guards, params = Params,
  mode = #tmode{mixins = MMixins, guards = MGuards}}) ->
  {type, Name,
    [{mixins, Mixins},
      {guards, Guards},
      {params, Params},
      {mode, [{mixins, MMixins}, {guards, MGuards}]}]};
unparse(#frame{name = Name, extend = Extend, fields = Fields}) ->
  {frame, Name,
    [{extend, Extend},
      {fields, [{FieldName, [Class, {guards, Guards}, {optional, Optional}]} ||
        #field{name = FieldName, class = Class, guards = Guards, optional = Optional} <- Fields]}]};
unparse(_) -> {error, wrong_unparse_format}.

namespace(#type{name = {Namespace, _}}) -> Namespace;
namespace(#frame{name = {Namespace, _}}) -> Namespace;
namespace({_, {Namespace, _}, _}) -> Namespace;
namespace(_) -> {error, unknown_format}.

name(#type{name = {_, Name}}) -> Name;
name(#frame{name = {_, Name}}) -> Name;
name({_, {_, Name}, _}) -> Name;
name(_) -> {error, unknown_format}.

key(#type{name = Key}) -> Key;
key(#frame{name = Key}) -> Key;
key({_, {_, _} = Key, _}) -> Key;
key(_) -> {error, unknown_format}.

kind(#type{}) -> type;
kind(#frame{}) -> frame;
kind(_) -> {error, unknown_record}.

parse_field({Name, MetaRaw}) when is_atom(Name) ->
  case jframe:new(MetaRaw) of
    {error, wrong_frame} -> {error, field_wrong_frame};
    Meta ->
      {Is, ListOf, RawGuards, Optional, Rest} = jframe:take([
        {is, []},
        {list_of, []},
        {guards, []},
        {optional, false}], Meta),
      case jframe:is_empty(Rest) of
        false -> {error, field_contains_wrong_keys};
        true ->
          case parse_class(Is, ListOf) of
            {error, _} = E -> E;
            Class ->
              case wrap_guards(RawGuards) of
                {error, _} = E -> E;
                Guards ->
                  #field{name = Name,
                    class = Class,
                    guards = Guards,
                    optional = Optional}
              end
          end
      end
  end;
parse_field(_) -> {error, wrong_field_format}.

%%
%% Local Functions
%%

parse_class(Is, []) ->
  case parse_class_name(Is) of
    {error, _} = E -> E;
    X -> {is, X}
  end;
parse_class([], ListOf) ->
  case parse_class_name(ListOf) of
    {error, _} = E -> E;
    X -> {list_of, X}
  end;
parse_class(_, _) -> {error, ambiguous_class}.

parse_class_name(Name) when is_atom(Name) -> parse_class_name({std, Name, []});
parse_class_name({Namespace, Name}) when is_atom(Namespace) andalso is_atom(Name) ->
  parse_class_name({Namespace, Name, []});
parse_class_name({Namespace, Name, RawParams}) when is_atom(Namespace) andalso is_atom(Name) ->
  case jframe:new(RawParams) of
    {error, _} -> {error, ambiguous_class};
    Params -> {Namespace, Name, Params}
  end;
parse_class_name(_) -> {error, ambiguous_class}.

is_class_key({Namespace, Name, {Parameter, _}}) when is_atom(Namespace) andalso is_atom(Name)
  andalso is_atom(Parameter) -> true;
is_class_key({Namespace, Name, Params}) when is_atom(Namespace) andalso is_atom(Name) andalso is_list(Params) ->
  jframe:is_frame(Params);
is_class_key({Namespace, Name}) when is_atom(Namespace) andalso is_atom(Name) -> true;
is_class_key({Name, {Parameter, _}}) when is_atom(Name) andalso is_atom(Parameter) -> true;
is_class_key({Name, Params}) when is_atom(Name) andalso is_list(Params) -> true;
is_class_key(Name) when is_atom(Name) -> true;
is_class_key(_) -> false.

is_list_of_class_keys(List) -> lists:all(fun is_class_key/1, List).

to_class_key({Namespace, Name}) when is_atom(Namespace) andalso is_atom(Name) -> {Namespace, Name, []};
to_class_key({Name, Params}) -> {std, Name, Params};
to_class_key(Name) ->
  case Name of
    {_, _, _} -> Name;
    _ -> {std, Name, []}
  end.

wrap_guards(RawGuards) when is_list(RawGuards) ->
  case lists:all(fun erlang:is_function/1, RawGuards) of
    false -> {error, guards_should_be_a_list_of_funs};
    true ->
      Guards =
        [case erlang:fun_info(Guard, arity) of
           {arity, 1} -> fun(Value, _) -> Guard(Value) end; % leads us to the known issue with unparse!
           {arity, 2} -> Guard;
           {arity, _} -> {error, guards_should_be_unary_or_binary_funs}
         end || Guard <- RawGuards],
      case [E || {error, _} = E <- Guards] of
        [] -> Guards;
        [E|_] -> E
      end
  end;
wrap_guards(_) -> {error, incorrect_guards_format}.
