%%% coding: UTF-8
%%%-------------------------------------------------------------------
%%% @author Konstantin Duman <said.dk@gmail.com>
%%% @copyright (C) 2015, FreeGeo
%%% Published under MIT license.
%%%
%%% @doc
%%% The heart of the jmeta library. Implements the validation logic.
%%% @end
%%%-------------------------------------------------------------------

-module(jmeta_check).

-include("jmeta.hrl").

-export([is/1, list_of/1]).

%% API Functions
-spec is({Type, any()}) -> true | {error, {not_a, {atom(), Type}}} when Type :: atom();
    ({{Namespace, Type}, any()}) -> true | {error, {not_a, {Namespace, Type}}} when Namespace :: atom(), Type :: atom().
is(X) -> jmeta_cache:for(fun() -> is_(X) end).

list_of(X) -> jmeta_cache:for(fun() -> list_of_(X) end).

%%
%% Local Functions
%%

is_({{_, _, _} = Key, RawData}) ->
  initial(Key, fun
    (type) -> is_type(Key, RawData);
    (frame) -> is_frame(Key, RawData)
  end);
is_({{Namespace, Name}, RawData}) when is_atom(Namespace) andalso is_atom(Name) ->
  is_({{Namespace, Name, []}, RawData});
is_({{Name, Params}, RawData}) when is_atom(Name) -> is_({{std, Name, Params}, RawData});
is_({Name, RawData}) when is_atom(Name) -> is_({{std, Name, []}, RawData});
is_(_) -> {error, wrong_check_format}.

list_of_({{list_of, Key}, RawData}) -> list_of(fun(Data) -> list_of_({Key, Data}) end, RawData);
list_of_({{_, _, _} = Key, RawData}) ->
  initial(Key, fun
    (type) -> list_of_type(Key, RawData);
    (frame) -> list_of_frame(Key, RawData)
  end);
list_of_({{Namespace, Name}, RawData}) when is_atom(Namespace) andalso is_atom(Name) ->
  list_of_({{Namespace, Name, []}, RawData});
list_of_({Name, RawData}) when is_atom(Name) -> list_of_({{std, Name}, RawData});
list_of_(_) -> {error, wrong_list_check_format}.

initial(Key, Selector) -> Selector(jmeta_declaration:kind(jmeta_cache:type_or_frame(Key))).

list_of(Checker, ListOfRawData) when is_list(ListOfRawData) ->
  Check = fun(RawData, {Result, Prev}) ->
    Current = Prev + 1,
    case Checker(RawData) of
      true -> {Result, Current};
      Error -> {[[Error, {pos, Current}] | Result], Current}
    end
  end,
  {Result, _} = lists:foldl(Check, {[], 0}, ListOfRawData),
  case Result of
    [] -> true;
    _ -> lists:reverse(Result)
  end;
list_of(_, _) -> {error, data_is_not_a_list}.

list_of_type(Key, ListOfRawData) -> list_of(fun(RawData) -> is_type(Key, RawData) end, ListOfRawData).
list_of_frame(Key, ListOfRawData) -> list_of(fun(RawData) -> is_frame(Key, RawData) end, ListOfRawData).

is_type({_, _, ParamsActual} = Key, RawData) ->
  Scenario = fun() ->
    #type{mode = #tmode{guards = MGuards, mixins = MMixins}, guards = Guards, params = ParamsDefault,
      mixins = Mixins} = jmeta_cache:type(Key),
    Params = jframe:extend(ParamsDefault, ParamsActual),
    true = lists:MMixins(fun(Mixin) -> true =:= is_type(Mixin, RawData) end, Mixins),
    true = lists:MGuards(fun(Guard) -> true =:= Guard(RawData, Params) end, Guards)
  end,
  jmeta_exception:safe_try(Scenario, fun(_) -> {error, {not_a, Key}} end).

is_frame(Key, RawData) ->
  case true =:= jframe:is_frame(RawData) of
    false -> {error, not_a_frame};
    true ->
      Fields = jmeta_cache:extended_fields(Key),
      {Violated, Rest} = lists:foldl(fun process_field/2, {[], RawData}, Fields),
      Result = [X || X <- [
        case Violated of
          [] -> skip;
          _ -> {violated, lists:reverse(Violated)}
        end,
        case Rest of
          [] -> skip;
          _ -> {extra_keys, jframe:keys(Rest)}
        end
      ], X =/= skip],
      case Result of
        [] -> true;
        _ -> {error, [{not_a, Key} | Result]}
      end
  end.

process_field({FieldName, #field{class = Class, guards = Guards, optional = Optional}}, {Violated, Frame}) ->
  {FieldData, Rest} = jframe:take(FieldName, Frame),
  Scenario = fun() ->
    case jframe:has(FieldName, Frame) of
      false ->
        case Optional =:= true of
          true -> true;
          false -> error({badmatch, missed})
        end;
      true -> true =
        case Class of
          {is, Key} -> is_({Key, FieldData});
          {list_of, Key} -> list_of_({Key, FieldData})
        end,
        jmeta_exception:safe_try(fun() -> lists:all(fun(Guard) -> true = Guard(FieldData, []) end, Guards) end,
          fun(_) -> error({badmatch, {Class, but_breaking_a_guard}}) end)
    end,
    {Violated, Rest}
  end,
  jmeta_exception:safe_try(Scenario, fun({error, {badmatch, Case}}) ->
      {[{FieldName, case Case of {error, E} -> E; _ -> Case end} | Violated], Rest}
  end).
