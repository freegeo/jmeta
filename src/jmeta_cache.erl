%%% coding: UTF-8
%%%-------------------------------------------------------------------
%%% @author Konstantin Duman <said.dk@gmail.com>
%%% @copyright (C) 2015, FreeGeo
%%% Published under MIT license.
%%%
%%% @doc
%%% The cache implementation.
%%% @end
%%%-------------------------------------------------------------------

-module(jmeta_cache).

-include("jmeta.hrl").

-define(KEY, jmeta_cache).

%% external api
-export([for/1, reset/0]).

%% internal api
-export([type/1, frame/1, type_or_frame/1, extended_fields/1]).

%% API Functions
-spec for(fun(() -> X)) -> X when X :: any().
for(Scenario) ->
  case get(?KEY) of
    undefined ->
      put(?KEY, jframe:new()),
      R = Scenario(),
      %io:format(<<"Cache:~n~p~n~nResult:~n">>, [get(?KEY)]), % cache info
      erase(?KEY),
      R;
    _ -> Scenario()
  end.

-spec reset() -> any().
reset() -> put(?KEY, jframe:new()).

type(Key) -> std(fun(Cache) -> type(Key, Cache) end).
frame(Key) -> std(fun(Cache) -> frame(Key, Cache) end).
type_or_frame(Key) -> std(fun(Cache) -> type_or_frame(Key, Cache) end).
extended_fields(Key) -> std(fun(Cache) -> extended_fields(Key, Cache) end).

%%
%% Local Functions
%%

std(Scenario) ->
  {Result, Value, Cache} = Scenario(get(?KEY)),
  case Result of
    new -> put(?KEY, Cache);
    old -> dont_care
  end,
  Value.

extract_key({_, _} = Key) -> Key;
extract_key({Namespace, Name, _}) -> {Namespace, Name}.

lookup(KeySpec, Cache, OnData) ->
  Key = extract_key(KeySpec),
  case proplists:get_value(Key, Cache) of
    undefined ->
      case jmeta_namespace:get(Key) of
        {error, Reason} -> jmeta_exception:new(Reason);
        Data -> OnData({jmeta_declaration:kind(Data), Data})
      end;
    Value -> {old, Value, Cache}
  end.

type(Key, Cache) ->
  lookup(Key, Cache, fun
    ({type, Data}) -> {new, Data, [{extract_key(Key), Data} | Cache]};
    ({frame, _}) -> jmeta_exception:new({Key, expected_type_but_frame_found})
  end).

frame(Key, Cache) ->
  lookup(Key, Cache, fun
    ({frame, Data}) -> {new, Data, [{extract_key(Key), Data} | Cache]};
    ({type, _}) -> jmeta_exception:new({Key, expected_frame_but_type_found})
  end).

type_or_frame(Key, Cache) -> lookup(Key, Cache, fun({_, Data}) -> {new, Data, [{extract_key(Key), Data} | Cache]} end).
store(Key, Value, Cache) -> lists:keystore(Key, 1, Cache, {Key, Value}).
extended_fields(Key, Cache) -> extend(dont_care, Key, jframe:new(), Cache).

extend(_, #frame{name = Key, extend = Frames, fields = Fields, extended_fields = undefined} = Frame, Result, Cache) ->
  Extend = fun(F, {_, Extended, OldCache}) -> extend(dont_care, F, Extended, OldCache) end,
  {_, ResultExtended, ResultCache} = lists:foldl(Extend, {dont_care, jframe:new(), Cache}, Frames),
  ExtendedFields = jframe:extend(ResultExtended, [{Field#field.name, Field} || Field <- Fields]),
  ResultFrame = Frame#frame{extended_fields = ExtendedFields},
  extend(new, ResultFrame, Result, store(Key, ResultFrame, ResultCache));
extend(KeepCache, #frame{extended_fields = ExtendedFields}, Result, Cache) ->
  {KeepCache, jframe:extend(Result, ExtendedFields), Cache};
extend(_, {_, _, _} = Key, Result, Cache) ->
  {KeepCache, Value, NewCache} = frame(Key, Cache),
  extend(KeepCache, Value, Result, NewCache);
extend(_, _, _, _) ->
  jmeta_exception:new(extend_fields_format).
