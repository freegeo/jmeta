%% Author: Said
%% Created: 20.02.2013
%% Description: TODO: Add description to jmeta_cache
-module(jmeta_cache).

%%
%% Include files
%%

-include("jmeta.hrl").

-define(KEY, jmeta_cache).

%%
%% Exported Functions
%%

% test
-export([test/0]).

% external api
-export([for/1, reset/0]).

% internal api 
-export([type/1, frame/1, type_or_frame/1, extended_fields/1]).


%%
%% API Functions
%%

test() ->
    % TODO
    jmeta_test:done().

for(Scenario) ->
    case get(?KEY) of
        undefined ->
            put(?KEY, new()),
            R = Scenario(),
            %io:format(<<"Cache:~n~p~n~nResult:~n">>, [get(?KEY)]), % cache info
            erase(?KEY),
            R;
        _ -> Scenario()
    end.

reset() ->
    put(?KEY, new()).

type(Key) ->
    std(fun(Cache) -> type(Key, Cache) end).

frame(Key) ->
    std(fun(Cache) -> frame(Key, Cache) end).

type_or_frame(Key) ->
    std(fun(Cache) -> type_or_frame(Key, Cache) end).

extended_fields(Key) ->
    std(fun(Cache) -> extended_fields(Key, Cache) end).

%%
%% Local Functions
%%

new() ->
    []. % proplist for types and frames

std(Scenario) ->
    case Scenario(get(?KEY)) of
        {error, _} = E -> E;
        {ok, Value, Cache} ->
            put(?KEY, Cache),
            Value
    end.

lookup(Key, Cache, OnData) ->
    case proplists:get_value(Key, Cache) of
        undefined ->
            case jmeta_namespace:get(Key) of
                {error, _} = E -> E;
                Data -> OnData({jmeta_declaration:kind(Data), Data})
            end;
        Value -> {ok, Value, Cache}
    end.

type(Key, Cache) ->
    lookup(Key, Cache,
           fun({type, Data}) -> {ok, Data, [{Key, Data}|Cache]};
              ({frame, _}) -> {error, {Key, expected_type_but_frame_found}}
           end).

frame(Key, Cache) ->
    lookup(Key, Cache,
           fun({frame, Data}) -> {ok, Data, [{Key, Data}|Cache]};
              ({type, _}) -> {error, {Key, expected_frame_but_type_found}}
           end).

type_or_frame(Key, Cache) ->
    lookup(Key, Cache, fun({_, Data}) -> {ok, Data, [{Key, Data}|Cache]} end).

store(Key, Value, Cache) ->
    lists:keystore(Key, 1, Cache, {Key, Value}).

extended_fields(Key, Cache) ->
    extend(Key, jframe:new(), Cache).

% TODO inspect and optimize
extend(#frame{name=Key, extend=Frames, fields=Fields, extended_fields=undefined} = Frame, Result, Cache) ->
    Extend =
        fun(_, {error, _} = E) -> E;
           (F, {Extended, OldCache}) ->
                case extend(F, Extended, OldCache) of
                    {error, _} = E -> E;
                    {ok, Value, NewCache} -> {Value, NewCache}
                end
        end,
    case lists:foldl(Extend, {jframe:new(), Cache}, Frames) of
        {error, _} = E -> E;
        {ResultExtended, ResultCache} ->
            ExtendedFields = jframe:extend(ResultExtended, [{Field#field.name, Field} || Field <- Fields]),
            ResultFrame = Frame#frame{extended_fields=ExtendedFields},
            extend(ResultFrame, Result, store(Key, ResultFrame, ResultCache))
    end;
extend(#frame{extended_fields=ExtendedFields}, Result, Cache) ->
    {ok, jframe:extend(Result, ExtendedFields), Cache};
extend({_, _} = Key, Result, Cache) ->
    case frame(Key, Cache) of
        {error, _} = E -> E;
        {ok, Value, NewCache} -> extend(Value, Result, NewCache)
    end;
extend(_, _, _) -> {error, extend_fields_format}.
