%% Author: Said
%% Created: 06.02.2013
%% Description: TODO: Add description to jframe
-module(jframe).

%%
%% Include files
%%

-include("jtils.hrl").

%%
%% Exported Functions
%%

% test
-export([test/0]).

% api
-export([new/0, new/1, new/2,
         % transform
         find/2, store/2, take/2, delete/2, update/2,
         valuesmap/2, keysort/1, extend/2,
         % base
         keys/1, values/1,
         is_frame/1, is_new/1, is_empty/1,
         compare/3, sort/2]).

%%
%% API Functions
%%

test() ->
    [] = new(),
    {error, wrong_frame} = new([{id, 1}, {id, 2}]),
    [{a, 1}, {b, 2}, {c, 3}] = new(new([a, b, c], [1, 2, 3])),
    test_transform(),
    test_base(),
    {ok, done}.

new() ->
    [].

new(ListOfTuples) ->
    case is_frame(ListOfTuples) of
        true -> ListOfTuples;
        false -> {error, wrong_frame}
    end.

new(Keys, Values) ->
    lists:zip(Keys, Values).

% transform

find(Keys, Frame) when is_list(Keys) ->
    list_to_tuple([find(Key, Frame) || Key <- Keys]);
find({Key, Default}, Frame) ->
    case lists:keyfind(Key, 1, Frame) of
        false -> Default;
        {_, Value} -> Value
    end;
find(Key, Frame) -> find({Key, undefined}, Frame).

store({Key, _} = KV, Frame) ->
    lists:keystore(Key, 1, Frame, KV);
store(KVs, Frame) when is_list(KVs) ->
    lists:foldl(fun(KV, Acc) -> store(KV, Acc) end, Frame, KVs).

take(Keys, Frame) when is_list(Keys) ->
    Lookup =
        fun(Key, {Values, CurrentFrame}) ->
                {V, NewFrame} = take(Key, CurrentFrame),
                {[V|Values], NewFrame}
        end,
    {Result, Rest} = lists:foldl(Lookup, {[], Frame}, Keys),
    list_to_tuple(lists:reverse(Result) ++ [Rest]);
take({Key, Default}, Frame) ->
    case lists:keytake(Key, 1, Frame) of
        false -> {Default, Frame};
        {value, {_, Value}, NewFrame} -> {Value, NewFrame}
    end;
take(Key, Frame) -> take({Key, undefined}, Frame).

delete([], Frame) -> Frame;
delete([Key|Keys], Frame) -> delete(Keys, delete(Key, Frame));
delete(Key, Frame) -> lists:keydelete(Key, 1, Frame).

update({Key, Fun}, Frame) ->
    store({Key, Fun(find(Key, Frame))}, Frame);
update(KFs, Frame) when is_list(KFs) ->
    lists:foldl(fun(KF, Acc) -> update(KF, Acc) end, Frame, KFs).

valuesmap(Fun, [{_, _}|_] = Frame) -> lists:keymap(Fun, 2, Frame);
valuesmap(Fun, Frames) -> [valuesmap(Fun, Frame) || Frame <- Frames].

keysort([{_, _}|_] = Frame) -> lists:keysort(1, Frame);
keysort(Frames) -> lists:map(fun keysort/1, Frames).

extend(Target, []) -> Target;
extend(Target, [Frame|Frames]) -> extend(store(Frame, Target), Frames).

% base

keys(Frame) ->
    {Keys, _} = lists:unzip(Frame),
    Keys.

values(Frame) ->
    {_, Values} = lists:unzip(Frame),
    Values.

is_frame(Frame) when is_list(Frame) ->
    lists:all(fun({_, _}) -> true; (_) -> false end, Frame) andalso
        jtils:is_list_elements_unique(keys(Frame));
is_frame(_) -> false.

is_new(Frame) ->
    undefined =:= find(id, Frame).

is_empty(Frame) ->
    [] =:= Frame.

compare(_, _, []) -> false;
compare(FrameA, FrameB, [{Field, Method}|CompareFrame]) ->
    case find(Field, FrameA) =:= find(Field, FrameB) of
        true -> compare(FrameA, FrameB, CompareFrame);
        false ->
            Comparer =
                case Method of
                    '>' -> fun erlang:'>'/2;
                    '<' -> fun erlang:'<'/2
                end,
            Comparer(find(Field, FrameA), find(Field, FrameB))
    end.

sort(CompareFrame, ListOfFrames) ->
    lists:sort(fun(A, B) -> compare(A, B, CompareFrame) end, ListOfFrames).

%%
%% Local Functions
%%

test_transform() ->
    F10 = [{id, 10}] = store({id, 10}, new()),
    10 = find(id, F10),
    {} = find([], F10),
    F11 = [{id, 12}] = store({id, 12}, F10),
    F12 = store([{name, <<"Kostya">>}, {age, 25}], F11),
    {12, 25} = find([id, age], F12),
    {<<"Kostya">>, undefined, m} = find([name, gender, {gender, m}], F12),
    F13 = store({gender, m}, F12),
    m = find(gender, F13),
    {F13} = take([], F13),
    {undefined, F13} = take(lastname, F13),
    {<<"Duman">>, F13} = take({lastname, <<"Duman">>}, F13),
    {<<"Kostya">>, m, F14} = take([name, gender], F13),
    {12, undefined, 25, undefined} = find([id, name, age, gender], F14),
    F14 = delete(gender, F14),
    [{id, 12}] = delete(age, F14),
    [] = delete([id, age], F14),
    35 = find(age, update({age, fun(Age) -> Age + 10 end}, F14)),
    Inc = fun(X) -> X + 1 end,
    {13, 26} = find([id, age], update([{id, Inc}, {age, Inc}], F14)),
    F14 = extend(F14, []),
    F15 = extend(F10, [F11, F12, F13, F14]),
    {12, <<"Kostya">>, 25, m} = find([id, name, age, gender], F15),
    SF15 = [{age, 25}, {gender, m}, {id, 12}, {name, <<"Kostya">>}] = keysort(F15),
    [SF15, SF15, SF15] = keysort([F15, F15, F15]),
    [] = valuesmap(Inc, []),
    F16 = keysort(delete([gender, name], F15)),
    F17 = [{age, 26}, {id, 13}] = valuesmap(Inc, F16),
    [F17, F17, F17] = valuesmap(Inc, [F16, F16, F16]).

test_base() ->
    F10 = new(),
    F11 = store({name, <<>>}, F10),
    F12 = keysort(store({id, 1}, F11)),
    [true, true, false] = lists:map(fun is_new/1, [F10, F11, F12]),
    [true, false, false] = lists:map(fun is_empty/1, [F10, F11, F12]),
    [id, name] = keys(F12),
    [1, <<>>] = values(F12),
    [true, true, false, false] = lists:map(fun is_frame/1, [new(), F12, F12 ++ F12, [1, 2, 3]])
    % TODO compare/3, sort/2
    .