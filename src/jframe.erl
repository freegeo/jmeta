%%% coding: UTF-8
%%%-------------------------------------------------------------------
%%% @author Konstantin Duman <said.dk@gmail.com>
%%% @copyright (C) 2015, FreeGeo
%%% Published under MIT license.
%%%
%%% @doc
%%% Provides methods to create and transform frames.
%%% @end
%%%-------------------------------------------------------------------

-module(jframe).

-include("jtils.hrl").

%% api
-export([new/0, new/1, new/2,
  % transform
  find/2, store/2, take/2, delete/2, update/2,
  valuesmap/2, keysort/1, extend/2,
  % base
  keys/1, values/1, has/2,
  is_frame/1, is_new/1, is_empty/1,
  is_key_identical/2,
  % compare & sort
  compare/3, sort/2,
  % diff
  diff/2]).

-type empty_frame() :: [].
-type field() :: {atom(), any()}.
-type frame() :: [field()].
-type list_of_frames() :: [frame()].
-type transformation() :: fun((any()) -> any()).

-export_type([empty_frame/0, field/0, frame/0, list_of_frames/0, transformation/0]).

-spec new() -> empty_frame().
new() -> [].

-spec new({Key, Value}) -> [{Key, Value}] when Key :: atom(), Value :: any();
    (Frame) -> Frame | {error, wrong_frame} when Frame :: frame().
new({Key, _} = KV) when is_atom(Key) -> [KV];
new(ListOfTuples) ->
  case is_frame(ListOfTuples) of
    true -> ListOfTuples;
    false -> {error, wrong_frame}
  end.

-spec new([atom()], [any()]) -> frame().
new(Keys, Values) -> lists:zip(Keys, Values).

%% transform
-spec find([atom() | field()], frame()) -> tuple();
    (field(), frame()) -> any();
    (atom(), frame()) -> any() | undefined.
find(Keys, Frame) when is_list(Keys) -> list_to_tuple([find(Key, Frame) || Key <- Keys]);
find({Key, Default}, Frame) ->
  case lists:keyfind(Key, 1, Frame) of
    false -> Default;
    {_, Value} -> Value
  end;
find(Key, Frame) -> find({Key, undefined}, Frame).

-spec store(field(), frame()) -> frame();
    (frame(), frame()) -> frame().
store({Key, _} = KV, Frame) -> lists:keystore(Key, 1, Frame, KV);
store(KVs, Frame) when is_list(KVs) -> lists:foldl(fun(KV, Acc) -> store(KV, Acc) end, Frame, KVs).

-spec take([], Frame) -> {Frame} when Frame :: frame();
    ([atom() | field(), ...], frame()) -> tuple();
    (field(), frame()) -> {any(), frame()};
    (atom(), frame()) -> {undefined | any(), frame()}.
take(Keys, Frame) when is_list(Keys) ->
  Lookup = fun(Key, {Values, CurrentFrame}) ->
    {V, NewFrame} = take(Key, CurrentFrame),
    {[V | Values], NewFrame}
  end,
  {Result, Rest} = lists:foldl(Lookup, {[], Frame}, Keys),
  list_to_tuple(lists:reverse(Result) ++ [Rest]);
take({Key, Default}, Frame) ->
  case lists:keytake(Key, 1, Frame) of
    false -> {Default, Frame};
    {value, {_, Value}, NewFrame} -> {Value, NewFrame}
  end;
take(Key, Frame) -> take({Key, undefined}, Frame).

-spec delete([atom()], frame()) -> frame();
    (atom(), frame()) -> frame().
delete([], Frame) -> Frame;
delete([Key | Keys], Frame) -> delete(Keys, delete(Key, Frame));
delete(Key, Frame) -> lists:keydelete(Key, 1, Frame).

- spec update({atom() | field(), transformation()}, frame()) -> frame();
    ([{atom() | field(), transformation()}], frame()) -> frame().
update({Key, Fun}, Frame) -> store({Key, Fun(find(Key, Frame))}, Frame);
update(KFs, Frame) when is_list(KFs) -> lists:foldl(fun(KF, Acc) -> update(KF, Acc) end, Frame, KFs).

-spec valuesmap(transformation(), frame()) -> frame();
    (transformation(), list_of_frames()) -> list_of_frames().
valuesmap(Fun, [{_, _} | _] = Frame) -> lists:keymap(Fun, 2, Frame);
valuesmap(Fun, Frames) -> [valuesmap(Fun, Frame) || Frame <- Frames].

-spec keysort(frame()) -> frame();
    (list_of_frames()) -> list_of_frames().
keysort([{_, _} | _] = Frame) -> lists:keysort(1, Frame);
keysort(Frames) -> lists:map(fun keysort/1, Frames).

- spec extend(Frame, []) -> Frame when Frame :: frame();
    (frame(), list_of_frames()) -> frame().
extend(Target, []) -> Target;
extend(Target, [Frame | Frames]) -> extend(store(Frame, Target), Frames).

%% base
-spec keys(frame()) -> [atom()].
keys(Frame) -> {Keys, _} = lists:unzip(Frame), Keys.

-spec values(frame()) -> [any()].
values(Frame) -> {_, Values} = lists:unzip(Frame), Values.

-spec has(atom() | [atom()], frame()) -> boolean().
has(Keys, Frame) when is_list(Keys) -> lists:all(fun(Key) -> has(Key, Frame) end, Keys);
has(Key, Frame) -> lists:keymember(Key, 1, Frame).

-spec is_frame(any()) -> boolean().
is_frame(Frame) when is_list(Frame) ->
  lists:all(fun({Key, _}) -> is_atom(Key); (_) -> false end, Frame) andalso
    jtils:is_list_elements_unique(keys(Frame));
is_frame(_) -> false.

-spec is_new(frame()) -> boolean().
is_new(Frame) -> undefined =:= find(id, Frame).

-spec is_empty(frame()) -> boolean().
is_empty(Frame) -> [] =:= Frame.

-spec is_key_identical(frame(), frame()) -> boolean().
is_key_identical(FrameA, FrameB) -> keys(FrameA) =:= keys(FrameB).

%% compare & sort
-spec compare(frame(), frame(), []) -> 'false';
    (frame(), frame(), frame()) -> boolean().
compare(_, _, []) -> false;
compare(FrameA, FrameB, [{Field, Method} | CompareFrame]) ->
  case find(Field, FrameA) =:= find(Field, FrameB) of
    true -> compare(FrameA, FrameB, CompareFrame);
    false ->
      Comparer =
        case Method of
          '>' -> fun erlang:'>'/2;
          '<' -> fun erlang:'<'/2;
          Fun when is_function(Fun, 2) -> Fun;
          NestedCompareFrame when is_list(NestedCompareFrame) -> fun(A, B) -> compare(A, B, NestedCompareFrame) end
        end,
      Comparer(find(Field, FrameA), find(Field, FrameB))
  end.

-spec sort(frame(), [frame()]) -> [frame()].
sort(CompareFrame, ListOfFrames) -> lists:sort(fun(A, B) -> compare(A, B, CompareFrame) end, ListOfFrames).

%% diff
-spec diff(frame(), frame()) -> frame().
diff(Previous, Next) ->
  P = Previous -- Next, N = Next -- Previous,
  PKeys = keys(P),
  Diff = fun(Key, Acc) ->
    Result =
      case has(Key, N) of
        false -> removed;
        true ->
          PV = find(Key, P), NV = find(Key, N),
          case is_frame(PV) andalso is_frame(NV) of
            false ->
              case PV =:= NV of
                true -> equal;
                false -> non_equal
              end;
            true ->
              case diff(PV, NV) of
                [] -> equal;
                R -> R
              end
          end
        end,
      case Result =:= equal of
        true -> Acc;
        false -> [{Key, Result} | Acc]
      end
    end,
  lists:foldl(Diff, [{K, added} || K <- keys(N) -- PKeys], PKeys).
