%%% coding: UTF-8
%%%-------------------------------------------------------------------
%%% @author Konstantin Duman <said.dk@gmail.com>
%%% @copyright (C) 2015, FreeGeo
%%% Published under MIT license.
%%%
%%% @doc
%%% Provides some useful methods.
%%% @end
%%%-------------------------------------------------------------------

-module(jtils).

-include("jtils.hrl").

%% api
-export([% list
  ulist/1,
  is_list_elements_unique/1,
  % misc
  random_list/2,
  random_token/1,
  jtoken/0,
  shuffle/1,
  hex/1,
  uid/0]).

%% list
ulist([]) -> [];
ulist([H | T]) -> [H | ulist([X || X <- T, X =/= H])].

is_list_elements_unique(List) -> length(List) =:= length(lists:usort(List)).

%% misc
random_list(Length, Allowed) ->
  GenerateString =
    fun(_, {Result, Seed}) ->
      {Pos, NextSeed} = random:uniform_s(length(Allowed), Seed),
      {[lists:nth(Pos, Allowed) | Result], NextSeed}
    end,
  {Reply, _} = lists:foldl(GenerateString, {"", now()}, lists:seq(1, Length)),
  Reply.

random_token(Lenght) -> random_list(Lenght, ?ALLOWED_CHARS).
jtoken() -> random_token(18).
shuffle(List) -> [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- List])].
hex(Bin) -> <<<<if V < 10 -> $0 + V; true -> $W + V end>> || <<V:4>> <= Bin>>.

uid() ->
  <<A:32, B:16, C:16, D:16, E:48>> = crypto:rand_bytes(16),
  <<(pad_right(integer_to_binary(A, 16), $0, 8))/binary, $-,
  (pad_right(integer_to_binary(B, 16), $0, 4))/binary, $-,
  (integer_to_binary(C band 16#0fff bor 16#4000, 16))/binary, $-,
  (integer_to_binary(D band 16#3fff bor 16#8000, 16))/binary, $-,
  (pad_right(integer_to_binary(E, 16), $0, 12))/binary>>.

%%
%% Local Functions
%%

% unsafe and very simple, only for internal purposes
pad_right(Bitstring, _, Number) when byte_size(Bitstring) >= Number -> Bitstring;
pad_right(Bitstring, Char, Number) ->
  <<(binary:copy(<<Char>>, Number - byte_size(Bitstring)))/binary, Bitstring/binary>>.
