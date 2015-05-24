%% Author: Said
%% Mailto: said.dk@gmail.com
%% Created: 06.02.2013
%% Published under MIT license.
%% Description: provides some useful methods. ;)
-module(jtils).

%%
%% Include files
%%

-include("jtest.hrl").
-include("jtils.hrl").

%%
%% Exported Functions
%%

% test
-export([test/0]).

% api
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

%%
%% API Functions
%%

test() ->
    test_list(),
    test_misc(),
    jmeta_test:done().

% list

ulist([]) -> [];
ulist([H|T]) -> [H|ulist([X || X <- T, X =/= H])].

is_list_elements_unique(List) ->
    length(List) =:= length(lists:usort(List)).

% misc

random_list(Length, Allowed) ->
    GenerateString =
        fun(_, {Result, Seed}) ->
                {Pos, NextSeed} = random:uniform_s(length(Allowed), Seed),
                {[lists:nth(Pos, Allowed)|Result], NextSeed}
        end,
    {Reply, _} = lists:foldl(GenerateString, {"", now()}, lists:seq(1, Length)),
    Reply.

random_token(Lenght) ->
    random_list(Lenght, ?ALLOWED_CHARS).

jtoken() ->
    random_token(18).

shuffle(List) ->
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- List])].

hex(Bin) ->
   <<<<if V < 10 -> $0 + V; true -> $W + V end>> || <<V:4>> <= Bin>>.

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

test_list() ->
    A = [],
    B = [1, 2, 3],
    C = [3, 2, 1],
    D = [3, 2, 1, 1, 2, 3],
    E = [[1, 2], [2, 1]],
    [A, B, C, C, E] = lists:map(fun ulist/1, [A, B, C, D, E]),
    [true, true, true, false, true] = lists:map(fun is_list_elements_unique/1, [A, B, C, D, E]).

test_misc() ->
    ?EXCEPTION(error, function_clause, random_list(-1, "ABC")),
    ?EXCEPTION(error, function_clause, random_list(10, "")),
    "" = random_list(0, ""),
    "" = random_list(0, ?ALLOWED_CHARS),
    "AAAAAAAAAA" = random_list(10, "A"),
    [[], [], []] = random_list(3, [[]]),
    "" = random_token(0),
    10 = length(random_token(10)),
    T1 = jtoken(),
    18 = length(T1),
    T2 = shuffle(T1),
    18 = length(T2),
    true = T1 =/= T2, % not quite safe, but the possible problem might occur very rarely
    <<"616263646566">> = hex(<<"abcdef">>),
    <<"414243f3010d096f41">> = hex(<<"ABC", 243, 1, 13, 9, 111, $A>>),
    0 = length([X || X <- [uid() || _ <- lists:seq(1, 25000)], byte_size(X) =/= 36]).
