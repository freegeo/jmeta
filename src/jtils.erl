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

uid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:rand_bytes(16),
    list_to_binary(io_lib:format(<<"~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b">>,
                                 [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E])).

%%
%% Local Functions
%%

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
    % At some day this test can blow up my leg. But... you know.
    T1 =/= T2.
