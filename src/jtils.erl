%% Author: Said
%% Created: 06.02.2013
%% Description: TODO: Add description to jtils
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
         random_string/2,
         random_token/1,
         jtoken/0]).

%%
%% API Functions
%%

test() ->
    test_list(),
    test_misc(),
    {ok, done}.

% list

ulist([]) -> [];
ulist([H|T]) -> [H|ulist([X || X <- T, X =/= H])].

is_list_elements_unique(List) ->
    length(List) =:= length(lists:usort(List)).

% misc

random_string(Length, AllowedChars) ->
    GenerateString =
        fun(_, {Result, Seed}) ->
                {Pos, NextSeed} = random:uniform_s(length(AllowedChars), Seed),
                {[lists:nth(Pos, AllowedChars)|Result], NextSeed}
        end,
    {Reply, _} = lists:foldl(GenerateString, {"", now()}, lists:seq(1, Length)),
    Reply.

random_token(Lenght) ->
    random_string(Lenght, ?ALLOWED_CHARS).

jtoken() ->
    random_token(18).

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
    ?EXCEPTION(error, function_clause, random_string(-1, "ABC")),
    ?EXCEPTION(error, function_clause, random_string(10, "")),
    "" = random_string(0, ""),
    "" = random_string(0, ?ALLOWED_CHARS),
    "AAAAAAAAAA" = random_string(10, "A"),
    "" = random_token(0),
    10 = length(random_token(10)),
    18 = length(jtoken()).
