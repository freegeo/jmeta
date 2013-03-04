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
         random_list/2,
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
    18 = length(jtoken()).
