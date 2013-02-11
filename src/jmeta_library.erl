%% Author: Said
%% Created: 09.02.2013
%% Description: TODO: Add description to jmeta_library
-module(jmeta_library).

%%
%% Include files
%%

%%
%% Exported Functions
%%

% test
-export([test/0]).

-export([types/0, frames/0]).

%%
%% API Functions
%%

test() ->
    [] = [R || {error, _} = R <- [jmeta_declaration:parse_type(T) || T <- types()]],
    {ok, done}.

types() ->
    [% numeric
     numeric(),
     integer(),
     bit(),
     float(),
     % boolean
     boolean(),
     % list
     list(),
     non_empty_list(),
     set_keys(),
     set_refs(),
     % tuple
     tuple(),
     % string and binary
     string(),
     string128(),
     binary(),
     % datetime
     iso8601(),
     timestamp(),
     timestamp_range(),
     % frame
     frame(),
     new_frame(),
     empty_frame()].

frames() ->
    [].

%%
%% Local Functions
%%

% numeric

numeric() ->
    {type, numeric,
     [{constraints, [fun is_number/1]}]}.

integer() ->
    {type, integer,
     [{constraints, [fun is_integer/1]}]}.

bit() ->
    {type, bit,
     [{mixins, [integer]},
      {constraints, [fun(X) -> X >= 0 andalso X =< 1 end]}]}.

float() ->
    {type, float,
     [{constraints, [fun is_float/1]}]}.

% boolean

boolean() ->
    {type, boolean,
     [{constraints, [fun is_boolean/1]}]}.

% list

list() ->
    {type, list,
     [{constraints, [fun is_list/1]}]}.

non_empty_list() ->
    {type, non_empty_list,
     [{mixins, [list]},
      {constraints, [fun(X) -> length(X) > 0 end]}]}.

set_keys() ->
    {type, set_keys,
     [{mixins, [list]},
      {constraints, [fun(List) -> lists:all(fun erlang:is_bitstring/1, List) end]}]}.

set_refs() ->
    {type, set_refs,
     [{mixins, [list]},
      {constraints, [fun(List) -> lists:all(fun erlang:is_integer/1, List) end]}]}.

% tuple

tuple() ->
    {type, tuple,
     [{constraints, [fun is_tuple/1]}]}.

% string and binary

string() ->
    {type, string,
     [{constraints, [fun is_bitstring/1]}]}.

string128() ->
    {type, string128,
     [{mixins, [string]},
      {constraints, [fun(X) -> size(X) =< 128 end]}]}.

binary() ->
    {type, binary,
     [{constraints, [fun is_binary/1]}]}.

% datetime

iso8601() ->
    {type, iso8601,
     [{mixins, [string]},
      {constraints, [fun(<<_:64/bitstring, "T", _:48/bitstring>>) -> true;
                        (_) -> false
                     end]}
     ]}.

% FIXME
timestamp() ->
    {type, timestamp,
     [{constraints, [fun({{Y, M, D}, {H, N, S}}) when is_float(S) ->
                             lists:all(fun is_integer/1, [Y, M, D, H, N]);
                        (_) -> false
                     end]}
     ]}.

timestamp_range() ->
    {type, timestamp_range,
     [
      {mixins, [list]},
      {constraints, [fun([T1, T2]) ->
                             Check = fun(T) -> {ok, checked} = jmeta:is_type({timestamp, T}) end,
                             Check(T1), Check(T2),
                             T1 =< T2
                     end]}
     ]}.

% frame

frame() ->
    {type, frame,
     [{constraints, [fun(X) -> jframe:is_frame(X) end]}]}.

new_frame() ->
    {type, frame,
     [{mixins, [frame]},
      {constraints, [fun(X) -> jframe:is_new(X) end]}]}.

empty_frame() ->
    {type, frame,
     [{mixins, [frame]},
      {constraints, [fun(X) -> jframe:is_empty(X) end]}]}.
