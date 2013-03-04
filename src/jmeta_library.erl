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

-export([std/0]).

%%
%% API Functions
%%

test() ->
    [] = [R || {error, _} = R <- [jmeta_declaration:parse(T) || T <- std()]],
    {ok, done}.

std() ->
    [%----------------------TYPES---------------------
     atom(),
     % numeric
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
     list_of_frames(),
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
     empty_frame(),
     %---------------------FRAMES---------------------
     base()].

%%
%% Local Functions
%%

%----------------------TYPES---------------------

atom() ->
    {type, atom,
     [{guards, [fun is_atom/1]},
      {default, 0}]}.

% numeric

numeric() ->
    {type, numeric,
     [{guards, [fun is_number/1]},
      {default, 0}]}.

integer() ->
    {type, integer,
     [{guards, [fun is_integer/1]},
      {default, 0}]}.

bit() ->
    {type, bit,
     [{mixins, [integer]},
      {guards, [fun(X) -> X >= 0 andalso X =< 1 end]},
      {default, 0}]}.

float() ->
    {type, float,
     [{guards, [fun is_float/1]},
      {default, 0.0}]}.

% boolean

boolean() ->
    {type, boolean,
     [{guards, [fun is_boolean/1]},
      {default, false}]}.

% list

list() ->
    {type, list,
     [{guards, [fun is_list/1]},
      {default, []}]}.

non_empty_list() ->
    {type, non_empty_list,
     [{mixins, [list]},
      {guards, [fun(X) -> length(X) > 0 end]}]}.

set_keys() ->
    {type, set_keys,
     [{mixins, [list]},
      {guards, [fun(List) -> lists:all(fun is_bitstring/1, List) end]},
      {default, []}]}.

set_refs() ->
    {type, set_refs,
     [{mixins, [list]},
      {guards, [fun(List) -> lists:all(fun is_integer/1, List) end]},
      {default, []}]}.

list_of_frames() ->
    {type, list_of_frames,
     [{mixins, [list]},
      {guards, [fun(List) -> lists:all(fun jframe:is_frame/1, List) end]},
      {default, []}]}.

% tuple

tuple() ->
    {type, tuple,
     [{guards, [fun is_tuple/1]},
      {default, {}}]}.

% string and binary

string() ->
    {type, string,
     [{guards, [fun is_bitstring/1]},
      {default, <<>>}]}.

string128() ->
    {type, string128,
     [{mixins, [string]},
      {guards, [fun(X) -> size(X) =< 128 end]},
      {default, <<>>}]}.

binary() ->
    {type, binary,
     [{guards, [fun is_binary/1]},
      {default, <<>>}]}.

% datetime

iso8601() ->
    {type, iso8601,
     [{mixins, [string]},
      {guards, [fun(<<_:64/bitstring, "T", _:48/bitstring>>) -> true;
                   (_) -> false
                end]},
      {default, <<"20000101T000000">>}]}.

% FIXME
timestamp() ->
    {type, timestamp,
     [{guards, [fun({{Y, M, D}, {H, N, S}}) when is_float(S) ->
                        lists:all(fun is_integer/1, [Y, M, D, H, N]);
                   (_) -> false
                end]},
      {default, {{2000, 1, 1}, {0, 0, 0.0}}}]}.

timestamp_range() ->
    {type, timestamp_range,
     [{mixins, [list]},
      {guards, [fun([T1, T2]) ->
                        Check = fun(T) -> true = jmeta:is_type({timestamp, T}) end,
                        Check(T1), Check(T2),
                        T1 =< T2
                end]},
      {default, [{{2000, 1, 1}, {0, 0, 0.0}},
                 {{2001, 1, 1}, {0, 0, 0.0}}]}]}.

% frame

frame() ->
    {type, frame,
     [{guards, [fun jframe:is_frame/1]},
      {default, []}]}.

new_frame() ->
    {type, new_frame,
     [{mixins, [frame]},
      {guards, [fun jframe:is_new/1]},
      {default, []}]}.

empty_frame() ->
    {type, empty_frame,
     [{mixins, [frame]},
      {guards, [fun jframe:is_empty/1]},
      {default, []}]}.

%---------------------FRAMES---------------------

base() ->
    {frame, base,
      [{fields,
        [{id, [{is, integer}, {optional, true}]}
        ]}
      ]}.
