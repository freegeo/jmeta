%%% coding: UTF-8
%%%-------------------------------------------------------------------
%%% @author Konstantin Duman <said.dk@gmail.com>
%%% @copyright (C) 2015, FreeGeo
%%% Published under MIT license.
%%%
%%% @doc
%%% The Type Library. Defines a set of common types.
%%% @end
%%%-------------------------------------------------------------------

-module(jmeta_library).

%% api
-export([std/0]).

%% API Functions
std() ->
  [%----------------------TYPES---------------------
    atom(),
    null(),
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
    date_(),
    timea(),
    timeb(),
    timestampa(),
    timestampb(),
    timestamp_rangea(),
    timestamp_rangeb(),
    % frame
    frame(),
    new_frame(),
    empty_frame(),
    %---------------------FRAMES---------------------
    base()].

%%
%% Local Functions
%%

%%----------------------TYPES---------------------

atom() ->
  {type, atom,
    [{guards, [fun is_atom/1]}
    ]}.

null() ->
  {type, null,
    [{guards, [fun(null) -> true end]}
    ]}.

%% numeric
numeric() ->
  {type, numeric,
    [{guards, [fun is_number/1]}
    ]}.

integer() ->
  {type, integer,
    [{guards, [fun is_integer/1]}
    ]}.

bit() ->
  {type, bit,
    [{guards, [fun(X) when is_integer(X) -> X >= 0 andalso X =< 1 end]}
    ]}.

float() ->
  {type, float,
    [{guards, [fun is_float/1]}
    ]}.

%% boolean
boolean() ->
  {type, boolean,
    [{guards, [fun is_boolean/1]}
    ]}.

%% list
list() ->
  {type, list,
    [{guards, [fun is_list/1]}
    ]}.

non_empty_list() ->
  {type, non_empty_list,
    [{guards, [fun(X) when is_list(X) -> length(X) > 0 end]}
    ]}.

set_keys() ->
  {type, set_keys,
    [{guards, [fun(List) when is_list(List) -> lists:all(fun is_bitstring/1, List) end]}
    ]}.

set_refs() ->
  {type, set_refs,
    [{guards, [fun(List) when is_list(List) -> lists:all(fun is_integer/1, List) end]}
    ]}.

list_of_frames() ->
  {type, list_of_frames,
    [{guards, [fun(List) when is_list(List) -> lists:all(fun jframe:is_frame/1, List) end]}
    ]}.

%% tuple
tuple() ->
  {type, tuple,
    [{guards, [fun is_tuple/1]}
    ]}.

%% string and binary
string() ->
  {type, string,
    [{guards, [fun is_bitstring/1]}
    ]}.

string128() ->
  {type, string128,
    [{guards, [fun(X) when is_bitstring(X) -> size(X) =< 128 end]}
    ]}.

%% its same as string
binary() ->
  {type, binary,
    [{guards, [fun is_binary/1]}
    ]}.

%% datetime
date_() ->
  {type, date,
    [{guards, [fun calendar:valid_date/1]}
    ]}.

timea() ->
  {type, 'time.a',
    [{guards, [fun({H, M, S}) when is_integer(H) andalso is_integer(M) andalso is_integer(S) ->
      H >= 0 andalso H < 24 andalso
        M >= 0 andalso M < 60 andalso
        S >= 0 andalso S < 60
    end]}
    ]}.

timeb() ->
  {type, 'time.b',
    [{guards, [fun({H, M, S}) when is_float(S) ->
      true = jmeta:is({'time.a', {H, M, trunc(S)}})
    end]}
    ]}.

timestamp_guard(TimeVersion) ->
  fun({Date, Time}) ->
    true = jmeta:is({date, Date}),
    true = jmeta:is({TimeVersion, Time})
  end.

timestampa() ->
  {type, 'timestamp.a',
    [{guards, [timestamp_guard('time.a')]}
    ]}.

timestampb() ->
  {type, 'timestamp.b',
    [{guards, [timestamp_guard('time.b')]}
    ]}.

timestamp_range_guard(TimestampVersion) ->
  fun([T1, T2] = Range) ->
    true = jmeta:list_of({TimestampVersion, Range}),
    T1 =< T2
  end.

timestamp_rangea() ->
  {type, 'timestamp_range.a',
    [{guards, [timestamp_range_guard('timestamp.a')]}
    ]}.

timestamp_rangeb() ->
  {type, 'timestamp_range.b',
    [{guards, [timestamp_range_guard('timestamp.b')]}
    ]}.

%% frame
frame() ->
  {type, frame,
    [{guards, [fun jframe:is_frame/1]}
    ]}.

new_frame() ->
  {type, new_frame,
    [{mixins, [frame]},
      {guards, [fun jframe:is_new/1]}
    ]}.

empty_frame() ->
  {type, empty_frame,
    [{mixins, [frame]},
      {guards, [fun jframe:is_empty/1]}
    ]}.

%%---------------------FRAMES---------------------

base() ->
  {frame, base,
    [{fields,
      [{id, [{is, integer}, {optional, true}]}
      ]}
    ]}.
