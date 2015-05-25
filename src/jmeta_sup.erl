%%% coding: UTF-8
%%%-------------------------------------------------------------------
%%% @author Konstantin Duman <said.dk@gmail.com>
%%% @copyright (C) 2015, FreeGeo
%%% Published under MIT license.
%%%
%%% @doc
%%% Supervisor specific boilerplate.
%%% @end
%%%-------------------------------------------------------------------

-module(jmeta_sup).

-behaviour(supervisor).

-include("jtils.hrl").

-export([start_link/0]).

-export([init/1]).

start_link() ->
  Info = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  lists:foreach(fun jmeta_namespace:add/1, jmeta_library:std()),
  Info.

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) -> {ok, {{simple_one_for_one, 1, 1}, [?WORKER(jmeta_namespace)]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

