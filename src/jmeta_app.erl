%%% coding: UTF-8
%%%-------------------------------------------------------------------
%%% @author Konstantin Duman <said.dk@gmail.com>
%%% @copyright (C) 2015, FreeGeo
%%% Published under MIT license.
%%%
%%% @doc
%%% Application boilerplate.
%%% @end
%%%-------------------------------------------------------------------

-module(jmeta_app).

-behaviour(application).

-export([start/2, stop/1]).

%% Callbacks
start(_, _) ->
  case jmeta_sup:start_link() of
    {ok, _} = P -> P;
    E -> E
  end.

stop(_) -> ok.
