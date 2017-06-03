%%%-------------------------------------------------------------------
%%% @author dom
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jun 2017 12:19
%%%-------------------------------------------------------------------
-module(erlcount).
-author("dom").
-behavior(application).

%% API
-export([start/2, stop/1]).

start(normal, _Args) ->
  erlcount_sup:start_link().

stop(_State) ->
  ok.
