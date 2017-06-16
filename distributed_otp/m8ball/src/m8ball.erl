%%%-------------------------------------------------------------------
%%% @author dom
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jun 2017 21:27
%%%-------------------------------------------------------------------
-module(m8ball).
-author("dom").
-behavior(application).

-export([start/2, stop/1]).
-export([ask/1]).

%%%%%%%%%%%%%%%%%
%%% CALLBACKS %%%
%%%%%%%%%%%%%%%%%

start(normal, []) ->
  m8ball_sup:start_link();
start({takeover, _OtherNode}, []) ->
  m8ball_sup:start_link().

stop(_State) ->
  ok.

%%%%%%%%%%%%%%%%%
%%% INTERFACE %%%
%%%%%%%%%%%%%%%%%

ask(Question) ->
  m8ball_server:ask(Question).