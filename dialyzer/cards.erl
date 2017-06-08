%%%-------------------------------------------------------------------
%%% @author dom
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jun 2017 10:48
%%%-------------------------------------------------------------------
-module(cards).
-author("dom").

%% API
-export([kind/1, main/0]).

-type suit() :: spades | clubs | hearts | diamonds.
-type value() :: 1..10 | j | q | k.
-type card() :: {suit(), value()}.

-export_type([card/0]).

-spec kind(card()) -> face | number.
kind({_, A}) when A >=1, A =< 10 -> number;
kind(_) -> face.

main() ->
  number = kind({spades, 7}),
  face   = kind({hearts, k}),
  number = kind({rubies, 4}),
  face   = kind({clubs, q}).
