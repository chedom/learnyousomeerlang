%%%-------------------------------------------------------------------
%%% @author dom
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jun 2017 12:26
%%%-------------------------------------------------------------------
-module(fifo_types).
-author("dom").

%% API
-export([new/0, push/2, pop/1, empty/1]).
-export([test/0]).

-type queue(Type) :: {fifo, list(Type), list(Type)}.
-export_type([queue/1]).

-spec new() -> {fifo, [], []}.
new() -> {fifo, [], []}.

-spec push({fifo, In::list(), Out:: list()}, term()) -> {fifo, list(), list()}.
push({fifo, In, Out}, X) -> {fifo, [X | In], Out}.

-spec pop({fifo, In::list(), Out::list()}) -> {term(), {fifo, list(), list()}}.
pop({fifo, [], []}) -> erlang:error('empty fifo');
pop({fifo, In, []}) -> pop({fifo, [], lists:reverse(In)});
pop({fifo, In, [H|T]}) -> {H, {fifo, In, T}}.

-spec empty({fifo, [], []}) -> true;
    ({fifo, nonempty_list(), []}) -> false;
    ({fifo, [], nonempty_list()}) -> false;
    ({fifo, nonempty_list(), nonempty_list()}) -> false.

empty({fifo, [], []}) -> true;
empty({fifo, _, _}) -> false.

test() ->
  N = new(),
  {2, N2} = pop(push(push(new(), 2), 5)),
  {5, N3} = pop(N2),
  N = N3,
  true = empty(N3),
  false = empty(N2),
  pop({fifo, [a, b], [e]}).