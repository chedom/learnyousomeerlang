%%%-------------------------------------------------------------------
%%% @author dom
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jun 2017 10:56
%%%-------------------------------------------------------------------
-module(convert).
-author("dom").
-export([main/0]).

main() ->
  [_,_] = convert({a,b}),
  {_,_} = convert([a,b]),
  [_,_] = convert([a,b]),
  {_,_} = convert({a,b}).

-spec convert(tuple()) -> list();
    (list()) -> tuple().
convert(Tup) when is_tuple(Tup) -> tuple_to_list(Tup);
convert(L = [_|_]) -> list_to_tuple(L).
