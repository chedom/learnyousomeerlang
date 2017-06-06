%%%-------------------------------------------------------------------
%%% @author dom
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jun 2017 14:58
%%%-------------------------------------------------------------------
-module(ops_tests).
-author("dom").
-include_lib("eunit/include/eunit.hrl").
%% API
add_test() ->
  4 = ops:add(2,2).
