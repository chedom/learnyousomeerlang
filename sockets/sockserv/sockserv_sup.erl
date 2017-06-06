%%%-------------------------------------------------------------------
%%% @author dom
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jun 2017 13:14
%%%-------------------------------------------------------------------
-module(sockserv_sup).
-author("dom").
-behavior(supervisor).
%% API
-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, Port} = application:get_env(port),
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, {packet, line}]),
  spawn_link(fun empty_listeners/0),
  {ok, {{simple_one_for_one, 60, 3600},
    [{socket,
      {sockserv_serv, start_link, [ListenSocket]},
      temprorary, 1000, worker, [sockserv_serv]}
    ]}}.

start_socket() ->
  supervisor:start_child(?MODULE, []).

empty_listeners() ->
  [start_socket() || _ <- lists:seq(1,20)],
  ok.