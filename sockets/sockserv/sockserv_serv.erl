%%%-------------------------------------------------------------------
%%% @author dom
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jun 2017 13:31
%%%-------------------------------------------------------------------
-module(sockserv_serv).
-author("dom").
-behavior(gen_server).
-record(state, {name,
  next,
  socket}).
%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  code_change/3, terminate/2]).

-define(TIME, 800).
-define(EXP, 50).

start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
  gen_server:cast(self(), accept),
  {ok, #state{socket=Socket}}.

handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(accept, S = #state{socket=ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  sockserv_sup:start_socket(),
  send(AcceptSocket, "What's your character's name?", []),
  {noreply, S#state{socket=AcceptSocket, next=name}};

handle_cast(roll_stats, S = #state{socket=Socket}) ->
  Roll = pq_stats:initial_roll(),
  send(Socket,
    "Stats for your character:~n"
    "  Charisma: ~B~n"
    "  Constitution: ~B~n"
    "  Dexterity: ~B~n"
    "  Intelligence: ~B~n"
    "  Strength: ~B~n"
    "  Wisdom: ~B~n~n"
    "Do you agree to these? y/n~n",
    [Points || {_Name, Points} <- lists:sort(Roll)]),
  {noreply, S#state{next={stats, Roll}}};
handle_cast(stats_accepted, S = #state{name=Name, next={stats, Stats}}) ->
  processquest:start_player(Name, [{stats,Stats},{time,?TIME},
    {lvlexp, ?EXP}]),
  processquest:subscribe(Name, sockserv_pq_events, self()),
  {noreply, S#state{next=playing}};
handle_cast(Event, S = #state{name=N, socket=Sock}) when element(1, Event) =:= N ->
  [case E of
     {wait, Time} -> timer:sleep(Time);
     IoList -> send(Sock, IoList, [])
   end || E <- sockserv_trans:to_str(Event)], % translate to a string
  {noreply, S}.

handle_info({tcp, _Socket, Str}, S=#state{next=name}) ->
  Name = line(Str),
  gen_server:cast(self(), roll_stats),
  {noreply, S#state{name=Name, next=stats}};
handle_info({tcp, Socket, Str}, S = #state{socket=Socket, next={stats, _}}) ->
  case line(Str) of
    "y" ->
      gen_server:cast(self(), stats_accepted);
    "n" ->
      gen_server:cast(self(), roll_stats);
    _ -> % ask again because we didn't get what we wanted
      send(Socket, "Answer with y (yes) or n (no)", [])
  end,
  {noreply, S};
handle_info({tcp, _Socket, "quit"++_}, S) ->
  processquest:stop_player(S#state.name),
  gen_tcp:close(S#state.socket),
  {stop, normal, S};
handle_info({tcp_closed, _Socket}, S) ->
  {stop, normal, S};
handle_info({tcp_error, _Socket, _}, S) ->
  {stop, normal, S};
handle_info(E, S) ->
  io:format("unexpected: ~p~n", [E]),
  {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok;
terminate(_Reason, _State) ->
  io:format("terminate reason: ~p~n", [_Reason]).

line(Str) ->
  hd(string:tokens(Str, "\r\n")).

send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++ "~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.