-module(test_monitor).
-compile(export_all).

start_proc1()->
    spawn(?MODULE, init1, []).

init1() ->
    spawn_monitor(?MODULE, init2, []),
    timer:sleep(3000),
    io:format("Die proc1~n").

init2() ->
    io:format("Start proc2 ~n"),
    timer:sleep(9000),
    io:format("Die proc2~n").
