-module(my_server).
-compile(export_all).

start(Module, InitialState) ->
    spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
    spawn_link(fun() -> init(Module, InitialState) end).



reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.

call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {sync, self(), Ref, Msg},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'Down', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
              erlang:error(timeout)
    end.

cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.


%% Private stuff
loop(Module, State) ->
    receive
        {async, Msg} -> 
            loop(Module, Module:handle_cast(Msg, State));
        {sync, Pid, Ref, Msg} ->
            loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
    end.


init(Module, InitialState) ->
    loop(Module, Module:init(InitialState)).
