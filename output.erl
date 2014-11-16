-module(output).
-export([start/1, remove/2, add/2, endSimulation/1].

start(ProcName) -> register(ProcName,
                       spawn(fun -> loop(0, 0) end)),
                   {ok}.

remove(Proc, Type) -> Proc ! {remove, Type}.
add(Proc, Type) -> Proc ! {add, Type}.
endSimulation(Proc) -> Proc ! {end}.

loop(TrainCount, StationCount) ->
    
