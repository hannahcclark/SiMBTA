-module(passTest).
-export([test/0]).

test() ->
    clock:start(clk),
    output:start(outMod, "outFile.txt"),
    register(alewife, spawn(fun() -> ale() end)),
    register(trn, spawn(fun() -> train() end)),
    register(pass, passenger:start(alewife, 2, porter)),
    io:fwrite("created~n", []),
    clock:add(clk, trn),
    io:fwrite("~p~n", [clk]),
    clock:add(clk, self()),
    clock:startClock(clk),
    recLoop().

recLoop()->
    receive
        {tick, 4} -> io:fwrite("rminute 4~n", []),
                           alewife ! train,
                           clock:remove(clk, self());
        {tick, Min} -> io:fwrite("rminute ~p~n", [Min]),
                            clk ! {minuteDone},
                            recLoop()
    end.
ale() ->
    receive
        {passengerEnters, Pid} -> io:fwrite("pass ~p entered~n", [Pid]),
                                ale();
        train -> io:fwrite("trn~n", []), pass ! {train, trn, ashmont},
                        ale();
        {passengerLeaves, Pid} -> io:fwrite("pass ~p left~n", [Pid])
    end.

train() ->
    receive
        {tick, 8} -> io:fwrite("tminute 8~n", []),
                            pass ! {station, davis, tr},
                            clk ! {minuteDone},
                            train();
        {tick, 10} -> io:fwrite("tminute 10~n", []),
                            pass ! {station, porter, tr},
                            clk ! {minuteDone},
                            train();
        {tick, Min} -> io:fwrite("tminute ~p~n", [Min]),
                            clk ! {minuteDone},
                            train();
        {board, Pid} -> io:fwrite("board rec'd~n", []),
                        Pid ! {changedLocation, self()},
                        train();
        {disembark, Pid} -> io:fwrite("dis rec'd~n", []),
                        Pid ! {changedLocation, porter},
                        clock:remove(clk, self())
    end.
