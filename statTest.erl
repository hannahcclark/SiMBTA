-module(statTest).
-export([test/0]).

test() -> clock:init(clk),
        output:start(outMod, "outFile.txt"),
        station:start(alewife),
        clock:add(clk, self()),
        clock:startClock(clk),
        loop(),
        clock:remove(clk, self()),
        clock:remove(clk, alewife),
        alewife ! {endSim}.

loop() ->
    io:fwrite("loop~n", []),
    receive
    {tick, 0} -> io:fwrite("0~n",[]),
                alewife ! {passengerEnters, self()},
                clk ! {minuteDone},
                loop();
    {tick, 2} -> io:fwrite("1~n", []), 
                alewife ! {trainIncoming, self(), ashmont},
                clk ! {minuteDone},
                loop();
    {tick, 3} -> io:fwrite("3~n", []), 
                alewife ! {trainEntry, self(), ashmont},
                receive
                    Message -> io:fwrite("~p~n", [Message])
                end,
                clk ! {minuteDone},
                loop();
    {train, Train, Direction} -> io:fwrite("train ~p ~p~n", [Train, Direction]),
                alewife ! {passengerLeaves, self()},
                loop();
    {tick, 7} -> io:fwrite("7~n", []), 
                alewife ! {trainLeaving, ashmont},
                clk ! {minuteDone},
                loop();
    {tick, 10} -> ok;
    {tick, Min} -> io:fwrite("~p~n", [Min]),
                    clk ! {minuteDone},
                loop()
    end.
