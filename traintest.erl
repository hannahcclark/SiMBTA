-module(traintest).
-export([run/0]).

run() ->
	register(clk, spawn(fun() -> fakeClock() end)),
	output:start(outMod, "outFile.txt"),
	station:start(ashmont),
	station:start(davis),
	station:start(alewife),
	T1 = train:start(50, 2, ashmont, alewife),
	T1 ! { tick, 1 },
	T1 ! { tick, 2 },
	T1 ! { tick, 3 },
	T1 ! { tick, 4 },
	T1 ! { tick, 5 },
	T1 ! { tick, 6 },
	T1 ! { tick, 7 },
	T1 ! { tick, 8 }.


fakeClock() ->
	receive
		_ -> fakeClock()
	end.