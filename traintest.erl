-module(traintest).
-export([run/0]).

run() ->
	T1 = train:start(50, 2, ashmont, alewife),
	clock:start(clk),
	clock:startClock(clk),
	T1 ! { tick, 1 },
	T1 ! { tick, 2 },
	T1 ! { tick, 3 },
	T1 ! { tick, 4 }.