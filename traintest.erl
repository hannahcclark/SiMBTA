-module(traintest).
-export([run/0]).

run() ->
	ClkP = clock:start(clk),
	Train1 = train:start(50, 5, in, alewife, ClkP),
	clock:add(ClkP, Train1).