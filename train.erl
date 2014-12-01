-module(train).
-export([start/4]).


start(Capacity, StartTime, Direction, StartStation) ->
	Pid = spawn(fun() ->
		loop(StartTime, Capacity, Direction, StartStation, [], 0, 0, 0)
	end),
    clock:add(clk, Pid),
    Pid.


loop(_StartTime, _Capacity, _Direction, endStation, _PassengerList, _, 
_DisembRemaining, _WaitTime) ->
	output:remove(outMod, train),
	clock:remove(clk, self()),
	ok;


loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation,
DisembRemaining, WaitTime) ->

	receive
        %Train begins
        {tick, Time} when (Time =:= (StartTime - 2)) -> io:fwrite("enqueue~n", []),
            output:add(outMod, train),
            CurrStation ! {trainIncoming, self(), Direction},
            receive
                {inQueue} -> ok
            end,
            clk ! {minuteDone},
            output:newTrainStat(outMod, {Direction, track, CurrStation, 
                length(PassengerList)}),
            loop(StartTime, Capacity, Direction, CurrStation, PassengerList, 1, 
                DisembRemaining, WaitTime);
		
        % Don't Do Anything if Haven't Started Yet
		{tick, Time } when (Time < StartTime - 2) ->
			io:fwrite("tick: ~p, not started~n", [Time]),
			clk ! { minuteDone },
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, 
			    TimeToStation, DisembRemaining, WaitTime);
            
		% Train is Outside Station, Ready to Enter
		{tick, Time } when (TimeToStation == 1) ->
			io:fwrite("tick: ~p, ready to enter~n", [Time]),
			case tryEnterPlatform(CurrStation, Direction) of
				% Being Blocked
				entryFailed ->
					io:fwrite("tick: ~p, failed entry~n", [Time]),
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, track, CurrStation,
					    length(PassengerList) }),
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList,
					    TimeToStation, DisembRemaining, WaitTime);

				% Successfully Pulled Into Station
				enteredPlatform ->

					% Tell Passengers On Board
					% Note: This formt is different - what is Name?
					DisembCount = notifyPassengers(PassengerList, { station, CurrStation, self() }, 0),
					io:fwrite("tick: ~p, entered station~n", [Time]),

					%DisembarkCount = peopleDisembarking(CurrStation, PassengerList, 0),
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, station, CurrStation, length(PassengerList) }),
					io:fwrite("haha~n", []),
                    loop(StartTime, Capacity, Direction, CurrStation, PassengerList, 0, DisembCount, 0)

				end;

		% Will Arive in Future, Currently in Transit
		{ tick, Time } when (TimeToStation > 1) ->
			io:fwrite("tick: ~p, currently in transit, ~p minutes remaining ~n", [Time, TimeToStation]),
			clk ! { minuteDone },
			output:newTrainStat(outMod, {Direction, track, CurrStation, length(PassengerList)}),
            loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation-1, DisembRemaining, WaitTime);

		% Train is Currently Boarding
		{ tick, Time } when (TimeToStation == 0) -> 

			io:fwrite("tick: ~p, currently boarding~n", [Time]),

			if

				% Train is At Capacity, So Leave
				Capacity == length(PassengerList) ->
					io:fwrite("tick: ~p, at capacity, leaving ~n", [Time]),
					CurrStation ! { trainLeaving, Direction, self() },
					receive
                        			{trainLeft} -> ok
                    			end,
                    			{NextStation, NewTimeToNext} = carto:timeToNext(CurrStation, Direction),
					NextStation ! { trainIncoming, self(), Direction },
                    			clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, station, CurrStation, length(PassengerList) }),
					emptyMailbox(),
					loop(StartTime, Capacity, Direction, NextStation, PassengerList, NewTimeToNext, 0, 0);
				
				% Not At Capacity, Waited for A Minutes, So Leave
				WaitTime == 1 ->
					io:fwrite("tick: ~p, waited, leaving~n", [Time]),
					CurrStation ! { trainLeaving, Direction, self() },
                    			receive
                        			{trainLeft} -> ok
                    			end,
					{NextStation, NewTimeToNext} = carto:timeToNext(CurrStation, Direction),
				    	if
                        			NextStation =/= endStation -> NextStation ! { trainIncoming, self(), Direction },
                                            		clk ! {minuteDone};
                        			true -> ok %kill passenger list
                    			end,
					output:newTrainStat(outMod, { Direction, track, NextStation, length(PassengerList) }),
					emptyMailbox(),
					loop(StartTime, Capacity, Direction, NextStation, PassengerList, NewTimeToNext, 0, 0);

				% Not At Capacity, Many People On Platform
				true ->
				    CurrStation ! {numWaiting, Direction, self()},
				    receive
				        {numWaiting, BoardCount} -> BoardCount
				     end,
					{NewPassList, MovedCurrTick, DisembarkRemaining} = 
					    stationMinute(Capacity, CurrStation, PassengerList, 0, DisembRemaining, BoardCount),
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, station, CurrStation, length(PassengerList) }),
					if
					    MovedCurrTick =:= 100 -> loop(StartTime, Capacity, Direction, 
					        CurrStation, NewPassList, TimeToStation, DisembarkRemaining, WaitTime);
                        true -> loop(StartTime, Capacity, Direction, CurrStation, 
                            NewPassList, TimeToStation, DisembarkRemaining, WaitTime + 1)
                    end
			end;

		% Increments Arrival Time To Simulate Delay
		% Delay Between Stations
		{ delay, Incr } when (TimeToStation > 0) ->
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation+Incr, DisembRemaining, WaitTime);

		% Delay While in Station
		{ delay, Incr } when (TimeToStation == 0) ->
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, DisembRemaining, WaitTime-Incr)

	end.


emptyMailbox() ->
	receive
		_ -> ok
	end.

stationMinute(Capacity, _, PassengerList, MovedThisTick, DisembRemaining, _BoardRemaining) 
    when (Capacity =:= length(PassengerList)) and (DisembRemaining =:= 0) -> {PassengerList, MovedThisTick, DisembRemaining};
stationMinute(_, _, PassengerList, MovedThisTick, 0, 0) ->
    {PassengerList, MovedThisTick, 0};
stationMinute(_, _, PassengerList, 100, DisembRemaining, _BoardRemaining) -> {PassengerList, 100, DisembRemaining};
stationMinute(Capacity, CurrStation, PassengerList, MovedThisTick, DisembRemaining, BoardRemaining) ->
    receive
        { disembark, Passenger } ->
			io:fwrite("disembark attempt success ~n"),
			Passenger ! { changedLocation, CurrStation },
			stationMinute(Capacity, CurrStation, lists:delete(Passenger, PassengerList), 
					    MovedThisTick+1, DisembRemaining-1, BoardRemaining);
        { board, Passenger } ->
			io:fwrite("board attempt ~n"),

			if
				% People Still Need to Get Off
				DisembRemaining > 0 ->
					io:fwrite("board attempt fail: still disembarkers ~n"),
					Passenger ! { entryFailed, self() },
					stationMinute(Capacity, CurrStation, PassengerList, MovedThisTick, DisembRemaining, BoardRemaining);

				% Otherwise, Allow the Person to Board
				true ->
					io:fwrite("board attempt success: person boarding ~n"),
					Passenger ! { changedLocation, self() },
					stationMinute(Capacity, CurrStation, [Passenger|PassengerList], MovedThisTick+1, DisembRemaining, BoardRemaining - 1)
			end
	end.

tryEnterPlatform(CurrStation, Direction) ->
	CurrStation ! { trainEntry, self(), Direction },
	receive
		{ entryFailed } -> io:fwrite("failed~n", []), entryFailed;
		{ enteredPlatform } -> io:fwrite("success~n", []), enteredPlatform
	end.



notifyPassengers([], _Message, DisembCount) -> DisembCount;
notifyPassengers([Passenger|PassengerList], Message, DisembCount) ->
	Passenger ! Message,
    receive
        {disYes} -> notifyPassengers(PassengerList, Message, DisembCount + 1);
        {disNo} -> notifyPassengers(PassengerList, Message, DisembCount)
    end.

