-module(train).
-export([start/4]).


start(Capacity, StartTime, Direction, StartStation) ->
	spawn(fun() ->
		loop(StartTime, Capacity, Direction, StartStation, [], 0, 0, 0, 0)
	end).


loop(_StartTime, _Capacity, _Direction, endStation, _PassengerList, _, _MovedThisTick, _DisembRemaining, _WaitTime) ->
	output:remove(outMod, train),
	clock:remove(clk, self()),
	ok;


loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime) ->

	receive

		% Don't Do Anything if Haven't Started Yet
		{tick, Time } when (Time < StartTime) ->
			io:fwrite("tick: ~p, not started~n", [Time]),
			clk ! { minuteDone },
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime);

		% Train is Almost At Station, Notify Station Accordingly
		{tick, Time} when (TimeToStation == 2) ->
			CurrStation ! { trainIncoming, self(), Direction },
			io:fwrite("tick: ~p, notifying station ~n", [Time]),
			clk ! { minuteDone },
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation-1, MovedThisTick, DisembRemaining, WaitTime);

		% Train is Outside Station, Ready to Enter
		{tick, Time } when (TimeToStation == 1) ->
			io:fwrite("tick: ~p, ready to enter~n", [Time]),
			case tryEnterPlatform(CurrStation, Direction) of
				% Being Blocked
				entryFailed ->
					io:fwrite("tick: ~p, failed entry~n", [Time]),
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, track, CurrStation, length(PassengerList) }),
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime);

				% Successfully Pulled Into Station
				{ enteredPlatform, _Station } ->

					% Tell Passengers On Board
					% Note: This formt is different - what is Name?
					notifyPassengers(PassengerList, { station, CurrStation, self() }),
					io:fwrite("tick: ~p, entered station~n", [Time]),

					DisembarkCount = peopleDisembarking(CurrStation, PassengerList, 0),
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, track, CurrStation, length(PassengerList) }),
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, 0, 0, DisembarkCount, 0)

				end;

		% Will Arive in Future, Currently in Transit
		{ tick, Time } when (TimeToStation > 2) ->
			io:fwrite("tick: ~p, currently in transit, ~p minutes remaining ~n", [Time, TimeToStation]),
			clk ! { minuteDone },
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation-1, MovedThisTick, DisembRemaining, WaitTime);

		% Train is Currently Boarding
		{ tick, Time } when (TimeToStation == 0) -> 

			io:fwrite("tick: ~p, currently boarding~n", [Time]),

			if

				% Train is At Capacity, So Leave
				Capacity == length(PassengerList) ->
					io:fwrite("tick: ~p, at capacity, leaving ~n", [Time]),
					CurrStation ! { trainLeaving, Direction },
					{NextStation, NewTimeToNext} = carto:timeToNext(CurrStation, Direction),
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, station, CurrStation, length(PassengerList) }),
					loop(StartTime, Capacity, Direction, NextStation, PassengerList, NewTimeToNext, 0, 0, 0);
				
				% Not At Capacity, Waited for 2 Minutes, So Leave
				WaitTime == 2 ->
					io:fwrite("tick: ~p, waited, leaving~n", [Time]),
					CurrStation ! { trainLeaving, Direction },
					{NextStation, NewTimeToNext} = carto:timeToNext(CurrStation, Direction),
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, station, CurrStation, length(PassengerList) }),
					loop(StartTime, Capacity, Direction, NextStation, PassengerList, NewTimeToNext, 0, 0, 0);

				% Not At Capacity, Many People On Platform
				MovedThisTick > 10 ->
					io:fwrite("tick: ~p, too many people moved~n", [Time]),
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, station, CurrStation, length(PassengerList) }),
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime);

				% Not At Capacity, Few People On Platform
				true ->
					io:fwrite("tick: ~p, not at capcity, waiting time: ~p~n", [Time, WaitTime]),
					clk ! { minuteDone },
					%output:newTrainStat(outMod, { Direction, station, CurrStation, length(PassengerList) }),
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime+1)
			end;


		{ board, Passenger } ->
			io:fwrite("    board attempt ~n"),

			if 
				% Too Many People Have Boarded this Minute
				MovedThisTick > 10 -> 
					io:fwrite("    board attempt fail: too many moved this tick ~n"),
					Passenger ! { entryFailed, self() },
					emptyMailbox(),
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime);

				% Train Can't Hold Any More People
				length(PassengerList) == Capacity ->
					io:fwrite("    board attempt fail: train at capacity ~n"),
					Passenger ! { entryFailed, self() },
					emptyMailbox(),
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime);

				% People Still Need to Get Off
				DisembRemaining > 0 ->
					io:fwrite("    board attempt fail: still disembarkers ~n"),
					Passenger ! { entryFailed, self() },
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime);

				% Otherwise, Allow the Person to Board
				true ->
					io:fwrite("    board attempt success: person boarding ~n"),
					Passenger ! { changedLocation, self() },
					loop(StartTime, Capacity, Direction, CurrStation, [Passenger|PassengerList], TimeToStation, MovedThisTick+1, DisembRemaining, WaitTime)
			end;


		{ disembark, Passenger } ->
			if
				% Too Many People Have Attempted Disembarking This Minute
				MovedThisTick > 10 ->
					io:fwrite("    disembark attempt fail: too many moved this tick ~n"),
					Passenger ! { disembarkFailed, self() },
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime);

				% Otherwise, Allow Passenger to Disembark
				true ->
					io:fwrite("    disembark attempt success ~n"),
					Passenger ! { changedLocation, CurrStation },
						loop(StartTime, Capacity, Direction, CurrStation, lists:delete(Passenger, PassengerList), TimeToStation, MovedThisTick+1, DisembRemaining-1, WaitTime)
			end;

		% Increments Arrival Time To Simulate Delay
		% Delay Between Stations
		{ delay, Incr } when (TimeToStation > 0) ->
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation+Incr, MovedThisTick, DisembRemaining, WaitTime);

		% Delay While in Station
		{ delay, Incr } when (TimeToStation == 0) ->
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime-Incr)

	end.


emptyMailbox() ->
	receive
		_ -> ok
	end.


tryEnterPlatform(CurrStation, Direction) ->
	CurrStation ! { trainEntry, self(), Direction },
	receive
		{ entryFailed } -> entryFailed;
		{ enteredPlatform, Station } -> Station
	end.



notifyPassengers([], _Message) -> ok;
notifyPassengers([Passenger|PassengerList], Message) ->
	Passenger ! Message,
	notifyPassengers(PassengerList, Message).




% Note: This was not documented, I just made this up
peopleDisembarking(_CurrStation, [], Count) -> Count;
peopleDisembarking(CurrStation, [Passenger|PassengerList], Count) ->
	Passenger ! { isDisembarking, CurrStation, self() },
	receive
		{ yes } ->
			peopleDisembarking(CurrStation, PassengerList, Count+1);
		{ no } ->
			peopleDisembarking(CurrStation, PassengerList, Count)
	end.


