-module(train).
-export([start/5]).


start(Capacity, StartTime, Direction, StartStation, Clock) ->
	spawn(fun() ->
		loop(StartTime, Capacity, Direction, StartStation, [], 0, 0, 0, 0, Clock)
	end).


loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime, Clock) ->
	receive

		% Don't Do Anything if Haven't Started Yet
		{tick, Time } when (Time < StartTime) ->
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime, Clock);

		% Train is Outside Station, Ready to Enter
		{tick, _Time } when (TimeToStation == 1) ->
			case tryEnterPlatform(CurrStation, Direction) of
				% Being Blocked
				entryFailed ->
					Clock ! { minuteDone },
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime, Clock);

				% Successfully Pulled Into Station
				{ enteredPlatform, _Station } ->
					DisembarkCount = peopleDisembarking(CurrStation, PassengerList, 0),
					Clock ! { minuteDone },
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, 0, 0, DisembarkCount, 0, Clock)

				end;

		% Will Arive in Future, Currently in Transit
		{ tick, _Time } when (TimeToStation > 1) ->
			Clock ! { minuteDone },
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation-1, MovedThisTick, DisembRemaining, WaitTime, Clock);

		% Train is Currently Boarding
		{ tick, _Time } when (TimeToStation == 0) -> 
			
			% Tell Passengers On Board
			% Note: Do I do this every tick or just once?
			% Note: This formt is different - what is Name?
			notifyPassengers(PassengerList, { station, CurrStation, self() }),

			if
				% Train is At Capacity, So Leave
				Capacity == length(PassengerList) ->
					CurrStation ! { trainLeaving, Direction },
					NextStation = carto:next(CurrStation),
					NewTimeToNext = carto:timeToNext(NextStation, Direction),
					Clock ! { minuteDone },
					loop(StartTime, Capacity, Direction, NextStation, PassengerList, NewTimeToNext, 0, 0, 0, Clock);
				
				% Not At Capacity, Waited for 2 Minutes, So Leave
				WaitTime == 2 ->
					CurrStation ! { trainLeaving, Direction },
					NextStation = carto:next(CurrStation),
					NewTimeToNext = carto:timeToNext(NextStation, Direction),
					Clock ! { minuteDone },
					loop(StartTime, Capacity, Direction, NextStation, PassengerList, NewTimeToNext, 0, 0, 0, Clock);

				% Not At Capacity, Many People On Platform
				MovedThisTick > 10 ->
					Clock ! { minuteDone },
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime, Clock);

				% Not At Capacity, Few People On Platform
				true ->
					Clock ! { minuteDone },
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime+1, Clock)
			end;


		{ board, Passenger } ->
			if 
				% Too Many People Have Boarded this Minute
				MovedThisTick > 10 -> 
					Passenger ! { entryFailed, self() },
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime, Clock);

				% Train Can't Hold Any More People
				length(PassengerList) == Capacity ->
					Passenger ! { entryFailed, self() },
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime, Clock);

				% People Still Need to Get Off
				DisembRemaining > 0 ->
					Passenger ! { entryFailed, self() },
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime, Clock);

				% Otherwise, Allow the Person to Board
				true ->
					Passenger ! { changedLocation, self() },
					loop(StartTime, Capacity, Direction, CurrStation, [Passenger|PassengerList], TimeToStation, MovedThisTick+1, DisembRemaining, WaitTime, Clock)
			end;


		{ disembark, Passenger } ->
			if
				% Too Many People Have Attempted Disembarking This Minute
				MovedThisTick > 10 ->
					Passenger ! { disembarkFailed, self() },
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation, MovedThisTick, DisembRemaining, WaitTime, Clock);

				% Otherwise, Allow Passenger to Disembark
				true ->
					Passenger ! { changedLocation, self() },
						loop(StartTime, Capacity, Direction, CurrStation, lists:delete(Passenger, PassengerList), TimeToStation, MovedThisTick+1, DisembRemaining-1, WaitTime, Clock)
			end;

		% Increments Arrival Time To Simulate Delay
		{ delay, Incr } ->
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation+Incr, MovedThisTick, DisembRemaining, WaitTime, Clock)

	end.







% Note: I don't need the station PId returned from Station
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
% peopleOnPlatform(CurrStation, Direction) ->
% 	CurrStation ! { getNumWaiting, Direction },
% 	receive
% 		{ numWaiting, NumPassengers } -> NumPassengers
% 	end.

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


