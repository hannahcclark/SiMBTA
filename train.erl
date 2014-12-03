% Module: Train
% Purpose: Act as a train on the T
% Its state is given by { StartTime, Capacity, Direction, EndStation, PassengerList, TimeToStation, DisembRemaining, WaitTime }
%   where
%   StartTime is the minute number at which the train left its initial station [set at init]
%   Capacity is the maximum number of people that can be on the train [set at init]
%	Direction is an atom of the final station on the route [set at init]
%	CurrStation is an atom of the station the train is currently in or is approaching
%	PassengerList is a list of passenger PIds of people currently on the train
%	TimeToStation is the number of minutes until a train reaches the next station (under ideal conditions)
%		This is decremented every minute of travel unless a delay is encountered
%	DisembRemaining is the number of people waiting to disembark at a station.
%		This is set when a train enters a station and prevents boarding until it is zero.
%	WaitTime is the number of minutes a train has waited at a station without significant boarding.
%		This is set to zero when a train arrives, and then increments every minute that fewer than the maximum
%		  number of people attempt to board the train.
% Interface:
%    start/4 initializes a train based on input specifications
% Original Author: Andrew Stephens
% Date: 11/15/14
% ChangeLog:
%	12/02/14 - AMS/RAD - Fixed issues related to boarding and disembarking flow
%   12/01/14 - HCC - Made changes to allow for more realistic boarding/disembarkation
%   12/01/14 - HCC - Made changes to allow for more realistic time/passenger movement parameters
%   11/22/14 - HCC - Made changes necessary for adding to output
%	11/19/14 - HCC - Fixed bug with what passengers was being told as new locations
%	11/15/14 - AMS - Created file

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
	receive
		{ tick, Time } ->
			io:fwrite("train dies here ~n", []),
			output:remove(outMod, train),
			clock:remove(clk, self())
		end,
	ok;


loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation,
DisembRemaining, WaitTime) ->

	receive
        %Train begins
        {tick, Time} when (Time =:= (StartTime - 2)) -> 
        	%io:fwrite("enqueue~n", []),
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
			%io:fwrite("tick: ~p, not started~n", [Time]),
			clk ! { minuteDone },
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, 
			    TimeToStation, DisembRemaining, WaitTime);
            
		% Train is Outside Station, Ready to Enter
		{tick, Time } when (TimeToStation == 1) ->
			%io:fwrite("tick: ~p, ready to enter~n", [Time]),
			case tryEnterPlatform(CurrStation, Direction) of
				% Being Blocked
				entryFailed ->
					%io:fwrite("tick: ~p, failed entry~n", [Time]),
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, track, CurrStation,
					    length(PassengerList) }),
					loop(StartTime, Capacity, Direction, CurrStation, PassengerList,
					    TimeToStation, DisembRemaining, WaitTime);

				% Successfully Pulled Into Station
				enteredPlatform ->

					% Tell Passengers On Board
					DisembCount = notifyPassengers(PassengerList, { station, CurrStation, self() }, 0),
					io:fwrite("tick: ~p, entered station~n", [Time]),
					io:fwrite("disemb count ~p ~n", [DisembCount]),

					%DisembarkCount = peopleDisembarking(CurrStation, PassengerList, 0),
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, station, CurrStation, length(PassengerList) }),
					%io:fwrite("train stat recorded~n", []),
                    loop(StartTime, Capacity, Direction, CurrStation, PassengerList, 0, DisembCount, 0)

				end;

		% Will Arive in Future, Currently in Transit
		{ tick, Time } when (TimeToStation > 1) ->
			%io:fwrite("tick: ~p, currently in transit, ~p minutes remaining ~n", [Time, TimeToStation]),
			clk ! { minuteDone },
			output:newTrainStat(outMod, {Direction, track, CurrStation, length(PassengerList)}),
            loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation-1, DisembRemaining, WaitTime);

		% Train is Currently Boarding
		{ tick, Time } when (TimeToStation == 0) -> 

			io:fwrite("tick: ~p, currently boarding~n", [Time]),

			if

				% Train is At Capacity (after Everyone Has Disembarked)
				(Capacity == length(PassengerList)) and (DisembRemaining == 0) ->
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
				
				% Not At Capacity, Waited for A Minutes, So Leave (after Everyone has Disembarked)
				(WaitTime == 1) and (DisembRemaining == 0) ->
					io:fwrite("tick: ~p, waited, leaving~n", [Time]),
					CurrStation ! { trainLeaving, Direction, self() },
					receive
                    	{trainLeft} -> ok
					end,
                    %io:fwrite("left ok~n",[]),
					{NextStation, NewTimeToNext} = carto:timeToNext(CurrStation, Direction),
				    if
                    	NextStation =/= endStation -> 
                    		NextStation ! { trainIncoming, self(), Direction };
                    	true -> ok % TODO kill passenger list?
					end,
					clk ! {minuteDone},
                    %io:fwrite("send incoming message~n", []),
					output:newTrainStat(outMod, { Direction, track, NextStation, length(PassengerList) }),
					emptyMailbox(),
					%io:fwrite("mailbox empty~n", []),
					loop(StartTime, Capacity, Direction, NextStation, PassengerList, NewTimeToNext, 0, 0);

				% Other Cases
				true ->
				    CurrStation ! {numWaiting, Direction, self()},
				    receive
				        {numWaiting, BoardCount} -> BoardCount
				     end,
				     io:fwrite("tick ~p, entering station minute~n", [Time]),
					{NewPassList, MovedCurrTick, DisembarkRemaining} = 
					    stationMinute(Capacity, CurrStation, PassengerList, 0, DisembRemaining, BoardCount),
					 io:fwrite("size of new pass list: ~w ~n", [length(NewPassList)]),
					clk ! { minuteDone },
					io:fwrite("once again, size of new pass list: ~w ~n", [length(NewPassList)]),
					output:newTrainStat(outMod, { Direction, station, CurrStation, length(NewPassList) }),
					if
					    MovedCurrTick =:= 100 -> 
					    	loop(StartTime, Capacity, Direction, 
					        CurrStation, NewPassList, TimeToStation, DisembarkRemaining, WaitTime);
                        true -> 
                        	loop(StartTime, Capacity, Direction, CurrStation, 
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
		{ board, _ } -> 
			%io:fwrite("MAILBOX: removing board request ~n", []),
			emptyMailbox()
		after 0 -> 
			%io:fwrite("MAILBOX: no board requests ~n", []),
			ok
	end.



% Train is At Capacity; Stop Boarding
stationMinute(Capacity, _, PassengerList, MovedThisTick, DisembRemaining, _BoardRemaining) 
    when (Capacity =:= length(PassengerList)) and (DisembRemaining =:= 0) -> 
    io:fwrite("train at capacity ~n", []),
    {PassengerList, MovedThisTick, DisembRemaining};

% Nobody Needs to Disembark or Board
stationMinute(_, _, PassengerList, MovedThisTick, 0, 0) ->
	io:fwrite("no passenger movement ~n", []),
    {PassengerList, MovedThisTick, 0};

% 100 Passengers Have Moved; Try Again Next Minute
stationMinute(_, _, PassengerList, 100, DisembRemaining, _BoardRemaining) -> 
	io:fwrite("max passenger movement, ~w remaining ~n", [DisembRemaining]),
	{PassengerList, 100, DisembRemaining};

% Standard Case
stationMinute(Capacity, CurrStation, PassengerList, MovedThisTick, DisembRemaining, BoardRemaining) ->
    receive
        { disembark, Passenger } ->
			%io:fwrite("disembark attempt success, ~w remaining ~n"),
			Passenger ! { changedLocation, CurrStation },
			stationMinute(Capacity, CurrStation, lists:delete(Passenger, PassengerList), 
					    MovedThisTick+1, DisembRemaining-1, BoardRemaining);
        { board, Passenger } ->
			io:fwrite("board attempt ~n"),

			if
				% People Still Need to Get Off
				DisembRemaining > 0 ->
					io:fwrite("board attempt fail: still disembarkers ~n"),
					Passenger ! { boardFailed, self() },
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
		{ entryFailed } -> 
			%io:fwrite("failed~n", []), 
			entryFailed;
		{ enteredPlatform } -> 
			%io:fwrite("success~n", []), 
			enteredPlatform
	end.



notifyPassengers([], _Message, DisembCount) -> DisembCount;
notifyPassengers([Passenger|PassengerList], Message, DisembCount) ->
	Passenger ! Message,
    receive
        {disYes} -> notifyPassengers(PassengerList, Message, DisembCount + 1);
        {disNo} -> notifyPassengers(PassengerList, Message, DisembCount)
    end.

