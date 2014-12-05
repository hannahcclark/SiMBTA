% Module: Train
% Purpose: Act as a train on the T
% Its state is given by { StartTime, Capacity, Direction, EndStation, 
% PassengerList, TimeToStation, DisembRemaining, WaitTime }
%   where
%   StartTime is the minute number at which the train left its initial station 
%   [set at init]
%   Capacity is the maximum number of people that can be on the train 
%   [set at init]
%	Direction is an atom of the final station on the route [set at init]
%	CurrStation is an atom of the station the train is currently in or is 
%   approaching
%	PassengerList is a list of passenger PIds of people currently on the train
%	TimeToStation is the number of minutes until a train reaches the next 
%   station (under ideal conditions)
%		This is decremented every minute of travel unless a delay is encountered
%	DisembRemaining is the number of people waiting to disembark at a station.
%		This is set when a train enters a station and prevents boarding until it
%   is zero.
%	WaitTime is the number of minutes a train has waited at a station without 
%   significant boarding.
%		This is set to zero when a train arrives, and then increments every 
%   minute that fewer than the maximum
%		  number of people attempt to board the train.
% Interface:
%    start/4 initializes a train based on input specifications
% Original Author: Andrew Stephens
% Date: 11/15/14
% ChangeLog:
%    12/05/14 - HCC - enforced 80 character limit
%    12/04/14 - HCC - removed commented out code
%    12/03/14 - HCC - removed debugging output
%    12/03/14 - HCC - changes to make sure passengers are not still on train 
%                       (later removed due to changes in main)
%	12/02/14 - AMS/RAD - Fixed issues related to boarding and disembarking flow
%   12/01/14 - HCC - Made changes to allow for more realistic boarding/
%                       disembarkation
%   12/01/14 - HCC - Made changes to allow for more realistic time/passenger 
%                       movement parameters
%   11/22/14 - HCC - Made changes necessary for adding to output
%	11/19/14 - HCC - Fixed bug with what passengers was being told as new 
%                       locations
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
		{ tick, _Time } ->
			output:remove(outMod, train),
			clock:remove(clk, self())
		end,
	ok;


loop(StartTime, Capacity, Direction, CurrStation, PassengerList, TimeToStation,
DisembRemaining, WaitTime) ->

	receive
        %Train begins
        {tick, Time} when (Time =:= (StartTime - 2)) -> 
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
			clk ! { minuteDone },
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, 
			    TimeToStation, DisembRemaining, WaitTime);
            
		% Train is Outside Station, Ready to Enter
		{tick, _Time } when (TimeToStation == 1) ->
			case tryEnterPlatform(CurrStation, Direction) of
				% Being Blocked
				entryFailed ->
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, track, CurrStation,
					    length(PassengerList) }),
					loop(StartTime, Capacity, Direction, CurrStation, 
                        PassengerList,
					    TimeToStation, DisembRemaining, WaitTime);

				% Successfully Pulled Into Station
				enteredPlatform ->

					% Tell Passengers On Board
					DisembCount = notifyPassengers(PassengerList, { station, 
                        CurrStation, self() }, 0),
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, station, 
                        CurrStation, length(PassengerList) }),
                    loop(StartTime, Capacity, Direction, CurrStation, 
                        PassengerList, 0, DisembCount, 0)

				end;

		% Will Arive in Future, Currently in Transit
		{ tick, _Time } when (TimeToStation > 1) ->
			clk ! { minuteDone },
			output:newTrainStat(outMod, {Direction, track, CurrStation, 
                length(PassengerList)}),
            loop(StartTime, Capacity, Direction, CurrStation, PassengerList, 
                TimeToStation-1, DisembRemaining, WaitTime);

		% Train is Currently Boarding
		{ tick, _Time } when (TimeToStation == 0) -> 
			if

				% Train is At Capacity (after Everyone Has Disembarked)
				(Capacity == length(PassengerList)) and (DisembRemaining == 0) 
                ->
					CurrStation ! { trainLeaving, Direction, self() },
					receive
						{trainLeft} -> ok
                    end,
                    {NextStation, NewTimeToNext} = carto:timeToNext(CurrStation,
                        Direction),
					NextStation ! { trainIncoming, self(), Direction },
                    receive
                        {inQueue} -> ok
                    end,
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, station, 
                        CurrStation, length(PassengerList) }),
					emptyMailbox(),
					loop(StartTime, Capacity, Direction, NextStation, 
                        PassengerList, NewTimeToNext, 0, 0);
				
				% Not At Capacity, Waited for A Minutes, So Leave 
                %(after Everyone has Disembarked)
				(WaitTime == 1) and (DisembRemaining == 0) ->
					CurrStation ! { trainLeaving, Direction, self() },
					receive
                    	{trainLeft} -> ok
					end,
					{NextStation, NewTimeToNext} = carto:timeToNext(CurrStation,
                        Direction),
				    if
                    	NextStation =/= endStation -> 
                    		NextStation ! { trainIncoming, self(), Direction },
                            receive
                                {inQueue} -> ok
                            end;
                    	true -> ok
					end,
					clk ! {minuteDone},
					output:newTrainStat(outMod, { Direction, track, NextStation,
                        length(PassengerList) }),
					emptyMailbox(),
					loop(StartTime, Capacity, Direction, NextStation, 
                        PassengerList, NewTimeToNext, 0, 0);

				% Other Cases
				true ->
				    CurrStation ! {numWaiting, Direction, self()},
				    receive
				        {numWaiting, BoardCount} -> BoardCount
				     end,
					{NewPassList, MovedCurrTick, DisembarkRemaining} = 
					    stationMinute(Capacity, CurrStation, PassengerList, 0, 
                        DisembRemaining, BoardCount),
					clk ! { minuteDone },
					output:newTrainStat(outMod, { Direction, station, 
                        CurrStation, length(NewPassList) }),
					if
					    MovedCurrTick =:= 100 -> 
					    	loop(StartTime, Capacity, Direction, 
					        CurrStation, NewPassList, TimeToStation, 
                            DisembarkRemaining, WaitTime);
                        true -> 
                        	loop(StartTime, Capacity, Direction, CurrStation, 
                            NewPassList, TimeToStation, DisembarkRemaining, 
                            WaitTime + 1)
                    end
			end;

		% Increments Arrival Time To Simulate Delay
		% Delay Between Stations
		{ delay, Incr } when (TimeToStation > 0) ->
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, 
                TimeToStation+Incr, DisembRemaining, WaitTime);

		% Delay While in Station
		{ delay, Incr } when (TimeToStation == 0) ->
			loop(StartTime, Capacity, Direction, CurrStation, PassengerList, 
                TimeToStation, DisembRemaining, WaitTime-Incr)

	end.


emptyMailbox() ->
	receive
		{ board, _ } -> 
			emptyMailbox()
		after 0 -> 
			ok
	end.



% Train is At Capacity; Stop Boarding
stationMinute(Capacity, _, PassengerList, MovedThisTick, DisembRemaining, 
_BoardRemaining) 
    when (Capacity =:= length(PassengerList)) and (DisembRemaining =:= 0) -> 
    {PassengerList, MovedThisTick, DisembRemaining};

% Nobody Needs to Disembark or Board
stationMinute(_, _, PassengerList, MovedThisTick, 0, 0) ->
    {PassengerList, MovedThisTick, 0};

% 100 Passengers Have Moved; Try Again Next Minute
stationMinute(_, _, PassengerList, 100, DisembRemaining, _BoardRemaining) -> 
	{PassengerList, 100, DisembRemaining};

% Standard Case
stationMinute(Capacity, CurrStation, PassengerList, MovedThisTick, 
DisembRemaining, BoardRemaining) ->
    receive
        { disembark, Passenger } ->
			Passenger ! { changedLocation, CurrStation },
            %receive
            %    {exited} -> ok
            %end,
			stationMinute(Capacity, CurrStation, lists:delete(Passenger, 
            PassengerList), 
					    MovedThisTick+1, DisembRemaining-1, BoardRemaining);
        { board, Passenger } ->
			if
				% People Still Need to Get Off
				DisembRemaining > 0 ->
					Passenger ! { boardFailed, self() },
					stationMinute(Capacity, CurrStation, PassengerList, 
                        MovedThisTick, DisembRemaining, BoardRemaining);

				% Otherwise, Allow the Person to Board
				true ->
					Passenger ! { changedLocation, self() },
					stationMinute(Capacity, CurrStation, 
                        [Passenger|PassengerList], MovedThisTick+1, 
                        DisembRemaining, BoardRemaining - 1)
			end
	end.

tryEnterPlatform(CurrStation, Direction) ->
	CurrStation ! { trainEntry, self(), Direction },
	receive
		{ entryFailed } -> 
			entryFailed;
		{ enteredPlatform } -> 
			enteredPlatform
	end.



notifyPassengers([], _Message, DisembCount) -> DisembCount;
notifyPassengers([Passenger|PassengerList], Message, DisembCount) ->
	Passenger ! Message,
    receive
        {disYes} -> notifyPassengers(PassengerList, Message, DisembCount + 1);
        {disNo} -> notifyPassengers(PassengerList, Message, DisembCount)
    end.

