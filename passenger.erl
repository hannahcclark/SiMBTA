% Module: Passenger
% Purpose: Imitate a passenger taking the T
% Interface:
%    start adds the passenger to clock and starts the waiting loop
%        which will loop until the clock reaches the passenger start time
%        at which point that loop will be the passenger waiting at the station
%        and traversing the T
%    trip_stats sends output the total trip time of the passenger
% Original Author: Raewyn Duvall
% Date: 11/15/14
% ChangeLog:
%    11/15/14 - RAD - created file
%    11/18/14 - RAD - minor updates (typos, etc)
%    11/19/14 - HCC - fixed glitch with passenger being added to clock
%    11/19/14 - HCC - made changes to add to output module, fixed some typo 
%                     bugs/syntax issues
%    11/20/14 - HCC - added(then removed) some outputs for testing
%    11/22/14 - RAD - formatting, semicolon fix
%    12/01/14 - HCC - somewhat messy way of checking train check upon entry to 
%                     station, because was not otherwise notified
%    12/01/14 - RAD - updated to match station directional lists
%    12/01/14 - RAD - deleted no longer necissary train check upon entry

-module(passenger).
-export([start/3]).

start(StartStation, StartTime, EndStation) ->
	% Spawn self and add to clock
    Direction = carto:directionFromTo(StartStation, EndStation),
    Pid = spawn(fun() ->
	            wait(StartStation, StartTime, EndStation, Direction)
	      end),
    clock:add(clk, Pid),
    Pid.

wait(StartStation, StartTime, EndStation,  Direction) ->
    receive
	    {tick, StartTime} ->
            % Start passenger trip
	        StartStation ! {passengerEnters, self(), Direction},
            %receive
            %    {entered} -> ok
            %end,
            %io:fwrite("~p~n", [EndStation]),
            clock:remove(clk, self()),
	        loop(StartTime, StartStation, StartStation, EndStation, Direction);
	    {tick, _} ->
            % Wait for passenger start time
	        clk ! {minuteDone},
	        wait(StartStation, StartTime, EndStation, Direction)
    end.















loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction) ->
    receive
	    {train, nil, Direction} ->
            % Station is waiting for a train in given direction so wait
            loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
	    {train, Train, Direction} ->
            % There is a train at the station going in the given direction
            % So try to board
	        Train ! {board, self()},
	        loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
        {train, _, _} ->
            % Wait for correct train
            loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
	    {station, Endpoint, Train} ->
            % Disembark train
            Train ! {disYes},
	        Train ! {disembark, self()},
	        loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
	    {station, _, Train} ->
            % Don't disembark train
            Train ! {disNo},
	        loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
	    {changedLocation, Endpoint} ->
            % Disembarked so finish
            output:passengerDone(outMod, {StartStation, Endpoint, StartTime, 
	        trip_stats(clock:currTime(clk), StartTime)});
	    {changedLocation, Train} ->
            % Boarded train so leaves station
	        CurrentLocation ! {passengerLeaves, self(), Direction},
	        loop(StartTime, StartStation, Train, Endpoint, Direction);
        {boardFailed, Train} ->
            % Failed to board train, tries again, and waits
	        Train ! {board, self()},
	        loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
        {disembarkFailed, Train} ->
            % Failed to disembark, tries again, and waits
	        Train ! {disembark, self()},
	        loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction)
    end.

trip_stats(CurrentTime, StartTime) ->
    % Calcluates total time
    % This is a seperate function in case more info should be sent back in a
    % future version.
    CurrentTime - StartTime.	

