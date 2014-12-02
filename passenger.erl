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
%    11/19/14 - HCC - made changes to add to output module, fixed some typo bugs/syntax issues
%    11/20/14 - HCC - added(then removed) some outputs for testing
%    11/22/14 - RAD - formatting, semicolon fix
%    12/01/14 - HCC - somewhat messy way of checking train check upon entry to station, because was not otherwise notified
%    12/01/14 - RAD - updated to match station directional lists
%    12/01/14 - RAD - deleted no longer necissary train check upon entry

-module(passenger).
-export([start/3,loop/5,trip_stats/2]).

start(StartStation, StartTime, EndStation) ->
	%% add self to clock
    Direction = carto:directionFromTo(StartStation, EndStation),
    Pid = spawn(fun() ->
	wait(StartStation, StartTime, EndStation, Direction)
	end),
    clock:add(clk, Pid),
    Pid.

wait(StartStation, StartTime, EndStation,  Direction) ->
	%% receives {tick, Time} -> if time is start time ->
	%%				remove self from clock
	%%				add self to station
	%% sends {minuteDone} to Clock if time doesn't match start
	%%	 {passengerEnters, Pid} to Station when time is start
	%%		Pid is passenger's pid
    receive
	{tick, StartTime} ->
	    clock:remove(clk, self()),
	    StartStation ! {passengerEnters, self(), Direction},
	    loop(StartTime, StartStation, StartStation, EndStation, Direction);
	{tick, _} ->
	    clk ! {minuteDone},
	    wait(StartStation, StartTime, EndStation, Direction)
    end.

loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction) ->
	%% CurrentLocation is a pid of a station or a train
	%% receives {train, Pid, Direction} -> (sent from station)
	%%		Pid is train's pid, Direction is train's direction
	%%	    {station, Pid, Name} -> (sent from train)
	%%		Pid is station's pid, Name is station's name
	%%	    {changedLocation, NewLoc} -> (sent from train)
	%%		notifies successful board or disembark request
	%%	    {boardFailed, Pid} -> (sent from train)
	%%		notifies failure to board
	%%	    {disembarkFailed, Pid} -> (sent from train)
	%%		notifies failure to disembark
	%% sends {board, Pid} to train to request boarding
	%%	 {disembark, Pid} to train to request disembarking
    receive
	{train, nil, Direction} ->
            loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
	{train, Train, Direction} ->
	    Train ! {board, self()},
	    loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
        {train, _, _} ->
            loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
	{station, Endpoint, Train} ->
            Train ! {disYes},
	    Train ! {disembark, self()},
	    loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
	{station, _, Train} ->
            Train ! {disNo},
	    loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
	{changedLocation, Endpoint} ->
            output:passengerDone(outMod, {StartStation, Endpoint, StartTime, 
	        trip_stats(clock:currTime(clk), StartTime)}),
            io:fwrite("finclock ~p~n", [whereis(clk)]);
	{changedLocation, Train} ->
	    CurrentLocation ! {passengerLeaves, self(), Direction},
	    loop(StartTime, StartStation, Train, Endpoint, Direction);
	{boardFailed, Train} ->
	    Train ! {board, self()},
	    loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
	{disembarkFailed, Train} ->
	    Train ! {disembark, self()},
	    loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction)
    end.

trip_stats(CurrentTime, StartTime) ->
	%% results in travel time, maybe add more stats later
    CurrentTime - StartTime.
	

