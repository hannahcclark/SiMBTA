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
	    StartStation ! {passengerEnters, self()},
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
	{train, Train, Direction} ->
	    Train ! {board, self()},
	    loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
        {train, _, _} ->
            loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
	{station, Endpoint, Train} ->
	    Train ! {disembark, self()},
	    loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
	{station, _, _} ->
	    loop(StartTime, StartStation, CurrentLocation, Endpoint, Direction);
	{changedLocation, Endpoint} ->
            output:passengerDone(outMod, {StartStation, Endpoint, StartTime, 
	        trip_stats(clock:currTime(clk), StartTime)}),
	{changedLocation, Train} ->
	    CurrentLocation ! {passengerLeaves, self()},
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
	

