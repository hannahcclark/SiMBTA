-module(passenger).
-export([start/4,loop/4,trip_stats/2]).

start(StartStation, StartTime, EndStation, Clock) ->
	%% add self to clock
    Direction = corto:directionFromTo(StartStation, EndStation),
    WaitSelf = spawn(fun() ->
	wait(StartStation, StartTime, EndStation, Clock, Direction)
	end), 
    clock:add(Clock, WaitSelf).

wait(StartStation, StartTime, EndStation, Clock, Direction) ->
	%% receives {tick, Time} -> if time is start time ->
	%%				remove self from clock
	%%				add self to station
	%% sends {minuteDone} to Clock if time doesn't match start
	%%	 {passengerEnters, Pid} to Station when time is start
	%%		Pid is passenger's pid
    WaitSelf = self(),
    receives
	{tick, StartTime} ->
	    Self = spawn(fun() ->
	    	loop(StartTime, StartStation, EndStation, Direction)
		end),
	    clock:remove(Clock, WaitSelf),
	    StartStation ! {passengerEnters, Self};
	{tick, _} ->
	    Clock ! {minuteDone},
	    wait(StartStation, StartTime, EndStation, Clock, Direction);
    end.

loop(StartTime, CurrentLocation, Endpoint, Direction) ->
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
    recieves
	{train, Train, Direction} ->
	    Train ! {board, Self},
	    loop(StartTime, CurrentLocation, Endpoint, Direction);
	{station, Endpoint, Name, Train} ->
	    Train ! {disembark, Self},
	    loop(StartTime, CurrentLocation, Endpoint, Direction);
	{station, _, Name, Train} ->
	    loop(StartTime, CurrentLocation, Endpoint, Direction);
	{changedLocation, Endpoint} ->
	    CurrentTime = StartTime + 5, %% CHANGE THIS MOTHAFUCKA
	    trip_stats(CurrentTime, StartTime);
	{changedLocation, Train} ->
	    CurrentLocation ! {passengerLeaves, Self};
	    loop(StartTime, Train, Endpoint, Direction);
	{boardFailed, Train} ->
	    loop(StartTime, CurrentLocation, Endpoint, Direction);
	{disembarkFailed, Train} ->
	    loop(StartTime, CurrentLocation, Endpoing, Direction);
    end.

trip_stats(CurrentTime, StartTime) ->
	%% results in travel time, maybe add more stats later
    CurrentTime - StartTime.
	

