% Module: Passenger
% Purpose: Imitate a station of the T
% Interface:
%    start spawns the main loop of the station which keeps track of everything
%        happening in terms of passengers waiting and trains stopping
% Original Author: Raewyn Duvall
% Date: 11/15/14
% ChangeLog:
%    11/15/14 - RAD - created file
%    11/22/14 - HCC - fixed syntax errors
%    11/22/14 - HCC - fixed typo bugs
%    11/22/14 - HCC - added changes for output to station
%    11/22/14 - RAD - fixed bug
%    11/22/14 - RAD - formatting
%    12/01/14 - RAD - added waiting passenger count for train
%    12/01/14 - RAD - added direction based passenger lists

-module(station).
-export([start/1,loop/7]).

start(Name) ->
    IncomingIn = queue:new(),
    IncomingOut = queue:new(),
    io:fwrite("reg~n", []),
    register(Name, spawn(fun() -> 
                    loop(Name,[], [], nil, nil, IncomingIn, IncomingOut) end)),
    output:add(outMod, station).

loop(Name, PassengerListIn, PassengerListOut,
     PlatformIn, PlatformOut, IncomingIn, IncomingOut) ->
	%% receives {passengerEnters, Passenger} -> update passengerlist
	%%	    {passengerLeaves, Passenger} -> update passengerlist
	%%	    {trainIncoming, Train, Direction} -> addincomingtrain
	%%	    {trainEntry, Train, Direction} -> trytrainentry
	%%		if yes -> alertPassengers
	%%	    {trainLeaving, Direction} -> update queue 
    receive
        {sendInfo} ->
            case PlatformIn of
                nil -> InCase = false;
                 _ -> InCase = true
            end,
            case PlatformOut of
                nil -> OutCase = false;
                _ -> OutCase = true
            end,
            output:newStationStat(outMod, 
                   {Name, length(PassengerListIn) + length(PassengerListOut)
                   , InCase, OutCase}),
            loop(Name, PassengerListIn, PassengerListOut,
		 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
	{passengerEnters, Passenger, ashmont} -> 
	    NewPassengerList = [Passenger|PassengerListIn],
	    Passenger ! {train, PlatformIn, ashmont},
            loop(Name, NewPassengerList, PassengerListOut,
                 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
	{passengerEnters, Passenger, alewife} -> 
	    NewPassengerList = [Passenger|PassengerListOut],
	    Passenger ! {train, PlatformOut, alewife},
            loop(Name, PassengerListIn, NewPassengerList,
		 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
	{passengerLeaves, Passenger, ashmont} -> 
	    NewPassengerList = lists:delete(Passenger, PassengerListIn),
	    loop(Name, NewPassengerList, PassengerListOut,
		 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
	{passengerLeaves, Passenger, alewife} -> 
	    NewPassengerList = lists:delete(Passenger, PassengerListOut),
	    loop(Name, PassengerListIn, NewPassengerList,
		 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
	{trainIncoming, Train, ashmont} -> 
	    NewIncomingIn = queue:in(Train, IncomingIn),
	    Train ! {inQueue},
            loop(Name, PassengerListIn, PassengerListOut,
		 PlatformIn, PlatformOut, NewIncomingIn, IncomingOut);
	{trainIncoming, Train, alewife} ->
	    NewIncomingOut = queue:in(Train, IncomingOut),
	    Train ! {inQueue},
            loop(Name, PassengerListIn, PassengerListOut,
		 PlatformIn, PlatformOut, IncomingIn, NewIncomingOut);
	{trainEntry, Train, ashmont} -> 
	    case tryTrainEntry(Train, IncomingIn, PlatformIn) of
		yes -> {_, NewIncomingIn} = queue:out(IncomingIn),
		       alertPassengers(Train, ashmont, PassengerListIn),
		       loop(Name, PassengerListIn, PassengerListOut,
			    Train, PlatformOut, NewIncomingIn, IncomingOut);
		no -> loop(Name, PassengerListIn, PassengerListOut,
			   PlatformIn, PlatformOut, IncomingIn, IncomingOut)
	    end;
	{trainEntry, Train, alewife} ->
	    case tryTrainEntry(Train, IncomingOut, PlatformOut) of
		yes -> {_,NewIncomingOut} = queue:out(IncomingOut),
		       alertPassengers(Train, alewife, PassengerListOut),
		       loop(Name, PassengerListIn, PassengerListOut,
			    PlatformIn, Train, IncomingIn, NewIncomingOut);
		no -> loop(Name, PassengerListIn, PassengerListOut,
			   PlatformIn, PlatformOut, IncomingIn, IncomingOut)
	    end;
	{trainLeaving, ashmont, Train} -> 
            Train ! {trainLeft},
            loop(Name, PassengerListIn, PassengerListOut,
		 nil, PlatformOut, IncomingIn, IncomingOut);
	{trainLeaving, alewife, Train} ->
        Train ! {trainLeft},
	    loop(Name, PassengerListIn, PassengerListOut,
		 PlatformIn, nil, IncomingIn, IncomingOut);
	{numWaiting, ashmont, Train} ->
	    Train ! {numWaiting, length(PassengerListIn)},
            loop(Name, PassengerListIn, PassengerListOut,
		 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
	{numWaiting, alewife, Train} ->
	    Train ! {numWaiting, length(PassengerListOut)},
            loop(Name, PassengerListIn, PassengerListOut,
		 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
    	{endSim} -> ok
    end.

tryTrainEntry(Train, Queue, Platform) ->
	%% returns yes or no if entered
	%% sneds {enteredPlatform} if yes
	%%       {entryFailed} if no
    case Platform of
	nil -> {{value, FirstInQueue},_} = queue:out(Queue),
	       case FirstInQueue of
		   Train -> Train ! {enteredPlatform}, yes;
		   _ -> Train ! {entryFailed}, no
           end;
	_ -> Train ! {entryFailed}, no
    end.

alertPassengers(Train, Direction, PassengerList) ->
    %%
    lists:foreach(fun(Elem) -> Elem ! {train, Train, Direction} end, 
                    PassengerList).

