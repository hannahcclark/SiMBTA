-module(station).
-export([start/1,loop/6]).

start(Name) ->
    IncomingIn = queue:new(),
    IncomingOut = queue:new(),
    io:fwrite("reg~n", []),
    register(Name, spawn(fun() -> 
                    loop(Name, [], nil, nil, IncomingIn, IncomingOut) end)),
    clock:add(clk, Name),
    output:add(outMod, station).

loop(Name, PassengerList, PlatformIn, PlatformOut, IncomingIn, IncomingOut) ->
	%% receives {passengerEnters, Passenger} -> update passengerlist
	%%	    {passengerLeaves, Passenger} -> update passengerlist
	%%	    {trainIncoming, Train, Direction} -> addincomingtrain
	%%	    {trainEntry, Train, Direction} -> trytrainentry
	%%		if yes -> alertPassengers
	%%	    {trainLeaving, Direction} -> update queue 
    receive
        {tick, Minute} ->
            case PlatformIn of
                nil -> InCase = false;
                 _ -> InCase = true
            end,
            case PlatformOut of
                nil -> OutCase = false;
                _ -> OutCase = true
            end,
            output:newStationStat(outMod, 
                   {Name, length(PassengerList), InCase, OutCase}),
            case whereis(clk) of
            undefined -> ok;
            _ -> clk ! {minuteDone}
            end,
            loop(Name, PassengerList, PlatformIn, PlatformOut, IncomingIn, 
                    IncomingOut);
	{passengerEnters, Passenger} -> 
	    NewPassengerList = [Passenger|PassengerList],
	    loop(Name, NewPassengerList, PlatformIn, PlatformOut,
		 IncomingIn, IncomingOut);
	{passengerLeaves, Passenger} -> 
	    NewPassengerList = lists:delete(Passenger, PassengerList),
	    loop(Name, NewPassengerList, PlatformIn, PlatformOut,
		 IncomingIn, IncomingOut);
	{trainIncoming, Train, ashmont} -> 
	    NewIncomingIn = queue:in(Train, IncomingIn),
	    loop(Name, PassengerList, PlatformIn, PlatformOut,
		 NewIncomingIn, IncomingOut);
	{trainIncoming, Train, alewife} ->
	    NewIncomingOut = queue:in(Train, IncomingOut),
	    loop(Name, PassengerList, PlatformIn, PlatformOut,
		 IncomingIn, NewIncomingOut);
	{trainEntry, Train, ashmont} -> 
	    case tryTrainEntry(Train, IncomingIn, PlatformIn) of
		yes -> {_, NewIncomingIn} = queue:out(IncomingIn),
		       alertPassengers(Train, ashmont, PassengerList),
		       loop(Name, PassengerList, Train, PlatformOut,
			    NewIncomingIn, IncomingOut);
		no -> loop(Name, PassengerList, PlatformIn, PlatformOut,
			   IncomingIn, IncomingOut)
	    end;
	{trainEntry, Train, alewife} ->
	    case tryTrainEntry(Train, IncomingOut, PlatformOut) of
		yes -> {_,NewIncomingOut} = queue:out(IncomingOut),
		       alertPassengers(Train, alewife, PassengerList),
		       loop(Name, PassengerList, PlatformIn, Train,
			    IncomingIn, NewIncomingOut);
		no -> loop(Name, PassengerList, PlatformIn, PlatformOut,
			   IncomingIn, IncomingOut)
	    end;
	{trainLeaving, ashmont} -> 
            loop(Name, PassengerList, nil, PlatformOut,
		 IncomingIn, IncomingOut);
	{trainLeaving, alewife} ->
	    loop(Name, PassengerList, PlatformIn, nil,
		 IncomingIn, IncomingOut);
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
