-module(station).
-export([start/1,loop/5]).

start(Name) ->
    IncomingIn = queue:new(),
    IncomingOut = queue:new(),
    loop([], nil, {nil}, IncomingIn, IncomingOut).

loop(PassengerList, PlatformIn, PlatformOut, IncomingIn, IncomingOut) ->
	%% receives {passengerEnters, Passenger} -> update passengerlist
	%%	    {passengerLeaves, Passenger} -> update passengerlist
	%%	    {trainIncoming, Train, Direction} -> addincomingtrain
	%%	    {trainEntry, Train, Direction} -> trytrainentry
	%%		if yes -> alertPassengers
	%%	    {trainLeaving, Direction} -> update queue 
    receives
	{passengerEnters, Passenger} ->
	    NewPassengerList = [Passenger|PassengerList],
	    loop(NewPassengerList, PlatformIn, PlatformOut,
		 IncomingIn, IncomingOut);
	{passengerLeaves, Passenger} ->
	    NewPassengerList = lists:delete(Passenger, PassengerList),
	    loop(NewPassengerList, PlatformIn, PlatformOut,
		 IncomingIn, IncomingOut);
	{trainIncoming, Train, in} ->
	    NewIincomingIn = queue:in(Train, IncomingIn),
	    loop(PassengerList, PlatformIn, PlatformOut,
		 NewIncomingIn, IncomingOut);
	{trainIncoming, Train, out} ->
	    NewIncomingOut = queue:in(Train, IncomingOut),
	    loop(PassengerList, PlatformIn, PlatformOut,
		 IncomingIn, NewIncomingOut);
	{trainEntry, Train, in} ->
	    case tryTrainEntry(Train, IncomingIn, PlatformIn) of
		yes -> {_, NewIncomingIn} = queue:out(IncomingIn),
		       alertPassengers(Train, in, PassengerList),
		       loop(PassengerList, Train, PlatformOut,
			    NewIncomingIn, IncomingOut);
		no -> loop(PassengerList, PlatformIn, PlatformOut,
			   IncomingIn, IncomingOut);
	    end;
	{trainEntry, Train, out} ->
	    case tryTrainEntry(Train, IncomingOut, PlatformOut) of
		yes -> {_,NewIncomingOut} = queue:out(IncomingOut),
		       alertPassengers(Train, out, PassengerList),
		       loop(PassengerList, PlatformIn, Train,
			    IncomingIn, NewIncomingOut);
		no -> loop(PassengerList, PlatformIn, PlatformOut,
			   IncomingIn, IncomingOut);
	    end;
	{trainLeaving, in} ->
	    loop(PassengerList, nil, PlatformOut,
		 IncomingIn, IncomingOut);
	{trainLeaving, out} ->
	    loop(PassengerList, PlatformIn, nil,
		 IncomingIn, IncomingOut);


tryTrainEntry(Train, Queue, Platform) ->
	%% returns yes or no if entered
	%% sneds {enteredPlatform} if yes
	%%       {entryFailed} if no
    case Platform of
	nil -> {{Val, FirstInQueue},_} = queue:out(Queue),
	       case FirstInQueue of
		   Train -> Train ! {enteredPlatform}, yes;
		   _ -> Train ! {entryFailed}, no;
	_ -> Train ! {entryFailed}, no
    end.

alertPassengers(Train, Direction, PassengerList) ->
	%% sends {train, Train, Direction} to passengerlist
    lists:foreach(fun(Elem) -> Elem ! {train, Train, Direction} end).
