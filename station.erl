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
    % Spawns station with all basecases and registering station name
    IncomingIn = queue:new(),
    IncomingOut = queue:new(),
    %io:fwrite("reg~n", []),
    register(Name, spawn(fun() -> 
                    loop(Name,[], [], nil, nil, IncomingIn, IncomingOut) end)),
    output:add(outMod, station).

loop(Name, PassengerListIn, PassengerListOut,
     PlatformIn, PlatformOut, IncomingIn, IncomingOut) ->
    receive
        {sendInfo} ->
            % Sends info to output
            case PlatformIn of
                nil -> InCase = false;
                 _ -> InCase = true
            end,
            case PlatformOut of
                nil -> OutCase = false;
                _ -> OutCase = true
            end,
            output:newStationStat(outMod, 
                {Name, length(PassengerListIn) + length(PassengerListOut),
                 InCase, OutCase}),
            loop(Name, PassengerListIn, PassengerListOut,
                 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
        {passengerEnters, Passenger, ashmont} ->
            % Add passenger to corresponding direction list
            NewPassengerList = [Passenger|PassengerListIn],
            Passenger ! {train, PlatformIn, ashmont},
            loop(Name, NewPassengerList, PassengerListOut,
                 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
        {passengerEnters, Passenger, alewife} ->
            % Add passenger to corresponding direction list
            NewPassengerList = [Passenger|PassengerListOut],
            Passenger ! {train, PlatformOut, alewife},
            loop(Name, PassengerListIn, NewPassengerList,
                 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
        {passengerLeaves, Passenger, ashmont} ->
            % Remove passenger from corresponding direction list
            NewPassengerList = lists:delete(Passenger, PassengerListIn),
            loop(Name, NewPassengerList, PassengerListOut,
                 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
        {passengerLeaves, Passenger, alewife} ->
            % Remove passenger from corresponding direction list
            NewPassengerList = lists:delete(Passenger, PassengerListOut),
            loop(Name, PassengerListIn, NewPassengerList,
                 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
        {trainIncoming, Train, ashmont} ->
            % Add train to corresponding queue
            NewIncomingIn = queue:in(Train, IncomingIn),
            Train ! {inQueue},
            loop(Name, PassengerListIn, PassengerListOut,
                 PlatformIn, PlatformOut, NewIncomingIn, IncomingOut);
        {trainIncoming, Train, alewife} ->
            % Add train to corresponding queue
            NewIncomingOut = queue:in(Train, IncomingOut),
            Train ! {inQueue},
            loop(Name, PassengerListIn, PassengerListOut,
                 PlatformIn, PlatformOut, IncomingIn, NewIncomingOut);
        {trainEntry, Train, ashmont} ->
            % Check if train is first in corresponding queue
            case tryTrainEntry(Train, IncomingIn, PlatformIn) of
                % If so, remove from queue and alert passengers
                yes -> {_, NewIncomingIn} = queue:out(IncomingIn),
                    alertPassengers(Train, ashmont, PassengerListIn),
                    loop(Name, PassengerListIn, PassengerListOut,
                         Train, PlatformOut, NewIncomingIn, IncomingOut);
                % If not, do nothing
                no -> loop(Name, PassengerListIn, PassengerListOut,
                           PlatformIn, PlatformOut, IncomingIn, IncomingOut)
            end;
        {trainEntry, Train, alewife} ->
            % Check if train is first in corresponding queue
            case tryTrainEntry(Train, IncomingOut, PlatformOut) of
                % If so, remove from queue and alert passengers
                yes -> {_,NewIncomingOut} = queue:out(IncomingOut),
                    alertPassengers(Train, alewife, PassengerListOut),
                    loop(Name, PassengerListIn, PassengerListOut,
                         PlatformIn, Train, IncomingIn, NewIncomingOut);
                % If not, do nothing
                no -> loop(Name, PassengerListIn, PassengerListOut,
                           PlatformIn, PlatformOut, IncomingIn, IncomingOut)
            end;
        {trainLeaving, ashmont, Train} ->
            % Tell train it left successfully
            Train ! {trainLeft},
            % Set corresponding platform to nil
            loop(Name, PassengerListIn, PassengerListOut,
                 nil, PlatformOut, IncomingIn, IncomingOut);
        {trainLeaving, alewife, Train} ->
            % Tell train it left successfully
            Train ! {trainLeft},
            % Set corresponding platform to nil
            loop(Name, PassengerListIn, PassengerListOut,
                 PlatformIn, nil, IncomingIn, IncomingOut);
        {numWaiting, ashmont, Train} ->
            % Tell train how many passengers are wanting to board
            Train ! {numWaiting, length(PassengerListIn)},
            loop(Name, PassengerListIn, PassengerListOut,
                 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
        {numWaiting, alewife, Train} ->
            % Tell train how many passengers are wanting to board
            Train ! {numWaiting, length(PassengerListOut)},
            loop(Name, PassengerListIn, PassengerListOut,
                 PlatformIn, PlatformOut, IncomingIn, IncomingOut);
        {endSim, Sender} -> %io:fwrite("stationdone~n", []), 
                            Sender ! ok % End process at end of simulation
    end.

tryTrainEntry(Train, Queue, Platform) ->
    % Check if platform empty
    case Platform of
        % If so, get first train from queue
        nil -> {{value, FirstInQueue},_} = queue:out(Queue),
            % Check if first in queue is train
            case FirstInQueue of
                % If so, send train confirmation of entering platform
                Train -> Train ! {enteredPlatform}, yes;
                % If not, send train failure message
                _ -> Train ! {entryFailed}, no
            end;
        % If not, send train failure message
        _ -> Train ! {entryFailed}, no
    end.

alertPassengers(Train, Direction, PassengerList) ->
    % Alert all passengers in the list of a train arriving
    lists:foreach(fun(Elem) -> Elem ! {train, Train, Direction} end, 
                    PassengerList).

