% Module: Station
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
%    12/03/14 - HCC - changed line to receive {endSim} to try cleaner way of 
%                     causing station processes to die

-module(station).
-export([start/1,loop/7]).

start(Name) ->
    % Spawns station with all basecases and registering station name
    IncomingAsh = queue:new(),
    IncomingAle = queue:new(),
    register(Name, spawn(fun() -> 
                    loop(Name,[], [], nil, nil, IncomingAsh, IncomingAle) end)),
    output:add(outMod, station).





































loop(Name, PassengerListAsh, PassengerListAle,
     PlatformAsh, PlatformAle, IncomingAsh, IncomingAle) ->
    receive
        {sendInfo} ->
            % Sends info to output
            case PlatformAsh of
                nil -> InCase = false;
                 _ -> InCase = true
            end,
            case PlatformAle of
                nil -> OutCase = false;
                _ -> OutCase = true
            end,
            output:newStationStat(outMod, 
                {Name, length(PassengerListAsh) + length(PassengerListAle),
                 InCase, OutCase}),
            loop(Name, PassengerListAsh, PassengerListAle,
                 PlatformAsh, PlatformAle, IncomingAsh, IncomingAle);
        {passengerEnters, Passenger, ashmont} ->
            % Add passenger to corresponding direction list
            NewPassengerList = [Passenger|PassengerListAsh],
            Passenger ! {train, PlatformAsh, ashmont},
            loop(Name, NewPassengerList, PassengerListAle,
                 PlatformAsh, PlatformAle, IncomingAsh, IncomingAle);
        {passengerEnters, Passenger, alewife} ->
            % Add passenger to corresponding direction list
            NewPassengerList = [Passenger|PassengerListAle],
            Passenger ! {train, PlatformAle, alewife},
            loop(Name, PassengerListAsh, NewPassengerList,
                 PlatformAsh, PlatformAle, IncomingAsh, IncomingAle);
        {passengerLeaves, Passenger, ashmont} ->
            % Remove passenger from corresponding direction list
            NewPassengerList = lists:delete(Passenger, PassengerListAsh),
            loop(Name, NewPassengerList, PassengerListAle,
                 PlatformAsh, PlatformAle, IncomingAsh, IncomingAle);
        {passengerLeaves, Passenger, alewife} ->
            % Remove passenger from corresponding direction list
            NewPassengerList = lists:delete(Passenger, PassengerListAle),
            loop(Name, PassengerListAsh, NewPassengerList,
                 PlatformAsh, PlatformAle, IncomingAsh, IncomingAle);
        {trainIncoming, Train, ashmont} ->
            % Add train to corresponding queue
            NewIncomingAsh = queue:in(Train, IncomingAsh),
            Train ! {inQueue},
            loop(Name, PassengerListAsh, PassengerListAle,
                 PlatformAsh, PlatformAle, NewIncomingAsh, IncomingAle);
        {trainIncoming, Train, alewife} ->
            % Add train to corresponding queue
            NewIncomingAle = queue:in(Train, IncomingAle),
            Train ! {inQueue},
            loop(Name, PassengerListAsh, PassengerListAle,
                 PlatformAsh, PlatformAle, IncomingAsh, NewIncomingAle);
        {trainEntry, Train, ashmont} ->
            % Check if train is first in corresponding queue
            case tryTrainEntry(Train, IncomingAsh, PlatformAsh) of
                % If so, remove from queue and alert passengers
                yes -> {_, NewIncomingAsh} = queue:out(IncomingAsh),
                    alertPassengers(Train, ashmont, PassengerListAsh),
                    loop(Name, PassengerListAsh, PassengerListAle,
                         Train, PlatformAle, NewIncomingAsh, IncomingAle);
                % If not, do nothing
                no -> loop(Name, PassengerListAsh, PassengerListAle,
                           PlatformAsh, PlatformAle, IncomingAsh, IncomingAle)
            end;

        {trainEntry, Train, alewife} ->
            % Check if train is first in corresponding queue
            case tryTrainEntry(Train, IncomingAle, PlatformAle) of
                % If so, remove from queue and alert passengers
                yes -> {_,NewIncomingAle} = queue:out(IncomingAle),
                    alertPassengers(Train, alewife, PassengerListAle),
                    loop(Name, PassengerListAsh, PassengerListAle,
                         PlatformAsh, Train, IncomingAsh, NewIncomingAle);
                % If not, do nothing
                no -> loop(Name, PassengerListAsh, PassengerListAle,
                           PlatformAsh, PlatformAle, IncomingAsh, IncomingAle)
            end;
        {trainLeaving, ashmont, Train} ->
            % Tell train it left successfully
            Train ! {trainLeft},
            % Set corresponding platform to nil
            loop(Name, PassengerListAsh, PassengerListAle,
                 nil, PlatformAle, IncomingAsh, IncomingAle);
        {trainLeaving, alewife, Train} ->
            % Tell train it left successfully
            Train ! {trainLeft},
            % Set corresponding platform to nil
            loop(Name, PassengerListAsh, PassengerListAle,
                 PlatformAsh, nil, IncomingAsh, IncomingAle);
        {numWaiting, ashmont, Train} ->
            % Tell train how many passengers are wanting to board
            Train ! {numWaiting, length(PassengerListAsh)},
            loop(Name, PassengerListAsh, PassengerListAle,
                 PlatformAsh, PlatformAle, IncomingAsh, IncomingAle);
        {numWaiting, alewife, Train} ->
            % Tell train how many passengers are wanting to board
            Train ! {numWaiting, length(PassengerListAle)},
            loop(Name, PassengerListAsh, PassengerListAle,
                 PlatformAsh, PlatformAle, IncomingAsh, IncomingAle);
        {endSim, Sender} -> lists:map(fun(Elem) -> exit(Elem, kill) end,
                            lists:append(PassengerListAsh, PassengerListAle)),
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

