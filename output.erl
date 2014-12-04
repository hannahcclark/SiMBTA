% Module: Output
% Purpose: Handle output to results file for simulation
% Interface:
%   start/2 creates a process to handle output registered under the specified name and outputing to the specified file
%   add/2 adds a process, either a station or train, that will be sending output updates
%   remove/2 removes a process, either a station or a train, that had been sending updates
%   newStationStat/2 is used by stations to send updates
%   newTrainStat/2 is used by trains to send updates
%   passengerDone/2 is used by passenger to send trip end information
%   endSimulation/1 is used to end output process
% Original Author: Hannah Clark
% Date: 11/16/14
% ChangeLog:
%    12/04/14 - HCC - cleaned up commented out lines
%    12/03/14 - HCC - eliminated debugging outputs (commented or deleted)
%    12/03/14 - HCC - changed requesting of station info to be in if statement rather than recieve
%    12/03/14 - HCC - added case for train removal in loop where there are no more trains
%    12/02/14 - HCC - added message that output is allowed (from clock) to fix issue of trains not adding correctly
%    12/02/14 - AMS - adding module definition at top
%    12/01/14 - HCC - changed method by which sending station updates is triggered
%    11/24/14 - HCC - made messages synchronous  because of issue with scheduling
%    11/22/14 - HCC - fixed adding to clock in start
%    11/19/14 - HCC - changes made to endSim for correct ending of simulation
%    11/17/14 - HCC - many changes for debugging
%    11/16/14 - HCC - created module
-module(output).
-export([start/2, remove/2, add/2, newStationStat/2, newTrainStat/2,
        passengerDone/2, endSimulation/1, canWrite/1]).

%Starts output module as process registered under ProcName and outputting 
%to file whose name is given as OutputFile
start(ProcName, OutputFile) -> 
    {ok, Device} = file:open(OutputFile, [write]),
    Proc = spawn(fun() -> loop(0, 0, [], [], false, Device, false, false) end),
    clock:add(clk, Proc), %Synchronizes with clock so that output has meaning
    register(ProcName,Proc),
    {ok}.
canWrite(Proc) -> Proc ! {canWrite}.
%Removes a process of type station or train from being watched by output
remove(Proc, Type) -> Proc ! {remove, Type, self()},
                      receive
                         done -> ok
                      end. 
%Adds a process of type station or train from being watched by output
add(Proc, Type) ->  
                Proc ! {add, Type, self()},
                    receive
                        done ->  ok
                    end.
%Type should be atom of station or train only

%Call to flush output and close file when simulation is over
endSimulation(Proc) -> Proc ! {endSim, self()},
                        receive
                            Message -> Message
                        end.

%Use to provide a minute update from a station
newStationStat(Proc, Stats) -> 
    Proc ! {stationStat, Stats, self()},
    receive
        done -> ok
    end.
%Use to provide a minute update from a train
newTrainStat(Proc, Stats) -> %Stats: {Dir, CurrLoc, NextOrCurrStation, NumPass}
                                % CurrLoc should be atom station if train is 
                                % on a platform or track if it is in queue for a
                                % platform
    Proc ! {trainStat, Stats, self()},
    receive
        done -> ok
    end.
%Use to provide completed journey info from a passenger
passengerDone(Proc, PassInfo) -> Proc ! {passenger, PassInfo}.
                        %PassInfo: {StartStation, EndStation, BegTime, Duration}
                        
%Outputs all train updates from time period passed in to output file in format
%indicated by outputFormat.txt
printTrains([], _) -> {ok};
printTrains([{Dir, CurrLoc, Station, Pass}|Trains], Device) ->
    case CurrLoc of
        station -> io:fwrite(Device, 
                        "train Direction:~p Station:~p Passengers:~p~n",
                        [Dir, Station, Pass]);
        track -> io:fwrite(Device, 
                        "train Direction: ~p Approaching:~p Passengers:~p~n",
                        [Dir, Station, Pass])
    end,
    printTrains(Trains, Device).

%Outputs all station updates from time period passed in to output file in format 
%indicated by outputFormat.txt
printStations([], _) -> {ok};
printStations([{StationName, NumPass, HasAsh, HasAle}|Stations], Device) ->
    io:fwrite(Device, "station Name:~p Passengers:~p AshTrain:~p AleTrain:~p~n",
                    [StationName, NumPass, HasAsh, HasAle]),
    printStations(Stations, Device).
    
%Outputs a single passenger's information to output file in format 
%indicated by outputFormat.txt
printPassenger({Start, Dest, Time, Dur}, Device) ->
    io:fwrite(Device, "passenger Start:~p End:~p Began:~p Duration:~p~n",
                [Start, Dest, Time, Dur]).

%Action loop for output
%Case that all station and train updates are in for the given minute
%Because loop is called before there are any stations or trains added,
%Either at least one train or station must have been added for this
%to be relevant and not called when there is nothing to output
%In this case, all updates are printed and cleared, and the output
%process has completed its work for the minute
loop(TrainCnt, StationCnt, TrainStats, StationStats, _,  Device, true, true) 
    when (length(TrainStats) >= TrainCnt) and 
        (length(StationStats) >= StationCnt) and 
        ((TrainCnt > 0) or (StationCnt > 0)) ->
        
        printTrains(TrainStats, Device),
        printStations(StationStats, Device),
        clk ! {minuteDone},
        loop(TrainCnt, StationCnt, [], [], false, Device, false, false);

loop(TrainCnt, StationCnt, TrainStats, StationStats, false, Device, 
MinuteOccurred, CanWrite) when (length(TrainStats) >= TrainCnt) and (TrainCnt > 0) ->
        lists:foreach(fun(Station) -> Station ! {sendInfo} end,
                        carto:cartograph()),
        loop(TrainCnt, StationCnt, TrainStats, StationStats, true, Device, 
            MinuteOccurred, CanWrite);
loop(TrainCnt, StationCnt, TrainStats, StationStats, ReqStations, Device, 
    MinuteOccurred, CanWrite) ->
    if 
        (not ReqStations) and MinuteOccurred and CanWrite and ((TrainCnt =:= 0) 
        or (length(TrainStats) =:= TrainCnt)) -> 
            lists:foreach(fun(Station) -> Station ! {sendInfo} end, carto:cartograph()),
            loop(TrainCnt, StationCnt, TrainStats, StationStats, true, Device, MinuteOccurred, CanWrite);
        true -> 
    receive
        {add, train, Pid} -> Pid ! done,
                                loop(TrainCnt + 1, StationCnt, TrainStats, StationStats,
                                ReqStations, Device, MinuteOccurred, CanWrite);
        {add, station, Pid} -> Pid ! done,
                                loop(TrainCnt, StationCnt + 1, TrainStats,
                                StationStats, ReqStations, Device, MinuteOccurred, CanWrite);
        {remove, train, Pid} -> Pid ! done,
                                loop(TrainCnt - 1, StationCnt, TrainStats, 
                                StationStats, ReqStations, Device, MinuteOccurred, CanWrite);
        {remove, station, Pid} -> Pid ! done,
                                loop(TrainCnt, StationCnt - 1, TrainStats, 
                                StationStats, ReqStations, Device, MinuteOccurred, CanWrite);
        {tick, Minute} -> io:fwrite(Device, "Minute ~p~n", [Minute]),
                            loop(TrainCnt, StationCnt, TrainStats, 
                                        StationStats, ReqStations, Device, true, CanWrite);
        {trainStat, Stat, Pid} -> Pid ! done, 
                                loop(TrainCnt, StationCnt, [Stat|TrainStats],
                                StationStats, ReqStations, Device, MinuteOccurred, CanWrite);
        {stationStat, Stat, Pid} -> Pid ! done,
                                loop(TrainCnt, StationCnt, TrainStats,
                                [Stat|StationStats], ReqStations, Device, MinuteOccurred, CanWrite);
        {passenger, PassInfo} -> printPassenger(PassInfo, Device),
                                loop(TrainCnt, StationCnt, TrainStats,
                                StationStats, ReqStations, Device, MinuteOccurred, CanWrite);
        {canWrite} -> loop(TrainCnt, StationCnt, TrainStats, StationStats, ReqStations,
                            Device, MinuteOccurred, true); %sent by clock when all but output has reported minute done
                            %This ensures that add messages will be received because otherwise the train wouldn't have reported it's minute done because of synchronous message passing function
        %At the end of the simulation, the remaining statistics should be printed
        %it should be taken off the clock because it is not relevant anymore
        %and it must close the file to save the output before ending
        {endSim, Sender} -> printTrains(TrainStats, Device),
                    printStations(StationStats, Device),
                    clearPassengers(Device),
                    clock:remove(clk, self()),
                    Sender ! file:close(Device)
    end
    end.

clearPassengers(Device) ->
receive
    {passenger, PassInfo} -> printPassenger(PassInfo, Device), clearPassengers(Device)
    after 0 -> ok
    end.
