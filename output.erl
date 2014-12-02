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
%    12/01/14 - HCC - changed method by which sending station updates is triggered
%    11/24/14 - HCC - made messages synchronous  because of issue with scheduling
%    11/22/14 - HCC - fixed adding to clock in start
%    11/19/14 - HCC - changes made to endSim for correct ending of simulation
%    11/17/14 - HCC - many changes for debugging
%    11/16/14 - HCC - created module
-export([start/2, remove/2, add/2, newStationStat/2, newTrainStat/2,
        passengerDone/2, endSimulation/1]).

%Starts output module as process registered under ProcName and outputting 
%to file whose name is given as OutputFile
start(ProcName, OutputFile) -> 
    {ok, Device} = file:open(OutputFile, [write]),
    %Device = standard_io,
    Proc = spawn(fun() -> loop(0, 0, [], [], false, Device) end),
    clock:add(clk, Proc), %Synchronizes with clock so that output has meaning
    register(ProcName,Proc),
    {ok}.
%Removes a process of type station or train from being watched by output
remove(Proc, Type) -> Proc ! {remove, Type, self()},
                      receive
                         done -> ok
                      end. 
%Adds a process of type station or train from being watched by output
add(Proc, Type) -> Proc ! {add, Type, self()},
                    receive
                        done -> ok
                    end.
%Type should be atom of station or train only

%Call to flush output and close file when simulation is over
endSimulation(Proc) -> io:fwrite("end called ~n", []),
                        Proc ! {endSim, self()},
                        receive
                            Message -> Message
                        end.

%Use to provide a minute update from a station
newStationStat(Proc, Stats) -> %io:fwrite("s ~p~n", [self()]),%Stats: {StationName, NumPass, HasAsh, HasAle}
    Proc ! {stationStat, Stats, self()},
    receive
        done -> ok
    end.
%Use to provide a minute update from a train
newTrainStat(Proc, Stats) -> %Stats: {Dir, CurrLoc, NextOrCurrStation, NumPass}
                                % CurrLoc should be atom station if train is 
                                % on a platform or track if it is in queue for a
                                % platform
    %io:fwrite("t ~p~n", [self()]),
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
loop(TrainCnt, StationCnt, TrainStats, StationStats, _,  Device) 
    when (length(TrainStats) >= TrainCnt) and 
        (length(StationStats) >= StationCnt) and 
        ((TrainCnt > 0) or (StationCnt > 0)) ->
        
        printTrains(TrainStats, Device),
        printStations(StationStats, Device),
        case whereis(clk) of
            undefined -> io:fwrite("~p problem here~n", [whereis(clk)]);
            _ -> io:fwrite("clk ~p~n", [whereis(clk)]), clk ! {minuteDone}
        end,
        loop(TrainCnt, StationCnt, [], [], false, Device);

loop(TrainCnt, StationCnt, TrainStats, StationStats, false, Device)
    when (length(TrainStats) >= TrainCnt) and (TrainCnt > 0) ->
        lists:foreach(fun(Station) -> Station ! {sendInfo} end,
                        carto:cartograph()),
        loop(TrainCnt, StationCnt, TrainStats, StationStats, true, Device);
loop(TrainCnt, StationCnt, TrainStats, StationStats, ReqStations, Device) ->
    receive
        {add, train, Pid} -> Pid ! done,
                                loop(TrainCnt + 1, StationCnt, TrainStats, StationStats,
                                ReqStations, Device);
        {add, station, Pid} -> Pid ! done,
                                loop(TrainCnt, StationCnt + 1, TrainStats,
                                StationStats, ReqStations, Device);
        {remove, train, Pid} -> Pid ! done,
                                loop(TrainCnt - 1, StationCnt, TrainStats, 
                                StationStats, ReqStations, Device);
        {remove, station, Pid} -> Pid ! done,
                                loop(TrainCnt, StationCnt - 1, TrainStats, 
                                StationStats, ReqStations, Device);
        {tick, Minute} -> io:fwrite(Device, "Minute ~p~n", [Minute]),
                                if
                                    TrainCnt =:= 0 ->
                                        lists:foreach(fun(Station) -> 
                                            Station ! {sendInfo} end,
                                            carto:cartograph()),
                                        loop(TrainCnt, StationCnt, TrainStats, 
                                        StationStats, true, Device);
                                    true -> loop(TrainCnt, StationCnt, TrainStats, 
                                        StationStats, ReqStations, Device)
                                end;
        {trainStat, Stat, Pid} -> Pid ! done,
                                loop(TrainCnt, StationCnt, [Stat|TrainStats],
                                StationStats, ReqStations, Device);
        {stationStat, Stat, Pid} -> Pid ! done,
                                loop(TrainCnt, StationCnt, TrainStats,
                                [Stat|StationStats], ReqStations, Device);
        {passenger, PassInfo} -> printPassenger(PassInfo, Device),
                                loop(TrainCnt, StationCnt, TrainStats,
                                StationStats, ReqStations, Device);
        %At the end of the simulation, the remaining statistics should be printed
        %it should be taken off the clock because it is not relevant anymore
        %and it must close the file to save the output before ending
        {endSim, Sender} -> printTrains(TrainStats, Device),
                    printStations(StationStats, Device),
                    clock:remove(clk, self()),
                    Sender ! file:close(Device)
    end.
