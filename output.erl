-module(output).
-export([start/2, remove/2, add/2, newStationStat/2, newTrainStat/2,
        endSimulation/1]).

start(ProcName, OutputFile) -> 
    {ok, Device} = file:open(OutputFile, [write]),
    register(ProcName,
            spawn(fun() -> loop(0, 0, [], [], Device) end)),
    {ok}.

remove(Proc, Type) -> Proc ! {remove, Type}.
add(Proc, Type) -> Proc ! {add, Type}.
endSimulation(Proc) -> Proc ! {endSim}.
newStationStat(Proc, Stats) -> %Stats: {StationName, NumPass, HasAsh, HasAle}
    Proc ! {stationStat, Stats}.
newTrainStat(Proc, Stats) -> %Stats: {Dir, CurrLoc, NextOrCurrStation, NumPass}
                                % CurrLoc should be atom station or track
    Proc ! {trainStat, Stats}.

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

printStations([], _) -> {ok};
printStations([{StationName, NumPass, HasAsh, HasAle}|Stations], Device) ->
    io:fwrite(Device, "station Name:~p Passengers:~p AshTrain:~p AleTrain:~p~n",
                    [StationName, NumPass, HasAsh, HasAle]),
    printStations(Stations, Device).

loop(TrainCnt, StationCnt, TrainStats, StationStats, Device) 
    when (length(TrainStats) =:= TrainCnt) and 
    (length(StationStats) =:= StationCnt) ->
    printTrains(TrainStats, Device),
    printStations(StationStats, Device),
    loop(TrainCnt, StationCnt, [], [], Device);
loop(TrainCnt, StationCnt, TrainStats, StationStats, Device) ->
    receive
        {add, train} -> loop(TrainCnt + 1, StationCnt, TrainStats, StationStats,
                                Device);
        {add, station} -> loop(TrainCnt, StationCnt + 1, TrainStats,
                                StationStats, Device);
        {remove, train} -> loop(TrainCnt - 1, StationCnt, TrainStats, 
                                StationStats, Device);
        {remove, station} -> loop(TrainCnt, StationCnt - 1, TrainStats, 
                                StationStats, Device);
        {clockTick, Minute} -> io:fwrite(Device, "Minute ~p~n", [Minute]),
                                loop(TrainCnt, StationCnt, TrainStats, 
                                    StationStats, Device);
        {trainStat, Stat} -> loop(TrainCnt, StationCnt, [Stat|TrainStats],
                                StationStats, Device);
        {stationStat, Stat} -> loop(TrainCnt, StationCnt, TrainStats,
                                [Stat|StationStats], Device);

        {endSim} -> printTrains(TrainStats, Device),
                    printStations(StationStats, Device),
                    file:close(Device),
                    {ok}
    end.
