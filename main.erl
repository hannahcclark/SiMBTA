-module(main).
-export([start/1]).

start(FileName) -> lists:map(station:start, carto:cartograph()),
                   {ok, Device} = file:open(FileName, [read]),
                   clock:start(clk),
                   clock:add(clk, spawn(fun()->
                                    delayLoop(parseInput(Device, [])) end),
                   clock:startClock(clk),
                   {ok}.

parseInput(Device, Delays) -> 
    case io:fread(Device, "", "~a ")  of
        {ok, [Name|_]} -> 
            case Name of
                train -> 
                    {ok, [Dir, Time, Cap, NumDelays]} = 
                        io:fread(Device, "", "~a ~10u ~10u ~10u"),
                    parseInput(Device, parseDelays(NumDelays, train:start(Cap,
                    Time, Dir, carto:firstStation(Dir)), Delays, Device);
                passenger -> 
                    {ok, [Count, Time, Start, End]} =
                        io:fread(Device, "", "~10u ~10u ~a ~a"),
                    makeXPassengers(Count, Start, Time, End),
                    parseInput(Device, Delays)
            end;
        eof -> Delays
    end.

parseDelays(0, _, Delays, _) -> Delays;
parseDelays(NumDelays, Train, Delays, Device) -> 
    {ok, [Time, Length]} = io:fread(Device, "", "delay ~10u ~10u"),
    parseDelays(NumDelays - 1, Train, [{Train, Time, Length}|Delays], Device).

makeXPassengers(0, _, _, _) -> {ok};
makeXPassengers(X, StartStation, StartTime, EndStation) ->
    passenger:start(StartStation, StartTime, EndStation),
    makeXPassengers(X-1, StartStation, StartTime, EndStation).

delayLoop([]) -> clock:remove(clk, self());
delayLoop(Delays) ->
    receive
        {clockTick, Minute} -> delayLoop(lists:foldr(
            fun({Pid, Time, Length}, Rem) ->
                case Time of
                    Minute -> Pid ! {delay, Length},
                              Rem;
                    _ -> [{Pid, Time, Length}|Rem]
                end
            end, [], Delays))
    end.

