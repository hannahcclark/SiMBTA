-module(main).
-export([start/1]).

start(FileName) -> {ok, Device} = file:open(FileName, [read]),
                   clock:init(clk),
                   output:start(outMod, "outFile.txt"),
                   lists:map(fun(Elem) -> station:start(Elem) end, 
                            carto:cartograph()),
                   {Delays, Procs} = parseInput(Device, [], []),
                   clock:add(clk, spawn(fun()->
                                    delayLoop(Delays) end)),
                   clock:startClock(clk),
                   %procsAlive(Procs),
                   output:endSim(outMod),
                   lists:map(fun(Pid) -> clock:remove(clk, Pid),
                                    exit(Pid, simDone)
                        end, carto:cartograph()).
%procsAlive([]) -> ok;
%procsAlive(Procs) -> procsAlive(lists:filter(is_process_alive, Procs)).
parseInput(Device, Delays, Procs) -> 
    case io:fread(Device, "", "~a ")  of
        {ok, [Name|_]} -> 
            case Name of
                train -> 
                    {ok, [Dir, Time, Cap, NumDelays]} = 
                        io:fread(Device, "", "~a ~10u ~10u ~10u"),
                    Train = train:start(Cap, Time, Dir, carto:firstStation(Dir)),
                    parseInput(Device, parseDelays(NumDelays, Train, Delays, 
                                Device), [Train|Procs]);
                passenger -> 
                    {ok, [Count, Time, Start, End]} =
                        io:fread(Device, "", "~10u ~10u ~a ~a"),
                    parseInput(Device, Delays, lists:append(Procs,
                        makeXPassengers(Count, Start, Time, End)))
            end;
        eof -> {Delays,Procs}
    end.

parseDelays(0, _, Delays, _) -> Delays;
parseDelays(NumDelays, Train, Delays, Device) -> 
    {ok, [Time, Length]} = io:fread(Device, "", "delay ~10u ~10u"),
    parseDelays(NumDelays - 1, Train, [{Train, Time, Length}|Delays], Device).

makeXPassengers(0, _, _, _) -> [];
makeXPassengers(X, StartStation, StartTime, EndStation) ->
    [passenger:start(StartStation, StartTime, EndStation)|
    makeXPassengers(X-1, StartStation, StartTime, EndStation)].

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

