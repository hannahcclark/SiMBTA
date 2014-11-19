-module(main).
-export([start/1]).

start(FileName) -> lists:map(station:start, carto:cartograph()),
                   {ok, Device} = file:open(FileName, [read]),
                   clock:start(clk),
                   clock:add(clk, 
                       spawn(fun->delayLoop(parseInput(Device, [])) end),
                   clock:startClock(clk),
                   {ok}.

parseInput(Device, Delays) -> 
    Type = io:fread(Device, "", "~a "),
    case Type
        {ok, [Name|_]} -> 
            case Name
                train -> 
                    {ok, [Dir, Cap, Time, NumDelays]} = 
                        io:fread(Device, "", "~a ~10u ~10u ~10u~n"),
                    parseInput(Device, parseDelays(NumDelays, train:start(Cap,
                                Time, Dir, carto:firstStation(Dir)), Delays),
                passenger -> 
                    {ok, [Count, Time, Start, End]} =
                        io:fread(Device, "", "~10u ~10u ~a ~a"),
                    makeXPassengers(Count, Start, End),
                    parseInput(Device, Delays)
            end
        eof -> Delays
    end.

parseDelays(0, _, Delays) -> Delays;
parseDelays(NumDelays, Train, Delays) -> 
    {ok, [Time, Length]} = io:fread(Device, "", "delay ~10u ~10u~n"),
    parseDelays(NumDelays - 1, Train, [{Train, Time, Length}|Delays]).

makeXPassengers(0, StartStation, StartTime, EndStation) -> {ok};
makeXPassengers(X, StartStation, StartTime, EndStation) ->
    passenger:start(StartStation, StartTime, EndStation),
    makeXPassengers(X-1, StartStation, StartTime, EndStation).

delayLoop([]) -> clock:remove(clk, self());
delayLoop(Delays) ->
    recieve
        {clockTick, Minute} -> delayLoop(lists:foldr(
            fun({Pid, Time, Length}, Rem) ->
                case Time of
                    Minute -> Pid ! {delay, Length},
                              Rem;
                    _ -> [{Pid, Time, Length}|Rem]
                end
            end, [], Delays)
    end.

