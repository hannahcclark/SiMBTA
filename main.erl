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
                    {ok, [Dir, Cap, Time]} = 
                        io:fread(Device, "", "~a ~10u ~10u~n"),
                    train:start(Cap, Time, Dir, carto:firstStation(Dir)),
                    parseInput(Device);
                passenger -> 
                    {ok, [Count, Time, Start, End]} =
                        io:fread(Device, "", "~10u ~10u ~a ~a"),
                    makeXPassengers(Count, Start, End)
            end
        eof -> Delays
    end.

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

