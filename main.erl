-module(main).
-export([start/1]).

start(FileName) -> lists:map(station:start, carto:cartograph()),
                   {ok, Device} = file:open(FileName, [read]),
                   ClockAtom = "clockProc",
                   clock:start(ClockAtom),
                   parseInput(Device, ClockAtom),
                   clock:startClock(ClockAtom),
                   {ok}.

parseInput(Device, ClockAtom) -> 
    Type = io:fread(Device, "", "~a "),
    case Type
        {ok, [Name|_]} -> 
            case Name
                train -> 
                    {ok, [Dir, Cap, Time]} = 
                        io:fread(Device, "", "~a ~10u ~10u~n"),
                    train:start(Cap, Time, Dir, carto:firstStation(Dir), 
                        ClockAtom),
                    parseInput(Device, ClockAtom);
                passenger -> 
                    {ok, [Count, Time, Start, End]} =
                        io:fread(Device, "", "~10u ~10u ~a ~a"),
                    makeXPassengers(Count, Start, End, ClockAtom)
            end
        eof -> {ok}
    end.

makeXPassengers(0, StartStation, StartTime, EndStation, ClockAtom) -> {ok};
makeXPassengers(X, StartStation, StartTime, EndStation, ClockAtom) ->
    passenger:start(StartStation, StartTime, EndStation, ClockAtom),
    makeXPassengers(X-1, StartStation, StartTime, EndStation, ClockAtom.
    
