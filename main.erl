-module(main).
-export([start/1]).

start(FileName) -> {ok, Device} = file:open(FileName, [read]), 
                   clock:init(clk), %Init clock so processes can add themselves
                                      %when created without errors
                   output:start(outMod, "outFile.txt"), %Start output so processes can add 
                                      %themselves when created without errors
                   lists:foreach(fun(Elem) -> station:start(Elem) end, 
                            carto:cartograph()), %make station processes
                   {Delays, Procs} = parseInput(Device, [], []),
                                      %Read file to make trains, passengers, and delays
                   clock:add(clk, spawn(fun()->
                                    delayLoop(Delays) end)),
                                    %Create process to send delays, adding it to the clock
                                    %so that it sends them appropriately
                   clock:startClock(clk), %Everything is ready, so start clock's count
                   procsAlive(Procs),
                   output:endSimulation(outMod), %Everything is done, so output may be ended
                   lists:foreach(fun(Pid) -> clock:remove(clk, Pid),
                                    exit(whereis(Pid), simDone)
                        end, carto:cartograph()), %Remove remaining processes from clock
                                      %and cause them to exit so that the clock will stop
                    ok.
                     
%Waits until all processes in a list are dead by filtering on alive processes at each recursion
%and ending when there are no longer any
procsAlive([]) -> ok;
procsAlive(Procs) -> procsAlive(lists:filter(fun(Proc) -> is_process_alive(Proc) end, Procs)).

%Parses input file and starts processes as indicated by file
%Procs is a list of Pids of trains and passengers created so far
%Delays is a list of Delays created so far
parseInput(Device, Delays, Procs) -> 
    case io:fread(Device, "", "~a ")  of %read in a passenger or train
        {ok, [Name|_]} -> 
            case Name of
                train -> 
                    {ok, [Dir, Time, Cap, NumDelays]} = 
                        io:fread(Device, "", "~a ~10u ~10u ~10u"),
                    Train = train:start(Cap, Time, Dir, carto:firstStation(Dir)),
                    parseInput(Device, parseDelays(NumDelays, Train, Delays, 
                                Device), [Train|Procs]); %Parse next input,
                                %adding train to procs, processing delays, if any,
                                % and adding them to delays
                passenger -> 
                    {ok, [Count, Time, Start, End]} =
                        io:fread(Device, "", "~10u ~10u ~a ~a"),
                    parseInput(Device, Delays, lists:append(Procs,
                        makeXPassengers(Count, Start, Time, End)))
                                      %Parse next input, adding passengers to procs
            end;
        eof -> {Delays,Procs} %Return delays and procs when nothing more to read in
    end.

%Function to parse a number of delays belonging to train whose pid is Train
%from a file, returns list of tuples representing delays
parseDelays(0, _, Delays, _) -> Delays;
parseDelays(NumDelays, Train, Delays, Device) -> 
    {ok, [Time, Length]} = io:fread(Device, "", "delay ~10u ~10u"),
                                      %Read delay
    parseDelays(NumDelays - 1, Train, [{Train, Time, Length}|Delays], Device).
                                      %add delay to list and read next

%Function to produce number of passengers with starting specifications
%given in the parameters and return a list of their Pids
makeXPassengers(0, _, _, _) -> [];
makeXPassengers(X, StartStation, StartTime, EndStation) ->
    [passenger:start(StartStation, StartTime, EndStation)|
    makeXPassengers(X-1, StartStation, StartTime, EndStation)].

%Sends delays to trains as appropriate
delayLoop([]) -> clock:remove(clk, self());
delayLoop(Delays) ->
    receive %Every minute, iterate over list of delays
           %send any for that minute, call self with list of remaining ones
        {tick, Minute} -> Remaining = lists:foldr(
            fun({Pid, Time, Length}, Rem) ->
                case Time of
                    Minute -> Pid ! {delay, Length},
                              Rem;
                    _ -> [{Pid, Time, Length}|Rem]
                end
            end, [], Delays),
            clk ! {minuteDone},
            delayLoop(Remaining)
    end.

