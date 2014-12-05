% Module: Main
% Purpose: Parse specification for and run simulation
% Interface:
%    run/1 runs the simulation with parameters specified by the input file
% Original Author: Hannah Clark
% Date: 11/8/14
% ChangeLog:
%    12/05/14 - HCC - enforced 80 character rule
%    12/05/14 - HCC - changed to proper way of ending station processes
%    12/04/14 - HCC - changed start/1 to run/1 for clarity, cleaned up commented
%                     out lines
%    12/03/14 - HCC - eliminated debugging outputs (commented or deleted)
%    12/03/14 - HCC - changed start and parser so simulation ends when trains 
%                     are done and kills passenger processes still waiting in 
%                     stations
%    12/02/14 - HCC - fixed delay to not send minute done when about to
%                     remove from clock
%    12/01/14 - RAD - fixed a Pid message
%    11/24/14 - HCC - fixed bugs in code to exit simulation
%    11/23/14 - HCC - fixed bugs in delay loop function and procsAlive function
%    11/22/14 - HCC - fixed bugs in start due to order of function calls that 
%                     appeared in integration testing
%    11/19/14 - HCC - changes to return values from functions and their usage to
%                     be able to automatically end simulation
%    11/19/14 - HCC - fixed errors when inputting from file
%    11/17/14 - HCC - fixed functions to support delays
%    11/16/14 - HCC - added functions to support delays
%    11/08/14 - HCC - created module
-module(main).
-export([run/1]).

run(FileName) -> {ok, Device} = file:open(FileName, [read]), 
                   clock:init(clk), %Init clock so processes can add themselves
                                      %when created without errors
                    %Start output so processes can add themselves when created
                    %without errors
                    output:start(outMod, "outFile.txt"), 
                   lists:foreach(fun(Elem) -> station:start(Elem) end, 
                            carto:cartograph()), %make station processes
                   %Read file to make trains, passengers, and delays
                   {Delays, Trains, Passengers} = parseInput(Device, [], [], 
                                                    []),
                   %Create process to send delays, adding it to the clock
                    clock:add(clk, spawn(fun()-> delayLoop(Delays) end)),
                    %Everything is ready, so start clock's count
                    clock:startClock(clk), 
                   procsAlive(Trains),
                   output:endSimulation(outMod), %Everything is done, so output 
                                                %may be ended and clock will end
                   %cause stations to exit to clean up
                   lists:foreach(fun(Pid) ->  Pid ! {endSim, self()},
                        receive
                            ok -> ok
                        end
                        end, carto:cartograph()),
                    %eliminate any passengers that did not yet enter a station
                    lists:foreach(fun(Pid) -> exit(Pid, kill) end,
                        lists:filter(fun(Pid)->is_process_alive(Pid) end,
                            Passengers)),
                    ok.
                     
%Waits until all processes in a list are dead by filtering on alive processes at
%each recursion and ending when there are no longer any
procsAlive([]) -> ok;
procsAlive(Procs) ->
    procsAlive(lists:filter(fun(Proc) -> 
        is_process_alive(Proc) 
    end, Procs)).

%Parses input file and starts processes as indicated by file
%Procs is a list of Pids of trains and passengers created so far
%Delays is a list of Delays created so far
parseInput(Device, Delays, Trains, Passengers) -> 
    case io:fread(Device, "", "~a ")  of %read in a passenger or train
        {ok, [Name|_]} -> 
            case Name of
                train -> 
                    {ok, [Dir, Time, Cap, NumDelays]} = 
                        io:fread(Device, "", "~a ~10u ~10u ~10u"),
                    Train = train:start(Cap, Time, Dir, 
                        carto:firstStation(Dir)),
                    %Parse next input, adding train to procs, processing delays,
                    %if any, and adding them to delays
                   parseInput(Device, parseDelays(NumDelays, Train, Delays, 
                                Device), [Train|Trains], Passengers); 
                passenger -> 
                    {ok, [Count, Time, Start, End]} =
                        io:fread(Device, "", "~10u ~10u ~a ~a"),
                    %Parse next input, adding passengers to procs
                    parseInput(Device, Delays, Trains, lists:append(Passengers,
                      makeXPassengers(Count, Start, Time, End)))
            end;
        %Return delays and procs when nothing more to read in
        eof -> {Delays,Trains, Passengers}
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
delayLoop([]) -> 
  clock:remove(clk, self());
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
            %io:fwrite("delay loop minute done~n", []),
            if
                Remaining =:= [] ->  ok;
                true -> clk ! {minuteDone}
            end,
            delayLoop(Remaining)
    end.

