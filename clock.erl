% Module: Clock
% Purpose: Maintains the simulation's concept of time and notify objects watching when it changes
% Interface:
%    init/1 spawns the clock process that will not count until clock is started
%    add/2 adds a process to processes watching the clock
%    remove/2 removes a process from the processes watching the clock
%    startClock/1 begins incrementing of time
%    currTime/1 returns the current time
% Original Author: Hannah Clark
% Date: 11/7/14
% ChangeLog:
%    12/01/14 - HCC - fixed a glitch in loop when sending current time
%    11/22/14 - HCC - switched mapping over to send tick message to a foreach
%    11/22/14 - HCC - changed name of start/1 to init/1 for more clarity
%    11/19/14 - HCC - added currTime/1
%    11/19/14 - HCC - fixed issue with notification of minute 0 missed in preliminary debugging
%    11/16/14 - HCC - cleaned up debugging outputs
%    11/07/14 - HCC - created/debugged module
-module(clock).
-export([init/1, remove/2, add/2, currTime/1, startClock/1]).

%Create clock
init(RegName) -> register(RegName, spawn(fun() -> wait([]) end)).

%Adds item to clock
add(Clock, Pid) -> Clock ! {add, Pid}.

%Removes given item from clock
remove(Clock, Pid) -> Clock ! {remove, Pid}.

%Set clock to begin counting
startClock(Clock) -> Clock ! {startClock}.

%Retrieve current time from clock
currTime(Clock) -> Clock ! {timeCheck, self()},
                    receive
                        {timeRet, Minute} -> Minute
                    end.

%Period between clock's initialization and starting count, items will not be removed during this period
wait(ObjList) -> 
                receive
                    {add, Pid} -> wait([Pid|ObjList]);
                    {startClock} -> AllPass = add_rest(ObjList), %Retrieve any remaining add requests before starting
                                    lists:foreach(fun(Pid) ->
                                        Pid ! {tick, 0} end, AllPass), %Alert all objects that clock has started
                                    loop(0, AllPass, 0) %Begin clock loop
                end.
%Used to clear mailbox of add requests before retrieving objects
add_rest(ObjList) -> receive
                        {add, Pid} -> add_rest([Pid|ObjList])
                     after 100 -> ObjList
                     end.

%Clock's main action loop, ends when no more objects are "listening" to the clock
loop(_, [], _) -> io:fwrite("clock dies~n", []), ok;
loop(Minute, ObjList, ObjDone) ->
                receive
                    {minuteDone} ->  if
                            ObjDone + 1 == length(ObjList) -> %case that all watched objects have finished their minute
                                lists:foreach(fun(Pid) -> %signal all watched objects
                                        Pid ! {tick, Minute + 1}
                                    end, ObjList),
                                    io:fwrite("~p objects received of ~p total, TIME MOVES FORWARD ~n", [ObjDone+1, length(ObjList)]),
                                loop(Minute + 1, ObjList, 0); %loop with next minute
                            ObjDone + 1 < length(ObjList) -> %case of still waiting on objects
                                io:fwrite("~p objects received of ~p total ~n", [ObjDone+1, length(ObjList)]),
                                loop(Minute, ObjList, ObjDone + 1)
                        end;
                    {add, Pid} -> loop(Minute, [Pid|ObjList], ObjDone);
                    {remove, Pid} -> 
                        io:fwrite("removing from clock tracking, ~p objects remain: ~w minus ~w ~n", [length(ObjList)-1, ObjList, Pid]),
                        loop(Minute, lists:delete(Pid, ObjList), ObjDone);
                    {timeCheck, Pid} -> Pid ! {timeRet, Minute},
                                        loop(Minute, ObjList, ObjDone)
                end. 
