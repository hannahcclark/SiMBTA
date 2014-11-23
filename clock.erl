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
loop(_, [], _) -> ok;
loop(Minute, ObjList, ObjDone) ->
                receive
                    {minuteDone} ->  if
                            ObjDone + 1 == length(ObjList) -> %case that all watched objects have finished their minute
                                lists:foreach(fun(Pid) -> %signal all watched objects
                                        Pid ! {tick, Minute + 1}
                                    end, ObjList),
                                loop(Minute + 1, ObjList, 0); %loop with next minute
                            ObjDone + 1 < length(ObjList) -> %case of still waiting on objects
                                loop(Minute, ObjList, ObjDone + 1)
                        end;
                    {add, Pid} -> loop(Minute, [Pid|ObjList], ObjDone);
                    {remove, Pid} -> loop(Minute, lists:delete(Pid, ObjList), ObjDone);
                    {timeCheck, Pid} -> Pid ! {timeRet, Minute}
                end. 
