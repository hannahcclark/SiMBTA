-module(clock).
-export([init/1, remove/2, add/2, currTime/1, startClock/1]).

init(RegName) -> register(RegName, spawn(fun() -> wait([]) end)).

add(Clock, Pid) -> Clock ! {add, Pid}.

remove(Clock, Pid) -> Clock ! {remove, Pid}.

startClock(Clock) -> Clock ! {startClock}.

currTime(Clock) -> Clock ! {timeCheck, self()},
                    receive
                        {timeRet, Minute} -> Minute
                    end.

wait(ObjList) -> 
                receive
                    {add, Pid} -> wait([Pid|ObjList]);
                    {startClock} -> lists:map(fun(Pid) ->
                                        Pid ! {tick, 0} end, ObjList),
                                    loop(0, add_rest(ObjList), 0)
                end.

add_rest(ObjList) -> receive
                        {add, Pid} -> add_rest([Pid|ObjList])
                     after 100 -> ObjList
                     end.

loop(Minute, ObjList, ObjDone) ->
                receive
                    {minuteDone} ->  if
                            ObjDone + 1 == length(ObjList) ->
                                lists:map(fun(Pid) ->
                                        Pid ! {tick, Minute + 1}
                                    end, ObjList),
                                loop(Minute + 1, ObjList, 0);
                            ObjDone + 1 < length(ObjList) ->
                                loop(Minute, ObjList, ObjDone + 1)
                        end;
                    {add, Pid} -> loop(Minute, [Pid|ObjList], ObjDone);
                    {remove, Pid} -> NewList = lists:delete(Pid, ObjList),
                        if
                            NewList /= [] -> loop(Minute, NewList, ObjDone)
                        end;
                    {timeCheck, Pid} -> Pid ! {timeRet, Minute}
                end. 