-module(clock).
-export([start/1, remove/1, add/1]).

start(RegName) -> register(RegName, spawn(fun() -> wait([]) end)).

add(Pid) -> self() ! {add, Pid}.

remove(Pid) -> self() ! {remove, Pid}.

wait(ObjList) -> receive
                    {add, Pid} -> wait([Pid|ObjList]);
                    {startClock} -> loop(0, add_rest(ObjList), 0)
                end.

add_rest(ObjList) -> receive
                        {add, Pid} -> add_rest([Pid|ObjList])
                     after 100 -> ObjList
                     end.

loop(Minute, ObjList, ObjDone) ->
                receive
                    {minuteDone} ->
                        if
                            ObjDone + 1 =:= length(ObjList) ->
                                lists:map(fun(Pid) ->
                                        Pid ! {clockTick, Minute + 1}
                                    end, ObjList),
                                loop(Minute + 1, ObjList, 0);
                            ObjDone + 1 < length(ObjList) -> 
                                loop(Minute, ObjList, ObjDone + 1)
                        end;
                    {add, Pid} -> loop(Minute, [Pid|ObjList], ObjDone);
                    {remove, Pid} -> loop(Minute, lists:delete(Pid, ObjList),
                                            ObjDone)
                end.
