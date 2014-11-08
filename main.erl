-module(main).
-export([start/1]).

start(FileName) -> lists:map(station:start, carto:cartograph()).
                   {ok, Device} = file:open(FileName, [read]).
