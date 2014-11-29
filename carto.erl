% name: Raewyn Duvall
% updated: 11/5/14

%Travel times found from the MBTA Realtime api's schedulebyroute endpoint provide by MassDOT

-module(carto).
-export([cartograph/0, directionFromTo/2, timeToNext/2, firstStation/1]).

cartograph() ->
    [alewife, davis, porter, harvard, central, kendall, charles_mgh,
     park_street, downtown_crossing, south, broadway, andrew, jfk_umass,
     savin_hill, fields_corner, shawmut, ashmont].

firstStation(Dir) ->
    case Dir of
        ashmont -> alewife;
        alewife -> ashmont
    end.

directionFromTo(Start, End) ->
    case Start of
        alewife -> ashmont;
        davis ->
            case End of
                alewife -> alewife;
                _ -> ashmont
            end;
        porter ->
            case End of
                alewife -> alewife;
                davis -> alewife;
                _ -> ashmont
            end;
        harvard ->
            case End of
                alewife -> alewife;
                davis -> alewife;
                porter -> alewife;
                _ -> ashmont
            end;
        central ->
            case End of
                alewife -> alewife;
                davis -> alewife;
                porter -> alewife;
                harvard -> alewife;
                _ -> ashmont
            end;
        kendall ->
            case End of
                alewife -> alewife;
                davis -> alewife;
                porter -> alewife;
                harvard -> alewife;
                central -> alewife;
                _ -> ashmont
            end;
        charles_mgh ->
            case End of
                alewife -> alewife;
                davis -> alewife;
                porter -> alewife;
                harvard -> alewife;
                central -> alewife;
                kendall -> alewife;
                _ -> ashmont
            end; 
        park_street ->
            case End of
                alewife -> alewife;
                davis -> alewife;
                porter -> alewife;
                harvard -> alewife;
                central -> alewife;
                kendall -> alewife;
                charles_mgh -> alewife;
                _ -> ashmont
            end; 
        downtown_crossing ->
            case End of
                alewife -> alewife;
                davis -> alewife;
                porter -> alewife;
                harvard -> alewife;
                central -> alewife;
                kendall -> alewife;
                charles_mgh -> alewife;
		        park_street-> alewife;
                _ -> ashmont
            end;
        south ->
            case End of
                broadway -> ashmont;
                andrew -> ashmont;
                jfk_umass-> ashmont;
                savin_hill -> ashmont;
                fields_corner -> ashmont;
                shawmut -> ashmont;
                ashmont -> ashmont;
                _ -> alewife
            end;
        broadway ->
            case End of
                andrew -> ashmont;
                jfk_umass-> ashmont;
                savin_hill -> ashmont;
                fields_corner -> ashmont;
                shawmut -> ashmont;
                ashmont -> ashmont;
                _ -> alewife
            end;
        andrew ->
            case End of
                jfk_umass-> ashmont;
                savin_hill -> ashmont;
                fields_corner -> ashmont;
                shawmut -> ashmont;
                ashmont -> ashmont;
                _ -> alewife
            end;
        jfk_umass ->
            case End of
                savin_hill -> ashmont;
                fields_corner -> ashmont;
                shawmut -> ashmont;
                ashmont -> ashmont;
                _ -> alewife
            end;
        savin_hill ->
            case End of
                fields_corner -> ashmont;
                shawmut -> ashmont;
                ashmont -> ashmont;
                _ -> alewife
            end;
        fields_corner ->
            case End of
                shawmut -> ashmont;
                ashmont -> ashmont;
                _ -> alewife
            end;
        shawmut ->
            case End of
                ashmont -> ashmont;
                true -> alewife
            end;
        ashmont -> alewife
    end.

timeToNext(Station, Direction) ->
    case Direction of
        alewife ->
            case Station of
                alewife -> {endStation, 0};
                davis -> {alewife, 3};
                porter -> {davis, 3};
                harvard -> {porter, 3};
                central -> {harvard, 2};
                kendall -> {central, 2};
                charles_mgh -> {kendall, 2};
                park_street -> {charles_mgh, 2};
                downtown_crossing -> {park_street, 1};
                south -> {downtown_crossing, 2};
                broadway -> {south, 2};
                andrew -> {broadway, 2};
                jfk_umass -> {andrew, 2};
                savin_hill -> {jfk_umass, 3};
                fields_corner -> {savin_hill, 3};
                shawmut -> {fields_corner, 2};
                ashmont -> {shawmut, 2}
            end;
        ashmont ->
            case Station of
                alewife -> {davis, 2};
                davis -> {porter, 2};
                porter -> {harvard, 2};
                harvard -> {central, 3};
                central -> {kendall, 2};
                kendall -> {charles_mgh, 2};
                charles_mgh -> {park_street, 2};
                park_street -> {downtown_crossing, 2};
                downtown_crossing -> {south, 2};
                south -> {broadway, 2};
                broadway -> {andrew, 2};
                andrew -> {jfk_umass, 2};
                jfk_umass -> {savin_hill, 3};
                savin_hill -> {fields_corner, 3};
                fields_corner -> {shawmut, 3};
                shawmut -> {ashmont, 2};
                ashmont -> {endStation, 0}
            end
    end.

