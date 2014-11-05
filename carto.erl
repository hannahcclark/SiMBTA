% name: Raewyn Duvall
% updated: 11/5/14

-module(carto).
-export([cartograph/0, directionFromTo/2, timeToNext/2, lastStation/1]).

cartograph() ->
    [alewife, davis, porter, harvard, central, kendall, charles_mgh,
     park_street, downtown_crossing, south, broadway, andrew, jfk_umass,
     savin_hill, fields_corner, shawmut, ashmont].

directionFromTo(Start, End) ->
    case Start of
        alewife -> south;
        davis ->
            case End of
                alewife -> north;
                true -> south
            end.
        porter ->
            case End of
                alewife -> north;
                davis -> north;
                true -> south
            end.
        harvard ->
            case End of
                alewife -> north;
                davis -> north;
                porter -> north;
                true -> south
            end.
        central ->
            case End of
                alewife -> north;
                davis -> north;
                porter -> north;
                harvard -> north;
                true -> south
            end.
        kendall ->
            case End of
                alewife -> north;
                davis -> north;
                porter -> north;
                harvard -> north;
                central -> north;
                true -> south
            end.
        charles_mgh ->
            case End of
                alewife -> north;
                davis -> north;
                porter -> north;
                harvard -> north;
                central -> north;
                kendall -> north;
                true -> south
            end. 
        park_street ->
            case End of
                alewife -> north;
                davis -> north;
                porter -> north;
                harvard -> north;
                central -> north;
                kendall -> north;
                charles_mgh -> north;
                true -> south
            end. 
        downtown_crossing ->
            case End of
                south -> south;
                broadway -> south;
                andrew -> south;
                jfk_umass-> south;
                savin_hill -> south;
                fields_corner -> south;
                shwamut -> south;
                ashmont -> south;
                true -> north
            end.
        south ->
            case End of
                broadway -> south;
                andrew -> south;
                jfk_umass-> south;
                savin_hill -> south;
                fields_corner -> south;
                shwamut -> south;
                ashmont -> south;
                true -> north
            end.
        broadway ->
            case End of
                andrew -> south;
                jfk_umass-> south;
                savin_hill -> south;
                fields_corner -> south;
                shwamut -> south;
                ashmont -> south;
                true -> north
            end.
        andrew ->
            case End of
                jfk_umass-> south;
                savin_hill -> south;
                fields_corner -> south;
                shwamut -> south;
                ashmont -> south;
                true -> north
            end.
        jfk_umass ->
            case End of
                savin_hill -> south;
                fields_corner -> south;
                shwamut -> south;
                ashmont -> south;
                true -> north
            end.
        savin_hill ->
            case End of
                fields_corner -> south;
                shwamut -> south;
                ashmont -> south;
                true -> north
            end.
        fields_corner ->
            case End of
                shwamut -> south;
                ashmont -> south;
                true -> north
            end.
        shawmut ->
            case End of
                ashmont -> south;
                true -> north
            end.
        ashmont -> north
    end.

timeToNext(Station, Direction) ->
    case Direction of
        north ->
            case Station of
                alewife -> 0;
                davis -> 1;
                porter -> 2;
                harvard -> 3;
                central -> 4;
                kendall -> 5;
                charles_mgh -> 6;
                park_street -> 7;
                downtown_crossing -> 8;
                south -> 9;
                broadway -> 10;
                andrew -> 11;
                jfk_umass -> 12;
                savin_hill -> 13;
                fields_corner -> 14;
                shawmut -> 15;
                ashmont -> 16
            end.
        south ->
            case Station of
                alewife -> 1;
                davis -> 2;
                porter -> 3;
                harvard -> 4;
                central -> 5;
                kendall -> 6;
                charles_mgh -> 7;
                park_street -> 8;
                downtown_crossing -> 9;
                south -> 10;
                broadway -> 11;
                andrew -> 12;
                jfk_umass -> 13;
                savin_hill -> 14;
                fields_corner -> 15;
                shawmut -> 16;
                ashmont -> 0
            end
    end.

lastStation(Direction) ->
    case Direction of
        north -> alwife;
        south -> ashmont
    end.


