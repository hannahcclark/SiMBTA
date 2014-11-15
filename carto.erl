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
        alewife -> in;
        davis ->
            case End of
                alewife -> out;
                true -> in
            end.
        porter ->
            case End of
                alewife -> out;
                davis -> out;
                true -> in
            end.
        harvard ->
            case End of
                alewife -> out;
                davis -> out;
                porter -> out;
                true -> in
            end.
        central ->
            case End of
                alewife -> out;
                davis -> out;
                porter -> out;
                harvard -> out;
                true -> in
            end.
        kendall ->
            case End of
                alewife -> out;
                davis -> out;
                porter -> out;
                harvard -> out;
                central -> out;
                true -> in
            end.
        charles_mgh ->
            case End of
                alewife -> out;
                davis -> out;
                porter -> out;
                harvard -> out;
                central -> out;
                kendall -> out;
                true -> in
            end. 
        park_street ->
            case End of
                alewife -> out;
                davis -> out;
                porter -> out;
                harvard -> out;
                central -> out;
                kendall -> out;
                charles_mgh -> out;
                true -> in
            end. 
        downtown_crossing ->
            case End of
                south -> in;
                broadway -> in;
                andrew -> in;
                jfk_umass-> in;
                savin_hill -> in;
                fields_corner -> in;
                shwamut -> in;
                ashmont -> in;
                true -> out
            end.
        south ->
            case End of
                broadway -> out;
                andrew -> out;
                jfk_umass-> out;
                savin_hill -> out;
                fields_corner -> out;
                shwamut -> out;
                ashmont -> out;
                true -> in
            end.
        broadway ->
            case End of
                andrew -> out;
                jfk_umass-> out;
                savin_hill -> out;
                fields_corner -> out;
                shwamut -> out;
                ashmont -> out;
                true -> in
            end.
        andrew ->
            case End of
                jfk_umass-> out;
                savin_hill -> out;
                fields_corner -> out;
                shwamut -> out;
                ashmont -> out;
                true -> in
            end.
        jfk_umass ->
            case End of
                savin_hill -> out;
                fields_corner -> out;
                shwamut -> out;
                ashmont -> out;
                true -> in
            end.
        savin_hill ->
            case End of
                fields_corner -> out;
                shwamut -> out;
                ashmont -> out;
                true -> in
            end.
        fields_corner ->
            case End of
                shwamut -> out;
                ashmont -> out;
                true -> in
            end.
        shawmut ->
            case End of
                ashmont -> out;
                true -> in
            end.
        ashmont -> in
    end.

timeToNext(Station, Direction) ->
    case Direction of
        in ->
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
        out ->
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

lastStation(CurrStation, Direction) ->
    case Direction of
        in -> 
	    case CurrStation of
                alewife -> ashmont;
                davis -> ashmont;
                porter -> ashmont;
                harvard -> ashmont;
                central -> ashmont;
                kendall -> ashmont;
                charles_mgh -> ashmont;
                park_street -> ashmont;
                downtown_crossing -> ashmont;
                south -> alewife;
                broadway -> alewife;
                andrew -> alewife;
                jfk_umass -> alewife;
                savin_hill -> alewife;
                fields_corner -> alewife;
                shawmut -> alewife;
                ashmont -> alewife
	    end;
        out ->
	    case CurrStation of
                alewife -> alewife;
                davis -> alewife;
                porter -> alewife;
                harvard -> alewife;
                central -> alewife;
                kendall -> alewife;
                charles_mgh -> alewife;
                park_street -> alewife;
                downtown_crossing -> alewife;
                south -> ashmont;
                broadway -> ashmont;
                andrew -> ashmont;
                jfk_umass -> ashmont;
                savin_hill -> ashmont;
                fields_corner -> ashmont;
                shawmut -> ashmont;
                ashmont -> ashmont
	    end;
	    
    end.


