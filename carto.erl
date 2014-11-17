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
        alewife -> ashmont;
        davis ->
            case End of
                alewife -> alewife;
                true -> ashmont
            end.
        porter ->
            case End of
                alewife -> alewife;
                davis -> alewife;
                true -> ashmont
            end.
        harvard ->
            case End of
                alewife -> alewife;
                davis -> alewife;
                porter -> alewife;
                true -> ashmont
            end.
        central ->
            case End of
                alewife -> alewife;
                davis -> alewife;
                porter -> alewife;
                harvard -> alewife;
                true -> ashmont
            end.
        kendall ->
            case End of
                alewife -> alewife;
                davis -> alewife;
                porter -> alewife;
                harvard -> alewife;
                central -> alewife;
                true -> ashmont
            end.
        charles_mgh ->
            case End of
                alewife -> alewife;
                davis -> alewife;
                porter -> alewife;
                harvard -> alewife;
                central -> alewife;
                kendall -> alewife;
                true -> ashmont
            end. 
        park_street ->
            case End of
                alewife -> alewife;
                davis -> alewife;
                porter -> alewife;
                harvard -> alewife;
                central -> alewife;
                kendall -> alewife;
                charles_mgh -> alewife;
                true -> ashmont
            end. 
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
                true -> ashmont
            end.
        south ->
            case End of
                broadway -> ashmont;
                andrew -> ashmont;
                jfk_umass-> ashmont;
                savin_hill -> ashmont;
                fields_corner -> ashmont;
                shwamut -> ashmont;
                ashmont -> ashmont;
                true -> alewife
            end.
        broadway ->
            case End of
                andrew -> ashmont;
                jfk_umass-> ashmont;
                savin_hill -> ashmont;
                fields_corner -> ashmont;
                shwamut -> ashmont;
                ashmont -> ashmont;
                true -> alewife
            end.
        andrew ->
            case End of
                jfk_umass-> ashmont;
                savin_hill -> ashmont;
                fields_corner -> ashmont;
                shwamut -> ashmont;
                ashmont -> ashmont;
                true -> alewife
            end.
        jfk_umass ->
            case End of
                savin_hill -> ashmont;
                fields_corner -> ashmont;
                shwamut -> ashmont;
                ashmont -> ashmont;
                true -> alewife
            end.
        savin_hill ->
            case End of
                fields_corner -> ashmont;
                shwamut -> ashmont;
                ashmont -> ashmont;
                true -> alewife
            end.
        fields_corner ->
            case End of
                shwamut -> ashmont;
                ashmont -> ashmont;
                true -> alewife
            end.
        shawmut ->
            case End of
                ashmont -> ashmont;
                true -> alewife
            end.
        ashmont -> alewife
    end.

timeToNext(Station, Direction) ->
    case Direction of
        alewife ->
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
        ashmont ->
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

