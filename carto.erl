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
                alewife -> (endStation, 0);
                davis -> (alewife, 1);
                porter -> (davis, 2);
                harvard -> (porter, 3);
                central -> (harvard, 4);
                kendall -> (central, 5);
                charles_mgh -> (kendall, 6);
                park_street -> (charles_mgh, 7);
                downtown_crossing -> (park_street, 8);
                south -> (downtown_crossing, 9);
                broadway -> (south, 10);
                andrew -> (broadway, 11);
                jfk_umass -> (andrew, 12);
                savin_hill -> (jfk_umass, 13);
                fields_corner -> (savin_hill, 14);
                shawmut -> (fields_corner, 15);
                ashmont -> (shawmut, 16);
            end.
        ashmont ->
            case Station of
                alewife -> (davis, 1);
                davis -> (porter, 2);
                porter -> (harvard, 3);
                harvard -> (central, 4);
                central -> (kendall, 5);
                kendall -> (charles_mgh, 6);
                charles_mgh -> (park_street, 7);
                park_street -> (downtown_crossing, 8);
                downtown_crossing -> (south, 9);
                south -> (broadway, 10);
                broadway -> (andrew, 11);
                andrew -> (jfk_umass, 12);
                jfk_umass -> (savin_hill, 13);
                savin_hill -> (fields_corner, 14);
                fields_corner -> (shawmut, 15);
                shawmut -> (ashmont, 16);
                ashmont -> (endStation, 0)
            end
    end.

