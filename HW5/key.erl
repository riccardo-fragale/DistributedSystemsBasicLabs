-module(key).


-export([generate/0]).
-export([between/3]).


generate()->
    rand:uniform(1000000000).

between(Key, From, To) ->
    case {From, To} of
        {F, T} when F == T -> %Full circle; everything inside
            true; 
        {F, T} when F < T -> %normal case
            (Key > F andalso Key =< T);
        {F, T} ->
            % F > T, example circle from 0 to 12; I want from 10 to 2 which means numbers
            % [10,11,12,0,1,2] 
            (Key > F) orelse (Key =< T)
    end.
