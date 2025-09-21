-module(time).

-export([zero/0]).
-export([inc/2]).
-export([merge/2]).
-export([leq/2]).


%Initial lamport value set to 0
zero()->
    0.


%Return T +1, Name excluded
inc(_Name,T)->
    T + 1.

%Return the maximum among the two values
merge(Ti,Tj)->
    erlang:max(Ti,Tj).

%Return true if Ti <= Tj
leq(Ti,Tj)->
    Ti =< Tj.




