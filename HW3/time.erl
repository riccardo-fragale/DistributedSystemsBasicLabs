-module(time).

-export([zero/0]).
-export([inc/2]).
-export([merge/2]).
-export([leq/2]).
-export([rec/3]).
-export([clock/1]).
-export([update/3]).
-export([safe/2]).


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

%Simply updates the counter of a single worker
rec(Name,Local,Remote)->
    inc(Name, merge(Local,Remote)).


%Return a clock that can keep track of the nodes
clock(Nodes)->
    [{Node,0} || Node <- Nodes].

%return a clock that has been updated given that we have received a log message from a node at a given time;
update(Node, Time, Clock)->
    OldTime =
            case lists:keyfind(Node,1,Clock) of
                {Node,T} -> T;
                false -> 0
            end,
    [{Node, merge(OldTime,Time)} | lists:keydelete(Node,1,Clock)].

%Safe if time is lower or equal wrt the clock?
safe(Time,Clock)->
    lists:all(fun({_Node,T}) -> Time =< T end,Clock).




