-module(holdb_queue).

-export([new/0]).
-export([add/4]).
-export([sort/1]).
-export([partition/2]).
-export([sort/1]).
-export([size/1]).

%The queue contains messages in the form {From;Time,Msg}
new()->
    [].

%Add a new tuple into the list
add(From,Time,Msg,Queue)->
    NewList = [{From,Time,Msg} | Queue],
    sort(NewList).

%Sort the queue based on time
sort(Queue)->
    %lists:keysort(2,Queue).
    lists:sort(
        fun({_F1, T1, _M1}, {_F2, T2, _M2}) ->
            % T1 should come before T2 if T1 happens-before T2
            vect:leq(T1, T2) andalso not vect:leq(T2, T1)
        end,
        Queue
    ).

%Divide the holdback queue based on safeness of time
partition(Queue,Clock)->
    {Safe,Unsafe} = lists:partition(
fun({_F,Time,_Msg}) -> time:safe(Time,Clock) end, Queue
    ),
    {lists:keysort(2,Safe),lists:keysort(2,Unsafe)}.


%Return the size of the queue
size(Queue)->
    length(Queue).
