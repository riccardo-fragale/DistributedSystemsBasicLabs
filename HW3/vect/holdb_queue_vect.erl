-module(holdb_queue_vect).

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
    lists:keysort(2,Queue).

%Divide the holdback queue based on safeness of time
partition(Queue,Clock)->
    {Safe,Unsafe} = lists:partition(
fun({_F,Time,_Msg}) -> vect:safe(Time,Clock) end, Queue
    ),
    {lists:keysort(2,Safe),lists:keysort(2,Unsafe)}.


%Return the size of the queue
size(Queue)->
    length(Queue).
