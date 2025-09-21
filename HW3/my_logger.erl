-module(my_logger).

-export([start/1, stop/1]).
-export([log/1]).
-export([log/3]).


start(Nodes)->
    spawn_link(fun()->init(Nodes) end).

stop(Logger)->
    Logger ! stop.

init(Nodes)->
    loop(holdb_queue:new(),time:clock(Nodes)).

loop(Queue,Clock)->
    {Safe,Unsafe} = holdb_queue:partition(Queue,Clock),
    log(Safe),
    receive
        {log, From, Time, Msg} ->
            case time:safe(Time,Clock) of 
                true -> 
                    my_logger:log(From,Time,Msg),
                    UpdatedClock=time:update(From,Time,Clock),
                    loop(Queue,UpdatedClock);
                false ->
                    NewQueue = holdb_queue:add(From,Time,Msg,Unsafe),
                    io:format("Holdback queue length: ~p~n", [holdb_queue:size(NewQueue)]),
                    NewClock = time:update(From,Time,Clock),
                    loop(NewQueue,NewClock)
            end;
        stop ->
            %Send remaining queue in order
            Sorted = holdb_queue:sort(Unsafe),
            log(Unsafe),
            ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n",[Time, From, Msg]).

log(Queue) ->
    lists:foreach(
        fun({From,Time,Msg}) -> my_logger:log(From,Time,Msg) end,
            Queue).
