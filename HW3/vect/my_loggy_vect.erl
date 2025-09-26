-module(my_loggy_vect).

-export([start/1, stop/1]).
-export([log/1]).
-export([log/3]).


start(Nodes)->
    spawn_link(fun()->init(Nodes) end).

stop(Logger)->
    Logger ! stop.

%QueueFile is where to dump the messages
init(Nodes)->
    ok = ensure_dir(".././test/testVectorQueueLength2.csv"),
    {ok, QueueFile} = file:open(".././test/testVectorQueueLength2.csv", [append]),
    loop(holdb_queue_vect:new(),vect:clock(Nodes),QueueFile,0).

loop(Queue,Clock,QueueFile,Index)->
    {Safe,Unsafe} = holdb_queue_vect:partition(Queue,Clock),
    log(Safe),
    receive
        {log, From, Time, Msg} ->
            case vect:safe(Time,Clock) of 
                true -> 
                    UpdatedClock=vect:update(From,Time,Clock),
                    my_logger:log(From,Time,Msg),
                    UpdatedClock=vect:update(From,Time,Clock),
                    loop(Queue,UpdatedClock,QueueFile,Index+1);
                false ->
                    NewQueue = holdb_queue_vect:add(From,Time,Msg,Unsafe),
                    %io:format("~p ~n",[NewQueue]),
                    %io:format(QueueFile,"~p,~p ~n", [Index,holdb_queue_vect:size(NewQueue)]),
                    %io:format("~p,~p,~p~n",[Index,holdb_queue_vect:size(NewQueue),NewQueue]),
                    NewClock = vect:update(From,Time,Clock),
                    loop(NewQueue,NewClock,QueueFile,Index+1)
            end;
        stop ->
            %Send remaining queue in order
            Sorted = holdb_queue_vect:sort(Unsafe),
            %io:format(QueueFile, "Final,~p ~n", [holdb_queue_vect:size(Sorted)]),
            log(Unsafe),
            file:close(QueueFile),
            ok
    end.

log(From, Time, Msg) ->
    ok = ensure_dir("./test/testVect1.csv"),
    {ok, IoDevice} = file:open("./test/testVect1.csv", [append]),
    io:format("log: ~w ~w ~p~n",[Time, From, Msg]),
    %io:format(IoDevice, "~w,~w,~p~n", [Time, From, Msg]),
    file:close(IoDevice).

log(Queue) ->
    lists:foreach(
        fun({From,Time,Msg}) -> my_loggy_vect:log(From,Time,Msg) end,
            Queue).

ensure_dir(FilePath) ->
    Dir = filename:dirname(FilePath),
    filelib:ensure_dir(Dir ++ "/").
