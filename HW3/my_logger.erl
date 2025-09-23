-module(my_logger).

-export([start/1, stop/1]).
-export([log/1]).
-export([log/3]).


start(Nodes)->
    spawn_link(fun()->init(Nodes) end).

stop(Logger)->
    Logger ! stop.

%QueueFile is where to dump the messages
init(Nodes)->
    ok = ensure_dir("./test/testLamportQueueLength2.csv"),
    {ok, QueueFile} = file:open("./test/testLamportQueueLength2.csv", [append]),
    loop(holdb_queue:new(),time:clock(Nodes),QueueFile,0).

loop(Queue,Clock,QueueFile,Index)->
    {Safe,Unsafe} = holdb_queue:partition(Queue,Clock),
    log(Safe),
    receive
        {log, From, Time, Msg} ->
            case time:safe(Time,Clock) of 
                true -> 
                    my_logger:log(From,Time,Msg),
                    UpdatedClock=time:update(From,Time,Clock),
                    loop(Queue,UpdatedClock,QueueFile,Index+1);
                false ->
                    NewQueue = holdb_queue:add(From,Time,Msg,Unsafe),
                    io:format("~p ~n",[NewQueue]),
                    io:format(QueueFile,"~p,~p ~n", [Index,holdb_queue:size(NewQueue)]),
                    NewClock = time:update(From,Time,Clock),
                    loop(NewQueue,NewClock,QueueFile,Index+1)
            end;
        stop ->
            %Send remaining queue in order
            Sorted = holdb_queue:sort(Unsafe),
            io:format(QueueFile, "Final,~p ~n", [holdb_queue:size(Sorted)]),
            log(Unsafe),
            file:close(QueueFile),
            ok
    end.

log(From, Time, Msg) ->
    %ok = ensure_dir("./test/testLamp3.csv"),
    %{ok, IoDevice} = file:open("./test/testLamp3.csv", [append]),
    ok.
    %io:format("log: ~w ~w ~p~n",[Time, From, Msg]).
    %io:format(IoDevice, "~w,~w,~p~n", [Time, From, Msg]),
    %file:close(IoDevice).

log(Queue) ->
    lists:foreach(
        fun({From,Time,Msg}) -> my_logger:log(From,Time,Msg) end,
            Queue).

ensure_dir(FilePath) ->
    Dir = filename:dirname(FilePath),
    filelib:ensure_dir(Dir ++ "/").
