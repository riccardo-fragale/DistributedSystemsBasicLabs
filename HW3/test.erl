-module(test).
-export([run/2]).

run(Sleep, Jitter) ->
    io:format("Starting loggers"),
    Log = my_logger:start([john, paul, ringo, george]),
    A = worker:start(john, Log, 13, Sleep, Jitter),
    B = worker:start(paul, Log, 23, Sleep, Jitter),
    C = worker:start(ringo, Log, 36, Sleep, Jitter),
    D = worker:start(george, Log, 49, Sleep, Jitter),
    io:format("Setting peers for workers"),
    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),
    io:format("Sleeping for five seconds"),
    timer:sleep(120000),
    io:format("Stopping logger and workers"),
    my_logger:stop(Log),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).