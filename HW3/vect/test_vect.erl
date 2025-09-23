-module(test_vect).
-export([run/2]).

run(Sleep, Jitter) ->
    io:format("Starting loggers"),
    Log = my_loggy_vect:start([john, paul, ringo, george]),
    A = worker_vect:start(john, Log, 13, Sleep, Jitter),
    B = worker_vect:start(paul, Log, 23, Sleep, Jitter),
    C = worker_vect:start(ringo, Log, 36, Sleep, Jitter),
    D = worker_vect:start(george, Log, 49, Sleep, Jitter),
    io:format("Setting peers for workers"),
    worker_vect:peers(A, [B, C, D]),
    worker_vect:peers(B, [A, C, D]),
    worker_vect:peers(C, [A, B, D]),
    worker_vect:peers(D, [A, B, C]),
    io:format("Sleeping for five seconds"),
    timer:sleep(3000),
    io:format("Stopping logger and workers"),
    my_loggy_vect:stop(Log),
    worker_vect:stop(A),
    worker_vect:stop(B),
    worker_vect:stop(C),
    worker_vect:stop(D).