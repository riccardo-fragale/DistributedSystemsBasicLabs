-module(worker_vect).

-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name,Logger,Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive 
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter,vect:zero());
        stop ->
            ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter,Clock) ->
    Wait = random:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            NewClock = vect:rec(Name,Clock,Time),
            Log ! {log, Name, NewClock, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter,NewClock);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        Time = vect:inc(Name,Clock),
        Message = {hello, random:uniform(100)},
        Selected ! {msg, Time, Message},
        jitter(Jitter),
        Log ! {log, Name, Time, {sending,Message}},
        %jitter(Jitter),
        loop(Name, Log, Peers, Sleep, Jitter,Time)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
