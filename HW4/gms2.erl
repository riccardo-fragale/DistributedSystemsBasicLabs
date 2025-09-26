-module(gms2).

-export([leader/4]).
-export([slave/5]).
-export([start/1]).
-export([start/2]).
-export([init/2]).
-export([init/3]).
-export([bcast/3]).
-export([election/4]).

-define(timeout,10000).

leader(Id, Master, Slaves, Group) ->
    receive
            {mcast, Msg} -> bcast(Id, {msg, Msg}, Slaves),
                            Master ! Msg,
                            leader(Id, Master, Slaves, Group);
            {join, Wrk, Peer} -> Slaves2 = lists:append(Slaves, [Peer]),
                                Group2 = lists:append(Group, [Wrk]),
                                bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
                                Master ! {view, Group2},
                                leader(Id, Master, Slaves2, Group2);
            stop -> ok
    end.

bcast(Id, Msg, Slaves) ->
    lists:foreach(fun(Pid) -> Pid ! Msg end,Slaves).

slave(Id, Master, Leader, Slaves, Group) ->
            receive     
                {mcast, Msg} ->
                                Leader ! {mcast, Msg},
                                slave(Id, Master, Leader, Slaves, Group);
                {join, Wrk, Peer} ->
                                Leader ! {join, Wrk, Peer},
                                slave(Id, Master, Leader, Slaves, Group);
                {msg, Msg} ->
                                Master ! Msg,
                                slave(Id, Master, Leader, Slaves, Group);
                {view, [Leader|Slaves2], Group2} ->
                                Master ! {view, Group2},
                                slave(Id, Master, Leader, Slaves2, Group2);
                {'DOWN', _Ref, process, Leader, _Reason} ->
                                election(Id, Master, Slaves, Group);
                stop ->     ok
            end.

start(Id) -> Self = self(),
             {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) -> leader(Id, Master, [], [Master]).

start(Id, Grp) ->
                Self = self(),
                {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.
init(Id, Grp, Master) -> 
                Self = self(),
                Grp ! {join, Master, Self},
                receive
                    {view, [Leader|Slaves], Group} ->
                                                    Master ! {view, Group},
                                                    erlang:monitor(process, Leader),
                                                    slave(Id, Master, Leader, Slaves, Group)
                after ?timeout ->
                    Master ! {error,"no reply from leader"}
                end.


election(Id, Master, Slaves, [_|Group]) ->
        Self = self(),
        case Slaves of
            [Self|Rest] -> 
                io:format("Node ~w: I am the new leader~n", [Id]),
                bcast(Id, {view, Slaves, Group}, Rest),
                Master ! {view, Group},
                leader(Id, Master, Rest, Group);
            [Leader|Rest] ->
                io:format("Node ~w: New Leader elected is ~w~n", [Id,Leader]),
                erlang:monitor(process, Leader),
                slave(Id, Master, Leader, Rest, Group)
        end.
