-module(gms3).

-export([leader/5]).
-export([slave/7]).
-export([start/1]).
-export([start/2]).
-export([init/3]).
-export([init/4]).
-export([bcast/3]).
-export([election/6]).

-define(timeout,10000).
-define(arghh,1000).

%This module is a bit more complex and introduces node crashes detection

leader(Id, Master, N, Slaves, Group) ->
    receive
            {mcast, Msg} -> bcast(Id, {msg, N, Msg}, Slaves),
                            Master ! Msg,
                            leader(Id, Master, N+1, Slaves, Group);
            {join, Wrk, Peer} -> Slaves2 = lists:append(Slaves, [Peer]),
                                Group2 = lists:append(Group, [Wrk]),
                                bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
                                Master ! {view, Group2},
                                leader(Id, Master, N+1, Slaves2, Group2);
            stop -> ok
    end.

%Broadcast of part 2
%bcast(Id, Msg, Slaves) ->
%    lists:foreach(fun(Pid) -> Pid ! Msg end,Slaves).

bcast(Id, Msg, Nodes) ->
        lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id)->
    case random:uniform(?arghh) of 
        ?arghh ->
            io:format("leader ~w: crash ~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
            receive     
                {mcast, Msg} ->
                                Leader ! {mcast, Msg},
                                slave(Id, Master, Leader, N, Last, Slaves, Group);
                {join, Wrk, Peer} ->
                                Leader ! {join, Wrk, Peer},
                                slave(Id, Master, Leader, N, Last, Slaves, Group);
                %%ignore duplicates
                {msg, I, _} when I < N ->
                                %io:format("Node ~w: Ignoring duplicate msg with index ~w (current N=~w)~n", [Id, I, N]),
                                slave(Id, Master, Leader, N, Last, Slaves, Group);
                {view, I, _, _} when I < N ->
                                %io:format("Node ~w: Ignoring duplicate view with index ~w (current N=~w)~n",[Id,I,N]),
                                slave(Id, Master, Leader, N, Last, Slaves, Group);
                {msg, N, Msg} ->
                                %io:format("Node ~w: Received msg with index ~w, updating N to ~w~n", [Id, N, N+1]),
                                Master ! Msg,
                                slave(Id, Master, Leader, N+1, {msg, N, Msg}, Slaves, Group);
                {view, N, [Leader|Slaves2], Group2} ->
                                %io:format("Node ~w: Received view with index ~w, updating N to ~w~n", [Id, N, N+1]),
                                Master ! {view, Group2},
                                slave(Id, Master, Leader, N+1, {view, N, [Leader | Slaves2], Group2}, Slaves2, Group2);
                {'DOWN', _Ref, process, Leader, _Reason} ->
                                election(Id, Master, N, Last, Slaves, Group);
                stop ->     ok
            end.

start(Id) -> 
        Rnd = random:uniform(1000),
        Self = self(),
        {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
                random:seed(Rnd,Rnd,Rnd),
                leader(Id,Master, 0, [],[Master]).

%Old init for start(Id)
%init(Id, Master) -> leader(Id, Master, [], [Master]).

%Slave
start(Id, Grp) ->
                Rnd = random:uniform(1000),
                Self = self(),
                {ok, spawn_link(fun()-> init(Id, Grp,Rnd, Self) end)}.


init(Id, Grp, Rnd, Master) -> 
                random:seed(Rnd,Rnd,Rnd),
                Self = self(),
                Grp ! {join, Master, Self},
                receive
                    {view, N, [Leader|Slaves], Group} ->
                                                    Master ! {view, Group},
                                                    erlang:monitor(process, Leader),
                                                    slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group)
                after ?timeout ->
                    Master ! {error,"no reply from leader"}
                end.


election(Id, Master, N, Last, Slaves, Group) ->
        Self = self(),
        case Slaves of
            [Self|Rest] -> 
                io:format("Node ~w: I am the new leader~n", [Id]),
                bcast(Id, {view, N, Slaves, Group},Rest),
                Master ! {view, Group},
                bcast(Id, Last, Rest),
                leader(Id, Master, N+1, Rest, Group);
            [Leader|Rest] ->
                io:format("Node ~w: New Leader elected is ~w~n", [Id,Leader]),
                erlang:monitor(process, Leader),
                slave(Id, Master, Leader, N, Last, Rest, Group)
        end.
