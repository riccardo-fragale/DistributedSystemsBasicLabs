-module(node4).
-define(Stabilize,1000).
-define(Timeout,10000).

-compile(export_all).


%Check termination printing
node(Id, Predecessor, Successor, Next, Storage, Replica) ->
    receive
        {key, Qref, Peer} -> 
                Peer ! {Qref, Id},
                node(Id, Predecessor, Successor, Next, Storage, Replica);

        {notify, New} ->
                {Pred, NewStore, Rep} = notify(New, Id, Predecessor, Storage, Replica),
                node(Id, Pred, Successor, Next, NewStore, Rep);

        {request, Peer} ->
                %io:format("Request received"),
                request(Peer, Predecessor, Successor),
                node(Id, Predecessor, Successor, Next, Storage, Replica);

        {status, Pred, Nx} ->
                {Succ, Nxt} = stabilize(Pred, Id, Successor, Nx),
                completeReplica(Succ, Storage),
                node(Id, Predecessor, Succ, Nxt, Storage, Replica);

        {terminate, Reason} -> %Termination printing
                io:format("Node ~p terminating: ~p~n", [Id, Reason]),
                ok;

        probe ->
                create_probe(Id, Successor),
                node(Id, Predecessor, Successor, Next, Storage, Replica);

        {probe, Id, Nodes, T} ->
                remove_probe(T, Nodes),
                node(Id, Predecessor, Successor, Next, Storage, Replica);

        {probe, Ref, Nodes, T} ->
                forward_probe(Ref, T, Nodes, Id, Successor),
                self() ! debug,
                node(Id, Predecessor, Successor, Next, Storage, Replica);

        stabilize ->
                %io:format("Stabilization started"),
                stabilize(Successor),
                node(Id, Predecessor, Successor, Next, Storage, Replica);

        %debug ->
                %io:format("~w: pre: ~w suc: ~w~n", [Id, Predecessor, Successor]),
                %node(Id, Predecessor, Successor)

        {add, Key, Value, Qref, Client} ->
                Added = add(Key, Value, Qref, Client,
                            Id, Predecessor, Successor, Storage),
                node(Id, Predecessor, Successor, Next, Added, Replica);

        {lookup, Key, Qref, Client} ->
                lookup(Key, Qref, Client, Id, Predecessor, Successor, Storage),
                node(Id, Predecessor, Successor, Next, Storage, Replica);

        %replication
        {completeReplica, Store} ->
                case storage:size(Store) /= storage:size(Replica) of
                    true ->
                        io:format("~w: inconsistent replica ~n ", [Id]),
                        node(Id, Predecessor, Successor, Next, Storage, Replica);
                    false ->
                        node(Id, Predecessor, Successor, Next, Storage, Replica)
                end;
        
        {replicate, Key, Value, Qref, Client} ->
                Added = storage:add(Key, Value, Replica),
                Client ! {Qref, ok},
                node(Id, Predecessor, Successor, Next, Storage, Added);


        {handover, Elements} ->
                Merged = storage:merge(Storage, Elements),
                 io:format("~w got handover: store: ~w, rep: ~w~n", [Id, storage:size(Merged), storage:size(Replica)]),
                node(Id, Predecessor, Successor, Next, Merged, Replica);

        {'DOWN', Ref, process, _, _} ->
                {Pred, Succ, Nxt, Sto, Rep} = down(Ref, Predecessor, Successor, Next, Storage, Replica),
                node(Id, Pred, Succ, Nxt, Sto, Rep);
        stop ->
                io:format("! node ~w shutting down~n", [Id]),
                ok;
        debug ->
                io:format("~w: pre: ~w suc: ~w, storage: ~w, replica: ~w~n", [Id, Predecessor, Successor, storage:size(Storage), storage:size(Replica)]),
                node(Id, Predecessor, Successor, Next, Storage, Replica)
    end.


stabilize(Pred, Id, Successor, Nx) ->
        {Skey, Sref, Spid} = Successor,
        case Pred of
                nil -> % Successor has no predecessor, notify it about our existence
                        Spid ! {notify,{Id, self()}},
                        {Successor,Nx};
                {Id, _} -> % Successor's predecessor is us, ring is stable, do nothing
                        {Successor, Nx};
                {Skey, _} -> %Successor predecessor is itself, let's notify it about our existence
                        Spid ! {notify, {Id, self()}},
                        {Successor, Nx};
                {Xkey, Xpid} ->
                        case key:between(Xkey, Id, Skey) of
                                true -> % pred is between this and sec, pred is new suc
                                        drop(Sref),
                                        Xref = monitor(Xpid),
                                        Xpid ! {request,self()},
                                        {{Xkey, Xref, Xpid}, Successor};
                                false ->
                                        Spid ! {notify, {Id, self()}},
                                        {Successor, Nx}
                        end
        end.

stabilize({_, _, Spid}) ->
        Spid ! {request, self()}.

%Stabilize scheduler
schedule_stabilize() ->
        timer:send_interval(?Stabilize, self(), stabilize).

request(Peer, Predecessor, Successor) ->
        case Predecessor of
                nil ->
                        Peer ! {status, nil, Successor};
                {Pkey, Ppid} ->
                        Peer ! {status, {Pkey, Ppid}, Successor}
        end.

notify({Nkey, Npid}, Id, Predecessor, Store, Replica) ->
     case Predecessor of
         nil ->
            {Keep, Rep} = handover(Id, Store, Replica, Nkey, Npid),
            Nref = monitor(Npid),
            {{Nkey, Nref, Npid}, Keep, Rep};
         {Pkey, Pref, _} ->
             case key:between(Nkey, Pkey, Id) of
                true ->
                        %New one is between
                        {Keep, Rep} = handover(Id, Store, Replica, Nkey, Npid),
                        drop(Pref),
                        Nref = monitor(Npid),
                        {{Nkey, Nref, Npid}, Keep, Rep};
                false ->
                        %keep old one
                        {Predecessor, Store, Replica}
                end
     end.
	

start(Id) ->
        start(Id, nil).

start(Id, Peer) ->
        timer:start(),
        spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
        Predecessor = nil,
        {ok, Successor} = connect(Id, Peer),
        schedule_stabilize(),
        Storage = storage:create(),
        Replica = storage:create(),
        Next = nil,
        node(Id, Predecessor, Successor, Next, Storage, Replica).


connect(Id, nil) ->
        Ref = monitor(self()),
        {ok,{Id, Ref, self()}};
connect(Id, Peer) ->
        Qref = make_ref(),
        Peer ! {key, Qref, self()},
        receive
                {Qref, Skey} ->
                     SRef = monitor(self()),
                     {ok,{Skey, SRef, Peer}}
        after ?Timeout ->
                io:format("Time out: no response~n")
        end.

add(Key, Value, Qref, Client, _Id, nil, {_, _, Spid}, Store)->
    %No predecessor, we handle all keys
    Spid ! {replicate, Key, Value, Qref, Client},
    storage:add(Key, Value, Store);
add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
        case key:between(Key, Pkey, Id) of
                true ->
                        Spid ! {replicate, Key, Value, Qref, Client},
                        storage:add(Key, Value, Store);
                false ->
                        Spid ! {add, Key, Value, Qref, Client},
                        Store
        end.
lookup(Key, Qref, Client, _Id, nil, Successor, Store) ->
        Result = storage:lookup(Key, Store),
        Client ! {Qref, Result};
lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
        case key:between(Key, Pkey, Id) of
                true ->
                        Result = storage:lookup(Key, Store),
                        Client ! {Qref, Result};
                false ->
                        {_, _, Spid} = Successor,
                        Spid ! {lookup, Key, Qref, Client},
                        ok
        end.


create_probe(Id, {_, _, Spid}) ->
        T = erlang:system_time(micro_seconds),
        Spid ! {probe, Id, [Id], T}.

remove_probe(T, Nodes) ->
        T2 = erlang:system_time(micro_seconds),
        io:format("Probe passed into ring [~w] in ~w microseconds ~n",[Nodes,T2 - T]).

forward_probe(Ref, T, Nodes, Id, Successor) ->
        {_, _, Spid} = Successor,
        Spid ! {probe, Ref, Nodes++[Id], T}.


handover(Id, Store, Replica, Nkey, Npid) ->
        {Keep, Rest} = storage:split(Nkey, Id, Store),
         io:format("~w: handing over to ~w -> ~w keys, keeping ~w keys~n", [Id, Nkey, storage:size(Rest), storage:size(Keep)]),
        Npid ! {handover, Rest, Keep},
        {Keep, Rest}.

completeReplica({_, _, Spid}, Store) ->
    Spid ! {completeReplica, Store}.

%Node crashes
monitor(Pid) ->
    erlang:monitor(process, Pid).
drop(nil) ->
    ok;
drop(Ref) ->
    erlang:demonitor(Ref, [flush]).


down(Ref, {_, Ref, _}, Successor, Next, Storage, Replica) ->
    Sto = storage:merge(Storage, Replica),
    {nil, Successor, Next, Sto, storage:replica()}; %predecessor died, set to nil
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Nref, Npid}, Storage, Replica) ->
    Npid ! {request,self()}, %Successor died, adopt next as successor
    {Predecessor, {Nkey, Nref, Npid}, nil, Storage, Replica}.