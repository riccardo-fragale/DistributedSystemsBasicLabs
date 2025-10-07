-module(node3).
-define(Stabilize,500).
-define(Timeout,10000).

-compile(export_all).


%Check termination printing
node(Id, Predecessor, Successor, Next, Storage) ->
    receive
        {key, Qref, Peer} -> 
                Peer ! {Qref, Id},
                node(Id, Predecessor, Successor, Next, Storage);

        {notify, New} ->
                {Pred, NewStore} = notify(New, Id, Predecessor, Storage),
                node(Id, Pred, Successor, Next, NewStore);

        {request, Peer} ->
                %io:format("Request received"),
                request(Peer, Predecessor, Successor),
                node(Id, Predecessor, Successor, Next, Storage);

        {status, Pred, Nx} ->
                {Succ, Nxt} = stabilize(Pred, Id, Successor, Nx),
                node(Id, Predecessor, Succ, Nxt, Storage);

        {terminate, Reason} -> %Termination printing
                io:format("Node ~p terminating: ~p~n", [Id, Reason]),
                ok;

        probe ->
                create_probe(Id, Successor),
                node(Id, Predecessor, Successor, Next, Storage);

        {probe, Id, Nodes, T} ->
                remove_probe(T, Nodes),
                node(Id, Predecessor, Successor, Next, Storage);

        {probe, Ref, Nodes, T} ->
                forward_probe(Ref, T, Nodes, Id, Successor),
                node(Id, Predecessor, Successor, Next, Storage);

        stabilize ->
                %io:format("Stabilization started"),
                stabilize(Successor),
                node(Id, Predecessor, Successor, Next, Storage);

        %debug ->
                %io:format("~w: pre: ~w suc: ~w~n", [Id, Predecessor, Successor]),
                %node(Id, Predecessor, Successor)

        {add, Key, Value, Qref, Client} ->
                Added = add(Key, Value, Qref, Client,
                            Id, Predecessor, Successor, Storage),
                node(Id, Predecessor, Successor, Next, Added);

        {lookup, Key, Qref, Client} ->
                lookup(Key, Qref, Client, Id, Predecessor, Successor, Storage),
                node(Id, Predecessor, Successor, Next, Storage);

        {handover, Elements} ->
                %reverse order
                Merged = storage:merge(Elements, Storage),
                node(Id, Predecessor, Successor, Next, Merged);

        {'DOWN', Ref, process, _, _} ->
                {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
                node(Id, Pred, Succ, Nxt, Storage);
        stop ->
                ok;
        _ -> 
            node(Id, Predecessor, Successor, Next, Storage)  %Catch all clause
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
                {Pkey, _, Ppid} ->
                        Peer ! {status, {Pkey, Ppid}, Successor}
        end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
     case Predecessor of
         nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            Nref = monitor(Npid),
            {{Nkey, Nref, Npid}, Keep};
         {Pkey, Pref, _} ->
             case key:between(Nkey, Pkey, Id) of
                true ->
                        %New one is between
                        Keep = handover(Id, Store, Nkey, Npid),
                        drop(Pref),
                        Nref = monitor(Npid),
                        {{Nkey, Nref, Npid}, Keep};
                false ->
                        %keep old one
                        {Predecessor, Store}
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
        Next = nil,
        node(Id, Predecessor, Successor, Next, Storage).


connect(Id, nil) ->
        Ref = monitor(self()),
        {ok,{Id, Ref, self()}};
connect(Id, Peer) ->
        Qref = make_ref(),
        Peer ! {key, Qref, self()},
        receive
                {Qref, Skey} ->
                     SRef = monitor(Peer),
                     {ok,{Skey, SRef, Peer}}
        after ?Timeout ->
                io:format("Time out: no response~n")
        end.

add(Key, Value, Qref, Client, _Id, nil, _Successor, Store)->
    %No predecessor, we handle all keys
    Client ! {Qref, ok},
    storage:add(Key, Value, Store);
add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
        case key:between(Key, Pkey, Id) of
                true ->
                        Client ! {Qref, ok},
                        storage:add(Key, Value, Store);
                false ->
                        Spid ! {add, Key, Value, Qref, Client},
                        Store
        end.
lookup(Key, Qref, Client, Id, nil, Successor, Store) ->
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


handover(Id, Store, Nkey, Npid) ->
        {Keep, Rest} = storage:split(Nkey, Id, Store),
        Npid ! {handover, Rest},
        Keep.


%Node crashes
monitor(Pid) ->
    erlang:monitor(process, Pid).
drop(nil) ->
    ok;
drop(Ref) ->
    erlang:demonitor(Ref, [flush]).


down(Ref, {_, Ref, _}, Successor, Next) ->
    {nil, Successor, Next}; %predecessor died, set to nil
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Nref, Npid}) ->
    Npid ! {request,self()}, %Successor died, adopt next as successor
    {Predecessor, {Nkey, Nref, Npid}, nil}.