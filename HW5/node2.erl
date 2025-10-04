-module(node2).
-define(Stabilize,1000).
-define(Timeout,10000).

-compile(export_all).


%Check termination printing
node(Id, Predecessor, Successor, Storage) ->
    receive
        {key, Qref, Peer} -> 
                Peer ! {Qref, Id},
                node(Id, Predecessor, Successor, Storage);

        {notify, New} ->
                {Pred, NewStore} = notify(New, Id, Predecessor, Storage),
                node(Id, Pred, Successor, NewStore);

        {request, Peer} ->
                %io:format("Request received"),
                request(Peer, Predecessor),
                node(Id, Predecessor, Successor, Storage);

        {status, Pred} ->
                Succ = stabilize(Pred, Id, Successor),
                node(Id, Predecessor, Succ, Storage);

        {terminate, Reason} -> %Termination printing
                io:format("Node ~p terminating: ~p~n", [Id, Reason]),
                ok;

        probe ->
                create_probe(Id, Successor),
                node(Id, Predecessor, Successor, Storage);

        {probe, Id, Nodes, T} ->
                remove_probe(T, Nodes),
                node(Id, Predecessor, Successor, Storage);

        {probe, Ref, Nodes, T} ->
                forward_probe(Ref, T, Nodes, Id, Successor),
                node(Id, Predecessor, Successor, Storage);

        stabilize ->
                %io:format("Stabilization started"),
                stabilize(Successor),
                node(Id, Predecessor, Successor, Storage);

        %debug ->
                %io:format("~w: pre: ~w suc: ~w~n", [Id, Predecessor, Successor]),
                %node(Id, Predecessor, Successor)

        {add, Key, Value, Qref, Client} ->
                Added = add(Key, Value, Qref, Client,
                            Id, Predecessor, Successor, Storage),
                node(Id, Predecessor, Successor, Added);

        {lookup, Key, Qref, Client} ->
                lookup(Key, Qref, Client, Id, Predecessor, Successor, Storage),
                node(Id, Predecessor, Successor, Storage);

        {handover, Elements} ->
                Merged = storage:merge(Storage, Elements),
                node(Id, Predecessor, Successor, Merged);
        _ -> 
            node(Id, Predecessor, Successor, Storage)  %Catch all clause
    end.


stabilize(Pred, Id, Successor) ->
        {Skey, Spid} = Successor,
        case Pred of
                nil -> % Successor has no predecessor, notify it about our existence
                        Spid ! {notify,{Id, self()}},
                        Successor;
                {Id, _} -> % Successor's predecessor is us, ring is stable, do nothing
                        Successor;
                {Skey, _} -> %Successor predecessor is itself, let's notify it about our existence
                        Spid ! {notify, {Id, self()}},
                        Successor;
                {Xkey, Xpid} ->
                        case key:between(Xkey, Id, Skey) of
                                true ->
                                        Xpid ! {request,self()},
                                        {Xkey, Xpid};
                                false ->
                                        Spid ! {notify, {Id, self()}},
                                        Successor
                        end
        end.

stabilize({_, Spid}) ->
        Spid ! {request, self()}.

%Stabilize scheduler
schedule_stabilize() ->
        timer:send_interval(?Stabilize, self(), stabilize).

request(Peer, Predecessor) ->
        case Predecessor of
                nil ->
                        Peer ! {status, nil};
                {Pkey, Ppid} ->
                        Peer ! {status, {Pkey, Ppid}}
        end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
     case Predecessor of
         nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey,Npid}, Keep};
         {Pkey, _} ->
             case key:between(Nkey, Pkey, Id) of
                true ->
                        %New one is between
                        Keep = handover(Id, Store, Nkey, Npid),
                        {{Nkey, Npid}, Keep};
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
        node(Id, Predecessor, Successor, Storage).


connect(Id, nil) ->
        {ok,{Id,self()}};
connect(Id, Peer) ->
        Qref = make_ref(),
        Peer ! {key, Qref, self()},
        receive
                {Qref, Skey} ->
                     {ok,{Skey, Peer}}
        after ?Timeout ->
                io:format("Time out: no response~n")
        end.

add(Key, Value, Qref, Client, _Id, nil, _Successor, Store)->
    %No predecessor, we handle all keys
    Client ! {Qref, ok},
    storage:add(Key, Value, Store);
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
        case key:between(Key, Pkey, Id) of
                true ->
                        Client ! {Qref, ok},
                        storage:add(Key, Value, Store);
                false ->
                        Spid ! {add, Key, Value, Qref, Client},
                        Store
        end.
lookup(Key, Qref, Client, Id, {Okey, _}, Successor, Store) ->
        Result = storage:lookup(Key, Store),
        Client ! {Qref, Result};
lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
        case key:between(Key, Pkey, Id) of
                true ->
                        Result = storage:lookup(Key, Store),
                        Client ! {Qref, Result};
                false ->
                        {_, Spid} = Successor,
                        Spid ! {lookup, Key, Qref, Client},
                        ok
        end.


create_probe(Id, Successor) ->
        T = erlang:system_time(micro_seconds),
        {Skey, Spid} = Successor,
        Spid ! {probe, Id, [Id], T}.

remove_probe(T, Nodes) ->
        T2 = erlang:system_time(micro_seconds),
        io:format("Probe passed into ring [~w] in ~w microseconds ~n",[Nodes,T2 - T]).

forward_probe(Ref, T, Nodes, Id, Successor) ->
        {_, Spid} = Successor,
        Spid ! {probe, Ref, Nodes++[Id], T}.


handover(Id, Store, Nkey, Npid) ->
        {Keep, Rest} = storage:split(Nkey, Id, Store),
        Npid ! {handover, Rest},
        Keep.