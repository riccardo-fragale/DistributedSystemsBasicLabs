-module(node1).
-define(Stabilize,1000).

%Check termination printing
node(Id, Predecessor, Successor) ->
    receive
        {key, Qref, Peer} -> 
                Peer ! {Qref, Id},
                node(Id, Predecessor, Successor);
        {notify, New} ->
                Pred = notify(New, Id, Predecessor),
                node(Id, Pred, Successor);
        {request, Peer} ->
                request(Peer, Predecessor),
                node(Id, Predecessor, Successor);
        {status, Pred} ->
                Succ = stabilize(Pred, Id, Successor),
                node(Id, Predecessor, Succ);
        {terminate, Reason} ->. %Termination printing
            io:format("Node ~p terminating: ~p~n", [Id, Reason]),
            ok;
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);
        _ -> 
            node(Id, Predecessor, Successor)  %Catch all clause
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
        Spid ! {request, self()}

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
