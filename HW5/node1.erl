-module(node1).
-define(Stabilize,1000).
-define(Timeout,200).

-export([start/1]).
-export([start/2]).
-export([node/3]).
-export([connect/2]).
-export([print_state/3]).


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
                %io:format("Request received"),
                request(Peer, Predecessor),
                node(Id, Predecessor, Successor);
        {status, Pred} ->
                Succ = stabilize(Pred, Id, Successor),
                node(Id, Predecessor, Succ);
        {terminate, Reason} -> %Termination printing
                io:format("Node ~p terminating: ~p~n", [Id, Reason]),
                ok;
        stabilize ->
                %io:format("Stabilization started"),
                stabilize(Successor),
                node(Id, Predecessor, Successor);
        {print_state} ->
                print_state(Id, Predecessor, Successor),
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

%% notify({Nkey, Npid}, Id, Predecessor) ->
%%     case Predecessor of
%%         nil ->
%%             if % No predecessor yet, but check if we are notified by ourself
%%                 Nkey =:= Id andalso Npid =:= self() ->
%%                     nil; % Don't set ourself as predecessor
%%                 true ->
%%                     {Nkey, Npid}
%%             end;
%%         {Pkey, _} ->
%%             % Check if new node should be our predecessor
%%             if
%%                 Nkey =:= Id andalso Npid =:= self() ->
%%                     Predecessor;    % Don't update if we are predecessor
%%                 true ->
%%                     case key:between(Nkey, Pkey, Id) of
%%                         true  -> {Nkey, Npid};
%%                         false -> Predecessor
%%                     end
%%             end
%%     end.
notify({Nkey, Npid},Id,Predecessor) ->
	case Predecessor of
		nil ->
			{Nkey, Npid};
		{Pkey, _} ->
			case key:between( Nkey, Pkey, Id) of
				true ->
					{Nkey, Npid};
				false ->
					Predecessor
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
        node(Id, Predecessor, Successor).


connect(Id, nil) ->
        {ok,{Id,self()}};
connect(Id, Peer) ->
        Qref = make_ref(),
        Peer ! {key, Qref, self()},
        receive
                {Qref, Skey} ->
                     {ok,{Skey,Peer}},
                     io:format("Connected") 
        after ?Timeout ->
                io:format("Time out: no response~n",[])
        end.

print_state(Id, Predecessor, Successor) ->
    io:format("Node ~p | Pred=~p | Succ=~p~n", [Id, Predecessor, Successor]).

