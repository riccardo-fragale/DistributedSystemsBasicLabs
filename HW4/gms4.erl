-module(gms4).

-export([start/1]).
-export([start/2]).

-define(arghh,1000).
-define(recheck,50).
-define(ignore,10).
-define(timeout, 500).
-define(attempts, 3).


%Leader
start(Id) ->
  ok = ensure_dir("./gms4Test1.txt"),
  {ok, File} = file:open("gms4Test1.txt", [append]),
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Self, File) end)}.


init(Id, Rnd, Master, File) ->
  random:seed(Rnd,Rnd,Rnd),
  leader(Id, Master, 0, [], [Master], maps:new(), File).


%Slave
start(Id, Grp) ->
  {ok, File} = file:open("gms4Test1.txt", [append]),
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Grp, Rnd, Self, File) end)}.



init(Id, Grp, Rnd, Master, File) ->
  random:seed(Rnd,Rnd,Rnd),
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, N, [Leader|Slaves], Group} ->
      Master ! {view, Group},
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group, File)
  after ?timeout ->
    Master ! {error, "no reply from leader"}
  end.



slave(Id, Master, Leader, N, Last, Slaves, Group, File) ->
  receive
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group, File);
    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group, File);
    %%ignore duplicates
    {msg, I, _} when I < N ->
      %io:format("Node ~w: Ignoring duplicate msg with index ~w (current N=~w)~n", [Id, I, N]),
      Leader ! {ack, I, self()},
      slave(Id, Master, Leader, N, Last, Slaves, Group, File);
    {view, I, _, _} when I < N ->
      %io:format("Node ~w: Ignoring duplicate view with index ~w (current N=~w)~n",[Id,I,N]),
      Leader ! {ack, I, self()},
      slave(Id, Master, Leader, N, Last, Slaves, Group, File);
    {msg, N, Msg} ->
      case random:uniform(?ignore) of
        ?ignore ->
          io:format(File, "Worker ~w ignoring ~w~n", [self(), N]),
          slave(Id, Master, Leader, N, {msg, N, Msg}, Slaves, Group, File);
        _ ->
          %io:format("Node ~w: Received msg with index ~w, updating N to ~w~n", [Id, N, N+1]),
          Leader ! {ack, N, self()},
          Master ! Msg,
          slave(Id, Master, Leader, N+1, {msg, N, Msg}, Slaves, Group, File)
      end;
    {view, N, [Leader|Slaves2], Group2} ->
      %io:format("Node ~w: Received view with index ~w, updating N to ~w~n", [Id, N, N+1]),
      Leader ! {ack, N, self()},
      Master ! {view, Group2},
      slave(Id, Master, Leader, N+1, {view, N, [Leader | Slaves2], Group2}, Slaves2, Group2, File);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      election(Id, Master, N, Last, Slaves, Group, File);
    stop ->
      file:close(File),
      ok
  end.

bcast(Id, N, Msg, Nodes) ->
        lists:foreach(fun(Node) -> Node ! Msg, crash(Id, N) end, Nodes).

crash(Id,N) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ ->
            ok
end.


election(Id, Master, N, Last, Slaves, [_ |Group], File) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      %io:format("Node ~w: I am the new leader~n", [Id]),
      bcast(Id, N, {view, N, Slaves, Group}, Rest),
      Master ! {view, Group},
      bcast(Id, N, Last, Rest),
      leader(Id, Master, N+1, Rest, Group, maps:new(), File);
    [Leader|Rest] ->
      %io:format("Node ~w: New Leader elected is ~w~n", [Id, Leader]),
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N, Last, Rest, Group, File)
  end.
    


leader(Id, Master, N, Slaves, Group, PendingAck, File) ->
    receive
        {mcast, Msg} ->
            Entry = maps:new(),
            E2 = maps:put(nodes, Slaves, Entry),
            E3 = maps:put(msg, {msg, N, Msg}, E2),
            PA2 = maps:put(N, E3, PendingAck),

            bcast(Id, N, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N+1, Slaves, Group, PA2, File);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, N, {view, N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N+1, Slaves2, Group2, PendingAck, File);
        {ack, Num, Peer} ->
            % io:format("got ack from ~w for ~w~n", [Peer, Num]),
            CurAck = maps:get(Num, PendingAck, maps:new()),
            Entry = maps:get(nodes, CurAck, []),
            E2 = lists:delete(Peer, Entry),
            % io:format("len: ~w~n", [length(E2)]),

            case length(E2) == 0 of
                true ->
                    %io:format("dropping acks for ~w~n", [Num]),
                    PA2 = maps:remove(Num, PendingAck);
                false ->
                    %io:format("updating acks for ~w~n", [Num]),
                    CA2 = maps:put(nodes, E2, CurAck),
                    PA2 = maps:put(Num, CA2, PendingAck)
            end,

            leader(Id, Master, N, Slaves, Group, PA2, File);
        stop ->
            file:close(File),
            ok
    after
        ?recheck ->
            %io:format("checking pending acks: ~w~n", [PendingAck]),

            % cleanup

            P2 = maps:filter(fun(_Num, CurAck) ->
                length(maps:get(nodes, CurAck, [])) > 0
            end, PendingAck),

            case maps:size(P2) > 0 of
                true ->
                    P3 = maps:map(fun(Num, CurAck) ->
                        Nodes = maps:get(nodes, CurAck, []),
                        Msg = maps:get(msg, CurAck, {msg, 0, 0}),
                        Attempts = maps:get(attempts, CurAck, maps:new()),

                        NodeAtt = lists:foldl(fun(Node, Att) ->
                            NAt = maps:get(Node, Attempts, ?attempts)-1,
                            maps:put(Node, NAt, Att)
                        end, Attempts, Nodes),

                        NodesFilt = lists:filter(fun(Node) ->
                            maps:get(Node, NodeAtt, 0) > 0
                        end, Nodes),

                        %io:format("final node iter: ~w~n", [NodeAtt]),

                        lists:foreach(fun(Node) ->
                            io:format(File, "sending recast to ~w for ~w~n", [Node, Msg]),
                            Node ! Msg
                        end, NodesFilt),

                        maps:put(attempts, NodeAtt, CurAck)
                    end, P2),
                    leader(Id, Master, N, Slaves, Group, P3, File);
                _ ->
                    %io:format("nothing to check~n"),
                    leader(Id, Master, N, Slaves, Group, P2, File)
            end
    end.


ensure_dir(FilePath) ->
    Dir = filename:dirname(FilePath),
    filelib:ensure_dir(Dir ++ "/").