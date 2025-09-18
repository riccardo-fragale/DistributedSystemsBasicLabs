-module(routy).
-export([start/2, stop/1, init/1]).
-export([status/1]).
-export([update/1]).
-export([broadcast/1]).

start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    Intf = intf:new(),
    Map = map:new(),
    %Table = dijkstra:table(maps_tolist(Intf),Map),
    %Table = dijkstra:table(Intf, Map),
    Table = dijkstra:table([Name | intf:list(Intf)], Map),
    Hist = hist:new(Name),
    io:format("Router ~p started ~n",[Name]),
    router(Name, 0, Hist, Intf, Table, Map).  

router(Name, N, Hist, Intf, Table, Map) ->
    receive
        {links, Node, R, Links} ->
            case hist:update(Node, R, Hist) of
                {new, Hist1} ->
                    intf:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    router(Name, N, Hist1, Intf, Table, Map1);
                old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;
        {add, Node, Pid} ->
            Ref = erlang:monitor(process,Pid),
            Intf1 = intf:add(Node, Ref, Pid, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {remove, Node} ->
            {ok, Ref} = intf:ref(Node, Intf),
            erlang:demonitor(Ref),
            Intf1 = intf:remove(Node, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {'DOWN', Ref, process, _, _} ->
            {ok, Down} = intf:name(Ref, Intf),
            io:format("~w: exit recived from ~w~n", [Name, Down]),
            Intf1 = intf:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {status, From} ->
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);
        {route, Name, From, Message} ->
            io:format("~p: received message ~p ~n", [Name, Message]),
            router(Name, N, Hist, Intf, Table, Map);
        update -> 
            Table1 = dijkstra:table([Name | intf:list(Intf)], Map),
            router(Name, N, Hist, Intf, Table1, Map);
        broadcast ->
            Message = {links, Name, N, intf:list(Intf)},
            intf:broadcast(Message, Intf),
            router(Name, N+1, Hist, Intf, Table, Map);
        {route, To, From, Message} ->
            io:format("~p: routing message (~p)~n", [Name, Message]),
            case dijkstra:route(To, Table) of
                {ok, Gw} when Gw =:= Name->
                    %Message is arrived at destination
                    io:format("~w: received message ~p ~n", [Name, Message]),
                    router(Name, N, Hist, Intf, Table, Map);
                {ok, Gw} ->
                    %Forward it
                    case intf:lookup(Gw, Intf) of
                        {ok, Pid} ->
                            Pid ! {route, To, From, Message};
                        notfound ->
                            io:format("~w: gateway ~p not found in interfaces~n", [Name, Gw]),
                            ok
                    end,
                    router(Name, N, Hist, Intf, Table, Map);
                notfound ->
                    io:format("~w: no route to ~p~n", [Name, To]),
                    router(Name, N, Hist, Intf, Table, Map)
            end;
        {send, To, Message} ->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);
        {stop} ->
            ok
    end.

status(Node)->
    Node ! {status,self()},
    receive 
        {status,{Name,N,Hist,Intf,Table,Map}}->
            io:format("~n--- Router ~w-----~n",[Name]),
            io:format("Counter: ~p~n", [N]),
            io:format("History: ~p~n", [Hist]),
            io:format("Interfaces: ~p~n", [intf:list(Intf)]),
            io:format("Routing Table: ~p~n", [Table]),
            io:format("Map: ~p~n", [Map]),
            ok
    after 2000 ->
        io:format("No response from ~p~n", [Node]),
        error
    end.

update(Reg) ->
            Reg ! update.

broadcast(Reg) ->
            Reg ! broadcast.