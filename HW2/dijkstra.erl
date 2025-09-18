-module(dijkstra).

-export([table/2]).
-export([route/2]).
-export([update/4]).

entry(Node, Sorted)->
    case lists:keyfind(Node,1,Sorted) of
        {Node,Distance,Gateway} ->
            Distance;
        false ->
            0
    end.

replace(Node, N, Gateway, Sorted) ->
    %io:format("        replace(~p, dist=~p, gw=~p)~n", [Node, N, Gateway]),
    case lists:keyfind(Node,1,Sorted) of 
        false ->
            Sorted;
        _ ->
            lists:keysort(2, [{Node,N,Gateway} | lists:keydelete(Node,1,Sorted)])
        end.

update(Node, N, Gateway, Sorted) ->
  case entry(Node,Sorted) of
    Distance when N < Distance ->
        replace(Node,N, Gateway,Sorted);
    _ ->
        Sorted
    end.



iterate([],_Map,_Gateways,Table) ->
    lists:reverse(Table);
    %Table;

iterate([{Node, inf,_GW} | _Rest], _Map, _Gateways, Table) ->
    %Case infinite distance
    lists:reverse(Table);
    %Table;

iterate([H | T], Map, Gateways, Table) ->
    {Node, N, Gateway} = H,

    %All reachable nodes from the map
    Reachable = map:reachable(Node,Map),

    Sorted = lists:foldl(
        fun(ReachableNode, Acc) ->
            NewGateway = 
                case lists:member(Node, Gateways) of 
                    true -> Node;
                    false -> Gateway
                end,
            update(ReachableNode, N+1, NewGateway, Acc)
        end,
        T,
        Reachable
    ),

    %Add entry to the table
    iterate(Sorted,Map, Gateways, [{Node,Gateway} | Table]).


table(Gateways, Map) ->

    %Try also simply AllNodes = map:all_nodes(Map)
    AllNodes = lists:usort(Gateways ++ map:all_nodes(Map)),
    Initial = [
        {Node, 
            case lists:member(Node, Gateways) of
                true -> 0;
                false -> inf
            end,
            case lists:member(Node, Gateways) of 
                true -> Node;
                false -> unknown
            end}
         || Node <- AllNodes],
    Sorted = lists:keysort(2,Initial),
    iterate(Sorted, Map, Gateways, []).


route(Node, Table)->
    case lists:keyfind(Node, 1, Table) of 
        {Node, Gateway} ->
            {ok, Gateway};
        false ->
            notfound
    end.









