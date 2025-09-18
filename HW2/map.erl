-module(map).

-export([new/0]).
-export([update/3]).
-export([reachable/2]).
-export([all_nodes/1]).

%a directional map where you can easily update the map and 
%find nodes directly connected to a given node. 
%We could represent it as a list of entries where each entry consists of a city 
%with a list of directly connected cities. 

new()->
    [].

update(Node, Links, Map)->
    [{Node,Links} | lists:keydelete(Node, 1, Map)].

reachable(Node,Map)->
    case lists:keyfind(Node,1,Map) of
        {Node, Links} -> Links;
        false -> []
    end.

%I had some issues here
all_nodes(Map)->
    lists:usort([N || {Node, Links} <- Map, N <- [Node | Links]]).


