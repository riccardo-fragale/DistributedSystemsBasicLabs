-module(hist).

-export([new/1]).
-export([update/3]).

%%IN THE COMMENT A PAST IMPLEMENTATION WITH MAPS
new(Name)->
   [{Name,inf}].

update(Node,N,History)->
    %case maps:get(Node,History,0) of
        %Max when N =< Max ->
         %   old;
       % Max ->
          %  {new,maps:put(Node,N,History)}
    %end.
    case lists:keyfind(Node,1, History) of
        {Node, Biggest} when N =< Biggest ->
            old;
        {Node, Biggest} when N > Biggest ->
            NewHistory = lists:keyreplace(Node,1, History, {Node,N}),
            {ok, NewHistory};
        false ->
            {new, [{Node, N} | History]}
    end.



