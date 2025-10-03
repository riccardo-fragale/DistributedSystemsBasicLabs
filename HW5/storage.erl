-module(storage).

-compile(export_all).

create() ->
    [].

add(Key, Value, Store) ->
    case lists:keyreplace(Key, 1, Store, {Key, Value}) of
        false ->
            [{Key, Value} | Store];
        NewStore ->
            NewStore
    end.

lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
    Updated = lists:filter(fun({K,_}) -> key:between(K, From, To) end, Store),
    Rest = lists:filter(fun({K,_}) -> not key:between(K, From, To)end, Store),
    {Updated, Rest}.

merge(Entries, Store) ->
    lists:foldl(fun({K,V}, Acc) -> add(K, V, Acc) end, Store, Entries).