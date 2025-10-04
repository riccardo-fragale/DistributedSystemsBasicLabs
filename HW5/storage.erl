-module(storage).

-compile(export_all).

create() ->
    maps:new().

add(Key, Value, Store) ->
    maps:put(Key, Value, Store).

lookup(Key, Store) ->
    case maps:find(Key, Store) of 
        {ok, Val} -> {Key, Val};
        error -> false
    end.

split(From, To, Store) ->
    maps:fold(fun(Key, Value, {UpdatedAcc, RestAcc}) ->
        case key:between(Key, From, To) of 
            true ->
                {maps:put(Key, Value, UpdatedAcc), RestAcc};
            false ->
                {UpdatedAcc, maps:put(Key, Value, RestAcc)}
            end
        end,
        {create(),create()},
        Store).

merge(Entries, Store) ->
    maps:merge(Entries, Store).