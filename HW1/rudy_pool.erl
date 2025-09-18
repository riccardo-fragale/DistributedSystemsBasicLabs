-module(rudy_pool).

-export([init/2, start/2, stop/0, handler/0]).

%Initialize the pool of handlers
init(Port,PoolSize)->
    Opt = [list, {active,false}, {reuseaddr, true}],
    io:format("Server starting on Port ~p~n",[Port]),
    {ok, Listen} = gen_tcp:listen(Port, Opt),
    Pool = [spawn(?MODULE, handler, []) || _ <- lists:seq(1,PoolSize)],  %spawn an array of processes
    accept_loop(Listen, Pool).

%Assign a request to a handler and handle the queuing system
accept_loop(Listen, Pool) -> 
    case gen_tcp:accept(Listen) of 
        {ok, Client} ->
            Worker = hd(Pool),
            Worker ! {handle, Client}, %Handle is just a tag
            NewPool = tl(Pool) ++ [Worker],  %I append the worker after the rest of the list except for the tail
            accept_loop(Listen,NewPool);
        {error, Issue} ->
            io:format("Error ~p~n",[issue]),
            accept_loop(Listen,Pool)
    end.

handler()->
    receive
        {handle, Client} ->
            request(Client),
            handler();
        {error, Error} ->  %Better to include this for a higher error handling
            error
        end.

request(Client)->
    case gen_tcp:recv(Client, 0) of
        {ok, Str} ->
            io:format("Received ~p~n", [Str]),
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("Rudy: error: ~w~n",[Error])
        end,
        gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    Body = "<html><body><h1>You requested: " ++ URI ++ "</h1></body></html>",
    timer:sleep(40),
    http:ok(Body).

%PoolSize is the parameter through which we decide how many handlers to start
start(Port,PoolSize) ->
    register(rudy, spawn(fun() -> init(Port,PoolSize) end)).

stop()->
    exit(whereis(rudy), "Time to die").






