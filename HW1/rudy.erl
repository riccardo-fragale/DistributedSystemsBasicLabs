-module(rudy).
-export([init/1]).
-export([handler/1]).
-export([request/1]).
-export([reply/1]).
-export([start/1]).
-export([stop/0]).

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    io:format(" Created process ~p~n",[pid_to_list(self())]),
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
                    handler(Listen),
                    gen_tcp:close(Listen),
                    ok;
        {error, Error} ->
            error
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
                    request(Client),  %COMMA IN ERLANG TO SEPARATE BRANCH IN A CASE.
                    handler(Listen);   
        {error, Error} ->
                    error
    end.

request(Client) ->
        Recv = gen_tcp:recv(Client, 0),
        case Recv of
            {ok, Str} ->
                io:format("Received ~p~n", [Str]),
                Request = http:parse_request(Str),
                Response = reply(Request),
                gen_tcp:send(Client, Response);
            {error, Error} ->
                io:format("rudy: error: ~w~n", [Error])
        end,
        gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
        Body = "<html><body><h1>You requested: " ++ URI ++ "</h1></body></html>",
        timer:sleep(40),
        http:ok(Body).

start(Port)->
        register(rudy,spawn(fun() -> init(Port) end)).

stop() -> 
    exit(whereis(rudy), "Time to die").







