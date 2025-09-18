-module(test).
-export([bench/2]).
-export([run/3]).
-export([request/2]).
-export([print_time/2]).

bench(Host,Port) ->
        Start = erlang:system_time(micro_seconds),
        run(1000,Host,Port),
        Finish = erlang:system_time(micro_seconds),
        Finish - Start.

run(N, Host, Port) ->
        StartTime = erlang:system_time(micro_seconds),
        if
            N == 0 ->
                ok;
            true -> 
                request(Host, Port),
                FinishTime = erlang:system_time(micro_seconds),
                TotalTime = FinishTime - StartTime,
                %Index = 100 - N,  %Check every time this value
                print_time(150-N, TotalTime),  %I pass 150-N as the iterations start from 0
                run(N-1, Host, Port)
        end.

request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect(Host, Port, Opt),
    gen_tcp:send(Server, http:get("foo")),
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
        {ok, _} -> 
            ok;
        {error, Error} ->
            io:format("test: error: ~w~n", [Error])
    end,
    gen_tcp:close(Server).

print_time(N, TotalTime) ->
    %{ok, Fd} = file:open("ResultSingolar1.csv", [append]),  Version for rudimental server
    {ok,Fd} = file:open("Result1.csv",[append]),  %Version for optional task
    io:format(Fd, "~p,~p ~n",[N, TotalTime]),
    file:close(Fd).