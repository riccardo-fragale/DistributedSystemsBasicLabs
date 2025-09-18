-module(waitp4).
-export([hello/0]).

hello()-> 
    receive 
        {msg, From, Text} ->
            io:format("Received message: ~s~n", [Text]),
            %reply to the sender using its Pid
            From ! {reply,"Hello from wait!"},
            hello() %continue waiting for more messages
        end.
