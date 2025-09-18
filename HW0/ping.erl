-module(ping).
-export([start/0]).

start()->
    Remote = {waitp4,'foo@10.93.19.135'},

    Remote ! {msg, self(), "A message from ping"},

    receive 
        {reply, Text} ->
            io:format("Received reply: ~s~n", [Text])
        end.

