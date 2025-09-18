-module(routy_machine).
-export([bench/0]).
-define(SWEDEN,'sweden@n160-p24.eduroam.kth.se').
-define(FRANCE,'france@n160-p24.eduroam.kth.se').

bench()->
    %Start the routers 
    routy:start(r1,paris),
    routy:start(r2,lyon),
    routy:start(r3,marseille),
    routy:start(r4,bordeaux),

    timer:sleep(1500),
    %Build the connections
    r1 ! {add,lyon,{r2,?FRANCE}},
    r2 ! {add,paris,{r1,?SWEDEN}},
    r1 ! {add,marseille,{r3,?FRANCE}},
    r3 ! {add,paris,{r1,?FRANCE}},
    r3 ! {add,bordeaux,{r4,?FRANCE}},
    r4 ! {add,marseille,{r3,?FRANCE}},

    

    timer:sleep(500),

    r1 ! {add,stockholm,{r1,?SWEDEN}},


    timer:sleep(1500),

    io:format("Routers started and connected.~n"),

    %Broadcasting messages
    routy:broadcast(r1),
    timer:sleep(100),
    routy:broadcast(r2),
    timer:sleep(100),
    routy:broadcast(r3),
    timer:sleep(100),
    routy:broadcast(r4),

    timer:sleep(2000),
    
    %Updating routing tables
    routy:update(r1),
    timer:sleep(100),
    routy:update(r2),
    timer:sleep(100),
    routy:update(r3),
    timer:sleep(100),
    routy:update(r4),

    timer:sleep(2000),

    %print status
    routy:status(r1),
    routy:status(r2),
    routy:status(r3),
    routy:status(r4),

    timer:sleep(1000).

    %routy:update(r1),
    %routy:update(r2),
    %routy:update(r3),
    %routy:update(r4),

    %routy:status(r1).


    %send a message
    %r1 ! {route, goteborg, r1, "Hello from Stockholm to Goteborg!"},

    %timer:sleep(500),
    %Send a second message
    %r3 ! {route,goteborg,r1,"Hello bro"},

    %timer:sleep(500),
    %r1 ! {remove,lulea},
