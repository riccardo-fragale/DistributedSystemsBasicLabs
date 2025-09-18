-module(routy_demo).
-export([bench/0]).
%-define(ITALY,'italy@130.229.168.103').
%-define(SPAIN,'spain@130.229.168.103').
-define(SWEDEN,'sweden@n160-p24.eduroam.kth.se').
-define(FRANCE,'france@n160-p24.eduroam.kth.se').

bench()->
    %Start the routers 
    routy:start(r1,stockholm),
    routy:start(r2,lund),
    routy:start(r3,lulea),
    routy:start(r4,orebro),
    routy:start(r5,goteborg),


    timer:sleep(1500),

    %Build the connections
    r1 ! {add,lund,{r2,?SWEDEN}},
    r2 ! {add,stockholm,{r1,?SWEDEN}},
    r1 ! {add,lulea,{r3,?SWEDEN}},
    r3 ! {add,stockholm,{r1,?SWEDEN}},
    r2 ! {add,orebro,{r4,?SWEDEN}},
    r4 ! {add,lund,{r2,?SWEDEN}},
    r4 ! {add,goteborg,{r5,?SWEDEN}},
    r5 ! {add,orebro,{r4,?SWEDEN}},

    timer:sleep(500),
    r1 ! {add,paris,{r1,?FRANCE}},
    

    timer:sleep(1500),

    %Gateway to france
    %r1 ! {add,paris,{r1,?FRANCE}},

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
    timer:sleep(100),
    routy:broadcast(r5),

    timer:sleep(2000),
    
    %Updating routing tables
    routy:update(r1),
    timer:sleep(100),
    routy:update(r2),
    timer:sleep(100),
    routy:update(r3),
    timer:sleep(100),
    routy:update(r4),
    timer:sleep(100),
    routy:update(r5),

    timer:sleep(2000),

    %print status
    routy:status(r1),
    routy:status(r2),
    routy:status(r3),
    routy:status(r4),
    routy:status(r5),

    timer:sleep(1000),

    %send a message
    %r1 ! {route, goteborg, r1, "Hello from Stockholm to Goteborg!"},

    %r1 ! {remove,lulea},
    %timer:sleep(500),
    %Send a second message
    %r3 ! {route,goteborg,r1,"Hello bro"},

    %timer:sleep(500),
    %r1 ! {remove,lulea},


    timer:sleep(800).
    %routy:broadcast(r1),
    %routy:update(r1)
    %routy:update(r2),
    %routy:update(r4),
    %routy:update(r5),

    %io:format("~n Separation line ~n"),

    %timer:sleep(500),
    %routy:status(r1).


    %io:format("~n Separation line ~n"),
    %r1 ! {route, goteborg, r1, "Hello from Stockholm to Goteborg!"},


    %ok.
