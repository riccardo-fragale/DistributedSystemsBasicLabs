-module(dijkstra_debug).

%%CREATED TO DO THE OPTIONAL TASK
-export([bench/0]).
-define(ITALY,'italy@130.229.168.103').
-define(SPAIN,'spain@130.229.168.103').
-define(SWEDEN,'sweden@n128-p22.eduroam.kth.se').

bench()->
    %Start the routers 
    routy:start(r1,stockholm),
    routy:start(r2,lund),
    routy:start(r3,lulea),
    routy:start(r4,orebro),
    routy:start(r5,goteborg),

    timer:sleep(1000),
    %Build the connections
    r1 ! {add,lund,{r2,?SWEDEN}},
    r2 ! {add,stockholm,{r1,?SWEDEN}},
    r1 ! {add,lulea,{r3,?SWEDEN}},
    r3 ! {add,stockholm,{r1,?SWEDEN}},
    r2 ! {add,orebro,{r4,?SWEDEN}},
    r4 ! {add,lund,{r2,?SWEDEN}},
    r4 ! {add,goteborg,{r5,?SWEDEN}},
    r5 ! {add,orebro,{r4,?SWEDEN}},

    timer:sleep(1500),
    %Gateway to Italy
    r3 ! {add,turin,{r3,?ITALY}},
    r3 ! {add,madrid,{r5,?SPAIN}},

    

    %io:format("Sleeping for routers to initialize"),
    timer:sleep(1000),

    io:format("Routers started and connected.~n"),

    %Broadcasting messages
    routy:broadcast(r1),
    %timer:sleep(100),
    routy:broadcast(r2),
    %timer:sleep(100),
    routy:broadcast(r3),
    %timer:sleep(100),
    routy:broadcast(r4),
    %timer:sleep(100),
    routy:broadcast(r5),

    timer:sleep(2500),
    
    %Updating routing tables
    routy:update(r1),
    %timer:sleep(100),
    routy:update(r2),
    %timer:sleep(100),
    routy:update(r3),
    %timer:sleep(100),
    routy:update(r4),
    %timer:sleep(100),
    routy:update(r5),

    timer:sleep(1000),

    %print status
    routy:status(r1),
    routy:status(r2),
    routy:status(r3),
    routy:status(r4),
    routy:status(r5),

    %timer:sleep(2000),
    %send a message
    %r1 ! {route, goteborg, r1, "Hello from Stockholm to Goteborg!"},

    %timer:sleep(500),
    %Send a second message
    %r3 ! {route,goteborg,r1,"Hello bro"},

    %timer:sleep(500),
    %r1 ! {remove,lund},
    %timer:sleep(200),
    %r1 ! {route, goteborg, r1, "Hello from Stockholm to Goteborg!"},


    ok.