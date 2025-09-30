-module(test). 

-compile(export_all).
% Used to create the first worker, try: 
% % W1 = test:first(1, gms1, 1000) 
first(N, Module, Sleep) -> 
        worker:start(N, Module, random:uniform(256), Sleep). 
        
% Used to create additional workers, try: 
% % test:add(2, gms1, W1, 1000) and
% test:add(3, gms1, W1, 1000) and ... 
add(N, Module, Wrk, Sleep) -> 
        worker:start(N, Module, random:uniform(256), Wrk, Sleep). 

%% To create a number of workers in one go, 
more(N, Module, Sleep) when N > 1 -> 
        Wrk = first(1, Module, Sleep), 
        Ns = lists:seq(2,N), 
        lists:map(fun(Id) -> add(Id, Module, Wrk, Sleep) end, Ns), 
        Wrk. 


% These are messages that we can send to one of the workers. It will 
% multicast it to all workers. They should (if everything works) 
% receive the message at the same (logical) time. 
freeze(Wrk) -> 
        Wrk ! {send, freeze}. 

go(Wrk) -> 
        Wrk ! {send, go}. 

sleep(Wrk, Sleep) -> 
        Wrk ! {send, {sleep, Sleep}}. 
        
stop(Wrk) -> 
    Wrk ! {send, stop}.

stresstest() ->
        Duration = 60,  % seconds
        Sleep1 = 3000,   % ms between operations
        Sleep2 = 2000,
        io:format("Starting stress test: ~p seconds~n", [Duration]),
        W1 = first(1, gms3, 1000),
        timer:sleep(Sleep1),
        W2 = add(2, gms3, W1, 1000),
        timer:sleep(Sleep1),
        W3 = add(3, gms3, W2, 1000),
        timer:sleep(Sleep1),
        W4 = add(4, gms3, W3, 1000),
        timer:sleep(Sleep1),
        exit(W1, kill),
        W5 = add(5, gms3, W4, 1000),
        timer:sleep(Sleep1),
        exit(W3, kill),
        timer:sleep(Sleep2),
        W6 = add(6, gms3, W5, 1000),
        timer:sleep(Sleep1),
        exit(W2, kill),
        timer:sleep(Sleep2),
        W7 = add(7, gms3, W6, 1000),
        timer:sleep(Sleep2),
        exit(W4, kill),
        timer:sleep(Sleep1),
        W8 = add(8, gms3, W7, 1000),
        timer:sleep(Sleep2),
        exit(W5, kill),
        timer:sleep(Sleep2),
        W9 = add(9, gms3, W8, 1000),
        timer:sleep(Sleep1),
        W10 = add(10, gms3, W9, 1000),
        timer:sleep(Sleep1),
        exit(W7, kill),
        exit(W6,kill),
        timer:sleep(Sleep2),
        W11 = add(11, gms3, W10, 1000),
        timer:sleep(Sleep2),
        exit(W8, kill),
        W12 = add(12, gms3, W11, 1000).
        

