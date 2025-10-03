
-module(test).
-compile(export_all).
-define(Timeout, 1000).

%% Starting up a set of nodes is made easier using this function.
start(Module) ->
	Id = key:generate(),
	apply(Module, start, [Id]).

start(Module, P) ->
	Id = key:generate(),
	apply(Module, start, [Id, P]).

start(_, 0, _) ->
	ok;
start(Module, N, P) ->
	start(Module, P),
	start(Module, N-1, P).

%% The functions add and lookup can be used to test if a DHT works.
add(Key, Value, P) ->
	Q = make_ref(),
	P ! {add, Key, Value, Q, self()},
	receive
		{Q, ok} -> ok
	after ?Timeout ->
		{error, "timeout"}
	end.

lookup(Key, Node) ->
	Q = make_ref(),
	Node ! {lookup, Key, Q, self()},
	receive
		{Q, Value} -> Value
	after ?Timeout ->
		{error, "timeout"}
	end.

%% This benchmark can be used for a DHT where we can add and lookup key.
%% In order to use it you need to implement a store.
keys(N) ->
	lists:map(fun(_) -> key:generate() end, lists:seq(1, N)).

add(Keys, P) ->
	lists:foreach(fun(K) -> add(K, gurka, P) end, Keys).

check(Keys, P) ->
	T1 = now(),
	{Failed, Timeout} = check(Keys, P, 0, 0),
	T2 = now(),
	Done = (timer:now_diff(T2, T1) div 1000),
	io:format("~w lookup operation in ~w ms ~n", [length(Keys), Done]),
	io:format("~w lookups failed, ~w caused a timeout ~n", [Failed, Timeout]).

check([], _, Failed, Timeout) ->
	{Failed, Timeout};
check([Key|Keys], P, Failed, Timeout) ->
	case lookup(Key, P) of
		{Key, _} -> check(Keys, P, Failed, Timeout);
		{error, _} -> check(Keys, P, Failed, Timeout+1);
		false -> check(Keys, P, Failed+1, Timeout)
	end.


run() ->
    %% Start first node (bootstrap)
    Id1 = key:generate(),
    P1 = node1:start(Id1),

    %% Start 3 more nodes connected to P1
    Id2 = key:generate(),
    P2 = node1:start(Id2, P1),

    Id3 = key:generate(),
    P3 = node1:start(Id3, P1),

    Id4 = key:generate(),
    P4 = node1:start(Id4, P1),

    io:format("Started 4 nodes: ~p, ~p, ~p, ~p~n", [P1, P2, P3, P4]),

    %% Give stabilize some time
    timer:sleep(2000),

	P1 ! {print_state},
	P2 ! {print_state},
	P3 ! {print_state},
	P4 ! {print_state}.

    %% Generate keys
    %Keys = keys(5),
    %io:format("Generated keys: ~p~n", [Keys]),

    %% Add them via P1
    %add(Keys, P1),

    %% Check lookups
    %check(Keys, P1).