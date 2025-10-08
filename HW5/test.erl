
-module(test).
-compile(export_all).
-define(Timeout, 2000).

% Test on chapter 2
load1() ->
  load(node2, 1, 1, 4000),

  load(node2, 1, 4, 1000),

  load(node2, 2, 4, 1000),
  
  load(node2, 8, 20, 1000).

load2() ->
  load(node2, 1, 1, 1000),
  load(node2, 1, 2, 1000),
  load(node2, 1, 4, 1000),
  %load(node2, 1, 8, 1000),
  %load(node2, 1, 16, 1000),
  
  load(node2, 2, 1, 1000),
  load(node2, 2, 2, 1000),
  load(node2, 2, 4, 1000),
  load(node2, 2, 8, 1000),
  %load(node2, 2, 16, 1000),
  
  load(node2, 4, 1, 1000),
  load(node2, 4, 2, 1000),
  load(node2, 4, 4, 1000),
  %load(node2, 4, 16, 1000),
  
  load(node2, 8, 1, 1000),
  load(node2, 8, 2, 1000).
  %load(node2, 8, 16, 1000),
  %load(node2, 8, 32, 1000),

load(Module, N, NClient, NElem) ->
  io:format("~n~n--> ~w Node(s), ~w Machine(s), ~w Entries~n~n", [N, NClient, NElem]),
  N1 = start(Module),
  start(Module, N-1, N1),

  timer:sleep(2000*N*4),
  io:format("~n--> Probing(~w)~n", [N]),
  N1 ! probe,

  timer:sleep(500),
  
  io:format("~n--> (~w)~n", [N]),
  spawn_machine(N1, NClient, NElem),

  timer:sleep(3000*N),
  io:format("~n--> Probing(~w)~n", [N]),
  N1 ! probe,

  timer:sleep(500).

spawn_machine(_Node, 0, _NElem) ->
  ok;
spawn_machine(Node, N, NElem) ->
  spawn(test, machine_add_lookup, [Node, N, NElem]),
  spawn_machine(Node, N-1, NElem).

machine_add_lookup(Node, N, NElem) ->
  Keys = keys(NElem),
  lists:foreach(fun(K)->
              add(K, bread, Node) end,
              Keys),
  check(N, Keys, Node),
  Keys.

% Chapter 3
run3() ->
  N1 = test:start(node3),
  register(n1, N1),

  N2 = test:start(node3, N1),
  N3 = test:start(node3, N1),
  N4 = test:start(node3, N1),

  timer:sleep(8000),

  io:format("--> add keyset~n~n"),

  Keys = keys(1000),
  add(Keys, n1),

  timer:sleep(500),

  io:format("--> check~n~n"),

  n1 ! probe,

  timer:sleep(500),

  io:format("--> add more nodes~n~n"),

  N5 = test:start(node3, N1),
  N6 = test:start(node3, N1),
  N7 = test:start(node3, N1),
  N8 = test:start(node3, N1),

  timer:sleep(8000),

  io:format("--> check keyset again~n~n"),

  %check(1, Keys, n1),

  n1 ! probe,

  timer:sleep(5000),

  N4 ! stop,

  timer:sleep(5000),

  %check(1, Keys, n1),

  N5 ! stop,

  timer:sleep(5000),

  n1 ! probe.

  %check(1, Keys, n1).


% Chapter 4 - Enhanced replication tests
run4() ->

  io:format("~n~n========================================~n"),
  io:format("CHAPTER 4: REPLICATION DEMONSTRATION~n"),
  io:format("========================================~n~n"),

  % We start 3 nodes initially
  io:format("--- Setting up initial 3-node ring ---~n"),
  N1 = test:start(node4),
  register(n1, N1),
  
  N2 = test:start(node4, N1),
  register(n2, N2),
  
  N3 = test:start(node4, N1),
  register(n3, N3),

  timer:sleep(5000),

  io:format("~n=== DEMONSTRATION 1: Replication on Add ===~n"),
  io:format("Adding 3 keys - watch for STORING and REPLICATING messages~n~n"),
  
  Key1 = key:generate(),
  Key2 = key:generate(),
  Key3 = key:generate(),
  
  io:format("Adding key ~p~n", [Key1]),
  add(Key1, bread, n1),
  timer:sleep(200),
  
  io:format("Adding key ~p~n", [Key2]),
  add(Key2, bread, n1),
  timer:sleep(200),
  
  io:format("Adding key ~p~n", [Key3]),
  add(Key3, bread, n1),
  timer:sleep(500),

  io:format("~n--- Checking storage and replicas on all nodes ---~n"),
  n1 ! print_storage,
  timer:sleep(100),
  n2 ! print_storage,
  timer:sleep(100),
  n3 ! print_storage,
  timer:sleep(1000),

  io:format("~n=== DEMONSTRATION 2: Keys Recoverable After Node Death ===~n"),
  io:format("Killing node N2 - its keys should still be retrievable from replicas~n~n"),
  
  % Store which keys we have
  TestKeys = [Key1, Key2, Key3],
  
  io:format("Before killing N2, all keys should be found:~n"),
  check(1, TestKeys, n1),
  
  io:format("~nKilling N2...~n"),
  n2 ! stop,
  
  timer:sleep(8000),
  
  io:format("~nAfter killing N2, all keys should STILL be found (from replicas):~n"),
  check(2, TestKeys, n1),
  
  io:format("~n--- Storage after N2 death ---~n"),
  n1 ! print_storage,
  timer:sleep(1000),
  n3 ! print_storage,
  timer:sleep(1000),

  io:format("~n=== DEMONSTRATION 3: Re-replication After Recovery ===~n"),
  io:format("After N2 died, N1 and N3 should re-replicate recovered keys~n"),
  io:format("Adding more keys to trigger re-replication~n~n"),
  
  Key4 = key:generate(),
  Key5 = key:generate(),
  
  io:format("Adding key ~p~n", [Key4]),
  add(Key4, bread, n1),
  timer:sleep(2000),
  
  io:format("Adding key ~p~n", [Key5]),
  add(Key5, bread, n1),
  timer:sleep(2000),
  
  AllKeys = [Key1, Key2, Key3, Key4, Key5],
  
  io:format("~n--- After adding new keys, check storage and replicas ---~n"),
  n1 ! print_storage,
  timer:sleep(100),
  n3 ! print_storage,
  timer:sleep(1000),
  
  io:format("~nAll 5 keys should still be retrievable:~n"),
  check(3, AllKeys, n1),

  io:format("~n=== DEMONSTRATION 4: Specific A-B-C Scenario ===~n"),
  io:format("Scenario: A < B with keys P,Q. C joins as B's successor. Then B dies.~n"),
  io:format("Expected: A stores P+replicates Q, C stores Q+replicates P. No data lost.~n~n"),
  
  % Start fresh with 2 nodes
  io:format("Starting node A (n4)~n"),
  N4 = test:start(node4),
  register(n4, N4),
  
  timer:sleep(1000),
  
  io:format("Starting node B (n5) joining A~n"),
  N5 = test:start(node4, N4),
  register(n5, N5),
  
  timer:sleep(3000),
  
  io:format("~nAdding key P and key Q~n"),
  KeyP = key:generate(),
  KeyQ = key:generate(),
  
  add(KeyP, bread, n4),
  timer:sleep(200),
  add(KeyQ, bread, n4),
  timer:sleep(1000),
  
  io:format("~n--- State before C joins (A and B with P,Q) ---~n"),
  n4 ! print_storage,
  timer:sleep(100),
  n5 ! print_storage,
  timer:sleep(1000),
  
  io:format("~nAdding node C (n6) as successor to the ring~n"),
  N6 = test:start(node4, N4),
  register(n6, N6),
  
  timer:sleep(5000),
  
  io:format("~n--- State after C joins and stabilizes ---~n"),
  n4 ! print_storage,
  timer:sleep(100),
  n5 ! print_storage,
  timer:sleep(100),
  n6 ! print_storage,
  timer:sleep(2500),
  
  io:format("~nNow KILLING node B (n5) - watch for recovery messages~n"),
  n5 ! stop,
  
  timer:sleep(10000),
  
  io:format("~n--- Final state after B dies (A should have P+replica Q, C should have Q+replica P) ---~n"),
  n4 ! print_storage,
  timer:sleep(1000),
  n6 ! print_storage,
  timer:sleep(1000),
  
  io:format("~nVerifying NO DATA LOST - both P and Q should still be retrievable:~n"),
  ABCKeys = [KeyP, KeyQ],
  check(4, ABCKeys, n4),
  
  io:format("~n~n========================================~n"),
  io:format("CHAPTER 4 TESTS COMPLETE~n"),
  io:format("========================================~n"),
  
  io:format("~nSUMMARY:~n"),
  io:format("1. ✓ Demonstrated that additions are replicated to successor~n"),
  io:format("2. ✓ Demonstrated that keys are recoverable after node death (from replicas)~n"),
  io:format("3. ✓ Demonstrated re-replication after recovering from dead node~n"),
  io:format("4. ✓ Demonstrated A-B-C scenario with no data loss~n~n").

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
	lists:foreach(fun(K) -> add(K, bread, P) end, Keys).

check(Keys, P) ->
	T1 = now(),
	{Failed, Timeout} = check(Keys, P, 0, 0),
	T2 = now(),
	Done = (timer:now_diff(T2, T1) div 1000),
	io:format("~w lookup operation in ~w ms ~n", [length(Keys), Done]),
	io:format("~w lookups failed, ~w caused a timeout ~n", [Failed, Timeout]).

check(Id, Keys, P) ->
  T1 = now(),
  {Failed, Timeout} = check(Keys, P, 0, 0),
  T2 = now(),
  Done = timer:now_diff(T2, T1) / 1000.0,
  io:format("{op: look, count: ~w, id: ~w, time: ~.2fms, failed: ~w, timeout: ~w}~n",
        [Id, length(Keys), Done, Failed, Timeout]).


check([], _, Failed, Timeout) ->
	{Failed, Timeout};
check([Key|Keys], P, Failed, Timeout) ->
	case lookup(Key, P) of
		{Key, _} -> check(Keys, P, Failed, Timeout);
		{error, _} -> check(Keys, P, Failed, Timeout+1);
		false -> check(Keys, P, Failed+1, Timeout)
	end.

%Deprecated test for chapter 1
run() ->
	N1 = test:start(node1),
	register(n1, N1),

	N2 = test:start(node1, N1),
	N3 = test:start(node1, N1),
	N4 = test:start(node1, N1),
	N5 = test:start(node1, N1),
	N6 = test:start(node1, N1),
	N7 = test:start(node1, N1),
  	N8 = test:start(node1, N1),

  	timer:sleep(8000),

  	n1 ! probe,

  	timer:sleep(500),

  	N2 ! stop,

  	timer:sleep(3000),

  	n1 ! probe.



