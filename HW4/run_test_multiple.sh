#!/bin/sh

# Start node1 in a new Terminal window, compile modules, create and register the first worker
osascript <<EOF
tell application "Terminal"
    do script "cd $PWD; erl -sname node1 -setcookie mycookie -eval \"
    compile:file(test), compile:file(worker), compile:file(gms3), compile:file(gui),
    W1 = test:more(2, gms3, 1000), 
    register(w1, W1).\""
end tell
EOF

sleep 3

# Start node2 in another Terminal window, compile modules, connect, and add a worker using the registered name
osascript <<EOF
tell application "Terminal"
    do script "cd $PWD; erl -sname node2 -setcookie mycookie -eval \"
    compile:file(test), compile:file(worker), compile:file(gms3), compile:file(gui), 
    net_adm:ping('node1@$(hostname -s)'), 
    test:add(3, gms3, {w1, 'node1@$(hostname -s)'}, 1000).\""
end tell
EOF