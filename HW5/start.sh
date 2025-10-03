#!/bin/sh

# Start an Erlang node named Kista with cookie 1234
erl -sname kista -setcookie 1234 -eval "
    % Compile the modules
    compile:file(test),
    compile:file(node1),
    compile:file(key),
    io:format(\"Modules compiled successfully.~n\"),
"
