#!/bin/bash
# Start an Erlang node with short name "kista" and cookie "1234"
# Then compile gms1, test, gui, worker.

erl -sname kista -setcookie 1234 -eval "
    compile:file(gms1),
    compile:file(test),
    compile:file(gui),
    compile:file(worker),
    compile:file(gms2),
    compile:file(gms3),
    compile:file(gms4).
"