#!/bin/bash

# Start an Erlang node named Sweden with cookie 1234 and connect_all set to false,
# then compile the given modules and leave the shell running.
erl -name Sweden@127.0.0.1 -setcookie 1234 -kernel connect_all false -eval "
    compile:file(my_logger),
    compile:file(hold_queue),
    compile:file(time),
    compile:file(vector),
    compile:file(test),
    compile:file(worker).
"
