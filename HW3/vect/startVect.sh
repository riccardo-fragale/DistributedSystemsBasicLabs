#!/bin/bash

# Start an Erlang node named Sweden with cookie 1234 and connect_all set to false,
# then compile the given modules and leave the shell running.
erl -name Norway@127.0.0.1 -setcookie 1234 -kernel connect_all false -eval "
    compile:file(my_loggy_vect),
    compile:file(holdb_queue_vect),
    compile:file(vect),
    compile:file(test_vect),
    compile:file(worker_vect).
"
