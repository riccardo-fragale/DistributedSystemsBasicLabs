#!/bin/bash
cd "/Users/itsrich/Documents/VSCode/DistributedSystems/HW5"
erl -sname lulea -setcookie 1234 -kernel connect_all false -eval "$(cat '/Users/itsrich/Documents/VSCode/DistributedSystems/HW5/start_lulea.cmd')"
