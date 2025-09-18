#!/bin/bash

# Get current working directory
CURRENT_DIR=$(pwd)

# Open a new Terminal window and start Erlang node in current directory
osascript -e "tell application \"Terminal\" to do script \"cd $CURRENT_DIR; erl -name france -setcookie routy -connect_all false -eval 'compile:file(\\\"routy\\\"), compile:file(\\\"intf\\\"), compile:file(\\\"map\\\"), compile:file(\\\"hist\\\"),compile:file(\\\"routy_demo\\\"),compile:file(\\\"routy_machine\\\"), compile:file(\\\"dijkstra\\\").'\""


