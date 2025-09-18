#!/bin/bash
erlc rudy_multi.erl http.erl
erl -sname foo -setcookie secret -eval "rudy_multi:start(8080)."