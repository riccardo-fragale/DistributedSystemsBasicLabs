#!/bin/bash

pkill -u "$USER" beam.smp

echo "Erlang processes killed at: $(date +%s)"