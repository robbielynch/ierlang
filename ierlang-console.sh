#!/bin/sh

#Compile erlang modules
echo "Compiling IErlang..."
./compile-erlang-modules.sh

#Start ierlang console
echo "Starting IErlang Console..."
cd src
ERL_LIBS=$ERL_LIBS ./start-ierl-console.sh