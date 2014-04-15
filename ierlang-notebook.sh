#!/bin/sh

#Compile erlang modules
echo "Compiling IErlang..."
./compile-erlang-modules.sh

#Start ierlang notebook
echo "Starting IErlang Notebook..."
cd src
ERL_LIBS=$ERL_LIBS ./start-ierl-notebook.sh