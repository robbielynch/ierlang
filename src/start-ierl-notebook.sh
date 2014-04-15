#!/bin/sh

#Start IErlang Notebook
##Important - change escript dir in next command to the correct dir according to your machine
## on OSX, it could be     /usr/local/bin/escript
## on Ubuntu, it could be  /usr/lib/erlang/bin/escript
ipython2 notebook --KernelManager.kernel_cmd='["/usr/lib/erlang/bin/escript", "ipython_kernel.erl", "{connection_file}"]' --Session.key="" --Session.keyfile=""

##TODO - move kernel to diff directory and test from there using the command:
##ipython2 notebook --KernelManager.kernel_cmd='["/usr/bin/escript", "/home/robbie/.ipython/profile_erlang/erlang_kernel/src/ipython_kernel.erl", "{connection_file}"]' --Session.key="" --Session.keyfile="" --NbConvertBase.default_language="erlang"
