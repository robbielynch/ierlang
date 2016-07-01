#!/bin/sh

mkdir -p ~/.ipython/kernels/ierlang/
START_SCRIPT_PATH=$(cd `dirname "$0"` && pwd)/start_script.sh
CONTENT='{
   "argv": ["'${START_SCRIPT_PATH}'", "{connection_file}"],
                "display_name": "IErlang",
                "language": "Erlang"
}'
echo $CONTENT > ~/.ipython/kernels/ierlang/kernel.json
