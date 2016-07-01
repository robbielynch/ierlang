#!/bin/sh

if [ ! $# -eq 1 ]
then
  echo "Usage: ./start_script.sh [connection_file]"
  exit 1
fi

IERLANG_PATH=$(cd `dirname "$0"` && pwd)/
cd $IERLANG_PATH

rebar3 escriptize
./_build/default/rel/r1/bin/r1 start
./_build/default/rel/r1/bin/r1 eval "ierl_script:main(\""$1"\")."
