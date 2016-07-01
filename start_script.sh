#!/bin/sh

if [ ! $# -eq 1 ]
then
  echo "Usage: ./start_script.sh [connection_file]"
  exit 1
fi

IERLANG_PATH=$(cd `dirname "$0"` && pwd)/
cd $IERLANG_PATH
CONNECTION_FILE=$1 escript bin/start_kernel
