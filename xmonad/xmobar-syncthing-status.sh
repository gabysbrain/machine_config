#!/bin/sh

status="$(syncthing-quick-status)"

conns=$(echo "${status}" | grep 'tcp-server\|relay-server' | wc -l)
syncs=$(echo "${status}" | grep 'syncing' | wc -l)

if [ $syncs -gt 0 ]; then
  echo "↯"
elif [ $conns -gt 0 ]; then
  echo "✓"

else
  echo "X"
fi


