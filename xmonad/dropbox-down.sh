#!/usr/bin/env bash

# drobox-down
# echos the Dropbox download speed

status=`dropbox status | grep Downloading`
SYNC_REGEX="([0-9,]+) KB/sec"

[[ $status =~ $SYNC_REGEX ]]
download_speed="${BASH_REMATCH[1]}"
if [[ $download_speed != "" ]] ; then
  echo "$download_speed KB/sec"
fi

