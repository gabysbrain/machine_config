#!/usr/bin/env bash

# Dropbox-files
# lists a single filename if only a single file is being synced
# otherwise, echos the number of files synced

status=`dropbox status`
STATUS_REGEX="^Upgrading"

if [[ $status =~ $STATUS_REGEX ]]; then
  echo "Connected"
else
  echo $status
fi

