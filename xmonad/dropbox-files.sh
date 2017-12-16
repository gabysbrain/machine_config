#!/usr/bin/env bash

# Dropbox-files
# lists a single filename if only a single file is being synced
# otherwise, echos the number of files synced

status=`dropbox status | grep Syncing`
SYNC_REGEX="([0-9,]+) files remaining"
FILENAME_REGEX='"(.*)"'

[[ $status =~ $SYNC_REGEX ]]
files_remaining="${BASH_REMATCH[1]}"
if [[ $files_remaining == "" ]]; then

    [[ $status =~ $FILENAME_REGEX ]]
    filename="${BASH_REMATCH[1]}"
    echo $filename

else
    echo "$files_remaining files"
fi
