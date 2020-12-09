#! /usr/bin/env bash

# from https://medium.com/@zoispag/collect-restic-metrics-when-using-resticker-cf862f87fc41

# This script will execute restic commands and will pipe the output to jq,
# in order to format it in a prometheus compatible metric.
# Add this script in the host crontab, to be executed every few hours.

HOST=${HOST:-`hostname`}

TEXTFILE_COLLECTOR_DIR=/metrics
# Create directory.
[ -d ${TEXTFILE_COLLECTOR_DIR} ] || mkdir -p ${TEXTFILE_COLLECTOR_DIR}

# Create a temp unique file, that will not be parsed by node exporter.
TEMP_FILE="${TEXTFILE_COLLECTOR_DIR}/restic.prom.$$"
PERM_FILE="${TEXTFILE_COLLECTOR_DIR}/restic.prom"
touch ${TEMP_FILE}

# Note the start time of the script.
START="$(date +%s)"

# Get last backup timestamp
restic snapshots --host ${HOST} latest --json | jq -r 'max_by(.time) | .time | sub("[.][0-9]+"; "") | sub("Z"; "+00:00") | def parseDate(date): date | capture("(?<no_tz>.*)(?<tz_sgn>[-+])(?<tz_hr>\\d{2}):(?<tz_min>\\d{2})$") | (.no_tz + "Z" | fromdateiso8601) - (.tz_sgn + "60" | tonumber) * ((.tz_hr | tonumber) * 60 + (.tz_min | tonumber)); parseDate(.) | "restic_last_snapshot_ts \(.)"' >> ${TEMP_FILE}
# Get last backup size in bytes and files count
restic stats --host ${HOST} latest --json | jq -r '"restic_stats_total_size_bytes \(.total_size)\nrestic_stats_total_file_count \(.total_file_count)"' >> ${TEMP_FILE}

# Write out metrics to a temporary file.
END="$(date +%s)"
echo "restic_collector_duration_seconds $(($END - $START))" >> ${TEMP_FILE}
echo "restic_collector_last_run_ts ${END}" >> ${TEMP_FILE}

# Rename the temporary file atomically.
# This avoids the node exporter seeing half a file.
# In case a temp file was not created, delete the permanent file,
# to avoid outdated metrics.
mv "${TEMP_FILE}" "${PERM_FILE}" || rm "${PERM_FILE}"
