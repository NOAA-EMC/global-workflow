#! /usr/bin/env bash

#######
# Preamble script to be SOURCED at the beginning of every script. Sets 
#   useful PS4 and optionally turns on set -x and set -eu. Also sets up 
#   crude script timing and provides a postamble that runs on exit.
#
# Syntax:
#   preamble.sh [id]
#   
#   Aruguments:
#     id: Optional identifier string. Use when running the same script 
#           multiple times in the same job (e.g. MPMD)
#
# Input environment variables:
#   TRACE (YES/NO): Whether to echo every command (set -x) [default: "YES"]
#   STRICT (YES/NO): Whether to exit immediately on error or undefined variable
#     (set -eu) [default: "YES"]
#
#######
set +x
if [[ -v '1' ]]; then
	id="(${1})"
else
	id=""
fi

# Record the start time so we can calculate the elapsed time later
start_time=$(date +%s)

# Get the base name of the calling script
_calling_script=$(basename ${BASH_SOURCE[1]})

# Announce the script has begun
echo "Begin ${_calling_script} at $(date -u)"

# Stage our variables
export STRICT=${STRICT:-"YES"}
export TRACE=${TRACE:-"YES"}
export ERR_EXIT_ON=""
export TRACE_ON=""

if [[ $STRICT == "YES" ]]; then
	# Exit on error and undefined variable
	export ERR_EXIT_ON="set -eu"
fi
if [[ $TRACE == "YES" ]]; then
 	export TRACE_ON="set -x"
	# Print the script name and line number of each command as it is executed
	export PS4='+ $(basename $BASH_SOURCE)[$LINENO]'"$id: "
fi

postamble() {
	#
	# Commands to execute when a script ends. 
	#
	# Syntax:
	#   postamble script start_time rc
	#
	#   Arguments:
	#     script: name of the script ending
	#     start_time: start time of script (in seconds)
	#     rc: the exit code of the script
	#

	set +x
	script=${1}
	start_time=${2}
	rc=${3}

	# Calculate the elapsed time
	end_time=$(date +%s)
	elapsed_sec=$((end_time - start_time))
	elapsed=$(date -d@${elapsed_sec} -u +%H:%M:%S)

	# Announce the script has ended, then pass the error code up
	echo "End ${script} at $(date -u) with error code ${rc:-0} (time elapsed: ${elapsed})"
	exit ${rc}
}

# Place the postamble in a trap so it is always called no matter how the script exits
trap "postamble ${_calling_script} ${start_time} \$?" EXIT

# Turn on our settings
$ERR_EXIT_ON
$TRACE_ON
