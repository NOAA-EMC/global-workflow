#!/bin/bash
#Simplest calculator two dates difference. By default in days

# Usage:
# ./datediff.sh first_date second_date [-(s|m|h|d) | --(seconds|minutes|hours|days)]

first_date=$(date -d "$1" "+%s")
second_date=$(date -d "$2" "+%s")

case "$3" in
"--seconds" | "-s") period=1;;
"--minutes" | "-m") period=60;;
"--hours" | "-h") period=$((60*60));;
"--days" | "-d" | "") period=$((60*60*24));;
esac

datediff=$(( ($first_date - $second_date)/($period) ))
echo $datediff
