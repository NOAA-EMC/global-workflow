#!/usr/bin/env bash
source {HOMEgfs}/dev/ush/gw_setup.sh

# Run rocotorun
{rocotorunstr}

# Monitor for failed jobs using rocotostat
LOCKFILE={expdir}/.failed_jobs.lock
ROCOTOSTAT=$(which rocotostat)

if [[ -n "$ROCOTOSTAT" ]]; then
    FAILED_JOBS=$($ROCOTOSTAT -d {expdir}/{pslot}.db -w {expdir}/{pslot}.xml -c all 2>/dev/null | grep -E 'DEAD|FAILED' | awk '{{print $1" "$2" "$4" (JobID: "$3")"}}' | sort -u)

    if [[ -n "$FAILED_JOBS" ]]; then
        # Read previously reported failures
        PREV_FAILED=""
        if [[ -f "$LOCKFILE" ]]; then
            PREV_FAILED=$(cat "$LOCKFILE")
        fi

        # Check if failures have changed
        if [[ "$FAILED_JOBS" != "$PREV_FAILED" ]]; then
            # Send email notification
            echo "The following jobs have failed in experiment {pslot}:" > /tmp/rocoto_fail_msg_$$.txt
            echo "" >> /tmp/rocoto_fail_msg_$$.txt
            echo "$FAILED_JOBS" | while read line; do echo "  - $line"; done >> /tmp/rocoto_fail_msg_$$.txt
            echo "" >> /tmp/rocoto_fail_msg_$$.txt
            echo "Experiment directory: {expdir}" >> /tmp/rocoto_fail_msg_$$.txt
            echo "Database: {expdir}/{pslot}.db" >> /tmp/rocoto_fail_msg_$$.txt
            echo "XML: {expdir}/{pslot}.xml" >> /tmp/rocoto_fail_msg_$$.txt
            echo "" >> /tmp/rocoto_fail_msg_$$.txt
            echo "Check logs with: rocotostat -d" >> /tmp/rocoto_fail_msg_$$.txt
            echo "  {expdir}/{pslot}.db -w {expdir}/{pslot}.xml -c all" >> /tmp/rocoto_fail_msg_$$.txt

            # Determine email address (use REPLYTO or fallback to USER@noaa.gov)
            EMAIL="{replyto}"
            if command -v mail &> /dev/null && [[ -n "$EMAIL" ]]; then
                cat /tmp/rocoto_fail_msg_$$.txt | mail -s "[{pslot}] Workflow Job Failures Detected" "$EMAIL"
            fi
            rm -f /tmp/rocoto_fail_msg_$$.txt

            # Update lockfile with current failures
            echo "$FAILED_JOBS" > "$LOCKFILE"
        fi
    else
        # No failures, remove lockfile if it exists
        [[ -f "$LOCKFILE" ]] && rm -f "$LOCKFILE"
    fi
fi
