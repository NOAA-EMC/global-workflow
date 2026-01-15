#!/usr/bin/env bash
source {HOMEgfs}/dev/ush/gw_setup.sh

# Run rocotorun
{rocotorunstr}

# Monitor for failed jobs using rocotostat
LOCKFILE={expdir}/.failed_jobs.lock
ROCOTOSTAT=$(which rocotostat)

if [[ -n "$ROCOTOSTAT" ]]; then
    FAILED_JOBS=$($ROCOTOSTAT -d {expdir}/{pslot}.db -w {expdir}/{pslot}.xml -c all 2>/dev/null | grep -E 'DEAD')

    if [[ -n "$FAILED_JOBS" ]]; then
        # Read previously reported failures
        PREV_FAILED=""
        if [[ -f "$LOCKFILE" ]]; then
            PREV_FAILED=$(cat "$LOCKFILE")
        fi

        # Check for NEW failures only (not just changes)
        NEW_FAILURES=""
        while IFS= read -r job; do
            if [[ -n "$job" ]] && ! echo "$PREV_FAILED" | grep -qF "$job"; then
                NEW_FAILURES="${{NEW_FAILURES}}${{job}}"$'\n'
            fi
        done <<< "$FAILED_JOBS"

        # Send email only if there are NEW failures
        if [[ -n "$NEW_FAILURES" ]]; then
            TIMESTAMP=$(date +%Y%m%d%H%M%S)
            MSGFILE="/tmp/rocoto_fail_msg_$$.txt"
            NOTIFYFILE="{expdir}/logs/NEW_FAILED_JOBS_${{TIMESTAMP}}.txt"
            EMAILLOG="{expdir}/logs/scron_email.log"

            # Create logs directory if it doesn't exist
            mkdir -p "{expdir}/logs"

            echo "The following NEW jobs have failed in experiment {pslot}:" > "$MSGFILE"
            echo "" >> "$MSGFILE"

            # Format each failed job with detailed information
            while IFS= read -r line; do
                if [[ -n "$line" ]]; then
                    # Parse rocotostat output: Cycle Task JobID State Try MaxTries Duration
                    read -r cycle task jobid state try maxtries duration <<< "$line"
                    # Extract YYYYMMDDHH from cycle (first 10 characters)
                    cycle_short=${{cycle:0:10}}
                    # Get current timestamp
                    timestamp=$(date -u '+%m/%d/%y %H:%M:%S UTC')

                    # Format similar to user's example
                    echo "$timestamp :: {pslot}.xml :: Cycle $cycle, Task $task, \
                        jobid=$jobid, in state $state, ran for $duration seconds, \
                        try=$try (of $maxtries)" >> "$MSGFILE"
                    echo "Error log: {comroot}/{pslot}/logs/$cycle_short/$task.log" >> "$MSGFILE"
                    echo "" >> "$MSGFILE"
                fi
            done <<< "$NEW_FAILURES"

            # Try to send email
            EMAIL="{replyto}"
            EMAIL_SENT=false
            if [[ -n "$EMAIL" ]]; then
                if command -v mailx &> /dev/null; then
                    cat "$MSGFILE" | mailx -s "[{pslot}] NEW Workflow Job Failures Detected" "$EMAIL" 2>&1 | tee -a "$EMAILLOG"
                    [[ ${{PIPESTATUS[0]}} -eq 0 ]] && EMAIL_SENT=true
                elif command -v mail &> /dev/null; then
                    cat "$MSGFILE" | mail -s "[{pslot}] NEW Workflow Job Failures Detected" "$EMAIL" 2>&1 | tee -a "$EMAILLOG"
                    [[ ${{PIPESTATUS[0]}} -eq 0 ]] && EMAIL_SENT=true
                fi

                if [[ "$EMAIL_SENT" == "true" ]]; then
                    echo "[$(date)] Email notification sent to $EMAIL" >> "$EMAILLOG"
                else
                    echo "[$(date)] Failed to send email notification to $EMAIL" >> "$EMAILLOG"
                fi
            fi

            # Always save notification file for manual checking
            cp "$MSGFILE" "$NOTIFYFILE"
            echo "[$(date)] NEW failed jobs notification saved to: $NOTIFYFILE" >> "$EMAILLOG"

            rm -f "$MSGFILE"
        fi

        # Always update lockfile to reflect current failures
        echo "$FAILED_JOBS" > "$LOCKFILE"
    else
        # No failures, remove lockfile if it exists
        [[ -f "$LOCKFILE" ]] && rm -f "$LOCKFILE"
    fi
fi
