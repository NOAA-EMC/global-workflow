# Rocoto Email Notification System

## Overview
This solution implements email notifications for scrontab-launched Rocoto workflows when jobs fail or workflows stall.

## Components

### 1. Monitoring Script
**File:** `dev/ci/scripts/rocoto_monitor_notify.sh`

A wrapper script that:
- Runs `rocotorun` to advance the workflow
- Monitors workflow status using `rocotostat`
- Detects failed and stalled jobs
- Sends email notifications
- Prevents email spam using lock files

### 2. Key Features

#### Failure Detection
- Parses `rocotostat` output for DEAD/FAILED jobs
- Tracks unique failure states using MD5 hashes
- Only sends one email per unique failure scenario

#### Stall Detection
- Identifies workflows with no QUEUED/RUNNING jobs but incomplete work
- Creates stall lock file on first detection
- Sends notification after 1 hour of stall condition
- Clears lock when workflow resumes

#### Spam Prevention
- Uses lock files in `.rocoto_notify_locks/` directory
- Hash-based tracking of failure states
- Separate locks for failed vs stalled notifications
- Automatic cleanup of old lock files

## Usage

### Basic Usage
```bash
./rocoto_monitor_notify.sh -d /path/to/workflow.db -w /path/to/workflow.xml
```

### With Custom Email
```bash
./rocoto_monitor_notify.sh -d /path/to/workflow.db -w /path/to/workflow.xml -m your.email@noaa.gov
```

### In scrontab
```bash
# Option 1: Direct usage (email via scrontab --mail-type=FAIL)
*/5 * * * * /path/to/rocoto_monitor_notify.sh -d /path/to/workflow.db -w /path/to/workflow.xml

# Option 2: With explicit email address
*/5 * * * * /path/to/rocoto_monitor_notify.sh -d /path/to/workflow.db -w /path/to/workflow.xml -m user@noaa.gov
```

### scrontab with SLURM Mail Type
To get SLURM notifications for script failures, add to scrontab:
```bash
#SCRON --mail-type=FAIL
#SCRON --mail-user=your.email@noaa.gov
*/5 * * * * /path/to/rocoto_monitor_notify.sh -d /path/to/workflow.db -w /path/to/workflow.xml
```

## Email Notifications

### Failed Jobs Email
```
Subject: FAILED: Rocoto workflow EXPERIMENT_NAME

Rocoto Workflow Notification
============================

Experiment: EXPERIMENT_NAME
Status: FAILED
Time: 2026-01-07 14:30:00
Database: /path/to/workflow.db
Workflow: /path/to/workflow.xml

FAILED Jobs:
-----------------------------
20240101 00:00:00  prep        DEAD
20240101 00:00:00  anal        FAILED
-----------------------------
```

### Stalled Workflow Email
```
Subject: STALLED: Rocoto workflow EXPERIMENT_NAME

Rocoto Workflow Notification
============================

Experiment: EXPERIMENT_NAME
Status: STALLED
Time: 2026-01-07 15:30:00

[Full rocotostat output showing workflow state]
```

## Lock File Management

Lock files are stored in: `<WORKFLOW_DIR>/.rocoto_notify_locks/`

### Lock File Types
- `failed_<hash>.lock` - Tracks unique failure states
- `stalled.lock` - Timestamps when stall first detected
- `stalled_notified.lock` - Prevents duplicate stall emails

### Cleanup
- Old failure locks automatically cleaned (keeps last 10)
- Stall locks removed when workflow resumes
- Notification locks persist until condition changes

## Integration with Existing Workflows

### Update Crontab Entries
Replace standard rocotorun calls:
```bash
# OLD
*/5 * * * * /apps/rocoto/1.3.7/bin/rocotorun -d /path/to/workflow.db -w /path/to/workflow.xml

# NEW
*/5 * * * * /path/to/rocoto_monitor_notify.sh -d /path/to/workflow.db -w /path/to/workflow.xml
```

### Update run_all_tests.sh
Modify the crontab entry generation to use the monitoring script:
```bash
# In run_all_tests.sh, replace rocotorun path with monitor script path
MONITOR_SCRIPT="${GW_HOMEgfs}/dev/ci/scripts/rocoto_monitor_notify.sh"
crontab_entry="*/5 * * * * ${MONITOR_SCRIPT} -d ${db_path} -w ${xml_path}"
```

## Testing

### Test Notifications
```bash
# Run in verbose mode
./rocoto_monitor_notify.sh -d workflow.db -w workflow.xml -v

# Check lock directory
ls -la .rocoto_notify_locks/

# Simulate failure (manually set job to DEAD in database)
sqlite3 workflow.db "UPDATE jobs SET state='DEAD' WHERE taskname='prep'"
./rocoto_monitor_notify.sh -d workflow.db -w workflow.xml
```

### Verify Email Delivery
Check that your environment has working mail/sendmail commands:
```bash
which mail
which sendmail
```

## Acceptance Criteria Verification

✅ **No change to CI tests** - Script is standalone, doesn't modify CI logic

✅ **scrontab-based runs launch successfully** - Compatible with existing scrontab syntax

✅ **Errors are reported via email** - Both failed and stalled conditions trigger emails

✅ **Only one email per failed job or group** - Lock file mechanism prevents spam

✅ **--mail-type=FAIL integration** - Works with SLURM scrontab directives

## Troubleshooting

### No emails received
1. Check mail command availability: `which mail sendmail`
2. Check lock directory for notification locks: `ls .rocoto_notify_locks/`
3. Run with `-v` flag to see verbose output
4. Check fallback notification files in workflow directory

### Duplicate emails
1. Verify lock directory is writable
2. Check for multiple crontab entries running simultaneously
3. Ensure lock files are not being manually deleted

### Stall notifications not working
1. Check that workflow has been stalled for >1 hour
2. Verify `rocotostat` is in PATH
3. Check `.rocoto_notify_locks/stalled.lock` timestamp

## Future Enhancements
- Configurable stall timeout (currently 1 hour)
- Grouped notifications (daily summary)
- Integration with Slack/Teams webhooks
- HTML-formatted emails with color coding
- Dashboard for monitoring multiple experiments
