#!/bin/bash
###############################################################################
# Script: setup_scrontab_with_notifications.sh
# Description: Helper script to add Rocoto workflows to scrontab with email
#              notifications enabled
#
# Usage: ./setup_scrontab_with_notifications.sh EXPDIR [EMAIL]
###############################################################################

set -eu

EXPDIR="${1:-}"
EMAIL="${2:-${USER}@noaa.gov}"

if [[ -z "${EXPDIR}" ]]; then
  echo "Usage: $0 EXPDIR [EMAIL]"
  echo ""
  echo "Example:"
  echo "  $0 /scratch3/NCEPDEV/global/Anton.Fernando/RUNTESTS/EXPDIR/C96C48_ufs_hybatmDA_t1"
  echo "  $0 /path/to/expdir user@noaa.gov"
  exit 1
fi

if [[ ! -d "${EXPDIR}" ]]; then
  echo "ERROR: Experiment directory does not exist: ${EXPDIR}"
  exit 1
fi

# Find workflow XML and database
PSLOT=$(basename "${EXPDIR}")
XML_FILE="${EXPDIR}/${PSLOT}.xml"
DB_FILE="${EXPDIR}/${PSLOT}.db"

if [[ ! -f "${XML_FILE}" ]]; then
  echo "ERROR: Workflow XML not found: ${XML_FILE}"
  exit 1
fi

# Determine script location
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MONITOR_SCRIPT="${SCRIPT_DIR}/rocoto_monitor_notify.sh"

if [[ ! -f "${MONITOR_SCRIPT}" ]]; then
  echo "ERROR: Monitor script not found: ${MONITOR_SCRIPT}"
  exit 1
fi

echo "=============================================================================="
echo "Setting up scrontab entry with email notifications"
echo "=============================================================================="
echo "Experiment: ${PSLOT}"
echo "Directory:  ${EXPDIR}"
echo "Email:      ${EMAIL}"
echo "Monitor:    ${MONITOR_SCRIPT}"
echo "=============================================================================="
echo ""

# Generate scrontab entry
cat <<EOF

Add the following to your scrontab (scrontab -e):

#SCRON --mail-type=FAIL
#SCRON --mail-user=${EMAIL}
#SCRON --job-name=${PSLOT}
*/5 * * * * ${MONITOR_SCRIPT} -d ${DB_FILE} -w ${XML_FILE} -m ${EMAIL}

Or for crontab (crontab -e):

*/5 * * * * ${MONITOR_SCRIPT} -d ${DB_FILE} -w ${XML_FILE} -m ${EMAIL}

==============================================================================

Features enabled:
- Email on failed jobs (job-specific failures detected by rocotostat)
- Email on stalled workflows (no progress for >1 hour)
- Spam prevention (one email per unique failure state)
- SLURM scrontab failure notifications (--mail-type=FAIL)

To test the notification system:
  ${MONITOR_SCRIPT} -d ${DB_FILE} -w ${XML_FILE} -m ${EMAIL} -v

Lock files will be stored in:
  ${EXPDIR}/.rocoto_notify_locks/

For more information, see:
  ${SCRIPT_DIR}/ROCOTO_EMAIL_NOTIFICATIONS.md
==============================================================================
EOF
