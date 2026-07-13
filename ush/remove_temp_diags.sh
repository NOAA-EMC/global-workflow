#! /usr/bin/env bash

#---------------------------------------------------------
# remove_temp_diags.sh
#
# Remove the shared temporary GSI diagnostic staging directory (gsidiags)
# that the analysis producer jobs create under DATAROOT and the diagnostic
# jobs consume. This is invoked as a standalone cleanup script at the end of
# the diagnostic jobs, after the diagnostic tarballs have been written to COM.
#
# Retention is gated by the existing KEEPDATA convention plus an optional
# KEEP_TEMP_DIAGS override, and removal is protected by a narrow safety guard
# so it can only ever delete the resolved gsidiags subtree. A cleanup problem
# is only ever surfaced as a log/warning line: the script always exits 0 and
# never aborts an otherwise successful job.
#
# Syntax:
#   remove_temp_diags.sh temp_diag_dir
#
#     temp_diag_dir: The resolved temporary diagnostic directory to remove,
#                    e.g. "${pCOMIN_ATMOS_ANALYSIS}/gsidiags".
#
# Environment used:
#   KEEPDATA               - existing retention flag (YES retains, else removes)
#   KEEP_TEMP_DIAGS        - optional override (YES / NO / unset / other)
#   DATAROOT               - used by the safety guard to scope removal
#   COMIN_ATMOS_ANALYSIS   - used by the safety guard (never deleted)
#   COMOUT_ATMOS_ANALYSIS  - used by the safety guard (never deleted)
#
# Example:
#   "${USHglobal}/remove_temp_diags.sh" "${pCOMIN_ATMOS_ANALYSIS}/gsidiags"
#---------------------------------------------------------

temp_diag_dir="${1:-}"

# Req 5.3: unset/empty path -> skip cleanup and exit success.
if [[ -z "${temp_diag_dir}" ]]; then
    echo "INFO: remove_temp_diags.sh: no temporary diagnostic directory specified; skipping cleanup"
    exit 0
fi

#-----------------------------------------------------
# Retention decision (design decision table).
# KEEP_TEMP_DIAGS overrides KEEPDATA; when KEEP_TEMP_DIAGS is unset/empty
# the directory follows the KEEPDATA decision. Matching is case-insensitive
# (YES/yes/Yes and NO/no all accepted).
#   YES         -> retain (any KEEPDATA)            (Req 3.4, 4.1)
#   NO          -> remove (any KEEPDATA)            (Req 3.5, 4.2)
#   unset/empty -> follow KEEPDATA                  (Req 3.1, 3.2, 3.3, 4.5)
#   other       -> retain + error indication (fail safe: an ambiguous/typo'd
#                  value must never trigger a deletion)   (Req 4.6)
#-----------------------------------------------------
keepdata="${KEEPDATA:-}"
keep_temp_diags="${KEEP_TEMP_DIAGS:-}"
action=""

# Upper-case the flag so YES/yes/Yes (and NO/no/No) are all accepted.
case "${keep_temp_diags^^}" in
    YES)
        action="retain"
        ;;
    NO)
        action="remove"
        ;;
    "")
        if [[ "${keepdata^^}" == "YES" ]]; then
            action="retain"
        else
            action="remove"
        fi
        ;;
    *)
        # Req 4.6: unrecognized value (likely a typo) -> RETAIN and emit an
        # error indication naming the offending value. Keeping scratch is
        # always safer than deleting on an ambiguous flag.
        echo "ERROR: remove_temp_diags.sh: unrecognized KEEP_TEMP_DIAGS value '${keep_temp_diags}'; retaining '${temp_diag_dir}' to be safe" >&2
        action="retain"
        ;;
esac

if [[ "${action}" == "retain" ]]; then
    echo "INFO: remove_temp_diags.sh: retaining temporary diagnostic directory '${temp_diag_dir}'"
    exit 0
fi

#-----------------------------------------------------
# Safety guard: scope removal strictly to the resolved gsidiags subtree.
# On any guard failure, log a skip message and exit 0 without deleting
# anything (Req 5.1, 5.3, 5.4, 5.5).
#-----------------------------------------------------

# Resolve the path lexically (do not follow symlinks, allow missing parts)
# so the containment/parent checks operate on a canonical path.
if ! resolved=$(realpath -m -s "${temp_diag_dir}" 2> /dev/null); then
    echo "WARNING: remove_temp_diags.sh: unable to resolve '${temp_diag_dir}'; skipping cleanup" >&2
    exit 0
fi

# Positive scoping: the basename must be exactly 'gsidiags'.
base="${resolved##*/}"
if [[ "${base}" != "gsidiags" ]]; then
    echo "WARNING: remove_temp_diags.sh: refusing to remove '${resolved}' (basename is not 'gsidiags'); skipping cleanup" >&2
    exit 0
fi

# DATAROOT must be defined to bound the removal.
if [[ -z "${DATAROOT:-}" ]]; then
    echo "WARNING: remove_temp_diags.sh: DATAROOT is not defined; skipping cleanup of '${resolved}'" >&2
    exit 0
fi

if ! dataroot_resolved=$(realpath -m -s "${DATAROOT}" 2> /dev/null); then
    echo "WARNING: remove_temp_diags.sh: unable to resolve DATAROOT '${DATAROOT}'; skipping cleanup" >&2
    exit 0
fi

# The resolved path must sit strictly under DATAROOT.
if [[ "${resolved}" != "${dataroot_resolved}/"* ]]; then
    echo "WARNING: remove_temp_diags.sh: '${resolved}' is not under DATAROOT '${dataroot_resolved}'; skipping cleanup" >&2
    exit 0
fi

# Reject a path equal to, or a parent of, any protected path (Req 5.4).
for protected in "${DATAROOT}" "${COMIN_ATMOS_ANALYSIS:-}" "${COMOUT_ATMOS_ANALYSIS:-}"; do
    if [[ -z "${protected}" ]]; then
        continue
    fi
    if ! prot_resolved=$(realpath -m -s "${protected}" 2> /dev/null); then
        continue
    fi
    if [[ "${resolved}" == "${prot_resolved}" ]]; then
        echo "WARNING: remove_temp_diags.sh: '${resolved}' equals protected path '${prot_resolved}'; skipping cleanup" >&2
        exit 0
    fi
    if [[ "${prot_resolved}" == "${resolved}/"* ]]; then
        echo "WARNING: remove_temp_diags.sh: '${resolved}' is a parent of protected path '${prot_resolved}'; skipping cleanup" >&2
        exit 0
    fi
done

# Reject a gsidiags symlink whose physical target is outside DATAROOT
# (Req 5.5): do not delete through a link that escapes the subtree.
if [[ -L "${temp_diag_dir}" ]]; then
    if ! physical=$(realpath "${temp_diag_dir}" 2> /dev/null); then
        echo "WARNING: remove_temp_diags.sh: unable to resolve symlink '${temp_diag_dir}'; skipping cleanup" >&2
        exit 0
    fi
    if [[ "${physical}" != "${dataroot_resolved}/"* ]]; then
        echo "WARNING: remove_temp_diags.sh: '${temp_diag_dir}' is a symlink pointing outside DATAROOT ('${physical}'); skipping cleanup" >&2
        exit 0
    fi
fi

#-----------------------------------------------------
# Removal.
#-----------------------------------------------------

# Req 1.3, 2.3: nothing to remove -> log INFO and exit success.
if [[ ! -e "${resolved}" && ! -L "${resolved}" ]]; then
    echo "INFO: remove_temp_diags.sh: '${resolved}' does not exist; nothing to remove"
    exit 0
fi

# Req 1.1, 1.2, 2.1, 2.2, 5.1: remove only the resolved gsidiags subtree.
echo "INFO: remove_temp_diags.sh: removing temporary diagnostic directory '${resolved}'"
if ! rm -rf "${resolved}"; then
    # Req 1.5, 2.6: on failure emit a warning naming the path and still exit
    # success so the job completes normally.
    echo "WARNING: remove_temp_diags.sh: failed to remove '${resolved}'; leaving it in place" >&2
fi

exit 0
