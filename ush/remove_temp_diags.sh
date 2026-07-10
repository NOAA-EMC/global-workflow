#! /usr/bin/env bash

#---------------------------------------------------------
# remove_temp_diags.sh
#
# Remove the shared temporary GSI diagnostic staging directory (gsidiags)
# that the analysis producer jobs create under DATAROOT and the diagnostic
# jobs consume. This is called from the diagnostic job postamble after the
# diagnostic tarballs have been written to COM.
#
# Retention is gated by the existing KEEPDATA convention plus an optional
# KEEP_TEMP_DIAGS override, and removal is protected by a narrow safety guard
# so it can only ever delete the resolved gsidiags subtree. A cleanup problem
# is only ever surfaced as a log/warning line: the function always returns 0
# and never aborts an otherwise successful job (it is safe under
# `set -eu -o pipefail`).
#
# Syntax:
#   remove_temp_diags temp_diag_dir
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
#   remove_temp_diags "${pCOMIN_ATMOS_ANALYSIS}/gsidiags"
#---------------------------------------------------------

remove_temp_diags() {
    set +x
    local temp_diag_dir="${1:-}"

    # Req 5.3: unset/empty path -> skip cleanup and return success.
    if [[ -z "${temp_diag_dir}" ]]; then
        echo "INFO: remove_temp_diags: no temporary diagnostic directory specified; skipping cleanup"
        set -x
        return 0
    fi

    #-----------------------------------------------------
    # Retention decision (design decision table).
    # KEEP_TEMP_DIAGS overrides KEEPDATA; when KEEP_TEMP_DIAGS is unset/empty
    # the directory follows the KEEPDATA decision.
    #   YES         -> retain (any KEEPDATA)            (Req 3.4, 4.1)
    #   NO          -> remove (any KEEPDATA)            (Req 3.5, 4.2)
    #   unset/empty -> follow KEEPDATA                  (Req 3.1, 3.2, 3.3, 4.5)
    #   other       -> remove + error indication        (Req 4.6)
    #-----------------------------------------------------
    local keepdata="${KEEPDATA:-}"
    local keep_temp_diags="${KEEP_TEMP_DIAGS:-}"
    local action

    case "${keep_temp_diags}" in
        YES)
            action="retain"
            ;;
        NO)
            action="remove"
            ;;
        "")
            if [[ "${keepdata}" == "YES" ]]; then
                action="retain"
            else
                action="remove"
            fi
            ;;
        *)
            # Req 4.6: unrecognized value -> remove and emit an error indication
            # naming the offending value.
            echo "ERROR: remove_temp_diags: unrecognized KEEP_TEMP_DIAGS value '${keep_temp_diags}'; treating as 'NO' and removing '${temp_diag_dir}'" >&2
            action="remove"
            ;;
    esac

    if [[ "${action}" == "retain" ]]; then
        echo "INFO: remove_temp_diags: retaining temporary diagnostic directory '${temp_diag_dir}'"
        set -x
        return 0
    fi

    #-----------------------------------------------------
    # Safety guard: scope removal strictly to the resolved gsidiags subtree.
    # On any guard failure, log a skip message and return 0 without deleting
    # anything (Req 5.1, 5.3, 5.4, 5.5).
    #-----------------------------------------------------

    # Resolve the path lexically (do not follow symlinks, allow missing parts)
    # so the containment/parent checks operate on a canonical path.
    local resolved
    if ! resolved=$(realpath -m -s "${temp_diag_dir}" 2>/dev/null); then
        echo "WARNING: remove_temp_diags: unable to resolve '${temp_diag_dir}'; skipping cleanup" >&2
        set -x
        return 0
    fi

    # Positive scoping: the basename must be exactly 'gsidiags'.
    local base="${resolved##*/}"
    if [[ "${base}" != "gsidiags" ]]; then
        echo "WARNING: remove_temp_diags: refusing to remove '${resolved}' (basename is not 'gsidiags'); skipping cleanup" >&2
        set -x
        return 0
    fi

    # DATAROOT must be defined to bound the removal.
    if [[ -z "${DATAROOT:-}" ]]; then
        echo "WARNING: remove_temp_diags: DATAROOT is not defined; skipping cleanup of '${resolved}'" >&2
        set -x
        return 0
    fi

    local dataroot_resolved
    if ! dataroot_resolved=$(realpath -m -s "${DATAROOT}" 2>/dev/null); then
        echo "WARNING: remove_temp_diags: unable to resolve DATAROOT '${DATAROOT}'; skipping cleanup" >&2
        set -x
        return 0
    fi

    # The resolved path must sit strictly under DATAROOT.
    if [[ "${resolved}" != "${dataroot_resolved}/"* ]]; then
        echo "WARNING: remove_temp_diags: '${resolved}' is not under DATAROOT '${dataroot_resolved}'; skipping cleanup" >&2
        set -x
        return 0
    fi

    # Reject a path equal to, or a parent of, any protected path (Req 5.4).
    local protected prot_resolved
    for protected in "${DATAROOT}" "${COMIN_ATMOS_ANALYSIS:-}" "${COMOUT_ATMOS_ANALYSIS:-}"; do
        if [[ -z "${protected}" ]]; then
            continue
        fi
        if ! prot_resolved=$(realpath -m -s "${protected}" 2>/dev/null); then
            continue
        fi
        if [[ "${resolved}" == "${prot_resolved}" ]]; then
            echo "WARNING: remove_temp_diags: '${resolved}' equals protected path '${prot_resolved}'; skipping cleanup" >&2
            set -x
            return 0
        fi
        if [[ "${prot_resolved}" == "${resolved}/"* ]]; then
            echo "WARNING: remove_temp_diags: '${resolved}' is a parent of protected path '${prot_resolved}'; skipping cleanup" >&2
            set -x
            return 0
        fi
    done

    # Reject a gsidiags symlink whose physical target is outside DATAROOT
    # (Req 5.5): do not delete through a link that escapes the subtree.
    if [[ -L "${temp_diag_dir}" ]]; then
        local physical
        if ! physical=$(realpath "${temp_diag_dir}" 2>/dev/null); then
            echo "WARNING: remove_temp_diags: unable to resolve symlink '${temp_diag_dir}'; skipping cleanup" >&2
            set -x
            return 0
        fi
        if [[ "${physical}" != "${dataroot_resolved}/"* ]]; then
            echo "WARNING: remove_temp_diags: '${temp_diag_dir}' is a symlink pointing outside DATAROOT ('${physical}'); skipping cleanup" >&2
            set -x
            return 0
        fi
    fi

    #-----------------------------------------------------
    # Removal.
    #-----------------------------------------------------

    # Req 1.3, 2.3: nothing to remove -> log INFO and return success.
    if [[ ! -e "${resolved}" && ! -L "${resolved}" ]]; then
        echo "INFO: remove_temp_diags: '${resolved}' does not exist; nothing to remove"
        set -x
        return 0
    fi

    # Req 1.1, 1.2, 2.1, 2.2, 5.1: remove only the resolved gsidiags subtree.
    echo "INFO: remove_temp_diags: removing temporary diagnostic directory '${resolved}'"
    if ! rm -rf "${resolved}"; then
        # Req 1.5, 2.6: on failure emit a warning naming the path and still
        # return success so the job completes normally.
        echo "WARNING: remove_temp_diags: failed to remove '${resolved}'; leaving it in place" >&2
    fi

    set -x
    return 0
}

declare -xf remove_temp_diags
