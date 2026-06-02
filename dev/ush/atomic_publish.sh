#! /usr/bin/env bash

###############################################################################
# atomic_publish.sh
#
# Atomic publish utility for the global-workflow.
# Implements the stage-verify-move pattern to ensure downstream consumers
# never observe partial or corrupted products in ${COMOUT}.
#
# USAGE:
#   atomic_publish <file1> [file2] [file3] ...
#
# ENVIRONMENT VARIABLES (required):
#   COMOUT  - Final output directory for products
#   jobid   - Unique job identifier for staging isolation
#
# ENVIRONMENT VARIABLES (optional):
#   SENDDBN      - Set to "YES" to send dbn_alert after publish (default: NO)
#   DBN_ALERT_TYPE - Alert type for dbn_alert (default: empty)
#   job          - Job name for dbn_alert (default: empty)
#   DBNROOT      - Root path for dbn_alert binary (default: empty)
#   ATOMIC_PUBLISH_HASH_CHECK - Set to "YES" to enable SHA-256 hash verification
#
# DESCRIPTION:
#   1. Stages all specified files to ${COMOUT}/.staging/${jobid}/
#   2. Verifies all staged files are non-empty (and hash-checks if enabled)
#   3. Atomically moves each file to its final ${COMOUT} location
#   4. Sends dbn_alert only after file is at final location (if SENDDBN=YES)
#   5. On any verification failure, calls err_exit and leaves COMOUT unchanged
#
# Uses cpfs for inter-filesystem copies per EE2 standards.
#
# Traces to: Requirement 7 (Atomic Delivery)
###############################################################################

# Ensure required variables are set
if [[ -z "${COMOUT:-}" ]]; then
    echo "FATAL ERROR: COMOUT is not set"
    export err=1
    err_exit "atomic_publish: COMOUT is not set"
fi

if [[ -z "${jobid:-}" ]]; then
    echo "FATAL ERROR: jobid is not set"
    export err=1
    err_exit "atomic_publish: jobid is not set"
fi

if [[ $# -eq 0 ]]; then
    echo "FATAL ERROR: No files specified for atomic publish"
    export err=1
    err_exit "atomic_publish: No files specified for publish"
fi

###############################################################################
# atomic_stage - Stage files to the hidden staging directory
#
# Uses cpfs for inter-filesystem copies per EE2 conventions.
#
# Arguments:
#   $@ - List of source file paths to stage
#
# Returns:
#   0 on success, non-zero on failure
###############################################################################
atomic_stage() {
    local staging_dir="${COMOUT}/.staging/${jobid}"

    # Create staging directory
    mkdir -p "${staging_dir}"
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "atomic_publish: Failed to create staging directory ${staging_dir}"
    fi

    # Stage each file using cpfs (EE2 inter-filesystem copy)
    local src
    for src in "$@"; do
        if [[ ! -f "${src}" ]]; then
            export err=1
            err_exit "atomic_publish: Source file does not exist: ${src}"
        fi

        local basename
        basename=$(basename "${src}")
        cpfs "${src}" "${staging_dir}/${basename}"
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "atomic_publish: cpfs failed to stage ${src} to ${staging_dir}/${basename}"
        fi
    done

    return 0
}

###############################################################################
# atomic_verify - Verify all staged files are non-empty and optionally
#                 hash-check against source files
#
# Arguments:
#   $@ - List of source file paths (used for hash comparison)
#
# Returns:
#   0 on success, non-zero on failure (calls err_exit on failure)
###############################################################################
atomic_verify() {
    local staging_dir="${COMOUT}/.staging/${jobid}"
    local hash_check="${ATOMIC_PUBLISH_HASH_CHECK:-NO}"

    local src
    for src in "$@"; do
        local basename
        basename=$(basename "${src}")
        local staged_file="${staging_dir}/${basename}"

        # Verify file exists
        if [[ ! -f "${staged_file}" ]]; then
            export err=1
            err_exit "atomic_publish: Staged file missing: ${staged_file}"
        fi

        # Verify file is non-empty
        if [[ ! -s "${staged_file}" ]]; then
            export err=1
            err_exit "atomic_publish: Staged file is empty: ${staged_file}"
        fi

        # Optional SHA-256 hash verification
        if [[ "${hash_check^^}" == "YES" ]]; then
            local src_hash staged_hash
            src_hash=$(sha256sum "${src}" | awk '{print $1}')
            staged_hash=$(sha256sum "${staged_file}" | awk '{print $1}')
            if [[ "${src_hash}" != "${staged_hash}" ]]; then
                export err=1
                err_exit "atomic_publish: Hash mismatch for ${basename} (source: ${src_hash}, staged: ${staged_hash})"
            fi
        fi
    done

    return 0
}

###############################################################################
# atomic_move - Atomically move staged files to final COMOUT location
#
# Uses mv within the same filesystem for atomicity.
#
# Arguments:
#   $@ - List of source file paths (basenames used for final names)
#
# Returns:
#   0 on success, non-zero on failure
###############################################################################
atomic_move() {
    local staging_dir="${COMOUT}/.staging/${jobid}"

    local src
    for src in "$@"; do
        local basename
        basename=$(basename "${src}")
        local staged_file="${staging_dir}/${basename}"
        local final_file="${COMOUT}/${basename}"

        mv "${staged_file}" "${final_file}"
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "atomic_publish: Failed to move ${staged_file} to ${final_file}"
        fi
    done

    # Clean up the staging directory
    rmdir "${staging_dir}" 2>/dev/null || true

    return 0
}

###############################################################################
# atomic_alert - Send dbn_alert for published files
#
# Only sends alerts when SENDDBN is set to YES (case-insensitive).
# Alerts are sent only after files are at their final COMOUT location.
#
# Arguments:
#   $@ - List of source file paths (basenames used for final names)
###############################################################################
atomic_alert() {
    if [[ "${SENDDBN^^}" != "YES" ]]; then
        return 0
    fi

    local alert_type="${DBN_ALERT_TYPE:-}"
    local job_name="${job:-}"
    local dbn_root="${DBNROOT:-}"

    if [[ -z "${dbn_root}" || -z "${alert_type}" ]]; then
        return 0
    fi

    local src
    for src in "$@"; do
        local basename
        basename=$(basename "${src}")
        local final_file="${COMOUT}/${basename}"

        # Only alert if file is confirmed at final location
        if [[ -f "${final_file}" ]]; then
            "${dbn_root}/bin/dbn_alert" MODEL "${alert_type}" "${job_name}" "${final_file}"
        fi
    done

    return 0
}

###############################################################################
# atomic_publish - Main entry point: stage, verify, move, alert
#
# Orchestrates the full atomic publish workflow:
#   1. Stage all files to .staging/${jobid}/
#   2. Verify all staged files (non-empty + optional hash)
#   3. Atomically move to final location
#   4. Send dbn_alert (if SENDDBN=YES)
#
# On any failure during staging or verification, COMOUT remains unchanged.
#
# Arguments:
#   $@ - List of source file paths to publish
###############################################################################
atomic_publish() {
    local files=("$@")

    # Stage all files
    atomic_stage "${files[@]}"

    # Verify all staged files
    atomic_verify "${files[@]}"

    # Move to final location (atomic within same filesystem)
    atomic_move "${files[@]}"

    # Send alerts only after files are at final location
    atomic_alert "${files[@]}"

    return 0
}

# Execute if called with arguments (not just sourced)
if [[ $# -gt 0 ]]; then
    atomic_publish "$@"
fi
