#!/usr/bin/env bash
# shellcheck disable=SC2317  # Don't warn about unreachable commands in this file

set -eu

#####################################################################################
# Script description: Driver script to run run_check_gitlab_ci.sh on all experiment
#                     directories that contain .xml files
#
# DISCLAIMER: This script is designed to emulate the load behavior of running
#             multiple experiments in a CI/CD pipeline for performance evaluation
#             purposes. It simulates realistic workloads to assess system
#             performance under concurrent experiment execution scenarios.
#####################################################################################

usage() {
    cat << EOF
Usage: ${0} TEST_DIR SYSTEM_BUILD_DIR

Arguments:
    TEST_DIR         Directory containing RUNTESTS/EXPDIR with experiment subdirectories
    SYSTEM_BUILD_DIR Name of the system build directory (e.g., "global-workflow")

Description:
    This script searches for experiment directories in TEST_DIR/RUNTESTS/EXPDIR/
    that contain *.xml files, then runs run_check_gitlab_ci.sh on each experiment 
    in parallel using the directory basename as the pslot name.
    
    Note: Database (.db) files are not required as they will be created by rocotorun
    during the first execution of the run_check_gitlab_ci.sh script.

Example:
    ${0} /path/to/test/dir global-workflow
    
    # From the global-workflow repository root:
    ./dev/ci/scripts/utils/run_all_experiments.sh /path/to/test/dir global-workflow

Directory Structure Expected:
    TEST_DIR/
    ├── SYSTEM_BUILD_DIR/         # Contains global-workflow code
    └── RUNTESTS/
        └── EXPDIR/
            ├── C48_ATM/          # Experiment directory (pslot=C48_ATM)
            │   └── C48_ATM.xml   # XML file required
            ├── C96_COUPLED/      # Experiment directory (pslot=C96_COUPLED)  
            │   └── C96_COUPLED.xml
            └── ...
EOF
}

# Check arguments
if [[ ${#} -ne 2 ]]; then
    echo "ERROR: Invalid number of arguments"
    usage
    exit 1
fi

TEST_DIR="${1}"
SYSTEM_BUILD_DIR="${2}"

# Validate TEST_DIR exists
if [[ ! -d "${TEST_DIR}" ]]; then
    echo "ERROR: TEST_DIR '${TEST_DIR}' does not exist"
    exit 1
fi

# Validate SYSTEM_BUILD_DIR exists within TEST_DIR
if [[ ! -d "${TEST_DIR}/${SYSTEM_BUILD_DIR}" ]]; then
    echo "ERROR: SYSTEM_BUILD_DIR '${TEST_DIR}/${SYSTEM_BUILD_DIR}' does not exist"
    exit 1
fi

# Validate RUNTESTS/EXPDIR structure
RUNTESTS="${TEST_DIR}/RUNTESTS"
EXPDIR="${RUNTESTS}/EXPDIR"
if [[ ! -d "${EXPDIR}" ]]; then
    echo "WARNING: Expected experiment directory '${EXPDIR}' does not exist"
    echo "Attempting to generate workflows using generate_workflow.sh..."

    # Locate generate_workflow.sh script
    GENERATE_WORKFLOW_SCRIPT="${TEST_DIR}/${SYSTEM_BUILD_DIR}/dev/workflow/generate_workflow.sh"

    if [[ ! -f "${GENERATE_WORKFLOW_SCRIPT}" ]]; then
        echo "ERROR: generate_workflow.sh not found at '${GENERATE_WORKFLOW_SCRIPT}'"
        exit 1
    fi

    # Run generate_workflow.sh with required flags
    echo "Running: ${GENERATE_WORKFLOW_SCRIPT} -GESC ${RUNTESTS}"
    # Save current directory and change to script directory
    ORIGINAL_DIR="$(pwd)"
    SCRIPT_DIR="$(dirname "${GENERATE_WORKFLOW_SCRIPT}")"
    cd "${SCRIPT_DIR}"
    if ! ./generate_workflow.sh -GESC "${RUNTESTS}"; then
        echo "ERROR: Failed to generate workflows"
        exit 1
    fi
    # Return to original directory
    cd "${ORIGINAL_DIR}"

    # Verify EXPDIR was created
    if [[ ! -d "${EXPDIR}" ]]; then
        echo "ERROR: EXPDIR '${EXPDIR}' was not created by generate_workflow.sh"
        exit 1
    fi
    echo "Successfully generated workflows in '${EXPDIR}'"
fi

# Script location
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUN_CHECK_SCRIPT="${SCRIPT_DIR}/../run_check_gitlab_ci.sh"

# Validate run_check_gitlab_ci.sh exists
if [[ ! -f "${RUN_CHECK_SCRIPT}" ]]; then
    echo "ERROR: run_check_gitlab_ci.sh not found at '${RUN_CHECK_SCRIPT}'"
    exit 1
fi

echo "================================================================================"
echo "Starting parallel experiment runs"
echo "TEST_DIR: ${TEST_DIR}"
echo "SYSTEM_BUILD_DIR: ${SYSTEM_BUILD_DIR}"
echo "EXPDIR: ${EXPDIR}"
echo "================================================================================"

# Array to store background process PIDs
declare -a pids=()
declare -a experiments=()

# Find all experiment directories with .xml files
while IFS= read -r -d '' experiment_dir; do
    pslot="$(basename "${experiment_dir}")"
    
    # Check if directory contains .xml files
    xml_files=("${experiment_dir}"/*.xml)
    
    # Check if XML files actually exist (not just glob patterns)
    if [[ -f "${xml_files[0]}" ]]; then
        echo "Found valid experiment: ${pslot}"
        echo "  Directory: ${experiment_dir}"
        xml_files_list=$(find "${experiment_dir}" -maxdepth 1 -name "*.xml")
        xml_count=$(echo "${xml_files_list}" | wc -l)
        echo "  XML files: ${xml_count}"
        
        # Run the check script in background
        echo "Starting experiment ${pslot}..."
        "${RUN_CHECK_SCRIPT}" "${TEST_DIR}" "${pslot}" "${SYSTEM_BUILD_DIR}" &
        
        # Store PID and experiment name
        pids+=($!)
        experiments+=("${pslot}")
        
        echo "  PID: ${!} for experiment: ${pslot}"
        echo ""
    else
        echo "Skipping ${pslot}: No .xml files found"
    fi
done < <(find "${EXPDIR}" -mindepth 1 -maxdepth 1 -type d -print0 || true)

# Check if any experiments were found
if [[ ${#pids[@]} -eq 0 ]]; then
    echo "ERROR: No valid experiment directories found in ${EXPDIR}"
    echo "Looking for directories containing *.xml files"
    exit 1
fi

echo "================================================================================"
echo "Started ${#pids[@]} experiments in parallel:"
for i in "${!experiments[@]}"; do
    echo "  ${experiments[i]} (PID: ${pids[i]})"
done
echo "================================================================================"

# Function to cleanup background processes on script exit
# shellcheck disable=SC2317  # Don't warn about unreachable commands in cleanup function
cleanup() {
    echo ""
    echo "Cleaning up background processes..."
    for pid in "${pids[@]}"; do
        if kill -0 "${pid}" 2>/dev/null; then
            echo "Terminating PID: ${pid}"
            kill -TERM "${pid}" 2>/dev/null || true
        fi
    done
    wait
}

# Set trap for cleanup on script exit
trap cleanup EXIT INT TERM

# Monitor all background processes
echo "Monitoring experiment progress..."
echo "Use Ctrl+C to stop all experiments"
echo ""

# Arrays to track completion status
declare -a completed=()
declare -a failed=()
declare -a running=()

# Initialize all as running
for exp in "${experiments[@]}"; do
    running+=("${exp}")
done

# Monitor loop
while [[ ${#running[@]} -gt 0 ]]; do
    # Check each running process
    for i in "${!pids[@]}"; do
        pid="${pids[i]}"
        exp="${experiments[i]}"
        
        # Skip if already completed or failed
        if [[ ! "${running[*]}" =~ ${exp} ]]; then
            continue
        fi
        
        # Check if process is still running
        if ! kill -0 "${pid}" 2>/dev/null; then
            # Process has finished, get exit status
            wait "${pid}"
            exit_status=$?
            
            # Remove from running array
            running=("${running[@]/${exp}}")
            
            if [[ ${exit_status} -eq 0 ]]; then
                completed+=("${exp}")
                echo "[PASS] COMPLETED: ${exp} (PID: ${pid})"
            else
                failed+=("${exp}")
                echo "[FAIL] FAILED: ${exp} (PID: ${pid}) - Exit code: ${exit_status}"
            fi
        fi
    done
    
    # Show progress
    total=${#experiments[@]}
    completed_count=${#completed[@]}
    failed_count=${#failed[@]}
    running_count=${#running[@]}
    done_count=$((completed_count + failed_count))
    echo "Progress: ${done_count}/${total} experiments finished (${completed_count} completed, ${failed_count} failed, ${running_count} running)"
    
    # Wait before next check
    sleep 30
done

echo ""
echo "================================================================================"
echo "ALL EXPERIMENTS FINISHED"
echo "================================================================================"

# Final summary
echo "===== FINAL SUMMARY ====="
echo "  Total experiments: ${#experiments[@]}"
echo "  Completed successfully: ${#completed[@]}"
echo "  Failed: ${#failed[@]}"
echo ""

if [[ ${#completed[@]} -gt 0 ]]; then
    echo "[PASS] SUCCESSFUL EXPERIMENTS:"
    for exp in "${completed[@]}"; do
        echo "  - ${exp}"
    done
    echo ""
fi

if [[ ${#failed[@]} -gt 0 ]]; then
    echo "[FAIL] FAILED EXPERIMENTS:"
    for exp in "${failed[@]}"; do
        echo "  - ${exp}"
    done
    echo ""
fi

# Exit with failure if any experiments failed
if [[ ${#failed[@]} -gt 0 ]]; then
    echo "Some experiments failed. Check individual logs for details."
    exit 1
else
    echo "All experiments completed successfully! [SUCCESS]"
    exit 0
fi
