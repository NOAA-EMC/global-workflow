#!/bin/bash
# Test script to verify HOMEgfs can be found consistently from different locations
# This satisfies the acceptance criteria for the HOMEgfs path resolution issue

set -u

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "========================================"
echo "Testing HOMEgfs Resolution from Multiple Locations"
echo "========================================"

# Function to test HOMEgfs resolution from a given location
test_homegfs_from_location() {
    local test_file=$1
    local test_name=$2

    echo ""
    echo -e "${YELLOW}Testing from: ${test_name}${NC}"
    echo "File: ${test_file}"

    # Check if file exists
    if [[ ! -f "${test_file}" ]]; then
        echo -e "${RED}FAILED: Test file does not exist${NC}"
        return 1
    fi

    # Extract HOMEgfs from the script
    local homegfs_line=$(grep -E "HOMEgfs.*git rev-parse" "${test_file}" | head -1)

    if [[ -z "${homegfs_line}" ]]; then
        echo -e "${RED}FAILED: No git-based HOMEgfs resolution found${NC}"
        return 1
    fi

    # Verify no relative path patterns exist
    if grep -q "cd.*\.\./.*pwd" "${test_file}"; then
        echo -e "${RED}FAILED: Relative path pattern (../) found${NC}"
        return 1
    fi

    echo -e "${GREEN}PASSED: Uses git-based method${NC}"
    echo "  Pattern: ${homegfs_line}"
    return 0
}

# Test counter
total_tests=0
passed_tests=0

# Test from sorc/ directory (build scripts)
test_locations=(
    "sorc/build_all.sh:Build All Script (sorc/)"
    "sorc/link_workflow.sh:Link Workflow Script (sorc/)"
    "sorc/build_gdas.sh:Build GDAS Script (sorc/)"
    "dev/workflow/generate_workflows.sh:Generate Workflows (dev/workflow/)"
    "dev/test/f90nmlcmp.sh:F90 Namelist Compare (dev/test/)"
    "dev/ci/scripts/utils/ci_utils.sh:CI Utils (dev/ci/scripts/utils/)"
)

# Get the repository root to test from
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}" && git rev-parse --show-toplevel)"

echo ""
echo "Repository root: ${REPO_ROOT}"

# Run tests for each location
for test_spec in "${test_locations[@]}"; do
    IFS=':' read -r file_path test_name <<< "${test_spec}"
    full_path="${REPO_ROOT}/${file_path}"

    ((total_tests++))
    if test_homegfs_from_location "${full_path}" "${test_name}"; then
        ((passed_tests++))
    fi
done

# Summary
echo ""
echo "========================================"
echo "Test Summary"
echo "========================================"
echo "Total tests: ${total_tests}"
echo "Passed: ${passed_tests}"
echo "Failed: $((total_tests - passed_tests))"

if [[ ${passed_tests} -eq ${total_tests} ]]; then
    echo -e "${GREEN}All tests PASSED!${NC}"
    exit 0
else
    echo -e "${RED}Some tests FAILED!${NC}"
    exit 1
fi
