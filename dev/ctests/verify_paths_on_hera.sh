#!/bin/bash
#
# HERA Path Verification Script for CTest Fixes
# This script verifies that the directory paths in our test case YAML files
# match the actual directory structure from the nightly CI/CD runs.
#

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Base paths
BASE_PATH="/scratch3/NCEPDEV/global/role.glopara/GFS_CI_CD/HERA/BUILDS/GITLAB/stable/RUNTESTS/COMROOT"
C48_ATM_CASE="C48_ATM_388b1fe3-4737"
TEST_DATE="gfs.20210323/12"

FULL_PATH="${BASE_PATH}/${C48_ATM_CASE}/${TEST_DATE}"

echo "=============================================="
echo "HERA Path Verification for CTest Fixes"
echo "=============================================="
echo ""
echo -e "${BLUE}Base Path:${NC} ${FULL_PATH}"
echo ""

# Function to check if path exists
check_path() {
    local path="$1"
    local description="$2"
    
    if [ -d "$path" ] || [ -f "$path" ]; then
        echo -e "${GREEN}✅ EXISTS:${NC} $description"
        echo -e "   Path: $path"
        return 0
    else
        echo -e "${RED}❌ MISSING:${NC} $description"
        echo -e "   Path: $path"
        return 1
    fi
}

# Function to list directory contents
list_dir() {
    local path="$1"
    local description="$2"
    local max_items="${3:-20}"
    
    echo ""
    echo -e "${YELLOW}📂 Listing:${NC} $description"
    if [ -d "$path" ]; then
        ls -lh "$path" | head -n "$max_items"
    else
        echo -e "${RED}   Directory does not exist${NC}"
    fi
}

# Function to count files matching pattern
count_files() {
    local path="$1"
    local pattern="$2"
    local description="$3"
    
    if [ -d "$path" ]; then
        local count=$(find "$path" -name "$pattern" -type f 2>/dev/null | wc -l)
        echo -e "${BLUE}📊 Count:${NC} $description"
        echo -e "   Pattern: $pattern"
        echo -e "   Count: $count files"
    fi
}

echo "=============================================="
echo "1. TOP-LEVEL STRUCTURE CHECK"
echo "=============================================="
check_path "${FULL_PATH}" "Main test directory"
list_dir "${FULL_PATH}" "Top-level directories"

echo ""
echo "=============================================="
echo "2. MODEL DIRECTORY CHECK (Input Files)"
echo "=============================================="
check_path "${FULL_PATH}/model" "model/ directory"
check_path "${FULL_PATH}/model/atmos" "model/atmos/ directory"
check_path "${FULL_PATH}/model/atmos/master" "model/atmos/master/ directory"
list_dir "${FULL_PATH}/model/atmos/master" "Master GRIB2 files" 15

echo ""
echo "=============================================="
echo "3. PRODUCTS DIRECTORY CHECK (Output Files)"
echo "=============================================="
check_path "${FULL_PATH}/products" "products/ directory (NEW CORRECT PATH)"
check_path "${FULL_PATH}/products/atmos" "products/atmos/ directory"
check_path "${FULL_PATH}/products/atmos/grib2" "products/atmos/grib2/ directory"

# Check all three grid resolutions
for grid in 0p25 0p50 1p00; do
    echo ""
    check_path "${FULL_PATH}/products/atmos/grib2/${grid}" "products/atmos/grib2/${grid}/ directory"
    count_files "${FULL_PATH}/products/atmos/grib2/${grid}" "*.pgrb2.*" "pgrb2 files in ${grid}"
    count_files "${FULL_PATH}/products/atmos/grib2/${grid}" "*.idx" "index files in ${grid}"
    count_files "${FULL_PATH}/products/atmos/grib2/${grid}" "*.flux.*" "flux files in ${grid}"
done

echo ""
echo "=============================================="
echo "4. OLD PATH CHECK (Should NOT Exist)"
echo "=============================================="
if [ -d "${FULL_PATH}/atmos" ]; then
    echo -e "${RED}⚠️  WARNING:${NC} Old 'atmos/' directory exists (without products/ prefix)"
    check_path "${FULL_PATH}/atmos/grib2" "atmos/grib2/ (OLD INCORRECT PATH)"
else
    echo -e "${GREEN}✅ CORRECT:${NC} Old 'atmos/' path does not exist"
    echo -e "   This confirms our fix is correct!"
fi

echo ""
echo "=============================================="
echo "5. SPECIFIC FILE VERIFICATION"
echo "=============================================="
echo "Checking for specific files expected by test case..."
echo ""

# Files that should be in model/atmos/master (inputs)
echo -e "${YELLOW}Input Files (from model/atmos/master):${NC}"
check_path "${FULL_PATH}/model/atmos/master/gfs.t12z.master.grb2f000" "master.grb2f000"
check_path "${FULL_PATH}/model/atmos/master/gfs.t12z.master.grb2f003" "master.grb2f003"
check_path "${FULL_PATH}/model/atmos/master/gfs.t12z.sfluxgrbf000.grib2" "sfluxgrbf000.grib2"
check_path "${FULL_PATH}/model/atmos/master/gfs.t12z.sfluxgrbf003.grib2" "sfluxgrbf003.grib2"

echo ""
echo -e "${YELLOW}Output Files (from products/atmos/grib2/0p25):${NC}"
check_path "${FULL_PATH}/products/atmos/grib2/0p25/gfs.t12z.pgrb2.0p25.f000" "pgrb2.0p25.f000"
check_path "${FULL_PATH}/products/atmos/grib2/0p25/gfs.t12z.pgrb2.0p25.f000.idx" "pgrb2.0p25.f000.idx"
check_path "${FULL_PATH}/products/atmos/grib2/0p25/gfs.t12z.flux.0p25.f000" "flux.0p25.f000"

echo ""
echo "=============================================="
echo "6. FIND ALL PGRB2 FILES"
echo "=============================================="
echo "Searching for all pgrb2 files in the test directory..."
echo ""
find "${FULL_PATH}" -name "*pgrb2*" -type f 2>/dev/null | head -20

echo ""
echo "=============================================="
echo "7. DIRECTORY TREE (Limited Depth)"
echo "=============================================="
if command -v tree &> /dev/null; then
    echo "Full directory tree (up to 4 levels):"
    tree -L 4 -d "${FULL_PATH}" 2>&1 | head -60
else
    echo "Tree command not available, using find instead:"
    find "${FULL_PATH}" -maxdepth 4 -type d 2>&1 | head -60
fi

echo ""
echo "=============================================="
echo "SUMMARY"
echo "=============================================="
echo ""
echo -e "${BLUE}Path Fix Verification:${NC}"
echo ""

# Check critical paths
PRODUCTS_EXISTS=false
ATMOS_OLD_EXISTS=false

[ -d "${FULL_PATH}/products/atmos/grib2" ] && PRODUCTS_EXISTS=true
[ -d "${FULL_PATH}/atmos/grib2" ] && ATMOS_OLD_EXISTS=true

if [ "$PRODUCTS_EXISTS" = true ] && [ "$ATMOS_OLD_EXISTS" = false ]; then
    echo -e "${GREEN}✅ FIX IS CORRECT!${NC}"
    echo -e "   - Products are in ${GREEN}products/atmos/grib2/${NC}"
    echo -e "   - Old path ${GREEN}atmos/grib2/${NC} does not exist"
    echo -e "   - Our YAML file updates are ${GREEN}CORRECT${NC}"
elif [ "$PRODUCTS_EXISTS" = false ] && [ "$ATMOS_OLD_EXISTS" = true ]; then
    echo -e "${RED}❌ FIX IS WRONG!${NC}"
    echo -e "   - Products are in ${RED}atmos/grib2/${NC} (without products/)"
    echo -e "   - Need to ${RED}REVERT${NC} our changes"
elif [ "$PRODUCTS_EXISTS" = true ] && [ "$ATMOS_OLD_EXISTS" = true ]; then
    echo -e "${YELLOW}⚠️  BOTH PATHS EXIST!${NC}"
    echo -e "   - Need to investigate which one is correct"
else
    echo -e "${RED}❌ NEITHER PATH EXISTS!${NC}"
    echo -e "   - Files may be in a different location"
    echo -e "   - Need to investigate further"
fi

echo ""
echo "=============================================="
echo "Script completed!"
echo "=============================================="
