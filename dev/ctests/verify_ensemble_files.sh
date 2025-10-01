#!/bin/bash
# =============================================================================
# Verification Script for C48_S2SWA_gefs Ensemble Member 001 Test Case
# =============================================================================
# This script validates that all input and output files referenced in
# C48_S2SWA_gefs-gefs_fcst_mem001_seg0.yaml actually exist on disk in the
# nightly stable run.
#
# Usage: ./verify_ensemble_files.sh
# =============================================================================

set -u

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Base directory for the stable run
BASE_DIR="/scratch3/NCEPDEV/global/role.glopara/GFS_CI_CD/HERA/BUILDS/GITLAB/stable/RUNTESTS/COMROOT"
PSLOT="C48_S2SWA_gefs_388b1fe3-4737"
FULL_PATH="${BASE_DIR}/${PSLOT}"

# Test parameters matching the YAML
PDY="20210323"
CYC="12"
CYC_OFFSET="06"  # -6H from cycle 12

echo -e "${BLUE}=========================================================================${NC}"
echo -e "${BLUE}C48_S2SWA_gefs Ensemble Member 001 File Verification${NC}"
echo -e "${BLUE}=========================================================================${NC}"
echo ""
echo -e "Base Directory: ${FULL_PATH}"
echo -e "Test Date: ${PDY} Cycle: ${CYC}Z"
echo -e "Input Cycle (offset -6H): ${CYC_OFFSET}Z"
echo ""

# Counters
TOTAL_INPUT=0
FOUND_INPUT=0
MISSING_INPUT=0
TOTAL_OUTPUT=0
FOUND_OUTPUT=0
MISSING_OUTPUT=0

# =============================================================================
# INPUT FILES SECTION
# =============================================================================
echo -e "${BLUE}=========================================================================${NC}"
echo -e "${BLUE}CHECKING INPUT FILES (from cycle ${CYC_OFFSET}Z)${NC}"
echo -e "${BLUE}=========================================================================${NC}"
echo ""

# Input directory base
INPUT_BASE="${FULL_PATH}/gefs.${PDY}/${CYC_OFFSET}/model"

echo -e "${YELLOW}--- Atmosphere Initial Conditions (13 files) ---${NC}"
ATMOS_INPUT_DIR="${INPUT_BASE}/atmos/input/mem001"
ATMOS_FILES=(
    "gfs_ctrl.nc"
    "gfs_data.tile1.nc"
    "gfs_data.tile2.nc"
    "gfs_data.tile3.nc"
    "gfs_data.tile4.nc"
    "gfs_data.tile5.nc"
    "gfs_data.tile6.nc"
    "sfc_data.tile1.nc"
    "sfc_data.tile2.nc"
    "sfc_data.tile3.nc"
    "sfc_data.tile4.nc"
    "sfc_data.tile5.nc"
    "sfc_data.tile6.nc"
)

for file in "${ATMOS_FILES[@]}"; do
    TOTAL_INPUT=$((TOTAL_INPUT + 1))
    FILEPATH="${ATMOS_INPUT_DIR}/${file}"
    if [[ -f "${FILEPATH}" ]]; then
        echo -e "  ${GREEN}✓${NC} ${file}"
        FOUND_INPUT=$((FOUND_INPUT + 1))
    else
        echo -e "  ${RED}✗${NC} ${file} - MISSING"
        MISSING_INPUT=$((MISSING_INPUT + 1))
    fi
done

echo ""
echo -e "${YELLOW}--- Ice Restart Files (1 file) ---${NC}"
ICE_RESTART_DIR="${INPUT_BASE}/ice/restart/mem001"
ICE_FILE="${PDY}.${CYC}0000.cice_model.res.nc"
TOTAL_INPUT=$((TOTAL_INPUT + 1))
FILEPATH="${ICE_RESTART_DIR}/${ICE_FILE}"
if [[ -f "${FILEPATH}" ]]; then
    echo -e "  ${GREEN}✓${NC} ${ICE_FILE}"
    FOUND_INPUT=$((FOUND_INPUT + 1))
else
    echo -e "  ${RED}✗${NC} ${ICE_FILE} - MISSING"
    MISSING_INPUT=$((MISSING_INPUT + 1))
fi

echo ""
echo -e "${YELLOW}--- Ocean Restart Files (1 file) ---${NC}"
OCEAN_RESTART_DIR="${INPUT_BASE}/ocean/restart/mem001"
OCEAN_FILE="${PDY}.${CYC}0000.MOM.res.nc"
TOTAL_INPUT=$((TOTAL_INPUT + 1))
FILEPATH="${OCEAN_RESTART_DIR}/${OCEAN_FILE}"
if [[ -f "${FILEPATH}" ]]; then
    echo -e "  ${GREEN}✓${NC} ${OCEAN_FILE}"
    FOUND_INPUT=$((FOUND_INPUT + 1))
else
    echo -e "  ${RED}✗${NC} ${OCEAN_FILE} - MISSING"
    MISSING_INPUT=$((MISSING_INPUT + 1))
fi

echo ""
echo -e "${YELLOW}--- Wave Restart Files (1 file) ---${NC}"
WAVE_RESTART_DIR="${INPUT_BASE}/wave/restart/mem001"
WAVE_FILE="${PDY}.${CYC}0000.restart.ww3"
TOTAL_INPUT=$((TOTAL_INPUT + 1))
FILEPATH="${WAVE_RESTART_DIR}/${WAVE_FILE}"
if [[ -f "${FILEPATH}" ]]; then
    echo -e "  ${GREEN}✓${NC} ${WAVE_FILE}"
    FOUND_INPUT=$((FOUND_INPUT + 1))
else
    echo -e "  ${RED}✗${NC} ${WAVE_FILE} - MISSING"
    MISSING_INPUT=$((MISSING_INPUT + 1))
fi

# =============================================================================
# OUTPUT FILES SECTION
# =============================================================================
echo ""
echo -e "${BLUE}=========================================================================${NC}"
echo -e "${BLUE}CHECKING OUTPUT FILES (from cycle ${CYC}Z)${NC}"
echo -e "${BLUE}=========================================================================${NC}"
echo ""

# Output directory base
OUTPUT_BASE="${FULL_PATH}/gefs.${PDY}/${CYC}/model"

echo -e "${YELLOW}--- Atmosphere Forecast History (2 files) ---${NC}"
ATMOS_HISTORY_DIR="${OUTPUT_BASE}/atmos/history/mem001"
ATMOS_OUTPUT_FILES=(
    "atmf006.nc"
    "sfcf006.nc"
)

for file in "${ATMOS_OUTPUT_FILES[@]}"; do
    TOTAL_OUTPUT=$((TOTAL_OUTPUT + 1))
    FILEPATH="${ATMOS_HISTORY_DIR}/${file}"
    if [[ -f "${FILEPATH}" ]]; then
        SIZE=$(stat -c%s "${FILEPATH}" 2>/dev/null || echo "0")
        SIZE_MB=$(echo "scale=2; ${SIZE}/1048576" | bc)
        echo -e "  ${GREEN}✓${NC} ${file} (${SIZE_MB} MB)"
        FOUND_OUTPUT=$((FOUND_OUTPUT + 1))
    else
        echo -e "  ${RED}✗${NC} ${file} - MISSING"
        MISSING_OUTPUT=$((MISSING_OUTPUT + 1))
    fi
done

echo ""
echo -e "${YELLOW}--- Ocean Forecast History (1 file) ---${NC}"
OCEAN_HISTORY_DIR="${OUTPUT_BASE}/ocean/history/mem001"
OCEAN_OUTPUT_FILE="gefs.ocean.t${CYC}z.6hr_avg.f006.nc"
TOTAL_OUTPUT=$((TOTAL_OUTPUT + 1))
FILEPATH="${OCEAN_HISTORY_DIR}/${OCEAN_OUTPUT_FILE}"
if [[ -f "${FILEPATH}" ]]; then
    SIZE=$(stat -c%s "${FILEPATH}" 2>/dev/null || echo "0")
    SIZE_MB=$(echo "scale=2; ${SIZE}/1048576" | bc)
    echo -e "  ${GREEN}✓${NC} ${OCEAN_OUTPUT_FILE} (${SIZE_MB} MB)"
    FOUND_OUTPUT=$((FOUND_OUTPUT + 1))
else
    echo -e "  ${RED}✗${NC} ${OCEAN_OUTPUT_FILE} - MISSING"
    MISSING_OUTPUT=$((MISSING_OUTPUT + 1))
fi

echo ""
echo -e "${YELLOW}--- Ice Forecast History (1 file) ---${NC}"
ICE_HISTORY_DIR="${OUTPUT_BASE}/ice/history/mem001"
ICE_OUTPUT_FILE="gefs.ice.t${CYC}z.6hr_avg.f006.nc"
TOTAL_OUTPUT=$((TOTAL_OUTPUT + 1))
FILEPATH="${ICE_HISTORY_DIR}/${ICE_OUTPUT_FILE}"
if [[ -f "${FILEPATH}" ]]; then
    SIZE=$(stat -c%s "${FILEPATH}" 2>/dev/null || echo "0")
    SIZE_MB=$(echo "scale=2; ${SIZE}/1048576" | bc)
    echo -e "  ${GREEN}✓${NC} ${ICE_OUTPUT_FILE} (${SIZE_MB} MB)"
    FOUND_OUTPUT=$((FOUND_OUTPUT + 1))
else
    echo -e "  ${RED}✗${NC} ${ICE_OUTPUT_FILE} - MISSING"
    MISSING_OUTPUT=$((MISSING_OUTPUT + 1))
fi

echo ""
echo -e "${YELLOW}--- Wave Forecast History (2 files) ---${NC}"
WAVE_HISTORY_DIR="${OUTPUT_BASE}/wave/history/mem001"
WAVE_OUTPUT_FILES=(
    "gefs.wave.t${CYC}z.glo_30m.f006.nc"
    "gefs.wave.t${CYC}z.at_10m.f006.nc"
)

for file in "${WAVE_OUTPUT_FILES[@]}"; do
    TOTAL_OUTPUT=$((TOTAL_OUTPUT + 1))
    FILEPATH="${WAVE_HISTORY_DIR}/${file}"
    if [[ -f "${FILEPATH}" ]]; then
        SIZE=$(stat -c%s "${FILEPATH}" 2>/dev/null || echo "0")
        SIZE_MB=$(echo "scale=2; ${SIZE}/1048576" | bc)
        echo -e "  ${GREEN}✓${NC} ${file} (${SIZE_MB} MB)"
        FOUND_OUTPUT=$((FOUND_OUTPUT + 1))
    else
        echo -e "  ${RED}✗${NC} ${file} - MISSING"
        MISSING_OUTPUT=$((MISSING_OUTPUT + 1))
    fi
done

echo ""
echo -e "${YELLOW}--- Atmosphere Restart Files (2 files) ---${NC}"
ATMOS_RESTART_DIR="${OUTPUT_BASE}/atmos/restart/mem001"
ATMOS_RESTART_FILES=(
    "${PDY}.${CYC}0000.coupler.res"
    "${PDY}.${CYC}0000.fv_core.res.nc"
)

for file in "${ATMOS_RESTART_FILES[@]}"; do
    TOTAL_OUTPUT=$((TOTAL_OUTPUT + 1))
    FILEPATH="${ATMOS_RESTART_DIR}/${file}"
    if [[ -f "${FILEPATH}" ]]; then
        SIZE=$(stat -c%s "${FILEPATH}" 2>/dev/null || echo "0")
        SIZE_KB=$(echo "scale=2; ${SIZE}/1024" | bc)
        echo -e "  ${GREEN}✓${NC} ${file} (${SIZE_KB} KB)"
        FOUND_OUTPUT=$((FOUND_OUTPUT + 1))
    else
        echo -e "  ${RED}✗${NC} ${file} - MISSING"
        MISSING_OUTPUT=$((MISSING_OUTPUT + 1))
    fi
done

echo ""
echo -e "${YELLOW}--- Ocean Restart Files (1 file) ---${NC}"
OCEAN_RESTART_OUTPUT_DIR="${OUTPUT_BASE}/ocean/restart/mem001"
OCEAN_RESTART_FILE="${PDY}.${CYC}0000.MOM.res.nc"
TOTAL_OUTPUT=$((TOTAL_OUTPUT + 1))
FILEPATH="${OCEAN_RESTART_OUTPUT_DIR}/${OCEAN_RESTART_FILE}"
if [[ -f "${FILEPATH}" ]]; then
    SIZE=$(stat -c%s "${FILEPATH}" 2>/dev/null || echo "0")
    SIZE_MB=$(echo "scale=2; ${SIZE}/1048576" | bc)
    echo -e "  ${GREEN}✓${NC} ${OCEAN_RESTART_FILE} (${SIZE_MB} MB)"
    FOUND_OUTPUT=$((FOUND_OUTPUT + 1))
else
    echo -e "  ${RED}✗${NC} ${OCEAN_RESTART_FILE} - MISSING"
    MISSING_OUTPUT=$((MISSING_OUTPUT + 1))
fi

echo ""
echo -e "${YELLOW}--- Ice Restart Files (1 file) ---${NC}"
ICE_RESTART_OUTPUT_DIR="${OUTPUT_BASE}/ice/restart/mem001"
ICE_RESTART_FILE="${PDY}.${CYC}0000.cice_model.res.nc"
TOTAL_OUTPUT=$((TOTAL_OUTPUT + 1))
FILEPATH="${ICE_RESTART_OUTPUT_DIR}/${ICE_RESTART_FILE}"
if [[ -f "${FILEPATH}" ]]; then
    SIZE=$(stat -c%s "${FILEPATH}" 2>/dev/null || echo "0")
    SIZE_MB=$(echo "scale=2; ${SIZE}/1048576" | bc)
    echo -e "  ${GREEN}✓${NC} ${ICE_RESTART_FILE} (${SIZE_MB} MB)"
    FOUND_OUTPUT=$((FOUND_OUTPUT + 1))
else
    echo -e "  ${RED}✗${NC} ${ICE_RESTART_FILE} - MISSING"
    MISSING_OUTPUT=$((MISSING_OUTPUT + 1))
fi

# =============================================================================
# SUMMARY SECTION
# =============================================================================
echo ""
echo -e "${BLUE}=========================================================================${NC}"
echo -e "${BLUE}VERIFICATION SUMMARY${NC}"
echo -e "${BLUE}=========================================================================${NC}"
echo ""

echo -e "${YELLOW}Input Files (cycle ${CYC_OFFSET}Z):${NC}"
echo -e "  Total Expected: ${TOTAL_INPUT}"
echo -e "  Found:          ${GREEN}${FOUND_INPUT}${NC}"
echo -e "  Missing:        ${RED}${MISSING_INPUT}${NC}"
echo ""

echo -e "${YELLOW}Output Files (cycle ${CYC}Z):${NC}"
echo -e "  Total Expected: ${TOTAL_OUTPUT}"
echo -e "  Found:          ${GREEN}${FOUND_OUTPUT}${NC}"
echo -e "  Missing:        ${RED}${MISSING_OUTPUT}${NC}"
echo ""

# Overall status
TOTAL_FILES=$((TOTAL_INPUT + TOTAL_OUTPUT))
TOTAL_FOUND=$((FOUND_INPUT + FOUND_OUTPUT))
TOTAL_MISSING=$((MISSING_INPUT + MISSING_OUTPUT))

echo -e "${YELLOW}Overall Status:${NC}"
echo -e "  Total Files:    ${TOTAL_FILES}"
echo -e "  Found:          ${GREEN}${TOTAL_FOUND}${NC}"
echo -e "  Missing:        ${RED}${TOTAL_MISSING}${NC}"
echo ""

if [[ ${TOTAL_MISSING} -eq 0 ]]; then
    echo -e "${GREEN}=========================================================================${NC}"
    echo -e "${GREEN}✓ SUCCESS: All files exist! YAML file is correct.${NC}"
    echo -e "${GREEN}=========================================================================${NC}"
    exit 0
else
    echo -e "${RED}=========================================================================${NC}"
    echo -e "${RED}✗ FAILURE: ${TOTAL_MISSING} file(s) missing. YAML needs correction.${NC}"
    echo -e "${RED}=========================================================================${NC}"
    exit 1
fi
