#! /usr/bin/env bash

#---------------------------------------------------------
# test_remove_temp_diags.sh
#
# Standalone, self-contained unit test for the ush/remove_temp_diags.sh
# helper. It sources the helper and exercises it against mktemp -d fixtures
# (a populated `gsidiags` directory plus a sibling "COM" directory containing
# diagnostic tarballs). No cluster resources are required.
#
# Coverage (see .kiro/specs/feature-gfsv17-gsidiag):
#   - Retention decision table: KEEP_TEMP_DIAGS in {YES, NO, unset, "maybe"}
#     x KEEPDATA in {YES, NO, unset}          (Req 3.1-3.5, 4.1, 4.2, 4.5, 4.6)
#   - Edge cases: missing dir, empty/unset path arg   (Req 1.3, 2.3, 5.3)
#   - Guard cases: path == DATAROOT, path parent of DATAROOT/COM, symlink out
#     of DATAROOT                                       (Req 5.1, 5.4, 5.5)
#   - Side effect: remove deletes gsidiags, COM tarballs remain
#                                             (Req 1.1, 1.2, 2.1, 2.2, 5.2)
#   - Failure tolerance: read-only parent -> warning, returns success
#                                                          (Req 1.5, 2.6)
#
# Usage:
#   ./test_remove_temp_diags.sh
#
# Exit status: 0 if every case passes, 1 otherwise.
#---------------------------------------------------------

# Deliberately NOT using `set -e` in the harness: assertions manage control
# flow explicitly. The helper itself is invoked under `set -eu -o pipefail`
# (see run_helper) to prove it is strict-mode safe.

#-----------------------------------------------------
# Locate and source the helper under test.
#-----------------------------------------------------
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${HERE}/../../../.." && pwd)"
HELPER="${REPO_ROOT}/ush/remove_temp_diags.sh"

if [[ ! -f "${HELPER}" ]]; then
    echo "FATAL: cannot find helper at '${HELPER}'" >&2
    exit 1
fi
# shellcheck source=/dev/null
source "${HELPER}"

#-----------------------------------------------------
# Tiny assertion framework.
#-----------------------------------------------------
TESTS_RUN=0
TESTS_FAILED=0
CURRENT=""       # description of the case currently running
_OUT=""          # captured stdout+stderr of the last helper invocation
_RC=0            # captured return code of the last helper invocation

start_case() { CURRENT="$1"; }

pass() {
    TESTS_RUN=$((TESTS_RUN + 1))
    printf '  PASS: %s\n' "$1"
}

fail() {
    TESTS_RUN=$((TESTS_RUN + 1))
    TESTS_FAILED=$((TESTS_FAILED + 1))
    printf '  FAIL: %s\n' "$1"
    if [[ -n "${_OUT}" ]]; then
        printf '        --- helper output (rc=%s) ---\n' "${_RC}"
        printf '        %s\n' "${_OUT}" | sed 's/^/        /'
    fi
}

assert_rc() {            # assert_rc <expected> <msg>
    if [[ "${_RC}" -eq "$1" ]]; then
        pass "${CURRENT}: ${2} (rc=${_RC})"
    else
        fail "${CURRENT}: ${2} (expected rc=$1, got rc=${_RC})"
    fi
}

assert_exists() {        # assert_exists <path> <msg>
    if [[ -e "$1" || -L "$1" ]]; then
        pass "${CURRENT}: ${2}"
    else
        fail "${CURRENT}: ${2} ('$1' is missing)"
    fi
}

assert_missing() {       # assert_missing <path> <msg>
    if [[ ! -e "$1" && ! -L "$1" ]]; then
        pass "${CURRENT}: ${2}"
    else
        fail "${CURRENT}: ${2} ('$1' still exists)"
    fi
}

assert_contains() {      # assert_contains <needle> <msg>
    if [[ "${_OUT}" == *"$1"* ]]; then
        pass "${CURRENT}: ${2}"
    else
        fail "${CURRENT}: ${2} (output missing '$1')"
    fi
}

assert_not_contains() {  # assert_not_contains <needle> <msg>
    if [[ "${_OUT}" != *"$1"* ]]; then
        pass "${CURRENT}: ${2}"
    else
        fail "${CURRENT}: ${2} (output unexpectedly contained '$1')"
    fi
}

#-----------------------------------------------------
# Invoke the helper under strict mode, capturing output and rc.
# All KEEPDATA / KEEP_TEMP_DIAGS / DATAROOT / COM* variables are read from
# the current shell (command substitution inherits them).
#-----------------------------------------------------
run_helper() {           # run_helper <arg...>
    _OUT="$(set -eu -o pipefail; remove_temp_diags "$@" 2>&1)"
    _RC=$?
}

run_helper_noarg() {
    _OUT="$(set -eu -o pipefail; remove_temp_diags 2>&1)"
    _RC=$?
}

reset_env() {
    unset KEEPDATA KEEP_TEMP_DIAGS DATAROOT \
          COMIN_ATMOS_ANALYSIS COMOUT_ATMOS_ANALYSIS 2>/dev/null || true
}

#-----------------------------------------------------
# Fixture helpers.
#-----------------------------------------------------
# Create a DATAROOT with a populated gsidiags dir and a sibling COM dir
# holding the four diagnostic tarballs. Sets globals: DATAROOT, GSIDIAGS,
# COMOUT_ATMOS_ANALYSIS, COMIN_ATMOS_ANALYSIS.
setup_fixture() {
    DATAROOT="$(mktemp -d)"
    local cycdir="${DATAROOT}/gdas.20240101/00/analysis/atmos"
    GSIDIAGS="${cycdir}/gsidiags"
    mkdir -p "${GSIDIAGS}/subdir"
    echo "conv ges" > "${GSIDIAGS}/diag_conv_ges.nc"
    echo "conv anl" > "${GSIDIAGS}/subdir/diag_conv_anl.nc"

    COMOUT_ATMOS_ANALYSIS="${cycdir}/com"
    mkdir -p "${COMOUT_ATMOS_ANALYSIS}"
    local t
    for t in cnvstat oznstat radstat pcpstat; do
        echo "tarball ${t}" > "${COMOUT_ATMOS_ANALYSIS}/gdas.t00z.${t}"
    done
    COMIN_ATMOS_ANALYSIS="${COMOUT_ATMOS_ANALYSIS}"
}

cleanup_fixture() {
    # Restore any permissions we may have removed so cleanup can proceed.
    [[ -n "${DATAROOT:-}" && -d "${DATAROOT}" ]] && chmod -R u+rwX "${DATAROOT}" 2>/dev/null
    [[ -n "${DATAROOT:-}" ]] && rm -rf "${DATAROOT}" 2>/dev/null
    true
}

#=====================================================
# 1. Retention decision table
#    KEEP_TEMP_DIAGS in {YES, NO, UNSET, maybe} x KEEPDATA in {YES, NO, UNSET}
#=====================================================
echo "== Retention decision table =="

# retention_case <ktd_state> <keepdata_state> <expected: retain|remove>
retention_case() {
    local ktd_state="$1" kd_state="$2" expected="$3"
    reset_env
    setup_fixture

    if [[ "${ktd_state}" == "UNSET" ]]; then
        unset KEEP_TEMP_DIAGS
    else
        KEEP_TEMP_DIAGS="${ktd_state}"
    fi
    if [[ "${kd_state}" == "UNSET" ]]; then
        unset KEEPDATA
    else
        KEEPDATA="${kd_state}"
    fi

    start_case "KEEP_TEMP_DIAGS=${ktd_state}, KEEPDATA=${kd_state} -> ${expected}"
    run_helper "${GSIDIAGS}"

    assert_rc 0 "returns success"
    if [[ "${expected}" == "retain" ]]; then
        assert_exists "${GSIDIAGS}" "gsidiags retained"
        assert_contains "retaining" "logs retention"
    else
        assert_missing "${GSIDIAGS}" "gsidiags removed"
        # COM tarballs must always survive.
        assert_exists "${COMOUT_ATMOS_ANALYSIS}/gdas.t00z.cnvstat" "COM cnvstat preserved"
    fi

    # "maybe" (invalid) must additionally emit an error indication (Req 4.6).
    if [[ "${ktd_state}" == "maybe" ]]; then
        assert_contains "ERROR" "invalid value emits error indication"
        assert_contains "maybe" "error names the unrecognized value"
    fi

    cleanup_fixture
}

# KEEP_TEMP_DIAGS=YES -> retain regardless of KEEPDATA (Req 3.4, 4.1)
retention_case "YES"   "YES"   "retain"
retention_case "YES"   "NO"    "retain"
retention_case "YES"   "UNSET" "retain"

# KEEP_TEMP_DIAGS=NO -> remove regardless of KEEPDATA (Req 3.5, 4.2)
retention_case "NO"    "YES"   "remove"
retention_case "NO"    "NO"    "remove"
retention_case "NO"    "UNSET" "remove"

# KEEP_TEMP_DIAGS unset -> follow KEEPDATA (Req 3.1, 3.2, 3.3, 4.5)
retention_case "UNSET" "YES"   "retain"
retention_case "UNSET" "NO"    "remove"
retention_case "UNSET" "UNSET" "remove"

# KEEP_TEMP_DIAGS invalid -> remove + error indication (Req 4.6)
retention_case "maybe" "YES"   "remove"
retention_case "maybe" "NO"    "remove"
retention_case "maybe" "UNSET" "remove"

#=====================================================
# 2. Edge cases (Req 1.3, 2.3, 5.3)
#=====================================================
echo "== Edge cases =="

# Missing Temp_Diag_Dir -> success, no error (Req 1.3, 2.3)
reset_env
setup_fixture
KEEPDATA="NO"
rm -rf "${GSIDIAGS}"          # remove it so the helper finds nothing to do
start_case "missing Temp_Diag_Dir"
run_helper "${GSIDIAGS}"
assert_rc 0 "returns success"
assert_contains "does not exist" "logs that there is nothing to remove"
assert_not_contains "ERROR" "no error emitted"
assert_not_contains "WARNING" "no warning emitted"
cleanup_fixture

# Empty path argument -> skip with message, success (Req 5.3)
reset_env
KEEPDATA="NO"
start_case "empty path argument"
run_helper ""
assert_rc 0 "returns success"
assert_contains "no temporary diagnostic directory specified" "logs skip message"

# Missing path argument (no arg at all) -> skip with message, success (Req 5.3)
reset_env
KEEPDATA="NO"
start_case "no path argument"
run_helper_noarg
assert_rc 0 "returns success"
assert_contains "no temporary diagnostic directory specified" "logs skip message"

#=====================================================
# 3. Dangerous-path / guard cases (Req 5.1, 5.4, 5.5)
#=====================================================
echo "== Guard cases =="

# 3a. Path equal to DATAROOT -> skip, nothing deleted (Req 5.4)
reset_env
KEEPDATA="NO"
_parent="$(mktemp -d)"
DATAROOT="${_parent}/gsidiags"     # basename gsidiags so it passes basename check
mkdir -p "${DATAROOT}"
echo "keep me" > "${DATAROOT}/important"
start_case "path equal to DATAROOT"
run_helper "${DATAROOT}"
assert_rc 0 "returns success"
assert_exists "${DATAROOT}/important" "DATAROOT contents untouched"
assert_contains "skipping cleanup" "logs a skip indication"
rm -rf "${_parent}"

# 3b. Path is a parent of the COM directory -> skip, nothing deleted (Req 5.4)
reset_env
KEEPDATA="NO"
DATAROOT="$(mktemp -d)"
GSIDIAGS="${DATAROOT}/gsidiags"
COMOUT_ATMOS_ANALYSIS="${GSIDIAGS}/com/output"   # COM lives *under* gsidiags
mkdir -p "${COMOUT_ATMOS_ANALYSIS}"
echo "tar" > "${COMOUT_ATMOS_ANALYSIS}/gdas.t00z.cnvstat"
start_case "path is a parent of COM"
run_helper "${GSIDIAGS}"
assert_rc 0 "returns success"
assert_exists "${GSIDIAGS}" "gsidiags not deleted"
assert_exists "${COMOUT_ATMOS_ANALYSIS}/gdas.t00z.cnvstat" "COM output preserved"
assert_contains "parent of protected path" "logs parent-of-protected skip"
rm -rf "${DATAROOT}"

# 3c. gsidiags is a symlink pointing outside DATAROOT -> skip, nothing deleted
#     (Req 5.5, 5.1)
reset_env
KEEPDATA="NO"
DATAROOT="$(mktemp -d)"
_outside="$(mktemp -d)"
echo "outside data" > "${_outside}/precious.nc"
ln -s "${_outside}" "${DATAROOT}/gsidiags"
start_case "gsidiags symlink pointing outside DATAROOT"
run_helper "${DATAROOT}/gsidiags"
assert_rc 0 "returns success"
assert_exists "${DATAROOT}/gsidiags" "symlink itself preserved"
assert_exists "${_outside}/precious.nc" "external target untouched"
assert_contains "symlink pointing outside DATAROOT" "logs symlink skip"
rm -rf "${DATAROOT}" "${_outside}"

#=====================================================
# 4. Side-effect case: remove deletes gsidiags, COM tarballs remain
#    (Req 1.1, 1.2, 2.1, 2.2, 5.2)
#=====================================================
echo "== Removal side effects =="

reset_env
setup_fixture
KEEP_TEMP_DIAGS="NO"        # force remove
start_case "remove deletes gsidiags subtree, preserves COM tarballs"
run_helper "${GSIDIAGS}"
assert_rc 0 "returns success"
assert_missing "${GSIDIAGS}" "gsidiags directory removed"
assert_missing "${GSIDIAGS}/subdir/diag_conv_anl.nc" "gsidiags contents removed"
for _t in cnvstat oznstat radstat pcpstat; do
    assert_exists "${COMOUT_ATMOS_ANALYSIS}/gdas.t00z.${_t}" "COM ${_t} preserved"
done
cleanup_fixture

#=====================================================
# 5. Failure tolerance: read-only parent -> warning, returns success
#    (Req 1.5, 2.6)
#=====================================================
echo "== Failure tolerance =="

reset_env
KEEP_TEMP_DIAGS="NO"
DATAROOT="$(mktemp -d)"
_sub="${DATAROOT}/sub"
GSIDIAGS="${_sub}/gsidiags"
mkdir -p "${GSIDIAGS}"
echo "data" > "${GSIDIAGS}/diag_conv_ges.nc"
chmod a-w "${_sub}"          # read-only parent: cannot unlink gsidiags entry
start_case "non-removable path (read-only parent)"
run_helper "${GSIDIAGS}"
assert_rc 0 "returns success despite removal failure"
assert_contains "WARNING" "emits a warning"
assert_contains "failed to remove" "warning names the removal failure"
assert_exists "${GSIDIAGS}" "directory left in place after failed removal"
chmod u+w "${_sub}"          # restore so cleanup can proceed
rm -rf "${DATAROOT}"

#=====================================================
# Summary
#=====================================================
echo ""
echo "======================================================"
printf 'Ran %d assertions, %d failed\n' "${TESTS_RUN}" "${TESTS_FAILED}"
echo "======================================================"

if [[ "${TESTS_FAILED}" -ne 0 ]]; then
    exit 1
fi
exit 0
