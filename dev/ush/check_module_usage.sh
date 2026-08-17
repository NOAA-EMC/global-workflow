#!/bin/bash
#
# check_module_usage.sh - Determine minimum modules needed for a global-workflow job
#
# Method: Static code analysis + ldd (no runtime execution needed)
#
# Usage:
#   ./check_module_usage.sh -j JGFS_ATMOS_AWIPS_20KM_1P0DEG
#   ./check_module_usage.sh -j JGLOBAL_FORECAST -e /path/to/EXPDIR/experiment
#
# Options:
#   -j JJOB     J-Job name (e.g., JGFS_ATMOS_AWIPS_20KM_1P0DEG)
#   -e EXPDIR   Path to experiment EXPDIR (sources config files for conditional flags)
#
set -u

# ============================================================
# Parse arguments
# ============================================================
JJOB_MODE="NO"
JJOB_NAME=""
EXTRA_ARGS=""
EXPDIR_INPUT=""

while [[ $# -gt 0 ]]; do
    case "${1}" in
        -j)
            JJOB_MODE="YES"
            JJOB_NAME="${2:-}"
            shift 2
            ;;
        -e)
            EXPDIR_INPUT="${2:-}"
            shift 2
            ;;
        *)
            if [[ "${JJOB_MODE}" == "YES" ]]; then
                EXTRA_ARGS="${EXTRA_ARGS} ${1}"
                shift
            else
                break
            fi
            ;;
    esac
done

if [[ "${JJOB_MODE}" != "YES" && $# -lt 1 ]]; then
    echo "Usage:"
    echo "  $0 -j <JJOB_NAME> [-e <EXPDIR>]"
    echo "  $0 <script>"
    echo ""
    echo "Options:"
    echo "  -j JJOB     J-Job name (e.g., JGFS_ATMOS_AWIPS_20KM_1P0DEG)"
    echo "  -e EXPDIR   Experiment dir (sources config.base + job configs for flags)"
    echo ""
    echo "Examples:"
    echo "  $0 -j JGFS_ATMOS_AWIPS_20KM_1P0DEG"
    echo "  $0 -j JGLOBAL_FORECAST -e /lfs/h2/emc/stmp/user/EXPDIR/experiment"
    exit 1
fi

if [[ "${JJOB_MODE}" == "YES" && -z "${JJOB_NAME}" ]]; then
    echo "ERROR: -j requires a J-Job name"
    exit 1
fi
EXTRA_ARGS=$(echo "${EXTRA_ARGS}" | xargs)

# ============================================================
# Resolve HOMEgfs
# ============================================================
HOMEGFS="${HOMEgfs:-${HOMEglobal:-}}"
if [[ -z "${HOMEGFS}" || ! -d "${HOMEGFS}" ]]; then
    echo ""
    echo "  HOMEgfs is not set. Please provide the path to global-workflow:"
    read -rp "  HOMEgfs: " HOMEGFS
    if [[ -z "${HOMEGFS}" || ! -d "${HOMEGFS}" ]]; then
        echo "ERROR: '${HOMEGFS}' is not a valid directory."
        exit 1
    fi
fi
export HOMEgfs="${HOMEGFS}"
export HOMEglobal="${HOMEGFS}"

# ============================================================
# If J-Job mode, find the J-Job and extract info
# ============================================================
if [[ "${JJOB_MODE}" == "YES" ]]; then
    JJOB_FILE=""
    for dir in "${HOMEGFS}/jobs" "${HOMEGFS}/dev/jobs"; do
        if [[ -f "${dir}/${JJOB_NAME}" ]]; then
            JJOB_FILE="${dir}/${JJOB_NAME}"
            break
        fi
    done

    if [[ -z "${JJOB_FILE}" ]]; then
        echo "ERROR: Cannot find J-Job '${JJOB_NAME}' in jobs/ or dev/jobs/"
        exit 1
    fi

    echo "Found J-Job: ${JJOB_FILE}"

    # Determine module type from jjob_header.sh -e flag
    MODULE_ENV=$(grep -oP 'jjob_header\.sh.*-e\s+"?\K[^" ]+' "${JJOB_FILE}" | head -1 || true)
    echo "Job environment: ${MODULE_ENV:-unknown}"

    # Map job environment to module type
    case "${MODULE_ENV}" in
        anal*|eobs|ediag|eupd|esfc*)  MOD_TYPE="gsi" ;;
        atmanl*|aero*|snow*|ocn*|ice*anl*) MOD_TYPE="ufsda" ;;
        fcst*) MOD_TYPE="ufswm" ;;
        *) MOD_TYPE="run" ;;
    esac
    echo "Module type: ${MOD_TYPE}"

    # Extract ex-script
    EXSCRIPT_BASENAME=$(grep -oP 'ex[a-z0-9_]+\.(sh|py)' "${JJOB_FILE}" | head -1 || true)
    CMD_SCRIPT=""
    if [[ -n "${EXSCRIPT_BASENAME}" ]]; then
        for dir in "${HOMEGFS}/scripts" "${HOMEGFS}/dev/scripts"; do
            if [[ -f "${dir}/${EXSCRIPT_BASENAME}" ]]; then
                CMD_SCRIPT="${dir}/${EXSCRIPT_BASENAME}"
                break
            fi
        done
    fi
    if [[ -z "${CMD_SCRIPT}" ]]; then
        CMD_SCRIPT="${JJOB_FILE}"
    fi

    echo "Ex-script: ${CMD_SCRIPT}"
    SCAN_FILES="${JJOB_FILE} ${CMD_SCRIPT}"
    JOB_NAME="${JJOB_NAME}"
else
    CMD_SCRIPT="${1}"
    shift
    EXTRA_ARGS="$*"
    SCAN_FILES="${CMD_SCRIPT}"
    JOB_NAME=$(basename "${CMD_SCRIPT}" .sh)
    MOD_TYPE="run"
    JJOB_FILE=""
fi

# ============================================================
# Load modules (save current state, purge, load fresh)
# ============================================================
echo ""
echo "----------------------------------------------"
echo " Loading modules (type: ${MOD_TYPE})..."
echo "----------------------------------------------"

SAVED_MODULES=$(module -t list 2>&1 | grep -v "^$\|Currently\|No modules" || true)
module purge 2>/dev/null || true

# shellcheck disable=SC1091
source "${HOMEGFS}/dev/ush/load_modules.sh" "${MOD_TYPE}"
echo ""

# ============================================================
# Source experiment config files (for conditional flags only)
# ============================================================
echo "----------------------------------------------"
echo " Config Setup"
echo "----------------------------------------------"

# Extract config list from J-Job's -c flag
JJOB_CONFIGS=""
if [[ -n "${JJOB_FILE}" ]]; then
    JJOB_CONFIGS=$(grep -oP 'jjob_header\.sh.*-c\s+"?\K[^"]+' "${JJOB_FILE}" | head -1 || true)
fi

if [[ -n "${EXPDIR_INPUT}" && -d "${EXPDIR_INPUT}" ]]; then
    echo "  Using EXPDIR: ${EXPDIR_INPUT}"
    # Source config.base for flags like DO_WAVE, DO_OCN, etc.
    if [[ -f "${EXPDIR_INPUT}/config.base" ]]; then
        echo "  Sourcing: config.base"
        set +u
        # shellcheck disable=SC1090
        source "${EXPDIR_INPUT}/config.base" 2>/dev/null || true
        set -u
    fi
    # Source job-specific configs (e.g., config.fcst sets CCPP_SUITE)
    for cfg in ${JJOB_CONFIGS}; do
        [[ "${cfg}" == "base" ]] && continue  # already sourced
        cfg_file="${EXPDIR_INPUT}/config.${cfg}"
        if [[ -f "${cfg_file}" ]]; then
            echo "  Sourcing: config.${cfg}"
            set +u
            # shellcheck disable=SC1090
            source "${cfg_file}" 2>/dev/null || true
            set -u
        fi
    done
else
    echo "  No -e EXPDIR given. All code branches will be included (superset)."
fi
echo ""

# Derived paths
export PARMgfs="${HOMEGFS}/parm"
export PARMglobal="${PARMgfs}"
export USHgfs="${HOMEGFS}/ush"
export USHglobal="${USHgfs}"
export SCRgfs="${HOMEGFS}/scripts"
export SCRglobal="${SCRgfs}"
export EXECgfs="${HOMEGFS}/exec"
export FIXgfs="${HOMEGFS}/fix"

# Base modules always present on WCOSS2 (module reset provides these)
DEFAULT_MODULES="PrgEnv-intel intel craype"

# ============================================================
# Setup workspace
# ============================================================
WORKDIR=$(mktemp -d "${TMPDIR:-/tmp}/modcheck.XXXXXX")
trap 'rm -rf "${WORKDIR}"' EXIT

LOADED_MODS="${WORKDIR}/loaded_modules.txt"
MOD_ROOTS="${WORKDIR}/module_roots.txt"
REPORT="${WORKDIR}/report.txt"
STATIC_REPORT="${WORKDIR}/static_report.txt"
> "${STATIC_REPORT}"

echo "=============================================="
echo " Module Usage Checker"
echo "=============================================="
echo " Job:      ${JOB_NAME}"
echo " Modules:  ${MOD_TYPE}"
echo " Method:   Static analysis + ldd"
echo ""

# ============================================================
# Step 1: Capture loaded modules and their install roots
# ============================================================
echo "[1/4] Capturing loaded modules..."

module -t list 2>&1 | grep -v "^$\|Currently\|No modules" > "${LOADED_MODS}" || true

if [[ ! -s "${LOADED_MODS}" ]]; then
    echo "ERROR: No modules loaded."
    exit 1
fi

echo "  Found $(wc -l < "${LOADED_MODS}") loaded modules"

> "${MOD_ROOTS}"
while IFS= read -r mod; do
    root=$(module show "${mod}" 2>&1 | grep -oP '(?<=")(/apps|/opt|/contrib|/usr/local|/lfs)[^"]*' | \
        head -1 | sed 's|/bin.*||; s|/lib.*||; s|/share.*||; s|/include.*||' || true)
    if [[ -n "${root}" ]]; then
        echo "${mod}|${root}" >> "${MOD_ROOTS}"
    else
        echo "${mod}|UNKNOWN" >> "${MOD_ROOTS}"
    fi
done < "${LOADED_MODS}"

# ============================================================
# Step 2: Discover all scripts in the job's call chain (recursive)
# ============================================================
echo "[2/4] Discovering script call chain..."

ALL_SCRIPTS="${SCAN_FILES}"

# Recurse up to 3 levels deep to catch A->B->C->D chains
for _depth in 1 2 3; do
    _new_scripts=""
    for scan_file in ${ALL_SCRIPTS}; do
        if [[ ! -f "${scan_file}" ]]; then continue; fi

        # Find sourced scripts
        sourced=$(grep -oP '(source|\.)\s+.*?([a-zA-Z0-9_]+\.sh)' "${scan_file}" | \
            grep -oP '[a-zA-Z0-9_]+\.sh' || true)

        # Scripts referenced via variable defaults: ${VARSH:-"${USH}/script.sh"}
        var_scripts=$(grep -oP '[A-Z_]+SH:-.*?([a-zA-Z0-9_]+\.sh)' "${scan_file}" | \
            grep -oP '[a-zA-Z0-9_]+\.sh' || true)

        # Scripts called via path variables: "${SCRglobal}/exscript.sh"
        called_scripts=$(grep -oP '(SCRglobal|SCRgfs|USHglobal|USHgfs|HOMEgfs|HOMEglobal)[^}]*\}/([a-zA-Z0-9_]+\.(sh|py))' "${scan_file}" | \
            grep -oP '[a-zA-Z0-9_]+\.(sh|py)' || true)

        # Python files called directly
        py_scripts=$(grep -oP '"?\$\{[A-Z_]+\}/[a-zA-Z0-9_]+\.py"?' "${scan_file}" | \
            grep -oP '[a-zA-Z0-9_]+\.py' || true)

        for s in ${sourced} ${var_scripts} ${called_scripts} ${py_scripts}; do
            for dir in "${HOMEGFS}/ush" "${HOMEGFS}/dev/ush" "${HOMEGFS}/scripts" "${HOMEGFS}/dev/scripts" "${HOMEGFS}/ush/python" "${HOMEGFS}/gempak/ush"; do
                if [[ -f "${dir}/${s}" && ! "${ALL_SCRIPTS}" == *"${dir}/${s}"* ]]; then
                    ALL_SCRIPTS="${ALL_SCRIPTS} ${dir}/${s}"
                    _new_scripts="${_new_scripts} ${dir}/${s}"
                    break
                fi
            done
        done
    done
    # Stop recursing if no new scripts found
    if [[ -z "${_new_scripts}" ]]; then break; fi
done

echo "  Scripts found: $(echo ${ALL_SCRIPTS} | wc -w)"
for sf in ${ALL_SCRIPTS}; do
    echo "    - $(basename "${sf}")"
done


# ============================================================
# Step 3: Dynamic module analysis (no hardcoding)
#   - Queries each loaded module to learn what it provides
#   - Matches script content against module-provided resources
#   - Uses ldd to trace library deps back to module directories
# ============================================================
echo "[3/4] Analyzing modules and matching against scripts..."

# Build a map of what each module provides by querying the module system
MOD_PROVIDES="${WORKDIR}/mod_provides.txt"
MOD_ENVVARS="${WORKDIR}/mod_envvars.txt"
> "${MOD_PROVIDES}"
> "${MOD_ENVVARS}"

# Skip meta-modulefiles (gw_run, gw_awips, etc.) — these are the aggregate
# modulefiles we're trying to decompose; they shouldn't appear as "needed"
SKIP_META_PATTERN="^gw_"

while IFS= read -r mod; do
    # Skip our own meta-modulefiles
    if [[ "${mod}" =~ ${SKIP_META_PATTERN} ]]; then
        continue
    fi
    mod_info=$(module show "${mod}" 2>&1 || true)

    # Extract setenv variables (Lmod format: setenv("VAR","value"))
    # Also handle TCL format: setenv  VAR  value
    while IFS= read -r line; do
        var=$(echo "${line}" | grep -oP 'setenv\(\s*"([^"]+)"' | sed 's/setenv(\s*"//; s/"$//' || true)
        if [[ -z "${var}" ]]; then
            # Try TCL format: setenv  VARNAME  value
            var=$(echo "${line}" | grep -oP '^\s*setenv\s+(\S+)' | awk '{print $2}' || true)
        fi
        if [[ -n "${var}" ]]; then
            echo "${mod}|ENV|${var}" >> "${MOD_PROVIDES}"
            echo "${var}|${mod}" >> "${MOD_ENVVARS}"
        fi
    done <<< "$(echo "${mod_info}" | grep 'setenv')"

    # Extract PATH additions
    while IFS= read -r line; do
        dir=$(echo "${line}" | grep -oP 'prepend_path\(\s*"PATH"\s*,\s*"([^"]+)"' | sed 's/prepend_path(\s*"PATH"\s*,\s*"//; s/"$//' || true)
        if [[ -n "${dir}" ]]; then
            echo "${mod}|PATH|${dir}" >> "${MOD_PROVIDES}"
        fi
    done <<< "$(echo "${mod_info}" | grep 'prepend_path.*"PATH"')"

    # Extract LD_LIBRARY_PATH additions
    while IFS= read -r line; do
        dir=$(echo "${line}" | grep -oP 'prepend_path\(\s*"LD_LIBRARY_PATH"\s*,\s*"([^"]+)"' | sed 's/prepend_path(\s*"LD_LIBRARY_PATH"\s*,\s*"//; s/"$//' || true)
        if [[ -n "${dir}" ]]; then
            echo "${mod}|LIB|${dir}" >> "${MOD_PROVIDES}"
        fi
    done <<< "$(echo "${mod_info}" | grep 'prepend_path.*"LD_LIBRARY_PATH"')"
done < "${LOADED_MODS}"

echo "  Module provides: $(wc -l < "${MOD_PROVIDES}") entries from $(wc -l < "${LOADED_MODS}") modules"

# Concatenate all script content for matching
# Use TWO pools: full (for reporting) and job-only (for module detection)
# The job-only pool excludes framework infrastructure scripts that reference
# vars/commands generically (not specific to this job's actual runtime needs).
_infra_scripts="jjob_header.sh jjob_shell_setup.sh jjob_standard_vars.sh set_strict.sh"
all_script_content=""
job_script_content=""
for scan_file in ${ALL_SCRIPTS}; do
    if [[ -f "${scan_file}" ]]; then
        _content=$(cat "${scan_file}")
        all_script_content="${all_script_content}
${_content}"
        _sfbase=$(basename "${scan_file}")
        _is_infra="NO"
        for _inf in ${_infra_scripts}; do
            if [[ "${_sfbase}" == "${_inf}" ]]; then _is_infra="YES"; break; fi
        done
        if [[ "${_is_infra}" == "NO" ]]; then
            job_script_content="${job_script_content}
${_content}"
        fi
    fi
done

# Track needed modules
declare -A NEEDED_MODS

echo "" >> "${STATIC_REPORT}"
echo "--- Module variables referenced in scripts ---" >> "${STATIC_REPORT}"

# Method 1: Match ${VARIABLE} references against module-set environment variables
# Skip infrastructure path variables (HOMEgfs, USHgfs, etc.) which every job uses
# and are set by the job framework itself, not by a tool module at runtime.
# NOTE: Do NOT filter tool-command variables (WGRIB2, TOCGRIB2, GRB2INDEX, etc.)
#       — those indicate real runtime dependencies on the providing module.
_infra_vars_pattern="^(HOME[a-z]|USH[a-z]|SCR[a-z]|EXEC[a-z]|FIX[a-z]|PARM[a-z]|PACKAGEROOT|NWROOT|UTILROOT|COMDATEROOT|COMLISTROOT|COMLOGSROOT|FSYNC|MDATE|NDATE|NHOUR|ve_gfs_ver|model_ver|_ver$)"
while IFS='|' read -r var mod; do
    # Skip path/version infrastructure vars that don't indicate a runtime dep
    if [[ "${var}" =~ ${_infra_vars_pattern} ]]; then continue; fi
    if echo "${job_script_content}" | grep -q "\${${var}}" 2>/dev/null; then
        printf "  \${%-20s --> %s\n" "${var}}" "${mod}" >> "${STATIC_REPORT}"
        NEEDED_MODS["${mod}"]="${NEEDED_MODS[${mod}]:-}\${${var}}; "
    fi
done < "${MOD_ENVVARS}"

echo "" >> "${STATIC_REPORT}"
echo "--- Executables traced to modules (via PATH) ---" >> "${STATIC_REPORT}"

# Method 2: Find executables in COMMAND POSITION only (not random words)
# Command position = start of line, after |, after $(), after &&/||, after ;
script_commands=$(echo "${job_script_content}" | \
    grep -oP '(?:^\s*|(?<=\| )|(?<=\|\s)|(?<=\$\()|(?<=&&\s)|(?<=\|\|\s)|(?<=;\s))[a-z][a-z0-9_.-]+' | \
    sort -u || true)
# Also capture commands after ${VAR} on same line (e.g., ${WGRIB2} is handled by Method 1,
# but bare commands like "wgrib2 file" at line start are caught here)

for cmd in ${script_commands}; do
    # Skip shell builtins, keywords, and very common OS commands
    case "${cmd}" in
        echo|cat|cd|rm|mv|cp|ls|mkdir|chmod|chown|grep|sed|awk|cut|tr|sort|uniq|wc|head|tail) continue ;;
        test|true|false|exit|return|export|local|declare|read|printf|set|unset|shift) continue ;;
        if|then|else|elif|fi|for|do|done|while|until|case|esac|in|source|eval) continue ;;
        sleep|wait|date|touch|find|xargs|basename|dirname|tee|tar|gzip|gunzip|zcat) continue ;;
        file|id|env|module|bash|sh|ksh|pwd|kill|trap|ulimit|umask|type|which|man) continue ;;
        # Skip common non-command words that appear at line starts in scripts
        err|msg|pgm|pgmout|export|the|and|not|from|this|that|with|are|was|has|had) continue ;;
        # Skip words shorter than 3 chars (rarely real external commands)
        [a-z]|[a-z][a-z]) continue ;;
    esac

    cmd_path=$(which "${cmd}" 2>/dev/null || true)
    if [[ -z "${cmd_path}" ]]; then continue; fi

    cmd_dir=$(dirname "${cmd_path}")
    providing_mod=$(grep "|PATH|${cmd_dir}$" "${MOD_PROVIDES}" | head -1 | cut -d'|' -f1 || true)
    if [[ -n "${providing_mod}" ]]; then
        printf "  %-20s --> %s\n" "${cmd}" "${providing_mod}" >> "${STATIC_REPORT}"
        NEEDED_MODS["${providing_mod}"]="${NEEDED_MODS[${providing_mod}]:-}${cmd}(PATH); "
    fi
done

echo "" >> "${STATIC_REPORT}"
echo "--- Library dependencies (ldd -> module) ---" >> "${STATIC_REPORT}"

# Simulate runtime LD_LIBRARY_PATH: if the job scripts add extra paths
# (e.g., LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${HOMEglobal}/lib"), add them
# now so ldd sees the same environment the job will. Libraries resolved by
# these paths don't need a module loaded.
_extra_ldpaths=""
for _dir in "${HOMEGFS}/lib" "${HOMEGFS}/sorc/gdas.cd/build/lib"; do
    if [[ -d "${_dir}" ]]; then
        export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+${LD_LIBRARY_PATH}:}${_dir}"
        _extra_ldpaths="${_extra_ldpaths} ${_dir}"
    fi
done
if [[ -n "${_extra_ldpaths}" ]]; then
    echo "  (added to LD_LIBRARY_PATH for ldd:${_extra_ldpaths})" >> "${STATIC_REPORT}"
fi

# Method 3: ldd on binaries in exec/ that are referenced in scripts
# Only add modules for libraries that the binary CANNOT resolve via RPATH alone.
# If a binary resolves all its libs when only its own module is loaded, the
# transitive deps are satisfied via RPATH and don't need explicit module loads.
if [[ -d "${HOMEGFS}/exec" ]]; then
    for bin_path in "${HOMEGFS}/exec"/*; do
        [[ -f "${bin_path}" && -x "${bin_path}" ]] || continue
        bin_name=$(basename "${bin_path}")
        if ! echo "${all_script_content}" | grep -q "${bin_name}"; then continue; fi

        missing_libs=$(ldd "${bin_path}" 2>/dev/null | grep "not found" | awk '{print $1}' || true)

        if [[ -n "${missing_libs}" ]]; then
            # Only trace the RESOLVED libs to find modules that would satisfy the missing ones
            resolved_libs=$(ldd "${bin_path}" 2>/dev/null | grep "=>" | grep -v "not found" | awk '{print $3}' || true)
            for lib in ${resolved_libs}; do
                lib_dir=$(dirname "${lib}")
                providing_mod=$(grep "|LIB|${lib_dir}$" "${MOD_PROVIDES}" | head -1 | cut -d'|' -f1 || true)
                if [[ -n "${providing_mod}" ]]; then
                    NEEDED_MODS["${providing_mod}"]="${NEEDED_MODS[${providing_mod}]:-}${bin_name}(ldd); "
                fi
            done
            printf "  %-20s MISSING: %s\n" "${bin_name}" "${missing_libs}" >> "${STATIC_REPORT}"
        else
            # Binary resolves everything — it needs whatever module provides it in PATH,
            # which is already handled by Method 2. Don't add transitive lib deps.
            n_mods=$(ldd "${bin_path}" 2>/dev/null | grep "=>" | grep -v "not found" | wc -l)
            printf "  %-20s %s libs OK (RPATH resolves all)\n" "${bin_name}" "${n_mods}" >> "${STATIC_REPORT}"
        fi
    done
fi

# Also ldd any ELF tools found via PATH matching — ONLY if they have missing libs
# (If a tool resolves all its libs, its module is sufficient — don't add transitive deps)
for cmd in ${script_commands}; do
    cmd_path=$(which "${cmd}" 2>/dev/null || true)
    if [[ -z "${cmd_path}" || ! -f "${cmd_path}" ]]; then continue; fi
    file_type=$(file -b "${cmd_path}" 2>/dev/null || true)
    if [[ ! "${file_type}" == *"ELF"* ]]; then continue; fi
    if [[ "${cmd_path}" == "${HOMEGFS}/exec/"* ]]; then continue; fi

    # Only report if there are MISSING libraries (tool can't self-resolve)
    missing_libs=$(ldd "${cmd_path}" 2>/dev/null | grep "not found" | awk '{print $1}' || true)
    if [[ -n "${missing_libs}" ]]; then
        # Find which module provides the missing libraries
        resolved_libs=$(ldd "${cmd_path}" 2>/dev/null | grep "=>" | grep -v "not found" | awk '{print $3}' || true)
        for lib in ${resolved_libs}; do
            lib_dir=$(dirname "${lib}")
            providing_mod=$(grep "|LIB|${lib_dir}$" "${MOD_PROVIDES}" | head -1 | cut -d'|' -f1 || true)
            if [[ -n "${providing_mod}" ]]; then
                NEEDED_MODS["${providing_mod}"]="${NEEDED_MODS[${providing_mod}]:-}${cmd}(ldd-missing); "
            fi
        done
        printf "  %-20s has missing libs: %s\n" "${cmd}" "${missing_libs}" >> "${STATIC_REPORT}"
    fi
done

# Method 4: Scan Python files for subprocess/exec patterns that launch binaries
echo "" >> "${STATIC_REPORT}"
echo "--- Python-launched executables ---" >> "${STATIC_REPORT}"

for scan_file in ${ALL_SCRIPTS}; do
    if [[ ! -f "${scan_file}" ]]; then continue; fi
    # Only process .py files
    [[ "${scan_file}" == *.py ]] || continue
    fname=$(basename "${scan_file}")

    # Look for subprocess, os.exec, Popen, mpiexec patterns
    py_execs=$(grep -oP '(subprocess\.(run|Popen|call)|os\.exec\w*|mpiexec|srun|aprun)\s*\(\s*\[?\s*["\x27]([^"\x27]+)["\x27]' "${scan_file}" | \
        grep -oP '["\x27][a-zA-Z0-9_./-]+["\x27]' | tr -d "\"'" || true)
    # Also look for references to exec/ binaries
    py_exec_refs=$(grep -oP '\bexec/[a-zA-Z0-9_.-]+' "${scan_file}" | sed 's|exec/||' || true)
    # Also look for executable variable references
    py_var_execs=$(grep -oP 'self\.\w*exec\w*|EXEC[A-Z_]*' "${scan_file}" || true)

    all_py_execs="${py_execs} ${py_exec_refs}"
    if [[ -n "$(echo "${all_py_execs}" | xargs)" ]]; then
        printf "  %-30s %s\n" "${fname}:" "${all_py_execs}" >> "${STATIC_REPORT}"
        # Check if any found binaries exist in exec/
        for pbin in ${all_py_execs}; do
            pbin_base=$(basename "${pbin}")
            bin_path="${HOMEGFS}/exec/${pbin_base}"
            if [[ -f "${bin_path}" && -x "${bin_path}" ]]; then
                resolved_libs=$(ldd "${bin_path}" 2>/dev/null | grep "=>" | grep -v "not found" | awk '{print $3}' || true)
                for lib in ${resolved_libs}; do
                    lib_dir=$(dirname "${lib}")
                    providing_mod=$(grep "|LIB|${lib_dir}$" "${MOD_PROVIDES}" | head -1 | cut -d'|' -f1 || true)
                    if [[ -n "${providing_mod}" ]]; then
                        NEEDED_MODS["${providing_mod}"]="${NEEDED_MODS[${providing_mod}]:-}${pbin_base}(py+ldd); "
                    fi
                done
            fi
        done
    fi
done

# Method 5: MPI auto-include — scans only the job's own ex-script and sourced
# scripts for MPI launcher invocations. Infrastructure scripts (jjob_header.sh,
# jjob_shell_setup.sh) are excluded because they define APRUN variables
# generically for all jobs, not as evidence this job uses MPI.
_uses_mpi="NO"
_uses_cfp="NO"
_mpi_scan_files=""
for _sf in ${ALL_SCRIPTS}; do
    # Skip infrastructure scripts that define APRUN generically
    _sfbase=$(basename "${_sf}")
    case "${_sfbase}" in
        jjob_header.sh|jjob_shell_setup.sh|jjob_standard_vars.sh|set_strict.sh) continue ;;
    esac
    _mpi_scan_files="${_mpi_scan_files} ${_sf}"
done
_mpi_scan_content=""
for _sf in ${_mpi_scan_files}; do
    [[ -f "${_sf}" ]] && _mpi_scan_content="${_mpi_scan_content}
$(cat "${_sf}")"
done
# Only match actual MPI launcher INVOCATIONS (command position), not variable assignments/exports
if echo "${_mpi_scan_content}" | grep -qP '^\s*(mpiexec|mpirun|srun|aprun)\b' 2> /dev/null; then
    _uses_mpi="YES"
elif echo "${_mpi_scan_content}" | grep -qP '\$\{?APRUN[A-Z_]*\}?\s+[^=]' 2> /dev/null; then
    # ${APRUN_SOMETHING} followed by non-= means it's being invoked, not assigned
    _uses_mpi="YES"
elif echo "${_mpi_scan_content}" | grep -qP 'run_mpmd|mpmd' 2> /dev/null; then
    # run_mpmd.sh or any mpmd reference means parallel execution
    _uses_mpi="YES"
fi
# Detect cfp usage: direct call, via APRUN*CFP variable, or run_mpmd (which uses cfp)
if echo "${_mpi_scan_content}" | grep -qP '^\s*(cfp|poe|cfp_mp)\b|\$\{?APRUN[A-Z_]*CFP|run_mpmd|mpmd' 2> /dev/null; then
    _uses_cfp="YES"
fi
if [[ "${_uses_mpi}" == "YES" ]]; then
    # cray-pals (job launcher) needed for MPI execution
    cray_pals_mod=$(grep -i "^cray-pals" "${LOADED_MODS}" | head -1 || true)
    cray_mpich_mod=$(grep -i "^cray-mpich" "${LOADED_MODS}" | head -1 || true)
    if [[ -n "${cray_pals_mod}" ]]; then
        NEEDED_MODS["${cray_pals_mod}"]="${NEEDED_MODS[${cray_pals_mod}]:-}MPI-launcher(auto); "
        printf "  %-20s --> auto-included (MPI job launcher)\n" "${cray_pals_mod}" >> "${STATIC_REPORT}"
    fi
    if [[ -n "${cray_mpich_mod}" ]]; then
        NEEDED_MODS["${cray_mpich_mod}"]="${NEEDED_MODS[${cray_mpich_mod}]:-}MPI-runtime(auto); "
        printf "  %-20s --> auto-included (MPI runtime)\n" "${cray_mpich_mod}" >> "${STATIC_REPORT}"
    fi
    # Include cfp if the job uses MPMD execution
    if [[ "${_uses_cfp}" == "YES" ]]; then
        cfp_mod=$(grep -i "^cfp" "${LOADED_MODS}" | head -1 || true)
        if [[ -n "${cfp_mod}" ]]; then
            NEEDED_MODS["${cfp_mod}"]="${NEEDED_MODS[${cfp_mod}]:-}cfp(auto); "
            printf "  %-20s --> auto-included (parallel forking)\n" "${cfp_mod}" >> "${STATIC_REPORT}"
        fi
    fi
fi

# Note for ufswm module type jobs
if [[ "${MOD_TYPE}" == "ufswm" ]]; then
    echo "" >> "${STATIC_REPORT}"
    echo "--- NOTE: ufswm job (forecast) ---" >> "${STATIC_REPORT}"
    echo "  This job uses the UFS Weather Model. Module deps come from the" >> "${STATIC_REPORT}"
    echo "  build system: sorc/ufs_model.fd/modulefiles/ufs_\${MACHINE_ID}.intel" >> "${STATIC_REPORT}"
    echo "  Refer to that modulefile for the definitive compiled-against deps." >> "${STATIC_REPORT}"
fi

# ============================================================
# Step 4: Shell/script module detection
# ============================================================
echo "[4/4] Scanning for shell utility functions and script commands..."

# Map shell functions/commands to their providing module
declare -A SHELL_FUNC_TO_MODULE=(
    ["err_exit"]="prod_util"
    ["err_chk"]="prod_util"
    ["prep_step"]="prod_util"
    ["startmsg"]="prod_util"
    ["postmsg"]="prod_util"
    ["cpreq"]="prod_util"
    ["cpfs"]="prod_util"
    ["setpdy.sh"]="prod_util"
    ["make_ntc_bull.pl"]="util_shared"
    ["formbul.pl"]="util_shared"
    ["make_NTC_file.pl"]="util_shared"
    ["make_tif.sh"]="util_shared"
    ["tranjb"]="util_shared"
    ["dbn_alert"]="prod_util"
    ["ndate"]="prod_util"
    ["nhour"]="prod_util"
    ["mdate"]="prod_util"
    ["finddate"]="prod_util"
)

declare -A SHELL_MODS_NEEDED

echo "" >> "${STATIC_REPORT}"
echo "--- Shell functions/scripts detected ---" >> "${STATIC_REPORT}"

for scan_file in ${ALL_SCRIPTS}; do
    if [[ ! -f "${scan_file}" ]]; then continue; fi
    fname=$(basename "${scan_file}")
    for func in "${!SHELL_FUNC_TO_MODULE[@]}"; do
        if grep -q "\b${func}\b" "${scan_file}" 2>/dev/null; then
            mod="${SHELL_FUNC_TO_MODULE[${func}]}"
            printf "  %-20s in %-30s (from: %s)\n" "${func}" "${fname}" "${mod}" >> "${STATIC_REPORT}"
            SHELL_MODS_NEEDED["${mod}"]="${SHELL_MODS_NEEDED[${mod}]:-}${func} "
        fi
    done
done

# ============================================================
# RESULTS
# ============================================================
echo ""
echo "=============================================="
echo " RESULTS: ${JOB_NAME}"
echo "=============================================="
echo ""

# Print static analysis findings
cat "${STATIC_REPORT}"

# ============================================================
# RECOMMENDATION
# ============================================================
echo ""
echo "=============================================="
echo " RECOMMENDATION for: ${JOB_NAME}"
echo "=============================================="

# Collect all needed modules with versions
echo ""
echo "----------------------------------------------"
echo " MINIMUM MODULES (rocoto / dev testing)"
echo "----------------------------------------------"
echo ""

# Always needed on WCOSS2
echo " Base (from module reset):"
for def in ${DEFAULT_MODULES}; do
    _def_ver_var="${def//-/_}_ver"
    echo "   module load ${def}/\${${_def_ver_var}}"
done

# Binary/library modules
echo ""
echo " Job-specific (binary/library):"
if [[ ${#NEEDED_MODS[@]} -gt 0 ]]; then
    for nmod in $(echo "${!NEEDED_MODS[@]}" | tr ' ' '\n' | sort -u); do
        _nmod_base="${nmod%%/*}"
        is_default="NO"
        for def in ${DEFAULT_MODULES}; do
            if [[ "${_nmod_base}" == "${def}" ]]; then is_default="YES"; break; fi
        done
        if [[ "${is_default}" == "YES" ]]; then continue; fi
        # Skip meta-modulefiles and module-reset artifacts
        if [[ "${_nmod_base}" =~ ^gw_ ]]; then continue; fi
        if [[ "${_nmod_base}" =~ ^craype-(x86|network) ]]; then continue; fi
        if [[ "${_nmod_base}" == "libfabric" ]]; then continue; fi

        matched=$(grep -i "^${nmod}" "${LOADED_MODS}" | head -1 || true)
        if [[ -n "${matched}" ]]; then
            _mod_base="${matched%%/*}"
            _slashes="${matched//[^\/]/}"
            if [[ ${#_slashes} -ge 2 ]]; then
                _mod_base="${matched%/*}"
            fi
            _mod_ver_var="${_mod_base//\//_}"
            _mod_ver_var="${_mod_ver_var//-/_}"
            _mod_ver_var="${_mod_ver_var%_D}"
            _mod_ver_var="${_mod_ver_var%_A}"
            _mod_ver_var="${_mod_ver_var}_ver"
            echo "   module load ${_mod_base}/\${${_mod_ver_var}}"
        else
            _mod_ver_var="${_nmod_base//-/_}_ver"
            echo "   module load ${_nmod_base}/\${${_mod_ver_var}}"
        fi
    done
else
    echo "   (none detected)"
fi

# Shell/script modules
if [[ ${#SHELL_MODS_NEEDED[@]} -gt 0 ]]; then
    echo ""
    echo " Job-specific (shell/script utilities):"
    for smod in $(echo "${!SHELL_MODS_NEEDED[@]}" | tr ' ' '\n' | sort -u); do
        funcs=$(echo "${SHELL_MODS_NEEDED[${smod}]}" | tr ' ' '\n' | sort -u | grep -v "^$" | tr '\n' ',' | sed 's/,$//')
        _smod_ver_var="${smod//-/_}_ver"
        echo "   module load ${smod}/\${${_smod_ver_var}}  (${funcs})"
    done
fi

# ecFlow section
echo ""
echo "----------------------------------------------"
echo " MINIMUM MODULES (ecFlow on WCOSS2)"
echo "----------------------------------------------"
echo " Base module (required - enables compiler module hierarchy):"
echo "   module load PrgEnv-intel/\${PrgEnv_intel_ver}"
echo ""
echo " If job fails, also try adding these (may be needed on some nodes):"
for def in intel craype; do
    _def_ver_var="${def//-/_}_ver"
    echo "   #module load ${def}/\${${_def_ver_var}}"
done
echo ""
echo " head.h already provides (via module reset + head.h loads):"
for artifact in craype-x86-rome craype-network-ofi libfabric cray-mpich ecflow prod_util prod_envir; do
    _art_ver_var="${artifact//-/_}_ver"
    echo "   module load ${artifact}/\${${_art_ver_var}}"
done
echo ""
echo " Job body must add:"
# Modules that head.h already provides (match by base name, not version)
_ecf_provided="PrgEnv-intel intel craype cray-mpich ecflow prod_util prod_envir craype-x86-rome craype-network-ofi libfabric"
if [[ ${#NEEDED_MODS[@]} -gt 0 ]]; then
    for nmod in $(echo "${!NEEDED_MODS[@]}" | tr ' ' '\n' | sort -u); do
        _nmod_base="${nmod%%/*}"
        is_provided="NO"
        for prov in ${_ecf_provided}; do
            if [[ "${_nmod_base}" == "${prov}" ]]; then is_provided="YES"; break; fi
        done
        if [[ "${is_provided}" == "YES" ]]; then continue; fi
        # Also skip meta-modulefiles
        if [[ "${_nmod_base}" =~ ^gw_ ]]; then continue; fi

        matched=$(grep -i "^${nmod}" "${LOADED_MODS}" | head -1 || true)
        if [[ -n "${matched}" ]]; then
            _mod_base="${matched%%/*}"
            _slashes="${matched//[^\/]/}"
            if [[ ${#_slashes} -ge 2 ]]; then
                _mod_base="${matched%/*}"
            fi
            _mod_ver_var="${_mod_base//\//_}"
            _mod_ver_var="${_mod_ver_var//-/_}"
            _mod_ver_var="${_mod_ver_var%_D}"
            _mod_ver_var="${_mod_ver_var%_A}"
            _mod_ver_var="${_mod_ver_var}_ver"
            echo "   module load ${_mod_base}/\${${_mod_ver_var}}"
        else
            _mod_ver_var="${_nmod_base//-/_}_ver"
            echo "   module load ${_nmod_base}/\${${_mod_ver_var}}"
        fi
    done
fi
# Shell modules (skip prod_util since head.h loads it)
if [[ ${#SHELL_MODS_NEEDED[@]} -gt 0 ]]; then
    for smod in $(echo "${!SHELL_MODS_NEEDED[@]}" | tr ' ' '\n' | sort -u); do
        if [[ "${smod}" == "prod_util" ]]; then continue; fi  # already in head.h
        _smod_ver_var="${smod//-/_}_ver"
        echo "   module load ${smod}/\${${_smod_ver_var}}"
    done
fi

# NOT NEEDED
echo ""
echo "----------------------------------------------"
echo " NOT NEEDED (safe to remove from ${MOD_TYPE} modulefile)"
echo "----------------------------------------------"
while IFS='|' read -r mod root; do
    mod_base="${mod%%/*}"
    is_default="NO"
    for def in ${DEFAULT_MODULES}; do
        if [[ "${mod_base}" == *"${def}"* ]]; then is_default="YES"; break; fi
    done
    if [[ "${is_default}" == "YES" ]]; then continue; fi
    # Skip meta-modulefiles and module-reset artifacts
    if [[ "${mod_base}" =~ ^gw_ ]]; then continue; fi
    if [[ "${mod_base}" =~ ^craype-(x86|network) ]]; then continue; fi
    if [[ "${mod_base}" == "libfabric" ]]; then continue; fi

    is_needed="NO"
    for nmod in "${!NEEDED_MODS[@]}"; do
        if [[ "${mod}" == *"${nmod}"* || "${nmod}" == *"${mod_base}"* ]]; then is_needed="YES"; break; fi
    done
    for smod in "${!SHELL_MODS_NEEDED[@]}"; do
        if [[ "${mod}" == *"${smod}"* ]]; then is_needed="YES"; break; fi
    done

    if [[ "${is_needed}" == "NO" ]]; then
        echo "   ${mod}"
    fi
done < "${MOD_ROOTS}"

echo ""
echo "=============================================="

# ============================================================
# COPY-PASTE SECTIONS
# Ready-to-use module load statements with version variables
# ============================================================
echo ""
echo ""
echo "############################################################"
echo "# COPY-PASTE: Minimum modules for ${JOB_NAME}"
echo "############################################################"

# --- Helper: convert module name to version variable ---
# e.g., "cray-mpich/8.1.19" -> "cray_mpich_ver"
#       "hdf5-D/1.14.0"     -> "hdf5_ver"
#       "ve/gfs/17.0"       -> "ve_gfs_ver"
_mod_to_ver_var() {
    local _mod="${1}"
    local _base="${_mod%%/*}"
    # If base contains a second / (like ve/gfs), include it
    local _slashes="${_mod//[^\/]/}"
    if [[ ${#_slashes} -ge 2 ]]; then
        # Module like ve/gfs/17.0 — base is ve/gfs
        _base="${_mod%/*}"
    fi
    # Replace / and - with _
    local _var="${_base//\//_}"
    _var="${_var//-/_}"
    # Strip trailing _D or _A suffixes (hdf5-D -> hdf5, ncdiag-A -> ncdiag)
    _var="${_var%_D}"
    _var="${_var%_A}"
    echo "${_var}_ver"
}

# Collect job-specific modules (not provided by base/head.h)
_ecf_provided="PrgEnv-intel intel craype cray-mpich cray-pals cfp ecflow prod_util prod_envir craype-x86-rome craype-network-ofi libfabric"
declare -a _job_mods=()

if [[ ${#NEEDED_MODS[@]} -gt 0 ]]; then
    for nmod in $(echo "${!NEEDED_MODS[@]}" | tr ' ' '\n' | sort -u); do
        _nmod_base="${nmod%%/*}"
        is_skip="NO"
        for prov in ${_ecf_provided}; do
            if [[ "${_nmod_base}" == "${prov}" ]]; then is_skip="YES"; break; fi
        done
        if [[ "${_nmod_base}" =~ ^gw_ ]]; then is_skip="YES"; fi
        if [[ "${is_skip}" == "YES" ]]; then continue; fi

        matched=$(grep -i "^${nmod}" "${LOADED_MODS}" | head -1 || true)
        if [[ -n "${matched}" ]]; then
            _job_mods+=("${matched}")
        else
            _job_mods+=("${nmod}")
        fi
    done
fi
# Add shell/script modules (skip prod_util — head.h provides it)
if [[ ${#SHELL_MODS_NEEDED[@]} -gt 0 ]]; then
    for smod in $(echo "${!SHELL_MODS_NEEDED[@]}" | tr ' ' '\n' | sort -u); do
        if [[ "${smod}" == "prod_util" ]]; then continue; fi
        matched=$(grep -i "^${smod}/" "${LOADED_MODS}" | head -1 || true)
        if [[ -n "${matched}" ]]; then
            _job_mods+=("${matched}")
        else
            _job_mods+=("${smod}")
        fi
    done
fi

# --- ecFlow copy-paste block ---
echo ""
echo "# ---- ecFlow (.ecf) ----"
echo "# head.h already provides: PrgEnv-intel, intel, craype, cray-mpich,"
echo "#   craype-x86-rome, craype-network-ofi, libfabric, ecflow, prod_util, prod_envir"
echo "# Paste into the .ecf body after %include <head.h>:"
echo ""
if [[ "${_uses_mpi}" == "YES" ]]; then
    # cray-pals needed for MPI job launching (head.h does NOT provide it)
    matched=$(grep -i "^cray-pals" "${LOADED_MODS}" | head -1 || true)
    if [[ -n "${matched}" ]]; then
        _ver_var=$(_mod_to_ver_var "${matched}")
        echo "module load cray-pals/\${${_ver_var}}"
    fi
    if [[ "${_uses_cfp}" == "YES" ]]; then
        matched=$(grep -i "^cfp" "${LOADED_MODS}" | head -1 || true)
        if [[ -n "${matched}" ]]; then
            _ver_var=$(_mod_to_ver_var "${matched}")
            echo "module load cfp/\${${_ver_var}}"
        fi
    fi
fi
for _m in "${_job_mods[@]}"; do
    _base="${_m%%/*}"
    _slashes="${_m//[^\/]/}"
    if [[ ${#_slashes} -ge 2 ]]; then
        _base="${_m%/*}"
    fi
    _ver_var=$(_mod_to_ver_var "${_m}")
    echo "module load ${_base}/\${${_ver_var}}"
done
if [[ "${_uses_cfp}" == "YES" ]]; then
    echo "export USE_CFP=YES"
fi

# --- Rocoto copy-paste block ---
echo ""
echo ""
echo "# ---- Rocoto (load_modules.sh / modulefile) ----"
echo "# module-setup.sh does module purge + module reset which provides:"
echo "#   PrgEnv-intel, intel, craype, craype-x86-rome, craype-network-ofi"
echo "# Paste into gw_<jobtype>.wcoss2.lua or equivalent:"
echo ""
# MPI modules if needed
if [[ "${_uses_mpi}" == "YES" ]]; then
    for _mpi_mod in cray-mpich cray-pals; do
        matched=$(grep -i "^${_mpi_mod}" "${LOADED_MODS}" | head -1 || true)
        if [[ -n "${matched}" ]]; then
            _ver_var=$(_mod_to_ver_var "${matched}")
            echo "module load ${_mpi_mod}/\${${_ver_var}}"
        fi
    done
    if [[ "${_uses_cfp}" == "YES" ]]; then
        matched=$(grep -i "^cfp" "${LOADED_MODS}" | head -1 || true)
        if [[ -n "${matched}" ]]; then
            _ver_var=$(_mod_to_ver_var "${matched}")
            echo "module load cfp/\${${_ver_var}}"
            echo "export USE_CFP=YES"
        fi
    fi
fi
# Job-specific modules
for _m in "${_job_mods[@]}"; do
    _base="${_m%%/*}"
    _slashes="${_m//[^\/]/}"
    if [[ ${#_slashes} -ge 2 ]]; then
        _base="${_m%/*}"
    fi
    _ver_var=$(_mod_to_ver_var "${_m}")
    echo "module load ${_base}/\${${_ver_var}}"
done

echo ""
echo "############################################################"
echo ""
echo "=============================================="

# ============================================================
# Restore user's original module environment
# ============================================================
echo ""
echo "Restoring your original module environment..."
module purge 2>/dev/null || true
if [[ -n "${SAVED_MODULES}" ]]; then
    while IFS= read -r mod; do
        module load "${mod}" 2>/dev/null || true
    done <<< "${SAVED_MODULES}"
    echo "  Restored $(echo "${SAVED_MODULES}" | wc -l) modules."
else
    echo "  (no modules were loaded before)"
fi
