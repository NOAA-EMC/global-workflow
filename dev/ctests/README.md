# CTest Framework Quick Start

This directory contains the CTest framework for testing Rocoto workflow jobs (JJOBS) independently. Each test runs in an isolated environment with staged input files from nightly baseline runs.

> **📖 Complete Documentation**: See the [comprehensive testing documentation](../../docs/source/testing.rst) for detailed information on framework architecture, YAML configuration, test patterns, CI/CD integration, and troubleshooting.

## Quick Start Guide

### Prerequisites

The following environment variables must be set (either in your environment or via platform configuration):

```bash
HPC_ACCOUNT       # Your HPC allocation account
STAGED_CTESTS     # Path to nightly baseline COMROOT
ICSDIR_ROOT       # Path to initial condition files
```

These are typically defined in `$HOMEgfs/dev/ci/platforms/config.$MACHINE_ID`.

### Configure and Build

```bash
cd $HOMEgfs/dev/ctests
mkdir -p build
cd build

# Configure using environment variables from config.$MACHINE_ID
cmake ../../..

# Or configure with explicit command-line options
cmake -DHPC_ACCOUNT=myaccount \
      -DSTAGED_CTESTS=/path/to/baselines/RUNTESTS \
      -DICSDIR_ROOT=/path/to/ics \
      ../../..
```

### Run Tests

```bash
# Run all tests
ctest

# Run tests for a specific configuration case
ctest -L C48_ATM

# Run test for a specific JJOB
cest -L C48_ATM-gfs_atmos_prod_f000-f002

# Run a specific test with verbose output
ctest -R test_C48_ATM-gfs_fcst_seg0_execute -V

# Run tests in parallel (4 concurrent tests)
ctest -j 4

# Show test list without running
ctest -N
```

### Common CTest Options

| Option | Description |
|--------|-------------|
| `-V` | Verbose output |
| `-VV` | Extra verbose output |
| `-N` | Dry run (list tests without executing) |
| `-L <label>` | Run tests matching label (e.g., `-L C48_ATM`) |
| `-R <regex>` | Run tests matching regex pattern |
| `-j <N>` | Run N tests in parallel |
| `--output-on-failure` | Show output only for failed tests |
| `--rerun-failed` | Rerun only previously failed tests |

## Test Structure

Each test consists of four phases executed sequentially:

1. **Setup**: Creates experiment directory and configuration
2. **Stage**: Stages input files from baseline into test COMROOT
3. **Execute**: Runs the job script and monitors execution
4. **Validate**: Compares outputs against baseline results

Test phases are automatically chained via CMake dependencies.

## Validation Modes

Control validation behavior with the `CTEST_VALIDATION_MODE` environment variable:

```bash
# PRESENCE_ONLY (default): Verify files exist, no checksum validation
export CTEST_VALIDATION_MODE=PRESENCE_ONLY
ctest -R validate

# STRICT: All files must exist AND checksums must match
export CTEST_VALIDATION_MODE=STRICT
ctest -R validate

# CHECKSUM_ONLY: Validate checksums for existing files, ignore missing files
export CTEST_VALIDATION_MODE=CHECKSUM_ONLY
ctest -R validate
```

## Available Tests

Current test cases include:

| Test Name | Configuration | Component | Description |
|-----------|--------------|-----------|-------------|
| `C48_ATM-gfs_fcst_seg0` | C48_ATM | Atmosphere | Atmosphere-only forecast |
| `C48_ATM-gfs_atmos_prod_f000-f002` | C48_ATM | Products | Atmosphere product generation |
| `C48_S2SW-gfs_fcst_seg0` | C48_S2SW | Coupled | Coupled forecast (atmos-ocean-ice-wave) |
| `C48_S2SW-gfs_ocean_prod_f006` | C48_S2SW | Products | Ocean product generation |
| `C48_S2SW-gfs_ice_prod_f006` | C48_S2SW | Products | Ice product generation |
| `C48_S2SWA_gefs-gefs_fcst_mem001_seg0` | C48_S2SWA_gefs | Ensemble | GEFS ensemble member forecast |

## Adding New Tests

### 1. Add test definition to `CMakeLists.txt`:

```cmake
AddJJOBTest(
  CASE "C48_ATM"              # Configuration case
  JOB  "gfs_analysis"         # Job identifier
  TEST_DATE "2021032312"      # Test cycle (YYYYMMDDHH)
)
```

### 2. Create YAML file `cases/C48_ATM-gfs_analysis.yaml`:

```yaml
{% set cyc = TEST_DATE | strftime('%H') %}
{% set PDY = TEST_DATE | to_YMD %}
{% set SRC_DIR = STAGED_CTESTS + '/COMROOT/' + PSLOT %}
{% set DST_DIR = RUNTESTS + '/COMROOT/' + TEST_NAME %}

input_files:
    mkdir:
        - {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/input
    
    copy:
        - [{{ SRC_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/input/gfs_ctrl.nc,
           {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/input/gfs_ctrl.nc]
        # Add additional required files...

output_files:
    cmpfiles:
        - [{{ SRC_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/analysis/atminc.nc,
           {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/analysis/atminc.nc]
        # Add additional output files...
```

### 3. Build and test:

```bash
cd build
cmake ../../..
ctest -L C48_S2SW -j 3
```

## Naming Convention

Test names follow the pattern: `CASE-JOB.yaml`

**Examples:**
- `C48_ATM-gfs_fcst_seg0.yaml` - Atmosphere forecast
- `C48_S2SW-gfs_ocean_prod_f006.yaml` - Ocean products
- `C48_S2SWA_gefs-gefs_fcst_mem001_seg0.yaml` - Ensemble member forecast

## Troubleshooting

### Missing Input Files

```bash
# Compare baseline with test environment
ls ${STAGED_CTESTS}/COMROOT/${PSLOT}/gfs.${PDY}/${cyc}/
ls ${RUNTESTS}/COMROOT/${TEST_NAME}/gfs.${PDY}/${cyc}/
```

Add missing files to the YAML `input_files.copy` section.

### Checksum Validation Failures

For development, use presence-only validation:
```bash
export CTEST_VALIDATION_MODE=PRESENCE_ONLY
ctest -R validate
```

### Verbose Debugging

```bash
# Enable debug logging
export LOGGING_LEVEL=DEBUG

# Run with maximum verbosity
ctest -R test_name -VV

# Check test logs
tail -f ${RUNTESTS}/COMROOT/${TEST_NAME}_*/EXPDIR/logs/*.log
```

### Manual Execution

Run test phases manually for debugging:

```bash
cd build/scripts
./setup.sh TEST_NAME CASE_YAML TEST_DATE
./stage.sh CASE_NAME TEST_NAME TEST_DATE
./execute.sh TEST_NAME JOB_NAME TEST_DATE
./validate.sh CASE_NAME TEST_NAME TEST_DATE
```

## Key Directories

```
$HOMEgfs/dev/ctests/              # Framework root
├── build/                        # CMake build directory (create this)
├── cases/                        # YAML test definitions
├── scripts/                      # Test phase scripts
└── CMakeLists.txt                # Test configuration

$HOMEgfs/dev/ci/platforms/        # Platform-specific configuration
└── config.$MACHINE_ID            # Machine settings (STAGED_CTESTS, HPC_ACCOUNT, etc.)

${STAGED_CTESTS}/COMROOT/         # Nightly baseline outputs (input source)
${RUNTESTS}/COMROOT/              # Test execution environments (created by tests)
```

## Platform Configuration

Platform-specific settings are in `$HOMEgfs/dev/ci/platforms/config.$MACHINE_ID`:

```bash
# Example from config.hera
export GFS_CI_ROOT=/scratch1/NCEPDEV/global/Terry.McGuinness/GFS_CI_ROOT
export GITLAB_BUILDS_DIR=${GFS_CI_ROOT}/BUILDS/GITLAB
export STAGED_CTESTS=${GITLAB_BUILDS_DIR}/stable/RUNTESTS
export ICSDIR_ROOT=/scratch1/NCEPDEV/global/glopara/data/ICSDIR
export HPC_ACCOUNT=nems
```

Source the appropriate configuration before running CMake:

```bash
source $HOMEgfs/ush/detect_machine.sh
source $HOMEgfs/dev/ci/platforms/config.$MACHINE_ID
```

## Additional Resources

- **Complete Documentation**: `docs/source/testing.rst`
- **Test Case Examples**: `cases/*.yaml`
- **CI/CD Pipeline**: `../ci/gitlab-ci-hosts.yml`
- **Job Scripts**: `../../jobs/JGLOBAL_*`
- **Platform Configuration**: `../ci/platforms/config.*`

---

**Framework Version**: 1.0  
**Last Updated**: October 2025  
**Status**: Active Development

For detailed architecture, YAML templating, CI/CD integration, and comprehensive troubleshooting, see the [full testing documentation](../../docs/source/testing.rst).
