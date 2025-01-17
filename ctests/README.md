# CTest Framework for NOAA Global Workflow

This directory contains the CTest framework for testing Rocoto JJOBS. The framework allows you to stage, execute, and validate individual JJOBS independently from other jobs in the workflow. Each test requires its own YAML definition of inputs and configurations.

## Overview

The CTest framework consists of the following scripts:
- **setup.sh.in**: Prepares the environment and creates the experiment.
- **stage.sh.in**: Stages the input files needed to run a JJOB.
- **execute.sh.in**: Executes the JJOB and monitors its status.
- **validate.sh.in**: (TODO) Validates the results of the JJOB.

## Usage

### CMake Configuration

To configure the CTest framework using CMake, you need to provide several environment variables or default values. Here is an example of how to configure and build the project:

```bash
# Set environment variables (may also be include at command line with -D)
export HPC_ACCOUNT="your_hpc_account"
export ICSDIR_ROOT="/path/to/icsdir_root"
export STAGED_TESTS_DIR="/path/to/staged_tests_dir"

# Run CMake to configure the ctest framework
cmake -S /path/to/HOMEgfs -B /path/to/build -DRUNTESTS=/path/to/runtests

```

### Running Tests with CTest

Once the project is configured, you can run the tests using CTest. Here are some examples:

#### Run All Tests

```bash
cd /path/to/build
ctest
```

#### Run Tests for a Specific Case

You can use the `-L` option with CTest to run tests for a specific case. For example, to run tests for the `C48_ATM` case:

```bash
cd /path/to/build
ctest -L C48_ATM
```

To add a new test use the **AddJJOBTest()** function at the end of the `$HOMEgfs/ctest/CMakeLists.txt` file as follows:
```cmake
AddJJOBTest(
  CASE "C48_ATM"
  JOB  "gfs_fcst_seg0"
  TEST_DATE "2021032312"
)
```
Then create a new YAML file with the required staged input files as is done with this example found in `$HOMEgfs/ctests/cases/C48_ATM_gfs_fcts_seg0.yaml`
