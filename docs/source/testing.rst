############################
Testing Global Workflow Jobs
############################

The Global Workflow includes a comprehensive CTest framework for testing individual Rocoto workflow jobs (JJOBS) independently from the full workflow system. This framework enables developers to validate job behavior, verify outputs, and perform rapid development iterations without running complete workflow cycles (see `README.md <https://github.com/NOAA-EMC/global-workflow/tree/develop/dev/ctests#readme>`_ for quick start guidelines).

**********
Overview
**********

Testing Philosophy
==================

The CTest framework follows a **self-contained test philosophy** where each test creates an isolated environment with all required input files. This approach provides:

* **Independence**: Tests can run without dependencies on other workflow jobs
* **Parallelization**: Multiple tests execute simultaneously without interference
* **Reproducibility**: Consistent results across test runs
* **Rapid Development**: Quick iteration cycles for debugging and feature development
* **Validation**: Automated verification of job outputs against baseline runs

Framework Components
====================

The testing framework consists of four primary execution phases:

1. **Setup** (``setup.sh``): Creates the experiment directory structure and configuration
2. **Stage** (``stage.sh``): Stages input files from nightly baseline runs into test COMROOT
3. **Execute** (``execute.sh``): Runs the job script and monitors execution status
4. **Validate** (``validate.sh``): Compares outputs against expected results from baseline runs

Each phase is orchestrated through CMake/CTest, providing seamless integration with continuous integration (CI) pipelines and local development workflows.

*********************
Framework Architecture
*********************

Directory Structure
===================

The testing framework operates within a well-defined directory hierarchy:

**Test Execution Environment**::

    ${RUNTESTS}/COMROOT/${TEST_NAME}/          # Test-specific COMROOT
    ├── gfs.YYYYMMDD/                          # Cycle date directory
    │   └── HH/                                # Cycle hour
    │       └── model/                         # Model output structure
    │           ├── atmos/                     # Atmosphere component
    │           │   ├── input/                 # Initial conditions
    │           │   ├── history/               # Forecast outputs
    │           │   └── restart/               # Restart files
    │           ├── ocean/                     # Ocean component
    │           ├── ice/                       # Sea ice component
    │           └── wave/                      # Wave component
    └── EXPDIR/                                # Experiment configuration

**Baseline Source Data**::

    ${STAGED_CTESTS}/COMROOT/${PSLOT}/         # Nightly stable baseline
    ├── gfs.YYYYMMDD/                          # Same structure as test
    │   └── HH/
    │       └── model/
    └── ...

Configuration Management
=========================

Platform-specific configuration is defined in::

    $HOMEgfs/dev/ci/platforms/config.$MACHINE_ID

**Key Variables**:

.. code-block:: bash

    # Example from config.hera
    export GITLAB_BUILDS_DIR=${GFS_CI_ROOT}/BUILDS/GITLAB
    export STAGED_CTESTS=${GITLAB_BUILDS_DIR}/stable/RUNTESTS
    export ICSDIR_ROOT=/scratch1/NCEPDEV/global/glopara/data/ICSDIR
    export HPC_ACCOUNT=nems

These variables define:

* ``STAGED_CTESTS``: Location of nightly stable baseline COMROOT directories
* ``ICSDIR_ROOT``: Root directory for initial condition files
* ``HPC_ACCOUNT``: HPC allocation for test job submission
* ``RUNTESTS``: Working directory for test execution (defaults to ``${CMAKE_BINARY_DIR}/RUNTESTS``)

CMake Integration
=================

The framework uses CMake to configure and manage test execution. The main CMakeLists.txt file is located at ``$HOMEgfs/dev/ctests/CMakeLists.txt``.

**AddJJOBTest Function**:

.. code-block:: cmake

    AddJJOBTest(
      CASE "C48_ATM"              # Configuration case
      JOB  "gfs_fcst_seg0"        # Job identifier
      TEST_DATE "2021032312"      # Test cycle (YYYYMMDDHH)
    )

This function generates four CTest test cases:

* ``test_${TEST_NAME}_setup``
* ``test_${TEST_NAME}_stage``
* ``test_${TEST_NAME}_execute``
* ``test_${TEST_NAME}_validate``

Each test depends on the previous phase, ensuring proper execution order.

*************************
Test Case Configuration
*************************

Naming Conventions
==================

Test cases follow a structured naming convention that connects configuration cases, job names, and YAML filenames:

**Format**: ``CASE-JOB.yaml``

**Components**:

* **CASE**: Configuration identifier (e.g., ``C48_ATM``, ``C48_S2SW``, ``C48_S2SWA_gefs``)
* **JOB**: Job name from ``jobs/JGLOBAL_*`` scripts (e.g., ``gfs_fcst_seg0``, ``gfs_atmos_prod_f000-f002``)

**Naming Examples**:

+---------------------------+---------------------------------------+----------------------------+
| CMakeLists.txt Entry      | YAML Filename                         | Job Script                 |
+===========================+=======================================+============================+
| ``CASE "C48_ATM"``        | ``C48_ATM-gfs_fcst_seg0.yaml``        | ``JGLOBAL_FORECAST``       |
| ``JOB "gfs_fcst_seg0"``   |                                       |                            |
+---------------------------+---------------------------------------+----------------------------+
| ``CASE "C48_S2SW"``       | ``C48_S2SW-gfs_ocean_prod_f006.yaml`` | ``JGLOBAL_OCEAN_PRODUCTS`` |
| ``JOB "gfs_ocean_prod_``  |                                       |                            |
| ``f006"``                 |                                       |                            |
+---------------------------+---------------------------------------+----------------------------+
| ``CASE "C48_S2SWA_gefs"`` | ``C48_S2SWA_gefs-gefs_fcst_mem001_``  | ``JGLOBAL_FORECAST``       |
| ``JOB "gefs_fcst_mem001_``| ``seg0.yaml``                         |                            |
| ``seg0"``                 |                                       |                            |
+---------------------------+---------------------------------------+----------------------------+

YAML File Structure
===================

Each test case is defined by a YAML file using Jinja2 templating. The YAML file specifies:

1. Input files to stage from baseline runs
2. Directory structure to create
3. Expected output files for validation

**Basic Template Structure**:

.. code-block:: yaml

    {% set cyc = TEST_DATE | strftime('%H') %}
    {% set PDY = TEST_DATE | to_YMD %}
    {% set SRC_DIR = STAGED_CTESTS + '/COMROOT/' + PSLOT %}
    {% set DST_DIR = RUNTESTS + '/COMROOT/' + TEST_NAME %}

    input_files:
        mkdir:
            - {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/input
            - {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/history
        
        copy:
            - [{{ SRC_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/input/gfs_ctrl.nc,
               {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/input/gfs_ctrl.nc]
            - [{{ SRC_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/input/gfs_data.tile1.nc,
               {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/input/gfs_data.tile1.nc]
            # ... Additional input files

    output_files:
        cmpfiles:
            - [{{ SRC_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/history/gfs.t{{ cyc }}z.atmf006.nc,
               {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/history/gfs.t{{ cyc }}z.atmf006.nc]
            # ... Additional output files for comparison

**Available Jinja2 Variables**:

* ``TEST_DATE``: Python datetime object for the test cycle
* ``STAGED_CTESTS``: Path to baseline COMROOT
* ``RUNTESTS``: Path to test execution directory
* ``PSLOT``: Baseline case name
* ``TEST_NAME``: Generated test case identifier
* ``cyc``: Cycle hour (derived from TEST_DATE)
* ``PDY``: Production date YYYYMMDD (derived from TEST_DATE)

**Jinja2 Filters**:

* ``strftime('%H')``: Format time component
* ``to_YMD``: Convert to YYYYMMDD format
* Standard arithmetic and string operations

Multi-Cycle Tests
=================

Some tests require data from multiple cycles (e.g., coupled forecasts needing restart files from previous cycle). Use offset variables for this pattern:

.. code-block:: yaml

    {% set H_offset = '-6H' %}
    {% set TEST_DATE_offset = TEST_DATE + H_offset %}
    {% set cyc_offset = TEST_DATE_offset | strftime('%H') %}

    input_files:
        mkdir:
            # Current cycle (12Z)
            - {{ DST_DIR }}/gefs.{{ PDY }}/{{ cyc }}/mem001/model/atmos/input
            
            # Previous cycle (06Z)
            - {{ DST_DIR }}/gefs.{{ PDY }}/{{ cyc_offset }}/mem001/model/ocean/restart
        
        copy:
            # Current cycle ICs
            - [{{ SRC_DIR }}/gefs.{{ PDY }}/{{ cyc }}/mem001/model/atmos/input/gfs_ctrl.nc,
               {{ DST_DIR }}/gefs.{{ PDY }}/{{ cyc }}/mem001/model/atmos/input/gfs_ctrl.nc]
            
            # Previous cycle restarts
            - [{{ SRC_DIR }}/gefs.{{ PDY }}/{{ cyc_offset }}/mem001/model/ocean/restart/MOM.res.nc,
               {{ DST_DIR }}/gefs.{{ PDY }}/{{ cyc_offset }}/mem001/model/ocean/restart/MOM.res.nc]

This creates directory structures for **both** current and previous cycles in the test environment.

**************************
Common Test Patterns
**************************

Atmosphere-Only Forecast
========================

**Use Case**: Testing atmosphere component independently

**Input Requirements**:

* 13 atmosphere initial condition files:
    * ``gfs_ctrl.nc``
    * ``gfs_data.tile[1-6].nc`` (6 files)
    * ``sfc_data.tile[1-6].nc`` (6 files)

**Example**: ``C48_ATM-gfs_fcst_seg0.yaml``

**Key Characteristics**:

* Single cycle data staging
* No component coupling
* Simplest test pattern for forecast validation

Coupled Forecast
================

**Use Case**: Testing coupled atmosphere-ocean-ice-wave system

**Input Requirements**:

* 13 atmosphere ICs (current cycle)
* 1 ocean restart file (previous cycle): ``MOM.res.nc``
* 1 ice restart file (previous cycle): ``cice.res.nc``
* 1 wave restart file (previous cycle): ``restart.ww3``
* Wave grid definition files: ``mod_def.*``

**Example**: ``C48_S2SW-gfs_fcst_seg0.yaml``

**Key Characteristics**:

* Multi-cycle data staging (current + previous)
* Requires restart files from prior cycle
* Tests component coupling interfaces
* More complex directory structures

Ensemble Member Test
====================

**Use Case**: Testing GEFS ensemble member forecasts

**Input Requirements**:

* 13 atmosphere ICs from current cycle (in ``mem001/`` subdirectory)
* 3 restart files from previous cycle (in ``mem001/`` subdirectory):
    * ``MOM.res.nc`` (ocean)
    * ``cice.res.nc`` (ice)
    * ``restart.ww3`` (wave)
* 1 wave prep file from current cycle

**Example**: ``C48_S2SWA_gefs-gefs_fcst_mem001_seg0.yaml``

**Special Considerations**:

* Files organized in member-specific subdirectories (``mem001/``, ``mem002/``, etc.)
* Requires both current and previous cycle directory structures
* Tests ensemble-specific workflow paths
* Member numbers embedded in job and file names

Product Generation Test
=======================

**Use Case**: Testing post-processing and product generation

**Input Requirements**:

* Forecast history files from previous forecast test/baseline
* May require multiple forecast hours
* Product-specific configuration files

**Example**: ``C48_ATM-gfs_atmos_prod_f000-f002.yaml``

**Key Characteristics**:

* Depends on forecast output files
* Tests downstream processing
* Validates product file formats
* Multiple forecast hour handling

**************************
Running Tests with CMake
**************************

Environment Setup
=================

Before running tests, ensure the required environment variables are set. These can be provided via:

1. Platform configuration files (``$HOMEgfs/dev/ci/platforms/config.$MACHINE_ID``)
2. Command-line CMake options (``-DVARIABLE=value``)
3. Environment variables exported in shell

**Required Variables**:

* ``HPC_ACCOUNT``: HPC allocation account
* ``STAGED_CTESTS``: Path to nightly baseline COMROOT
* ``ICSDIR_ROOT``: Path to initial condition files root

**Optional Variables**:

* ``RUNTESTS``: Test execution directory (defaults to ``${CMAKE_BINARY_DIR}/RUNTESTS``)
* ``HOMEgfs``: Global workflow root (defaults to ``${PROJECT_SOURCE_DIR}``)

Configuration
=============

Configure the CTest framework using CMake from the ctests directory:

.. code-block:: bash

    cd $HOMEgfs/dev/ctests
    mkdir -p build
    cd build
    
    # Configure with environment variables
    cmake ../..
    
    # Or configure with command-line options
    cmake -DHPC_ACCOUNT=myaccount \
          -DSTAGED_CTESTS=/path/to/baselines/RUNTESTS \
          -DICSDIR_ROOT=/path/to/ics \
          ../..

CMake will:

1. Detect the platform via ``detect_machine.sh``
2. Read platform-specific configuration
3. Generate test scripts from templates (``*.sh.in``)
4. Register all tests with CTest
5. Create the ``RUNTESTS`` directory structure

Running Tests
=============

**Run All Tests**:

.. code-block:: bash

    cd $HOMEgfs/dev/ctests/build
    ctest

**Run Tests by Label** (all tests for a specific case):

.. code-block:: bash

    # Run all C48_ATM tests
    ctest -L C48_ATM
    
    # Run all C48_S2SW tests
    ctest -L C48_S2SW

**Run Specific Test with Verbose Output**:

.. code-block:: bash

    # Run specific test with detailed logging
    ctest -R test_C48_ATM-gfs_fcst_seg0_execute -V
    
    # Run entire test sequence for one case
    ctest -R C48_S2SW-gfs_fcst_seg0 -V

**Run Specific Test Phase**:

.. code-block:: bash

    # Run only setup phase
    ctest -R test_C48_ATM-gfs_fcst_seg0_setup
    
    # Run only validation phase
    ctest -R test_C48_ATM-gfs_fcst_seg0_validate

**Parallel Test Execution**:

.. code-block:: bash

    # Run up to 4 tests in parallel
    ctest -j 4

**Common CTest Options**:

* ``-V``: Verbose output (shows test execution details)
* ``-VV``: Extra verbose (shows all subprocess output)
* ``-N``: Dry run (show tests without executing)
* ``--output-on-failure``: Show output only for failed tests
* ``--rerun-failed``: Rerun only previously failed tests

Validation Modes
================

The validation phase supports multiple modes controlled by the ``CTEST_VALIDATION_MODE`` environment variable:

**PRESENCE_ONLY** (default):
    * Verify all expected output files exist
    * No checksum validation
    * Fastest validation mode

**STRICT**:
    * All files must exist **AND** checksums must match baseline
    * Most rigorous validation
    * Detects any output differences

**CHECKSUM_ONLY**:
    * Validate checksums only for existing files
    * Don't fail on missing files
    * Useful for partial output validation

Set validation mode before running tests:

.. code-block:: bash

    export CTEST_VALIDATION_MODE=STRICT
    ctest -R test_C48_ATM-gfs_fcst_seg0_validate -V

************************
Adding New Tests
************************

Step-by-Step Procedure
=======================

**Step 1: Add Test Definition to CMakeLists.txt**

Add the test at the end of ``$HOMEgfs/dev/ctests/CMakeLists.txt``:

.. code-block:: cmake

    AddJJOBTest(
      CASE "C48_ATM"              # Configuration case
      JOB  "gfs_analysis"         # Job name
      TEST_DATE "2021032312"      # Cycle date/hour (YYYYMMDDHH)
    )

**Step 2: Create YAML Case File**

Create a YAML file following the naming convention in ``$HOMEgfs/dev/ctests/cases/``:

**Filename**: ``${CASE}-${JOB}.yaml``

For the example above: ``C48_ATM-gfs_analysis.yaml``

**Step 3: Identify Required Input Files**

**Method 1: Inspect Stable Baseline Run**

.. code-block:: bash

    # Navigate to stable baseline COMROOT
    cd ${STAGED_CTESTS}/COMROOT/${PSLOT}
    
    # List atmosphere input files
    ls gfs.20210323/12/model/atmos/input/
    
    # List restart files from previous cycle
    ls gfs.20210323/06/model/ocean/restart/

**Method 2: Analyze Job Script Requirements**

Review the job script (e.g., ``jobs/JGLOBAL_ANALYSIS``) to understand:

* Environment variables pointing to input directories
* Files the job expects to exist
* Cycles referenced (current vs previous)
* Component-specific requirements

**Step 4: Define Input Files in YAML**

Create the YAML file with proper input staging configuration:

.. code-block:: yaml

    {% set cyc = TEST_DATE | strftime('%H') %}
    {% set PDY = TEST_DATE | to_YMD %}
    {% set SRC_DIR = STAGED_CTESTS + '/COMROOT/' + PSLOT %}
    {% set DST_DIR = RUNTESTS + '/COMROOT/' + TEST_NAME %}

    input_files:
        mkdir:
            # Create all necessary directory structures
            - {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/input
            - {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/analysis
        
        copy:
            # Stage all required input files from baseline
            - [{{ SRC_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/input/gfs_ctrl.nc,
               {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/input/gfs_ctrl.nc]
            # ... Add all required files

    output_files:
        cmpfiles:
            # Define expected output files for validation
            - [{{ SRC_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/analysis/atminc.nc,
               {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/analysis/atminc.nc]
            # ... Add all output files to validate

**Step 5: Build and Test**

.. code-block:: bash

    cd $HOMEgfs/dev/ctests/build
    cmake ../..
    ctest -R test_C48_ATM-gfs_analysis_execute -V

**Step 6: Iterate and Debug**

* Review test output for missing files
* Add missing inputs to YAML configuration
* Verify directory structures match job expectations
* Validate outputs against baseline results

Best Practices
==============

1. **Start with Working Examples**: Copy and modify an existing similar test
2. **Test Incrementally**: Add input files one at a time and test
3. **Document Assumptions**: Add comments in YAML explaining file sources
4. **Use Consistent Naming**: Follow the established ``CASE-JOB.yaml`` convention
5. **Verify Stable Baseline**: Ensure nightly runs have completed before creating tests
6. **Check Both Cycles**: For coupled tests, verify both current and previous cycle files exist
7. **Validate Early**: Run validation frequently during development to catch issues

****************************
CI/CD Integration
****************************

GitLab CI Pipeline
==================

The CTest framework integrates with GitLab CI/CD pipelines through configuration files:

* ``dev/ci/gitlab-ci-hosts.yml``: Production pipeline configuration
* ``dev/ci/gitlab-ci-hosts_dev.yml``: Development pipeline configuration

**Pipeline Stages**:

1. **build**: Compile workflow components
2. **setup_tests**: Prepare test environments
3. **run_tests**: Execute CTest test cases in parallel
4. **setup_experiments**: Create full workflow experiments
5. **run_experiments**: Execute workflow cycles
6. **finalize**: Cleanup and archiving

**Parallel Test Matrix**:

.. code-block:: yaml

    .ctests_cases_template:
      extends: .run_ctests_template
      stage: run_tests
      parallel:
        matrix:
          - CTEST_NAME:
            - C48_ATM-gfs_fcst_seg0
            - C48_ATM-gfs_atmos_prod_f000-f002
            - C48_S2SW-gfs_fcst_seg0
            - C48_S2SW-gfs_ocean_prod_f006
            - C48_S2SW-gfs_ice_prod_f006
            - C48_S2SWA_gefs-gefs_fcst_mem001_seg0

This configuration creates parallel CI jobs, running multiple tests simultaneously across available runners.

Platform Support
================

CTest framework runs on all Tier 1 and Tier 2 platforms:

**Tier 1** (Full operational support):

* **WCOSS2**: NOAA operational system
* **Hercules**: MSU research system (no TC Tracker)

**Tier 2** (Development support):

* **Hera**: NOAA RDHPCS research system
* **Orion**: MSU research system (GSI runs slowly)

Platform-specific configurations are maintained in ``dev/ci/platforms/config.$MACHINE_ID``.

****************************
Troubleshooting
****************************

Common Issues
=============

Missing Input Files
-------------------

**Error**: Job fails with "No such file or directory"

**Diagnosis**:

.. code-block:: bash

    # Compare stable baseline
    ls ${STAGED_CTESTS}/COMROOT/${PSLOT}/gfs.${PDY}/${cyc}/
    
    # With test environment
    ls ${RUNTESTS}/COMROOT/${TEST_NAME}/gfs.${PDY}/${cyc}/

**Solution**: Add missing files to YAML ``input_files.copy`` section

Wrong Directory Structure
--------------------------

**Error**: Job can't find files in expected locations

**Diagnosis**: Verify directory structure matches job expectations

**Solution**: Ensure ``mkdir`` entries create all necessary directories:

.. code-block:: yaml

    input_files:
        mkdir:
            - {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/input
            - {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/history
            - {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc_offset }}/model/ocean/restart  # Previous cycle!

Cycle Offset Issues
-------------------

**Error**: Job expects files from 06Z but only 12Z directories exist

**Diagnosis**: Missing previous cycle data for coupled forecasts

**Solution**: Use offset variables for previous cycle data:

.. code-block:: yaml

    {% set H_offset = '-6H' %}
    {% set TEST_DATE_offset = TEST_DATE + H_offset %}
    {% set cyc_offset = TEST_DATE_offset | strftime('%H') %}
    
    # Stage files using cyc_offset for previous cycle paths
    - {{ SRC_DIR }}/gfs.{{ PDY }}/{{ cyc_offset }}/model/ocean/restart/...

Checksum Validation Failures
-----------------------------

**Error**: Validation fails with checksum mismatch

**Diagnosis**: Output differs from baseline (could be expected or error)

**Solutions**:

1. Use ``CTEST_VALIDATION_MODE=PRESENCE_ONLY`` to skip checksum validation during development
2. Investigate output differences to determine if they are acceptable
3. Update baseline if changes are intentional
4. Fix code if differences are unintended

Missing Baseline Data
---------------------

**Error**: STAGED_CTESTS directory or PSLOT not found

**Diagnosis**: Nightly baseline runs incomplete or configuration incorrect

**Solution**: 

1. Verify ``STAGED_CTESTS`` path in ``config.$MACHINE_ID``
2. Check nightly CI runs completed successfully
3. Confirm ``PSLOT`` naming matches baseline case names
4. Wait for nightly runs to complete before running tests

HPC Account Issues
------------------

**Error**: Job submission fails with account validation error

**Diagnosis**: Incorrect or expired HPC allocation

**Solution**:

.. code-block:: bash

    # Set correct account
    cmake -DHPC_ACCOUNT=correct_account ../..
    
    # Or export before cmake
    export HPC_ACCOUNT=correct_account
    cmake ../..

Debugging Strategies
====================

**Enable Verbose Logging**:

.. code-block:: bash

    # Set logging level
    export LOGGING_LEVEL=DEBUG
    ctest -R test_name -VV

**Inspect Test Artifacts**:

.. code-block:: bash

    # Test experiment directory
    cd ${RUNTESTS}/COMROOT/${TEST_NAME}_${HASH}
    
    # Check logs
    tail -f EXPDIR/logs/test_name.log

**Manual Test Execution**:

.. code-block:: bash

    # Run test phases manually
    cd $HOMEgfs/dev/ctests/build/scripts
    ./setup.sh TEST_NAME CASE_YAML TEST_DATE
    ./stage.sh CASE_NAME TEST_NAME TEST_DATE
    ./execute.sh TEST_NAME JOB_NAME TEST_DATE
    ./validate.sh CASE_NAME TEST_NAME TEST_DATE

**Compare YAML Rendering**:

.. code-block:: python

    # Test YAML parsing
    from wxflow import parse_j2yaml, to_datetime
    
    data = {
        'TEST_DATE': to_datetime('2021032312'),
        'STAGED_CTESTS': '/path/to/baselines',
        'RUNTESTS': '/path/to/tests',
        'PSLOT': 'C48_ATM_baseline',
        'TEST_NAME': 'C48_ATM-gfs_fcst_seg0_hash'
    }
    
    config = parse_j2yaml('cases/C48_ATM-gfs_fcst_seg0.yaml', data)
    print(config)

****************************
Additional Resources
****************************

Related Documentation
=====================

* :doc:`development` - Contributing guidelines and development workflow
* :doc:`jobs` - Job descriptions and workflow structure
* :doc:`configure` - Configuration switches and options

Directory Reference
===================

**Key Directories**:

* ``$HOMEgfs/dev/ctests/`` - CTest framework root
* ``$HOMEgfs/dev/ctests/cases/`` - YAML test case definitions
* ``$HOMEgfs/dev/ctests/build/`` - CMake build directory
* ``${STAGED_CTESTS}/COMROOT/`` - Stable baseline outputs
* ``${RUNTESTS}/COMROOT/`` - Test execution environments
* ``$HOMEgfs/jobs/JGLOBAL_*`` - Production job scripts

**Configuration Files**:

* ``$HOMEgfs/dev/ci/platforms/config.$MACHINE_ID`` - Platform settings
* ``$HOMEgfs/dev/ctests/CMakeLists.txt`` - Test definitions
* ``$HOMEgfs/dev/ci/gitlab-ci-hosts.yml`` - CI/CD pipeline

Development History
===================

* **Created**: January 2025
* **Last Updated**: October 2025
* **Framework Version**: 1.0
* **Status**: Active Development

The CTest framework continues to evolve with additional test cases, enhanced validation capabilities, and improved CI/CD integration. Contributions following the guidelines in :doc:`development` are welcome.
