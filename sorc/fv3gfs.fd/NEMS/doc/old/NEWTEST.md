<a name="new-system"></a>New Test System
========================================

The old regression test system has been replaced by a new system.  It
has a different design that the old one.  It has a superset of the
capabilities of the old system, but the different design leads to
advantages and disadvantages.

Presently, that implementation is available by the NEMS/tests/rtgen
script, and two scripts it generates (rtrun, rtreport).  For backward
compatibility, there is a wrapper "rt.sh" script to prevent users from
having to learn a new system if they are only running the regression
tests (not modifying them).

<a name="design"></a>Design and Capabilities
--------------------------------------------

This system works on a different principle than the older one.  The
old system ran shell scripts specific to each model or test which
copied files from outside the NEMS test area and ran external programs
to generate some inputs.

The new system has a directory of prepared inputs, has no external
dependencies, and simply runs the NEMS executable without any
test-specific scripts.  In other words, scripts like the
`exglobal_fcst_nems.sh` are no longer used.  This makes porting and
workflow changes simpler, but has the disadvantage of not testing
model workflow scripts.  That disadvantage is intentional; the purpose
of the NEMS regression tests is to test the NEMS, not model workflow
scripts.

<a name="running"></a>Running the System
----------------------------------------

This section explains how to run the system in its simplest form.
Later sections discuss [running subsets of the tests](#run-sub),
[dependency resolution](#dep-res), and [available tests](#list-avail).
We provide two methods: a simple way using the rt.sh wrapper, and a
more complex way that provides complete control and flexibility.

### <a name="new-rtsh"></a>Simple Method: rt.sh

For backward compatibility, there is an rt.sh script that acts
similarly to the old rt.sh.  Some aspects are different to give extra
flexibility.

To execute in an sh-family shell (sh, bash, ksh, etc.)

    cd NEMS/tests
    ./rt.sh (options) > rt.log 2>&1 &

To execute in a csh-family shell (csh, tcsh):

    cd NEMS/tests
    ./rt.sh (options) >& rt.log &

This will run rt.sh in the background and send all output to the
`rt.log` file.  To see the success or failure information, look in the
`rt.log` file.

The `(options)` specify what is to be run.  Common needs are:

* `-f` = run the full test suite
* `-s` = only run the "standard" tests
* `-t setname` = run the specified set of tests.  See
  `compsets/all.input` for the full list.  Common names are `standard`,
  `gfs`, and `nmm`
* `-b setname` = regenerate the baseline.
* `-n /path/to/baseline` = specifies the location of the baseline
  when running the suite in verification or baseline generation modes.
* `-r PLATFORM:/path/to/rtgen.###` - used by the full test method.
  See below.
* `-p project` = set the project or account to use for CPU hours.
  If unspecified, one will be automatically picked based on 
  cpu availability.

### Full Test Method

The process of running is:

    ./NEMS/tests/rtgen   # generates rtrun and rtreport commands
    /path/to/USERNAME/rtgen.(ID)/rtrun (options)
    /path/to/USERNAME/rtgen.(ID)/rtreport

To use this for a commit to the trunk, one must copy the results to
the NEMS/tests directory.  This could be done manually, or one could
run rt.sh and tell it to skip the rtgen step.  To do this, use the
`rt.sh -r` option:

    ./rt.sh -r (PLATFORM):/path/to/USERNAME/rtgen.(ID)

where `(PLATFORM)` is "theia" or "wcoss.phase1"

The rest of this section explains the purpose and function of rtgen,
rtrun and rtreport.

### Step 1: Generate Test Scripts (rtgen)

The first step is to run `rtgen`.  This will generate a set of scripts
to run the requested tests.  If you do not request any tests, it will
run all tests.

    ./NEMS/tests/rtgen

That command will give you instructions and will log the more
important parts of its execution:

    11/17 18:42:38Z rtgen-INFO:  Will run all known tests.
    11/17 18:42:50Z rtgen-INFO:  Auto-chosen project for job submission is 'cmp'
    11/17 18:42:51Z rtgen-INFO:  Auto-chosen ptmp is '/path/to/USERNAME'
    11/17 18:42:51Z rtgen-INFO:  Generating workflow with id 23768.
    11/17 18:42:55Z rtgen-INFO:  Requested test has been generated.
    You need to run the test now.   You have three options:
    OPTION 1: Put this in your cron:
      */3 * * * * /path/to/USERNAME/rtgen.23768/rtrun --step --zero-exit \
         > /path/to/USERNAME/rtgen.23768/rtrun-cron.log 2>&1

    OPTION 2: Run this program:
      /path/to/USERNAME/rtgen.23768/rtrun --loop

    OPTION 3: Verbose mode: run this program:
      /path/to/USERNAME/rtgen.23768/rtrun) -v --loop
    Adding -n to that command will disable colors.

### Step 2: Run the Test (rtrun)

The rtrun command runs the tests until all have succeeded or failed.
You have three options for how to run this.  The easiest execution
option is number 3, which runs on the command line and reports the
queue status every few minutes.  The path to rtrun will vary, but the
command will look something like this:

    /path/to/USERNAME/rtgen.23768/rtrun -v --loop

If the colors annoy you, add the `-n` switch, and if you don't want
the queue state, remove the `-v` switch.

The components of that path are:

* `/path/to` - a scrub area, such as /scratch4/NCEPDEV/stmp4 or /ptmpp1
* `USERNAME` - your username, such as `emc.nemspara` or `Samuel.Trahan`

The `rtrun` command will generate output like this:

    11/17 00:19:21Z rtrun INFO: check dependencies and submit jobs...
    11/17 00:19:22Z rtrun INFO: check status...
    11/17 00:19:22Z rtrun INFO: workflow is still running and no jobs have failed.
    11/17 00:19:22Z rtrun INFO: sleep 2
    11/17 00:19:24Z rtrun INFO: get queue information
     Job ID  Reserv   Queue   Procs ST Queue Time  Stdout Location
    -------- ------ --------- ----- -- ----------- ------------------------------------
      573626        dev          64 R  11/17 00:14 /.../tmp/log/test_gfs_gocart_nemsio.log
    From bjobs -l  -u Samuel.Trahan (age 0 sec.)
    11/17 00:19:24Z rtrun INFO: sleep 100

It will keep looping until all jobs have succeeded or failed.  If all
goes well, the tests will all pass and you will see this message:

    11/17 00:21:04Z rtrun INFO: check dependencies and submit jobs...
    11/17 00:21:05Z rtrun INFO: check status...
    11/17 00:21:05Z rtrun INFO: workflow is complete and all jobs succeeded.

### Step 3: Report Results (rtreport)

At that point, you can run rtreport to get a report of the tests.
Actually, you can run rtreport at any time.  If the tests are not yet
complete, it will tell you which ones are complete.  It will report
all it knows about failed tests too.  There are two output formats:

To run:

    /path/to/USERNAME/rtgen.23768/rtreport [mode]

Where the optional `mode` is one of:

  * `status` - short output that only lists failed tests and counts
    the number of failed, complete, and unfinished tests.

  * `txt` - full text output of all information (the default).

The output of `txt` mode (the default) looks something like this

    BUILD nmm.x: SUCCEEDED
    BUILD nmm.debug.x: SUCCEEDED
    BUILD gsm.x: SUCCEEDED
    BUILD gsm_gocart.x: SUCCEEDED
    TEST #1: PASS
      Test nmm_cntrl starting.
      Wed Nov 16 22:51:23 UTC 2016
      .../REGRESSION_TEST/NMMB_glob/nmmb_hst_01_bin_0000h_00m_00.00s: bit-for-bit identical
      .../REGRESSION_TEST/NMMB_glob/nmmb_hst_01_bin_0024h_00m_00.00s: bit-for-bit identical
      .../REGRESSION_TEST/NMMB_glob/nmmb_hst_01_bin_0048h_00m_00.00s: bit-for-bit identical
      .../REGRESSION_TEST/NMMB_glob/nmmb_hst_01_nio_0000h_00m_00.00s: bit-for-bit identical
      .../REGRESSION_TEST/NMMB_glob/nmmb_hst_01_nio_0024h_00m_00.00s: bit-for-bit identical
      .../REGRESSION_TEST/NMMB_glob/nmmb_hst_01_nio_0048h_00m_00.00s: bit-for-bit identical
      .../REGRESSION_TEST/NMMB_glob/nmmb_rst_01_bin_0024h_00m_00.00s: bit-for-bit identical
      .../REGRESSION_TEST/NMMB_glob/nmmb_rst_01_nio_0024h_00m_00.00s: bit-for-bit identical
      TEST PASSED
    TEST #2: PASS
      Test nmm_nemsio starting.
    ... information about more tests ...


### <a name="rerun"></a>Rerunning Failed Tests

If a test fails, you can request that it be rerun via the `rtrewind`
command.  The command is located in the same directory as `rtrun`
and can be called in two different ways:

    /path/to/USERNAME/rtgen.23768/rtrewind -a

    /path/to/USERNAME/rtgen.23768/rtrewind job1 [job2 [...]]

The first method requests a rerun of ALL tests and builds while the
second requests only certain ones be rerun.

The jobs (`job1`, `job2`, ...) are the names from the test suite such
as `gsm.x` or `nmm_cntrl`.  You can optionally include `test_` or
`build_` before the name, as it is printed by the `rtreport` command.

### <a name="run-sub"></a>Running Subsets of the Test Suite

The test suite, as of this writing, has 48 tests and 5 build options.
Frequently, you only want to run a few of them.  The `rtgen` script
has a simple set arithmetic language for specifying what to run.  The
subsetting is done by the command line.  For example, to run all
standard nmm tests, you need to take the intersection of those two
sets of tests:

    ./NEMS/tests/rtgen 'inter(nmm,standard)'

The `rtgen` will generate a workflow to run just those tests.  

Other subsetting operations:

    union(nmm,wam)   # run all nmm and wam tests
    minus(gfs,wam)   # run all gsm (gfs) tests that are not wam tests
    {gfs_slg,nmm_cntrl}  # run the gfs_slg and nmm_cntrl tests

You can combine multiple operations:

    minus(inter(union(gfs,nmm),standard),{gfs_slg,nmm_cntrl})

That will ask rtgen to run all gsm (gfs) and nmm tests that are
standard tests, except for `gfs_slg` and `nmm_cntrl`.

Despite that, the rtgen will still run the gfs_slg test.  Why?
Dependency resolution.

### <a name="dep-res"></a>Dependency Resolution

Some tests have dependencies, and `rtgen` will resolve those
dependencies automatically, similar to how `make` works.  For example,
the `gfs_slg_rsthst` requires the `gfs_slg` to run first.  Output from
`gfs_slg` is used as input to `gfs_slg_rsthst`.  If you ask `rtgen` to
run `gfs_slg_rsthst` without running `gfs_slg`, it will see the
dependency and add `gfs_slg` to your list of tests.  The builds are
handled the same way.  The `gfs_slg` has a dependency on the build
`gsm.x`, and so `rtgen` will always add the `gsm.x` build if you
select the `gfs_slg` test.


### <a name="list-avail"></a>List of Available Tests and Sets

The configuration for `rtgen` is stored in the compsets/all.input file
in the app level repository.  This is where you specify the available
tests and sets of tests.

The top few lines of that file look like this

    load 'gsm.input'
    load 'nmm.input'
    run nmm_cntrl              @ nmm, standard, baseline, nmmglob
    run nmm_nemsio             @ nmm,                     nmmglob
    run nmm_rest               @ nmm,                     nmmglob
    ... many more "run" statements ...

The first two lines import the details of the test from other files.
The lines beginning with `run` specify a test to run and the sets it
belongs to.  The test must be one declared in the other file,
as discussed later in this document.

The list of sets after the @ sign are the ones recognized by the
[subsetting functionality of rtgen](#run-sub). 

Note that you can enable tests on only certain platforms by including
a comparison operator in the list of subsets:

    run gfs_slg_2thread        @ gfs, standard, baseline, slg, plat==wcoss.phase1

This line ensures the `gfs_slg_2thread` is only available on WCOSS Phase 1.



<a name="work-area"></a>Work Area Contents
------------------------------------------

Running the `rtgen` creates a directory in a scrub area which will
contain the generated scripting system, input and output files, logs,
and resource usage information.  This section documents those files
and directories.

Recall that running `rtgen` creates a directory with a name like this:

    /path/to/USERNAME/rtgen.23768

That directory contains the following:

* rtrun script

* rtreport script

* jobs directory

* scripts directory

* ush directory

* src directory

  * install.sh

  * uninstall.sh

* exec directory

* include directory

* rocoto directory

* com directory

* tmp directory

  * tmp/log directory

### Jobs, Scripts and Ush

These are the three tier NCEP workflow directories and have the usual
meanings:

* jobs - sets up the environment and passes control to the "scripts" level

* scripts - high-level logic for each test

* ush - low-level utility functions

For each test, there is one "jobs" directory file and one "scripts"
directory file.  The "scripts" directory and "jobs" directory are
populated by the tests blocks which will be discussed in great detail
in the [Test Description Language](#desc-lang) section.  They are
generated from the [test blocks](#new-tests).

### Src, Exec, and Include

The `src` directory does not contain source code.  Instead, it
contains two scripts that describe how to build or uninstall the
`NEMS.x`

* install.sh - knows how to build the NEMS.x based on the instructions
  in the [build blocks](#new-build) as explained in the [Test
  Description Language](#desc-lang) section in great detail.

* uninstall.sh - deletes the copies of `NEMS.x` and `modules.nems`
  created by install.sh.

The `install.sh` creates executables and modulefiles which are copied
into the `exec` and `include` directories.

* exec - one executable for each NEMS build

* include - one file for each NEMS build containing a sequence of
  of "module load" commands.  These commands will be run before
  executing the NEMS.x

### Rocoto Directory

The `rtgen` makes one file in the `rocoto` directory.  The `rtrun`
will create a second file.

* workflow.xml - the definition of the workflow generated by `rtgen`.
  This includes dependencies and resource requirements.  There is one
  shell command for each test or build.

* workflow.db - created by `rtrun`, this contains the Rocoto internal
  state information.

### Tmp and Logs

The `tmp` directory contains all logs and all execution directories
for each test.

* tmp/log/rocoto.log - log file from Rocoto.  Contains information about
  batch system events, such as job failures or job submissions.

* tmp/log/*.log - all other files contain logs about a test or build

* tmp/* - all other directories are work areas for tests.  They
  contain inputs and outputs from the NEMS.x

### Scripts rtrun and rtreport

These are discussed in earlier sections.  The scripts are generated
automatically by `rtgen`.  The `rtrun` runs Rocoto and the `rtreport`
scans the reports, combining them into one text file.

### COM directory

This directory contains one subdirectory for each test with all
verified files as described in a test's (criteria)[#criteria] block.
It also contains the "report.txt" file with the report of the test
success or failure.

<a name="desc-lang"></a>Test Description Language
-------------------------------------------------

This chapter discusses the language used by the `rtgen` tool to
describe regression tests and compsets.  The language consists of
"modules" which are simply a collection of variables and functions. A
module has a type: build, test, hash, etc.  A set of `run` commands
list which runnable modules should be executed.

### <a name="vardef"></a>Variable Definitions and Modules

The simplest type of module is a hash, which looks like this:

    nems_vars={
        atm_model='none'
        atm_petlist_bounds="-1 -1"
        ocn_model='none'
        ocn_petlist_bounds="-1 -1"
        ice_model='none'
        ice_petlist_bounds="-1 -1"
        med_model='nems'
        med_petlist_bounds="-1 -1"
        med_atm_coupling_interval_sec='-1'
        med_ocn_coupling_interval_sec='-1'
    }

In this example, we have declared a hash called `nems_vars` which
contains several variables, such as `atm_model` and
`atm_petlist_bounds`.  Later on, another module declaration can "use"
this module, to import its variables:

    nmm_vars_global={
        use plat%nmm_dflt
        use nems_vars
        use common_vars
        use nmm_vars
        use nmm_aliases
        use nmm_uncoupled
        GBRG="glob"
        CNTL_NAME='NMMB_glob'
    }

Values can include variable substitution, which uses a similar syntax
as shell, but with different escape characters:

    common_vars={
        THRD=1
        WLCLK=15
        GEFS_ENSEMBLE=0
        GEN_ENSEMBLE=0
        WRITE_DOPOST='.false.'
        POST_GRIBVERSION='grib1'
        CONF="@[plat%PARMnems]"
    }

Here, the `CONF` variable in the `common_vars` module has the value of
the `PARMnems` variable in the `plat` module.

### Strings

There are three ways of specifying a string:

* Double quotes: "... text here with @[VARIABLE] expansion ..."
* Single quotes: '... text here with no variable expansion ...'
* Block string:

        [[[multi-line string
        with @[VARIABLE] expansion ]]]

If you need to insert a literal @ into the string, you have three
options.  In these examples, we'll use the multi-line string format:

* [[[  @['this text is not expanded']   ]]]
* [[[  @["this text is not expanded"]  ]]]
* [[[ Simple literal @[@] ]]]

### <a name="embedscript"></a> Embedded Scripts

Most of the scripts required to run the tests are automatically
generated, but there are occasional instances when you need to specify
specific code.  This is done via `embed` blocks:

    embed bash nems_regtest_prep(RUNDIR,modules,CNTL) [[[
            mkdir -p "$RUNDIR" "$CNTL"
            cd @[RUNDIR]
            source "$modules"
            export MPI_TYPE_DEPTH=20
            export ESMF_RUNTIME_COMPLIANCECHECK=OFF:depth=4
    ]]]

In this example, we have embedded a bash script called
`nems_regtest_prep`.  

#### Embedded Script Variables: $ vs. @

In the example script, there are two methods of doing variable substitution:

* `@[RUNDIR]`
* `"$RUNDIR"`

They have slightly different meanings.  In the case of `@[RUNDIR]`,
the value of the `RUNDIR` variable is substituted directly in the
generated script.  If the variable contained any shell metacharacters,
those would be copied verbatim.  In the case of `$RUNDIR`, the bash
variable is used instead.  That variable's value is set before the
code in `nems_regtest_prep` is run.

Either approach is valid.  It is up to the user to decide which one to use.

### Platform Detection

The test suite needs to reconfigure certain aspects based on platform;
WCOSS vs. Theia vs. GAEA, etc.  This is done with `platform` blocks.
These are simply modules with a `detect` function.  After all
platforms are defined, an `autodetect` block selects between them.

Here is an example of a platform.  This is the one for Phase 1 of WCOSS.

    platform wcoss.phase1 {
        use wcoss.common
        CPU_ACCOUNT='NAM-T2O'
        pex='1'
        cores_per_node=32
        MPI='LSF'
        SHORT_TEST_QUEUE='&SHORTQ;'
        LONG_TEST_QUEUE='&LONGQ;'
        BUILD_QUEUE='&BUILDQ;'
    
        embed bash detect [[[
            # This function is used at PARSE TIME to detect whether we are
            # on WCOSS Phase 1.  It must be very fast and low resource
            # usage since the parser runs it.
            if [[ -d /usrx && -d /global && -e /etc/redhat-release && \
                  -e /etc/prod ]] ; then
                # We are on WCOSS Phase 1 or 2.
                if ( ! cat /proc/cpuinfo |grep 'processor.*32' ) ; then
                    # Fewer than 32 fake (hyperthreading) cpus, so Phase 1.
                    exit 0
                fi
            fi
            exit 1
        ]]]
        ... more wcoss stuff ...
    }

Note the `embed bash` block called `detect`.  This is the bash
function that is run to detect whether the script is running on WCOSS
Phase 1.

Once all platforms are defined, there is an autodetect block:

    autodetect plat (/ wcoss.phase1, theia /)

This will define the `plat` variable, which is a duplicate of either
`wcoss.phase1` or `theia`.

### <a name="new-build"></a> Build Definition

The `build` blocks define a method of building an executable.  They
must define three variables and a function:

* `NEMS.x` = path to the NEMS executable created by this build

* `modules.nems` = list of "module load" commands to execute before
   running the executable

* `target` = file to check to ensure the build succeeded; should be
   the same as the `NEMS.x` variable

* `build` = an `embed bash` function that builds the program.

Here is an example.  This builds the GOCART-capable standalone GSM in
the NEMSLegacy branch:

    build gsm_gocart.x {
        use plat
        NEMS.x="@[plat%EXECrt]/NEMS_gocart.x"
        modules.nems="@[plat%INCrt]/NEMS_gocart.x.modules"
        target="@[NEMS.x]"
        build=NEMSAppBuilder(NEMS.x="@[NEMS.x]",modules.nems="@[modules.nems]",
                             OPTS="app=GSM-GOCART")
    }

The NEMSAppBuilder function is declared elsewhere.  It is used by most
of the `build` definitions to avoid duplication.  That function looks
like this:

    embed bash NEMSAppBuilder(NEMS.x,modules.nems,OPTS)
    [[[
            mkdir -p "@[plat%EXECrt]" "@[plat%INCrt]"
            rm -f "@[NEMS.x]" "@[modules.nems]"
            cd @[plat%HOMEnems]
    
            # NOTE: Replace "rebuild" with "norebuild" to disable "gmake clean"
            ./NEMS/NEMSAppBuilder rebuild $OPTS
    
            cd @[plat%SRCnems]
            cp -fp ../exe/NEMS.x "@[NEMS.x]"
            cp -fp conf/modules.nems "@[modules.nems]"
    ]]]

Notice that the four variables we're passing from gsm_gocart.x%build
are in the definition line of NEMSAppBuilder:

    embed bash NEMSAppBuilder(NEMS.x,modules.nems,OPTS)
    ...
    build gsm_gocart.x {
        ...
        build=NEMSAppBuilder(NEMS.x="@[NEMS.x]",modules.nems="@[modules.nems]",
                             OPTS="app=GSM-GOCART")

### <a name="new-tests"></a>Tests

A test is a module that defines the following:

* dependencies - any other tests or builds that have to run first

* `prep` - a preparation step to run before anything else.  This is
  generally `mkdir`, `module` or `cd` commands.

* `input` - a `filter` block that provides a list of input files or
  directories and instructions on how to copy or filter them.  This is
  described below.

* `execute` - a `spawn` block that describes how to run the `NEMS.x`.
  This is also used to generate job cards to request the needed
  resources.

* `output` - criteria for validating the test output.  These are
  usually `criteria` blocks, described below.

This is the `test` block for the global nmm control.  Later text
describe the meaning of each part:

    # nmm_cntrl test
    test nmm_cntrl: nmm.x {
        use nmm_vars_global
    
        # Convenience variables:
        RUNDIR_ROOT="@[plat%TMPrt]"
        RUNDIR="@[RUNDIR_ROOT]/@[TEST_NAME]"
        TEST_DESCR="Compare NMMB-global results with previous trunk version"
        CNTL="@[plat%BASELINE]/@[CNTL_NAME]"      # Control baseline area
        TEST_IN="@[plat%INPUTS]/@[CNTL_NAME]"   # Test-specific input data
        COM="@[plat%COMrt]/@[TEST_NAME]"
    
        criteria output {
            #    NEMS.x output file --------- comparison - control file or dir
            "nmmb_hst_01_bin_0000h_00m_00.00s" .bitcmp. "@[CNTL]"
            "nmmb_hst_01_bin_0024h_00m_00.00s" .bitcmp. "@[CNTL]"
            "nmmb_hst_01_bin_0048h_00m_00.00s" .bitcmp. "@[CNTL]"
            "nmmb_hst_01_nio_0000h_00m_00.00s" .bitcmp. "@[CNTL]"
            "nmmb_hst_01_nio_0024h_00m_00.00s" .bitcmp. "@[CNTL]"
            "nmmb_hst_01_nio_0048h_00m_00.00s" .bitcmp. "@[CNTL]"
            "nmmb_rst_01_bin_0024h_00m_00.00s" .bitcmp. "@[CNTL]"
            "nmmb_rst_01_nio_0024h_00m_00.00s" .bitcmp. "@[CNTL]"
        }
    
        # The prep is run at the top of any job.  It should do such things
        # like making directories and loading modules.
        prep=nems_regtest_prep(
            RUNDIR="@[RUNDIR]",modules="@[nmm.x%modules.nems]",
            CNTL="@[CNTL]")
    
        # The execute step runs the program:
        spawn execute {
            { "@[nmm.x%NEMS.x]", ranks="@[TASKS]", threads="@[OpenMPThreads]" }
        }
    
        filters input {
            # work file         operation   input file
         "input_domain_01"        .copy. "@[TEST_IN]/test_input_nmmb_global"
         "input_domain_01_nemsio" .copy. "@[TEST_IN]/test_input_nmmb_global.nemsio"
         "GWD_bin_01"             .copy. "@[TEST_IN]/GWD_bin_01"
    
         "nems.configure"      .atparse. "@[CONF]/nems.configure.@[nems_configure].IN"
         "atmos.configure"     .atparse. "@[CONF]/atmos.configure_nmm"
    
         "configure_file_01"   .atparse. "@[CONF]/nmm_conf/nmm_@[GBRG]_conf.IN"
         "model_configure"        .copy. "configure_file_01"
    
         "*"                   .copydir. "@[plat%NMM_DATA]"
    
         "VEGPARM.TBL"            .copy. "IGBP_VEGPARM.TBL"
         "LANDUSE.TBL"            .copy. "IGBP_LANDUSE.TBL"
         "ETAMPNEW_DATA"          .copy. "ETAMPNEW_DATA.expanded_rain"
         "fort.28"                .link. "global_o3prdlos.f77"
         "fort.48"                .link. "global_o3clim.txt"
    
         "solver_state.txt"       .copy. "@[plat%PARMnems]/solver_state.txt"
         "nests.txt"              .copy. "@[plat%PARMnems]/nests.txt"
        }
    }
    
#### Test Dependencies

The first line (after the comment) is this:

    test nmm_cntrl: nmm.x {

The `: nmm.x` indicates that the `nmm.x` build has to run before the
`nmm_cntrl` can start.  The test suite will include that dependency in
its Rocoto or ecFlow automation system.

#### Test Prep

The prep step is a simple script that prepares the environment.  In
this case, it just runs the nems_regtest_prep, which we discussed
earlier:

        # The prep is run at the top of any job.  It should do such things
        # like making directories and loading modules.
        prep=nems_regtest_prep(
            RUNDIR="@[RUNDIR]",modules="@[nmm.x%modules.nems]",
            CNTL="@[CNTL]")

Note that it refers to `@[RUNDIR]` and `@[CNTL]`.  Those variables are
defined earlier in the same test:

        # Convenience variables:
        RUNDIR_ROOT="@[plat%TMPrt]"
        RUNDIR="@[RUNDIR_ROOT]/@[TEST_NAME]"
        TEST_DESCR="Compare NMMB-global results with previous trunk version"
        CNTL="@[plat%BASELINE]/@[CNTL_NAME]"      # Control baseline area
        TEST_IN="@[plat%INPUTS]/@[CNTL_NAME]"   # Test-specific input data
        COM="@[plat%COMrt]/@[TEST_NAME]"

#### Test Input Filter

This block specifies the input files and how to prepare them.  It
declares an `input` variable inside the `nmm_cntrl` test, which is of
type `filters`:

        filters input {
            # work file         operation   input file
         "input_domain_01"        .copy. "@[TEST_IN]/test_input_nmmb_global"
         "input_domain_01_nemsio" .copy. "@[TEST_IN]/test_input_nmmb_global.nemsio"
         "GWD_bin_01"             .copy. "@[TEST_IN]/GWD_bin_01"
    
         "nems.configure"      .atparse. "@[CONF]/nems.configure.@[nems_configure].IN"
         "atmos.configure"     .atparse. "@[CONF]/atmos.configure_nmm"
    
         "configure_file_01"   .atparse. "@[CONF]/nmm_conf/nmm_@[GBRG]_conf.IN"
         "model_configure"        .copy. "configure_file_01"
    
         "*"                   .copydir. "@[plat%NMM_DATA]"
    
         "VEGPARM.TBL"            .copy. "IGBP_VEGPARM.TBL"
         "LANDUSE.TBL"            .copy. "IGBP_LANDUSE.TBL"
         "ETAMPNEW_DATA"          .copy. "ETAMPNEW_DATA.expanded_rain"
         "fort.28"                .link. "global_o3prdlos.f77"
         "fort.48"                .link. "global_o3clim.txt"
    
         "solver_state.txt"       .copy. "@[plat%PARMnems]/solver_state.txt"
         "nests.txt"              .copy. "@[plat%PARMnems]/nests.txt"
        }

Notice that there are four different operations in the middle column:

| Local file          | Operation   | Remote file or directory        |  
| ------------------- | ----------- | ------------------------------- |
| `"GWD_bin_01"`      | `.copy.`    | `"@[TEST_IN]/GWD_bin_01"`       |
| `"*"`               | `.copydir.` | `"@[plat%NMM_DATA]"`            |
| `"fort.28"`         | `.link.`    | `"global_o3prdlos.f77"`         |
| `"atmos.configure"` | `.atparse.` | `"@[CONF]/atmos.configure_nmm"` |

* `.copy.` - copies the remote file (third column) to the local file
  (first column).  

        cp -p "$third_column" "$first_column"

* `.link.` - makes a symbolic link to the remote file (third column)
  from the local file (first column)

        ln -s "$third_column" "$first_column"

* `.copydir.` - copies from the remote file or directory (third
  column) all files that match the glob (first column) into the local
  directory.

        cp -rp "$third_column"/$first_column

* `.atparse.` - runs the remote file (third column) through a filter
  to create the local file (first column).  The filter will replace
  text like `@[varname]` with the corresponding variable.  

  In the `.atparse.` variable replacement, only variables from the
  test's module are replaced.  Hence, if you want many variables
  accessible to `.atparse.`d files, you need to either declare or
  `use` them.  The `nmm_cntrl` test does that at the top of its
  declaration:

        test nmm_cntrl: nmm.x {
            use nmm_vars_global
        
            # Convenience variables:
            RUNDIR_ROOT="@[plat%TMPrt]"
            RUNDIR="@[RUNDIR_ROOT]/@[TEST_NAME]"
            TEST_DESCR="Compare NMMB-global results with previous trunk version"
            CNTL="@[plat%BASELINE]/@[CNTL_NAME]"      # Control baseline area
            TEST_IN="@[plat%INPUTS]/@[CNTL_NAME]"   # Test-specific input data
            COM="@[plat%COMrt]/@[TEST_NAME]"

  Everything in the `nmm_vars_global` module will be available plus
  all six of the declared "convenience variables"

  Note that variables with a period (".") or percent ("%") in their
  name are not yet available.  That will be fixed in a later release.

#### Test Execution

The next step is to actually run the `NEMS.x`:

        # The execute step runs the program:
        spawn execute {
            { "@[nmm.x%NEMS.x]", ranks="@[TASKS]", threads="@[OpenMPThreads]" }
        }

The columns inside the `execute` block have these meanings:

* `"@[nmm.x%NEMS.x]"` - the program to run

* `ranks="@[TASKS]"` - number of mpi ranks

* `threads="@[OpenMPThreads]"` - optional; number of threads per rank.
  Default is 1.

* ppn=8 - not used here; optional.  Specifies the number of MPI ranks
  per node.  The GSM needs this due to memory limits.  Default is
  calculated automatically by the system, and will be the largest
  number of MPI ranks possible.

#### <a name="criteria"></a> Test Verification or Baseline Generation

The last step is to either verify the results or generate the
baseline.  Both cases are handled by the output criteria block:

    criteria output {
        #    NEMS.x output file --------- comparison - control file or dir
        "nmmb_hst_01_bin_0000h_00m_00.00s" .bitcmp. "@[CNTL]"
	"nmmb_hst_01_bin_0024h_00m_00.00s" .bitcmp. "@[CNTL]"
	"nmmb_hst_01_bin_0048h_00m_00.00s" .bitcmp. "@[CNTL]"
	"nmmb_hst_01_nio_0000h_00m_00.00s" .bitcmp. "@[CNTL]"
	"nmmb_hst_01_nio_0024h_00m_00.00s" .bitcmp. "@[CNTL]"
	"nmmb_hst_01_nio_0048h_00m_00.00s" .bitcmp. "@[CNTL]"
	"nmmb_rst_01_bin_0024h_00m_00.00s" .bitcmp. "@[CNTL]"
	"nmmb_rst_01_nio_0024h_00m_00.00s" .bitcmp. "@[CNTL]"
    }

The columns have this meaning:

* `"nmmb_hst_01_bin_0000h_00m_00.00s"` - local directory file

* `.bitcmp.` - verification method.  Only `.bitcmp.` is supported for now.

* `"@[CNTL]"` - remote directory file or remote directory that
  contains the baseline.  If it is a remote directory, the file is
  assumed to have the same name.

In verification mode, the comparisons are performed after running NEMS.x

In baseline generation mode, the local file (first column) is copied
to the remote location (third column).