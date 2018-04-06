Running: NEMSCompsetRun {#running}
=======================

NEMS uses "component sets", or "`compsets`," to systematically label
run configurations. The labels are associated with scripts that pull
together all the files and inputs needed to run the specified
configurations. Compset labels include which components and mediators
are part of the configuration, whether each component is running in a
prognostic or test mode, resolutions, and initial conditions. This
approach offers a number of benefits:

* standard runs can be set up easily and consistently

* it is a very effective way to implement regression testing across a
  coupled system with many possible combinations of components

* easy access to configurations with non-prognostic versions of
  components facilitates controlled experimentation

Compsets were originated by the [Community Earth System Model (CESM)](http://www.cesm.ucar.edu/models/cesm1.2/cesm/doc/modelnl/compsets.html).

[Supported NEMS compsets are listed here](https://docs.google.com/spreadsheets/d/1v9tJb03YuCbwDsXff4M5i6jz4lvBxUrhdImqaGnK_IE/edit#gid=0).

Old and New NEMSCompsetRun
--------------------------

The NEMS is transitioning from an old compset runner to a new one.
The old system was based on model-specific workflows that duplicated
one another's fuctionality.  The new one is a more generic tool which
uses an abstract description to generate the workflow.  Both systems
are explained in this document - older repository revisions referred
to by the \ref milestones still require use of the older
NEMSCompsetRun.

In both systems, the compset front-end is at:

    ./NEMS/NEMSCompsetRun [arguments]

Executing this front-end will start one or more compsets, potentially
in parallel, and eventually report the results.  What varies between
the old and new systems is the calling convention, capabilities and
internals.

### Comparison of compset runners

| Capability                     | Old runner | New runner |
| ------------------------------ | ---------- | ---------- |
| Editable shell scripts         | yes        | no         |
| Connection to build system     | no         | yes        |
| Parallel execution of compsets | no         | yes        |
| Re-running failed compset      | no         | yes        |

New NEMSCompsetRun
------------------

This system generates workflow scripts from an abstract description of
compsets.  Users can specify multiple compsets either by name or by
grouping.  The system allows basic set arithmetic operations to
combine several group of compsets into a single workflow.  Any
dependencies are automatically resolved, including compilation of NEMS
executables.  The generated workflow is run by the Rocoto workflow
manager.  That manager allows failed compsets or builds to be rerun
manually, or automatically.

Design and Capabilities
------------------------

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

Running the System
------------------

This section explains how to run the system in its simplest form.
Later sections discuss how to check the status of the execution, rerun
a failed job, or resume a workflow (such as after logging out).

### Initial Execution

The first step is to start the workflow:

    ./NEMS/NEMSCompsetRun [options] [run-mode] compset-specification

Options will be discussed later.  The `run-mode` determines whether
the system will create a new baseline, or execute and verify against
the old baseline.  To generate a new baseline:

    ./NEMS/NEMSCompsetRun [options] --make-baseline compset-specification

Without that `--make-baseline` flag, the system will run in the
default mode: execute the tests and verify against the baseline.

The compset specification can be done in the following ways.  These
select either specific compsets or groups of compsets.  We will
discuss how compset groups work, in the next section.

 * `--make-baseline` -- run all tests that are part of the baseline
 * `-f` -- run all known tests
 * `-s` -- run tests in the "standard" group
 * `--compset (compset-name)` -- run the specified compset
 * `group-name` -- run the specified group of compsets
 * `set-specification` -- set arithmetic that specifies the compsets
   to run.  This is discussed in detail in the next section.

### Compset Groups

The NEMSCompsetRun can group together several compsets, to allow users
to run multiple compsets without having to type each one.  Compset
groups are defined in the `compsets/all.input`.  As of this writing,
the `all.input` for the NEMSGSM app was the following:

    load 'gsm.input'
    
    run gfs_eulerian           @ gfs, standard, baseline
    run wam_gh_l150            @ gfs, standard, baseline, wam
    run wam_gh_l150_nemsio     @ gfs, standard, baseline, wam
    run gfs_slg                @ gfs, standard, baseline, slg
    run gfs_slg_2thread        @ gfs, standard, baseline, slg, plat==wcoss.phase1
    run gfs_slg_48pe           @ gfs, standard, baseline, slg
    run gfs_slg_adiabatic      @ gfs, standard, baseline, slg
    run gfs_slg_land           @ gfs, standard, baseline, slg
    run gfs_slg_nsst           @ gfs, standard, baseline, slg
    run gfs_slg_rsthst         @ gfs, standard, baseline, slg
    run gfs_slg_stochy         @ gfs, standard, baseline, slg
    run gfs_slg_t574           @ gfs, standard, baseline, slg
    run gfs_gocart_nemsio      @ gfs, standard, baseline, gocart

The `run` lines list known compsets and which groups they belong to.
In later sections, we will discuss the `load 'gsm.input'` and how
compsets are defined.  The text between `run` and `@` is a compset
name, while after the `@` lists the groups to which the compset
belongs.  Let's examine these three:

    run gfs_eulerian           @ gfs, standard, baseline
    run wam_gh_l150            @ gfs, standard, baseline, wam
    run wam_gh_l150_nemsio     @ gfs, standard, baseline, wam

All three are part of the `gfs` group, as well as the `standard` and
`baseline` groups.  Only the `wam_gh_l150` and `wam_gh_l150_nemsio`
are part of the `wam` group.  Hence, if we ask to run the `wam` group
of compsets, the `gfs_eulerian` will not be run.  If we ask to run the
`gfs` group, all three will be run.

There are two special compset groups:

 * `standard` -- all compsets run by `NEMSCompsetRun -s`
 * `baseline` -- all compsets run when a baseline is generated.

The `baseline` set allows a special usage: some compsets are intended
to generate the same results as one another.  This is often used for
regression testing, such as ensuring that different decompositions of
a grid generate the same results.  In such situations, you only want
to run one control when generating baselines.  

### Compset Groups and Set Arithmetic

The NEMSCompsetRun understands basic set arithmetic operations.  For
example, this requests execution of all `gocart` and `wam` compsets:

    ./NEMS/NEMSCompsetRun 'union(gocart,wam)'

You can also specify lists of compsets:

    ./NEMS/NEMSCompsetRun '{gfs_slg_t574,gfs_slg}'

and use such compsets in set arithmetic.  For example, if we want to
run all GFS semilagrangian tests except `gfs_slg` and `gfs_slg_t574`:

    ./NEMS/NEMSCompsetRun 'minus(slg,{gfs_slg_t574,gfs_slg})'

The set arithmetic understands the following:

 * `union(group1,group2)` --- union of group1 and group2
 * `inter(group1,group2)` --- intersection of group1 and group2
 * `minus(group1,group2)` --- all compsets in group1 that are not in group2
 * `*` --- the universal set (all known compsets)
 * `{compset1,compset2,compset3}` --- compsets compset1, compset2, and compset3

In all cases, explicit compset sets (`{compset1,compset2,compset3}`)
can be used in place of groups.  Also, nested operations are allowed:

    ./NEMS/NEMSCompsetRun minus(*,union(minus(slg,{gfs_slg_t574,gfs_slg}),gocart))

### Options: Disk Area, Batch System Account, and More

The NEMSCompsetRun has a number of command-line options for
configuring various aspects of execution.  There is one special
option:

    ./NEMS/NEMSCompsetRun --help

which will print a usage message, describing options and compset
specification syntax.

#### Resources

The following will be given suitable defaults, if possible, on your
platform.  In some cases, no default is possible.  For example, Jet
has no system-wide temporary area so the user must specify that
manually.

 * `--project=project` --- specifies the CPU project or account under
   which jobs will be submitted.

 * `--temp-dir=/path/to/temp/area` --- scrub area in which to generate
   the workflow and execute the compsets.  The NEMSCompsetRun will
   make a subdirectory, `rtgen.(number)`, underneath this directory.

#### Generation and Execution

 * `--baseline-dir` --- the location of the baseline.  In baseline
   generation mode, this is the directory in which to place the
   baseline.  In execution mode, this is the area that contains the
   baseline against which to compare runs.  The default is specified
   in the compset platform description, discussed in detail in a later
   section.

 * `--dry-run` --- don't run anything.  Just print what would be done.

 * `--verbose` --- be verbose

 * `--mode=MODE` --- run in `BASELINE` mode or verify against a
   baseline in `EXECUTION` mode

 * `--baseline` --- generate a baseline; same as `--mode=BASELINE`

 * `--resume` --- continue execution of a terminated workflow.  This
   is discussed in detail in the next section.

### Resuming and Rerunning

The NEMSCompsetRun supports rerunning failed jobs and resuming a
terminated workflow.  If a user logs out or kills (control-C) the
NEMSCompsetRun process, it can be resumed with the `--resume` option.
If a compset or build job has failed, the user can rerun the failed
job via the `rtrewind` command, and then use the `NEMSCompsetRun
--resume` to resume the workflow.  The `rtreport` command reports on
the status of the build and compset jobs.

All of these commands require the path to the generated workflow.
That path is printed out by NEMSCompsetRun:

    /home/you>  ./NEMS/NEMSCompsetRun [options] compset-specificaton
    ... other log messages ...
    Test will run in: /path/to/temp-dir/rtgen.NUMBER

#### Resuming the Workflow

After the NEMSCompsetRun is terminated, you can resume the workflow by
starting a new NEMSCompsetRun for the same directory:

    ./NEMS/NEMSCompsetRun --resume /path/to/temp-dir/rtgen.NUMBER

Note that it is critical never to run two NEMSCompsetRun processes for
the same workflow at the same time.  Although it will usually work,
there is a chance that a batch job or Rocoto will fail when its script
is being rewritten.

#### Getting the Status of a Workflow

The `rtreport` program displays a short or long status message.  The
long format is the default.  It prints the status of all compsets and
builds, and detailed verification information:

    /path/to/temp-dir/rtgen.NUMBER/rtreport

The short message just reports the number of jobs completed or failed,
and explicitly lists failed jobs:

    /path/to/temp-dir/rtgen.NUMBER/rtreport status

#### Rerunning Failed Jobs

The first step to rerunning failed jobs is to figure out which jobs
have failed:

    /path/to/temp-dir/rtgen.NUMBER/rtreport status

This will print lines like the following:

    build_gsm-ww3.exe: FAIL: /path/to/temp-dir/rtgen.NUMBER/tmp/log/build_gsm-ww3.log

The `build_gsm-ww3.exe` is the name of the failed job.  The `rtrewind`
command will tell the NEMSCompsetRun's "Rocoto" tool to forget it ran
a job, so that it will rerun later:

    /path/to/temp-dir/rtgen.NUMBER/rtrewind build_gsm-ww3.exe

\note You do NOT need to terminate NEMSCompsetRun when running
`rtrewind`.  Rocoto has proper file locking mechanisms to allow it to
safely rewind a running workflow.

If you do terminate NEMSCompsetRun, then you must resume it, as
discussed earlier.


### Interpreting the Log Messages

After running the NEMSCompsetRun, you will see log messages like the following:

    11/17 18:42:38Z rtgen-INFO:  Will run all known tests.
    11/17 18:42:50Z rtgen-INFO:  Auto-chosen project for job submission is 'cmp'
    11/17 18:42:51Z rtgen-INFO:  Auto-chosen ptmp is '/path/to/USERNAME'
    11/17 18:42:51Z rtgen-INFO:  Generating workflow with id 23768.
    11/17 18:42:55Z rtgen-INFO:  Requested test has been generated.
    11/17 18:42:55Z rtgen-INFO:  Test will run in: /path/to/USERNAME/rtgen.23768

These messages include timestamps, and a warning level.  In this case,
nothing went wrong, so the level is `INFO`.  These first few messages
contain critical information:

 * `Will run all known tests` --- describes the tests to be run.  If
   you use options like `-s` or `union(wam,gocart)` then this will be
   stated.

 * `... project for job submission is 'cmp'` --- the accounting
   project used for submission to the batch queue.  This can be
   manually specified with the `--project` option.  

 * `... ptmp is '/path/to/USERNAME'` --- scrub area.  This can be
   manually specified with the `--temp-dir` option.

 * `Test will run in: /path/...` --- location in which the test is generated.

During the bulk of the execution, the `NEMSCompsetRun` will generate
output like this:

    11/17 00:19:21Z rtrun INFO: check dependencies and submit jobs...
    11/17 00:19:22Z rtrun INFO: check status...
    11/17 00:19:22Z rtrun INFO: workflow is still running and no jobs have failed.
    11/17 00:19:22Z rtrun INFO: sleep 2
    11/17 00:19:24Z rtrun INFO: get queue information
    573626        dev          64 R  11/17 00:14 /.../tmp/log/test_gfs_gocart_nemsio.log

It will keep looping until all jobs have succeeded or failed.  If all
goes well, the tests will all pass and you will see this message:

    11/17 00:21:04Z rtrun INFO: check dependencies and submit jobs...
    11/17 00:21:05Z rtrun INFO: check status...
    11/17 00:21:05Z rtrun INFO: workflow is complete and all jobs succeeded.






Work Area Contents
------------------

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

* scripts - high-level logic for each compset

* ush - low-level utility functions

For each compset, there is one "jobs" directory file and one "scripts"
directory file.  The "scripts" directory and "jobs" directory are
populated by the compset blocks which will be discussed in great detail
in the \ref desc_lang "Compset Description Language" section.  They are
generated from the [compset blocks](new-compsets).

### Src, Exec, and Include

The `src` directory does not contain source code.  Instead, it
contains two scripts that describe how to build or uninstall the
`NEMS.x`

* install.sh - knows how to build the NEMS.x based on the instructions
in the [build blocks](new-build) as explained in the
[Compset Description Language](desc_lang) section in great detail.

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
shell command for each compset or build.

* workflow.db - created by `rtrun`, this contains the Rocoto internal
state information.

### Tmp and Logs

The `tmp` directory contains all logs and all execution directories
for each compset.

* `tmp/log/rocoto.log` - log file from Rocoto.  Contains information about
batch system events, such as job failures or job submissions.

* `tmp/log/*.log` - all other files contain logs about a compset or build

* `tmp/*` - all other directories are work areas for compsets.  They
contain inputs and outputs from the NEMS.x

### Scripts rtrun and rtreport

These are discussed in earlier sections.  The scripts are generated
automatically by `rtgen`.  The `rtrun` runs Rocoto and the `rtreport`
scans the reports, combining them into one text file.

### COM directory

This directory contains one subdirectory for each compset with all
verified files as described in a compset's \ref criteria block.
It also contains the "report.txt" file with the report of the compset
success or failure.

\anchor desc_lang
5.4 Compset Description Language
--------------------------------

This chapter discusses the language used by the `rtgen` tool to
describe regression tests and compsets.  The language consists of
"modules" which are simply a collection of variables and functions. A
module has a type: build, compset, hash, etc.  A set of `run` commands
list which runnable modules should be executed.

### Variable Definitions and Modules

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

* `[[[  @['this text is not expanded']   ]]]`
* `[[[  @["this text is not expanded"]  ]]]`
* `[[[ Simple literal @[@] ]]]`

###  Embedded Scripts

Most of the scripts required to run the compsets are automatically
generated, but there are occasional instances when you need to specify
specific code.  This is done via `embed` blocks:

    embed bash nems_compset_prep(RUNDIR,modules,CNTL) [[[
    mkdir -p "$RUNDIR" "$CNTL"
    cd @[RUNDIR]
    source "$modules"
    export MPI_TYPE_DEPTH=20
    export ESMF_RUNTIME_COMPLIANCECHECK=OFF:depth=4
    ]]]

In this example, we have embedded a bash script called
`nems_compset_prep`.  

#### Embedded Script Variables: `$` vs. `@`

In the example script, there are two methods of doing variable substitution:

* `@[RUNDIR]`
* `"$RUNDIR"`

They have slightly different meanings.  In the case of `@[RUNDIR]`,
the value of the `RUNDIR` variable is substituted directly in the
generated script.  If the variable contained any shell metacharacters,
those would be copied verbatim.  In the case of `$RUNDIR`, the bash
variable is used instead.  That variable's value is set before the
code in `nems_compset_prep` is run.

Either approach is valid.  It is up to the user to decide which one to use.

### Platform Detection

The compset suite needs to reconfigure certain aspects based on platform;
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
            if [[ -d /usrx && -d /global && -e /etc/redhat-release && \\
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

###  Build Definition

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

Notice that the four variables we're passing from `gsm_gocart.x%build`
are in the definition line of NEMSAppBuilder:

    embed bash NEMSAppBuilder(NEMS.x,modules.nems,OPTS)
    ...
    build gsm_gocart.x {
    ...
    build=NEMSAppBuilder(NEMS.x="@[NEMS.x]",modules.nems="@[modules.nems]",
    OPTS="app=GSM-GOCART")

### Compsets

A compset block is a module that defines the following:

* dependencies - any other compsets or builds that have to run first

* `prep` - a preparation step to run before anything else.  This is
generally `mkdir`, `module` or `cd` commands.

* `input` - a `filter` block that provides a list of input files or
directories and instructions on how to copy or filter them.  This is
described below.

* `execute` - a `spawn` block that describes how to run the `NEMS.x`.
This is also used to generate job cards to request the needed
resources.

* `output` - criteria for validating the compset output.  These are
usually `criteria` blocks, described below.

This is the `compset` block for the global nmm control.  Later text
describe the meaning of each part:

    # nmm_cntrl compset
    compset nmm_cntrl: nmm.x {
        use nmm_vars_global
    
        # Convenience variables:
        RUNDIR_ROOT="@[plat%TMPrt]"
        RUNDIR="@[RUNDIR_ROOT]/@[TEST_NAME]"
        TEST_DESCR="Compare NMMB-global results with previous trunk version"
        CNTL="@[plat%BASELINE]/@[CNTL_NAME]"      # Control baseline area
        TEST_IN="@[plat%INPUTS]/@[CNTL_NAME]"   # Compset-specific input data
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
        prep=nems_compset_prep(
        RUNDIR="@[RUNDIR]",modules="@[nmm.x%modules.nems]",
        CNTL="@[CNTL]")
    
        # The execute step runs the program:
        spawn execute {
            { "@[nmm.x%NEMS.x]", ranks="@[TASKS]", threads="@[OpenMPThreads]" }
        }
    
        filters input {
            # work file         operation   input file
            "input_domain_01"        <=copy= "@[TEST_IN]/test_input_nmmb_global"
            "input_domain_01_nemsio" <=copy= "@[TEST_IN]/test_input_nmmb_global.nemsio"
            "GWD_bin_01"             <=copy= "@[TEST_IN]/GWD_bin_01"
        
            "nems.configure"      <=atparse= "@[CONF]/nems.configure.@[nems_configure].IN"
            "atmos.configure"     <=atparse= "@[CONF]/atmos.configure_nmm"
        
            "configure_file_01"   <=atparse= "@[CONF]/nmm_conf/nmm_@[GBRG]_conf.IN"
            "model_configure"        <=copy= "configure_file_01"
        
            "*"                   <=copydir= "@[plat%NMM_DATA]"
        
            "VEGPARM.TBL"            <=copy= "IGBP_VEGPARM.TBL"
            "LANDUSE.TBL"            <=copy= "IGBP_LANDUSE.TBL"
            "ETAMPNEW_DATA"          <=copy= "ETAMPNEW_DATA.expanded_rain"
            "fort.28"                <=link= "global_o3prdlos.f77"
            "fort.48"                <=link= "global_o3clim.txt"
        
            "solver_state.txt"       <=copy= "@[plat%PARMnems]/solver_state.txt"
            "nests.txt"              <=copy= "@[plat%PARMnems]/nests.txt"
        }
    }

#### Compset Dependencies

The first line (after the comment) is this:

    compset nmm_cntrl: nmm.x {

The `: nmm.x` indicates that the `nmm.x` build has to run before the
`nmm_cntrl` can start.  The test suite will include that dependency in
its Rocoto automation system.

#### Compset Prep

The prep step is a simple script that prepares the environment.  In
this case, it just runs the nems_compset_prep, which we discussed
earlier:

    # The prep is run at the top of any job.  It should do such things
    # like making directories and loading modules.
    prep=nems_compset_prep(
        RUNDIR="@[RUNDIR]",modules="@[nmm.x%modules.nems]",
        CNTL="@[CNTL]")

Note that it refers to `@[RUNDIR]` and `@[CNTL]`.  Those variables are
defined earlier in the same compset:

    # Convenience variables:
    RUNDIR_ROOT="@[plat%TMPrt]"
    RUNDIR="@[RUNDIR_ROOT]/@[TEST_NAME]"
    TEST_DESCR="Compare NMMB-global results with previous trunk version"
    CNTL="@[plat%BASELINE]/@[CNTL_NAME]"      # Control baseline area
    TEST_IN="@[plat%INPUTS]/@[CNTL_NAME]"   # Test-specific input data
    COM="@[plat%COMrt]/@[TEST_NAME]"

#### Compset Input Filter

This block specifies the input files and how to prepare them.  It
declares an `input` variable inside the `nmm_cntrl` compset, which is of
type `filters`:

    filters input {
        # work file           operation  input file
        "input_domain_01"        <=copy= "@[TEST_IN]/test_input_nmmb_global"
        "input_domain_01_nemsio" <=copy= "@[TEST_IN]/test_input_nmmb_global.nemsio"
        "GWD_bin_01"             <=copy= "@[TEST_IN]/GWD_bin_01"
    
        "nems.configure"      <=atparse= "@[CONF]/nems.configure.@[nems_configure].IN"
        "atmos.configure"     <=atparse= "@[CONF]/atmos.configure_nmm"
    
        "configure_file_01"   <=atparse= "@[CONF]/nmm_conf/nmm_@[GBRG]_conf.IN"
        "model_configure"        <=copy= "configure_file_01"
    
        "*"                   <=copydir= "@[plat%NMM_DATA]"
    
        "VEGPARM.TBL"            <=copy= "IGBP_VEGPARM.TBL"
        "LANDUSE.TBL"            <=copy= "IGBP_LANDUSE.TBL"
        "ETAMPNEW_DATA"          <=copy= "ETAMPNEW_DATA.expanded_rain"
        "fort.28"                <=link= "global_o3prdlos.f77"
        "fort.48"                <=link= "global_o3clim.txt"
    
        "solver_state.txt"       <=copy= "@[plat%PARMnems]/solver_state.txt"
        "nests.txt"              <=copy= "@[plat%PARMnems]/nests.txt"
    }

Notice that there are four different operations in the middle column:

| Local file          | Operation    | Remote file or directory        |  
| ------------------- | ------------ | ------------------------------- |
| `"GWD_bin_01"`      | `<=copy=`    | `"@[TEST_IN]/GWD_bin_01"`       |
| `"*"`               | `<=copydir=` | `"@[plat%NMM_DATA]"`            |
| `"fort.28"`         | `<=link=`    | `"global_o3prdlos.f77"`         |
| `"atmos.configure"` | `<=atparse=` | `"@[CONF]/atmos.configure_nmm"` |

* `<=copy=` - copies the remote file (third column) to the local file
(first column).  

    cp -p "$third_column" "$first_column"

* `<=link=` - makes a symbolic link to the remote file (third column)
from the local file (first column)

    ln -s "$third_column" "$first_column"

* `<=copydir=` - copies from the remote file or directory (third
column) all files that match the glob (first column) into the local
directory.

    cp -rp "$third_column"/$first_column

* `<=atparse=` - runs the remote file (third column) through a filter
to create the local file (first column).  The filter will replace
text like `@[varname]` with the corresponding variable.  

In the `<=atparse=` variable replacement, only variables from the
compset's module are replaced.  Hence, if you want many variables
accessible to `<=atparse=`d files, you need to either declare or
`use` them.  The `nmm_cntrl` compset does that at the top of its
declaration:

    compset nmm_cntrl: nmm.x {
    use nmm_vars_global

    # Convenience variables:
    RUNDIR_ROOT="@[plat%TMPrt]"
    RUNDIR="@[RUNDIR_ROOT]/@[TEST_NAME]"
    TEST_DESCR="Compare NMMB-global results with previous trunk version"
    CNTL="@[plat%BASELINE]/@[CNTL_NAME]"      # Control baseline area
    TEST_IN="@[plat%INPUTS]/@[CNTL_NAME]"   # Compset-specific input data
    COM="@[plat%COMrt]/@[TEST_NAME]"

Everything in the `nmm_vars_global` module will be available plus
all six of the declared "convenience variables"

Note that variables with a period (".") or percent ("%") in their
name are not yet available.  That will be fixed in a later release.

#### Compset Execution

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

\anchor criteria
#### Compset Verification or Baseline Generation

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

### List of Available Compsets and Sets

The configuration for `rtgen` is stored in the compsets/all.input file
in the app level repository.  This is where you specify the available
compsets and sets of compsets.

The top few lines of that file look like this

    load 'gsm.input'
    load 'nmm.input'
    run nmm_cntrl              @ nmm, standard, baseline, nmmglob
    run nmm_nemsio             @ nmm,                     nmmglob
    run nmm_rest               @ nmm,                     nmmglob
    ... many more "run" statements ...

The first two lines import the details of the compset from other files.
The lines beginning with `run` specify a compset to run and the sets it
belongs to.  The compset must be one declared in the other file,
as discussed later in this document.

The list of sets after the @ sign are the ones recognized by the
\ref run_sub "subsetting functionality of rtgen".

Note that you can enable compsets on only certain platforms by including
a comparison operator in the list of subsets:

run gfs_slg_2thread        @ gfs, standard, baseline, slg, plat==wcoss.phase1

This line ensures the `gfs_slg_2thread` is only available on WCOSS Phase 1.
