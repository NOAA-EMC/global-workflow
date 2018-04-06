Old Regression Test System
==========================

This section documents the old rt.sh system, which has been replaced.
This system is no longer supported, but has been retained for backward
compatibility.  Furthermore, the current NEMSCompsetRun requires this
script.  The NEMSCompsetRun will be updated shortly, after some
modulefile changes are made in NEMS.

Running rt.sh
-------------

The older regression test system is run as follows in bash, sh, or ksh:

    cd NEMS/oldtests
    ./rt.sh (command) (options) > rt.log 2>&1 &

In csh or tcsh, do this:

    cd NEMS/oldtests
    ./rt.sh (command) (options) >& rt.log &

This will launch a background process that runs the `rt.sh` and logs
the status to `rt.log`.

The `(command)` must include at least one of the following, which
specify what is being run:

* `-f` - run all tests that work on your platform
* `-s` - run "standard" tests that work on your platform
* `-c gfs` - create a new baseline for the gsm, wam and gocart tests
* `-c nmm` - create a new baseline for nmm tests

In addition, the following `(option)`s work:

* `-m` - compare against user's baseline
* `-l FILE` - use an alternate file instead of `rt.conf`

The `>rt.log2>&` or `>&rt.log` are redirection operators in your
shell.  They will log additional information from `rt.sh` to `rt.log`.
This is critical when debugging problems such as batch system failures
or disk quota problems.

Output of rt.sh
---------------

The rt.sh produces several log files and a directory of results.

* `RegressionTests_(platform).log` - regression test results
* `Compile_(platform).log` - compilation logs
* `rt.log` - debug output of rt.sh if you used the redirection operators
* `/path/to/stmp/$USER/rt.$$/` - directory in which the tests were run

In these paths,

* `(platform)` - "theia" or "wcoss", the platform on which you ran
* `/path/to/stmp` - is the scrub area chosen by the scripts
* `$USER` - is your username
* `$$` - is a unique id chosen by the `rt.sh` to avoid overwriting
  an old run.  Generally it is the UNIX process id of `rt.sh`

To find `/path/to/stmp` and `$$` you need to look in the log of `rt.sh`
for a line like this, near the top of the `rt.log`:

    mkdir -p /scratch4/NCEPDEV/stmp3/Samuel.Trahan/rt_104307

Within that directory, you will find one directory for each test:

    you@theia> ls -1 /scratch4/NCEPDEV/stmp3/Samuel.Trahan/rt_104307
    gfs_eulerian
    gfs_gocart_nemsio
    gfs_slg
    gfs_slg_48pe
    gfs_slg_adiabatic
    gfs_slg_land
    gfs_slg_nsst
    gfs_slg_rsthst
    gfs_slg_stochy
    gfs_slg_t574
    nmm_2way_nests
    nmm_2way_nests_debug
    nmm_2way_nests_restart

Each directory contains input and output files for each test.  Some
files of interest are:

* `err` - stderr stream from the batch job that ran this program
* `out` - stdout stream from the batch job that ran this program
* `PET*.ESMF_LogFile` - ESMF log files from each MPI rank
* `nemsusage.xml` - resource usage information for all MPI ranks
* `timing.summary` - resource usage information for rank 0

Configuring rt.sh: The rt.conf
------------------------------

The `rt.sh` guides its function based on the `rt.conf` file.  That
file can be found in the NEMS/oldtests directory and has the following
syntax:

| COMMAND  |    METHOD    |  SUBSET  | PLATFORM |   VERSION  |
| -------- | ------------ | -------- | -------- | ---------- |
| APPBUILD | app=APP-NAME | standard |          |            |
| RUN      | test_name    | standard |          | nmm        |
| COMPILE  | nmm          | standard |          | nmmb_intel |

The available commands are:

* `APPBUILD` - run the [NEMSAppBuilder](#ni-appbuild) and load new modules
* `RUN` - run a test
* `COMPILE` - no longer supported; runs the [manual build system](#manual-method)

The meaning of the other arguments depends on the command, and is
described below.

### rt.conf APPBUILD Command

When the command is `APPBUILD` the other arguments have these meanings:

* `METHOD` - arguments to send to the NEMSAppBuilder
* `SUBSET` - `standard` or empty (all whitespace).  If `standard` is
  here, then only the `rt.sh -s` mode will run this build.
* `PLATFORM` - `wcoss` to run only on WCOSS, `theia` to run only on Theia,
  or empty (all whitespace) to run on all platforms
* `VERSION` - unused; leave this blank (all whitespace)

### rt.conf RUN Command

The RUN command runs a test.  The meanings of the columns are as follows:

* `METHOD` - name of the test.  This must correspond to a file in the
  NEMS/oldtests/tests directory.
* `SUBSET` - `standard` or empty (all whitespace).  If `standard` is
  here, then only the `rt.sh -s` mode will run this build.
* `PLATFORM` - `wcoss` to run only on WCOSS, `theia` to run only on Theia,
  or empty (all whitespace) to run on all platforms
* `VERSION` - which model this pertains to: `nmm` or `gfs`

### rt.conf COMPILE command

This command runs the [manual build system](#manual-method).  This is
unsupported and retained only for debugging the new build system.

* `METHOD` - arguments to the `make` command
* `SUBSET` - `standard` or empty (all whitespace).  If `standard` is
  here, then only the `rt.sh -s` mode will run this build.
* `PLATFORM` - Mandatory.  Must be `wcoss` to run only on WCOSS or
  `theia` to run only on Theia.  This is used to construct the
  `configure` command.
* `VERSION` - Mandatory. The ESMF version, passed to the `configure` command.

In the `COMPILE` mode, the following commands are run based on those
arguments:

    ./configure (VERSION)_(PLATFORM)
    source conf/modules.nems
    gmake clean
    gmake (METHOD) J=-j2

### Subsetting Tests in rt.conf

Note that you can explicitly disable parts of the test suite by
commenting out lines of rt.conf.  Note that disabling the build
commands (APPBUILD or COMPILE) will skip the build process and cause
tests to be run with whatever NEMS.x and modules.conf presently in
the NEMS external.
