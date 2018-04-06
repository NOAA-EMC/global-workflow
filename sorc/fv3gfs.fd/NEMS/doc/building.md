Compiling: NEMSAppBuilder {#building}
=========================

NEMS is the technical backbone for multiple modeling
applications. These modeling applications are multi-component coupled
systems that must pull together, in a systematic way, components from
different centers and agencies. The NEMS AppBuilder enables users to
construct a specific, versioned modeling application from a versioned
set of model components and configuration files.

The AppBuilder approach also encourages coordination of changes made
to different applications as they get checked back into the NEMS
repository.  The NEMS AppBuilder introduces two levels of
configuration control: the modeling application level and the NEMS
level. This split provides the model developer with *full version
control over all aspects of the modeling application code*, while
hiding many technical details about the infrastructure at the NEMS
level.

Under modeling application control:
* Version of NEMS.
* List of external components (e.g. MOM5, CICE, ...) required by the application.
* Version of each required external component.
* Location of source and installation directory for NEMS and each external component.
* Compiler and 3rd party library versions.

Under NEMS control:
* Knowledge base of how to build each of the supported external components.
* Infrastructure to configure and build NEMS according to the
  information specified by the application layer.
* Coupling system based on ESMF/NUOPC.




Running the NEMSAppBuilder
--------------------------

The NEMS AppBuilder is located within the 
\ref structure
as ./NEMS/NEMSAppBuilder. It is important to call the tool
from within the same directory where the application specific
AppBuilder description file is located. For information on how to
create or modify such a build file, see 
\ref building-new-targets "later sections"

There are two ways to call the NEMSAppBuilder: interactively and
non-interactively.

### Interactive NEMSAppBuilder Compilation

The NEMSAppBuilder has a user-friendly interface, based on low-level
terminal-based Unix utilities for maximum ease of use and portability.
To run an interactive build, simply start NEMSAppBuilder without arguments:
    
    [~/UGCS-Seasonal]$ ./NEMS/NEMSAppBuilder
    
![](appbuilder-step0.png)

Next each of the external components is built. First CICE, then MOM5:

![](appbuilder-step1.png)

Next the NEMS build system is configured:

![](appbuilder-step2.png)

Finally the NEMS executable is being built:

![](appbuilder-step3.png)

The build finishes with a notification:

![](appbuilder-step4.png)

During the building process the NEMS AppBuilder monitors the progress
and reports on issues followed by a diagnostic screen. Each diagnostic
screen has the option to repeat the step that just had failed or to
bail out. Repeating a step can be useful when the reported issue can
be corrected in a second terminal and the build process can continue.

If the build process terminates with a problem condition,
`appBuilder.*log` files with build and debug information will be present
in the root directory from which the AppBuilder was started.



### Non-Interacive Compilation

The syntax is:

    ./NEMS/NEMSAppBuilder (options) app=(build-target)

Here, the `(build-target)` is the application build target, also known
as the "build," or "project."  It corresponds to a
`(build-target).appBuild` file at the top-level (app) checkout
directory.

The  `(options)` should be one of the following:

* `rebuild` - clean the source directory before recompiling.

* `norebuild` - do not clean; reuse existing libraries and object
    files whenever possible.

Further options can be obtained by running

    ./NEMS/NEMSAppBuilder --help




Troubleshooting Failed Builds
-----------------------------

### Incomplete Checkout

When there are network problems or high server load, your checkout
from the Subversion and Git repositories may fail.  This will lead to
any number of confusing errors while building.  With a Subversion
checkout, you can resume the checkout process by going to the top
level (above the NEMS directory) and running `svn update`.  Repeat
that until no more files are updated, and no errors are reported.
As of this writing, there is no equivalent command for Git.

### Unclean Environment

Setting up your environment incorrectly can lead to problems while
building.  If you see build issues from a clean, new checkout, this
may be the problem.  You should remove all `module` commands from your
`~/.*rc` files and get a clean, new login shell.  Then retry the
build.

### Unclean Checkout

Another common cause of failed builds is having unintended changes in
your source code or build system.  To test for this, get a clean, new
checkout from the repository and retry.

### Unsupported Platform

Some apps only support a few platforms.  For example, the NEMSLegacy
app is only supported on WCOSS Phase 1 (Gyre/Tide) and NOAA Theia.
Attempts to build on other platforms may or may not work.

### Simultaneous Builds

Attempting to build multiple times in the same NEMS checkout directory
will cause unexpected failures.  For example, if you are running the
regression test system twice at once, multiple builds will happen at
the same time.  On Theia, this frequently shows up as a massive, many
terabyte, file which cannot be created due to fileset quota limits.
Other failure modes are possible.



\anchor building-new-targets

Creating New Build Targets 
--------------------------

As discussed above, the NEMSAppBuilder builds the executable based on
build targets described in `*.appBuild` files.  The list of build
targets available for an app is found at the top level of the app in
`*.appBuilder` files.  The app-level documentation should have
information about the meanings and purpose of each build target.  If
no build target is suitable for your purposes, you may need to create
one.  Frequently, the best way is to modify an existing appBuild file.
This section describes the various parts of the build configuration so
to assist you in deciding what to change when making your own build.

### Build Description File: `*.appBuild`

As an example, let us take the UGCS-Seasonal.appBuilder file:

    # Climate Forecast System
    #
    ## GSM-MOM5-CICE NEMS Application Builder file
    
    COMPONENTS=( "GSM" "CICE" "MOM5" )
    
    # CICE
    CICE_SRCDIR=$ROOTDIR/CICE
    CICE_DIR=$ROOTDIR/CICE-INSTALL
    
    # MOM5
    MOM5_SRCDIR=$ROOTDIR/MOM5
    MOM5_DIR=$ROOTDIR/MOM5-INSTALL

    case "$FULL_MACHINE_ID" in
        yellowstone|gaea)
            CHOSEN_MODULE=$FULL_MACHINE_ID/ESMF_NUOPC
            ;;
        wcoss*|theia)
            CHOSEN_MODULE=$FULL_MACHINE_ID/ESMF_700_gsm
            CONFOPT="gsm_intel_${MACHINE_ID:?}"
            ;;
    esac

The script must set certain variables:

 * `COMPONENTS = ( LIST )` --- the list of components that should be
   enabled in the NEMS executable.

 * For each component:

    * `*_SRCDIR` --- the source code for that component

    * `*_DIR` --- the installation directory for that component

 * `CHOSEN_MODULE` --- a modulefile, relative to the app-level
   `modulefiles` directory, that sets up the environment.

The build can also override three configuration files by placing a new
copy of the file in the application-level `conf` directory.  

 * `CONFOPT` --- the `configure.nems` file with build-time and
   link-time settings.

 * `EXTERNALS_NEMS` --- an `externals.nems` file that specifies
   locations of data and stub components.

 * `ESMF_VERSION_DEFINE` - path to an `ESMFVersionDefine.h` file

Reasonable defaults will be provided if any of those are unspecified.




### Platform Adaption

Note that the `*.appBuild` file has different options for different machines:

    case "$FULL_MACHINE_ID" in
        yellowstone|gaea)
            CHOSEN_MODULE=$FULL_MACHINE_ID/ESMF_NUOPC
            ;;
        wcoss*|theia)
            CHOSEN_MODULE=$FULL_MACHINE_ID/ESMF_700_gsm
            CONFOPT="gsm_intel_${MACHINE_ID:?}"
            ;;
    esac

The `$FULL_MACHINE_ID` and `$MACHINE_ID` variables can be
used to detect which machine you're running on.  This lets your
`*.appBuild` file decide which options to use.

| Location                 | `$FULL_MACHINE_ID` | `$MACHINE_ID`  | `$PEX`    |
| ------------------------ | ------------------ | -------------- | --------- | 
| WCOSS Phase 1 (IBM side) | `wcoss.phase1`     | `wcoss`        | `1`       |
| WCOSS Phase 2 (IBM side) | `wcoss.phase2`     | `wcoss`        | `2`       |
| WCOSS Cray               | `wcoss_cray`       | `wcoss_cray`   | `c`       |
| Jet (all partitions)     | `jet`              | `jet`          | &nbsp;    |
| Theia                    | `theia`            | `theia`        | &nbsp;    |
| GAEA                     | `gaea`             | `gaea`         | &nbsp;    |
| Yellowstone              | `yellowstone`      | `yellowstone`  | &nbsp;    |
| (other)                  | `unknown`          | `unknown`      | &nbsp;    |

The confusing mixture of `.` and `_` is for historical reasons
(maintaining compatibility with other scripts).  For your convenience,
two aliases are provided:

* `FULL_MACHINE_ID_DOT` = wcoss.phase1, wcoss.phase2, wcoss.cray
* `FULL_MACHINE_ID_UNDER` = wcoss_phase1, wcoss_phase2, wcoss_cray

Similar aliases are provided for `$MACHINE_ID_DOT` and `$MACHINE_ID_UNDER`.




### Build Environment Specification

The NEMS build system requires two files to set up the build
environment.  

 * `modulefiles/$PLATFORM/$MODULE` - uses the unix `module` command to
   load `modulefiles`

 * `conf/configure.nems.*` - sets variables required by NEMS build scripts.

The second file will be chosen automatically from `NEMS/src/conf` if
none is provided.




### The `modulefile`

The NEMS `modulefiles` must follow the standard `modulefile` syntax.
That means they must begin with this line:

    #%Module######################################################################

Other lines specify the modules to load.  Here is the full modulefile
for one of the GFS apps on WCOSS:

    #%Module######################################################################
    # This script is responsible for loading modules that are compatible with
    # the NUOPC Layer version used in NEMS.
    
    module load  EnvVars/1.0.0
    module load ibmpe lsf NetCDF/4.2/serial ESMF/700
    module load ics/15.0.3

Note that this is not a shell script; it is tcl script.  You cannot
have any `source` or `export` commands.  It is best to stick with
these two commands if possible:

 * `module load module-name`
 * `module use /p/a/t/h`

The `modulefiles` can contain other commands, the most common of which
are:

 * `prepend-path /p/a/t/h`
 * `append-path /p/a/t/h`
 * `remove-path /p/a/t/h`
 * `setenv variable value`
 * `unsetenv variable`
 * `set variable value`
 * `unset variable`

There are multiple implementations of the `module` command, some of
which have more powerful features.  On some platforms, you may have to
use the more advanced features in order to properly set up the
environment.  That is why NEMS uses a different modulefile for each
platform.

No matter what you do, you must follow this critical rule:

\warning Never put a `module purge` command in a `modulefile`.

Placing a `module purge` in a `modulefile` will cause infinite loops,
corrupted environments, or segfaults of the `module` command on some
platforms.  The NEMS scripts already purge the modules by running a
"shell include" file before loading your `modulefile`.  This script
can be found in:

* `NEMS/src/conf/module-setup.sh.inc` (for bash, sh, and ksh)
* `NEMS/src/conf/module-setup.csh.inc` (for csh and tcsh)




### For More Information About `make` and `modulefiles`

Good general resources for learning about `modulefiles` are:

* NICS module website: http://www.nics.tennessee.edu/computing-resources/modules

* XSEDE computing environments page: https://www.xsede.org/software-environments

*  The `module` command:

       module help # list allowed "module" commands
       module avail # list available modules
       module spider # also list modules in hidden families

   Note that the `module spider` command is only available on
   platforms that are able to hide modules that are unavailable
   without prerequisites.  For example, hiding a `NetCDF` library that
   was compiled with `gfortran` until the `gfortran` module is loaded.

For `makefiles`,

* GNU Make tutorial: http://opensourceforu.com/2012/06/gnu-make-in-detail-for-beginners/

* GNU Make manual: https://www.gnu.org/s/make/manual/make.html

* If you have a lot of time on your hands, a book: https://notendur.hi.is/jonasson/software/make-book/


