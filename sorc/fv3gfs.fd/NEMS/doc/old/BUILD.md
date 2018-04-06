<a name="building"></a>Building NEMS
==========================================

This chapter describes the options for building the NEMSLegacy, and
the supported platforms.  There are three ways to build: the
NEMSAppBuilder in interactive or non-interactive mode, or a manual
process.  The recommended way to compile the NEMS is to use the
NEMSAppBuilder in non-interactive mode.  However, all methods are
described here.  We also provide troubleshooting information at the
end of this chapter.

Build Targets
-------------

The list of build targets available for an app is found at the top
level of the app in `*.appBuilder` files.  The app-level documentation
should have information about the meanings and purpose of each build
target.

<a name="ni-appbuild"></a> Recommended Method: Non-Interactive NEMSAppBuilder
-----------------------------------------------------------------------------

From the top level (directory above NEMS), run the `NEMSAppBuilder`.
This is a build script that knows how to build various apps, and works
for more than just the NEMSLegacy.  The syntax is:

    ./NEMS/NEMSAppBuilder (options) app=(app)

Here, the `(app)` is the selected application as discussed in the
[Supported Builds and Platforms section](#supbuild).  The `(options)`
should be one of the following:

* `rebuild` - clean the source directory before recompiling.
* `norebuild` - do not clean; reuse existing libraries and object
  files whenever possible.


Interactive NEMSAppBuilder
--------------------------

The NEMSAppBuilder can be run in interactive mode.  To do so, simply
run the command without any arguments:

    ./NEMS/NEMSAppBuilder

The NEMSAppBuilder will instruct you further.  Note that this method
will discard some of the log files, which makes build failures harder
to track.  Also, it has some errors in its process tracking, and will
kill the wrong processes when a build is canceled.  Such bugs are why
the non-interactive mode is preferred.



<a name="manual-method"></a>Manual Method: Configure, Clean, Make
-----------------------------------------------------------------

It is possible to build all apps via a manual method.  This method
also makes other, undocumented, features available.  Ultimately, the
NEMSAppBuilder is simply a wrapper around these manual commands.
Before using such manual commands, it is best to talk to code managers
to make sure you are building correctly.

The manual method works like this:

    cd NEMS/src/
    ./configure (method)
    source conf/modules.nems
    gmake clean
    gmake (nems-ver) J=-j2

The `(method)` is one of the available configurations.  Run
`./configure help` to get a list, or read the `configure` script.
The `(nems-ver)` is one of the following:

* `gsm` - build the GSM without GOCART
* `gsm GOCART_MODE=full`
* `nmm` - build the NMM without debug
* `nmm DEBUG=on` - build NMM in debug mode
* `nmm_post` - build NMM with inline post-processing



Troubleshooting Failed Builds
-----------------------------

### Incomplete Checkout

When there are network problems or high server load, your checkout
from the Subversion and Git repositories may fail.  This will lead to
any number of confusing errors while building.  You can continue the
checkout process by going to the top level (above the NEMS directory) and running
`svn update`.  Repeat that until no more files are updated, and no
errors are reported.

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







