NEMS Directory Structure
========================

The NEMS directory contains the source code and test scripts for the
NEMS.  Most of the documentation is in the `doc` subdirectory or in
the `../doc/` directory.  Most of the files that were in the NEMS have
been moved to the application layer, discussed below.  Further
documentation, specific to the app, is also at the app level.

Within NEMS resides:

* `exe` - NEMS.x and other executables built from `src`
* `src` - main program for NEMS
 * `ENS_Cpl` - The Ensemble coupler directory.
 * `conf` - various compliation specifications
* `doc` - documentation.
* `NEMSAppBuilder` - a script to build NEMS, as discussed elsewhere in the
  documentation
* `NEMSCompsetRun` - script to run NEMS, identical to the regression test runner
* `OldCompsetRun` - prior version of the compset runner
* `tests` - test execution logic
  * `rtgen` - front-end to the regression test runner
  * `rt.sh` - wrapper around rtgen for users familiar with the old system

At the application level resides these files:

* `doc` - application-specific documentation

* `oldtests` - application-specific, old, test suite which is
   deprecated but retained for backward compatibility

* `compsets` - configuration for the NEMSCompsetRun and regression
   test runner

* `oldcompsets` - configuration for the old compset system available
   via OldCompsetRunner

* `modulefiles` - module loading information for each platform
  * `theia` - NOAA Theia modulefiles
  * `wcoss.phase1` - WCOSS Phase 1 modulefiles
  * ... other directories for other computers ...
* `conf` - configuration for NEMS/src/configure
* `parm` - parameter files for the test suites
* `log` - log directory for the NEMSAppBuilder and NEMSCompsetRun