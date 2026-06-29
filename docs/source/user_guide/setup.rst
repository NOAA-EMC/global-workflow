.. _experiment-setup:

################
Experiment Setup
################

 Global workflow uses a set of scripts to help configure and set up the drivers (also referred to as Workflow Manager) that run the end-to-end system. While currently we use a `rocoto <https://github.com/christopherwharrop/rocoto/wiki/documentation>`__ based system and that is documented here, an `ecFlow <https://www.ecmwf.int/en/learning/training/introduction-ecmwf-job-scheduler-ecflow>`__ based systm is also under development and will be introduced to the Global Workflow when it is mature. To run the setup scripts, you need to have rocoto and a python3 environment with several specific libraries. The easiest way to guarantee this is to source the following script, which will load the necessary modules for your machine:

 .. code-block:: bash

   source dev/ush/gw_setup.sh

.. warning::
   Sourcing gw_setup.sh will wipe your existing lmod environment

.. note::
   Bash shell is required to source gw_setup.sh

There are two main ways to run GW experiments:

- Create the experiment directory and generate the workflow in separate steps. This is useful if you need to change settings in the experiment config files before execution. Scripts that will be used:
   - ``dev/workflow/setup_expt.py``
   - ``dev/workflow/setup_workflow.py``
- Using case files to combine experiment directory and workflow generation into a single step. This is the method used in the :doc:`../quick_start`. This is useful if you are running experiments with existing configurations, especially those from the CI test system. It also allows the generation of multiple experiments at once. Script used:
   - ``dev/workflow/generate_workflows.sh``

****************************************************
Generating experiment and workflow in separate steps
****************************************************

=================
Set user settings
=================

To make it easy for a user to specify the some of the user specific variables, users can create a ``.gwrc`` file in their home directory.  An example is provided in ``$TOP_OF_CLONE/dev/parm/workflow/gwrc`` that contains the following variables:

   - ACCOUNT: the account to charge for the run
   - HOMEDIR: the home directory of the user
   - STMP: the path to the DATAROOT storage area for the run
   - PTMP: the path to the COMROOT storage area for the run
   - HPSS_PROJECT: the project on HPSS to charge for the run

This file is read by the ``setup_expt.py`` script to set the user specific variables. If you do not have a ``.gwrc`` file, the setup script will revert to the default values in the repository.

===============================
Run experiment generator script
===============================

The following command examples include variables for reference but users should not use environmental variables but explicit values to submit the commands. Exporting variables like EXPDIR to your environment causes an error when the python scripts run. Please explicitly include the argument inputs when running both setup scripts:

::

   cd dev/workflow

   ./setup_expt.py NET MODE --idate $IDATE --edate $EDATE [--app $APP] [--start $START]
     [--interval $INTERVAL_GFS] [--resdetatmos $RESDETATMOS] [--resdetocean $RESDETOCEAN]
     [--pslot $PSLOT] [--configdir $CONFIGDIR] [--comroot $COMROOT] [--expdir $EXPDIR] [--gwrc $GWRC]
     [--resensatmos $RESENSATMOS] [--nens $NENS] [--run $RUN]

where:

   * ``NET`` is the first positional argument that initializes the parser for the correct system.  Valid values are:
       - gfs: Global Forecast System (GFS)
       - gefs: Global Ensemble Forecast System (GEFS)
       - sfs: Seasonal Forecast System (SFS)
       - gcafs: Global Chemistry and Aerosol Forecast System (GCAFS)
   * ``MODE`` is the second positional argument that instructs the setup script to produce an experiment directory for mode of execution.  Valid options are:
       - forecast-only: for running a forecast only experiment
       - cycled: for running a cycled experiment (forecast + data assimilation)

   Based on the ``NET`` and ``MODE`` arguments, the setup script will provide further input options to the user. The script will also check for the existence of the ``$ROTDIR`` and ``$EXPDIR`` directories and prompt the user to overwrite them if they already exist.

   * ``$APP`` is the target application, one of:

     - ATM: atmosphere-only [default]
     - ATMA: atm-aerosols
     - ATMW: atm-wave (currently non-functional)
     - S2S: atm-ocean-ice
     - S2SA: atm-ocean-ice-aerosols
     - S2SW: atm-ocean-ice-wave
     - S2SWA: atm-ocean-ice-wave-aerosols

   * ``$START`` is the start type (``warm`` or ``cold`` [default: ``cold``])
   * ``$IDATE`` is the initial start date of your run (first cycle date in ``YYYYMMDDCC``)
   * ``$EDATE`` is the ending date of your run (``YYYYMMDDCC``) and is the last cycle that will complete [default: ``$IDATE``]
   * ``$PSLOT`` is the name of your experiment [default: ``test``]
   * ``$CONFIGDIR`` is the path to the ``/config`` folder under the copy of the system you're using [default: ``$TOP_OF_CLONE/dev/parm/config/$NET``]
   * ``$RESDETATMOS`` is the resolution of the atmosphere component of the system (i.e. 768 for C768) [default: ``384``]
   * ``$RESDETOCEAN`` is the resolution of the ocean component of the system (i.e. 0.25 for 1/4 degree) [default: ``0.``; determined based on atmosphere resolution]
   * ``$INTERVAL_GFS`` is the forecast interval in hours [default: ``6``]
   * ``$COMROOT`` is the path to your experiment output directory. Your ``ROTDIR`` (rotating com directory) will be created using ``COMROOT`` and ``PSLOT``. [default: ``$HOME`` (but do not use default due to limited space in home directories normally, provide a path to a larger scratch space)]
   * ``$EXPDIR`` is the path to your experiment directory where your configs will be placed and where you will find your workflow monitoring files (i.e. rocoto database and xml file). DO NOT include PSLOT folder at end of path, it will be built for you. [default: ``$HOME``]
   * ``$GWRC`` is the custom user global-workflow resource configuration file. [default: ``$HOME/.gwrc`` or ``$TOP_OF_CLONE/dev/parm/workflow/gwrc``]

   For the ``cycled`` mode, additional options are available:

   * ``$SDATE_GFS`` cycle to begin GFS forecast [default: ``$IDATE + 6``]
   * ``$RESENSATMOS`` is the resolution of the atmosphere component of the ensemble forecast [default: ``192``]
   * ``$NENS`` is the number of ensemble members [default: ``20``]
   * ``$RUN`` is the starting phase [default: ``gdas``]

Examples:

Forecast-only with Atm-only configuration in the GFS:

::

   cd dev/workflow
   ./setup_expt.py gfs forecast-only --pslot test --idate 2020010100 --edate 2020010118 --resdetatmos 384 --interval 6 --comroot /some_large_disk_area/Joe.Schmo/comroot --expdir /some_safe_disk_area/Joe.Schmo/expdir

Forecast-only with Coupled model configuration in the GFS:

::

   cd dev/workflow
   ./setup_expt.py gfs forecast-only --app S2SW --pslot coupled_test --idate 2013040100 --edate 2013040100 --resdetatmos 384 --comroot /some_large_disk_area/Joe.Schmo/comroot --expdir /some_safe_disk_area/Joe.Schmo/expdir

Forecast-only with the Coupled model (including aerosols) in the GFS:

::

   cd dev/workflow
   ./setup_expt.py gfs forecast-only --app S2SWA --pslot coupled_test --idate 2013040100 --edate 2013040100 --resdetatmos 384 --comroot /some_large_disk_area/Joe.Schmo/comroot --expdir /some_safe_disk_area/Joe.Schmo/expdir

Cycled with the Atmosphere-only model (including ensembles) in the GFS:

::

   cd dev/workflow
   ./setup_expt.py gfs cycled --app ATM --pslot cycled_test --idate 2013040100 --edate 2013040100 --comroot /some_large_disk_area/Joe.Schmo/comroot --expdir /some_safe_disk_area/Joe.Schmo/expdir --resdetatmos 384 --resensatmos 192 --nens 80 --interval 6

==================================
Check user and experiment settings
==================================

Go to your ``EXPDIR`` and check the following variables within your ``config.base`` now before running the next script:

   * ``ACCOUNT``
   * ``HOMEDIR``
   * ``STMP``
   * ``PTMP``
   * ``ARCDIR`` (location on disk for online archive used by verification system)
   * ``HPSSARCH`` (YES turns on archival)
   * ``HPSS_PROJECT`` (project on HPSS if archiving)
   * ``ATARDIR`` (location on HPSS if archiving)

Some of those variables will be found within a machine-specific if-block so make sure to change the correct ones for the machine you'll be running on.

`NOTE`: If you selected ``ARCHCOM_TO='globus_hpss``, then you will need to activate your globus connections between Mercury and MSU.  See :doc: globus_arch.rst for more details.

Now is also the time to change any other variables/settings you wish to change in ``config.base`` or other configs. `Do that now.` Once done making changes to the configs in your EXPDIR go back to your clone to run the second setup script. See :doc:configure.rst for more information on configuring your run.
Go to your ``EXPDIR`` and check/change the following variables within your ``config.base`` now before running the next script.

=============================
Run workflow generator script
=============================

This step sets up the files needed by the Workflow Manager/Driver. At this moment only Rocoto configurations are generated:

::

   ./setup_workflow.py $EXPDIR/$PSLOT rocoto|ecflow

Example:

::

   ./setup_workflow.py /some_safe_disk_area/Joe.Schmo/expdir/test rocoto

Additional options for setting up Rocoto or ecFlow are available with `setup_workflow.py -h` that allow users to change the number of failed tries, number of concurrent cycles and tasks as well as verbosity levels.

Presently, only the Rocoto workflow engine is supported.  EcFlow capabilities are a work in progress.

================================
Confirm files from setup scripts
================================

You will now have a rocoto xml file in your ``$EXPDIR`` (``$PSLOT.xml``) and a crontab file generated for your use. Rocoto uses CRON as the scheduler. If you do not have a crontab file you may not have had the rocoto module loaded. To fix this load a rocoto module and then rerun setup_workflow.py script again. Follow directions for setting up the rocoto cron on the platform the experiment is going to run on.

****************
Using Case Files
****************

.. note::

   See the :doc:`../quick_start` for a detailed walk-through of running a case using this method.

==========
Case files
==========
Pre-configured cases for GW can be found in ``dev/ci/cases``, with the recommend (low-resolution) cases in the ``dev/ci/cases/pr`` directory. These are actually a cascade of yamls used to generate experiment settings.

* The main case file, which largely consists of just the settings that would be provided manually to ``setup_expt.py`` when running separately. The case file also points to a second yaml via ``experiment:yaml:``. Example from C48_ATM.yaml:

.. code-block:: yaml
   experiment:
      net: gfs
      mode: forecast-only
      pslot: {{ 'pslot' | getenv }}
      app: ATM
      resdetatmos: 48
      comroot: {{ 'RUNTESTS' | getenv }}/COMROOT
      expdir: {{ 'RUNTESTS' | getenv }}/EXPDIR
      icsdir: {{ BASE_IC }}/C48mx500/20250808
      idate: 2021032312
      edate: 2021032312
      yaml: {{ HOMEglobal }}/dev/ci/cases/yamls/gfs_defaults_ci.yaml

   workflow:
      engine: rocoto
      rocoto:
        maxtries: 2
        cyclethrottle: 3
        taskthrottle: 25
        verbosity: 2

- The second yaml points to a third yaml that contains default values, along with settings that override those in the default yaml. Other than the defaults section, other blocks refer to the config file the settings belong to (i.e. settings in ``base:`` modify ``config.base``). For the included cases, these are found in ``dev/ci/cases/yaml``. Continuing with our example:

   .. code-block:: yaml

      defaults:
        !INC {{ HOMEglobal }}/dev/parm/config/gfs/yaml/defaults.yaml
      base:
        DO_TEST_MODE: "YES"
        FETCHDIR: "/NCEPDEV/emc-global/1year/David.Grumm/test_data"
        DO_METP: "NO"

- The third yaml, incorporated by the defaults in the second, contains default values for all the config settings that are customizable using this method. These default files are found in ``dev/parm/config/<system>/yaml/default.yaml``. This file is too long to be included here.

- Finally, there is a separate yaml that includes machine-specific settings, mostly default paths. These can be found in ``dev/workflow/hosts/`` and are applied automatically.

=======================
Running from case files
=======================

To run experiments from cases files, use the ``generate_workflows.sh`` script in the ``dev/workflow`` directory. To run a single case from the ``cases/pr`` directory, this would be the command to run:

.. code-block:: bash

   ./generate_workflows.sh -A <hpc_account> -y 'test_case' /path/for/output

Where ``test_case`` is the file name in ``dev/ci/cases/pr`` (with or without the ``.yaml`` extension).

This will generate two directories in ``/path/for/output``: ``EXPDIR`` and ``COMROOT``. Inside each will be a test_case directory for the test case selected. The EXPDIR is where your experiment and workflow are generated. Output will then be placed in COMROOT.

.. note::

   The path to output is known as the RUNTESTS directory

To run multiple cases at once, simply add additional case names to the ``-y`` option, separating each case with a space and making sure all the entire list is enclosed in quotations:

.. code-block:: bash

   ./generate_workflows.sh -A <hpc_account> -y 'test_case_1 test_case_2' /path/for/output

Instead of specifying individual cases, you can also run all test cases for one or more systems by using one or more of the following options instead of ``-y``:

-G Run all GFS cases
-E Run all GEFS cases
-C Run all GCADS cases
-S Run all SFS cases

Specifying ``-GECS`` would run all tests valid for the current system. Some tests are turned off on certain systems.

If you wish to run test cases from a different directory (either another in ``dev/ci/cases`` or your own), you can specify that directory with the ``-Y`` option:

.. code-block:: bash

   ./generate_workflows.sh -A <hpc_account> -Y /path/to/cases -y 'test_case_1 test_case_2' /path/for/output

.. note::

   There is currently no way to combine cases from multiple directories at the same time

There is also an option to add a tag (label) to the experiment name using the ``-t`` option. This is useful if you are running multiple batches of experiments using the same cases (while the output locations are different, there is the potential for name collision in the temporary space used by the jobs).

.. code-block:: bash

   ./generate_workflows.sh -A <hpc_account> -t 'label' -y 'test_case_1 test_case_2' /path/for/output

This will append the label to the experiment name for each case, distinguishing it from others you may run.

There are a number of other options. Here is the full list of options available to ``generate_workflows.sh``:

.. code-block:: none

    -H Root directory of the global workflow.
       If not specified, then the directory is assumed to be one parent
       directory up from this script's residing directory.

    -b Run build_all.sh with default flags
       (build only on login nodes)

    -B Run build_all.sh -c with default flags [-c triggers build on compute nodes]
       (build only on compute nodes)

    -u Update submodules before building and/or generating experiments.

    -y "list of YAMLs to run"
       This option is incompatible with -G, -E, -S, or -C.
       Example: -y "C48_ATM C48_S2SW C96C48_hybatmDA"

    -D Delete the RUNTESTS and DATAROOT directories if they already exist

    -Y /path/to/directory/with/YAMLs
       If this option is not specified, then the ${HOMEglobal}/dev/ci/cases/pr
       directory is used.

    -G Run all valid GFS cases in the specified YAML directory.
       If -b is specified, then the GSI and GDASApp will also be
       built via build_all.sh.
       Note that these builds are disabled on some systems, which
       will result in a warning from build_all.sh.

    -E Run all valid GEFS cases in the specified YAML directory.
       If -b is specified, then "-w" will be passed to build_all.sh.

    -S Run all valid SFS cases in the specified YAML directory.

    -C Run all valid GCAFS cases in the specified YAML directory.

    NOTES on -G, -E, -S and -C:
         - Valid cases are determined by the experiment:system key as
           well as the skip_ci_on_hosts list in each YAML.

    -A "HPC account name"  Set the HPC account name.
       If this is not set, the default in
       $HOMEglobal/dev/ci/platform/config.$machine
       will be used.

    -I "/path/to/base_ic"  Override BASE_IC for all cases.
       If this is not set, BASE_IC is read from the hosts YAML
       ($HOMEglobal/dev/workflow/hosts/$machine.yaml).

    -c Append the chosen set of tests to your existing crontab
       If this option is not chosen, the new entries that would have been
       written to your crontab will be printed to stdout.
       NOTES:
          - For Orion/Hercules, this option will not work unless run on
            the [orion|hercules]-login-1 head node.

    -e "your@email.com" Email address to place in the crontab.
       If this option is not specified, then the existing email address in
       the crontab will be preserved.

    -t Add a 'tag' to the end of the case names in the pslots to distinguish
       pslots between multiple sets of tests.

    -v Verbose mode.  Prints output of all commands to stdout.

    -V Very verbose mode.  Passes -v to all commands and prints to stdout.

    -d Debug mode.  Same as -V but also enables logging (set -x).

    -h Display this message.

===================
Creating case files
===================

The easiest way to create your own case file is to start from an existing case file. Choose the existing case closest to the one you want to run, then copy the case to a new name and location. If you wish to modify the config settings, also copy the accompanying one from ``dev/ci/cases/yaml`` and update the reference in the case file. You should NOT need to change the defaults file, as those settings can all be overriden in the two files you have already copied. After you have made your modifications, use the ``-Y`` option to specify the location and ``-y`` to specify your new case name.
