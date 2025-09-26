.. _experiment-setup:

================
Experiment Setup
================

 Global workflow uses a set of scripts to help configure and set up the drivers (also referred to as Workflow Manager) that run the end-to-end system. While currently we use a `ROCOTO <https://github.com/christopherwharrop/rocoto/wiki/documentation>`__ based system and that is documented here, an `ecFlow <https://www.ecmwf.int/en/learning/training/introduction-ecmwf-job-scheduler-ecflow>`__ based systm is also under development and will be introduced to the Global Workflow when it is mature. To run the setup scripts, you need to have rocoto and a python3 environment with several specific libraries. The easiest way to guarantee this is to source the following script, which will load the necessary modules for your machine:

 ::

   source dev/ush/gw_setup.sh

.. warning::
   Sourcing gw_setup.sh will wipe your existing lmod environment

.. note::
   Bash shell is required to source gw_setup.sh

Scripts that will be used:

   * ``dev/workflow/setup_expt.py``
   * ``dev/workflow/setup_workflow.py``

****************************************
Step 1: Set user settings
****************************************

To make it easy for a user to specify the some of the user specific variables, users can create a ``.gwrc`` file in their home directory.  An example is provided in ``$TOP_OF_CLONE/dev/parm/workflow/gwrc`` that contains the following variables:

   - ACCOUNT: the account to charge for the run
   - HOMEDIR: the home directory of the user
   - STMP: the path to the DATAROOT storage area for the run
   - PTMP: the path to the COMROOT storage area for the run
   - HPSS_PROJECT: the project on HPSS to charge for the run

This file is read by the ``setup_expt.py`` script to set the user specific variables. If you do not have a ``.gwrc`` file, the setup script will revert to the default values in the repository.

***************************************
Step 2: Run experiment generator script
***************************************

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

******************************************
Step 3: Check user and experiment settings
******************************************

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

*************************************
Step 4: Run workflow generator script
*************************************

This step sets up the files needed by the Workflow Manager/Driver. At this moment only Rocoto configurations are generated:

::

   ./setup_workflow.py $EXPDIR/$PSLOT rocoto|ecflow

Example:

::

   ./setup_workflow.py /some_safe_disk_area/Joe.Schmo/expdir/test rocoto

Additional options for setting up Rocoto or ecFlow are available with `setup_workflow.py -h` that allow users to change the number of failed tries, number of concurrent cycles and tasks as well as verbosity levels.

Presently, only the Rocoto workflow engine is supported.  EcFlow capabilities are a work in progress.

****************************************
Step 5: Confirm files from setup scripts
****************************************

You will now have a rocoto xml file in your ``$EXPDIR`` (``$PSLOT.xml``) and a crontab file generated for your use. Rocoto uses CRON as the scheduler. If you do not have a crontab file you may not have had the rocoto module loaded. To fix this load a rocoto module and then rerun setup_workflow.py script again. Follow directions for setting up the rocoto cron on the platform the experiment is going to run on.
