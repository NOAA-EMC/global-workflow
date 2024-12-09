.. _experiment-setup:

================
Experiment Setup
================

 Global workflow uses a set of scripts to help configure and set up the drivers (also referred to as Workflow Manager) that run the end-to-end system. While currently we use a `ROCOTO <https://github.com/christopherwharrop/rocoto/wiki/documentation>`__ based system and that is documented here, an `ecFlow <https://www.ecmwf.int/en/learning/training/introduction-ecmwf-job-scheduler-ecflow>`__ based systm is also under development and will be introduced to the Global Workflow when it is mature. To run the setup scripts, you need to have rocoto and a python3 environment with several specific libraries. The easiest way to guarantee this is to source the following script, which will load the necessary modules for your machine:

 ::

   source workflow/gw_setup.sh

.. warning::
   Sourcing gw_setup.sh will wipe your existing lmod environment

.. note::
   Bash shell is required to source gw_setup.sh

^^^^^^^^^^^^^^^^^^^^^^^^
Forecast-only experiment
^^^^^^^^^^^^^^^^^^^^^^^^

Scripts that will be used:

   * ``workflow/setup_expt.py``
   * ``workflow/setup_xml.py``

***************************************
Step 1: Run experiment generator script
***************************************

The following command examples include variables for reference but users should not use environmental variables but explicit values to submit the commands. Exporting variables like EXPDIR to your environment causes an error when the python scripts run. Please explicitly include the argument inputs when running both setup scripts:

::

   cd workflow
   ./setup_expt.py gfs forecast-only --idate $IDATE --edate $EDATE [--app $APP] [--start $START] [--interval $INTERVAL_GFS] [--resdetatmos $RESDETATMOS] [--resdetocean $RESDETOCEAN]
     [--pslot $PSLOT] [--configdir $CONFIGDIR] [--comroot $COMROOT] [--expdir $EXPDIR] [--account $ACCOUNT]

where:

   * ``gfs`` is the first positional argument that instructs the setup script to produce a GFS experiment directory
   * ``forecast-only`` is the second positional argument that instructs the setup script to produce an experiment directory for forecast only experiments.
   * ``$APP`` is the target application, one of:

     - ATM: atmosphere-only [default]
     - ATMA: atm-aerosols
     - ATMW: atm-wave (currently non-functional)
     - S2S: atm-ocean-ice
     - S2SA: atm-ocean-ice-aerosols
     - S2SW: atm-ocean-ice-wave
     - S2SWA: atm-ocean-ice-wave-aerosols

   * ``$START`` is the start type (warm or cold [default])
   * ``$IDATE`` is the initial start date of your run (first cycle CDATE, YYYYMMDDCC)
   * ``$EDATE`` is the ending date of your run (YYYYMMDDCC) and is the last cycle that will complete [default: $IDATE]
   * ``$PSLOT`` is the name of your experiment [default: test]
   * ``$CONFIGDIR`` is the path to the ``/config`` folder under the copy of the system you're using [default: $TOP_OF_CLONE/parm/config/]
   * ``$RESDETATMOS`` is the resolution of the atmosphere component of the system (i.e. 768 for C768) [default: 384]
   * ``$RESDETOCEAN`` is the resolution of the ocean component of the system (i.e. 0.25 for 1/4 degree) [default: 0.; determined based on atmosphere resolution]
   * ``$INTERVAL_GFS`` is the forecast interval in hours [default: 6]
   * ``$COMROOT`` is the path to your experiment output directory. Your ``ROTDIR`` (rotating com directory) will be created using ``COMROOT`` and ``PSLOT``. [default: $HOME (but do not use default due to limited space in home directories normally, provide a path to a larger scratch space)]
   * ``$EXPDIR`` is the path to your experiment directory where your configs will be placed and where you will find your workflow monitoring files (i.e. rocoto database and xml file). DO NOT include PSLOT folder at end of path, it will be built for you. [default: $HOME]
   * ``$ACCOUNT`` is the HPC (i.e. slurm or PBS) account to use for the experiment. [default: $HPC_ACCOUNT; if $HPC_ACCOUNT is not set, then the default in the host file (workflow/hosts/<machine>.yaml) will be used]

Examples:

Atm-only:

::

   cd workflow
   ./setup_expt.py gfs forecast-only --pslot test --idate 2020010100 --edate 2020010118 --resdetatmos 384 --interval 6 --comroot /some_large_disk_area/Joe.Schmo/comroot --expdir /some_safe_disk_area/Joe.Schmo/expdir

Coupled:

::

   cd workflow
   ./setup_expt.py gfs forecast-only --app S2SW --pslot coupled_test --idate 2013040100 --edate 2013040100 --resdetatmos 384 --comroot /some_large_disk_area/Joe.Schmo/comroot --expdir /some_safe_disk_area/Joe.Schmo/expdir

Coupled with aerosols:

::

   cd workflow
   ./setup_expt.py gfs forecast-only --app S2SWA --pslot coupled_test --idate 2013040100 --edate 2013040100 --resdetatmos 384 --comroot /some_large_disk_area/Joe.Schmo/comroot --expdir /some_safe_disk_area/Joe.Schmo/expdir

****************************************
Step 2: Set user and experiment settings
****************************************

Go to your EXPDIR and check/change the following variables within your config.base now before running the next script:

   * ACCOUNT
   * HOMEDIR
   * STMP
   * PTMP
   * ARCDIR (location on disk for online archive used by verification system)
   * HPSSARCH (YES turns on archival)
   * HPSS_PROJECT (project on HPSS if archiving)
   * ATARDIR (location on HPSS if archiving)

Some of those variables will be found within a machine-specific if-block so make sure to change the correct ones for the machine you'll be running on.

Now is also the time to change any other variables/settings you wish to change in config.base or other configs. `Do that now.` Once done making changes to the configs in your EXPDIR go back to your clone to run the second setup script. See :doc:configure.rst for more information on configuring your run.

*************************************
Step 3: Run workflow generator script
*************************************

This step sets up the files needed by the Workflow Manager/Driver. At this moment only ROCOTO configurations are generated:

::

   ./setup_xml.py $EXPDIR/$PSLOT

Example:

::

   ./setup_xml.py /some_safe_disk_area/Joe.Schmo/expdir/test

Additional options for setting up Rocoto are available with `setup_xml.py -h` that allow users to change the number of failed tries, number of concurrent cycles and tasks as well as Rocoto's verbosity levels.

****************************************
Step 4: Confirm files from setup scripts
****************************************

You will now have a rocoto xml file in your EXPDIR ($PSLOT.xml) and a crontab file generated for your use. Rocoto uses CRON as the scheduler. If you do not have a crontab file you may not have had the rocoto module loaded. To fix this load a rocoto module and then rerun setup_xml.py script again. Follow directions for setting up the rocoto cron on the platform the experiment is going to run on.

^^^^^^^^^^^^^^^^^
Cycled experiment
^^^^^^^^^^^^^^^^^

Scripts that will be used:

   * ``workflow/setup_expt.py``
   * ``workflow/setup_xml.py``

***************************************
Step 1) Run experiment generator script
***************************************

The following command examples include variables for reference but users should not use environment variables but explicit values to submit the commands. Exporting variables like EXPDIR to your environment causes an error when the python scripts run. Please explicitly include the argument inputs when running both setup scripts:

::

   cd workflow
   ./setup_expt.py gfs cycled --idate $IDATE --edate $EDATE [--app $APP] [--start $START]
     [--interval $INTERVAL_GFS] [--sdate_gfs $SDATE_GFS]
     [--resdetatmos $RESDETATMOS] [--resdetocean $RESDETOCEAN] [--resensatmos $RESENSATMOS] [--nens $NENS] [--run $RUN]
     [--pslot $PSLOT] [--configdir $CONFIGDIR] [--comroot $COMROOT] [--expdir $EXPDIR] [--icsdir $ICSDIR]

where:

   * ``gfs`` is the first positional argument that instructs the setup script to produce a GFS experiment directory
   * ``cycled`` is the second positional argument that instructs the setup script to produce an experiment directory for cycled experiments.
   * ``$APP`` is the target application, one of:

     - ATM: atmosphere-only [default]
     - ATMA: atm-aerosols
     - ATMW: atm-wave (currently non-functional)
     - S2S: atm-ocean-ice
     - S2SA: atm-ocean-ice-aerosols
     - S2SW: atm-ocean-ice-wave
     - S2SWA: atm-ocean-ice-wave-aerosols

   * ``$IDATE`` is the initial start date of your run (first cycle CDATE, YYYYMMDDCC)
   * ``$EDATE`` is the ending date of your run (YYYYMMDDCC) and is the last cycle that will complete [default: $IDATE]
   * ``$START`` is the start type (warm or cold [default])
   * ``$INTERVAL_GFS`` is the forecast interval in hours [default: 6]
   * ``$SDATE_GFS`` cycle to begin GFS forecast [default: $IDATE + 6]
   * ``$RESDETATMOS`` is the resolution of the atmosphere component of the deterministic forecast [default: 384]
   * ``$RESDETOCEAN`` is the resolution of the ocean component of the deterministic forecast [default: 0.; determined based on atmosphere resolution]
   * ``$RESENSATMOS`` is the resolution of the atmosphere component of the ensemble forecast [default: 192]
   * ``$NENS`` is the number of ensemble members [default: 20]
   * ``$RUN`` is the starting phase [default: gdas]
   * ``$PSLOT`` is the name of your experiment [default: test]
   * ``$CONFIGDIR`` is the path to the config folder under the copy of the system you're using [default: $TOP_OF_CLONE/parm/config/]
   * ``$COMROOT`` is the path to your experiment output directory. Your ``ROTDIR`` (rotating com directory) will be created using ``COMROOT`` and ``PSLOT``. [default: $HOME]
   * ``$EXPDIR`` is the path to your experiment directory where your configs will be placed and where you will find your workflow monitoring files (i.e. rocoto database and xml file). DO NOT include PSLOT folder at end of path, it will be built for you. [default: $HOME]
   * ``$ICSDIR`` is the path to the ICs for your run if generated separately. [default: None]

Example:

::

   cd workflow
   ./setup_expt.py gfs cycled --pslot test --configdir /home/Joe.Schmo/git/global-workflow/parm/config --idate 2020010100 --edate 2020010118 --comroot /some_large_disk_area/Joe.Schmo/comroot --expdir /some_safe_disk_area/Joe.Schmo/expdir --resdetatmos 384 --resensatmos 192 --nens 80 --interval 6

Example ``setup_expt.py`` on Orion:

::

   Orion-login-3$ ./setup_expt.py gfs cycled --pslot test --idate 2022010118 --edate 2022010200 --resdetatmos 192 --resensatmos 96 --nens 80 --comroot /work/noaa/stmp/jschmo/comroot --expdir /work/noaa/global/jschmo/expdir
   EDITED:  /work/noaa/global/jschmo/expdir/test/config.base as per user input.
   EDITED:  /work/noaa/global/jschmo/expdir/test/config.aeroanl as per user input.
   EDITED:  /work/noaa/global/jschmo/expdir/test/config.ocnanal as per user input.

The message about the config.base.default is telling you that you are free to delete it if you wish but it’s not necessary to remove. Your resulting config.base was generated from config.base.default and the default one is there for your information.

What happens if I run ``setup_expt.py`` again for an experiment that already exists?

::

   Orion-login-3$ ./setup_expt.py gfs cycled --pslot test --idate 2022010118 --edate 2022010200 --resdetatmos 192 --resensatmos 96 --nens 80 --comroot /work/noaa/stmp/jschmo/comroot --expdir /work/noaa/global/jschmo/expdir

   directory already exists in /work/noaa/stmp/jschmo/comroot/test

   Do you wish to over-write [y/N]: y

   directory already exists in /work/noaa/global/jschmo/expdir/test

   Do you wish to over-write [y/N]: y
   EDITED:  /work/noaa/global/jschmo/expdir/test/config.base as per user input.
   EDITED:  /work/noaa/global/jschmo/expdir/test/config.aeroanl as per user input.
   EDITED:  /work/noaa/global/jschmo/expdir/test/config.ocnanal as per user input.

Your ``ROTDIR`` and ``EXPDIR`` will be deleted and remade. Be careful with this!

****************************************
Step 2: Set user and experiment settings
****************************************

Go to your EXPDIR and check/change the following variables within your config.base now before running the next script:

   * ACCOUNT
   * HOMEDIR
   * STMP
   * PTMP
   * ARCDIR (location on disk for online archive used by verification system)
   * HPSSARCH (YES turns on archival)
   * HPSS_PROJECT (project on HPSS if archiving)
   * ATARDIR (location on HPSS if archiving)

Some of those variables will be found within a machine-specific if-block so make sure to change the correct ones for the machine you'll be running on.

Now is also the time to change any other variables/settings you wish to change in config.base or other configs. `Do that now.` Once done making changes to the configs in your EXPDIR go back to your clone to run the second setup script. See :doc: configure.rst for more information on configuring your run.


*************************************
Step 3: Run workflow generator script
*************************************

This step sets up the files needed by the Workflow Manager/Driver. At this moment only ROCOTO configurations are generated:

::

   ./setup_xml.py $EXPDIR/$PSLOT

Example:

::

   ./setup_xml.py /some_safe_disk_area/Joe.Schmo/expdir/test

****************************************
Step 4: Confirm files from setup scripts
****************************************

You will now have a rocoto xml file in your EXPDIR ($PSLOT.xml) and a crontab file generated for your use. Rocoto uses CRON as the scheduler. If you do not have a crontab file you may not have had the rocoto module loaded. To fix this load a rocoto module and then rerun ``setup_xml.py`` script again. Follow directions for setting up the rocoto cron on the platform the experiment is going to run on.
