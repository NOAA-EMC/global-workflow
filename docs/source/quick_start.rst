#################
Quick Start Guide
#################

Run your first global workflow (GW) test case in no time!

.. note::

    If you wish to use your own AWS instance, you need to complete the :ref:`aws_setup` instructions first.

=======================================
Determine where you will put your stuff
=======================================

Determine where is appropriate to set up your experiments on your chosen HPC and project. You will need two locations: a place to store the code (hereafter ``$MY_SAVE``) and a place to put your experiment directory and output (hereafter ``$MY_NOSCRUB``). A third place (``$STMP``) will be used for temporary run directories. This last location is already defined in scripts, though you may wish to change it later.

Don't use your home directory (especially for output). Use available "scratch" or "lustre" space. Create the directories if they do not already exist.

Recommended locations by supported HPC:

+----------------+-------------------------------------------------+----------------------------------------------------+
| Resource       | Save                                            | Noscrub                                            |
+================+=================================================+====================================================+
| WCOSS2         | /lfs/h2/emc/<project>/save/${USER}/gw           | /lfs/h2/emc/<project>/noscrub/${USER}/gw           |
+----------------+-------------------------------------------------+----------------------------------------------------+
| Hera/Ursa      | /scratch3/<portfolio>/<project>/${USER}/save/gw | /scratch3/<portfolio>/<project>/${USER}/noscrub/gw |
|                | /scratch4/<portfolio>/<project>/${USER}/save/gw | /scratch4/<portfolio>/<project>/${USER}/noscrub/gw |
+----------------+-------------------------------------------------+----------------------------------------------------+
| Orion/Hercules | /work2/<portfolio>/<project>/${USER}/save/gw    | /work2/<portfolio>/<project>/${USER}/noscrub/gw    |
|                | /work/<portfolio>/<project>/${USER}/save/gw     | /work/<portfolio>/<project>/${USER}/noscrub/gw     |
+----------------+-------------------------------------------------+----------------------------------------------------+
| Gaea C6        | /gpfs/f6/<project>/scratch/${USER}/save/gw      | /gpfs/f6/<project>/scratch/${USER}/noscrub/gw      |
+----------------+-------------------------------------------------+----------------------------------------------------+
| Derecho        | /glade/work/${USER}/gw                          | /glade/derecho/scratch/${USER}/gw                  |
+----------------+-------------------------------------------------+----------------------------------------------------+
| AWS (native)   | /lustre/users/First.Last/save/gw                | /lustre/users/First.Last/noscrub/gw                |
+----------------+-------------------------------------------------+----------------------------------------------------+

You will also need to know what your HPC account for the job scheduler is for the platform you choose.

.. warning::

    If you are working on a machine other than Derecho or AWS, you must have rstprod to access observations for any case using data assimilation. See :ref:`restricted_data`.

=============================
Clone the official repository
=============================

.. code-block:: bash

    cd "${MY_SAVE}"
    git clone https://github.com/NOAA-EMC/global-workflow.git --recursive tutorial --jobs 8
    cd tutorial

You can change ``tutorial`` to be anything you wish.

This will checkout the current develop branch of GW, including all its sub-components.

=======================================
Build and install for forecast-only GFS
=======================================

.. code-block:: bash

    cd sorc
    ./build_all.sh -c -A <hpc_account> gfs
    ./link_workflow.sh

This builds the executables necessary to run your first test case and then creates links for necessary scripts and static data (fix) files.

.. note::

    If you are running on native AWS, the hpc_account is irrelevant. Use any string.

.. note::

    Code shown builds components using HPC compute nodes, which is highly recommended.

    You can build on the login node instead by omitting the ``-c -A <hpc_account>``.

==================================================
Create your experiment directory for the test case
==================================================

.. code-block:: bash

    cd ../dev/workflow
    source ../ush/gw_setup.sh
    ./generate_workflows.sh -A <hpc_account> -y C48_ATM "${MY_NOSCRUB}/tutorial"

Again, ``tutorial`` here can be changed to anything you wish (and does not need to match what you used for the code directory).

This will create two directories in ``${MY_NOSCRUB}/tutorial``:

- ``COMROOT``: where all the output will be stored
- ``EXPDIR``: where all the experiment directories will be stored.

In each, there will be a ``C48_ATM`` directory corresponding to the C48_ATM case configuration we specified. (Sourcing ``gw_setup.sh`` ensures you have the right environment set up, including rocoto).

This command will also print out a crontab line to run the experiment. Copy that, we will be using it in the next step.

=========================
Set up cron to run rocoto
=========================

Open up crontab in edit mode (``crontab -e``). On some systems, you will need to be on a certain or special login node to do this. Paste the line printed from the last step into your crontab, then save and exit.

.. note::

    On Gaea C6, you will need to use the scrontab utility instead of crontab. Please refer to the :ref:`scrontab` section for additional needed settings.

This will set up rocoto to run every five minutes. Rocoto is the workflow manager used for GW in development mode. When run, it will poll the job scheduler to get the status of any running jobs and then submit any jobs that have all their prerequisites met.

It is recommended that you do not set the update frequency shorter than five minutes if you are on a shared HPC resource, as that can overload the system and make sysadmins unhappy.

===========================
Monitor experiment progress
===========================

To view the progress of your jobs, use the rocotostat command:

.. code-block:: bash

    cd "${MY_NOSCRUB}/tutorial/EXPDIR/C48_ATM"
    rocotostat -w C48_ATM.xml -d C48_ATM.db

This will show the status of every job in the workflow pipeline. ``C48_ATM.xml`` is the workflow definition file, and ``C48_ATM.db`` is the database where rocoto stores the last known status of all the jobs. If you have no ``C48_ATM.db`` in your experiment directory, then ``rocotorun`` has not been run yet.

Alternatively, OMD has developed a tool to called rocoto viewer which will provide an interactive display to view job status. The display updates regularly and can also be used to call other common rocoto commands (``rocotocheck``, ``rocotorewind``, ``rocotoboot``). This tool can be found in the ``dev/workflow`` directory as ``rocoto_viewer.py``. Rocoto viewer requires ``TERM`` to be set to ``xterm``, so it is recommended you create a bash function or otherwise make sure TERM is set to 'xterm' before using:

.. code-block:: bash

    function rocoto_viewer {
        oldterm="${TERM}"
        export TERM="xterm"
        "${MY_SAVE}/tutorial/dev/workflow/rocoto_viewer.py" "$@"
        export TERM="${oldterm}"
    }

.. note::

    You may wish to copy ``rocoto_viewer.py`` to a permanent location so you do not have to keep updating the function.

See also :doc:`user_guide/monitor_rocoto`.

==================
Checking log files
==================

Log files for each job will be located in ``${MY_NOSCRUB}/tutorial/COMROOT/C48_ATM/logs/2021032312``. The file names match the name of the job as listed in rocoto, with the suffix ``.log`` (e.g. ``gfs_stage_ic.log``, ``gfs_fcst_seg0.log``, etc.). If a job has been rerun, a number will be appended to the filename with ascending age (``gfs_stage_ic.log.0`` would be the previous run, ``gfs_stage_ic.log.1`` would be the run before that, etc.).

===============
Checking output
===============

Output will be placed in ``${MY_NOSCRUB}/tutorial/COMROOT/C48_ATM``. Output is organized into a hierarchical structure:

.. code::

    ${RUN}.${PDY}/${cyc}
    └ Member (if any)
      └ Data category (conf/analysis/model/product)
        └ Component (atmos/chem/ice/ocean/wave)
          └ Data type
            └ Grid/domain (if any)

For this experiment, once complete you should see:

.. code::

    gfs.20210323
    └── 12
        ├── conf
        ├── model
        │   └── atmos
        │       ├── history
        │       ├── input
        │       └── master
        └── products
            └── atmos
                ├── cyclone
                │   ├── genesis_vital
                │   └── tracks
                └── grib2
                    ├── 0p25
                    ├── 0p50
                    └── 1p00

+-----------+-------------------------------------------------------+
| Directory | Contents                                              |
+===========+=======================================================+
| analysis* | analysis files                                        |
+-----------+-------------------------------------------------------+
| bmatrix*  | background error for analysis                         |
+-----------+-------------------------------------------------------+
| conf      | select configuration files, mostly forecast namelists |
+-----------+-------------------------------------------------------+
| model     | direct input/output from the forecast                 |
+-----------+-------------------------------------------------------+
| obs*      | observations used for data assimilation               |
+-----------+-------------------------------------------------------+
| products  | derived products typically published                  |
+-----------+-------------------------------------------------------+
| | * Not present for C48_ATM case                                  |
+-------------------------------------------------------------------+

==============
Try more cases
==============

Now that you have successfully run your first case, you are ready to try more cases.

To run many of the other cases, you will first need to rebuild with additional components. To do this, repeat step 2, but instead of 'gfs', use one or more of the following:

+--------+-----------------------------------------------------------+
| Build  | Description                                               |
+========+===========================================================+
| gfs    | components for GFS (no data assimilation)                 |
+--------+-----------------------------------------------------------+
| gsi    | components for "old" DA (GSI; atmosphere-only)            |
+--------+-----------------------------------------------------------+
| gdas   | components for "new" DA (JEDI-based)                      |
+--------+-----------------------------------------------------------+
| gefs   | components for medium-range global ensemble               |
+--------+-----------------------------------------------------------+
| sfs    | components for sub-seasonal forecast system               |
+--------+-----------------------------------------------------------+
| gcafs  | components for global chemistry & aerosol forecast system |
+--------+-----------------------------------------------------------+
| all    | build everything                                          |
+--------+-----------------------------------------------------------+

For a full list of what is built for each option, see :ref:`build options <build_options>`.

You can run a different test case(s) by changing what you pass into ``generate_workflows.sh`` with the ``-y`` option. By default, the script will look in the ``dev/ci/cases/pr`` directory for the case. You can change this by passing a different directory with the ``-Y`` option. You can also specify multiple cases by placing quotes around the argument and a space between each case (e.g. ``-y "C48_ATM C96_atm3DVar"``).

.. _test_configurations:

Available low-resolution cases (in the ``dev/ci/cases/pr`` directory):

+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| Case                     | Builds required |   WCOSS2   |    Ursa    |  Hercules  |  Gaea C6   |  Derecho   | Native AWS |
+==========================+=================+============+============+============+============+============+============+
| C48_ATM                  | gfs             |     X      |     X      |     X      |     X      |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C48_ATM_ecflow           | gfs             | Only run in special circumstances                                           |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C48_S2SW                 | gfs             |  extended  |     X      |     X      |     X      |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C48_S2SWA_gefs           | gefs            |     X      |     X      |     X      |     X      |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C48_S2SWA_gefs_RT        | gefs            | Only run in special circumstances                                           |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C48_gsienkf_atmDA        | gfs, gsi        |     X      |     X      |            |     X      |            |            |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C48_ufsenkf_atmDA        | gfs, gdas       |     X      |     X      |            |     X      |            |            |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C48mx500_3DVarAOWCDA     | gfs, gsi, gdas  |     X      |     X      |     X      |     X      |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C48mx500_hybAOWCDA       | gfs, gsi, gdas  |     X      |     X      |     X      |     X      |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C96C48_hybatmDA          | gfs, gsi        |     X      |     X      |     X      |     X      |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C96C48_hybatmsnowDA      | gfs, gsi, gdas  |     X      |     X      |            |     X      |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C96C48_hybatmsoilDA      | gfs, gsi, gdas  |     X      |     X      |            |     X      |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C96C48_ufs_hybatmDA      | gfs, gdas       |     X      |     X      |            |            |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C96C48_ufsgsi_hybatmDA   | gfs, gdas       |     X      |     X      |            |            |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C96C48mx500_S2SW_cyc_gfs | gfs, gsi, gdas  |     X      |     X      |     X      |     X      |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C96_atm3DVar             | gfs, gsi        |  extended  |     X      |     X      |     X      |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C96_gcafs_cycled         | gcafs, gdas     |     X      |     X      |     X      |     X      |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C96_gcafs_cycled_noDA    | gcafs           |     X      |     X      |            |     X      |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+
| C96mx100_S2S             | sfs             |     X      |     X      |     X      |     X      |     X      |     X      |
+--------------------------+-----------------+------------+------------+------------+------------+------------+------------+

Instead of specifying tests with ``-y``, you can also instead run all tests in a family with the following options (multiple can be specified):

-G  Run all GFS cases
-E  Run all GEFS cases
-C  Run all GCADS cases
-S  Run all SFS cases

Specifying ``-GECS`` would run all tests valid for the current system.
