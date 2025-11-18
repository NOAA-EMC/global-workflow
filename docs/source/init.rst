==================
Initial Conditions
==================

There are two types of initial conditions for the global-workflow:

#. Warm start: these ICs are taken directly from either the GFS in production or an experiment "warmed" up (at least one cycle in).
#. Cold start: any ICs converted to a new resolution or grid (e.g. C768 -> C384). These ICs are often prepared by chgres_cube (change resolution utility).

Most users will initiate their experiments with cold start ICs unless running high resolution (C768 deterministic with C384 EnKF) for a date with warm starts available. It is `not recommended` to run high resolution unless required or as part of final testing.

Atmosphere Resolutions:

* C48 = 2 degree ≈ 200km
* C96 = 1 degree ≈ 100km
* C192 = 1/2 degree ≈ 50km
* C384 = 1/4 degree ≈ 25km
* C768 = 1/8 degree ≈ 13km
* C1152 ≈ 9km
* C3072 ≈ 3km

Supported atmosphere resolutions in global-workflow: C48, C96, C192, C384, C768

Ocean Resolutions:

* mx500 = 5 degree
* mx100 = 1 degree
* mx050 = 1/2 degree
* mx025 = 1/4 degree

Supported ocean resolutions in global-workflow: mx500, mx100

^^^^^^^^^^^^^^^^^^^^^^^^^
Staged Initial Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^

* :ref:`Cycled ATM-only<staged_ics_cycled_atmonly>`
* :ref:`Cycled ATM w/ Coupled (S2S) model<staged_ics_cycled_coupled>`
* :ref:`Prototype<staged_ics_prototype>`

.. _staged_ics_cycled_atmonly:

***************
Cycled ATM-only
***************

Cold-start atmosphere-only cycled C96 deterministic C48 enkf (80 members) ICs are available in the following locations on supported platforms:

::

   Ursa/Hera: /scratch3/NCEPDEV/global/role.glopara/data/ICSDIR/C96C48
   Orion/Hercules: /work/noaa/global/glopara/data/ICSDIR/C96C48
   WCOSS2: /lfs/h2/emc/global/noscrub/emc.global/data/ICSDIR/C96C48
   AWS: https://noaa-nws-global-pds.s3.amazonaws.com/index.html#data/ICSDIR/C96C48

Start date = 2021122018

::

   -bash-4.2$ tree /scratch3/NCEPDEV/global/role.glopara/data/ICSDIR/C96C48/
   |-- enkfgdas.20211220
   |   `-- 18
   |       |-- mem### (where ### = 001 -> 080)
   |       |   `-- atmos
   |       |       `-- INPUT
   |       |           |-- gfs_ctrl.nc
   |       |           |-- gfs_data.tile1.nc
   |       |           |-- gfs_data.tile2.nc
   |       |           |-- gfs_data.tile3.nc
   |       |           |-- gfs_data.tile4.nc
   |       |           |-- gfs_data.tile5.nc
   |       |           |-- gfs_data.tile6.nc
   |       |           |-- sfc_data.tile1.nc
   |       |           |-- sfc_data.tile2.nc
   |       |           |-- sfc_data.tile3.nc
   |       |           |-- sfc_data.tile4.nc
   |       |           |-- sfc_data.tile5.nc
   |       |           `-- sfc_data.tile6.nc
   `-- gdas.20211220
       `-- 18
           `-- atmos
               |-- INPUT
               |   |-- gfs_ctrl.nc
               |   |-- gfs_data.tile1.nc
               |   |-- gfs_data.tile2.nc
               |   |-- gfs_data.tile3.nc
               |   |-- gfs_data.tile4.nc
               |   |-- gfs_data.tile5.nc
               |   |-- gfs_data.tile6.nc
               |   |-- sfc_data.tile1.nc
               |   |-- sfc_data.tile2.nc
               |   |-- sfc_data.tile3.nc
               |   |-- sfc_data.tile4.nc
               |   |-- sfc_data.tile5.nc
               |   `-- sfc_data.tile6.nc
               |-- gdas.t18z.abias.txt
               |-- gdas.t18z.abias_air.txt
               |-- gdas.t18z.abias_pc.txt
               `-- gdas.t18z.radstat.tar

.. _staged_ics_cycled_coupled:

*********************************
Cycled ATM w/ Coupled (S2S) model
*********************************

Warm-start cycled w/ coupled (S2S) model C48 atmosphere C48 enkf (80 members) 5 degree ocean/ice ICs are available in the following locations on supported platforms:

::

   Ursa/Hera: /scratch3/NCEPDEV/global/role.glopara/data/ICSDIR/C48C48mx500
   Orion/Hercules: /work/noaa/global/glopara/data/ICSDIR/C48C48mx500
   WCOSS2: /lfs/h2/emc/global/noscrub/emc.global/data/ICSDIR/C48C48mx500
   AWS: https://noaa-nws-global-pds.s3.amazonaws.com/index.html#data/ICSDIR/C48C48mx500

Start date = 2021032312

.. note::
   The EnKF member ICs are dummy duplicates of the deterministic at the moment.

::

   -bash-4.2$  tree /scratch3/NCEPDEV/global/role.glopara/data/ICSDIR/C48C48mx500
   ├── enkfgdas.20210323
   │   ├── 06
   │   │   ├── mem001
   │   │   │   └── model -> ../../../gdas.20210323/06/model
   │   │   ├── mem002
   │   │   │   └── model -> ../../../gdas.20210323/06/model
   │   │   ├── mem003
   │   │   │   └── model -> ../../../gdas.20210323/06/model
   ...
   │   │   └── mem080
   │   │       └── model -> ../../../gdas.20210323/06/model
   │   └── 12
   │       ├── mem001
   │       │   └── analysis
   │       │       └── ocean
   │       │           └── gdas.t12z.mom6_increment.i006.nc -> ../../../../../gdas.20210323/12/analysis/ocean/gdas.t12z.mom6_increment.i006.nc
   │       ├── mem002
   │       │   └── analysis
   │       │       └── ocean
   │       │           └── gdas.t12z.mom6_increment.i006.nc -> ../../../../../gdas.20210323/12/analysis/ocean/gdas.t12z.mom6_increment.i006.nc
   │       ├── mem003
   │       │   └── analysis
   │       │       └── ocean
   │       │           └── gdas.t12z.mom6_increment.i006.nc -> ../../../../../gdas.20210323/12/analysis/ocean/gdas.t12z.mom6_increment.i006.nc
   ...
   │       └── mem080
   │           └── analysis
   │               └── ocean
   │                   └── gdas.t12z.mom6_increment.i006.nc -> ../../../../../gdas.20210323/12/analysis/ocean/gdas.t12z.mom6_increment.i006.nc
   └── gdas.20210323
       ├── 06
       │   └── model
       │       ├── atmos
       │       │   └── restart
       │       │       ├── 20210323.120000.ca_data.tile1.nc
       │       │       ├── 20210323.120000.ca_data.tile2.nc
       │       │       ├── 20210323.120000.ca_data.tile3.nc
       │       │       ├── 20210323.120000.ca_data.tile4.nc
       │       │       ├── 20210323.120000.ca_data.tile5.nc
       │       │       ├── 20210323.120000.ca_data.tile6.nc
       │       │       ├── 20210323.120000.coupler.res
       │       │       ├── 20210323.120000.fv_core.res.nc
       │       │       ├── 20210323.120000.fv_core.res.tile1.nc
       │       │       ├── 20210323.120000.fv_core.res.tile2.nc
       │       │       ├── 20210323.120000.fv_core.res.tile3.nc
       │       │       ├── 20210323.120000.fv_core.res.tile4.nc
       │       │       ├── 20210323.120000.fv_core.res.tile5.nc
       │       │       ├── 20210323.120000.fv_core.res.tile6.nc
       │       │       ├── 20210323.120000.fv_srf_wnd.res.tile1.nc
       │       │       ├── 20210323.120000.fv_srf_wnd.res.tile2.nc
       │       │       ├── 20210323.120000.fv_srf_wnd.res.tile3.nc
       │       │       ├── 20210323.120000.fv_srf_wnd.res.tile4.nc
       │       │       ├── 20210323.120000.fv_srf_wnd.res.tile5.nc
       │       │       ├── 20210323.120000.fv_srf_wnd.res.tile6.nc
       │       │       ├── 20210323.120000.fv_tracer.res.tile1.nc
       │       │       ├── 20210323.120000.fv_tracer.res.tile2.nc
       │       │       ├── 20210323.120000.fv_tracer.res.tile3.nc
       │       │       ├── 20210323.120000.fv_tracer.res.tile4.nc
       │       │       ├── 20210323.120000.fv_tracer.res.tile5.nc
       │       │       ├── 20210323.120000.fv_tracer.res.tile6.nc
       │       │       ├── 20210323.120000.phy_data.tile1.nc
       │       │       ├── 20210323.120000.phy_data.tile2.nc
       │       │       ├── 20210323.120000.phy_data.tile3.nc
       │       │       ├── 20210323.120000.phy_data.tile4.nc
       │       │       ├── 20210323.120000.phy_data.tile5.nc
       │       │       ├── 20210323.120000.phy_data.tile6.nc
       │       │       ├── 20210323.120000.sfc_data.tile1.nc
       │       │       ├── 20210323.120000.sfc_data.tile2.nc
       │       │       ├── 20210323.120000.sfc_data.tile3.nc
       │       │       ├── 20210323.120000.sfc_data.tile4.nc
       │       │       ├── 20210323.120000.sfc_data.tile5.nc
       │       │       └── 20210323.120000.sfc_data.tile6.nc
       │       ├── ice
       │       │   └── restart
       │       │       └── 20210323.120000.cice_model.res.nc
       │       ├── med
       │       │   └── restart
       │       │       └── 20210323.120000.ufs.cpld.cpl.r.nc
       │       └── ocean
       │           └── restart
       │               └── 20210323.120000.MOM.res.nc
       └── 12
           └── analysis
               ├── atmos
               │   ├── gdas.t12z.abias.txt
               │   ├── gdas.t12z.abias_air.txt
               │   ├── gdas.t12z.abias_int.txt
               │   ├── gdas.t12z.abias_pc.txt
               │   └── gdas.t12z.radstat.tar
               └── ocean
                   └── gdas.t12z.mom6_increment.i006.nc

^^^^^^^^^^^^^^^^^^^^^^^^^^
Prepare Initial Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _automated-generation:

********************
Automated Generation
********************

.. _cycled:

-----------
Cycled mode
-----------

Not yet supported. See the UFS_UTILS documentation on the gdas_init utility to generate your own ICs for cycled or forecast-only mode: https://noaa-emcufs-utils.readthedocs.io/en/latest/ufs_utils.html#gdas-init

.. _forecastonly-coupled:

---------------------
Forecast-only coupled
---------------------
Coupled initial conditions are currently only generated offline and copied prior to the forecast run. Prototype initial conditions will automatically be used when setting up an experiment as an S2SW app, there is no need to do anything additional. Sample copies of initial conditions from the prototype runs are currently maintained on Ursa, Orion/Hercules, and WCOSS2 for CI testing.  The paths on each machine can be found in ``dev/ci/platforms/config.<host>``.

.. _forecastonly-atmonly:

-----------------------------
Forecast-only mode (atm-only)
-----------------------------

The table below lists for reference the needed initial condition files from past GFS versions to be used by the UFS_UTILS gdas_init utility. The utility will pull these files for you. See the next section (Manual Generation) for how to run the UFS_UTILS gdas_init utility and create initial conditions for your experiment.

Note for table: yyyy=year; mm=month; dd=day; hh=cycle

Operations/production output location on HPSS: /NCEPPROD/hpssprod/runhistory/rh ``yyyy``/``yyyymm``/``yyyymmdd``/

+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| Source         | Files                           | Tarball name                                                                | Where in ROTDIR                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v12 ops        |   gfs.t. ``hh`` z.sanl          | com_gfs_prod_gfs. ``yyyymmddhh`` .anl.tar                                   | gfs. ``yyyymmdd`` /``hh``      |
|                |                                 |                                                                             |                                |
|                |   gfs.t. ``hh`` z.sfcanl        |                                                                             |                                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v13 ops        |   gfs.t. ``hh`` z.sanl          | com2_gfs_prod_gfs. ``yyyymmddhh`` .anl.tar                                  | gfs. ``yyyymmdd`` /``hh``      |
|                |                                 |                                                                             |                                |
|                |   gfs.t. ``hh`` z.sfcanl        |                                                                             |                                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v14 ops        |   gfs.t. ``hh`` z.atmanl.nemsio | gpfs_hps_nco_ops_com_gfs_prod_gfs. ``yyyymmddhh`` .anl.tar                  | gfs. ``yyyymmdd`` /``hh``      |
|                |                                 |                                                                             |                                |
|                |   gfs.t. ``hh`` z.sfcanl.nemsio |                                                                             |                                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v15 ops        |   gfs.t. ``hh`` z.atmanl.nemsio | gpfs_dell1_nco_ops_com_gfs_prod_gfs. ``yyyymmdd`` _ ``hh`` .gfs_nemsioa.tar | gfs. ``yyyymmdd`` /``hh``      |
|                |                                 |                                                                             |                                |
| pre-2020022600 |   gfs.t. ``hh`` z.sfcanl.nemsio |                                                                             |                                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v15 ops        |   gfs.t. ``hh`` z.atmanl.nemsio | com_gfs_prod_gfs. ``yyyymmdd`` _ ``hh`` .gfs_nemsioa.tar                    | gfs. ``yyyymmdd`` /``hh``      |
|                |                                 |                                                                             |                                |
|                |   gfs.t. ``hh`` z.sfcanl.nemsio |                                                                             |                                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v16 retro      |   gfs.t. ``hh`` z.atmanl.nc     | gfs_netcdfa.tar*                                                            | gfs. ``yyyymmdd`` /``hh``/atmos|
|                |                                 |                                                                             |                                |
|                |   gfs.t. ``hh`` z.sfcanl.nc     |                                                                             |                                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v16.0[1] ops   |   gfs.t. ``hh`` z.atmanl.nc     | com_gfs_prod_gfs. ``yyyymmdd`` _ ``hh`` .gfs_nca.tar                        | gfs. ``yyyymmdd`` /``hh``/atmos|
|                |                                 |                                                                             |                                |
|                |   gfs.t. ``hh`` z.sfcanl.nc     |                                                                             |                                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v16.2[3]+ ops  |   gfs.t. ``hh`` z.atmanl.nc     | com_gfs\_ ``gfs_ver`` _gfs. ``yyyymmdd`` _ ``hh`` .gfs_nca.tar              | gfs. ``yyyymmdd`` /``hh``/atmos|
|                |                                 |                                                                             |                                |
|                |   gfs.t. ``hh`` z.sfcanl.nc     |                                                                             |                                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+

For HPSS path, see retrospective table in :ref:`pre-production parallel section <retrospective>` below

.. _manual-generation:

*****************
Manual Generation
*****************

.. _coldstarts:

----------------------------------------
Cold-start Initial Conditions Generation
----------------------------------------

The following information is for users needing to generate cold-start initial conditions for a cycled experiment that will run at a different resolution or layer amount than the operational GFS (C1152C384L127).

The ``chgres_cube`` code is available from the `UFS_UTILS repository <https://github.com/ufs-community/UFS_UTILS>`_ on GitHub and can be used to convert GFS ICs to a different resolution or number of layers. Users should see the `documentation to generation initial conditions in the UFS_UTILS repository <https://noaa-emcufs-utils.readthedocs.io/en/latest/ufs_utils.html#gdas-init>`_. The ``chgres_cube`` code/scripts currently support the following GFS inputs:

* pre-GFSv14
* GFSv14
* GFSv15
* GFSv16

See instructions in UFS_UTILS to clone, build and generate initial conditions: https://noaa-emcufs-utils.readthedocs.io/en/latest/ufs_utils.html#gdas-init

.. _warmstarts:

----------------------------------------
Warm-start Initial Conditions Generation
----------------------------------------
Warm-start initial conditions are taken from either the GFS in production or an experiment "warmed" up (at least one cycle in). Below is a list of tarballs required for a warm-start cycled experiment.

ATM:
* Previous cycle:
  * enkfgdas_restartb_grp#.tar (where # = ensemble group number, 1-8 for 80 members at C384)
  * gdas_restartb.tar
* Current cycle:
  * enkfgdas_restarta_grp#.tar (where # = ensemble group number, 1-8 for 80 members at C384)
  * gdas_restarta.tar

Ocean/Ice:
* Previous cycle:
  * gdasocean_restart.tar
* Current cycle:
  * gdasocean_analysis.tar

Waves:
* Previous cycle:
  * gdaswave_restart.tar


If you are restarting an experiment that you are currently running (e.g. you had a failure and need to rewind a few cycles), then untar these tarballs directly into your ROTDIR.

If you are starting a new experiment that requires warm-start initial conditions, retrieve them into a separate directory and then, when running ``setup_expt.py``, point the ``--icsdir`` argument to that directory so that the experiment setup can link to the correct files.

Two utility scripts are available to assist with warm restart processing for retrospective and realtime tests: :ref:`get_warm_s2sw_restart_tarballs.sh<warmstart-utility-scripts>` for retrieving and extracting tarballs from HPSS, and :ref:`make_ee2_links.sh<warmstart-utility-scripts>` for converting old (pre-EE2) filenames to EE2-compliant names by creating symbolic links.

.. _warmstart-utility-scripts:

-------------------------------------------
Utility Scripts for Warm Restart Processing
-------------------------------------------

Two utility scripts are available to assist with warm restart processing for retrospective and realtime tests:

**get_warm_s2sw_restart_tarballs.sh**

This script automates the retrieval and extraction of warm restart tarballs from HPSS for fully-coupled (S2S/S2SW) experiments.

* **Location:** ``dev/ush/get_warm_s2sw_restart_tarballs.sh``
* **Platform Support:** Currently only supported on Gaea C6
* **Experiment Types:** Fully-coupled experiments only (atmosphere-ocean-ice)
* **Usage:**

  ::

    get_warm_s2sw_restart_tarballs.sh YYYYMMDDHH HPSS_ROOT_DIR UNTAR_DIR HPC_ACCOUNT

  Where:

  - ``YYYYMMDDHH``: Starting cycle in YYYYMMDDHH format
  - ``HPSS_ROOT_DIR``: Root directory on HPSS where tarballs are stored
  - ``UNTAR_DIR``: Local directory where tarballs will be extracted
  - ``HPC_ACCOUNT``: HPC account for sbatch jobs

* **Description:** The script submits SLURM jobs to retrieve and extract restart tarballs from HPSS for both the starting cycle and the previous cycle (6 hours earlier). It handles ensemble groups and various restart components (atmosphere, ocean, wave).

* **Limitations:**

  - Currently only works on Gaea C6 with HPSS access
  - Only supports fully-coupled experiments
  - Assumes 80 ensemble members at C384 resolution
  - Additional work needed to support atmosphere-only or other partially coupled configurations

**make_ee2_links.sh**

This script converts filenames from the older (pre-EE2) naming convention to EE2-compliant names by creating symbolic links.

* **Location:** ``dev/ush/make_ee2_links.sh``
* **When to Use:** Only use this script for retrospective and realtime tests that use old (pre-EE2) filenames. Do NOT use for tests already using EE2-compliant filenames.
* **Usage:**

  ::

    make_ee2_links.sh <target_directory>

  Where ``<target_directory>`` is the directory containing your warm restart files (typically your ROTDIR).

* **Description:** The script scans the target directory for RUN.YYYYMMDD directories, and creates symbolic links to convert old filename formats to EE2-compliant names. This includes:

  - Analysis files (atmanl, sfcanl, ocninc, etc.)
  - Increment files (atminc, sfcinc, etc.)
  - Statistics files (radstat, cnvstat, gsistat, etc.)
  - Bias files (abias, abias_air, abias_pc, etc.)

* **Important Notes:**

  - The script only creates links needed to restart an existing experiment
  - Does not create all possible EE2-compatible links
  - Will not overwrite existing data files
  - Includes error checking to prevent data loss

* **Example Workflow:**

  After extracting warm restart tarballs using ``get_warm_s2sw_restart_tarballs.sh`` (or manually from HPSS), run the link script if your restart files use the old naming convention::

    cd $ROTDIR
    /path/to/global-workflow/dev/ush/make_ee2_links.sh .
