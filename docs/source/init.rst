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
               |-- gdas.t18z.abias
               |-- gdas.t18z.abias_air
               |-- gdas.t18z.abias_pc
               `-- gdas.t18z.radstat

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
   │       │           └── gdas.t12z.ocninc.nc -> ../../../../../gdas.20210323/12/analysis/ocean/gdas.t12z.ocninc.nc
   │       ├── mem002
   │       │   └── analysis
   │       │       └── ocean
   │       │           └── gdas.t12z.ocninc.nc -> ../../../../../gdas.20210323/12/analysis/ocean/gdas.t12z.ocninc.nc
   │       ├── mem003
   │       │   └── analysis
   │       │       └── ocean
   │       │           └── gdas.t12z.ocninc.nc -> ../../../../../gdas.20210323/12/analysis/ocean/gdas.t12z.ocninc.nc
   ...
   │       └── mem080
   │           └── analysis
   │               └── ocean
   │                   └── gdas.t12z.ocninc.nc -> ../../../../../gdas.20210323/12/analysis/ocean/gdas.t12z.ocninc.nc
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
               │   ├── gdas.t12z.abias
               │   ├── gdas.t12z.abias_air
               │   ├── gdas.t12z.abias_int
               │   ├── gdas.t12z.abias_pc
               │   └── gdas.t12z.radstat
               └── ocean
                   └── gdas.t12z.ocninc.nc

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

The following information is for users needing to generate cold-start initial conditions for a cycled experiment that will run at a different resolution or layer amount than the operational GFS (C1152C384L127).

The ``chgres_cube`` code is available from the `UFS_UTILS repository <https://github.com/ufs-community/UFS_UTILS>`_ on GitHub and can be used to convert GFS ICs to a different resolution or number of layers. Users should see the `documentation to generation initial conditions in the UFS_UTILS repository <https://noaa-emcufs-utils.readthedocs.io/en/latest/ufs_utils.html#gdas-init>`_. The ``chgres_cube`` code/scripts currently support the following GFS inputs:

* pre-GFSv14
* GFSv14
* GFSv15
* GFSv16

See instructions in UFS_UTILS to clone, build and generate initial conditions: https://noaa-emcufs-utils.readthedocs.io/en/latest/ufs_utils.html#gdas-init

.. _warmstarts-prod:

.. note::
   **DEPRECATED:** Warm starts from production are no longer supported. Please use warm starts from pre-production parallels or retrospective tests instead. See the sections below for guidance.

.. _warmstarts-preprod-parallels:

*******************************************
Warm starts (from pre-production parallels)
*******************************************

Recent pre-implementation parallel series was for GFS v16 (implemented March 2021). For the prior v15 (Q2FY19) see an additional table below.

* **What resolution are warm-starts available for?** Warm-start ICs are saved at the resolution the model was run at (C768/C384) and can only be used to run at the same resolution combination. If you need to run a different resolution you will need to make your own cold-start ICs. See cold start section above.
* **What dates have warm-start files saved?** Unfortunately the frequency changed enough during the runs that it’s not easy to provide a definitive list easily.
* **What files?** All warm-starts are saved in separate tarballs which include “restart” in the name. You need to pull the entirety of each tarball, all files included in the restart tarballs are needed.
* **Where are these tarballs?** See below for the location on HPSS for each v16 pre-implementation parallel.
* **What tarballs do I need to grab for my experiment?** Tarballs from two cycles are required. The tarballs are listed below, where $PDY$cyc is your starting cycle and $gPDY$gcyc is one cycle prior.

  - Forecast-only
    + ../$PDY$cyc/gfs_restarta.tar
    + ../$gPDY$gcyc/gdas_restartb.tar
  - Cycled w/EnKF
    + ../$PDY$cyc/gdas_restarta.tar
    + ../$PDY$cyc/enkfgdas_restarta_grp##.tar (where ## is 01 through 08) (note, older tarballs may include a period between enkf and gdas: "enkf.gdas")
    + ../$gPDY$gcyc/gdas_restartb.tar
    + ../$gPDY$gcyc/enkfgdas_restartb_grp##.tar (where ## is 01 through 08) (note, older tarballs may include a period between enkf and gdas: "enkf.gdas")

* **Where do I put the warm-start initial conditions?** Extraction should occur right inside your ROTDIR. You may need to rename the enkf folder (enkf.gdas.$PDY -> enkfgdas.$PDY).

Due to a recent change in the dycore, you may also need an additional offline step to fix the checksum of the NetCDF files for warm start. See the :ref:`Fix netcdf checksum section <gfsv17-checksum>`.
The current model has undergone several updates and the files generated may not be completely usable by the model.

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

* **Description:** The script scans the target directory for gdas, gfs, gcdas, gcafs, enkfgdas, enkfgfs, and enkfgcdas directories, and creates symbolic links to convert old filename formats to EE2-compliant names. This includes:

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

.. _retrospective:

--------------------------------------------------------------
GFSv16 (March 2021) Pre-Implementation Parallel HPSS Locations
--------------------------------------------------------------

+-----------------------------+---------------+--------------------------------------------------+
| Time Period                 | Parallel Name | Archive Location on HPSS                         |
|                             |               | PREFIX=/NCEPDEV/emc-global/5year/emc.glopara     |
+-----------------------------+---------------+--------------------------------------------------+
| 2019050106 ~ 2019060100     | v16retro0e    | $PREFIX/WCOSS_D/gfsv16/v16retro0e/``yyyymmddhh`` |
+-----------------------------+---------------+--------------------------------------------------+
| 2019060106 ~ 2019083118     | v16retro1e    | $PREFIX/WCOSS_D/gfsv16/v16retro1e/``yyyymmddhh`` |
+-----------------------------+---------------+--------------------------------------------------+
| 2019090100 ~ 2019110918     | v16retro2e    | $PREFIX/WCOSS_D/gfsv16/v16retro2e/``yyyymmddhh`` |
+-----------------------------+---------------+--------------------------------------------------+
| 2019111000 ~ 2020122200     | v16rt2        | $PREFIX/WCOSS_D/gfsv16/v16rt2/``yyyymmddhh``     |
+-----------------------------+---------------+--------------------------------------------------+
| 2020122206 ~ implementation | v16rt2n       | $PREFIX/WCOSS_D/gfsv16/v16rt2n/``yyyymmddhh``    |
+-----------------------------+---------------+--------------------------------------------------+

----------------------------------------------------------
GFSv15 (Q2FY19) Pre-Implementation Parallel HPSS Locations
----------------------------------------------------------

+---------------------+-----------------+-----------------------------------------------------------+
| Time Period         | Parallel Name   | Archive Location on HPSS                                  |
|                     |                 | PREFIX=/NCEPDEV/emc-global/5year                          |
+---------------------+-----------------+-----------------------------------------------------------+
| 20180525 - 20190612 | prfv3rt1        | $PREFIX/emc.glopara/WCOSS_C/Q2FY19/prfv3rt1               |
+---------------------+-----------------+-----------------------------------------------------------+
| 20171125 - 20170831 | fv3q2fy19retro1 | $PREFIX/Fanglin.Yang/WCOSS_DELL_P3/Q2FY19/fv3q2fy19retro1 |
+---------------------+-----------------+-----------------------------------------------------------+
| 20170525 - 20170625 | fv3q2fy19retro2 | $PREFIX/emc.glopara/WCOSS_C/Q2FY19/fv3q2fy19retro2        |
+---------------------+-----------------+-----------------------------------------------------------+
| 20170802 - 20171130 | fv3q2fy19retro2 | $PREFIX/Fanglin.Yang/WCOSS_DELL_P3/Q2FY19/fv3q2fy19retro2 |
+---------------------+-----------------+-----------------------------------------------------------+
| 20161125 - 20170531 | fv3q2fy19retro3 | $PREFIX/Fanglin.Yang/WCOSS_DELL_P3/Q2FY19/fv3q2fy19retro3 |
+---------------------+-----------------+-----------------------------------------------------------+
| 20160817 - 20161130 | fv3q2fy19retro4 | $PREFIX/emc.glopara/WCOSS_DELL_P3/Q2FY19/fv3q2fy19retro4  |
+---------------------+-----------------+-----------------------------------------------------------+
| 20160522 - 20160825 | fv3q2fy19retro4 | $PREFIX/emc.glopara/WCOSS_C/Q2FY19/fv3q2fy19retro4        |
+---------------------+-----------------+-----------------------------------------------------------+
| 20151125 - 20160531 | fv3q2fy19retro5 | $PREFIX/emc.glopara/WCOSS_DELL_P3/Q2FY19/fv3q2fy19retro5  |
+---------------------+-----------------+-----------------------------------------------------------+
| 20150503 - 20151130 | fv3q2fy19retro6 | $PREFIX/emc.glopara/WCOSS_DELL_P3/Q2FY19/fv3q2fy19retro6  |
+---------------------+-----------------+-----------------------------------------------------------+

.. _gfsv17-warmstarts:

***************************************
Using pre-GFSv17 warm starts for GFSv17
***************************************

If a user wishes to run a high-res (C1152C384L127) GFSv17 experiment with warm starts from the operational GFSv16 (or older) warm starts, they must process the initial condition files before using. See details below in the :ref:`Fix netcdf checksum section <gfsv17-checksum>`.

.. _gfsv17-checksum:

-------------------------
Fix NetCDF checksum issue
-------------------------

Due to a recent change in UFS, the setting to bypass the data verification no longer works, so you may also need an additional offline step to delete the checksum of the NetCDF files for warm start:

On RDHPCS:

::

   module load nco/4.9.3

On WCOSS2:

::

   module load intel/19.1.3.304
   module load netcdf/4.7.4
   module load udunits/2.2.28
   module load gsl/2.7
   module load nco/4.7.9

And then on all platforms:

::

   cd $ROTDIR
   for f in $(find ./ -name *tile*.nc); do echo $f; ncatted -a checksum,,d,, $f; done
