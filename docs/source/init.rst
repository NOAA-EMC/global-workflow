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

   Hera: /scratch1/NCEPDEV/global/glopara/data/ICSDIR/C96C48
   Orion/Hercules: /work/noaa/global/glopara/data/ICSDIR/C96C48
   WCOSS2: /lfs/h2/emc/global/noscrub/emc.global/data/ICSDIR/C96C48
   AWS: https://noaa-nws-global-pds.s3.amazonaws.com/index.html#data/ICSDIR/C96C48

Start date = 2021122018

::

   -bash-4.2$ tree /scratch1/NCEPDEV/global/glopara/data/ICSDIR/C96C48/
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

   Hera: /scratch1/NCEPDEV/global/glopara/data/ICSDIR/C48C48mx500
   Orion/Hercules: /work/noaa/global/glopara/data/ICSDIR/C48C48mx500
   WCOSS2: /lfs/h2/emc/global/noscrub/emc.global/data/ICSDIR/C48C48mx500
   Jet: /lfs5/HFIP/hfv3gfs/glopara/data/ICSDIR/C48C48mx500
   AWS: https://noaa-nws-global-pds.s3.amazonaws.com/index.html#data/ICSDIR/C48C48mx500

Start date = 2021032312

.. note::
   The EnKF member ICs are dummy duplicates of the deterministic at the moment.

::

   -bash-4.2$  tree /scratch1/NCEPDEV/global/glopara/data/ICSDIR/C48C48mx500
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

.. _staged_ics_prototype:

*********
Prototype
*********

Forecast-only P8 prototype initial conditions are made available to users on supported platforms in the following locations:

::

    WCOSS2: /lfs/h2/emc/global/noscrub/emc.global/IC/COUPLED
    HERA: /scratch1/NCEPDEV/climate/role.ufscpara/IC
    ORION/Hercules: /work/noaa/global/glopara/data/ICSDIR/prototype_ICs
    JET: /mnt/lfs5/HFIP/hfv3gfs/glopara/data/ICSDIR/prototype_ICs
    S4: /data/prod/glopara/coupled_ICs

These locations are known within the workflow via paths set in ``parm/config/config.coupled_ic``.

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
Coupled initial conditions are currently only generated offline and copied prior to the forecast run. Prototype initial conditions will automatically be used when setting up an experiment as an S2SW app, there is no need to do anything additional. Sample copies of initial conditions from the prototype runs are currently maintained on Hera, Orion/Hercules, Jet, and WCOSS2. The locations used are determined by ``parm/config/config.stage_ic``.
Note however, that due to the rapid changes in the model configuration, some staged initial conditions may not work.

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

.. note::
   Initial conditions cannot be generated on S4. These must be generated on another supported platform then pushed to S4. If you do not have access to a supported system or need assistance, please contact Innocent Souopgui (innocent.souopgui@noaa.gov).

.. _coldstarts:

The following information is for users needing to generate cold-start initial conditions for a cycled experiment that will run at a different resolution or layer amount than the operational GFS (C768C384L127).

The ``chgres_cube`` code is available from the `UFS_UTILS repository <https://github.com/ufs-community/UFS_UTILS>`_ on GitHub and can be used to convert GFS ICs to a different resolution or number of layers. Users should see the `documentation to generation initial conditions in the UFS_UTILS repository <https://noaa-emcufs-utils.readthedocs.io/en/latest/ufs_utils.html#gdas-init>`_. The ``chgres_cube`` code/scripts currently support the following GFS inputs:

* pre-GFSv14
* GFSv14
* GFSv15
* GFSv16

See instructions in UFS_UTILS to clone, build and generate initial conditions: https://noaa-emcufs-utils.readthedocs.io/en/latest/ufs_utils.html#gdas-init

.. _warmstarts-prod:

*****************************
Warm starts (from production)
*****************************

Output and warm start initial conditions from the operational GFS (FV3GFS) are saved on HPSS. Users can pull these warm start initial conditions from tape for their use in running operational resolution experiments.

See production output in the following location on HPSS:

``/NCEPPROD/hpssprod/runhistory/rhYYYY/YYYYMM/YYYYMMDD``

Example location for January 2nd 2023:

``/NCEPPROD/hpssprod/runhistory/rh2023/202301/20230102``

Example listing for January 2nd 2023 00z (2023010200) production tarballs:

::

  -bash-4.2$ hpsstar dir /NCEPPROD/hpssprod/runhistory/rh2023/202301/20230102 | grep gfs | grep _00. | grep -v idx
  [connecting to hpsscore1.fairmont.rdhpcs.noaa.gov/1217]
  -rw-r-----    1 nwprod    rstprod  34824086016 Jan  4 03:31 com_gfs_v16.3_enkfgdas.20230102_00.enkfgdas.tar
  -rw-r--r--    1 nwprod    prod     219779890688 Jan  4 04:04 com_gfs_v16.3_enkfgdas.20230102_00.enkfgdas_restart_grp1.tar
  -rw-r--r--    1 nwprod    prod     219779921408 Jan  4 04:13 com_gfs_v16.3_enkfgdas.20230102_00.enkfgdas_restart_grp2.tar
  -rw-r--r--    1 nwprod    prod     219775624192 Jan  4 04:23 com_gfs_v16.3_enkfgdas.20230102_00.enkfgdas_restart_grp3.tar
  -rw-r--r--    1 nwprod    prod     219779726848 Jan  4 04:33 com_gfs_v16.3_enkfgdas.20230102_00.enkfgdas_restart_grp4.tar
  -rw-r--r--    1 nwprod    prod     219777990656 Jan  4 04:42 com_gfs_v16.3_enkfgdas.20230102_00.enkfgdas_restart_grp5.tar
  -rw-r--r--    1 nwprod    prod     219780963328 Jan  4 04:52 com_gfs_v16.3_enkfgdas.20230102_00.enkfgdas_restart_grp6.tar
  -rw-r--r--    1 nwprod    prod     219775471104 Jan  4 05:02 com_gfs_v16.3_enkfgdas.20230102_00.enkfgdas_restart_grp7.tar
  -rw-r--r--    1 nwprod    prod     219779499008 Jan  4 05:11 com_gfs_v16.3_enkfgdas.20230102_00.enkfgdas_restart_grp8.tar
  -rw-r-----    1 nwprod    rstprod   2287770624 Jan  4 02:07 com_gfs_v16.3_gdas.20230102_00.gdas.tar
  -rw-r--r--    1 nwprod    prod      1026611200 Jan  4 02:07 com_gfs_v16.3_gdas.20230102_00.gdas_flux.tar
  -rw-r--r--    1 nwprod    prod     91233038336 Jan  4 02:16 com_gfs_v16.3_gdas.20230102_00.gdas_nc.tar
  -rw-r--r--    1 nwprod    prod     10865070592 Jan  4 02:08 com_gfs_v16.3_gdas.20230102_00.gdas_pgrb2.tar
  -rw-r-----    1 nwprod    rstprod  69913956352 Jan  4 02:11 com_gfs_v16.3_gdas.20230102_00.gdas_restart.tar
  -rw-r--r--    1 nwprod    prod     18200814080 Jan  4 02:17 com_gfs_v16.3_gdas.20230102_00.gdaswave_keep.tar
  -rw-r--r--    1 nwprod    prod      5493360128 Jan  4 02:18 com_gfs_v16.3_gfs.20230102_00.gfs.tar
  -rw-r--r--    1 nwprod    prod     62501531648 Jan  4 02:21 com_gfs_v16.3_gfs.20230102_00.gfs_flux.tar
  -rw-r--r--    1 nwprod    prod     121786191360 Jan  4 02:41 com_gfs_v16.3_gfs.20230102_00.gfs_nca.tar
  -rw-r--r--    1 nwprod    prod     130729495040 Jan  4 02:48 com_gfs_v16.3_gfs.20230102_00.gfs_ncb.tar
  -rw-r--r--    1 nwprod    prod     138344908800 Jan  4 02:29 com_gfs_v16.3_gfs.20230102_00.gfs_pgrb2.tar
  -rw-r--r--    1 nwprod    prod     59804635136 Jan  4 02:32 com_gfs_v16.3_gfs.20230102_00.gfs_pgrb2b.tar
  -rw-r--r--    1 nwprod    prod     25095460864 Jan  4 02:34 com_gfs_v16.3_gfs.20230102_00.gfs_restart.tar
  -rw-r--r--    1 nwprod    prod     21573020160 Jan  4 02:49 com_gfs_v16.3_gfs.20230102_00.gfswave_output.tar
  -rw-r--r--    1 nwprod    prod     32850422784 Jan  4 02:51 com_gfs_v16.3_gfs.20230102_00.gfswave_raw.tar
  -rw-r-----    1 nwprod    rstprod   7419548160 Jan  4 05:15 com_obsproc_v1.1_gfs.20230102_00.obsproc_gfs.tar

The warm starts and other output from production are at C768 deterministic and C384 EnKF. The warm start files must be converted to your desired resolution(s) using ``chgres_cube`` if you wish to run a different resolution. If you are running a C768C384L127 experiment you can use them as is.

------------------------------------------------------------------------------------------
What files should you pull for starting a new experiment with warm starts from production?
------------------------------------------------------------------------------------------

That depends on what mode you want to run -- forecast-only or cycled. Whichever mode, navigate to the top of your ``ROTDIR`` and pull the entirety of the tarball(s) listed below for your mode. The files within the tarball are already in the ``$RUN.$PDY/$CYC/$ATMOS`` folder format expected by the system.

For forecast-only there are two tarballs to pull

1. File #1 (for starting cycle SDATE):

::

  /NCEPPROD/hpssprod/runhistory/rhYYYY/YYYYMM/YYYYMMDD/com_gfs_vGFSVER_gfs.YYYYMMDD_CC.gfs_restart.tar

...where ``GFSVER`` is the version of the GFS (e.g. "16.3").

2. File #2 (for prior cycle GDATE=SDATE-06):

::

  /NCEPPROD/hpssprod/runhistory/rhYYYY/YYYYMM/YYYYMMDD/com_gfs_vGFSVER_gdas.YYYYMMDD_CC.gdas_restart.tar

...where ``GFSVER`` is the version of the GFS (e.g. "16.3").

For cycled mode there 18 tarballs to pull (9 for SDATE and 9 for GDATE (SDATE-06)):

::

    HPSS path: /NCEPPROD/hpssprod/runhistory/rhYYYY/YYYYMM/YYYYMMDD/

Tarballs per cycle:

::

   com_gfs_vGFSVER_gdas.YYYYMMDD_CC.gdas_restart.tar
   com_gfs_vGFSVER_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp1.tar
   com_gfs_vGFSVER_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp2.tar
   com_gfs_vGFSVER_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp3.tar
   com_gfs_vGFSVER_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp4.tar
   com_gfs_vGFSVER_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp5.tar
   com_gfs_vGFSVER_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp6.tar
   com_gfs_vGFSVER_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp7.tar
   com_gfs_vGFSVER_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp8.tar

Go to the top of your ``ROTDIR`` and pull the contents of all tarballs there. The tarballs already contain the needed directory structure.  Note that the directory structure has changed, so this may not be correct.

.. _warmstarts-preprod-parallels:

*******************************************
Warm starts (from pre-production parallels)
*******************************************

Recent pre-implementation parallel series was for GFS v16 (implemented March 2021). For the prior v15 (Q2FY19) see an additional table below.

* **What resolution are warm-starts available for?** Warm-start ICs are saved at the resolution the model was run at (C768/C384) and can only be used to run at the same resolution combination. If you need to run a different resolution you will need to make your own cold-start ICs. See cold start section above.
* **What dates have warm-start files saved?** Unfortunately the frequency changed enough during the runs that it’s not easy to provide a definitive list easily.
* **What files?** All warm-starts are saved in separate tarballs which include “restart” in the name. You need to pull the entirety of each tarball, all files included in the restart tarballs are needed.
* **Where are these tarballs?** See below for the location on HPSS for each v16 pre-implementation parallel.
* **What tarballs do I need to grab for my experiment?** Tarballs from two cycles are required. The tarballs are listed below, where $CDATE is your starting cycle and $GDATE is one cycle prior.

  - Forecast-only
    + ../$CDATE/gfs_restarta.tar
    + ../$GDATE/gdas_restartb.tar
  - Cycled w/EnKF
    + ../$CDATE/gdas_restarta.tar
    + ../$CDATE/enkfgdas_restarta_grp##.tar (where ## is 01 through 08) (note, older tarballs may include a period between enkf and gdas: "enkf.gdas")
    + ../$GDATE/gdas_restartb.tar
    + ../$GDATE/enkfgdas_restartb_grp##.tar (where ## is 01 through 08) (note, older tarballs may include a period between enkf and gdas: "enkf.gdas")

* **Where do I put the warm-start initial conditions?** Extraction should occur right inside your ROTDIR. You may need to rename the enkf folder (enkf.gdas.$PDY -> enkfgdas.$PDY).

Due to a recent change in the dycore, you may also need an additional offline step to fix the checksum of the NetCDF files for warm start. See the :ref:`Fix netcdf checksum section <gfsv17-checksum>`.
The current model has undergone several updates and the files generated may not be completely usable by the model.

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

If a user wishes to run a high-res (C768C384L127) GFSv17 experiment with warm starts from the operational GFSv16 (or older) warm starts, they must process the initial condition files before using. See details below in the :ref:`Fix netcdf checksum section <gfsv17-checksum>`.

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
