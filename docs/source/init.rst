==========================
Prepare Initial Conditions
==========================

There are two types of initial conditions for the global-workflow:

#. Warm start: these ICs are taken directly from either the GFS in production or an experiment "warmed" up (at least one cycle in).
#. Cold start: any ICs converted to a new resolution or grid (e.g. GSM-GFS -> FV3GFS). These ICs are often prepared by chgres_cube (change resolution utility).

Most users will initiate their experiments with cold start ICs unless running high resolution (C768 deterministic with C384 EnKF) for a date with warm starts available. It is `not recommended` to run high resolution unless required or as part of final testing.

Resolutions:

* C48 = 2­ degree ≈ 200km
* C96 = 1­ degree ≈ 100km
* C192 = 1/2­ degree ≈ 50km
* C384 = 1/4 degree ≈ 25km
* C768 = 1/8th degree ≈ 13km
* C1152 ≈ 9km
* C3072 ≈ 3km

Supported resolutions in global-workflow: C48, C96, C192, C384, C768

^^^^^^^^^^^^^^^^^^^^
Automated Generation
^^^^^^^^^^^^^^^^^^^^

***********
Cycled mode
***********

Not yet supported. See Manual Generation section below for how to create your ICs yourself (outside of workflow).

*****************************
Free-forecast mode (atm-only)
*****************************

Free-forecast mode in global workflow includes ``getic`` and ``init`` jobs for the gfs suite. The ``getic`` job pulls inputs for ``chgres_cube`` (init job) or warm start ICs into your ``ROTDIR/COMROT``. The ``init`` job then ingests those files to produce initial conditions for your experiment. 

Users on machines without HPSS access (e.g. Orion) need to perform the ``getic`` step manually and stage inputs for the ``init`` job. The table below lists the needed files for ``init`` and where to place them in your ``ROTDIR``.

Note for table: yyyy=year; mm=month; dd=day; hh=cycle

Operations/production output location on HPSS: /NCEPPROD/hpssprod/runhistory/rh ``yyyy``/``yyyymm``/``yyyymmdd``/

+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| Source         | Files                           | Tarball name                                                                | Where in ROTDIR                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v12 ops        | * gfs.t. ``hh`` z.sanl          | com_gfs_prod_gfs. ``yyyymmddhh`` .anl.tar                                   | gfs. ``yyyymmdd`` /``hh``      |
|                | * gfs.t. ``hh`` z.sfcanl        |                                                                             |                                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v13 ops        | * gfs.t. ``hh`` z.sanl          | com2_gfs_prod_gfs. ``yyyymmddhh`` .anl.tar                                  | gfs. ``yyyymmdd`` /``hh``      |
|                | * gfs.t. ``hh`` z.sfcanl        |                                                                             |                                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v14 ops        | * gfs.t. ``hh`` z.atmanl.nemsio | gpfs_hps_nco_ops_com_gfs_prod_gfs. ``yyyymmddhh`` .anl.tar                  | gfs. ``yyyymmdd` /`hh``        |
|                | * gfs.t. ``hh`` z.sfcanl.nemsio |                                                                             |                                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v15 ops        | * gfs.t. ``hh`` z.atmanl.nemsio | gpfs_dell1_nco_ops_com_gfs_prod_gfs. ``yyyymmdd`` _ ``hh`` .gfs_nemsioa.tar | gfs. ``yyyymmdd`` /``hh``      |
| pre-2020022600 | * gfs.t. ``hh`` z.sfcanl.nemsio |                                                                             |                                | 
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v15 ops        | * gfs.t. ``hh`` z.atmanl.nemsio | com_gfs_prod_gfs. ``yyyymmdd`` _ ``hh`` .gfs_nemsioa.tar                    | gfs. ``yyyymmdd`` /``hh``      |
|                | * gfs.t. ``hh`` z.sfcanl.nemsio |                                                                             |                                |  
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v16 ops        | * gfs.t. ``hh`` z.atmanl.nc     | com_gfs_prod_gfs. ``yyyymmdd`` _ ``hh`` .gfs_nca.tar                        | gfs. ``yyyymmdd`` /``hh``/atmos|
|                | * gfs.t. ``hh`` z.sfcanl.nc     |                                                                             |                                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+
| v16 retro      | * gfs.t. ``hh`` z.atmanl.nc     | gfs_netcdfa.tar*                                                            | gfs. ``yyyymmdd`` /``hh``/atmos|
|                | * gfs.t. ``hh`` z.sfcanl.nc     |                                                                             |                                |
+----------------+---------------------------------+-----------------------------------------------------------------------------+--------------------------------+

For HPSS path, see retrospective table in :ref:`pre-production parallel section <retrospective>`: below

*********************
Free-forecast coupled
*********************

Coupled initial conditions are currently only generated offline and copied prior to the forecast run. Prototype initial conditions will automatically be used when setting up an experiment as an S2SW app, there is no need to do anything additional. Copies of initial conditions from the prototype runs are currently maintained on Hera, Orion, and WCOSS2. The locations used are determined by ``parm/config/config.coupled_ic``. If you need prototype ICs on another machine, please contact Walter (Walter.Kolczynski@noaa.gov).

^^^^^^^^^^^^^^^^^
Manual Generation
^^^^^^^^^^^^^^^^^

NOTE: Initial conditions cannot be generated on S4. These must be generated on another supported platform then pushed to S4. If you do not have access to a supported system or need assistance, please contact David Huber (david.huber@noaa.gov).

***********
Cold starts
***********

The following information is for users needing to generate initial conditions for a cycled experiment that will run at a different resolution or layer amount than the operational GFS (C768C384L127).

The ``chgres_cube`` code is available from the `UFS_UTILS repository <https://github.com/ufs-community/UFS_UTILS>`_ on GitHub and can be used to convert GFS ICs to a different resolution or number of layers. Users may clone the develop/HEAD branch or the same version used by global-workflow develop (found in sorc/checkout.sh). The ``chgres_cube`` code/scripts currently support the following GFS inputs:

* pre-GFSv14 
* GFSv14 
* GFSv15 
* GFSv16 

Clone UFS_UTILS::

   git clone --recursive https://github.com/NOAA-EMC/UFS_UTILS.git

Then switch to a different tag or use the default branch (develop).

Build UFS_UTILS::

   sh build_all.sh
   cd fix
   sh link_fixdirs.sh emc $MACHINE

where ``$MACHINE`` is ``wcoss2``, ``hera``, ``jet``, or ``orion``. Note: UFS-UTILS builds on Orion but due to the lack of HPSS access on Orion the ``gdas_init`` utility is not supported there.

Configure your conversion::

   cd util/gdas_init
   vi config

Read the doc block at the top of the config and adjust the variables to meet you needs (e.g. ``yy, mm, dd, hh`` for ``SDATE``).

Submit conversion script::`

   ./driver.$MACHINE.sh

where ``$MACHINE`` is currently ``wcoss2``,  ``hera`` or ``jet``. Additional options will be available as support for other machines expands. Note: UFS-UTILS builds on Orion but due to lack of HPSS access there is no ``gdas_init`` driver for Orion nor support to pull initial conditions from HPSS for the ``gdas_init`` utility.

3 small jobs will be submitted:

  - 1 jobs to pull inputs off HPSS
  - 2 jobs to run ``chgres_cube`` (1 for deterministic/hires and 1 for each EnKF ensemble member)

The chgres jobs will have a dependency on the data-pull jobs and will wait to run until all data-pull jobs have completed.

Check output:

In the config you will have defined an output folder called ``$OUTDIR``. The converted output will be found there, including the needed abias and radstat initial condition files. The files will be in the needed directory structure for the global-workflow system, therefore a user can move the contents of their ``$OUTDIR`` directly into their ``$ROTDIR/$COMROT``.

Please report bugs to George Gayno (george.gayno@noaa.gov) and Kate Friedman (kate.friedman@noaa.gov).

*****************************
Warm starts (from production)
*****************************

The GFSv15 was implemented into production on June 12th, 2019 at 12z. The GFS was spun up ahead of that cycle and thus production output for the system is available from the 00z cycle (2019061200) and later. Production output tarballs from the prior GFSv14 system are located in the same location on HPSS but have "hps" in the name to represent that it was run on the Cray, where as the GFS now runs in production on the Dell and has "dell1" in the tarball name.

See production output in the following location on HPSS:

``/NCEPPROD/hpssprod/runhistory/rhYYYY/YYYYMM/YYYYMMDD``

Example location:

``/NCEPPROD/hpssprod/runhistory/rh2021/202104/20210420``

Example listing for 2021042000 production tarballs::

   [Kate.Friedman@m72a2 ~]$ hpsstar dir /NCEPPROD/hpssprod/runhistory/rh2021/202104/20210420 | grep gfs | grep _00. | grep -v idx
   [connecting to hpsscore1.fairmont.rdhpcs.noaa.gov/1217]
   ******************************************************************
   *   Welcome to the NESCC High Performance Storage System         *
   *                                                                *
   *   Current HPSS version: 7.5.3                                  *
   *                                                                *
   *                                                                *
   *       Please Submit Helpdesk Request to                        *
   *        rdhpcs.hpss.help@noaa.gov                               *
   *                                                                *
   *  Announcements:                                                *
   ******************************************************************
   Username: Kate.Friedman  UID: 2391  Acct: 2391(2391) Copies: 1 COS: 0 Firewall: off [hsi.6.3.0.p1-hgs Thu May 7 09:16:23 UTC 2020]
   /NCEPPROD/hpssprod/runhistory/rh2021/202104:
   drwxr-xr-x    2 nwprod    prod           11776 Apr 19 23:44 20210420
   [connecting to hpsscore1.fairmont.rdhpcs.noaa.gov/1217]
   -rw-r-----    1 nwprod    rstprod  51268255744 Apr 22 05:29 com_gfs_prod_enkfgdas.20210420_00.enkfgdas.tar
   -rw-r--r--    1 nwprod    prod     220121310720 Apr 22 06:42 com_gfs_prod_enkfgdas.20210420_00.enkfgdas_restart_grp1.tar
   -rw-r--r--    1 nwprod    prod     220124178944 Apr 22 07:04 com_gfs_prod_enkfgdas.20210420_00.enkfgdas_restart_grp2.tar
   -rw-r--r--    1 nwprod    prod     220120305664 Apr 22 07:24 com_gfs_prod_enkfgdas.20210420_00.enkfgdas_restart_grp3.tar
   -rw-r--r--    1 nwprod    prod     220116934656 Apr 22 07:38 com_gfs_prod_enkfgdas.20210420_00.enkfgdas_restart_grp4.tar
   -rw-r--r--    1 nwprod    prod     220121547776 Apr 22 07:56 com_gfs_prod_enkfgdas.20210420_00.enkfgdas_restart_grp5.tar
   -rw-r--r--    1 nwprod    prod     220125794816 Apr 22 08:09 com_gfs_prod_enkfgdas.20210420_00.enkfgdas_restart_grp6.tar
   -rw-r--r--    1 nwprod    prod     220117037568 Apr 22 08:23 com_gfs_prod_enkfgdas.20210420_00.enkfgdas_restart_grp7.tar
   -rw-r--r--    1 nwprod    prod     220117203968 Apr 22 08:33 com_gfs_prod_enkfgdas.20210420_00.enkfgdas_restart_grp8.tar
   -rw-r-----    1 nwprod    rstprod   9573153280 Apr 22 02:49 com_gfs_prod_gdas.20210420_00.gdas.tar
   -rw-r--r--    1 nwprod    prod      1020249088 Apr 22 02:49 com_gfs_prod_gdas.20210420_00.gdas_flux.tar
   -rw-r--r--    1 nwprod    prod     92950728704 Apr 22 03:05 com_gfs_prod_gdas.20210420_00.gdas_nc.tar
   -rw-r--r--    1 nwprod    prod     10647806464 Apr 22 02:50 com_gfs_prod_gdas.20210420_00.gdas_pgrb2.tar
   -rw-r-----    1 nwprod    rstprod  65121796608 Apr 22 02:56 com_gfs_prod_gdas.20210420_00.gdas_restart.tar
   -rw-r--r--    1 nwprod    prod     18200814080 Apr 22 03:06 com_gfs_prod_gdas.20210420_00.gdaswave_keep.tar
   -rw-r-----    1 nwprod    rstprod  13013076992 Apr 22 03:08 com_gfs_prod_gfs.20210420_00.gfs.tar
   -rw-r--r--    1 nwprod    prod     62663230976 Apr 22 03:13 com_gfs_prod_gfs.20210420_00.gfs_flux.tar
   -rw-r--r--    1 nwprod    prod     127932879360 Apr 22 03:47 com_gfs_prod_gfs.20210420_00.gfs_nca.tar
   -rw-r--r--    1 nwprod    prod     138633526272 Apr 22 04:00 com_gfs_prod_gfs.20210420_00.gfs_ncb.tar
   -rw-r--r--    1 nwprod    prod     140773240832 Apr 22 03:27 com_gfs_prod_gfs.20210420_00.gfs_pgrb2.tar
   -rw-r--r--    1 nwprod    prod     61253672960 Apr 22 03:32 com_gfs_prod_gfs.20210420_00.gfs_pgrb2b.tar
   -rw-r--r--    1 nwprod    prod     19702107136 Apr 22 03:34 com_gfs_prod_gfs.20210420_00.gfs_restart.tar
   -rw-r--r--    1 nwprod    prod     18617610240 Apr 22 04:02 com_gfs_prod_gfs.20210420_00.gfswave_output.tar
   -rw-r--r--    1 nwprod    prod     30737774592 Apr 22 04:05 com_gfs_prod_gfs.20210420_00.gfswave_raw.tar

The warm starts and other output from production are at C768 deterministic and C384 EnKF. The warm start files must be converted to your desired resolution(s) using ``chgres_cube`` if you wish to run a different resolution. If you are running a C768/C384 experiment you can use them as is.

.. _fix-netcdf:

-------------------------
Fix NetCDF checksum issue
-------------------------

Due to a recent change in UFS, the setting to bypass the data verification no longer works, so you may also need an additional offline step to delete the checksum of the NetCDF files for warm start:

On RDHPCS::

   module load nco/4.9.3

On WCOSS2::

   module load intel/19.1.3.304
   module load netcdf/4.7.4
   module load udunits/2.2.28
   module load gsl/2.7
   module load nco/4.7.9

And then on all platforms::

   cd $COMROT
   for f in $(find ./ -name *tile*.nc); do echo $f; ncatted -a checksum,,d,, $f; done

------------------------------------------------------------------------------------------
What files should you pull for starting a new experiment with warm starts from production?
------------------------------------------------------------------------------------------

That depends on what mode you want to run -- free-forecast or cycled. Whichever mode navigate to the top of your ``COMROT`` and pull the entirety of the tarball(s) listed below for your mode. The files within the tarball are already in the ``$CDUMP.$PDY/$CYC`` folder format expected by the system.

For free-forecast there are two tar balls to pull

   1. File #1 (for starting cycle SDATE)::
      /NCEPPROD/hpssprod/runhistory/rhYYYY/YYYYMM/YYYYMMDD/com_gfs_prod_gfs.YYYYMMDD_CC.gfs_restart.tar
   2. File #2 (for prior cycle GDATE=SDATE-06)::
      /NCEPPROD/hpssprod/runhistory/rhYYYY/YYYYMM/YYYYMMDD/com_gfs_prod_gdas.YYYYMMDD_CC.gdas_restart.tar

 For cycled mode there 18 tarballs to pull (9 for SDATE and 9 for GDATE (SDATE-06))::

    HPSS path: /NCEPPROD/hpssprod/runhistory/rhYYYY/YYYYMM/YYYYMMDD/

Tarballs per cycle::

   com_gfs_prod_gdas.YYYYMMDD_CC.gdas_restart.tar
   com_gfs_prod_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp1.tar
   com_gfs_prod_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp2.tar
   com_gfs_prod_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp3.tar
   com_gfs_prod_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp4.tar
   com_gfs_prod_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp5.tar
   com_gfs_prod_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp6.tar
   com_gfs_prod_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp7.tar
   com_gfs_prod_enkfgdas.YYYYMMDD_CC.enkfgdas_restart_grp8.tar

Go to the top of your ``COMROT/ROTDIR`` and pull the contents of all tarballs there. The tarballs already contain the needed directory structure.

*******************************************
Warm starts (from pre-production parallels)
*******************************************

Recent pre-implementation parallel series was for GFS v16 (implemented March 2021). For the prior v15 (Q2FY19) see an additional table below.

* **What resolution are warm-starts available for?** Warm-start ICs are saved at the resolution the model was run at (C768/C384) and can only be used to run at the same resolution combination. If you need to run a different resolution you will need to make your own cold-start ICs. See cold start section above.
* **What dates have warm-start files saved?** Unfortunately the frequency changed enough during the runs that it’s not easy to provide a definitive list easily.
* **What files?** All warm-starts are saved in separate tarballs which include “restart” in the name. You need to pull the entirety of each tarball, all files included in the restart tarballs are needed.
* **Where are these tarballs?** See below for the location on HPSS for each v16 pre-implementation parallel.
* **What tarballs do I need to grab for my experiment?** Tarballs from two cycles are required. The tarballs are listed below, where $CDATE is your starting cycle and $GDATE is one cycle prior.

  - Free-forecast
    + ../$CDATE/gfs_restarta.tar
    + ../$GDATE/gdas_restartb.tar
  - Cycled w/EnKF
    + ../$CDATE/gdas_restarta.tar
    + ../$CDATE/enkfgdas_restarta_grp##.tar (where ## is 01 through 08) (note, older tarballs may include a period between enkf and gdas: "enkf.gdas")
    + ../$GDATE/gdas_restartb.tar
    + ../$GDATE/enkfgdas_restartb_grp##.tar (where ## is 01 through 08) (note, older tarballs may include a period between enkf and gdas: "enkf.gdas")

* **Where do I put the warm-start initial conditions?** Extraction should occur right inside your COMROT. You may need to rename the enkf folder (enkf.gdas.$PDY -> enkfgdas.$PDY).

Due to a recent change in the dycore, you may also need an additional offline step to fix the checksum of the NetCDF files for warm start. See the :ref:`fix netcdf checksum section <fix-netcdf>`:  above

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
