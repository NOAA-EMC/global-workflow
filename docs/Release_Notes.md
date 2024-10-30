GFS V16.3.21 RELEASE NOTES

-------
PRELUDE
-------

The WAFS is separated from the GFS and is now its own package in production as WAFS.v7.0.0.

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub are used to manage the GFS code.  The SPA(s) handling the GFS implementation need to have permissions to clone VLab Gerrit repositories.  All NOAA-EMC organization repositories on GitHub are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
mkdir gfs.v16.3.21
cd gfs.v16.3.21
git clone -b EMC-v16.3.21 https://github.com/NOAA-EMC/global-workflow.git .
cd sorc
./checkout.sh -o
```

The checkout script extracts the following GFS components:

| Component | Tag         | POC               |
| --------- | ----------- | ----------------- |
| MODEL     | GFS.v16.3.1   | Jun.Wang@noaa.gov |
| GLDAS     | gldas_gfsv16_release.v.2.1.0 | Helin.Wei@noaa.gov |
| GSI       | gfsda.v16.3.20 | Andrew.Collard@noaa.gov |
| UFS_UTILS | ops-gfsv16.3.20 | George.Gayno@noaa.gov |
| POST      | upp_v8.3.0 | Wen.Meng@noaa.gov |

To build all the GFS components, execute:
```bash
./build_all.sh
```
The `build_all.sh` script compiles all GFS components. Runtime output from the build for each package is written to log files in directory logs. To build an individual program, for instance, gsi, use `build_gsi.sh`.

Next, link the executables, fix files, parm files, etc in their final respective locations by executing:
```bash
./link_fv3gfs.sh nco wcoss2
```

Lastly, link the ecf scripts by moving back up to the ecf folder and executing:
```bash
cd ../ecf
./setup_ecf_links.sh
```
VERSION FILE CHANGES
--------------------

* `versions/run.ver` - change `version=v16.3.21` and `gfs_ver=v16.3.21`

SORC CHANGES
------------

The WAFS is no longer a submodule that is checked out within the GFS package.
The `sorc/checkout.sh` and `Externals.cfg` checkout script no longer clone WAFS.
The `sorc/build_all.sh` script no longer builds the WAFS code.
The `sorc/build_gfs_wafs.sh` build script is deleted.
The `sorc/link_fv3gfs.sh` script no longer links/copies WAFS files/execs.

JOBS CHANGES
------------

All WAFS jobs are removed from the GFS ecFlow definition file, rocoto mesh, and `ush/ecflow/prod.yml`.
Jobs removed:
* `jgfs_atmos_wafs_gcip`
* `jgfs_atmos_wafs_fFFF`
* `jgfs_atmos_wafs_grib2`
* `jgfs_atmos_wafs_grib2_0p25`
* `jgfs_atmos_wafs_blending_0p25`

PARM/CONFIG CHANGES
-------------------

The following config files are deleted:
* `parm/config/config.wafs`
* `parm/config/config.wafsblending`
* `parm/config/config.wafsblending0p25`
* `parm/config/config.wafsgcip`
* `parm/config/config.wafsgrib2`
* `parm/config/config.wafsgrib20p25`

* The `WAFSF` flag is removed from `parm/config/config.base.emc.dyn` and `parm/config/config.base.nco.static`.
* All WAFS jobs are removed from platform env files, `parm/config/config.resources.emc.dyn`, and `parm/config/config.resources.nco.static`.
* The WAFS jobs are removed from experiment setup.

WAFS output is removed from the following transfer list files:
* `parm/product/transfer_gfs_1.list`
* `parm/product/transfer_gfs_7.list`

SCRIPT CHANGES
--------------

The following WAFS rocoto scripts are removed:
* `jobs/rocoto/wafs.sh`
* `jobs/rocoto/wafsblending.sh`
* `jobs/rocoto/wafsblending0p25.sh`
* `jobs/rocoto/wafsgcip.sh`
* `jobs/rocoto/wafsgrib2.sh`
* `jobs/rocoto/wafsgrib20p25.sh`

The following ecf scripts are removed from the GFS:
* `ecf/scripts/gfs/atmos/post_processing/grib2_wafs/jgfs_atmos_wafs_blending.ecf`
* `ecf/scripts/gfs/atmos/post_processing/grib2_wafs/jgfs_atmos_wafs_blending_0p25.ecf`
* `ecf/scripts/gfs/atmos/post_processing/grib2_wafs/jgfs_atmos_wafs_grib2.ecf`
* `ecf/scripts/gfs/atmos/post_processing/grib2_wafs/jgfs_atmos_wafs_grib2_0p25.ecf`
* `ecf/scripts/gfs/atmos/post_processing/grib_wafs/jgfs_atmos_wafs_master.ecf`
* `ecf/scripts/gfs/atmos/post_processing/jgfs_atmos_wafs_gcip.ecf`

The WAFS is removed from `ecf/setup_ecf_links.sh`.

The WAFS output is removed from archival (`ush/hpssarch_gen.sh`).

FIX CHANGES
-----------

* `fix/product/wafs_admin_msg` - removed

MODULE CHANGES
--------------

Modules needed by WAFS are removed from `modulefiles/module_base.wcoss_dell_p3`.

CHANGES TO FILE AND FILE SIZES
------------------------------

The following files will no longer be produced within the GFS COM:
* `gfs.tCCz.awf_0p25.fFFF.grib2` - renamed to `wafs.tCCz.awf.0p25.fFFF.grib2` in WAFSv7
* `gfs.tCCz.awf_grb45fFF.grib2` - renamed to `wafs.tCCz.awf_grid45.fFFF.grib2` in WAFSv7
* `wmo/grib2.tCCz.awf_grbfFF.45` - renamed to `wmo/grib2.wafs.tCCz.awf_grid45.fFFF` in WAFSv7
* `gfs.tCCz.control.wafsblending_0p25`
* `gfs.tCCz.wafs.0p25.anl` - renamed to `wafs.tCCz.0p25.anl.grib2` in WAFSv7
* `gfs.tCCz.wafs.0p25.anl.idx`
* `gfs.tCCz.wafs_0p25.fFFF.grib2`
* `gfs.tCCz.wafs_0p25.fFFF.grib2.idx`
* `gfs.tCCz.wafs_0p25_unblended.fFFF.grib2` - renamed to `WAFS_0p25_unblended_YYYYMMDDHHfFFF.grib2` in WAFSv7
* `gfs.tCCz.wafs_0p25_unblended.fFFF.grib2.idx`
* `gfs.tCCz.wafs.grb2fFFF` - renamed to `wafs.tCCz.master.fFFF.grib2` in WAFSv7
* `gfs.tCCz.wafs.grb2fFFF.idx`
* `gfs.tCCz.wafs_grb45fFF.grib2` - renamed to `gfs.tCCz.wafs_grb45fFFF.grib2` in WAFSv7
* `gfs.tCCz.wafs_grb45fFF.grib2.idx`
* `wmo/grib2.tCCz.wafs_grbfFF.45` - renamed to `wmo/grib2.wafs.tCCz.awf_grid45.fFFF` in WAFSv7
* `wmo/xtrn.wfsgfs0006.gfs_atmos_wafs_f30_00`
* `gfs.tCCz.gcip.fFF.grib2` - renamed to `wafs.tCCz.gcip.fFFF.grib2` in WAFSv7
* `WAFS_0p25_blended_YYYYMMDDHHf[06-48].grib2` - renamed to `WAFS_0p25_blended_ YYYYMMDDHHfFFF.grib2` in WAFSv7

The following files will no longer be produced within the GFS COM
and are being retired from the WAFS package:
* `gfs.tCCz.wafs_icao.grb2fFFF`
* `gfs.tCCz.wafs_icao.grb2fFFF.idx`
* `wafs.tCCz.master.fFFF.grib2` where FFF is from 001 to 005

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

* Updates to improve the enkfgdas_update job runtime stability.
  * Run with `--cpu-bind core` instead of `--cpu-bind depth`
  * Change tasks to 280 and threads to 16

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * None
* Does this change require a 30-day evaluation?
  * No

DISSEMINATION INFORMATION
-------------------------

* No changes from GFS v16.3.20

HPSS ARCHIVE
------------

* No changes from GFS v16.3.20

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes from GFS v16.3.20

DOCUMENTATION
-------------

* No changes from GFS v16.3.20

PREPARED BY
-----------
Kate.Friedman@noaa.gov
Yali.Mao@noaa.gov
Rahul.Mahajan@noaa.gov
