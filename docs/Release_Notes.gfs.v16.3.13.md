GFS V16.3.13 RELEASE NOTES

-------
PRELUDE
-------

This upgrade to the GFS includes updates to WAFS

WAFS:

  At GFS v16.3 implementation, UPP and WAFS package were updated to be ready to produce high resolution WAFS output to meet 2023 ICAO milestone with one switch.
  The switch was turned off at GFS v16.3. Now that UKMO has started to produce their corresponding high resolution WAFS output, EMC will work with NCO to turn on the switch.

  Additionally, EMC is updating WAFS ecf definition file to:
  1. trigger WAFS blending job 5 min later from 4:25 to 4:30 to compensate for 5-10 min delay in receiving UKMO high resolution data;
  2. stop producing blended 1.25 deg WAFS file per ICAO milestone.
  3. stop producing wafsgfs[37-44]* octant files because NCO decides to remove the related DBNET alerts

  Both sides agreed that we sill stop waiting for data from the other side at T+4:45.

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub .com are used to manage the GFS code.  The SPA(s) handling the GFS implementation need to have permissions to clone VLab Gerrit repositories and private NCAR UPP_GTG repository. All NOAA-EMC organization repositories are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
mkdir gfs.v16.3.13
cd gfs.v16.3.13
git clone -b EMC-v16.3.13 https://github.com/NOAA-EMC/global-workflow.git .
cd sorc
./checkout.sh -o
```

The checkout script extracts the following GFS components:

| Component | Tag         | POC               |
| --------- | ----------- | ----------------- |
| MODEL     | GFS.v16.3.1   | Jun.Wang@noaa.gov |
| GLDAS     | gldas_gfsv16_release.v.2.1.0 | Helin.Wei@noaa.gov |
| GSI       | gfsda.v16.3.12 | Andrew.Collard@noaa.gov |
| UFS_UTILS | ops-gfsv16.3.0 | George.Gayno@noaa.gov |
| POST      | upp_v8.3.0 | Wen.Meng@noaa.gov |
| WAFS      | gfs_wafs.v6.3.2 | Yali.Mao@noaa.gov |

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

* `versions/run.ver` - change `version=v16.3.13` and `gfs_ver=v16.3.13`

SORC CHANGES
------------

* No changes from GFS v16.3.12

JOBS CHANGES
------------

* No changes from GFS v16.3.12

PARM/CONFIG CHANGES
-------------------

* No changes from GFS v16.3.12

SCRIPT CHANGES
--------------

* Update ecf/scripts/gfs/atmos/post_processing/grib2_wafs/jgfs_atmos_wafs_blending_0p25.ecf to ICAO2023=yes
* Update ecf/scripts/gfs/atmos/post_processing/grib2_wafs/jgfs_atmos_wafs_grib2.ecf to ICAO2023=yes
* Update ecf/scripts/gfs/atmos/post_processing/grib2_wafs/jgfs_atmos_wafs_grib2_0p25.ecf to ICAO2023=yes
* Update ecf/scripts/gfs/atmos/post_processing/jgfs_atmos_wafs_gcip.ecf to ICAO2023=yes
* In ecf/defs/gfs_v16_3.def, remove jgfs_atmos_wafs_blending task, delay trigger time of jgfs_atmos_wafs_blending_0p25 by 5 minutes
* Update scripts/exgfs_atmos_wafs_grib2_0p25.sh to add forecast hour window to dbn_alert for awf and WAFS unblended hazard data
* In scripts/exgfs_atmos_wafs_grib2.sh, add dbn_alert of non-headed non-hazard 1.25 deg data (gfs.tCCz.wafs_grb45fFF.grib2) with subtype GFS_WAFS_1P25_GB2
* In scripts/exgfs_atmos_wafs_grib.sh, stop running ush/wafs_intdsk.sh which produces wafsgfs[37-44]* octant files because NCO decides to remove the related DBNET alerts.
* In ush/mkwfsgbl.sh, remove GFS_WAFS and GFS_XWAFS DBNET alerts.

FIX CHANGES
-----------

* No changes from GFS v16.3.12

MODULE CHANGES
--------------

* No changes from GFS v16.3.12

CHANGES TO FILE SIZES
---------------------

* WAFS - There will be file size increases.  Please see page 2 of the following Google slide:
https://docs.google.com/presentation/d/1VtPhyYXTe_PS9gXZGMrMPlQ32ELl-JJem-Vq8vO4j_U/edit#slide=id.g134dd9cf8ea_0_107

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

* jgfs_atmos_wafs_grib2_0p25.ecf changes from "mpiprocs=11:ompthreads=1:ncpus=11:mem=80GB" to "mpiprocs=39:ompthreads=1:ncpus=39:mem=200GB" because there will be 39 forecast hours and each forecast hour uses one processor through MPMD.

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * WAFS
* Does this change require a 30-day evaluation?
  * No

DISSEMINATION INFORMATION
-------------------------

* SCN23-111: Change to Global Aviation Products related to the World Area Forecast System (WAFS) Product on or about January 17, 2024. A pdf version is posted at:
https://www.weather.gov/media/notification/pdf_2023_24/scn23-111_wafs_products_change.pdf

* File changes
  1. Stop WAFS_blended_*.grib2, and wmo/grib2.*wafs_grb_wifs*.45
  2. Stop wafsgfs[37-44]* octant files, and wmo/com.wafs*
  3. Stop gfs.t??z.wafs_grb45f??.nouswafs.grib2
  4. gfs.tCCz.wafs_0p25.fFFF.grib2:  temporal resolution increases and forecast hour is extended to 120
  5. For the following files, vertical levels and file sizes are changed, and each level is the exact number of pressure level:
     - gfs.tCCz.awf_0p25.fFFF.grib2 (additionally temporal resolution increases and forecast hour is extended to 48)
     - gfs.tCCz.wafs_0p25_unblended.fFF.grib2 (additionally temporal resolution increases and forecast hour is extended to 48)
     - WAFS_0p25_blended_*.grib2 (additionally temporal resolution increases and forecast hour is extended to 48)
     - gfs.tCCz.wafs_grb45fFF.grib2, and wmo/grib2.tCCz.wafs_grbfFF.45 (additionally remove hazard data including CAT, ICIP and CB fields)
     - gfs.tCCz.gcip.f00.grib2   

HPSS ARCHIVE
------------

* Increase from 972M to 3G because both the vertical and temporal resolutions increase of both 0.25 unblended and blended hazard WAFS data.

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes from GFS v16.3.12

DOCUMENTATION
-------------

* No changes from GFS v16.3.12

PREPARED BY
-----------
Kate.Friedman@noaa.gov
Yali.Mao@noaa.gov
Hui-Ya.Chuang@noaa.gov
