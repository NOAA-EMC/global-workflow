GFS V16.3.26 RELEASE NOTES

-------
PRELUDE
-------
Data assimilation upgrade which implements a version of GSI closer to the head of the develop branch and
includes the following new data types for assimilation: Windborne Balloon Sondes; Saildrone surface obs;
GOES-19 clear sky radiances; GMI radiances; NOAA-19 Ozone Profile Retreievals.   Preparation for Metop-SG
instruments.  Also include minor modifications to ATMS thinning; treatment of surface ship pressure
observations and quality control of humidity Jacobians.

Also included in this upgrade are changes to add an indicator log file for WW3 gridded output.

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub are used to manage the GFS code.  The SPA(s) handling the GFS implementation need to have permissions to clone VLab Gerrit repositories.  All NOAA-EMC organization repositories on GitHub are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
mkdir gfs.v16.3.26
cd gfs.v16.3.26
git clone -b EMC-v16.3.26 https://github.com/NOAA-EMC/global-workflow.git .
cd sorc
./checkout.sh -o
```

The checkout script extracts the following GFS components:

| Component  | Tag                          | POC                     |
| ---------- | ---------------------------- | ----------------------- |
| MODEL      | GFS.v16.3.26                 | Jun.Wang@noaa.gov       |
| GLDAS      | gldas_gfsv16_release.v.2.1.0 | Helin.Wei@noaa.gov      |
| GSI        | gfsda.v16.3.26               | Andrew.Collard@noaa.gov |
| UFS_UTILS  | ops-gfsv16.3.20              | George.Gayno@noaa.gov   |
| POST       | upp_v8.3.0                   | Wen.Meng@noaa.gov       |
| GSI-Utils  | gsiutil.v16.3.26             | Andrew.Collard@noaa.gov |
| GSI-Monitor| gsimon_v16.3.26              | Edward.Safford@noaa.gov |

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

* add `ncdiag_ver=1.1.1` in `versions/build.ver` and `versions/run.ver`
* in `versions/run.ver` change `version=v16.3.26` and `gfs_ver=v16.3.26`
* also changed in version files for developers: `obsproc_run_ver=1.2.6` and `prepobs_run_ver=1.1.2`

SORC CHANGES
------------

* New MODEL tag: `GFS.v16.3.26`
* New EMC_verif-global tag: `verif_global_v2.10.0.1` (Gulf of America changes)

The GSI has been updated from an older release branch (gfsda.v16.3.20) to one much closer to the develop
branch (gfsda.v16.3.26) with a large number of changes to the code that do not affect results.
Between these versions there has been a reorganisation of the repositories
resulting in some functionality moving from GSI to new repositories.
Therefore the following new repositories are checked out:

GSI:  gfsda.v16.3.26
GSI-Utils:  gsiutil.v16.3.26
GSI-Monitor: gsimon_v16.3.26

JOBS CHANGES
------------

The jobs/ directory of GSI have been moved to global-workflow:
* `jobs/JGDAS_ATMOS_ANALYSIS_DIAG`
* `jobs/JGDAS_ATMOS_CHGRES_FORENKF`
* `jobs/JGDAS_ATMOS_VERFOZN`
* `jobs/JGDAS_ATMOS_VERFRAD`
* `jobs/JGDAS_ATMOS_VMINMON`
* `jobs/JGDAS_ENKF_DIAG`
* `jobs/JGDAS_ENKF_ECEN`
* `jobs/JGDAS_ENKF_FCST`
* `jobs/JGDAS_ENKF_POST`
* `jobs/JGDAS_ENKF_SELECT_OBS`
* `jobs/JGDAS_ENKF_SFC`
* `jobs/JGDAS_ENKF_UPDATE`
* `jobs/JGFS_ATMOS_VMINMON`
* `jobs/JGLOBAL_ATMOS_ANALYSIS`
* `jobs/JGLOBAL_ATMOS_ANALYSIS_CALC`

PARM/CONFIG CHANGES
-------------------

* No changes from GFS v16.3.25

SCRIPT CHANGES
--------------

* WW3-related changes to `scripts/exgfs_wave_post_gridded_sbs.sh` and `scripts/exglobal_forecast.sh`. Grid loop and counter updates. Log files are now checked for completion instead of the associated gridded output.

* The scripts/ directory of GSI have been moved to global-workflow:
  * `scripts/exgdas_atmos_chgres_forenkf.sh`
  * `scripts/exgdas_atmos_verfozn.sh`
  * `scripts/exgdas_atmos_verfrad.sh`
  * `scripts/exgdas_atmos_vminmon.sh`
  * `scripts/exgdas_enkf_ecen.sh`
  * `scripts/exgdas_enkf_fcst.sh`
  * `scripts/exgdas_enkf_post.sh`
  * `scripts/exgdas_enkf_select_obs.sh`
  * `scripts/exgdas_enkf_sfc.sh`
  * `scripts/exgdas_enkf_update.sh`
  * `scripts/exgfs_atmos_vminmon.sh`
  * `scripts/exglobal_atmos_analysis.sh`
  * `scripts/exglobal_atmos_analysis_calc.sh`
  * `scripts/exglobal_diag.sh`

FIX CHANGES
-----------

The following GSI-fix files have been modified to include new data:
* `cloudy_radiance_info.txt` (add MWS and modify GMI)
* `global_convinfo.txt` (adding Saildrone, Windborne, GOES-19 AMVs and GRACE-FO GNSSRO)
* `global_ozinfo.txt` (NOAA-21 OMPS)
* `global_satinfo.txt` (Add GOES-19 CSRs, modify GMI, preparation for Metop-SG)
* `global_scaninfo.txt` (Add MWS and GOES-19)
* `prepobs_errtable.global` (Add Windborne (301/401) and Saildrone (302/402))
* `mws_beamwidth.txt` has been added to support future implementation of MWS when available

MODULE CHANGES
--------------

* Add ncdiag module to modulefiles for the GSI

CHANGES TO FILE AND FILE SIZES
------------------------------

* No significant changes from GFS v16.3.25

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

Improvements are made to several wave post-processing job resources:
* gfs_wave_post_bndpnt job walltime reduced from 1hr to 30mins and compute resources reduced from 3 nodes/80 tasks to 1 node/1 task
* gfs_wave_post_bndpntbll walltime reduced from 1hr to 10mins and compute resources reduced from 4 nodes/112 tasks to 1 node/2 tasks
* gfs_wave_postpnt walltime reduced from 1.5hrs to 35mins and compute resources reduced from 4 nodes/50 tasks to 1 node/3 tasks

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * All DA and wave jobs
* Does this change require a 30-day evaluation?
  * No
* Building GSI requires CRTMv2.4.0.2
* GOES-19 and GMI bias correction files need to be updated before implementation. We usually spin up
  the bias correction for a couple of weeks before turning on active assimilation. This can either be
  done by a two step implementation (starting with monitoring and then switching to active once the
  bias correction is spun up) or by taking the coefficients from a parallel.  A second wrinkle here
  is that the GMI bias correction is already spun up - but is wrong so we need to zero it first in
  the bias correction file. We suggest discussing how precisely to do this before any pre-operational
  parallel runs are started.

DISSEMINATION INFORMATION
-------------------------

* No changes from GFS v16.3.25

HPSS ARCHIVE
------------

* No changes from GFS v16.3.25

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes from GFS v16.3.25

DOCUMENTATION
-------------

* No changes from GFS v16.3.25

PREPARED BY
-----------
Kate.Friedman@noaa.gov
Andrew.Collard@noaa.gov
Matthew.Masarik@noaa.gov
Jessica.Meixner@noaa.gov
Rahul.Mahajan@noaa.gov