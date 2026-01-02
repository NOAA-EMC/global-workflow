GFS V16.3.29 RELEASE NOTES

-------
PRELUDE
-------
Remove UKMET reference from gfs gempak jobs.

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub are used to manage the GFS code.  The SPA(s) handling the GFS implementation need to have permissions to clone VLab Gerrit repositories.  All NOAA-EMC organization repositories on GitHub are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT

mkdir gfs.v16.3.29
cd gfs.v16.3.29
git clone -b EMC-v16.3.29 https://github.com/NOAA-EMC/global-workflow.git .
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

* in `versions/run.ver` change `version=v16.3.29` and `gfs_ver=v16.3.29`

SORC CHANGES
------------

* No changes from GFS v16.3.28

JOBS CHANGES
------------

* No changes from GFS v16.3.28

PARM/CONFIG CHANGES
-------------------

* No changes from GFS v16.3.28

SCRIPT CHANGES
--------------

* Remove COM declarations UKMET and ECMWF from the JGDAS_ATMOS_GEMPAK_META_NCDC and JGFS_ATMOS_GEMPAK_META J-Jobs.
* Remove the gdas_ecmwf_meta_var.sh and gdas_ukmet_meta_var.sh scripts from gempak/ush.
* Remove references to UKMET and ECMWF from the gempak/ush gfs_meta_comp.sh, gfs_meta_crb.sh, gfs_meta_hur.sh, gfs_meta_mar_comp.sh, gfs_meta_sa2.sh, and gfs_meta_usext.sh scripts.

FIX CHANGES
-----------

* No changes from GFS v16.3.28.

MODULE CHANGES
--------------

* No changes from GFS v16.3.28.

CHANGES TO FILE AND FILE SIZES
------------------------------

* No significant changes from GFS v16.3.28.

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

* No changes from GFS v16.3.28.

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * N/A
* Does this change require a 30-day evaluation?
  * No

DISSEMINATION INFORMATION
-------------------------

* No changes from GFS v16.3.28

HPSS ARCHIVE
------------

* No changes from GFS v16.3.28

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes from GFS v16.3.28

DOCUMENTATION
-------------

* No changes from GFS v16.3.28

PREPARED BY
-----------
David.Huber@noaa.gov
