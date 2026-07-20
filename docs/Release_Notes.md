GFS V16.3.32 RELEASE NOTES

-------
PRELUDE
-------
Turn off Metop-C AMSU-A channel 4 which has become unusable.

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub are used to manage the GFS code.  The SPA(s) handling the GFS implementation need to have permissions to clone VLab Gerrit repositories.  All NOAA-EMC organization repositories on GitHub are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT

mkdir gfs.v16.3.33
cd gfs.v16.3.33
git clone -b OMD-gfs.v16.3.33 https://github.com/NOAA-EMC/global-workflow.git .

cd sorc
./checkout.sh -o
```

The checkout script extracts the following GFS components:

| Component  | Tag                          | POC                     |
| ---------- | ---------------------------- | ----------------------- |
| MODEL      | GFS.v16.3.26                 | Jun.Wang@noaa.gov       |
| GLDAS      | gldas_gfsv16_release.v.2.1.0 | Helin.Wei@noaa.gov      |
| GSI        | gfsda.v16.3.33               | Andrew.Collard@noaa.gov |
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

* in `versions/run.ver` change `version=v16.3.33` and `gfs_ver=v16.3.33`

SORC CHANGES
------------

* Update GSI checkout tag to `gfsda.v16.3.33` (was `gfsda.v16.3.32`).

JOBS CHANGES
------------

* No changes from GFS v16.3.32

PARM/CONFIG CHANGES
-------------------

* No changes from GFS v16.3.32

SCRIPT CHANGES
--------------

* No changes from GFS v16.3.32

FIX CHANGES
-----------

* Turn off Metop-C AMSU-A channel 4 in global_satinfo.txt which has become unusable.

MODULE CHANGES
--------------

* No changes from GFS v16.3.32.

CHANGES TO FILE AND FILE SIZES
------------------------------

* No changes from GFS v16.3.32.

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

* No changes from GFS v16.3.32.

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * N/A
* Does this change require a 30-day evaluation?
  * No

DISSEMINATION INFORMATION
-------------------------

* No changes from GFS v16.3.32

HPSS ARCHIVE
------------

* No changes from GFS v16.3.32

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes from GFS v16.3.32

DOCUMENTATION
-------------

* No changes from GFS v16.3.32

PREPARED BY
-----------
Andrew.Collard@noaa.gov
Travis.Elless@noaa.gov
David.Huber@noaa.gov
