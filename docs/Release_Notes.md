GFS V16.4.0 RELEASE NOTES

-------
PRELUDE
-------

TODO: ADD DA UPDATE DETAILS

Also included in this upgrade are changes to add an indicator log file for WW3 gridded output.

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub are used to manage the GFS code.  The SPA(s) handling the GFS implementation need to have permissions to clone VLab Gerrit repositories.  All NOAA-EMC organization repositories on GitHub are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
mkdir gfs.v16.4.0
cd gfs.v16.4.0
git clone -b EMC-v16.4.0 https://github.com/NOAA-EMC/global-workflow.git .
cd sorc
./checkout.sh -o
```

The checkout script extracts the following GFS components:

| Component | Tag         | POC               |
| --------- | ----------- | ----------------- |
| MODEL     | GFS.v16.4.0   | Jun.Wang@noaa.gov |
| GLDAS     | gldas_gfsv16_release.v.2.1.0 | Helin.Wei@noaa.gov |
| GSI       | gfsda.v16.3.22 | Andrew.Collard@noaa.gov |
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

* `versions/run.ver` - change `version=v16.4.0` and `gfs_ver=v16.4.0`

SORC CHANGES
------------

* New MODEL tag: `GFS.v16.4.0`

JOBS CHANGES
------------

* No changes from GFS v16.3.22

PARM/CONFIG CHANGES
-------------------

* No changes from GFS v16.3.22

SCRIPT CHANGES
--------------

* WW3-related changes to `scripts/exgfs_wave_post_gridded_sbs.sh` and `scripts/exglobal_forecast.sh`. Grid loop and counter updates. Log files are now checked for completion instead of the associated gridded output.

FIX CHANGES
-----------

* No changes from GFS v16.3.22

MODULE CHANGES
--------------

* No changes from GFS v16.3.22

CHANGES TO FILE AND FILE SIZES
------------------------------

* No changes from GFS v16.3.22

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

* No changes from GFS v16.3.22

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * All DA and wave jobs
* Does this change require a 30-day evaluation?
  * No

DISSEMINATION INFORMATION
-------------------------

* No changes from GFS v16.3.22

HPSS ARCHIVE
------------

* No changes from GFS v16.3.22

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes from GFS v16.3.22

DOCUMENTATION
-------------

* No changes from GFS v16.3.22

PREPARED BY
-----------
Kate.Friedman@noaa.gov
Matthew.Masarik@noaa.gov
Jessica.Meixner@noaa.gov
