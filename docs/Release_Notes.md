GFS V16.3.19 RELEASE NOTES

-------
PRELUDE
-------

PlanetiQ RO data has added noise and is placed into monitor mode until its impact on the cycled analysis forecast system can be assessed.

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub are used to manage the GFS code.  The SPA(s) handling the GFS implementation need to have permissions to clone VLab Gerrit repositories and the private NCAR UPP_GTG repository.  All NOAA-EMC organization repositories are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
mkdir gfs.v16.3.19
cd gfs.v16.3.19
git clone -b EMC-v16.3.19 https://github.com/NOAA-EMC/global-workflow.git .
cd sorc
./checkout.sh -o
```

The checkout script extracts the following GFS components:

| Component | Tag         | POC               |
| --------- | ----------- | ----------------- |
| MODEL     | GFS.v16.3.1   | Jun.Wang@noaa.gov |
| GLDAS     | gldas_gfsv16_release.v.2.1.0 | Helin.Wei@noaa.gov |
| GSI       | gfsda.v16.3.19 | Andrew.Collard@noaa.gov |
| UFS_UTILS | ops-gfsv16.3.0 | George.Gayno@noaa.gov |
| POST      | upp_v8.3.0 | Wen.Meng@noaa.gov |
| WAFS      | gfs_wafs.v6.3.3 | Yali.Mao@noaa.gov |

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

* `versions/run.ver` - change `version=v16.3.19` and `gfs_ver=v16.3.19`

SORC CHANGES
------------

* No changes from GFS v16.3.18

JOBS CHANGES
------------

* No changes from GFS v16.3.18

PARM/CONFIG CHANGES
-------------------

* No changes from GFS v16.3.18

SCRIPT CHANGES
--------------

* No changes from GFS v16.3.18

FIX CHANGES
-----------

* Updated `global_convinfo.txt` file in GSI package

MODULE CHANGES
--------------

* No changes from GFS v16.3.18

CHANGES TO FILE SIZES
---------------------

* No changes of existing file sizes from GFS v16.3.18

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

* No changes from GFS v16.3.18

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * Analysis
* Does this change require a 30-day evaluation?
  * No

DISSEMINATION INFORMATION
-------------------------

* No changes from GFS v16.3.18

HPSS ARCHIVE
------------

* No changes from GFS v16.3.18

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes from GFS v16.3.18

DOCUMENTATION
-------------

* No changes from GFS v16.3.18

PREPARED BY
-----------
Kate.Friedman@noaa.gov
Russ.Treadon@noaa.gov
