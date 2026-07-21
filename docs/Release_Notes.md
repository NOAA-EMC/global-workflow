GFS V16.3.34 RELEASE NOTES

-------
PRELUDE
-------
Downstream update due to RRFSv1.0 implementation/retirement of NAMv4.2. This update removes gempak products that are dependent on the NAM and are no longer needed. 

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub are used to manage the GFS code.  The SPA(s) handling the GFS implementation need to have permissions to clone VLab Gerrit repositories.  All NOAA-EMC organization repositories on GitHub are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT

mkdir gfs.v16.3.34
cd gfs.v16.3.34
git clone -b OMD-gfs.v16.3.34 https://github.com/NOAA-EMC/global-workflow.git .

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

* in `versions/run.ver` change `version=v16.3.34` and `gfs_ver=v16.3.34`
* in `versions/run.ver` remove `nam_ver=v4.2`

SORC CHANGES
------------

* No changes from GFS v16.3.33 

JOBS CHANGES
------------

* Remove COMINnam from jobs/JGFS_ATMOS_GEMPAK_META

PARM/CONFIG CHANGES
-------------------

* No changes from GFS v16.3.33

SCRIPT CHANGES
--------------

* Removes ush/gfs_meta_comp.sh and ush/gfs_meta_mar_comp.sh scripts.

FIX CHANGES
-----------

* Updates gempak/fix/gfs_meta file to account for script removal. 

MODULE CHANGES
--------------

* No changes from GFS v16.3.33.

CHANGES TO FILE AND FILE SIZES
------------------------------

* Removes files written to /lfs/h1/ops/prod/com/gfs/v16.3/gfs.$PDY/$cyc/atmos/gempak/meta/gfs_$PDY_$cyc_us_comp.
* Removes files written to /lfs/h1/ops/prod/com/gfs/v16.3/gfs.$PDY/$cyc/atmos/gempak/meta/gfs_$PDY_$cyc_us_mar_comp

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

* No changes from GFS v16.3.33.

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * N/A
* Does this change require a 30-day evaluation?
  * No

DISSEMINATION INFORMATION
-------------------------

* Removes DBN alerts for availability of /lfs/h1/ops/prod/com/gfs/v16.3/gfs.$PDY/$cyc/atmos/gempak/meta/gfs_$PDY_$cyc_us_comp 
* Removes DBN alerts for availability of /lfs/h1/ops/prod/com/gfs/v16.3/gfs.$PDY/$cyc/atmos/gempak/meta/gfs_$PDY_$cyc_us_mar_comp 

HPSS ARCHIVE
------------

* No changes from GFS v16.3.33

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes from GFS v16.3.33

DOCUMENTATION
-------------

* No changes from GFS v16.3.33

PREPARED BY
-----------
Travis.J.Elless@noaa.gov
Benjamin.Blake@noaa.gov
David.Huber@noaa.gov
Wen.Meng@noaa.gov
