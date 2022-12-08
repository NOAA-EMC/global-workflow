GFS V16.3.5 RELEASE NOTES

-------
PRELUDE
-------

The GFSv16.3.4 is upgraded to GFSv16.3.5 to allow continued use of atmospheric motion vectors from the GOES-W satellite as GOES-18 replaces GOES-17 and to include the assimilation of VIIRS radiances from S-NPP and NOAA-20 (which were to be included in the v16.3 implementation but were omitted due to data availability issues).

Specifically, the following changes are made to the indicated GSI fix files:
* global_satinfo.txt
  * Turn on VIIRS radiances from NPP and J1 for channels 12, 15, and 16
* global_convinfo.txt
  * Turn off GOES-17 (271) AMV winds for types: 245 (IRLW), 246 (WVCT), and 247(WVCA) due to deteriorating conditions
  * Turn on GOES-18 (272) AMV winds for types: 245 (IRLW), 246 (WVCT), and 247(WVCA)

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub .com are used to manage the GFS code.  The SPA(s) handling the GFS implementation need to have permissions to clone VLab Gerrit repositories and private NCAR UPP_GTG repository. All NOAA-EMC organization repositories are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
mkdir gfs.v16.3.5
cd gfs.v16.3.5
git clone -b EMC-v16.3.5 https://github.com/NOAA-EMC/global-workflow.git .
cd sorc
./checkout.sh -o
```

The checkout script extracts the following GFS components:

| Component | Tag         | POC               |
| --------- | ----------- | ----------------- |
| MODEL     | GFS.v16.3.0   | Jun.Wang@noaa.gov |
| GLDAS     | gldas_gfsv16_release.v.2.1.0 | Helin.Wei@noaa.gov |
| GSI       | gfsda.v16.3.5 | Emily.Liu@noaa.gov |
| UFS_UTILS | ops-gfsv16.3.0 | George.Gayno@noaa.gov |
| POST      | upp_v8.2.0 | Wen.Meng@noaa.gov |
| WAFS      | gfs_wafs.v6.3.1 | Yali.Mao@noaa.gov |

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

* `ecf/versions/gfs.ver` - change `obsproc_ver=v1.1.2`
* `versions/hera.ver` - change `obsproc_run_ver=v1.1.2`
* `versions/orion.ver` - change `obsproc_run_ver=v1.1.2`
* `versions/run.ver` - `change `version=v16.3.5` and `gfs_ver=v16.3.5`
* `versions/wcoss2.ver` - change `obsproc_run_ver=v1.1.2`

SORC CHANGES
------------

* No changes from GFS v16.3.4

JOBS CHANGES
------------

* No changes from GFS v16.3.4

PARM/CONFIG CHANGES
-------------------

* Removal of unused transfer parm files from `/parm/transfer` (based on NCO feedback). 

SCRIPT CHANGES
--------------

* No changes from GFS v16.3.4

FIX CHANGES
-----------

* GSI:
  * Modified:
    * `fix_gsi/global_satinfo.txt`
    * `fix_gsi/global_convinfo.txt`

MODULE CHANGES
--------------

* A couple R&D modulefiles were updated for developer support and do not impact WCOSS2 ops.

CHANGES TO FILE SIZES
---------------------

* No changes from GFS v16.3.4

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

* No changes from GFS v16.3.4

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * GSI 
* Does this change require a 30-day evaluation?
  * No

DISSEMINATION INFORMATION
-------------------------

* No changes from GFS v16.3.4

HPSS ARCHIVE
------------

* No changes from GFS v16.3.4

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes from GFS v16.3.4

DOCUMENTATION
-------------

* No changes from GFS v16.3.4

PREPARED BY
-----------
Kate.Friedman@noaa.gov
Andrew.Collard@noaa.gov
Emily.Liu@noaa.gov
