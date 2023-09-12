GFS V16.3.9 RELEASE NOTES

-------
PRELUDE
-------

This change turns of ingest of NOAA-21 radiances into the GSI until CRTM fix file issues can be solved.  This is necessary for a satingest upgrade to proceed.  PlanetIQ GPSRO and NOAA-20 OMPS Ozone retrievals will also be actively assimilated, and the OMPS observation errors adjusted.

Also included in this update is a fix to a compilation error with "-check all" for gfs_bufrsnd, which resolves NCO bugzilla #1208.

Remove NEMSIOGFS dependency in sorc/build_tropcy_NEMS.sh.

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub .com are used to manage the GFS code.  The SPA(s) handling the GFS implementation need to have permissions to clone VLab Gerrit repositories and private NCAR UPP_GTG repository. All NOAA-EMC organization repositories are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
mkdir gfs.v16.3.9
cd gfs.v16.3.9
git clone -b EMC-v16.3.9 https://github.com/NOAA-EMC/global-workflow.git .
cd sorc
./checkout.sh -o
```

The checkout script extracts the following GFS components:

| Component | Tag         | POC               |
| --------- | ----------- | ----------------- |
| MODEL     | GFS.v16.3.0   | Jun.Wang@noaa.gov |
| GLDAS     | gldas_gfsv16_release.v.2.1.0 | Helin.Wei@noaa.gov |
| GSI       | gfsda.v16.3.8 | Andrew.Collard@noaa.gov |
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

* `versions/run.ver` - change `version=v16.3.9`, and  `gfs_ver=v16.3.9`

SORC CHANGES
------------

* Compilation error fix in sorc/gfs_bufr.fd/meteorg.f, resolves NCO bugzilla #1208

JOBS CHANGES
------------

* No changes from GFS v16.3.8

PARM/CONFIG CHANGES
-------------------

* No changes from GFS v16.3.8

SCRIPT CHANGES
--------------

* Changes to sorc/gsi.fd/scripts/exglobal_atmos_analysis.sh and sorc/checkout.sh
* Change to sorc/build_tropcy_NEMS.sh

FIX CHANGES
-----------

* Change to sorc/gsi.fd/fix/global_convinfo.txt
* Change to sorc/gsi.fd/fix/global_ozinfo.txt

MODULE CHANGES
--------------

* Remove nemsiogfs from modulefiles/modulefile.storm_reloc_v6.0.0.wcoss2.lua

CHANGES TO FILE SIZES
---------------------

* No changes from GFS v16.3.8

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

* No changes from GFS v16.3.8

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * GSI 
* Does this change require a 30-day evaluation?
  * No

DISSEMINATION INFORMATION
-------------------------

* No changes from GFS v16.3.8

HPSS ARCHIVE
------------

* No changes from GFS v16.3.8

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes from GFS v16.3.8

DOCUMENTATION
-------------

* No changes from GFS v16.3.8

PREPARED BY
-----------
Kate.Friedman@noaa.gov
Andrew.Collard@noaa.gov
Iliana.Genkova@noaa.gov
Walter.Kolczynski@noaa.gov
