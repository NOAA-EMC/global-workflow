GFS V16.3.22 RELEASE NOTES

-------
PRELUDE
-------

The Gulf of Mexico is renamed to the Gulf of America. This is a non-science change and does not impact model output.

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub are used to manage the GFS code.  The SPA(s) handling the GFS implementation need to have permissions to clone VLab Gerrit repositories.  All NOAA-EMC organization repositories on GitHub are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
mkdir gfs.v16.3.22
cd gfs.v16.3.22
git clone -b EMC-v16.3.22 https://github.com/NOAA-EMC/global-workflow.git .
cd sorc
./checkout.sh -o
```

The checkout script extracts the following GFS components:

| Component | Tag         | POC               |
| --------- | ----------- | ----------------- |
| MODEL     | GFS.v16.3.22 | Jun.Wang@noaa.gov |
| GLDAS     | gldas_gfsv16_release.v.2.1.0 | Helin.Wei@noaa.gov |
| GSI       | gfsda.v16.3.20 | Andrew.Collard@noaa.gov |
| UFS_UTILS | ops-gfsv16.3.0 | George.Gayno@noaa.gov |
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

* `versions/run.ver` - change `version=v16.3.22` and `gfs_ver=v16.3.22`

SORC CHANGES
------------

* New MODEL tag `GFS.v16.3.22` - includes WW3 updates for name change, results do not change

JOBS CHANGES
------------

* No changes from GFS v16.3.21

PARM/CONFIG CHANGES
-------------------

Internal name changes updates:
* `parm/product/bufr_stalist.meteo.gfs`
* `parm/parm_wave/bull_awips_gfswave`

SCRIPT CHANGES
--------------

* No changes from GFS v16.3.21

FIX CHANGES
-----------

Internal name change updates:
* `wave_gfs.buoys`
* `wave_gfs.buoys.full`
* `wave_gfs.buoys.dat`

MODULE CHANGES
--------------

* No changes from GFS v16.3.21

CHANGES TO FILE AND FILE SIZES
------------------------------

* No changes from GFS v16.3.21

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

* No changes from GFS v16.3.21

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * None
* Does this change require a 30-day evaluation?
  * No

DISSEMINATION INFORMATION
-------------------------

* No changes from GFS v16.3.21

HPSS ARCHIVE
------------

* No changes from GFS v16.3.21

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes from GFS v16.3.21

DOCUMENTATION
-------------

* No changes from GFS v16.3.21

PREPARED BY
-----------
Kate.Friedman@noaa.gov
Jessica.Meixner@noaa.gov
Brian.Curtis@noaa.gov
Bo.Cui@noaa.gov
