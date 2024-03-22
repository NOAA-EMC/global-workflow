GFS V16.3.14 RELEASE NOTES

-------
PRELUDE
-------

This upgrade adds a new GDAS job - `gdaswdqms`. It generates 6-hourly summaries of observation reports, which contain information on time, location, station ID, and quality (usage flag and innovations) from GDAS analysis. The objective of this new job is to support the WMO WDQMS (WIGOS Data Quality Monitoring System) project and ensure that WMO observational data and products are reliable and correspond to agreed-upon needs.  

The 6-hourly summaries include surface (synop), upper-air, and surface marine (ship and buoy) observations. 

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub .com are used to manage the GFS code.  The SPA(s) handling the GFS implementation need to have permissions to clone VLab Gerrit repositories and private NCAR UPP_GTG repository. All NOAA-EMC organization repositories are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
mkdir gfs.v16.3.14
cd gfs.v16.3.14
git clone -b EMC-v16.3.14 https://github.com/NOAA-EMC/global-workflow.git .
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

* `versions/run.ver` - change `version=v16.3.14` and `gfs_ver=v16.3.14`

SORC CHANGES
------------

* No changes from GFS v16.3.13

JOBS CHANGES
------------

* Added a new `j-job` `JGDAS_ATMOS_ANALYSIS_WDQMS`

PARM/CONFIG CHANGES
-------------------

* Added a `config.wdqms` file for the new j-job added.
* Added new configuration parameter `DO_WDQMS` in the following configuration files
  - `config.base.emc.dyn` (default to NO)
  - `config.base.nco.static` (default to YES)
* Added resource parameters for `WDQMS` job in the following resource configuration files
  - `config.resource.emc.dyn`
  - `config.resource.nco.static`

SCRIPT CHANGES
--------------

* Added `exscript` `exgdas_atmos_analysis_wdqms.sh`
* Added a utility in `ush` `wdqms.py` called by the new job.
* Added `ecf` script `jgdas_atmos_analysis_wdqms.ecf` and updated the `ecflow` suite definition files.

FIX CHANGES
-----------

* No changes from GFS v16.3.13

MODULE CHANGES
--------------

* New job loads `python` module in the ecf script.

CHANGES TO FILE SIZES
---------------------

* No changes of existing file sizes from GFS v16.3.13
* The `gdaswdqms` job creates three observation quality reports in CSV format for the following data types
  - Upper air (~0.5 MB)
  - Marine (-6.5 MB)
  - Synop (~ 22 MB)

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

* Computing resource added for the `gdaswdqms` job for each gdas cycle: 

  ```
    export wtime_wdqms="00:20:00"
    export npe_wdqms=1
    export nth_wdqms=1
    export npe_node_wdqms=1
    export memory_wdqms="48GB"
  ```

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * WDQMS
* Does this change require a 30-day evaluation?
  * No

DISSEMINATION INFORMATION
-------------------------

* No changes from GFS v16.3.13

HPSS ARCHIVE
------------

* No changes from GFS v16.3.13

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* Added a new job `gdaswdqms` after `gdasanaldiag` with dependency on the completion of `gdasanaldiag` job.

DOCUMENTATION
-------------

* No changes from GFS v16.3.13

PREPARED BY
-----------
Kate.Friedman@noaa.gov Rahul.Mahajan@noaa.gov Emily.Liu@noaa.gov
