GFS V16.2.1 RELEASE NOTES

-------
PRELUDE
-------

Several bug fixes for the GFSv16.2 package to resolve issues with the gfs_forecast job (wave restart calculation) and the gfs_atmos_postsnd (bufr sounding) job.

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and both the NOAA-EMC and NCAR organization spaces on GitHub.com are used to manage the GFS.v16.2.1 code. The SPA(s) handling the GFS.v16.2.1 implementation need to have permissions to clone VLab gerrit repositories and the private NCAR UPP_GTG repository. All NOAA-EMC organization repositories are publicly readable and do not require access permissions. Please follow the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
mkdir gfs.v16.2.1
cd gfs.v16.2.1
git clone -b EMC-v16.2.1 https://github.com/NOAA-EMC/global-workflow.git .
cd sorc
./checkout.sh -o
```

The checkout script extracts the following GFS components:

| Component | Tag         | POC               |
| --------- | ----------- | ----------------- |
| MODEL     | GFS.v16.2.0   | Jun.Wang@noaa.gov |
| GSI       | gfsda.v16.2.0 | Russ.Treadon@noaa.gov |
| GLDAS     | gldas_gfsv16_release.v.2.0.0 | Helin.Wei@noaa.gov |
| UFS_UTILS | ops-gfsv16.2.0 | George.Gayno@noaa.gov |
| POST      | upp_v8.1.2 | Wen.Meng@noaa.gov |
| WAFS      | gfs_wafs.v6.2.8 | Yali.Mao@noaa.gov |

To build all the GFS components, execute:
```bash
./build_all.sh
```
The `build_all.sh` script compiles all GFS components. Runtime output from the build for each package is written to log files in directory logs. To build an individual program, for instance, gsi, use `build_gsi.sh`.

Next, link the executables, fix files, parm files etc in their final respective locations by executing:
```bash
./link_fv3gfs.sh nco wcoss2
```

Lastly, link the ecf scripts by moving back up to the ecf folder and executing:
```bash
cd ../ecf
./setup_ecf_links.sh
```

SORC CHANGES
------------

* Workflow
  * `sorc/gfs_bufr.fd/calpreciptype.f`
  * `sorc/gfs_bufr.fd/meteorg.f`

FIX CHANGES
-----------

* No changes from GFS v16.2.0

PARM/CONFIG CHANGES
-------------------

* Workflow
  * `env/WCOSS2.env` - postsnd adjustments
  * `parm/config/config.resources.nco.static` - postsnd adjustments
  * `parm/config/config.resources.emc.dyn` - postsnd adjustments
  * `parm/transfer/transfer_rdhpcs_gfs_nawips.list`

JOBS CHANGES
------------

* No changes from GFS v16.2.0

SCRIPT CHANGES
--------------

* Workflow
  * `ecf/scripts/gfs/atmos/post_processing/bulletins/jgfs_atmos_fbwind.ecf` - memory adjustment
  * `ecf/scripts/gfs/atmos/post_processing/bufr_sounding/jgfs_atmos_postsnd.ecf` - resource adjustment
  * `scripts/exglobal_forecast.sh` - update calculation of starting time of rerun based on if wave restarts exist

MODULE CHANGES
--------------

* Workflow
  * `modulefiles/gfs_bufr.wcoss2.lua` - no longer build with -qopenmp

CHANGES TO RESOURCES AND FILE SIZES
-----------------------------------

* File sizes
  * No change to GFSv16.2.0.
* Resource changes
  * Adjustment to the gfs_atmos_postsnd job resources.
  * Increase to memory for fbwind job.

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * The entire GFS v16.2.1 package needs to be installed and tested.
* Does this change require a 30-day evaluation?
  * No.

DISSEMINATION INFORMATION
-------------------------

* Where should this output be sent?
  * No change from GFS v16.2.0
* Who are the users?
  * No change from GFS v16.2.0
* Which output files should be transferred from PROD WCOSS2 to DEV WCOSS2?
  * No change from GFS v16.2.0
* Directory changes
  * No change from GFS v16.2.0
* File changes
  * No change from GFS v16.2.0

HPSS ARCHIVE
------------

* No change from GFS v16.2.0

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------
* No change from GFS v16.2.0
