GFS V16.3.22 RELEASE NOTES

-------
PRELUDE
-------
The upstream OBSPROC package is updated to v1.3. Along with this are the following companion updates:
* workflow and UFS_UTILS package updates to use the new AFWA global snow file due to the hemispheric snow files being phased out
* updated GSI code and convinfo file for saildrone observations
* ww3_outp is improved for the wave point output

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
| MODEL     | GFS.v16.3.22   | Jun.Wang@noaa.gov |
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

* * `versions/run.ver` - change `version=v16.3.22`, `gfs_ver=v16.3.22`, and `obsproc_ver=v1.3`

SORC CHANGES
------------

* New UFS_UTILS tag - `emcsfc_snow2mdl` program and associated scripts are updated to process global AFWA snow data
* New GSI tag - `src/gsi/read_prepbufr.f90` code update for new saildrone subtype
* New MODEL tag - WW3 program `ww3_outp` and associated scripts are improved to process the per-time-step point outputs more efficiently.

JOBS CHANGES
------------

* No changes from GFS v16.3.21

PARM/CONFIG CHANGES
-------------------

* `config.resources.emc.dyn` and `config.resources.nco.static` are changed to reduce the resources accordingly.

SCRIPT CHANGES
--------------

* `scripts/exgfs_wave_post_pnt.sh` is changed to create be compatible with the new `ww3_outp`.

FIX CHANGES
-----------

* GSI `global_convinfo.txt` fix update for saildrone```

MODULE CHANGES
--------------

* No changes from GFS v16.3.21

CHANGES TO FILE AND FILE SIZES
------------------------------

No longer ingest:
* `${RUN}.${cycle}.NPR.SNWN.SP.S1200.MESH16.grb` (`AFWA_NH_FILE`)	
* `${RUN}.${cycle}.NPR.SNWS.SP.S1200.MESH16.grb` (`AFWA_SH_FILE`)

Now ingest:	
* `${RUN}.${cycle}.snow.usaf.grib2` (`AFWA_GLOBAL_FILE`)

No longer uses the parallel command file `cmdfile` for the `scripts/exgfs_wave_post_pnt.sh`.


ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

Reduce the ncpu and wtime as follow:
* for jobs/JGLOBAL_WAVE_POST_BNDPNT; ncpu from 240 to 1; wtime from 1hr to 30min
* for jobs/JGLOBAL_WAVE_POST_BNDPNTBLL; ncpu from 448 to 2; wtime from 1hr to 10min
* for jobs/JGLOBAL_WAVE_POST_PNT; ncpu from 200 to 3; wtime from 1.5hr to 35min

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * emcsfc_sfc_prep and analysis
  * wave_post_pnt
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
George.Gayno@noaa.gov
Andrew.Collard@noaa.gov
Jessica.Meixner@noaa.gov
Ali.Salimi@noaa.gov
