GFS V17.0.0 RELEASE NOTES

-------
PRELUDE
-------
Major implementation of GFS to include .... 

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub are used to manage the GFS code.  The SPA(s) handling the GFS implementation need to have permissions to clone VLab Gerrit repositories.  All NOAA-EMC organization repositories on GitHub are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT

mkdir gfs.v16.3.32
cd gfs.v16.3.32
git clone -b OMD-gfs.v16.3.32 https://github.com/NOAA-EMC/global-workflow.git .

cd sorc
./checkout.sh -o
```

The checkout script extracts the following GFS components:

| Component  | Tag                          | POC                     |
| ---------- | ---------------------------- | ----------------------- |
| MODEL      | GFS.v16.3.26                 | Jun.Wang@noaa.gov       |
| GLDAS      | gldas_gfsv16_release.v.2.1.0 | Helin.Wei@noaa.gov      |
| GSI        | gfsda.v16.3.32               | Andrew.Collard@noaa.gov |
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

* List change... 

SORC CHANGES
------------

* List changes...  

JOBS CHANGES
------------

* New jobs include... 

PARM/CONFIG CHANGES
-------------------

* Many parm/config changes to accomatate the coupled forecast  

SCRIPT CHANGES
--------------

* Numerous script changes have been made to accomadate the coupled forecast 

FIX CHANGES
-----------

* Fix files have been updated for v17.  
  (Can we be more specific? There is a document desrcibing fix files) 

MODULE CHANGES
--------------

* (List module changes) 

CHANGES TO FILE AND FILE SIZES
------------------------------

* (List file and file size changes here) 

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

* (List resource changes here) 

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * All 
* Does this change require a 30-day evaluation?
  * Yes

DISSEMINATION INFORMATION
-------------------------

* (Note dissemination changes from v16 here)

HPSS ARCHIVE
------------

* (Note changes from v16 here) 

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* Job Dependencies and flow diagram can be found in the internal NOAA document 
  here: https://docs.google.com/presentation/d/1VRkZR5Qg7XPqEukza50soyzSGakMXpy_csgfog0yFDo/edit

DOCUMENTATION
-------------

* (Add info here) 

PREPARED BY
-----------
Jessica.Meixner@noaa.gov
Catherine.Thomas@noaa.gov
Ruiyu.Sun@noaa.gov
David.Huber@noaa.gov
Travis.J.Elless@noaa.gov
