GFS V17.0.0 RELEASE NOTES

-------
PRELUDE
-------
Major implementation of GFS to include .... 

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub are used to manage the GFS code.  All NOAA-EMC organization repositories on GitHub are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
```

# Clone the repository
```bash
git clone -b dev/gfs.v17 --recursive https://github.com/noaa-emc/global-workflow.git gfs.v17.0.0
```


# Set gfs.v17.0.0 as HOMEGFS and cd into it
```bash
export HOMEgfs=$PWD/gfs.v17.0.0
cd ${HOMEgfs}
```

#TO DO: update
The clone extracts the following GFS components:

| Component  | Tag                          | POC                     |
| ---------- | ---------------------------- | ----------------------- |
| MODEL      | GFS.v16.3.26                 | First.Last@noaa.gov       |



Build all executables and link them: The `build_all.sh` script compiles all GFS components. Runtime output from the build for each package is written to log files in directory logs. To build an individual program, for instance, gsi, use `build_gsi.sh`.
```bash
cd ${HOMEgfs}/sorc
./build_all.sh gfs gdas gsi 
```

Next, link the executables, fix files, parm files, etc in their final respective locations by executing:
```bash
./link_workflow.sh -o 
```

Setup gfs for NCO 
```bash
cd ${HOMEgfs}/dev/ush
source gw_setup.sh
./setup_gfs_for_nco.py
```

Create ecf links to create files for each forecast hour 
```bash
cd ${HOMEgfs}/ecf 
./setup_ecf_links.sh 
```

Note, cp /lfs/h2/emc/obsproc/noscrub/iliana.genkova/marine_dumplist $COMROOT_GFS/sdm_rtdm/. needs added to the release notes somehow


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
