GFS V17.0.0 RELEASE NOTES

-------
PRELUDE
-------
This is a major implementation of GFS (version 17.0) which includes: 
* A Fully Coupled atmosphere-land-ice-ocean and wave model 
* Using JEDI for new ocean/ice/snow DA while maintaining GSI for atmosphere/land DA
* Several updates to atmospheric physics, including using the Thompson microphysics and Noah-MP land model. 
* Increasing the atmospheric model resolution from 13 to 9km 
Additional details of the science upgrade can be found in the PNS here: https://www.weather.gov/media/notification/pdf_2026/pns26-29_Science_for_GFSv17.pdf 

OMD (previously EMC) conducted extensive retrospective and realtime testing.  Details of the retrospectives can be found here https://docs.google.com/spreadsheets/d/1N3isKTVmE4ITdiULDLP5lK1RoZOzkNlHFN-NFHwrH6o/edit?gid=492588212#gid=492588212 and the field evaluation home page is here: https://www.emc.ncep.noaa.gov/users/meg/gfsv17/ 

To accommodate the new components, many workflow changes including directory and filename changes have occurred.  This document describes the changes in detail. 


IMPLEMENTATION INSTRUCTIONS
---------------------------


Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
```

Clone the repository
```bash
git clone -b dev/gfs.v17 --recursive https://github.com/noaa-emc/global-workflow.git gfs.v17.0.0
```

Set gfs.v17.0.0 as HOMEGFS and cd into it
```bash
export HOMEgfs=$PWD/gfs.v17.0.0
cd ${HOMEgfs}
```

The clone extracts the following GFS components:

| Repository  | Link to production branch                     | 
| ---------- | ---------------------------------------------- |
| global-workflow | https://github.com/NOAA-EMC/global-workflow/tree/dev/gfs.v17 |
| GDAS App | https://github.com/NOAA-EMC/GDASApp/tree/release/gfs.v17 |
| jcb-algorithms | https://github.com/NOAA-EMC/jcb-algorithms/tree/release/1.0 |
| jcb-gdas | https://github.com/NOAA-EMC/jcb-gdas/tree/release/1.0 | 
| bufr-query | https://github.com/NOAA-EMC/bufr-query/tree/release/1.0 |
| DA-utils | https://github.com/NOAA-EMC/DA-utils/tree/release/1.0 | 
| fv3-jedi | https://github.com/JCSDA/fv3-jedi/tree/release/1.11 | 
| fv3-jedi-linearmodel | https://github.com/JCSDA/fv3-jedi-linearmodel/tree/release/1.7 | 
| GSW-Fortran | https://github.com/JCSDA-internal/GSW-Fortran/tree/release/soca-1.0.0 | 
| ioda | https://github.com/JCSDA/ioda/tree/release/2.11 | 
| jcb | https://github.com/NOAA-EMC/jcb/tree/release/1.0 |
| jedi-cmake | https://github.com/JCSDA/jedi-cmake/tree/master | 
| land-apply_jedi_incr | https://github.com/NOAA-EMC/land-apply_jedi_incr/tree/release/1.0 |
| oops | https://github.com/JCSDA/oops/tree/release/1.12 | 
| saber | https://github.com/JCSDA/saber/tree/release/1.12 | 
| soca | https://github.com/NOAA-EMC/soca/tree/release/1.0 |
| spoc| https://github.com/NOAA-EMC/spoc/tree/release/1.0 | 
| ufo | https://github.com/JCSDA/ufo/tree/release/1.12 |
| vader | https://github.com/JCSDA/vader/tree/release/1.9 |
| UFS_UTILS | https://github.com/ufs-community/UFS_UTILS/tree/production/GFS.v17.0.0 |
| ufs-weather-model | https://github.com/ufs-community/ufs-weather-model/tree/production/GFS.v17 |
| ufsatm | https://github.com/NOAA-EMC/ufsatm/tree/production/GFS.v17 | 
| UPP | https://github.com/NOAA-EMC/UPP/tree/release/gfs_v17 | 
| ccpp-physics | https://github.com/ufs-community/ccpp-physics/tree/production/GFS.v17 |
| WW3 | https://github.com/NOAA-EMC/WW3/tree/production/GFS.v17 |
| MOM6 | https://github.com/NOAA-EMC/MOM6/tree/GFSV17 |
| gfs-utils | https://github.com/NOAA-EMC/gfs-utils/tree/production/GFS.v17.0.0 |
| GSI | https://github.com/NOAA-EMC/GSI/tree/release/gfsda.v17 |
| GSI-utils | https://github.com/NOAA-EMC/GSI-utils/tree/release/gfs.v17 |
| build_gsinfo-fix | https://github.com/NOAA-PSL/build_gsinfo-fix/tree/gfsv17_historical |
| gsi_monitor | https://github.com/NOAA-EMC/GSI-Monitor/tree/release/gfs.v17 |


Build all executables and link them:  Utilizing `build_all.sh gfs gdas gsi` compiles all GFS components. Runtime output from the build for each package is written to log files in directory logs. To build an individual program, for instance, gsi_enkf, use `build_gsi_enkf.sh`.
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

The ecflow definition file is ${HOMEgfs}/ecf/defs/gfs_prod.def


VERSION FILE CHANGES
--------------------

In versions/build.ver the following are changed
* PrgEnv_intel_ver=8.5.0
* craype_ver-2.7.17
* cray_mpich_ver=8.1.19
* cmake_ver=3.27.9
* python_ver=3.12.0
* hdf5_ver=1.13.0
* netcdf_ver=4.9.2
* esmf_ver=8.8.0
* wgrib2_ver=2.8.0_wmo
* g2tmp_ver=1.17.0
* w3emc_ver=2.12.0
* bufr_ver=12.3.0
* g2_ver=3.5.1
* sp_ver=2.4.0
* ip_ver=5.2.0
* wrf_io_ver=1.2.0
* ncio_ver=1.1.2
* ncdiag_ver=1.1.2

In versions/build.ver the following are removed
* gempak_ver=7.14.1
* gfsio_ver=1.4.1
* upp_ver=8.3.0

In versions/build.ver the following are added
* cray-pals_ver 1.3.2
* zstd_ver=1.5.0
* libjpeg_ver=9c
* pnetcdf_ver=1.12.2
* pio_ver=2.5.10
* fms_ver=2024.01
* mapl_ver=2.53.0
* scotch_ver=7.0.4
* udunits_ver=2.2.28
* nemsiogfs_ver=2.5.3
* gsl_ver=2.7
* crtm_fix_ver=2.4.0.2
* Gftl_shared_ver-1.6.1
* landsfcutil_ver=2.4.1

In versions/run.ver the following are changed
* version=v17.0.0 
* gfs_ver=v17.0.0
* PrgEnv_intel_ver=8.5.0
* craype_ver=2.7.17
* cray_mpich_ver=8.1.19
* cray_pals_ver=1.3.2
* python_ver=3.12.0
* gempak_ver=7.15.1
* hdf5_ver=1.14.0
* netcdf_ver=4.9.2
* esmf_ver=8.8.0
* nco_ver=5.2.4
* bufr_dump_ver=1.3.2
* util_shared_ver=1.5.1
* grib_util_ver=1.2.4
* wgrib2_ver=2.0.8_wmo
* g2tmpl_ver=1.17.0
* nemsio_ver=2.5.4
* w3emc_ver=2.12.0
* g2_ver=3.5.1
* sp_ver=2.4.0
* ip_ver=5.2.0
* ncdiag_ver=1.1.2

In versions/run.ver the following are removed 
* radarl2_ver=v1.2 
* cmdaccel_ver=1.1 
* rtofs_ver=v2.5

In versions/run.ver the following are added:
* gw_ve_ver=17.0
* bufr_ver=12.3.0
* pnetcdf_ver=1.12.2
* mapl_ver=2.53.0
* pio_ver=2.5.10
* zstd_ver=1.5.0
* pigz_ver=2.3.4
* nccmp_ver=1.8.9.0
* boost_ver=1.79.0
* eigen_ver=3.4.0
* eckit_ver_1.28.0
* fckit_ver=0.13.1
* atlas_ver=0.39.0
* fms_ver=2024.01
* curl_ver=7.72.0
* gsl_lit_ver=v0.40.0
* wrf_io_ver=1.2.0
* scotch_ver=7.0.4

SORC CHANGES
------------

Renamed sorc directories:
* fv3gfs.fd -> ufs_model.fd 
* gsi.fd -> gsi_enkf.fd
* Global_enkf.fd -> enkf.fd
* Global_gsi.fd -> gsi.fd
* Global_chgres.fd -> chgres_cube.fd

Removed sorc directories:
* Enkf_chres_recenter.fd
* Filter_topo.fd
* fregrid.fd
* fv3nc2nemsio.fd
* gdas2gldas.fd
* gfs_post.fd
* gldas.fd
* gldas2gdas.fd
* Gldas_forcing.fd
* Gldas_model.fd
* Gldas_post.fd
* Gldas_rst.fd
* Make_hgrid.fd
* make_solo_mosaic.fd
* ncdiag_cat.fd
* nst_tf_chg.fd
* orog.fd
* Regrid_nemsio.fd
* shave.fd

Added sorc directories:
* ensadd.fd
* ensppf.fd
* ensstat.fd
* gdas.cd
* mkgfsawps.fd
* ocnicepost.fd
* overgridid.fd
* rdbfmsua.fd
* regridStates.fd
* Tref_calc.fd
* upp.fd
* Wave_stat.fd
* webtitle.fd
* WW3.fd 


JOBS CHANGES
------------

Many job scripts have been added, removed, or renamed to support the coupled forecast

Renamed job scripts are:
* JGDAS_ATMOS_ANALYSIS_DIAG -> JGLOBAL_ATMOS_ANALYSIS_DIAG
* JGDAS_ENKF_DIAG -> JGLOBAL_ENKF_DIAG
* JGDAS_ENKF_ECEN -> JGLOBAL_ENKF_ECEN
* JGDAS_ENKF_SELECT_OBS -> JGLOBAL_ENKF_SELECT_OBS
* JGDAS_ENKF_UPDATE -> JGLOBAL_ENKF_UPDATE
* JGDAS/JGFS_VMINMON -> JGLOBAL_VMINMON
* JGLOBAL_WAVE_POST_SBS -> JGLOBAL_WAVE_POST_GRIDDED

Removed jobs scripts are:
* JGDAS_ATMOS_GLDAS
* JGDAS_ENKF_SFC
* JGLOBAL_ATMOS_NCEPPOST
* JGLOBAL_ATMOS_POST_MANAGER
* JGLOBAL_WAVE_PREP

New job scripts are:
* JGLOBAL_ATMOS_SFCANL_GCYCLE
* JGLOBAL_ATMOS_SFCANL_REGRID
* JGLOBAL_ATMOS_PRODUCTS
* JGLOBAL_ATMOS_UPP
* JGLOBAL_ENKF_SFC_GCYCLE
* JGLOBAL_ENKF_SFC_REGRID
* JGLOBAL_FORECAST_MANAGER
* JGLOBAL_FSM
* JGLOBAL_MARINE_ANALYSIS_CHECKPOINT
* JGLOBAL_MARINE_ANALYSIS_ECEN
* JGLOBAL_MARINE_ANALYSIS_FINALIZE
* JGLOBAL_MARINE_ANALYSIS_INITIALIZE
* JGLOBAL_MARINE_ANALYSIS_VARIATIONAL
* JGLOBAL_MARINE_BMAT
* JGLOBAL_MARINE_BMAT_INITIALIZE
* JGLOBAL_MARINE_OBS_BUFR_DUMP
* JGLOBAL_MARINE_OBS_DUMP
* JGLOBAL_OCEANICE_PRODUCTS
* JGLOBAL_SNOW_ANALYSIS
* JGLOBAL_SNOWENS_ANALYSIS

PARM/CONFIG CHANGES
-------------------

* There are many parm/config changes to accommodate the coupled forecast including updating config files in ${PACKAGEROOT}/parm/config/gfs; similar to v16, these files set up job-specific parameters. Config files are added/removed for the job changes noted above, while modifications have been made to those currently used in v16. 

SCRIPT CHANGES
--------------

Many job scripts have been added, removed, or renamed to support the coupled forecast
* New scripts of note are exglobal_marine_* jobs for analysis and observation processing of marine obs. 
* exglobal_ceanice_products.py - New ocean & ice post processing script 
* exglobal_forecast_manager.sh - Copies forecast output to COM 

* The forecast job has been significantly refactored. 
* The wave gridded post processing is now by forecast hour and has been renamed to exglobal_wave_post_gridded.sh 
* Many scripts have been refactored with the goal of uniform standards and improved readability. 


FIX CHANGES
-----------
* The fix files have been overhauled to remove “fix” from the directory names and new fix files have been added for the coupled components.  A description of the fix files can be found in the google document here: https://docs.google.com/spreadsheets/d/1BeIvcz6TO3If4YCqkUK-oz_kGS9q2wTjwLS-BBemSEY/edit?gid=526608225#gid=526608225


MODULE CHANGES
--------------
* Currently waiting on GDIT installed ve/gw/17.0 and ecbuild/3.7.2
* See version file changes for other module updates



CHANGES TO FILE NAMES
---------------------

* A list of file name changes for files that are used by downstream models can be found here: https://docs.google.com/spreadsheets/d/1BG_IixIzNcwZqMLrPsa0ixlW_ji1vvXEwpVfwLNAgR0/edit?gid=0#gid=0 


RESOURCE CHANGES
--------------------------------

The total COM will increase from 8.9TB/cycle to 14.5 TB/cycle.

The follow jobs have significant time or resource changes: 

* JGDAS_ENKF_ECEN
increases in runtime from 3.6-3.9 min to 9-11.5 min 
* JGDAS_ENKF_SFC 
Increases in runtime from 1.6-1.8 to 9.5-13 min 
Increases from 1 to 8 nodes
* JGDAS_ENKF_FCST
Each member increases from 4 to 8 nodes 
* JGDAS_ENKF_POST
Decreases node usage from 3 to 1 node
Increases runtime from 7.5-8 min to 15.7-16.3 min 
* (GDAS) JGLOBAL_ATOMS_ANALYSIS
Node usage increases from 52 to 100 nodes
* (GDAS) JGLOBAL_FORECAST
Time increases from 20.6 to 36.3-38.2 min 
Increases from 27 to 95 nodes
*(GFS)JGLOBAL_ATMOS_ANALYSIS
Time increases from 25.2-27.8 to 26-30 min
Node usage increases from 55 to 100 min
* (GFS) JGLOBAL_FORECAST 
Time increases from 106-108 to 120-228 min 
Node usage increases from 112 to 494 nodes 

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * All 
* Does this change require a 30-day evaluation?
  * Yes

DISSEMINATION INFORMATION
-------------------------

* Details are described in the draft SCN here: https://docs.google.com/document/d/14La28XwsEemK6a1_zA7_HjnCy_thiaT8Pmp3HQH__R0/edit?usp=sharing

HPSS ARCHIVE
------------

* The HPSS archive is expected to jump from 2.75 TB to 4 TB per cycle in v17.  A list of name changes can be found here: https://docs.google.com/spreadsheets/d/1ZjZejZPyhr9FW9rd2tiVzzoTOFI0SoAfW6rqTo9jrE8/edit?gid=1430735894#gid=1430735894

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* Job Dependencies and flow diagram can be found in the internal NOAA document 
  here: https://docs.google.com/presentation/d/1VRkZR5Qg7XPqEukza50soyzSGakMXpy_csgfog0yFDo/edit


PREPARED BY
-----------

* Travis.J.Elless@noaa.gov
* Jessica.Meixner@noaa.gov
* Catherine.Thomas@noaa.gov
* Ruiyu.Sun@noaa.gov
* David.Huber@noaa.gov
