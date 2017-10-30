-----------------------------------------------------------
-----------------------------------------------------------
   NCEP-EMC GFS IMPLEMENTATION SUPERSTRUCTURE PACKAGES
       Version 15.0.0, GFS with FV3 Dynamical Core	
-----------------------------------------------------------
-----------------------------------------------------------

=============================================================
Instruction for Running Forcast-Only Experiments Using 
NEMS FV3GFS with Q7FY17 NEMS GSM Analyses, V3.0
May 2017
=============================================================

The workflow has been updated to run the NEMS FV3GFS instead 
of the stand-alone FV3GFS.  NEMS FV3GFS contains the Interoperable 
Physics Driver Version 4.0 (IPD4). The Near Sea Surface Temperature
(NSST) model is now included in IPD4. The forecast-only workflow
has been tested on WCOSS (Luna and Surge). Minor changes may be 
required for running forecast-only experiments on Theia.

A prototype Rocoto-based workflow has been developed as well. Please 
contact Rahul.Mahajan@noaa.gov if users wish to run cycled experiments 
with data assimilation. Right now it does not support verification.

The instruction given below is for running forecast-only experiments 
using the traditional psub and pend-based workflow.

1. Check out model https://svnemc.ncep.noaa.gov/projects/nems/apps/NEMSfv3gfs/trunk
   Use ./trunk/tests/compile.sh to compile for a particular configuration of the model.  
   /gpfs/hps/emc/global/noscrub/Fanglin.Yang/svn/fv3gfs/NEMSfv3gfs/build.sh can 
   be used to create different executables at once with hydro or non-hydro and 
   32-bit or 64-bit options.
   
2. create a directory on Luna/Surge
   mkdir -p /gpfs/hps/emc/global/noscrub/$LOGNAME/svn/fv3gfs
   cd  /gpfs/hps/emc/global/noscrub/$LOGNAME/svn/fv3gfs
   Check out the workflow repository from the trunk or a branch you created
      https://svnemc.ncep.noaa.gov/projects/fv3gfs/trunk
   
3. Under either ./trunk  you will find the following directories
   gfs_workflow.v15.0.0/
   global_shared.v15.0.0/
   gdas.v15.0.0/
   gfs.v15.0.0/

4. goto ./global_shared.v15.0.0/sorc, run "build_all.sh cray or theia" to compile 
   all utilities.

5. create your experiment directory, for instance
    mkdir -p /gpfs/hps/emc/global/noscrub/$LOGNAME/para_gfs/prfv3test
  
   then copy all files in ./gfs_workflow.v15.0.0/para/exp_fv3gfs to prfv3test

6. In ./prtest/submit_fv3gfs.sh, change "BASE_SVN" and "tags" to point to your local package.
    Change "CASE" for model resolution, and "START"/"LAST" for experiment dates.
    
7. Use "bsub <submit_fv3gfs.sh" to submit your job. The script will first run CHGRES to create 
    FV3GFS initial conditions using analyses from NEMS GSM parallel prnemsrn,  then submit a 
    forecast job.  After fcst completes, the system will automatically submit post, vrfy and arch jobs.


=======================================================================================
Instruction for Running Forcast Experiments with Operational GFS Initial Conditions, V2.0
March 2017
=======================================================================================

1. The package so far only works on Cray (Luna and Surge).

2. create a directory on Luna/Surge: 
   mkdir -p /gpfs/hps/emc/global/noscrub/$LOGNAME/svn/fv3gfs
   cd  /gpfs/hps/emc/global/noscrub/$LOGNAME/svn/fv3gfs
   
3. If you are going to run experiments without making changes, check out the trunk 
   svn co https://svnemc.ncep.noaa.gov/projects/fv3gfs/trunk

4. If you are going to make changes, create a branch of the trunk, for instance
   svn copy https://svnemc.ncep.noaa.gov/projects/fv3gfs/trunk  https://svnemc.ncep.noaa.gov/projects/fv3gfs/branches/yourname/myfv3

   then checkout your branch: svn co https://svnemc.ncep.noaa.gov/projects/fv3gfs/branches/yourname/myfv3

5. Under either ./trunk or ./myfv3 you will find the following directories
   gfs_workflow.v15.0.0/
   global_shared.v15.0.0/
   gdas.v15.0.0/
   gfs.v15.0.0/
   lib/

6. goto ./lib, run "build_sfcio.sh" to build the special sfcio library

7. goto ./global_shared.v15.0.0/sorc, run "build_all.sh cray" to build all utility  executables 

8. goto ./global_shared.v15.0.0/sorc/fv3gfs.fd/BUILD, run "COMP_ALL_avx" to build rwo forecst executables 
   for non-hydrostatic 32-bit and 64-bit cases.  You need to modify COMP_ALL_avx if you want to build
   hydrostatic executables as well.

9. create your experiment directory, for instance
    mkdir -p /gpfs/hps/emc/global/noscrub/$LOGNAME/para_gfs/prtest
  
   then copy all files in ./gfs_workflow.v15.0.0/para/exp_fv3gfs to prtest

10. In ./prtest/submit_fv3gfs.sh, change "BASE_SVN" and "tags" to point to your local package.
    Change "CASE" for model resolution, and "START"/"LAST" for experiment dates.
    
11. Use "bsub <submit_fv3gfs.sh" to submit your job. The script will first run CHGRES to create 
    FV3GFS initial conditions and then submit a forecast job.  After fcst completes, the system 
    will automatically submit post, vrfy and arch jobs.


Note: the system is still under development.  Chnages are made frequently and committed to the trunk.
      Not all parts are working without flaws. Cycled experiments cannot be run using this package yet.
      The forecast model included in this package will soon be replaced with the NEMS FV3GFS, workflow 
      and scripts will be updated accordingly.




==================
Instruction V1.0
December 2016
==================

1. The first working version that can be used for creating boundary and initial conditions 
   and for running FV3GFS forecast-only experiments on WCOSS CRAY computers was saved in the 
   trunk  https://svnemc.ncep.noaa.gov/projects/fv3gfs/trunk/. It has now been moved to 
   https://svnemc.ncep.noaa.gov/projects/fv3gfs/tags/fv3gfs_v1/

2. Based on the Q3FY17 GFS Implementation Superstructure Packages (version 14.1.0)
   https://svnemc.ncep.noaa.gov/projects/gfs/branches/gfs_q3fy17/, FV3GFS forecast model 
   and utilities have been implemented into the Superstructure packages version 15.0.0. 
   Now the trunk contains the following sub-directories

   gfs_workflow.v15.0.0/
   global_shared.v15.0.0/
   gdas.v15.0.0/
   gfs.v15.0.0/
   lib/

   Significant changes have been made to most scripts and utilities in the workflow
   and global_shared packages to simplify and streamline the steps from forecast to archive.  
   Right now the packages can be used for preparing cold start initial conditons, running 
   forecasts at different resolutions, and running post, verification and archive steps. 
   The steps from fcst to arch have been streamlined.  

3. Users can check out global_shared.v15.0.0, build utility executables on Cray using 
   ./global_shared.v15.0.0/sorc/build_all.sh.  The fv3gfs forecast model itself is saved
   under ./global_shared.v15.0.0/sorc/fv3gfs.fd. Use ./sorc/fv3gfs.fd/BUILD/COMP_ALL_avx
   to build forecast executables. Please note after the FV3GFS is implemented in the NEMS system,
   fv3gfs.fd will be replaced by NEMS FV3GFS.

4. Check out gfs_workflow.v15.0.0, copy ./gfs_workflow.v15.0.0/para/exp_fv3gfs to your experiment
   directory, for instance, Luna/Surge: /gpfs/hps/emc/global/noscrub/$LOGNAME/para_gfs/prtest, and use
   submit_fv3gfs.sh to create cold start initial conditions from current operational GFS ICs and submit
   a fcst job.  After fcst completes, the system will automatically submit post, vrfy and arch jobs.
   Forecast model resolution, computing node usage, forecast length and all environment variables
   and directories are set up in para_config.  Currently the system supports resolutions at C49, C96,
   C192, C384 and C768.  Note that in this version each step now has its own configure file. 

5. FV3-related utilities can be found in ./global_shared.v15.0.0/ush, including

   #--for creating cold start FV3GFS initial conditions 
   fv3gfs_driver_chgres.sh*  
   fv3gfs_chgres.sh*         

   #--for creating FV3 grids and fixed fields
   fv3gfs_driver_grid.sh*  
   fv3gfs_make_grid.sh*  
   fv3gfs_remap_weights.sh*
   fv3gfs_filter_topo.sh*  
   fv3gfs_make_orog.sh*  

   #--for remapping FV3 6-tile foercast output to global lat-lon grids
      and convert from netCDF to nemsio format. Both are used by post.sh.
   fv3gfs_remap.sh
   fv3gfs_nc2nemsio.sh*  
   


----------------------------------------------------------------------------------------
For questions and comments, please contact
Fanglin.Yang@noaa.gov
Jun.Wang@noaa.gov
Rusty.Benson@noaa.gov
