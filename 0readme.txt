-----------------------------------------------------------
-----------------------------------------------------------
   NCEP-EMC GFS IMPLEMENTATION SUPERSTRUCTURE PACKAGES
       Version 15.0.0, GFS with FV3 Dynamical Core	
-----------------------------------------------------------
-----------------------------------------------------------

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
