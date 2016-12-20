 program driver
!$$$  main program documentation block
!                .      .    .                                       .
! main program: snow2mdl
!   prgmmr: gayno            ORG: NP2                DATE: 2005-dec-16
!
! abstract: create a snow cover and snow depth analysis on a nam
! or global gfs gaussian grid using nesdis/ims snow cover, autosnow snow
! cover and/or afwa depth data.
!
! program history log:
!   2005-dec-16  gayno     initial version
!   2007-nov-30  gayno     added processing of nam b-grids.
!                          improved thinning for gfs grids.
!   2008-feb-01  gayno     added option to use autosnow data
!                          in southern hemisphere.
!   2014-feb-14  gayno     read grib 1 or grib2 version of nesdis/ims
!                          data
!   2014-sep-30  gayno     read grib 1 or grib2 version of model
!                          lat, lon and mask files.  convert
!                          nh weekly snow climatology to grib 2.
!                          option to output model depth and cover 
!                          analysis in grib2.
!
! note: The ims snow cover product is now produced by the national
!       ice center, not nesdis.  however, in this code, references
!       to ims still use the "nesdis" identifier.
!
!   input files, to define model grid, and for runtime configuration:
!     - fort.41 configuration namelist
!     - latitudes on model grid (grib 1 or grib 2)
!     - longitudes on model grid (grib 1 or grib 2)
!     - model land/sea mask (grib 1 or grib 2)
!     - gfs definition of reduced grid (ascii, optional)
!
!   input snow data files; see remarks section on how to select.
!     - nesdis/ims nh 4km (96 mesh) snow cover data (grib1 or grib 2)
!     - nesdis/ims nh 23km (16 mesh) snow cover data (grib1 or grib 2)
!     - nesdis/ims nh 23km land mask (ascii, required when using 23km ims)
!     - afwa nh/sh 8th mesh (46km) snow depth data (binary)
!     - afwa nh/sh 8th mesh land/sea mask (binary, required when using 
!                                     8th mesh nh/sh afwa data)
!     - afwa nh/sh 16th mesh (23km) snow depth data (grib 1)
!     - autosnow sh snow cover data (grib 2)
!     - nesdis/ims weekly nh snow cover climatology (grib 2)
!       (used to qc input snow data).
!
!   output files:  
!     - snow cover and depth on the model grid (grib1 or grib2)
!
!   exit states (non-zero condition is fatal):
!     cond =   0 - successful run
!          =  40 - bad open of file when checking its
!                  file type (routine grib_check).
!          =  41 - ims file not grib1 or grib2 format
!          =  47 - error in grib2_check.  unknown grid type.
!          =  48 - error creating model output grib2 file
!          =  49 - bad open of model output grib2 file
!          =  50 - error converting to grib2 grid description template
!          =  53 - bad nesdis/ims data.  
!          =  54 - bad input snow data selection
!          =  55 - error in iplib interpolation routine
!          =  57 - bad write of model snow depth data to grib1 file
!          =  58 - bad write of model snow cover data to grib1 file
!          =  59 - bad open of model output grib1 file
!          =  60 - bad open afwa snow data
!          =  61 - bad read afwa snow data
!          =  62 - bad open afwa land mask
!          =  63 - bad read afwa land mask
!          =  70 - bad degrib of nesdis/ims snow cover data
!          =  71 - bad degrib of nesdis/ims sea ice data
!          =  72 - bad degrib of nesdis/ims data header
!          =  73 - bad open of nesdis/ims data
!          =  74 - bad open of autosnow data
!          =  75 - bad degrib of autosnow data 
!          =  76 - problem with gfs 'lonsperlat' file
!          =  77 - bad open on configuration namelist
!          =  78 - bad read on configuration namelist
!          =  79 - unrecognized model grid type
!          =  80 - bad open on model latitude grib file
!          =  81 - bad read on model latitude grib header
!          =  82 - bad degrib of model latitude data
!          =  83 - bad open on model longitude file
!          =  84 - bad degrib of model longitude data
!          =  85 - bad open of model land mask file
!          =  86 - bad degrib of model land mask data
!          =  87 - bad open of nesdis/ims land mask file
!          =  88 - bad read of nesdis/ims land mask data
!          =  90 - model latitude file not grib1 or grib2
!          =  91 - model longitude file not grib1 or grib2
!          =  92 - model land mask file not grib1 or grib2
!
! remarks: The determination of cover and depth on the model
!   grid depends on the input snow data selected:
!
!   nam grids:
!   --------- 
!
!   1) nesdis/ims only - An analysis of snow cover on the
!      model grid is produced.  No depth analysis is 
!      produced.
!
!   2) afwa only - An analysis of snow cover and depth on
!      the model grid is produced.  Depth is determined from
!      the afwa data.  Cover is set to 100% where afwa indicates
!      snow and 0% otherwise.
!
!   3) nesdis/ims and afwa - An analysis of snow cover and
!      depth on the model grid is produced.  Cover is
!      determined by the nesdis/ims data.  If cover is 
!      greater than user-defined threshold (variable
!      snow_cvr_threshold) the depth is set to the afwa
!      value or a nominal value, whichever is greater.
!      The nominal value is user-defined (varaible 
!      min_snow_depth).  If cover is less than user-
!      defined threshold, the depth is set to 0,
!      regardless of the afwa depth value.
!
!   gfs grid:
!   --------
!
!   1) nesdis/ims and autosnow only - An analysis of snow 
!      cover and depth on the model grid is produced.  
!      Cover is determined from the ims and autosnow data.
!      If cover is greater than the user-defined
!      threshold (variable snow_cvr_threshold), the
!      the depth is set to the user-defined default
!      depth (variable min_snow_depth).
!
!   2) afwa only - An analysis of snow cover and depth on
!      the model grid is produced.  Depth is determined from
!      the afwa data.  Cover is set to 100% where afwa indicates
!      snow and 0% otherwise.
!
!   3) nesdis/ims, autosnow and afwa - An analysis of snow
!      cover and depth on the model grid is produced.  Cover is
!      determined by the ims and autosnow data.  If cover is 
!      greater than user-defined threshold (variable
!      snow_cvr_threshold) the depth is set to the afwa
!      value or a nominal value, whichever is greater.
!      The nominal value is user-defined (varaible 
!      min_snow_depth).  If cover is less than user-
!      defined threshold, the depth is set to 0,
!      regardless of the afwa depth value.
!
!$$$

 use snowdat,       only           : readnesdis, &
                                     readafwa,   &
                                     readautosnow

 use model_grid,    only           : read_mdl_grid_info, &
                                     model_grid_cleanup

 use snow2mdl,      only           : interp

 use program_setup, only           : read_config_nml

 implicit none

 call w3tagb('SNOW2MDL',2005,350,0000,'NP2')

 print*,''
 print*,"***********************"
 print*,"*** BEGIN EXECUTION ***"
 print*,"***********************"

!-----------------------------------------------------------------------
! get configuration stuff.
!-----------------------------------------------------------------------

 call read_config_nml

!-----------------------------------------------------------------------
! read input snow data.
!-----------------------------------------------------------------------

 call readautosnow ! autosnow snow cover data

 call readnesdis   ! nesdis/ims snow cover data

 call readafwa     ! afwa depth data

!-----------------------------------------------------------------------
! read information about the model grid to which the
! snow data will be interpolated.
!-----------------------------------------------------------------------

 call read_mdl_grid_info

!-----------------------------------------------------------------------
! interpolate the data to the model grid, then write
! it to a grib file.
!-----------------------------------------------------------------------
 
 call interp

!-----------------------------------------------------------------------
! free up memory
!-----------------------------------------------------------------------

 call model_grid_cleanup

 print*,''
 print*,'****************************'
 print*,'**** NORMAL TERMINATION ****'
 print*,'****************************'

 call w3tage('SNOW2MDL')

 stop

 end program driver
