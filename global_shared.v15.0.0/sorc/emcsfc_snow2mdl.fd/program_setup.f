 module program_setup
!$$$  module documentation block
!                .      .    .                                       .
! module:    program_setup
!   prgmmr: gayno         org: w/np2     date: 2005-DEC-16
!
! abstract: this module reads in data from the program's
!           configuration namelist.
!
! program history log:
!   2005-DEC-16  gayno   - initial version
!   2008-Feb-01  gayno   - added autosnow data
!   2014-Sep-30  gayno   - added 'output_grib2' flag
!
! usage: use program_setup
!
! remarks: some variable definitions
!   afwa_snow_nh_file       - path/name afwa n hemis snow depth
!   afwa_snow_sh_file       - path/name afwa s hemis snow depth
!   afwa_lsmask_nh_file     - path/name afwa n hemis land/sea mask
!   afwa_lsmask_sh_file     - path/name afwa s hemis land/sea mask
!   autosnow_file           - path/name s hemis autosnow file
!   grib_century/day/hour/month/year -  
!                             date of the final merged
!                             snow product that will be placed
!                             in grib header.
!   model_lat_file          - path/name lats on the model grid
!   model_lon_file          - path/name lons on the model grid
!   nesdis_lsmask_file      - path/name nesdis/ims land mask
!   nesdis_snow_file        - path/name nesdis/ims snow cover
!   lat_threshold           - equatorward of this latitude, model
!                             points with undefined cover or depth 
!                             (because the interpolation routines
!                             could not find valid snow data)
!                             are set to a default value of zero.
!                             poleward, undefined points are set
!                             according to logic in module snow2mdl
!   min_snow_depth          - minimum snow depth in meters at model
!                             points with coverage exceeding threshold.
!   output_grib2            - when true, output model snow analysis
!                             is grib 2.  when false, grib 1.
!   snow_cvr_threshold      - if percent coverage according to nesdis/ims
!                             or autosnow exceeds this value, then
!                             non-zero snow depth is assigned. below
!                             this threshold, depth is set to zero.
!
!$$$ 

 implicit none

 private

 character*200, public         :: afwa_snow_global_file   
 character*150, public         :: afwa_snow_nh_file   
 character*150, public         :: afwa_snow_sh_file   
 character*150, public         :: afwa_lsmask_nh_file  
 character*150, public         :: afwa_lsmask_sh_file  
 character*150, public         :: autosnow_file
 character*150, public         :: climo_qc_file
 character*150, public         :: gfs_lpl_file
 character*150, public         :: model_lat_file    
 character*150, public         :: model_lon_file      
 character*150, public         :: model_lsmask_file  
 character*150, public         :: model_snow_file    
 character*150, public         :: nesdis_lsmask_file   
 character*150, public         :: nesdis_snow_file   

 integer, public               :: grib_century      
 integer, public               :: grib_day           
 integer, public               :: grib_hour          
 integer, public               :: grib_month
 integer, public               :: grib_year

 logical, public               :: output_grib2

 real, public                  :: lat_threshold         
 real, public                  :: min_snow_depth        
 real, public                  :: snow_cvr_threshold    

 public                        :: read_config_nml

 contains

 subroutine read_config_nml
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_config_nml
!   prgmmr: gayno          org: w/np2     date: 2005-dec-16
!
! abstract: this subroutine reads the program's configuration namelist
!   that contains the the paths/filenames for input and output, and
!   other program control flags.
!
! program history log:
! 2005-dec-16  gayno    - initial version
! 2008-feb-01  gayno    - added read of autosnow path/file
! 2014-sep-30  gayno    - added read of 'output_grib2' flag.
!
! usage: call read_config_nml
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!   input:
!      - program configuration namelist, fort.41
!
!   output: none
!
! condition codes:  all fatal
!   77 - bad open on configuration namelist
!   78 - bad read on configuration namelist
!
! remarks: none.
!
!$$$

 implicit none

 integer, parameter              :: iunit=41
 integer                         :: istat

 namelist /source_data/ autosnow_file,         &
                        nesdis_snow_file,      &
                        nesdis_lsmask_file,    &
                        afwa_snow_global_file, &
                        afwa_snow_nh_file,     &
                        afwa_snow_sh_file,     &
                        afwa_lsmask_nh_file,   &
                        afwa_lsmask_sh_file

 namelist /qc/          climo_qc_file

 namelist /model_specs/ model_lat_file,       &
                        model_lon_file,       &
                        model_lsmask_file,    &
                        gfs_lpl_file

 namelist /output_data/ model_snow_file,   &
                        output_grib2

 namelist /output_grib_time/ grib_year,    &
                             grib_month,   &
                             grib_day,     &
                             grib_hour

 namelist /parameters/ lat_threshold,          &
                       min_snow_depth,         &
                       snow_cvr_threshold

 print*,''
 print*,"- READ CONFIGURATION NAMELIST"

 open(iunit, iostat=istat)

 if (istat /= 0) then
   print*,''
   print*,'- FATAL ERROR: BAD OPEN ON CONFIG NAMELIST.  ISTAT IS ', istat
   close(iunit)
   call w3tage('SNOW2MDL')
   call errexit(77)
 end if

 read(iunit, nml=source_data, iostat=istat, err=900)

 read(iunit, nml=qc, iostat=istat, err=900)

 read(iunit, nml=model_specs, iostat=istat, err=900)

 read(iunit, nml=output_data, iostat=istat, err=900)

 read(iunit, nml=output_grib_time, iostat=istat, err=900)

 read(iunit, nml=parameters, iostat=istat, err=900)

 close(iunit)

!-----------------------------------------------------------------------
! the user determines the date/time stamp for the final interpolated
! model data.
!-----------------------------------------------------------------------

 grib_century = grib_year / 100

 grib_year = mod(grib_year,100)

 if (grib_year == 0) then
   grib_year = 100
 else
   grib_century = grib_century + 1
 end if

 return

 900 continue
 print*,''
 print*,'- FATAL ERROR: BAD READ ON CONFIG NAMELIST.  ISTAT IS ', istat
 close(iunit)
 call w3tage('SNOW2MDL')
 call errexit(78)

 end subroutine read_config_nml

 end module program_setup
