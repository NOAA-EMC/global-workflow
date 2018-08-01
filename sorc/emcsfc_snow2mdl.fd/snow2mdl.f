 module snow2mdl
!$$$  module documentation block
!
! module:    snow2mdl
!   prgmmr: gayno         org: w/np2     date: 2005-dec-16
!
! abstract: interpolate snow data to model grid and grib the result
!
! program history log:
!   2005-DEC-16  gayno   - initial version
!   2007-SEP-20  gayno   - tested for b-grids. added improved
!                          thinning for gfs grid.
!   2008-feb-04  gayno   - added autosnow data
!   2014-sep-26  gayno   - added option to output analysed
!                          snow in grib2.
!
! usage: use snow2mdl
!
! remarks: some variable definitions
!   snow_cvr_mdl  - snow cover on model grid in percent
!   snow_dep_mdl  - snow depth on model grid in meters 
!                              
! attributes:
!   language: fortran 90
!   machine:  ibm wcoss
!
!$$$

 use program_setup,        only   : lat_threshold,          &
                                    model_snow_file,        &
                                    min_snow_depth,         &
                                    snow_cvr_threshold,     &
                                    grib_day,               &
                                    grib_century,           &
                                    grib_hour,              &
                                    grib_month,             &
                                    grib_year,              &
                                    output_grib2

 use model_grid,           only   : resol_mdl,       &
                                    imdl,            &
                                    jmdl,            &
                                    ijmdl,           &
                                    ipts_mdl,        &
                                    jpts_mdl,        &
                                    lsmask_mdl,      &
                                    lsmask_mdl_sav,  &
                                    lats_mdl,        &
                                    lat11, latlast,  &
                                    lon11, lonlast,  &
                                    lons_mdl,        &
                                    kgds_mdl,        &
                                    grid_id_mdl,     &
                                    thinned,         &
                                    lonsperlat_mdl

 use snowdat,              only   : nesdis_res,        &                                 
                                    afwa_res,          &
                                    autosnow_res,      &
                                    inesdis,           &
                                    jnesdis,           &
                                    mesh_nesdis,       &
                                    snow_cvr_nesdis,   &
                                    sea_ice_nesdis,    &
                                    bitmap_nesdis,     &
                                    iafwa,             &
                                    jafwa,             &
                                    snow_dep_afwa_global,  &
                                    snow_dep_afwa_nh,  &
                                    snow_dep_afwa_sh,  &
                                    bitmap_afwa_global,    &
                                    bitmap_afwa_nh,    &
                                    bitmap_afwa_sh,    &
                                    iautosnow,         &
                                    jautosnow,         &
                                    snow_cvr_autosnow, &
                                    bitmap_autosnow,   &
                                    use_global_afwa,   &
                                    use_nh_afwa,       &
                                    use_sh_afwa,       &
                                    use_nesdis,        &
                                    use_autosnow,      &
                                    kgds_nesdis,       &
                                    kgds_afwa_global,      &
                                    kgds_afwa_nh,      &
                                    kgds_afwa_sh,      &
                                    kgds_autosnow,     &
                                    bad_afwa_nh, bad_afwa_sh 

 use read_write_utils,     only   : uninterpred

 private

 real, allocatable               :: snow_cvr_mdl(:,:)  ! cover in % on mdl grid                                   
 real, allocatable               :: snow_dep_mdl(:,:)  ! depth on model grid

 public                          :: interp

 contains

 subroutine interp
!$$$  subprogram documentation block
!
! subprogram:   interp
!   prgmmr: gayno          org: w/np2     date: 2005-dec-16
!
! abstract:  interpolate snow data to model grid.
!
! program history log:
! 2005-dec-16  gayno    - initial version
! 2007-sep-20  gayno    - tested for b-grids. added improved
!                         thinning for gfs grid.
! 2008-feb-04  gayno    - add use of autosnow data
! 2014-sep-29  gayno    - add option to output model snow
!                         data in grib2 format.
!
! usage: call interp
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files: none
!
! condition codes: all fatal
!   54 - selected input snow data not valid for model grid
!   55 - error in ipolates interpolating snow data 
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
! attributes:
!   language: fortran 90
!   machine:  IBM WCOSS
!
!$$$

 use gdswzd_mod

 implicit none

 integer                   :: i, j, ii, jj, ij
 integer                   :: ijmdl2, istart, iend, imid, iii
 integer, allocatable      :: idum(:,:)
 integer                   :: int_opt, ipopt(20)
 integer                   :: kgds_mdl_tmp(200)
 integer                   :: no, ibo, iret, nret

 logical*1, allocatable    :: bitmap_mdl(:)

 real                      :: gridi(1)
 real                      :: gridj(1)
 real, allocatable         :: lsmask_1d(:)
 real, allocatable         :: snow_cvr_mdl_1d(:)
 real, allocatable         :: snow_dep_mdl_tmp(:) 
 real                      :: sumc, sumd, x1, r, fraction, gridis, gridie
 real, parameter           :: undefined_value = -999.

!----------------------------------------------------------------------
! for model grids fully or partially located in the southern
! hemisphere, the user must select sh data (either autosnow or afwa).
! this restriction is relaxed for the ndas domain, which is 
! has a small part below the equator.
!----------------------------------------------------------------------

 if (minval(lats_mdl) < -10.0 .and. .not. use_global_afwa .and. .not. use_sh_afwa .and. .not. use_autosnow) then
   print*,"- FATAL ERROR: MUST SELECT EITHER AFWA OR AUTOSNOW DATA FOR MODEL GRID WITH SH POINTS."
   call w3tage('SNOW2MDL')
   call errexit(54)
 endif

!----------------------------------------------------------------------
! if model grid is totally within the southern hemisphere, set flags
! so that nh afwa and nh nesdis/ims is not processed.  these flags
! are set to false if user inadvertantly selects these data sources.
!----------------------------------------------------------------------

 if (maxval(lats_mdl) < 0.0) then 
   use_nh_afwa=.false.
   use_nesdis=.false.
 endif

!----------------------------------------------------------------------
! if model grid is partially or fully located in the northern 
! hemisphere, the user must select either nesdis/ims or nh afwa data.
!----------------------------------------------------------------------

 if (maxval(lats_mdl) > 0.0 .and. .not. use_global_afwa .and. .not. use_nh_afwa .and. .not. use_nesdis) then
   print*,"- FATAL ERROR: MUST SELECT EITHER NESDIS/IMS OR AFWA DATA FOR MODEL GRID WITH NH POINTS."
   call w3tage('SNOW2MDL')
   call errexit(54)
 endif

!----------------------------------------------------------------------
! if model grid is totally within the northern hemisphere, set flag
! so that sh data is not processed.  these flags are set to false
! if user inadvertantly selects this data.
!----------------------------------------------------------------------

 if (minval(lats_mdl) > 0.0) then ! is model grid totally within northern hemisphere?
   use_sh_afwa=.false.
   use_autosnow=.false.
 endif

!----------------------------------------------------------------------
! if selected, interpolate nesdis/ims data to model grid.
!----------------------------------------------------------------------

 NESDIS_IMS : if (use_nesdis) then

   ipopt = 0
   if (nesdis_res < (0.5*resol_mdl)) then
     print*,"- INTERPOLATE NH NESDIS/IMS DATA TO MODEL GRID USING BUDGET METHOD."
     ipopt(1)=2  ! break model grid cell into 25 points.
     ipopt(2:4)=1  ! 25 points are weighted equally.
     ipopt(5)=10  ! 10% coverage of valid data in box
     ipopt(20) = nint(100.0 / nesdis_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = kgds_mdl_tmp(1) - 255 ! subset of grid
     int_opt = 3
     no = ijmdl
   else
     print*,"- INTERPOLATE NH NESDIS/IMS DATA TO MODEL GRID USING NEIGHBOR METHOD."
     ipopt(1) = nint(100.0 / nesdis_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
     int_opt = 2
     no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
   end if

   allocate (snow_cvr_mdl_1d(ijmdl))
   snow_cvr_mdl_1d = 0.0

   allocate (bitmap_mdl(ijmdl))
   bitmap_mdl=.false.  ! if interpolation routine can't find data
                       ! at a point, this flag is false.

   call ipolates(int_opt, ipopt, kgds_nesdis, kgds_mdl_tmp,   &
                (inesdis*jnesdis), ijmdl,               &
                 1, 1, bitmap_nesdis, snow_cvr_nesdis,  &
                 no, lats_mdl, lons_mdl, ibo, bitmap_mdl,     &
                 snow_cvr_mdl_1d, iret)

   deallocate (bitmap_nesdis, snow_cvr_nesdis)

   if (iret /= 0) then
     print*,"- FATAL ERROR: IN INTERPOLATION ROUTINE. IRET IS: ", iret 
     call w3tage('SNOW2MDL')
     call errexit(55)
   endif

!-----------------------------------------------------------------------
! if the interpolation routines did not find valid nesdis/ims data
! in the vicinity of the model point, need to set a default value
! of snow cover.  south of user-defined latitude threshold, set
! to zero.  otherwise, see if the nearest neighbor nesdis/ims point is
! sea ice.  if so, assume model point is snow covered.
!-----------------------------------------------------------------------

   do ij = 1, ijmdl
     if (lats_mdl(ij) < 0.0) cycle  ! only consider nh model points
     if (.not. bitmap_mdl(ij)) then 
       if (lats_mdl(ij) <= lat_threshold) then
         snow_cvr_mdl_1d(ij) = 0.0
       else 
         call gdswzd(kgds_nesdis,-1,1,undefined_value,gridi,gridj, &
                     lons_mdl(ij),lats_mdl(ij),nret) 
         if (nret /= 1) then
           print*,"- WARNING: MODEL POINT OUTSIDE NESDIS/IMS GRID: ", ipts_mdl(ij), jpts_mdl(ij)
           snow_cvr_mdl_1d(ij) = 0.0
         else
           ii = nint(gridi(1))
           jj = nint(gridj(1))
           if (sea_ice_nesdis(ii,jj) == 1) then
             snow_cvr_mdl_1d(ij) = 100.0
           else
             snow_cvr_mdl_1d(ij) = 0.0
           end if
         end if
       end if
     end if
   enddo

   deallocate (sea_ice_nesdis)
   deallocate (bitmap_mdl)

 endif NESDIS_IMS

!----------------------------------------------------------------------
! now interpolate nh afwa snow depth data.
!----------------------------------------------------------------------

 GLOBAL_AFWA : if (use_global_afwa) then

!----------------------------------------------------------------------
! determine interpolation method based on the resolution of 
! afwa data and the model grid.  
!----------------------------------------------------------------------

   ipopt = 0
   if (afwa_res < (0.5*resol_mdl)) then
     print*,"- INTERPOLATE GLOBAL AFWA DATA TO MODEL GRID USING BUDGET METHOD."
     ipopt(1)=-1  ! break model grid cell into 25 points.
     ipopt(2)=-1  ! 25 points are weighted equally.
     ipopt(20) = nint(100.0 / afwa_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = kgds_mdl_tmp(1) - 255 ! subset of grid
     no = ijmdl
     int_opt = 3
   else
     print*,"- INTERPOLATE GLOBAL AFWA DATA TO MODEL GRID USING NEIGHBOR METHOD."
     ipopt(1) = nint(100.0 / afwa_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
     int_opt = 2
     no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
   end if

   allocate (snow_dep_mdl_tmp(ijmdl))
   snow_dep_mdl_tmp = 0.0

   allocate (bitmap_mdl(ijmdl))
   bitmap_mdl = .false.

   call ipolates(int_opt, ipopt, kgds_afwa_global, kgds_mdl_tmp,    &
                (iafwa*jafwa), ijmdl,  &
                 1, 1, bitmap_afwa_global, snow_dep_afwa_global, &
                 no, lats_mdl, lons_mdl, ibo, bitmap_mdl,     &
                 snow_dep_mdl_tmp, iret)

   deallocate(bitmap_afwa_global, snow_dep_afwa_global)

   if (iret /= 0) then
     print*,"- FATAL ERROR IN INTERPOLATION ROUTINE. IRET IS: ", iret 
     call w3tage('SNOW2MDL')
     call errexit(55)
   endif

!----------------------------------------------------------------------
! if interpolation did not find afwa data near the model point, then
! use a nominal value based on latitude threshold.  this value
! may be overwritten below depending on nesdis/ims cover data (if
! user selects nesdis/ims data).
!----------------------------------------------------------------------

   do ij = 1, ijmdl
     if (.not. bitmap_mdl(ij)) then
       if (abs(lats_mdl(ij)) >= lat_threshold) then
         snow_dep_mdl_tmp(ij) = min_snow_depth
       else
         snow_dep_mdl_tmp(ij) = 0.0
       endif
     endif
   enddo

   deallocate(bitmap_mdl)

 endif GLOBAL_AFWA

!----------------------------------------------------------------------
! now interpolate nh afwa snow depth data.
!----------------------------------------------------------------------

 NH_AFWA : if (use_nh_afwa) then

!----------------------------------------------------------------------
! determine interpolation method based on the resolution of 
! afwa data and the model grid.  
!----------------------------------------------------------------------

   ipopt = 0
   if (afwa_res < (0.5*resol_mdl)) then
     print*,"- INTERPOLATE NH AFWA DATA TO MODEL GRID USING BUDGET METHOD."
     ipopt(1)=-1  ! break model grid cell into 25 points.
     ipopt(2)=-1  ! 25 points are weighted equally.
     ipopt(20) = nint(100.0 / afwa_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = kgds_mdl_tmp(1) - 255 ! subset of grid
     no = ijmdl
     int_opt = 3
   else
     print*,"- INTERPOLATE NH AFWA DATA TO MODEL GRID USING NEIGHBOR METHOD."
     ipopt(1) = nint(100.0 / afwa_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
     int_opt = 2
     no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
   end if

   allocate (snow_dep_mdl_tmp(ijmdl))
   snow_dep_mdl_tmp = 0.0

   allocate (bitmap_mdl(ijmdl))
   bitmap_mdl = .false.

   call ipolates(int_opt, ipopt, kgds_afwa_nh, kgds_mdl_tmp,    &
                (iafwa*jafwa), ijmdl,  &
                 1, 1, bitmap_afwa_nh, snow_dep_afwa_nh, &
                 no, lats_mdl, lons_mdl, ibo, bitmap_mdl,     &
                 snow_dep_mdl_tmp, iret)

   deallocate(bitmap_afwa_nh, snow_dep_afwa_nh)

   if (iret /= 0) then
     print*,"- FATAL ERROR IN INTERPOLATION ROUTINE. IRET IS: ", iret 
     call w3tage('SNOW2MDL')
     call errexit(55)
   endif

!----------------------------------------------------------------------
! if interpolation did not find afwa data near the model point, then
! use a nominal value based on latitude threshold.  this value
! may be overwritten below depending on nesdis/ims cover data (if
! user selects nesdis/ims data).
!----------------------------------------------------------------------

   do ij = 1, ijmdl
     if (lats_mdl(ij) >= 0.) then  ! only consider model pts in n hemi.
       if (.not. bitmap_mdl(ij)) then
         if (abs(lats_mdl(ij)) >= lat_threshold) then
           snow_dep_mdl_tmp(ij) = min_snow_depth
         else
           snow_dep_mdl_tmp(ij) = 0.0
         endif
       endif
     endif
   enddo

   deallocate(bitmap_mdl)

 endif NH_AFWA

!----------------------------------------------------------------------
! if nh data selected, use it to determine the cover and depth
! on the model grid.
!----------------------------------------------------------------------

 allocate (snow_dep_mdl(imdl,jmdl))
 allocate (snow_cvr_mdl(imdl,jmdl))
 snow_cvr_mdl = 0.0
 snow_dep_mdl = 0.0

 if (use_global_afwa .and. use_nesdis) then  ! set depth/cover on nesdis ims/afwa blend
   print*,"- BLEND NESDIS/IMS AND AFWA DATA IN NH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) >= 0.0) then
       if (snow_cvr_mdl_1d(ij) >= snow_cvr_threshold) then
         snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) =   &
                      max(snow_dep_mdl_tmp(ij), min_snow_depth)
       endif   
       snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_cvr_mdl_1d(ij)
     endif
   enddo
   deallocate (snow_cvr_mdl_1d)
 elseif (use_nh_afwa .and. use_nesdis) then  ! set depth/cover on nesdis ims/afwa blend
   print*,"- BLEND NESDIS/IMS AND AFWA DATA IN NH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) >= 0.0) then
       if (snow_cvr_mdl_1d(ij) >= snow_cvr_threshold) then
         snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) =   &
                      max(snow_dep_mdl_tmp(ij), min_snow_depth)
       endif   
       snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_cvr_mdl_1d(ij)
     endif
   enddo
   deallocate (snow_cvr_mdl_1d)
   deallocate (snow_dep_mdl_tmp)
 elseif (use_global_afwa) then  ! set depth/cover on afwa only
   print*,"- SET DEPTH/COVER FROM AFWA DATA IN NH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) >= 0.0) then
       if (snow_dep_mdl_tmp(ij) > 0.0) then
         snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = 100.0
         snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_dep_mdl_tmp(ij)
       endif
     endif
   enddo
 elseif (use_nh_afwa) then  ! set depth/cover on afwa only
   print*,"- SET DEPTH/COVER FROM AFWA DATA IN NH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) >= 0.0) then
       if (snow_dep_mdl_tmp(ij) > 0.0) then
         snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = 100.0
         snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_dep_mdl_tmp(ij)
       endif
     endif
   enddo
   deallocate (snow_dep_mdl_tmp)
 elseif (use_nesdis) then  ! set depth/cover on nesdis/ims only
   print*,"- SET DEPTH/COVER FROM NESDIS/IMS DATA IN NH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) >= 0.0) then
       if (snow_cvr_mdl_1d(ij) >= snow_cvr_threshold) then
         snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) = min_snow_depth
       endif   
       snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_cvr_mdl_1d(ij)
     endif
   enddo
   deallocate (snow_cvr_mdl_1d)
 end if

!----------------------------------------------------------------------
! if selected, interpolate autosnow data to model grid.
!----------------------------------------------------------------------

 AUTOSNOW : if (use_autosnow) then

   ipopt = 0
   if (autosnow_res < (0.5*resol_mdl)) then
     print*,"- INTERPOLATE AUTOSNOW DATA TO MODEL GRID USING BUDGET METHOD."
     ipopt(1)=2    ! break model grid cell into 25 points.
     ipopt(2:4)=1  ! 25 points are weighted equally.
     ipopt(5)=10   ! 10% coverage of valid data in box
     ipopt(20) = nint(100.0 / autosnow_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = kgds_mdl_tmp(1) - 255 ! subset of grid
     int_opt = 3
     no = ijmdl
   else
     print*,"- INTERPOLATE AUTOSNOW DATA TO MODEL GRID USING NEIGHBOR METHOD."
     ipopt(1) = nint(100.0 / autosnow_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
     int_opt = 2
     no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
   end if

   allocate (snow_cvr_mdl_1d(ijmdl))
   snow_cvr_mdl_1d = 0.0

   allocate (bitmap_mdl(ijmdl))
   bitmap_mdl=.false.  ! if interpolation routine can't find data
                       ! at a point, this flag is false.

   call ipolates(int_opt, ipopt, kgds_autosnow, kgds_mdl_tmp,   &
                (iautosnow*jautosnow), ijmdl,               &
                 1, 1, bitmap_autosnow, snow_cvr_autosnow,  &
                 no, lats_mdl, lons_mdl, ibo, bitmap_mdl,     &
                 snow_cvr_mdl_1d, iret)

   deallocate (snow_cvr_autosnow, bitmap_autosnow)

   if (iret /= 0) then
     print*,"- FATAL ERROR IN INTERPOLATION ROUTINE. IRET IS: ", iret 
     call w3tage('SNOW2MDL')
     call errexit(55)
   endif

!----------------------------------------------------------------------
! if interpolation fails to find autosnow data, set the cover
! at the model point to a nomimal value.
!----------------------------------------------------------------------

   do ij = 1, ijmdl
     if (lats_mdl(ij) < 0.0) then
       if (.not. bitmap_mdl(ij)) then
         if (abs(lats_mdl(ij)) <= lat_threshold) then
           snow_cvr_mdl_1d(ij) = 0.0
         else 
           snow_cvr_mdl_1d(ij) = 100.0
         end if
       end if
     end if
   enddo

   deallocate (bitmap_mdl)

 endif AUTOSNOW

!----------------------------------------------------------------------
! now interpolate sh afwa snow depth data.
!----------------------------------------------------------------------

 SH_AFWA : if (use_sh_afwa) then

!----------------------------------------------------------------------
! determine interpolation method based on the resolution of 
! afwa data and the model grid.  
!----------------------------------------------------------------------

   ipopt = 0
   if (afwa_res < (0.5*resol_mdl)) then
     print*,"- INTERPOLATE SH AFWA DATA TO MODEL GRID USING BUDGET METHOD."
     ipopt(1)=-1  ! break model grid cell into 25 points.
     ipopt(2)=-1  ! 25 points are weighted equally.
     ipopt(20) = nint(100.0 / afwa_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = kgds_mdl_tmp(1) - 255 ! subset of grid
     no = ijmdl
     int_opt = 3
   else
     print*,"- INTERPOLATE SH AFWA DATA TO MODEL GRID USING NEIGHBOR METHOD."
     ipopt(1) = nint(100.0 / afwa_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
     int_opt = 2
     no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
   end if

   allocate (snow_dep_mdl_tmp(ijmdl))
   snow_dep_mdl_tmp = 0.0

   allocate (bitmap_mdl(ijmdl))
   bitmap_mdl = .false.

   call ipolates(int_opt, ipopt, kgds_afwa_sh, kgds_mdl_tmp,    &
                (iafwa*jafwa), ijmdl,  &
                 1, 1, bitmap_afwa_sh, snow_dep_afwa_sh, &
                 no, lats_mdl, lons_mdl, ibo, bitmap_mdl,     &
                 snow_dep_mdl_tmp, iret)

   if (iret /= 0) then
     print*,"- FATAL ERROR IN INTERPOLATION ROUTINE. IRET IS: ", iret 
     call w3tage('SNOW2MDL')
     call errexit(55)
   endif

   deallocate (bitmap_afwa_sh, snow_dep_afwa_sh)

!----------------------------------------------------------------------
! if interpolation does not find afwa data, set model point to
! a nominal value.
!----------------------------------------------------------------------

   do ij = 1, ijmdl
     if (lats_mdl(ij) < 0.) then
       if (.not. bitmap_mdl(ij)) then
         if (abs(lats_mdl(ij)) >= lat_threshold) then
           snow_dep_mdl_tmp(ij) = min_snow_depth
         else
           snow_dep_mdl_tmp(ij) = 0.0
         endif
       endif
     endif
   enddo

  deallocate(bitmap_mdl)

 endif SH_AFWA

!----------------------------------------------------------------------
! if sh data selected, use it to determine the cover and depth
! on the model grid.
!----------------------------------------------------------------------

 if ((use_sh_afwa .or. use_global_afwa) .and. use_autosnow) then  ! set depth/cover on autosnow/afwa blend
   print*,"- BLEND AUTOSNOW AND AFWA DATA IN SH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) < 0.0) then
       if (snow_cvr_mdl_1d(ij) >= snow_cvr_threshold) then
         snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) =   &
                      max(snow_dep_mdl_tmp(ij), min_snow_depth)
       endif   
       snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_cvr_mdl_1d(ij)
     endif
   enddo
   deallocate (snow_cvr_mdl_1d)
   deallocate (snow_dep_mdl_tmp)
 elseif (use_sh_afwa .or. use_global_afwa) then  ! set depth/cover on afwa only
   print*,"- SET DEPTH/COVER FROM AFWA DATA IN SH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) < 0.0) then
       if (snow_dep_mdl_tmp(ij) > 0.0) then
         snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = 100.0
         snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_dep_mdl_tmp(ij)
       endif
     endif
   enddo
   deallocate (snow_dep_mdl_tmp)
 elseif (use_autosnow) then  ! set depth/cover on autosnow only
   print*,"- SET DEPTH/COVER FROM AUTOSNOW IN SH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) < 0.0) then
       if (snow_cvr_mdl_1d(ij) >= snow_cvr_threshold) then
         snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) = min_snow_depth
       endif   
       snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_cvr_mdl_1d(ij)
     endif
   enddo
   deallocate (snow_cvr_mdl_1d)
 end if

!----------------------------------------------------------------------
! if a global model grid, and if running on thinned grid, then
! take a linear weighting of full points located within the thin points.
! "4" is grid indicator for a gaussian grid.
!----------------------------------------------------------------------

 if (kgds_mdl(1) == 4 .and. thinned) then

   ijmdl2 = sum(lonsperlat_mdl) * 2
   allocate (snow_cvr_mdl_1d(ijmdl2))
   allocate (lsmask_1d(ijmdl2))
   allocate (snow_dep_mdl_tmp(ijmdl2))

   lsmask_1d = 0.0
   snow_cvr_mdl_1d = 0.0
   snow_dep_mdl_tmp = 0.0

   ij = 0
   do j = 1, jmdl
     jj = j
     if (jj > jmdl/2) jj = jmdl - j + 1
     r = float(imdl) / float(lonsperlat_mdl(jj))
     do i = 1, lonsperlat_mdl(jj)
       ij = ij + 1
       x1 = (i-1)*r
       imid = nint(x1+1.0)
       lsmask_1d(ij) = lsmask_mdl_sav(imid,j)
       if (lsmask_mdl_sav(imid,j) == 0.0) cycle
       gridis=x1+1.0-r/2.
       istart=nint(gridis)
       gridie=x1+1.0+r/2.
       iend=nint(gridie)
       sumc = 0.0   ! cover
       sumd = 0.0   ! depth
       do ii = istart, iend
         if (ii == istart) then
           fraction = 0.5 - (gridis - float(istart))
         elseif (ii == iend) then
           fraction = 0.5 + (gridie - float(iend))
         else
           fraction = 1.0
         endif
         if (fraction < 0.0001) cycle
         iii = ii
         if (iii < 1) iii = imdl + iii
         sumc = sumc + fraction * snow_cvr_mdl(iii,j)
         sumd = sumd + fraction * snow_dep_mdl(iii,j)
       enddo
       snow_cvr_mdl_1d(ij) = sumc / r
       snow_dep_mdl_tmp(ij) = 0.0
       if (snow_cvr_mdl_1d(ij) > snow_cvr_threshold) then
         snow_dep_mdl_tmp(ij) = max(sumd / r,min_snow_depth)
       end if
    enddo
   enddo

   deallocate (lsmask_mdl_sav)

!----------------------------------------------------------------------
! now place thinned points into 2-d array for output.
!----------------------------------------------------------------------

   allocate (idum(imdl,jmdl))
   idum = 0
   call uninterpred(1, idum, lsmask_1d, lsmask_mdl, imdl, jmdl, ijmdl2, lonsperlat_mdl)
   call uninterpred(1, idum, snow_cvr_mdl_1d, snow_cvr_mdl, imdl, jmdl, ijmdl2, lonsperlat_mdl)
   deallocate(snow_cvr_mdl_1d)
   call uninterpred(1, idum, snow_dep_mdl_tmp, snow_dep_mdl, imdl, jmdl, ijmdl2, lonsperlat_mdl)
   deallocate(snow_dep_mdl_tmp)
   deallocate(idum)

 end if

!----------------------------------------------------------------------
! grib the interpolated data.
!----------------------------------------------------------------------

 if (output_grib2) then
   print*,"- OUTPUT SNOW ANALYSIS DATA IN GRIB2 FORMAT"
   call write_grib2
 else
   print*,"- OUTPUT SNOW ANALYSIS DATA IN GRIB1 FORMAT"
   call write_grib1
 endif

 deallocate (snow_cvr_mdl)
 deallocate (snow_dep_mdl)

 return

 end subroutine interp

 subroutine write_grib2
!$$$  subprogram documentation block
!
! subprogram:   write_grib2
!   prgmmr: gayno          org: w/np2     date: 2014-sep-26
!
! abstract:  output snow cover and depth on the model grid 
!            in grib 2 format.
!
! program history log:
! 2014-sep-26  gayno    - initial version
!
! usage: call write_grib2
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!   input: none
!
!   output: 
!     - snow on the model grid, grib 2, unit=lugb
!
! condition codes: all fatal
!    48 - error writing model snow flie
!    49 - error opening model snow flie
!
! remarks: none.
!
!$$$

 use grib_mod

 implicit none

 character(len=1), allocatable :: cgrib(:)

 integer, parameter            :: numcoord = 0

 integer                       :: coordlist(numcoord)
 integer                       :: lugb, lcgrib, iret
 integer                       :: igds(5)
 integer                       :: listsec0(2)
 integer                       :: listsec1(13)
 integer                       :: ideflist, idefnum, ipdsnum, idrsnum
 integer                       :: igdstmplen, ipdstmplen, idrstmplen
 integer                       :: ipdstmpl(15)
 integer, allocatable          :: igdstmpl(:), idrstmpl(:)
 integer                       :: ngrdpts, ibmap, lengrib

 logical*1, allocatable        :: bmap(:), bmap2d(:,:)

 real, allocatable             :: fld(:)

!----------------------------------------------------------------------
! Setup variables and arrays required by grib2 library.
!----------------------------------------------------------------------

 call grib2_check(kgds_mdl, igdstmplen)

 allocate(igdstmpl(igdstmplen))

 call init_grib2(grib_century,grib_year, grib_month, grib_day, grib_hour, &
                 kgds_mdl, lat11, latlast, lon11, lonlast, &
                 listsec0, listsec1, igds, ipdstmpl, ipdsnum, igdstmpl,  &
                 igdstmplen, idefnum, ideflist, ngrdpts)

 lcgrib = imdl*jmdl*4
 allocate(cgrib(lcgrib))   ! this variable holds the grib2 message

 iret=0

!----------------------------------------------------------------------
! Create sections 0 and 1.  There is no section 2, local use section.
!----------------------------------------------------------------------

 print*,"- CREATE SECTIONS 0 AND 1"
 call gribcreate(cgrib,lcgrib,listsec0,listsec1,iret)
 if (iret /= 0) goto 900

!----------------------------------------------------------------------
! Create section 3, the grid description section.
!----------------------------------------------------------------------

 print*,"- CREATE SECTION 3"
 call addgrid(cgrib,lcgrib,igds,igdstmpl,igdstmplen,  &
              ideflist,idefnum,iret)
 if (iret /= 0) goto 900

!----------------------------------------------------------------------
! Setup arrays for section 5, the data representation section.
!----------------------------------------------------------------------

 idrsnum = 0       ! section 5, use simple packing
 idrstmplen = 5
 allocate (idrstmpl(idrstmplen))
 idrstmpl    = 0
 idrstmpl(3) = 0   ! decimal scaling factor

 allocate(fld(ngrdpts))
 fld = reshape(snow_cvr_mdl, (/imdl*jmdl/) )

 ibmap = 0 ! bitmap applies
 allocate(bmap2d(imdl,jmdl))
 bmap2d=.true.
 where (lsmask_mdl < 0.5) bmap2d=.false.
 allocate(bmap(ngrdpts))
 bmap = reshape(bmap2d, (/imdl*jmdl/) )
 deallocate (bmap2d)

 coordlist=0   ! not used

!----------------------------------------------------------------------
! Create section 4 (product definition section) and 5 (data 
! representation section) for cover.
!----------------------------------------------------------------------

 ipdstmpl(1) = 1     ! section 4, oct 10; parameter category, moisture
 ipdstmpl(2) = 42    ! section 4, oct 11; parameter, snow cover

 print*,"- CREATE SECTIONS 4 AND 5 FOR SNOW COVER"
 call addfield(cgrib,lcgrib,ipdsnum,ipdstmpl,ipdstmplen,  &
               coordlist,numcoord,idrsnum,idrstmpl,   &
               idrstmplen,fld,ngrdpts,ibmap,bmap,iret)
 if (iret /= 0) goto 900

!----------------------------------------------------------------------
! for regional model, if afwa data not used, don't output a depth
! record.  this tells the sfcupdate code to update the first
! guess snow with the new snow cover analysis.
!----------------------------------------------------------------------

 if (kgds_mdl(1) /= 4) then
   if (.not. use_global_afwa .and. .not. use_nh_afwa .and. .not. use_sh_afwa) goto 88 
 endif

!----------------------------------------------------------------------
! Create section 4 (product definition section) and 5 (data
! representation section) for depth.
!----------------------------------------------------------------------

 fld= reshape(snow_dep_mdl, (/imdl*jmdl/) )

 ipdstmpl(1) = 1     ! section 4, oct 10; parameter category, moisture
 ipdstmpl(2) = 11    ! section 4, oct 11; parameter, snow depth

 idrstmpl    = 0     ! section 5
 idrstmpl(3) = 4     ! section 5, decimal scaling factor

 print*,"- CREATE SECTIONS 4 AND 5 FOR SNOW DEPTH"
 call addfield(cgrib,lcgrib,ipdsnum,ipdstmpl,ipdstmplen,  &
               coordlist,numcoord,idrsnum,idrstmpl,   &
               idrstmplen,fld,ngrdpts,ibmap,bmap,iret)
 if (iret /= 0) goto 900

 88 continue

!----------------------------------------------------------------------
! Create section 8 - end section.
!----------------------------------------------------------------------

 call gribend(cgrib,lcgrib,lengrib,iret)
 if (iret /= 0) goto 900
 
!----------------------------------------------------------------------
! Now output grib message to file.
!----------------------------------------------------------------------

 lugb=53
 print*,"- OPEN OUTPUT GRIB FILE ", trim(model_snow_file)
 call baopenw(lugb, model_snow_file, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR: BAD OPEN OF OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('SNOW2MDL')
   call errexit(49)
 end if

 print*,'- WRITE OUTPUT GRIB FILE.'
 call wryte(lugb, lengrib, cgrib)

 call baclose (lugb, iret)

 deallocate(fld, bmap, idrstmpl, igdstmpl, cgrib)

 return

 900 continue
 print*,'- FATAL ERROR CREATING GRIB2 MESSAGE. IRET IS ', iret
 call w3tage('SNOW2MDL')
 call errexit(48)

 end subroutine write_grib2

 subroutine write_grib1
!$$$  subprogram documentation block
!
! subprogram:   write_grib1
!   prgmmr: gayno          org: w/np2     date: 2005-dec-16
!
! abstract:  output snow cover and depth on the model grid
!            in grib1 format.
!
! program history log:
! 2005-dec-16  gayno    - initial version
! 2014-sep-26  gayno    - rename as write_grib1 (was gribit)
!
! usage: call write_grib1
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!   input: none
!
!   output:
!     - snow on model grid, grib 1, unit=lugb
!
! condition codes:
!    57 - error writing model snow depth record
!    58 - error writing model snow cover record
!    59 - error opening model snow file
!
! remarks: none.
!      
!$$$

 implicit none

 integer                    :: iret 
 integer, parameter         :: lugb = 64    ! unit number of output grib file
 integer                    :: kpds(200)

 logical*1                  :: lbms(imdl,jmdl)

!----------------------------------------------------------------------
! set up pds section.  don't need to set the gds section.
! since the model grid is not changing, use the kgds array 
! already determined in module model_grid.   
!----------------------------------------------------------------------

 kpds = 0

 kpds(1)  = 7           ! center id
 kpds(2)  = 25          ! process id number. this determined from the 
                        ! input data as we are simply interpolating
                        ! that data to a different grid.  should
                        ! i request a process id for my codes? 
 kpds(3)  = grid_id_mdl ! grid specified in gds
 kpds(4)  = 192         ! include gds and a bit map section  
 kpds(5)  = 238         ! parameter number for snow cover
 kpds(6)  = 1           ! level - ground or water surface
 kpds(7)  = 0           ! height pressure of level
 kpds(8)  = grib_year   ! year of century     the time info is determined
 kpds(9)  = grib_month  ! month               by operational requirements
 kpds(10) = grib_day    ! day
 kpds(11) = grib_hour   ! hour
 kpds(12) = 0           ! minute
 kpds(13) = 1           ! fcst time unit - hour
 kpds(14) = 0           ! period of time, p1.  set to '0' for analysis
 kpds(15) = 0           ! number of time units, p2. 
 kpds(16) = 1           ! initialized analysis product
 kpds(17) = 0           ! number in average
 kpds(18) = 1           ! grib edition 1
 kpds(19) = 3           ! parameter table version number
 kpds(20) = 0           ! number missing from avg/accum
 kpds(21) = grib_century ! century - set as in the input file
 kpds(22) = 0           ! decimal scale factor
 kpds(23) = 4           ! subcenter - emc  
 kpds(24) = 0           ! reserved
 kpds(25) = 0           ! reserved

 lbms = .false.         ! set bitmap section
  
 where(lsmask_mdl > 0.0)  lbms = .true. 
 
 print*,"- OPEN OUTPUT GRIB FILE ", trim(model_snow_file)
 call baopenw(lugb, model_snow_file, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR OPENING OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('SNOW2MDL')
   call errexit(59)
 end if

 print*,"- WRITE OUTPUT GRIB FILE ", trim(model_snow_file)
 call putgb (lugb, (imdl*jmdl), kpds, kgds_mdl, lbms,  &
             snow_cvr_mdl, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR WRITING OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('SNOW2MDL')
   call errexit(58)
 end if

! for regional model, if afwa data not used, don't output a depth
! record.  this tells the sfcupdate code to update the first
! guess snow with the new snow cover analysis.

 if (kgds_mdl(1) /= 4) then
   if (.not. use_global_afwa .and. .not. use_nh_afwa .and. .not. use_sh_afwa) goto 88 
 endif

 kpds(5)  = 66  ! parameter number for snow depth
 kpds(22) = 4   ! scaling factor.  to nearest mm

 call putgb (lugb, (imdl*jmdl), kpds, kgds_mdl, lbms,  &
             snow_dep_mdl, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR WRITING OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('SNOW2MDL')
   call errexit(57)
 end if
 
 88 call baclose(lugb, iret)

 return

 end subroutine write_grib1

 end module snow2mdl
