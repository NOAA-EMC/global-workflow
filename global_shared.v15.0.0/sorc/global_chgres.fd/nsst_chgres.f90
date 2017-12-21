 subroutine nsst_chgres(im_input, jm_input,  &
                        mask_output, tskin_output, imo, ij_output, kgds_input, &
                        data_input, mask_input, data_output, num_nsst_fields, &
                        kgds_output, rlat_output, rlon_output)
!----------------------------------------------------------------
! subroutine: nsst_chgres
!
!   prgmmr: gayno            org: emc         date: 2011-Aug
!
!   abstract: 
!   ========
!   interpolate nsst fields from one grid to another.
!   nearest neighbor interpolation is used because some nsst
!   fields are not continuous. nsst fields are only required 
!   at open water points.  for consistency, the nsst land-sea
!   mask is set to the land-sea mask from the surface restart
!   file.  therefore, when converting an nsst restart file,
!   you must also convert a surface restart file.
!
!   program history:
!   ===============
!   2011-aug-05       initial version   gayno
!   2017-dec-19       add bilinear option. ensure mask
!                     consistency between skin t and tref.
!----------------------------------------------------------------

 implicit none

 integer, intent(in)          :: imo
                                 ! number of grid points on the
                                 ! cubed-sphere side
 integer, intent(in)          :: ij_output
                                 ! number of grid points - output grid.
 integer, intent(in)          :: kgds_input(200), kgds_output(200)
                                 ! grib 1 grid desc section - input/output grids
 integer, intent(in)          :: im_input, jm_input
                                 ! number of grid points in i/j direction - 
                                 ! input grid.
 integer, intent(in)          :: num_nsst_fields
                                 ! number of nsst fields

 real, intent(in)             :: mask_output(ij_output)
                                 ! land mask - output grid 
 real, intent(in)             :: tskin_output(ij_output)
                                 ! skin temperature - output grid 
 real, intent(in)             :: rlat_output(ij_output)
                                 ! latitudes on output grid
 real, intent(in)             :: rlon_output(ij_output)
                                 ! longitudes on output grid
 real, intent(in)             :: data_input(im_input,jm_input,num_nsst_fields)
                                 ! nsst data on input grid
 real, intent(in)             :: mask_input(im_input,jm_input)
                                 ! mask on input grid

 integer                      :: count_water
                                 ! number of output grid points that are open water.
 integer                      :: ij_input
                                 ! number of grid points, input grid
 integer                      :: ip, iret, ipopt(20)
 integer                      :: ibi(num_nsst_fields), ibo(num_nsst_fields)
 integer                      :: i, j, ij, k  
 integer, allocatable         :: ijsav_water(:)
 integer                      :: kgds(200)

 logical*1, allocatable       :: bitmap_input(:,:,:) 
 logical*1, allocatable       :: bitmap_water(:,:)

 real                         :: data_output(ij_output,num_nsst_fields)
 real, allocatable            :: data_water(:,:)
 real, allocatable            :: rlat_water(:), rlon_water(:)
 real                         :: mdl_res_input, mdl_res_output

!----------------------------------------------------------------
! Bitmap flag for input data.  All input fields will be
! interpolated using the same bitmap.
!----------------------------------------------------------------

 ibi=1 
      
!----------------------------------------------------------------
! mask is: 0-open water, 1-land, 2-sea ice.  nsst model
! only operates at open water points.  Mask out these points.
! The one exception is TREF.  Here include the TREF
! values at ice points.  This is done for consistency with
! the how skin temperature is interpolated in surface_chgres.f90.
! At non-land points, skin temperture is a blend of the ice 
! skin temp and the SST.  So non-land skin temp is interpolated
! to non-land points.  If TREF values at ice points are
! ignored, very large differences between skin/sst and TREF
! can happen near ice edges.
!----------------------------------------------------------------

 allocate(bitmap_input(im_input,jm_input,num_nsst_fields))
 bitmap_input=.false.
 do j=1,jm_input
 do i=1,im_input
   if (mask_input(i,j) < 0.5) then
     bitmap_input(i,j,1:16)=.true.
     bitmap_input(i,j,18:num_nsst_fields)=.true.
   endif
 enddo
 enddo

 bitmap_input(:,:,17) = .false.   ! TREF
 do j=1,jm_input
 do i=1,im_input
   if (mask_input(i,j) < 0.5 .or. mask_input(i,j) > 1.5) then
     bitmap_input(i,j,17)=.true.
   endif
 enddo
 enddo

!----------------------------------------------------------------
! Only interpolate to output points that are open water.
! Mask values are: 0-open water, 1-land, 2-sea ice.
!----------------------------------------------------------------

 count_water=0
 do ij=1, ij_output
   if (mask_output(ij) < 0.5) then
     count_water=count_water+1
   endif
 enddo

 allocate(rlat_water(count_water))
 allocate(rlon_water(count_water))
 allocate(ijsav_water(count_water))
 count_water=0
 do ij=1, ij_output
   if (mask_output(ij) < 0.5) then
     count_water=count_water+1
     rlat_water(count_water)=rlat_output(ij)
     rlon_water(count_water)=rlon_output(ij)
     ijsav_water(count_water)=ij
   endif
 enddo

 allocate(data_water(count_water,num_nsst_fields))
 data_water=0.0
 allocate(bitmap_water(count_water,num_nsst_fields))
 bitmap_water=.false.

!----------------------------------------------------------------
! ipolates options.  Must ensure these are the same
! values used in surface_chgres.f90.
!----------------------------------------------------------------

 mdl_res_input = 360.0 / float(kgds_input(2))
 mdl_res_output = 360.0 / (float(imo) * 4.0)

 ipopt=0

 if (mdl_res_input <= (0.75*mdl_res_output)) then
   print*,"- INTERPOLATE NSST DATA FIELDS USING BILINEAR METHOD."
   ip = 0
   ipopt(1)=1
   ipopt(2) = nint(1.0 / mdl_res_input) + 1   ! search box width of 1 deg.
 else
   print*,"- INTERPOLATE NSST DATA FIELDS USING NEIGHBOR METHOD."
   ipopt(1) = nint(1.0 / mdl_res_input) + 1   ! search box width of 1 deg.
   ip = 2
 end if

 kgds=kgds_output
 kgds(1)=-1         ! tell ipolates to interpolate to just water points.
                    ! default is to interpolate to all grid points.
 iret     =0
 ij_input =im_input*jm_input

 call ipolates(ip,ipopt,kgds_input,kgds,ij_input,count_water,& 
               num_nsst_fields, ibi, bitmap_input, data_input,  &
               count_water,rlat_water,rlon_water,ibo,bitmap_water, &
               data_water,iret) 

 if (iret /=0) then
   print*,'fatal error in ipolates interpolating nsst data ',iret
   stop 445
 endif

 deallocate(bitmap_input)

!----------------------------------------------------------------
! ipolates may not find data at every output grid point.
! This can happen with isolated lakes, for example.
! Need to fill these points with default values.
!----------------------------------------------------------------

 do k = 1, num_nsst_fields
   do ij=1, count_water
     if (.not.bitmap_water(ij,k)) then
       data_water(ij,k)=0.0  ! default value for most fields
       if (k==5) data_water(ij,k)=30.0  ! default value for xz
       if (k==16) data_water(ij,k)=1.0  ! default value for ifd
       if (k==17) then   ! default for tref is skin t (sst)
         data_water(ij,k)=tskin_output(ijsav_water(ij))
         write(6,50) rlat_water(ij), rlon_water(ij), data_water(ij,k)
       endif
     endif
   enddo
 enddo

!----------------------------------------------------------------
! IFD is a flag, but is stored as a float.  Remove and
! fractional values.
!----------------------------------------------------------------

 data_water(:,16) = float(nint(data_water(:,16)))

 deallocate(rlat_water,rlon_water,bitmap_water)

 50 format(1x,'- USING DEFAULT VALUE FOR TREF AT LAT: ',f6.2, &
           ' LON: ',f7.2,' IS ',f5.1)

!----------------------------------------------------------------
! Now put the water points back into the array that holds
! all output grid points.
!----------------------------------------------------------------

 data_output=0.0  ! zero out fields at non-water points
 data_output(:,5)  = 30.0  ! filler value for xz at non-water points
 data_output(:,17) = tskin_output  ! use skin temperature from the 
                                   ! land model as fill value 
                                   ! for tref at non-water points.

 do ij=1, count_water
   data_output(ijsav_water(ij),:)=data_water(ij,:)
 enddo

 deallocate (ijsav_water)
 deallocate (data_water)

 return
 end subroutine nsst_chgres
