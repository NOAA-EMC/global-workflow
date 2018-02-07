 subroutine nsst_chgres(im_input,jm_input,  &
                        mask_output, ij_output, kgds_input, &
                        nsst_input, data_output, num_nsst_fields, &
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
!----------------------------------------------------------------

 use nstio_module

 implicit none

 type(nstio_data), intent(in) :: nsst_input  ! nsst data - input grid

 integer, intent(in)          :: ij_output
                                 ! number of grid points - output grid.
 integer, intent(in)          :: kgds_input(200), kgds_output(200)
                                 ! grib 1 grid desc section - input/output grids
 integer, intent(in)          :: im_input, jm_input
                                 ! number of grid points in i/j direction - 
                                 ! input grid.
 integer, intent(in)          :: num_nsst_fields
 
 real, intent(in)             :: mask_output(ij_output)
                                 ! land mask - output grid 
 real, intent(in)             :: rlat_output(ij_output)
                                 ! latitudes on output grid
 real, intent(in)             :: rlon_output(ij_output)
                                 ! longitudes on output grid

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
 real, allocatable            :: data_input(:,:,:)
 real, allocatable            :: rlat_water(:), rlon_water(:)

!----------------------------------------------------------------
! ipolates can't handle a data structure, so copy input data 
! into an array.
!----------------------------------------------------------------

 allocate(data_input(im_input,jm_input,num_nsst_fields))

 data_input(:,:,1) = nsst_input%xt
 data_input(:,:,2) = nsst_input%xs
 data_input(:,:,3) = nsst_input%xu
 data_input(:,:,4) = nsst_input%xv
 data_input(:,:,5) = nsst_input%xz
 data_input(:,:,6) = nsst_input%zm
 data_input(:,:,7) = nsst_input%xtts
 data_input(:,:,8) = nsst_input%xzts
 data_input(:,:,9) = nsst_input%dt_cool
 data_input(:,:,10) = nsst_input%z_c
 data_input(:,:,11) = nsst_input%c_0
 data_input(:,:,12) = nsst_input%c_d
 data_input(:,:,13) = nsst_input%w_0
 data_input(:,:,14) = nsst_input%w_d
 data_input(:,:,15) = nsst_input%d_conv
 data_input(:,:,16) = nsst_input%ifd
 data_input(:,:,17) = nsst_input%tref
 data_input(:,:,18) = nsst_input%qrain

!----------------------------------------------------------------
! bitmap flag for input data.  all input fields will be
! interpolated using the same bitmap.
!----------------------------------------------------------------

 ibi=1 
      
!----------------------------------------------------------------
! mask is: 0-open water, 1-land, 2-sea ice.  nsst model
! only operates at open water points.
!----------------------------------------------------------------

 allocate(bitmap_input(im_input,jm_input,num_nsst_fields))
 bitmap_input=.false.
 do j=1,jm_input
 do i=1,im_input
   if (nsst_input%slmsk(i,j) < 0.5) then
     bitmap_input(i,j,:)=.true.
   endif
 enddo
 enddo

!----------------------------------------------------------------
! only interpolate to output points that are open water.
! mask values are: 0-open water, 1-land, 2-sea ice.
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
! ipolates options.
!----------------------------------------------------------------

 ip=2               ! use nearest neighbor interpolation as
                    ! some nsst fields are not continuous.
 ipopt=0
                    ! width of square for spiral search, use 5 deg.
 ipopt(1)= 5.0 / (float(kgds_input(9))*.001) 
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
   print*,'error in ipolates interpolating nsst data ',iret
   stop 445
 endif

 deallocate(data_input,bitmap_input)

!----------------------------------------------------------------
! ipolates may not find data at every output grid point.
! this can happen with isolated lakes, for example.
! need to fill these points with default values.
!----------------------------------------------------------------

 do k = 1, num_nsst_fields
   do ij=1, count_water
     if (.not.bitmap_water(ij,k)) then
       data_water(ij,k)=0.0  ! default value for most fields
       if (k==5) data_water(ij,k)=30.0  ! default value for xz
       if (k==16) data_water(ij,k)=1.0  ! default value for ifd
!      don't know of a good way to set a default value of tref.
!      this is my simple minded attempt.
       if (k==17) then 
         if(abs(rlat_water(ij))>=60.0)then
           data_water(ij,k)=273.16 ! default value for tref
         elseif(abs(rlat_water(ij))<=30.0) then
           data_water(ij,k)=300.0 ! default value for tref
         else
           data_water(ij,k)= (-.8947)*(abs(rlat_water(ij))) + 326.84
         endif
         write(6,50) rlat_water(ij), rlon_water(ij), data_water(ij,k)
       endif
     endif
   enddo
 enddo

 deallocate(rlat_water,rlon_water)

 50 format(1x,'- USING DEFAULT VALUE FOR TREF AT LAT: ',f6.2, &
           ' LON: ',f7.2,' IS ',f5.1)
 deallocate (data_water, bitmap_water)

!----------------------------------------------------------------
! now put the water points back into the array that holds
! all output grid points.
!----------------------------------------------------------------

 data_output=0.0  ! zero out fields at non-water points
 data_output(:,17)=255.0  ! filler value for tref at non-water points.
 do ij=1, count_water
   data_output(ijsav_water(ij),:)=data_water(ij,:)
 enddo

 deallocate (ijsav_water)

 return
 end subroutine nsst_chgres
