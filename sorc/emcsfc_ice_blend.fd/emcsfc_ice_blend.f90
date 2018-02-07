 program emcsfc_ice_blend
!$$$  main program documentation block
!
! program:  emcsfc_ice_blend           create blended ice product 
!                                      for gfs/gdas cycles
!
! prgmmr: gayno         org: emc               date: 2014-03-20
!
! abstract:  Create a global 5-minute blended ice concentration 
!            dataset for use by GDAS/GFS.  This blend is created
!            from National Ice Center Interactive Multisensor
!            Snow and Ice Mapping System (IMS) data, and the 
!            NCEP/MMAB global 5-minute ice concentration data.
!
! program history log:
!   2014-03-20  Initial version.
!   2014-10-20  Use grib2 for all i/o.  Mask out 'land' points
!               in the blended analysis using a bitmap
!               instead of the '1.57' flag.
!
! usage:
!   input files:
!      fort.11 - NH IMS ice data, interpolated to the 5-minute grid  (grib 2)
!      fort.15 - global 5-minute MMAB ice concentration file (grib 2)
!      fort.17 - global 5-minute MMAB land mask file (grib 2).  
!
!   output files:
!      fort.51 - global 5-minute blended ice concentration file (grib 2)
!
! condition codes:
!    0 - normal run
!    1 - bad open of 5-minute MMAB land mask file
!    2 - bad read of 5-minute MMAB land mask grib record
!    5 - bad open of IMS ice file
!    7 - bad read of IMS ice grib record
!    8 - bad open of 5-minute MMAB ice concentration file
!    9 - bad read of 5-minute MMAB ice concentration grib header
!   10 - bad read of 5-minute MMAB ice concentration grib record
!   16 - bad open of 5-minute blended ice concentration file
!   17 - bad write of 5-minute blended ice concentration file
!
! comments:
!   This program creates a sea ice concentration file 
!   suitable for use by the GFS global cycle program.  The 5-minute
!   ice concentration file from MMAB can't be used 'as is' because
!   the MMAB data can't give a reliable analysis for small
!   lakes now resolved by the GFS land mask.
!
!   This program creates an ice concentration file on the same
!   5-minute grid as the MMAB data using IMS data from the National
!   Ice Center to fill in small lakes.  The IMS data is NH only and
!   is a yes/no flag (not a concentration).  The IMS data is on
!   a 4km polar stereographic grid and must be interpolated to the
!   MMAB 5-minute grid (NCEP grid 173) before ingest to this program.
!   This interpolation may be done using copygb2 as follows:
!
!   grid173="0 0 0 0 0 0 0 0 4320 2160 0 0 89958000 42000 48 -89958000 359958000 83000 83000 0"
!   copygb2 -x -i3 -g "$grid173" ims.icec.grib2 ims.icec.5min.grib2
!
!   When using the "budget" interpolation option (-i3), the 
!   IMS yes/no flag will be converted to a pseudo ice concentration.
!
!   The IMS data contains a land/water mask.  Ice concentration is only
!   specified at 'water' points.
!
!   The 5-minute data mask contains 'land', 'coast' and
!   'water' points.  Ice concentration is specified at 'coast'
!   and 'water', but 'coast' points have error and are not used here.
!   
!   The blending process is as follows in the NH:
!      (1) If IMS indicates 'land', then the blended value is set to 'land'
!          and is bitmapped out.
!      (2) If the IMS is 'water', and the MMAB data point
!          is 'coast' or 'land', the IMS ice concentration is used
!          as the blended value.
!      (3) If IMS and MMAB indicate a point as water, then
!          (a) The blended value is set to zero if the IMS concentration is less
!              than 50%. 
!          (b) If the IMS concentration is >=50%, then the neighboring
!              5-minute data is checked for ice.  
!              (i) If the 5-minute point is ice free, but at least one 
!                  of its neighbors has ice, the blended value is set to
!                  the IMS ice concentration. Examination of the 5-minute
!                  showed isolated open water points within large areas of
!                  ice.  This logic attempts to remove these likely wrong points.
!              (ii) If (i) is false, then the blended value is set to
!                   the 5-minute value or 15%, whichever is greater.  15%
!                   is the lower threshold in the MMAB 5-minute product.
!   In the SH, the blended value is simply the 5-minute ice concentration
!   at 'water' points.  'Coast' and 'land' points are bitmapped out.
!
! attributes:
!   language: f90
!   machine: NCEP WCOSS
!
!$$$

 use grib_mod  ! grib 2 libraries

 implicit none

 type(gribfield)        :: ims, mask, mmab

 character(len=200)     :: infile, outfile

 integer, parameter     :: imax=4320
 integer, parameter     :: jmax=2160

 integer                :: i,j, istat, iunit
 integer                :: ii, iii, jj, jjj, count
 integer                :: lugi
 integer                :: jdisc, jgdtn, jpdtn, k
 integer                :: jids(200), jgdt(200), jpdt(200)
 integer, allocatable   :: mask_5min(:,:), mask_ims(:,:)

 logical*1, allocatable :: lbms_ims(:,:)
 logical                :: unpack

 real, allocatable      :: dummy(:,:)
 real, allocatable      :: ice_ims(:,:), ice_5min(:,:), ice_blend(:,:)

!--------------------------------------------------------------------------------
! Read the 5-minute mmab land mask file.  Required because the 5-minute data does
! not have a bitmap.
!--------------------------------------------------------------------------------

 call w3tagb('EMCSFC_ICE_BLEND',2014,75,0000,'EMC')

 call getenv("FORT17", infile)
 iunit=17
 print*,"- OPEN 5-MINUTE LAND-SEA MASK FILE: ", trim(infile)
 call baopenr (iunit, infile, istat)
 if (istat /= 0) then
   print*,'FATAL ERROR: BAD OPEN. ISTAT: ', istat
   stop 1
 endif
 
 nullify(mask%idsect)
 nullify(mask%local)
 nullify(mask%list_opt)
 nullify(mask%igdtmpl)
 nullify(mask%ipdtmpl)
 nullify(mask%coord_list)
 nullify(mask%idrtmpl)
 nullify(mask%bmap)
 nullify(mask%fld)

 j       = 0      ! search at beginning of file
 lugi    = 0      ! no grib index file
 jdisc   = 2      ! search for discipline
 jpdtn   = 0      ! search for product definition template number
 jgdtn   = 0      ! search for grid definition template number; 0 - lat/lon grid
 jids    = -9999  ! array of values in identification section, set to wildcard
 jgdt    = -9999  ! array of values in grid definition template 3.m
 jpdt    = -9999  ! array of values in product definition template 4.n
 jpdt(1) = 0      ! search for parameter category 
 jpdt(2) = 0      ! search for parameter number 
 unpack  = .true. ! unpack data

 print*,"- DEGRIB DATA"
 call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
             unpack, k, mask, istat)

 if (istat /= 0) then
   print*,'FATAL ERROR: BAD DEGRIB OF DATA. ISTAT: ', istat
   stop 2
 endif

 call baclose (iunit,istat)

!--------------------------------------------------------------------------------
!  The MMAB mask codes are:
!
!  0    - water
!  1.57 - land
!  1.95 - coast
!
!  Convert these to something easier to work with.
!--------------------------------------------------------------------------------

 allocate(dummy(imax,jmax))
 dummy=reshape (mask%fld , (/imax,jmax/) )

 if(associated(mask%idsect))     deallocate(mask%idsect)
 if(associated(mask%local))      deallocate(mask%local)
 if(associated(mask%list_opt))   deallocate(mask%list_opt)
 if(associated(mask%igdtmpl))    deallocate(mask%igdtmpl)
 if(associated(mask%ipdtmpl))    deallocate(mask%ipdtmpl)
 if(associated(mask%coord_list)) deallocate(mask%coord_list)
 if(associated(mask%idrtmpl))    deallocate(mask%idrtmpl)
 if(associated(mask%bmap))       deallocate(mask%bmap)
 if(associated(mask%fld))        deallocate(mask%fld)

 allocate(mask_5min(imax,jmax))

 do j = 1, jmax
 do i = 1, imax
   if (dummy(i,j) < 0.1) then
      mask_5min(i,j)=0   ! water
   elseif (dummy(i,j) > 1.94) then
      mask_5min(i,j)=1   ! coast
   else
      mask_5min(i,j)=2   ! land
   endif
 enddo
 enddo

 deallocate(dummy)

!--------------------------------------------------------------------------------
! Read ims data that has been interpolated to the 5-min grid.
!--------------------------------------------------------------------------------

 call getenv("FORT11", infile)
 iunit=11
 print*,"- OPEN IMS ICE DATA: ", trim(infile)
 call baopenr (iunit, infile, istat)
 if (istat /= 0) then
   print*,'FATAL ERROR: BAD OPEN. ISTAT: ', istat
   stop 5
 endif
 
 nullify(ims%idsect)
 nullify(ims%local)
 nullify(ims%list_opt)
 nullify(ims%igdtmpl)
 nullify(ims%ipdtmpl)
 nullify(ims%coord_list)
 nullify(ims%idrtmpl)
 nullify(ims%bmap)
 nullify(ims%fld)

 j       = 0      ! search at beginning of file
 lugi    = 0      ! no grib index file
 jdisc   = 10     ! search for discipline, ocean products
 jpdtn   = 0      ! search for product definition template number
 jgdtn   = 0      ! search for grid definition template number; 0 - lat/lon grid
 jids    = -9999  ! array of values in identification section, set to wildcard
 jgdt    = -9999  ! array of values in grid definition template 3.m
 jpdt    = -9999  ! array of values in product definition template 4.n
 jpdt(1) = 2      ! search for parameter category, ice
 jpdt(2) = 0      ! search for parameter number, ice cover
 unpack  = .true. ! unpack data

 print*,"- DEGRIB DATA"
 call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
             unpack, k, ims, istat)

 if (istat /= 0) then
   print*,'FATAL ERROR: BAD DEGRIB OF DATA. ISTAT: ', istat
   stop 7
 endif

 call baclose (iunit,istat)

 allocate (lbms_ims(imax,jmax))
 lbms_ims=reshape (ims%bmap , (/imax,jmax/) )
 allocate (ice_ims(imax,jmax))
 ice_ims=reshape (ims%fld , (/imax,jmax/) )

 print*,"- CREATE IMS LAND-SEA MASK FROM BITMAP."
 allocate(mask_ims(imax,jmax))
 mask_ims = 0  ! water
 do j = 1, jmax
 do i = 1, imax
   if (.not.lbms_ims(i,j)) mask_ims(i,j) = 2  ! land
 enddo
 enddo

 deallocate(lbms_ims)

 if(associated(ims%idsect))     deallocate(ims%idsect)
 if(associated(ims%local))      deallocate(ims%local)
 if(associated(ims%list_opt))   deallocate(ims%list_opt)
 if(associated(ims%igdtmpl))    deallocate(ims%igdtmpl)
 if(associated(ims%ipdtmpl))    deallocate(ims%ipdtmpl)
 if(associated(ims%coord_list)) deallocate(ims%coord_list)
 if(associated(ims%idrtmpl))    deallocate(ims%idrtmpl)
 if(associated(ims%bmap))       deallocate(ims%bmap)
 if(associated(ims%fld))        deallocate(ims%fld)

!--------------------------------------------------------------------------------
! Now read the MMAB 5-minute data.
!--------------------------------------------------------------------------------

 call getenv("FORT15", infile)
 iunit=15
 print*,"- OPEN 5-MINUTE ICE CONCENTRATION DATA: ", trim(infile)
 call baopenr (iunit, infile, istat)
 if (istat /= 0) then
   print*,'FATAL ERROR: BAD OPEN. ISTAT: ', istat
   stop 8
 endif
 
 nullify(mmab%idsect)
 nullify(mmab%local)
 nullify(mmab%list_opt)
 nullify(mmab%igdtmpl)
 nullify(mmab%ipdtmpl)
 nullify(mmab%coord_list)
 nullify(mmab%idrtmpl)
 nullify(mmab%bmap)
 nullify(mmab%fld)

 j       = 0      ! search at beginning of file
 lugi    = 0      ! no grib index file
 jdisc   = 10     ! search for discipline, ocean products
 jpdtn   = 0      ! search for product definition template number
 jgdtn   = 0      ! search for grid definition template number; 0 - lat/lon grid
 jids    = -9999  ! array of values in identification section, set to wildcard
 jgdt    = -9999  ! array of values in grid definition template 3.m
 jpdt    = -9999  ! array of values in product definition template 4.n
 jpdt(1) = 2      ! search for parameter category, ice
 jpdt(2) = 0      ! search for parameter number, ice cover
 unpack  = .true. ! unpack data

 print*,"- DEGRIB DATA"
 call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
             unpack, k, mmab, istat)

 if (istat /= 0) then
   print*,'FATAL ERROR: BAD DEGRIB OF DATA. ISTAT: ', istat
   stop 9
 endif

 allocate (ice_5min(imax,jmax))
 ice_5min=reshape (mmab%fld , (/imax,jmax/) )

 call baclose (iunit,istat)
 
!--------------------------------------------------------------------------------
! Blend IMS and 5-minute data in northern hemisphere.
!--------------------------------------------------------------------------------

 print*,"- BLEND IMS AND 5-MINUTE DATA IN NH."
 allocate(ice_blend(imax,jmax))
 ice_blend=-9.   ! flag for land.  will be bitmapped out.
 do j = 1, (jmax/2)
 do i = 1, imax
   if (mask_ims(i,j) == 0) then   ! ims water point
     if (mask_5min(i,j) > 0) then ! 5-min land
       ice_blend(i,j)=ice_ims(i,j)   ! use ims value
     else  ! ims and 5min mask indicate water point
       if (ice_ims(i,j) > .5) then   ! ims indicates ice
         count = 0
         do jj = -1, 1
         do ii = -1, 1
           if (ii == 0 .and. jj == 0) cycle
           jjj = j + jj
           if (jjj < 1) cycle
           iii = ii + i
           if (iii < 1) iii = iii + imax
           if (iii > imax) iii = iii - imax
           if (mask_5min(iii,jjj) == 0) then ! 5-min water
             if (ice_5min(iii,jjj) >= 0.5) then
               count = count + 1
             endif
           endif
         enddo
         enddo
         if (count > 0 .and. ice_5min(i,j) == 0.0) then
           ice_blend(i,j) = ice_ims(i,j)
         else
           ice_blend(i,j) = max(ice_5min(i,j),0.15)
         endif
       else  ! ims indicates open water.
         ice_blend(i,j) = 0.
       endif
     endif
   end if
 enddo
 enddo

 deallocate(mask_ims, ice_ims)

!--------------------------------------------------------------------------------
! In the SH, the blend is simply the 5-minute data.  Only consider 'water'
! points.
!--------------------------------------------------------------------------------

 do j = (jmax/2)+1, jmax
 do i = 1, imax
   if (mask_5min(i,j) == 0) then  ! 'water'
     ice_blend(i,j) = ice_5min(i,j)
   endif
 enddo
 enddo

 deallocate(mask_5min, ice_5min)

!--------------------------------------------------------------------------------
! Output blended data to a grib 2 file.
!--------------------------------------------------------------------------------

 call getenv("FORT51", outfile)
 iunit=51
 print*,"- OUTPUT BLENDED ICE DATA TO ", trim(outfile)
 print*,"- OPEN FILE."
 call baopenw(iunit, outfile, istat)
 if (istat /= 0) then
   print*,'FATAL ERROR: BAD OPEN. ISTAT: ', istat
   stop 16
 endif

!--------------------------------------------------------------------------------
! Use grib header information from the mmab file (stored in the mmab data
! structure) with the following exceptions:
!
! 1) Increase precision of corner point lat/lons and dx/dy.
! 2) Use simple packing instead of jpeg compression.  Grads has problems
!    with the latter.
! 3) Use a bitmap to mask out land points instead of the mmab convention
!    of using '0' at land points.
!--------------------------------------------------------------------------------

 mmab%igdtmpl(12)=89958333
 mmab%igdtmpl(13)=41667
 mmab%igdtmpl(15)=-89958333
 mmab%igdtmpl(16)=359958333
 mmab%igdtmpl(17)=83333
 mmab%igdtmpl(18)=83333

 deallocate (mmab%idrtmpl)   ! use simple packing
 mmab%idrtnum=0  
 mmab%idrtlen=5
 allocate(mmab%idrtmpl(mmab%idrtlen))
 mmab%idrtmpl=0
 mmab%idrtmpl(3) = 2         ! use two decimal points.

 mmab%fld=reshape(ice_blend, (/imax*jmax/) )

 mmab%ibmap=0  ! use bitmap
 allocate (mmab%bmap(imax*jmax))
 mmab%bmap=.true.
 where (mmab%fld < -8.) mmab%bmap=.false.   ! land

 print*,"- GRIB DATA."
 call putgb2(iunit, mmab, istat)
 if (istat /= 0) then
   print*,'FATAL ERROR: BAD WRITE. ISTAT: ', istat
   stop 17
 endif

 call baclose(iunit, istat)

 deallocate(ice_blend)

 if(associated(mmab%idsect))     deallocate(mmab%idsect)
 if(associated(mmab%local))      deallocate(mmab%local)
 if(associated(mmab%list_opt))   deallocate(mmab%list_opt)
 if(associated(mmab%igdtmpl))    deallocate(mmab%igdtmpl)
 if(associated(mmab%ipdtmpl))    deallocate(mmab%ipdtmpl)
 if(associated(mmab%coord_list)) deallocate(mmab%coord_list)
 if(associated(mmab%idrtmpl))    deallocate(mmab%idrtmpl)
 if(associated(mmab%bmap))       deallocate(mmab%bmap)
 if(associated(mmab%fld))        deallocate(mmab%fld)

 print*,''
 print*,'****************************'
 print*,'**** NORMAL TERMINATION ****'
 print*,'****************************'

 call w3tage('EMCSFC_ICE_BLEND')

 stop

 end program emcsfc_ice_blend
