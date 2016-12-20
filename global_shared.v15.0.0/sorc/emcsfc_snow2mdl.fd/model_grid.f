 module model_grid
!$$$  module documentation block
!            
! module:    model_grid
!   prgmmr: gayno         org: w/np2     date: 2005-dec-16
!
! abstract: read in data defining the model grid.
!     
! program history log:
!   2005-dec-16  gayno   - initial version
!   2007-nov-30  gayno   - improved method for thinning gfs grids.
!                          added nam b-grids.
!   2014-sep-29  gayno   - add option to read model lat, lon and
!                          landmask data in grib2.
!
! usage: use model_grid
!
! remarks: some variable definitions
!   grid_id_mdl    - grib id of model grid, 4-gaussian, 203-egrid
!   i/jpts_mdl     - i/j index of point on full grid
!   imdl           - i-dimension of model grid
!   jmdl           - j-dimension of model grid
!   ijmdl          - total number of model land points
!   kgds_mdl       - holds grib gds info of model grid
!   lats_mdl       - latitudes of model grid points
!   lons_mdl       - longitudes of model grid points
!   lonsperlat     - for global grids, the number of i points
!                    in each row (decrease toward pole)
!   lsmask_mdl     - land mask of model grid (0 - non land, 1-land)
!                    for global grids run thinned, will contain
!                    a modified version of the original mask
!                    that has land at all points encompassed by a 
!                    thinned point
!   lsmask_mdl_sav - saved copy of land mask of model grid (0 - non land, 1-land)
!                    only used for global thinned grids.
!   resol_mdl      - approximate model resolution in km.
!   thinned        - when true, global grids will run thinned
!                    (# i points decrease toward pole)
!
!$$$

 use program_setup, only         : model_lsmask_file, &
                                   model_lon_file, &
                                   model_lat_file, &
                                   gfs_lpl_file

 integer                        :: grid_id_mdl
 integer                        :: imdl
 integer                        :: jmdl
 integer                        :: ijmdl ! only land points
 integer, allocatable           :: ipts_mdl(:), jpts_mdl(:) 

 integer                        :: kgds_mdl(200)
 integer, allocatable           :: lonsperlat_mdl (:)
 
 logical                        :: thinned

 real, allocatable              :: lats_mdl    (:)
 real                           :: lat11, latlast
 real                           :: lon11, lonlast
 real, allocatable              :: lons_mdl    (:)
 real, allocatable              :: lsmask_mdl  (:,:)
 real, allocatable              :: lsmask_mdl_sav (:,:)
 real                           :: resol_mdl  ! in km

 contains

 subroutine read_mdl_grid_info
!$$$  subprogram documentation block
!              
! subprogram:    read_mdl_grid_info
!   prgmmr: gayno          org: w/np2     date: 2005-dec-16
!
! abstract: read latitude, longitude, land/sea mask on the
!   model grid.
!
! program history log:
! 2005-dec-16  gayno    - initial version
! 2007-nov-30  gayno    - improved method for thinning gfs grids
!                         added nam b-grids
! 2014-sep-29  gayno    - add option to read lat,lon and mask
!                         data in grib2.
!
! usage: call read_mdl_grid_info
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!   inputs:
!     - model latitudes (grib 1 or grib 2)
!     - model longitudes (grib 1 or grib 2)
!     - model landmask (grib 1 or grib 2)
!     - # pts per row, gfs grid (the "lonsperlat" file, ascii)
!
!  outputs: none
!
! condition codes: all fatal
!   76 - bad open/read gfs "lonsperlat" file
!   79 - unrecognized model grid
!   80 - bad open model latitude file
!   81 - bad read of model latitude grib 1 header
!   82 - bad read of model latitude data
!   83 - bad open model longitude file
!   82 - bad read of model longitude data
!   85 - bad open model landmask file
!   86 - bad read of model landmask data
!   90 - model latitude file not grib 1 or grib 2
!   91 - model longitude file not grib 1 or grib 2
!   92 - model landmask file not grib 1 or grib 2
!
! remarks: none.
!
!$$$

 use grib_mod  ! grib 2 library

 implicit none

 character*150           :: fngrib

 integer                 :: i, j, ij, jj
 integer                 :: ii, iii, istart, iend, imid
 integer                 :: iret
 integer                 :: isgrib
 integer, parameter      :: iunit = 14  ! unit of input grib file
 integer, parameter      :: iunit2 = 34  ! unit of input lonsperlat file
 integer                 :: jgds(200)
 integer                 :: jpds(200)
 integer                 :: jdisc, jgdtn, jpdtn, k
 integer                 :: jids(200), jgdt(200), jpdt(200)
 integer                 :: lskip
 integer, parameter      :: lugi = 0    ! unit of grib index file - not used
 integer                 :: kgds(200)
 integer                 :: kpds(200)
 integer                 :: message_num
 integer                 :: numbytes
 integer                 :: numpts

 logical*1, allocatable  :: lbms(:)
 logical                 :: unpack

 real                    :: gridis, gridie, fraction, x1, r
 real, allocatable       :: lats_mdl_temp  (:,:)
 real, allocatable       :: lons_mdl_temp  (:,:)
 
 type(gribfield)         :: gfld

 print*,"- READ MODEL GRID INFORMATION"

!-----------------------------------------------------------------------
! read latitudes on the model grid.  first check if file is grib1 or 2.
!-----------------------------------------------------------------------

 fngrib = model_lat_file

 call grib_check(fngrib, isgrib)

 if (isgrib==0) then
   print*,'- FATAL ERROR: MODEL LAT FILE MUST BE GRIB1 OR GRIB2 FORMAT'
   call w3tage('SNOW2MDL')
   call errexit(90)
 end if

 print*,"- OPEN MODEL LAT FILE ", trim(fngrib)
 call baopenr (iunit, fngrib, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR: BAD OPEN, IRET IS ', iret
   call w3tage('SNOW2MDL')
   call errexit(80)
 end if

 if (isgrib==1) then ! grib 1 file

!-----------------------------------------------------------------------
! tell degribber to search for latitudes
!-----------------------------------------------------------------------

   lskip   = -1  ! read beginning of file
   jgds    = -1
   jpds    = -1
   jpds(5) = 176 ! latitude
   kgds    = -1   
   kpds    = -1  

   print*,"- GET GRIB HEADER"
   call getgbh(iunit, lugi, lskip, jpds, jgds, numbytes,  &
               numpts, message_num, kpds, kgds, iret)

   if (iret /= 0) then
     print*,'- FATAL ERROR: BAD READ OF GRIB HEADER. IRET IS ', iret
     call w3tage('SNOW2MDL')
     call errexit(81)
   end if

!-----------------------------------------------------------------------
! save gds for gribbing the interpolated data later.  also required
! by ncep ipolates library.
!-----------------------------------------------------------------------

   kgds_mdl = kgds

!-----------------------------------------------------------------------
! get model grid specs from header.  model resolution (km) is used
! to determine the interpolation method.
!-----------------------------------------------------------------------

   grid_id_mdl = kpds(3) ! grib 1 grid id number. sect 1, oct 7

   if (kgds(1) == 4) then  ! gaussian grid
     imdl = kgds(2)  ! i-dimension of model grid
     jmdl = kgds(3)  ! j-dimension of model grid
     resol_mdl = float(kgds(9)) / 1000.0 * 111.0
   else if (kgds(1) == 203) then  ! e-grid 
     imdl = kgds(2)  ! i-dimension of model grid
     jmdl = kgds(3)  ! j-dimension of model grid
     resol_mdl = sqrt( (float(kgds(9)) / 1000.0)**2   +    &
                     (float(kgds(10)) / 1000.0)**2  )
     resol_mdl = resol_mdl * 111.0
   else if (kgds(1) == 205) then  ! b-grid 
     imdl = kgds(2)  ! i-dimension of model grid
     jmdl = kgds(3)  ! j-dimension of model grid
     resol_mdl = ((float(kgds(9)) / 1000.0) + (float(kgds(10)) / 1000.0)) &
                  * 0.5 * 111.0
   else
     print*,'- FATAL ERROR: UNRECOGNIZED MODEL GRID.'
     call w3tage('SNOW2MDL')
     call errexit(79)
   end if

   allocate(lats_mdl_temp(imdl,jmdl))
   allocate(lbms(imdl*jmdl))

   print*,"- DEGRIB DATA"
   call getgb(iunit, lugi, (imdl*jmdl), lskip, jpds, jgds, &
              numpts, message_num, kpds, kgds, lbms, lats_mdl_temp, iret)

   if (iret /= 0) then
     print*,'- FATAL ERROR: BAD DEGRIB OF FILE. IRET IS ',iret
     call w3tage('SNOW2MDL')
     call errexit(82)
   end if

   deallocate(lbms)

   lat11   = lats_mdl_temp(1,1)
   latlast = lats_mdl_temp(imdl,jmdl)

 elseif (isgrib==2) then ! grib 2 file

   j       = 0      ! search at beginning of file
   jdisc   = 0      ! search for discipline; 0 - meteorological products
   jpdtn   = -1     ! search for any product definition template number
   jgdtn   = -1     ! search for any grid definition template number
   jids    = -9999  ! array of values in identification section, set to wildcard
   jgdt    = -9999  ! array of values in grid definition template 3.m
   jpdt    = -9999  ! array of values in product definition template 4.n
   jpdt(1) = 191    ! search for parameter category - misc
   jpdt(2) = 1      ! search for parameter number - latitude
   unpack  = .true. ! unpack data

   call grib2_null(gfld)

   print*,"- DEGRIB DATA"
   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

   if (iret /=0) then
    print*,'- FATAL ERROR: BAD DEGRIB OF FILE, IRET IS ', iret
    call w3tage('SNOW2MDL')
    call errexit(82)
   endif

!-----------------------------------------------------------------------
! create the grib 1 gds array from the g2 gdt array.  the grib 1
! gds info is used by ipolates and for gribbing the final snow analysis.
!-----------------------------------------------------------------------

   call gdt_to_gds(gfld%igdtnum, gfld%igdtmpl, gfld%igdtlen, kgds_mdl, &
                   imdl, jmdl, resol_mdl)

   grid_id_mdl = 255 ! grib1 grid id number. n/a for grib2.
                     ! set to 'missing'.

   allocate(lats_mdl_temp(imdl,jmdl))
   lats_mdl_temp = reshape (gfld%fld , (/imdl,jmdl/) )

   lat11   = lats_mdl_temp(1,1)
   latlast = lats_mdl_temp(imdl,jmdl)

   call grib2_free(gfld)

 endif ! grib1 or grib2?

 call baclose(iunit,iret)

!-----------------------------------------------------------------------
! read longitudes on the model grid.
!-----------------------------------------------------------------------

 fngrib = model_lon_file

 call grib_check(fngrib, isgrib)

 if (isgrib==0) then
   print*,'- FATAL ERROR: MODEL LON FILE MUST BE GRIB1 OR GRIB2 FORMAT'
   call w3tage('SNOW2MDL')
   call errexit(91)
 end if

 print*,"- OPEN MODEL LON FILE ", trim(fngrib)
 call baopenr (iunit, fngrib, iret)

 if (iret /= 0) then
   print*,"- FATAL ERROR: BAD OPEN. IRET IS ", iret
   call w3tage('SNOW2MDL')
   call errexit(83)
 end if

 if (isgrib==1) then ! grib 1 file

   lskip   = -1  
   kgds    = -1   
   kpds    = -1  
   jgds    = -1
   jpds    = -1
   jpds(5) = 177  ! longitude 

   allocate(lons_mdl_temp(imdl,jmdl))
   allocate(lbms(imdl*jmdl))

   print*,"- DEGRIB DATA"
   call getgb(iunit, lugi, (imdl*jmdl), lskip, jpds, jgds, &
              numpts, message_num, kpds, kgds, lbms, lons_mdl_temp, iret)

   if (iret /= 0) then
     print*,'- FATAL ERROR: BAD DEGRIB OF DATA. IRET IS ',iret
     call w3tage('SNOW2MDL')
     call errexit(84)
   end if

   deallocate(lbms)

   lon11   = lons_mdl_temp(1,1)
   lonlast = lons_mdl_temp(imdl,jmdl)

 elseif (isgrib==2) then ! grib2

   j       = 0      ! search at beginning of file
   jdisc   = 0      ! search for discipline; 0 - meteorological products
   jpdtn   = -1     ! search for any product definition template number
   jgdtn   = -1     ! search for any grid definition template number
   jids    = -9999  ! array of values in identification section, set to wildcard
   jgdt    = -9999  ! array of values in grid definition template 3.m
   jpdt    = -9999  ! array of values in product definition template 4.n
   jpdt(1) = 191    ! search for parameter category - misc
   jpdt(2) = 2      ! search for parameter number - longitude
   unpack  = .true. ! unpack data

   call grib2_null(gfld)

   print*,"- DEGRIB DATA"
   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

   if (iret /=0) then
    print*,'- FATAL ERROR: BAD DEGRIB OF FILE, IRET IS ', iret
    call w3tage('SNOW2MDL')
    call errexit(84)
   endif

   allocate(lons_mdl_temp(imdl,jmdl))
   lons_mdl_temp = reshape (gfld%fld , (/imdl,jmdl/) )

   lon11   = lons_mdl_temp(1,1)
   lonlast = lons_mdl_temp(imdl,jmdl)

   call grib2_free(gfld)

 endif ! grib1 or grib?

 call baclose(iunit, iret)

!-----------------------------------------------------------------------
! read model land/sea mask. 
!-----------------------------------------------------------------------

 fngrib = model_lsmask_file

 call grib_check(fngrib, isgrib)

 if (isgrib==0) then
   print*,'- FATAL ERROR: MODEL LANDMASK FILE MUST BE GRIB1 OR GRIB2 FORMAT'
   call w3tage('SNOW2MDL')
   call errexit(92)
 end if

 print*,"- OPEN MODEL LANDMASK FILE ", trim(fngrib)
 call baopenr (iunit, fngrib, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR: BAD OPEN OF FILE. IRET IS ', iret
   call w3tage('SNOW2MDL')
   call errexit(85)
 end if

 if (isgrib==1) then ! grib 1 file

   lskip   = -1 
   kgds    = -1  
   kpds    = -1 
   jpds    = -1
   jgds    = -1
   jpds(5) = 81   ! land-sea mask

   allocate(lsmask_mdl(imdl,jmdl))
   allocate(lbms(imdl*jmdl))

   print*,"- DEGRIB DATA"
   call getgb(iunit, lugi, (imdl*jmdl), lskip, jpds, jgds, &
              numpts, message_num, kpds, kgds, lbms, lsmask_mdl, iret)

   if (iret /= 0) then
     print*,'- FATAL ERROR: BAD DEGRIB OF DATA. IRET IS ',iret
     call w3tage('SNOW2MDL')
     call errexit(86)
   end if

   deallocate (lbms)

 elseif (isgrib==2) then ! grib2

   j       = 0      ! search at beginning of file
   jdisc   = 2      ! search for discipline; 2 - land-sfc products
   jpdtn   = -1     ! search for any product definition template number
   jgdtn   = -1     ! search for any grid definition template number
   jids    = -9999  ! array of values in identification section, set to wildcard
   jgdt    = -9999  ! array of values in grid definition template 3.m
   jpdt    = -9999  ! array of values in product definition template 4.n
   jpdt(1) = 0      ! search for parameter category - veg_biomass
   jpdt(2) = 0      ! search for parameter number - landcover
   unpack  = .true. ! unpack data

   call grib2_null(gfld)

   print*,"- DEGRIB DATA"
   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

   if (iret /=0) then
    print*,'- FATAL ERROR: BAD DEGRIB OF FILE, IRET IS ', iret
    call w3tage('SNOW2MDL')
    call errexit(86)
   endif

   allocate(lsmask_mdl(imdl,jmdl))
   lsmask_mdl = reshape (gfld%fld , (/imdl,jmdl/) )

   call grib2_free(gfld)

 endif

 call baclose(iunit,iret)

!-----------------------------------------------------------------------
! global model runs on a thinned grid (# grid points decreases
! towards the poles).  if thinned logical is set, and this is a
! gaussian grid, modify the land/sea mask to account for the
! fact that delta x increases toward the poles.
!-----------------------------------------------------------------------

 thinned=.false.
 if (kgds(1) == 4 .and. (len_trim(gfs_lpl_file) > 0)) then

   thinned=.true.

   print*,"- RUNNING A THINNED GRID"

   allocate (lonsperlat_mdl(jmdl/2))

   print*,"- OPEN/READ GFS LONSPERLAT FILE: ",trim(gfs_lpl_file)
   open (iunit2, file=trim(gfs_lpl_file), iostat=iret)
   if (iret /= 0) then
     print*,'- FATAL ERROR: BAD OPEN OF LONSPERLAT FILE. ABORT. IRET: ', iret
     call w3tage('SNOW2MDL')
     call errexit(76)
   endif

   read (iunit2,*,iostat=iret) numpts, lonsperlat_mdl
   close(iunit2)
   if (iret /= 0) then
     print*,'- FATAL ERROR: BAD READ OF LONSPERLAT FILE. ABORT. IRET: ', iret
     call w3tage('SNOW2MDL')
     call errexit(76)
   endif

   if (numpts /= (jmdl/2)) then
     print*,'- FATAL ERROR: WRONG DIMENSIION IN LONSPERLAT FILE. ABORT.'
     call w3tage('SNOW2MDL')
     call errexit(76)
   endif

   allocate (lsmask_mdl_sav(imdl,jmdl))
   lsmask_mdl_sav = lsmask_mdl
   lsmask_mdl = 0.0   ! this will identify land points to be processed by
                      ! the ipolates routines.

!-----------------------------------------------------------------------
! loop over every point on the thinned grid.  calculate the start/end
! bounds with respect to the full grid in the 'i' direction.  if
! the thinned point contains land, set all full grid points within
! the bounds to be land.  this modified mask will identify the
! points to be processed by ipolates.  after the call to ipolates,
! the thinned points will be set to a linear weighting of the full points
! located within the thinned point.
!-----------------------------------------------------------------------

   do j = 1, jmdl
     jj = j
     if (j > jmdl/2) jj = jmdl - j + 1
     r = float(imdl)/ float(lonsperlat_mdl(jj))
     do i = 1, lonsperlat_mdl(jj)
       x1=float(i-1)*r
       imid = nint(x1+1.0)  ! for this thinned grid point, this is
                            ! the nearest 'i' index on the full grid.
       if (lsmask_mdl_sav(imid,j) > 0.0) then
         gridis = x1+1.0-r/2.
         istart = nint(gridis)
         gridie = x1+1.0+r/2.
         iend   = nint(gridie)
         do ii = istart, iend
           if (ii == istart) then
             fraction = 0.5 - (gridis - float(istart))
             if (fraction < 0.0001) cycle
           endif
           if (ii == iend) then
             fraction = 0.5 + (gridie - float(iend))
             if (fraction < 0.0001) cycle
           endif
           iii = ii
           if (iii < 1) iii = imdl + iii
           lsmask_mdl(iii,j) = lsmask_mdl_sav(imid,j)
         enddo
       endif
     enddo
   enddo
  
 end if

!-----------------------------------------------------------------------
! program only worries about land points.   save i/j coordinate
! with respect to 2-d grid.
!-----------------------------------------------------------------------

 ij = 0

 do j = 1, jmdl
 do i = 1, imdl
   if (lsmask_mdl(i,j) > 0.0) then
     ij = ij+1
   end if
 enddo
 enddo

 ijmdl = ij

 if (ijmdl == 0) then  ! grid has only water points, dont run
   print*,' '
   print*,'- MODEL GRID ONLY HAS WATER POINTS, DONT CREATE SNOW FILE.'
   print*,'- NORMAL TERMINATION.'
   call w3tage('SNOW2MDL')
   call errexit(0)
 endif

 allocate (lats_mdl(ijmdl))
 allocate (lons_mdl(ijmdl))
 allocate (ipts_mdl(ijmdl))
 allocate (jpts_mdl(ijmdl))

 ij = 0
 do j = 1, jmdl
 do i = 1, imdl
   if (lsmask_mdl(i,j) > 0.0) then
     ij = ij+1
     lats_mdl(ij) = lats_mdl_temp(i,j)
     lons_mdl(ij) = lons_mdl_temp(i,j)
     ipts_mdl(ij) = i
     jpts_mdl(ij) = j
   end if
 enddo
 enddo

 deallocate (lats_mdl_temp, lons_mdl_temp)

 return

 end subroutine read_mdl_grid_info

 subroutine model_grid_cleanup
!$$$  subprogram documentation block
!              
! subprogram:    model_grid_cleanup
!   prgmmr: gayno          org: w/np2     date: 2005-dec-16
!
! abstract: this deallocate this module's allocatable array.
!
! program history log:
! 2005-dec-16  gayno    - initial version
!
! usage: call model_grid_cleanup
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files: none
!
! condition codes: none
!
! remarks: none.
!
!$$$

 implicit none

 if (allocated(lsmask_mdl))     deallocate(lsmask_mdl)
 if (allocated(lats_mdl))       deallocate(lats_mdl)
 if (allocated(lons_mdl))       deallocate(lons_mdl)
 if (allocated(lonsperlat_mdl)) deallocate(lonsperlat_mdl)
 if (allocated(ipts_mdl))       deallocate(ipts_mdl)
 if (allocated(jpts_mdl))       deallocate(jpts_mdl)
  
 return

 end subroutine model_grid_cleanup

 end module model_grid
