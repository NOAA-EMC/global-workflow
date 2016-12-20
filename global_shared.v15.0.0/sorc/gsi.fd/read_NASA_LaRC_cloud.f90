subroutine  read_NASA_LaRC_cloud(nread,ndata,nouse,obstype,lunout,sis,nobs)
!
!   PRGMMR: Shun Liu          ORG: EMC        DATE: 2013-05-14
!
! ABSTRACT: 
!     This routine read in NASA LaRC cloud products based on GSD's cloud processing
!     code 
!
! PROGRAM HISTORY LOG:
!    2014-12-03 derber - remove unused variables
!
!   variable list
!
! USAGE:
!   INPUT FILES:  
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  CCS
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,i_kind,r_single
  use constants, only: zero,deg2rad,rad2deg
  use gridmod, only: regional,nlat,nlon,tll2xy,rlats,rlons
  use mpimod, only: npe

  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: obstype
  character(len=20),intent(in  ) :: sis
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: nread,ndata,nouse
  integer(i_kind) ,dimension(npe),intent(inout) :: nobs
! real(r_kind),dimension(nlat,nlon,nsig),intent(in):: hgtl_full

! Declare local parameters
  integer(i_kind),parameter:: maxdat=8

! Declare local variables
  logical outside    !,good0,lexist1,lexist2,lexist3
  real(r_kind),allocatable,dimension(:,:):: cdata_all
  real(r_kind) dlat_earth,dlon_earth
  real(r_kind) dlat,dlon
  real(r_kind) dlatmax,dlonmax,dlatmin,dlonmin
  real(r_kind) usage

  integer(i_kind) nreal,nchanl,ilat,ilon

  real(r_kind),parameter:: r360=360.0_r_kind


  CHARACTER*80   satfile
  
!     ****VARIABLES FOR THIS NETCDF FILE****
!
  REAL(r_single), allocatable ::   lat_l(:)
  REAL(r_single), allocatable ::   lon_l(:)
  REAL(r_single), allocatable ::   lwp_l(:)
  REAL(r_single), allocatable ::   teff_l(:)
  REAL(r_single), allocatable ::   ptop_l(:)
  integer(i_kind), allocatable ::   phase_l(:)
!
!  array for RR
!
  
  integer :: nfov
  parameter (nfov=650)

  integer i,k

  integer(i_kind) :: east_time, west_time
  integer :: maxobs,numobs

  character*10  atime

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
! set geogrid fle name
!
! allocate (Pxx(nlon,nlat,nfov),Txx(nlon,nlat,nfov),WPxx(nlon,nlat,nfov))
! allocate (xdist(nlon,nlat,nfov), xxxdist(nfov))
! allocate (PHxx(nlon,nlat,nfov),index(nlon,nlat), jndex(nfov))
! index=0
!
!  read in the NASA LaRC cloud data
  write(6,*)'start to read NASA_LaRC_cloud::'

  maxobs=(1800*700 + 1500*850)*1
  allocate(cdata_all(maxdat,maxobs))
  satfile='lgycldbufr'
  allocate(lat_l(maxobs))
  allocate(lon_l(maxobs))
  allocate(ptop_l(maxobs))
  allocate(teff_l(maxobs))
  allocate(phase_l(maxobs))
  allocate(lwp_l(maxobs))
  usage=0
  ptop_l=-9.0_r_kind
  teff_l=-9.0_r_kind
  lat_l =-9.0_r_kind
  lon_l =-9.0_r_kind
  lwp_l =-9.0_r_kind
  phase_l=-9
  call read_NASALaRC_cloud_bufr_survey(satfile,east_time, west_time)
  call read_NASALaRC_cloud_bufr(satfile,atime,east_time, west_time,   &
            maxobs,numobs, ptop_l, teff_l, phase_l, lwp_l,lat_l, lon_l)

!    write(6,*)'LaRC ptop =', (ptop_l(j),j=1,numobs,5000)
!    write(6,*)'LaRC teff =', (teff_l(j),j=1,numobs,5000)
!    write(6,*)'LaRC lat  =', (lat_l(j),j=1,numobs,5000)
!    write(6,*)'LaRC lon  =', (lon_l(j),j=1,numobs,5000)
!    write(6,*)'LaRC lwp  =', (lwp_l(j),j=1,numobs,5000)
!    write(6,*)'LaRC phase  =', (phase_l(j),j=1,numobs,5000)

   do i=1,numobs
     dlat_earth=lat_l(i)    !station lat (degrees)
     dlon_earth=lon_l(i)    !station lon (degrees)
     if (dlon_earth>=r360) dlon_earth=dlon_earth-r360
     if (dlon_earth<zero ) dlon_earth=dlon_earth+r360
     dlat_earth = dlat_earth * deg2rad
     dlon_earth = dlon_earth * deg2rad

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if (outside) cycle
        dlatmax=max(dlat,dlatmax)
        dlonmax=max(dlon,dlonmax)
        dlatmin=min(dlat,dlatmin)
        dlonmin=min(dlon,dlonmin)
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif

        if (phase_l(i).eq.4) phase_l(i) = 0   ! clear
        if (phase_l(i).eq.5) phase_l(i) = -9  ! bad data equivalent to "no data"
!       if (phase_l(i).eq.-9) ptop_l(i) = -9.0_r_kind
        if (phase_l(i).eq.0) ptop_l(i) = -20.0_r_kind
!    write(6,*)'dlat, dlon::',dlat,dlon

        cdata_all(1,i)=0.0_r_kind   !  time
        cdata_all(2,i)=dlon               !  grid relative longitude
        cdata_all(3,i)=dlat               !  grid relative latitude
        cdata_all(4,i)=ptop_l(i)          !  cloud top pressure (pa)
        cdata_all(5,i)=teff_l(i)          !  Cloud top temperature (K)
        cdata_all(6,i)=phase_l(i)         !  cloud cover
        cdata_all(7,i)=lwp_l(i)          
        cdata_all(8,i)=usage

   enddo

   ilon=2
   ilat=3
   nchanl=0
   nreal=maxdat
   nread=numobs
   ndata=numobs
   nouse=0
   call count_obs(numobs,maxdat,ilat,ilon,cdata_all,nobs)
   write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
   write(lunout) ((cdata_all(k,i),k=1,maxdat),i=1,numobs)
   write(6,*)'NASA larccld::',nreal,numobs

   return
end subroutine read_NASA_LaRC_cloud

subroutine read_NASALaRC_cloud_bufr(satfile,atime,east_time, west_time, &
             maxobs,numobs,ptop, teff, phase, lwp_iwp,lat, lon)
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2010-07-09
!
! ABSTRACT: 
!     This routine read in NASA LaRC cloud products 
!     from a bufr file                      
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,i_kind,r_double

  implicit none
!
!
!
  character(80):: hdstr='YEAR  MNTH  DAYS HOUR  MINU  SECO'
  character(80):: obstr='CLATH  CLONH CLDP HOCT CDTP EBBTH VILWC' 
! CLDP     |  CLOUD PHASE
! HOCB     | HEIGHT OF BASE OF CLOUD
! HOCT     | HEIGHT OF TOP OF CLOUD             (METERS)
! CDBP     | PRESSURE AT BASE OF CLOUD
! CDTP     | PRESSURE AT TOP OF CLOUD           (PA)
! EBBTH    | EQUIVALENT BLACK BODY TEMPERATURE  (KELVIN)
! VILWC    | VERTICALLY-INTEGRATED LIQUID WATER CONTENT

  real(8) :: hdr(6),obs(7,1)

  INTEGER(i_kind) :: ireadmg,ireadsb

  character(8) subset
  integer :: unit_in=10,idate,iret,nmsg,ntb

!
!  For NASA LaRC 
!
  CHARACTER*40   satfile
  integer(i_kind) :: east_time, west_time

  INTEGER ::   maxobs, numobs  ! dimension
  INTEGER(i_kind) ::  obs_time
  REAL*4      lat                            (  maxobs)
  REAL*4      lon                            (  maxobs)
  integer     phase                          (  maxobs)
  REAL*4      lwp_iwp                        (  maxobs)
  REAL*4      teff                           (  maxobs)
  REAL*4      ptop                           (  maxobs)
!
!
!  ** misc
      
!  integer i,j,k
!
!  integer :: status
  character*10  atime
!
!**********************************************************************
!
 open(24,file='NASA.bufrtable')
 open(unit_in,file=trim(satfile),form='unformatted')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,24)
 call datelen(10)
   nmsg=0
   ntb = 0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     nmsg=nmsg+1
     sb_report: do while (ireadsb(unit_in) == 0)
       call ufbint(unit_in,hdr,6,1,iret,hdstr)
       obs_time=int((hdr(1)-2000.0_r_kind)*100000000+hdr(2)*1000000+hdr(3)*10000+hdr(4)*100+hdr(5))
       call ufbint(unit_in,obs,7,1,iret,obstr)
       if(obs_time == east_time .or. obs_time == west_time ) then
       if(obs(5,1) < 1.e7 .and. obs(5,1) > 100.0_r_kind ) then
       if(obs(6,1) < 1.e7 .and. obs(6,1) > 10.0_r_kind) then
         ntb = ntb+1
         if(ntb > maxobs) then
           write(*,*) 'Error: need to increase maxobs',maxobs, ntb
           stop 1234
         endif
         lat(ntb)=obs(1,1)
         lon(ntb)=obs(2,1)
         phase(ntb)=int(obs(3,1))
         lwp_iwp(ntb)=obs(7,1)
         teff(ntb)=obs(6,1)
         ptop(ntb)=obs(5,1)/100.0_r_kind ! pa to hpa
       endif
       endif
       endif   ! east_time, west_time
     enddo sb_report
   enddo msg_report
   write(*,*) 'message/reports num=',nmsg,ntb
 call closbf(unit_in)
 numobs=ntb
 write(atime,'(I10)') idate

end subroutine read_NASALaRC_cloud_bufr

subroutine read_NASALaRC_cloud_bufr_survey(satfile,east_time, west_time)
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2010-07-09
!
! ABSTRACT: 
!     This routine read in NASA LaRC cloud products 
!     from a bufr file                      
!
! PROGRAM HISTORY LOG:
!   2015-05-12  s. liu  - increase max_obstime from 10 to 20 
!
!   variable list
!
! USAGE:
!   INPUT FILES:  
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,i_kind,r_double

  implicit none
!
!
!
  character(80):: hdstr='YEAR  MNTH  DAYS HOUR  MINU  SECO'
  real(8) :: hdr(6)

  integer(i_kind) :: ireadmg,ireadsb

  character(8) subset
  integer(i_kind) :: unit_in=10,idate,iret,nmsg,ntb

!
!  For NASA LaRC 
!
  CHARACTER*40, intent(in)    ::   satfile
  integer(i_kind),intent(out) :: east_time, west_time 

  INTEGER(i_kind) ::  obs_time

  INTEGER(i_kind),parameter :: max_obstime=20
  integer(i_kind) :: num_obstime_all(max_obstime)  
  integer(i_kind) :: num_subset_all(max_obstime) 
  integer(i_kind) :: num_obstime_hh(max_obstime) 
  integer(i_kind) :: num_obstime 

!
  integer :: i,ii,hhh
!
!**********************************************************************
!
 num_obstime=0
 hhh=99
 open(24,file='NASA.bufrtable')
 open(unit_in,file=trim(satfile),form='unformatted')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,24)
 call datelen(10)
   nmsg=0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     ntb = 0
     nmsg=nmsg+1
     sb_report: do while (ireadsb(unit_in) == 0)
       call ufbint(unit_in,hdr,6,1,iret,hdstr)
       obs_time=int((hdr(1)-2000.0_r_kind)*100000000+hdr(2)*1000000+hdr(3)*10000+hdr(4)*100+hdr(5))
       hhh=int(hdr(5))
       ntb=ntb+1
     enddo sb_report
! message inventory
     if(num_obstime == 0 ) then
       num_obstime=1
       num_obstime_all(num_obstime)=obs_time
       num_obstime_hh(num_obstime)=hhh
       num_subset_all(num_obstime)= ntb
     else
       ii=0
       DO i=1,num_obstime
          if(num_obstime_all(i) == obs_time ) ii=i
       ENDDO
       if( ii > 0 .and. ii <=num_obstime) then
          num_subset_all(ii)=num_subset_all(ii) + ntb
       else
          num_obstime=num_obstime+1
          if(num_obstime> max_obstime) then
             write(*,*) 'Error: too many message types'
             write(*,*) 'Need to increase :max_obstime'
             stop 1234
          endif
          num_obstime_all(num_obstime)=obs_time
          num_obstime_hh(num_obstime)=hhh
          num_subset_all(num_obstime)=num_subset_all(num_obstime)+ntb
       endif
     endif
   enddo msg_report
   write(*,*) 'message/reports num=',nmsg,ntb
 call closbf(unit_in)

 write(*,'(2x,a10,a10,a11)') 'time_level','subset_num'
 DO i=1,num_obstime
   write(*,'(i2,i12,i11,i10)') i,num_obstime_all(i),num_subset_all(i),num_obstime_hh(i)
 ENDDO
!  GOES EAST  : 1815, 1845, 1915, 2045
!  GOES WEST  : 1830, 1900, 2030
 east_time=0
 west_time=0 
 DO i=1,num_obstime
   if(num_subset_all(i) > 10) then
      if(num_obstime_hh(i) == 15 .or. num_obstime_hh(i) == 45 ) then
         if(east_time < num_obstime_all(i)) east_time=num_obstime_all(i)
      endif
      if(num_obstime_hh(i) == 30 .or. num_obstime_hh(i) == 0 ) then
         if(west_time < num_obstime_all(i)) west_time=num_obstime_all(i)
      endif
   endif
 ENDDO
 write(*,*) 'east_time=',east_time
 write(*,*) 'west_time=',west_time
end subroutine read_NASALaRC_cloud_bufr_survey
