subroutine read_nasa_larc(nread,ndata,infile,obstype,lunout,twind,sis,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_nasa_larc          Reading in NASA LaRC cloud   
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2009-09-21
!
! ABSTRACT: 
!     This routine reads in NASA LaRC cloud data. The data has already  
!          been interpolated into analysis grid and in form of BUFR.
!
! PROGRAM HISTORY LOG:
!    2009-09-21  Hu  initial
!    2010-04-09  Hu  make changes based on current trunk style
!    2013-03-27  Hu  add code to map obs from WRF mass H grid to analysis grid
!    2015-02-23  Rancic/Thomas - add l4densvar to time window logical
!    2015-03-23  Su  fix array size with maximum message and subset number
!                    from fixed number to dynamic allocated array
!    
!
!
!   input argument list:
!     infile   - unit from which to read NASA LaRC file
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     twind    - input group time window (hours)
!     sis      - observation variable name
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! USAGE:
!   INPUT FILES:  NASALaRCCloudInGSI.bufr
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,r_double,i_kind
  use constants, only: zero,one
  use convinfo, only: nconvtype,ctwind,cgross,cermax,cermin,cvar_b,cvar_pg, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype
  use gsi_4dvar, only: l4dvar,l4densvar,winlen
  use gridmod, only: nlon,nlat,nlon_regional,nlat_regional
  use mod_wrfmass_to_a, only: wrfmass_obs_to_a8
  use mpimod, only: npe

  implicit none
!
  
  character(10),    intent(in)   :: infile,obstype
  integer(i_kind),  intent(in)   :: lunout
  integer(i_kind),  intent(inout):: nread,ndata
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  real(r_kind),     intent(in   ):: twind
  character(20),    intent(in)   :: sis
!
!  For LaRC
!
  integer(i_kind) nreal,nchanl,ilat,ilon

  integer(i_kind) ifn,i
 
  logical :: larcobs

!
!  for read in bufr
!
    real(r_kind) :: hdr(5),obs(1,5)
    character(80):: hdrstr='SID XOB YOB DHR TYP'
    character(80):: obsstr='POB'

    character(8) subset
    integer(i_kind) :: lunin,idate
    integer(i_kind)  :: ireadmg,ireadsb

    integer(i_kind)  ::  maxlvl
    integer(i_kind)  ::  numlvl,numlarc,numobsa,maxobs,nmsgmax
    integer(i_kind)  ::  k,iret
    integer(i_kind)  ::  nmsg,ntb

    real(r_kind),allocatable :: larccld_in(:,:)   ! 3D reflectivity in column

    integer(i_kind)  :: ikx
    real(r_kind)     :: timeo,t4dv


!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
   larcobs = .false.
   ikx=0
   do i=1,nconvtype
       if(trim(obstype) == trim(ioctype(i)) .and. abs(icuse(i))== 1) then
           larcobs =.true.
           ikx=i
       endif
   end do

   nchanl= 0
   nread = 0
   ndata = 0
   ifn = 15
!
   if(larcobs) then
!! get message and subset counts

      call getcount_bufr(infile,nmsgmax,maxobs)

      lunin = 10            
      maxlvl= 5
      allocate(larccld_in(maxlvl+2,maxobs))

      open  ( unit = lunin, file = trim(infile),form='unformatted',err=200)
      call openbf  ( lunin, 'IN', lunin )
      call datelen  ( 10 )

      nmsg=0
      ntb = 0
      msg_report: do while (ireadmg(lunin,subset,idate) == 0)
         nmsg=nmsg+1
         if (nmsg>nmsgmax) then
            write(6,*)'read_nasa_larc: messages exceed maximum ',nmsgmax
            call stop2(50)
         endif
         loop_report: do while (ireadsb(lunin) == 0)
            ntb = ntb+1
            if (ntb>maxobs) then
                write(6,*)'read_nasa_larc: reports exceed maximum ',maxobs
                call stop2(50)
            endif

!    Extract type, date, and location information
            call ufbint(lunin,hdr,5,1,iret,hdrstr)
! check time window in subset
            if (l4dvar.or.l4densvar) then
               t4dv=hdr(4)
               if (t4dv<zero .OR. t4dv>winlen) then
                  write(6,*)'read_nasalarc:      time outside window ',&
                       t4dv,' skip this report'
                  cycle loop_report
               endif
            else
               timeo=hdr(4)
               if (abs(timeo)>ctwind(ikx) .or. abs(timeo) > twind) then
                  write(6,*)'read_nasalarc:  time outside window ',&
                       timeo,' skip this report'
                  cycle loop_report
               endif
            endif

! read in observations
            call ufbint(lunin,obs,1,maxlvl,iret,obsstr)
            numlvl=iret

            larccld_in(1,ntb)=hdr(2)*10.0_r_kind       ! observation location, grid index i
            larccld_in(2,ntb)=hdr(3)*10.0_r_kind       ! observation location, grid index j

            do k=1,numlvl
              larccld_in(2+k,ntb)=obs(1,k)             ! NASA LaRC cloud products: k=1 cloud top pressure
            enddo                                      ! k=2 cloud top temperature, k=3 cloud fraction     
                                                       ! k=4 lwp,  k=5, cloud levels
         enddo loop_report
      enddo msg_report

      write(6,*)'read_nasalarc: messages/reports = ',nmsg,'/',ntb
      numlarc=ntb
!
      ilon=1
      ilat=2
      nread=numlarc
      ndata=numlarc
      nreal=maxlvl+2
      if(numlarc > 0 ) then
          if(nlon==nlon_regional .and. nlat==nlat_regional) then
             call count_obs(numlarc,maxlvl+2,ilat,ilon,larccld_in,nobs)
             write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
             write(lunout) ((larccld_in(k,i),k=1,maxlvl+2),i=1,numlarc)
          else
             call wrfmass_obs_to_a8(larccld_in,nreal,numlarc,ilat,ilon,numobsa)
             nread=numobsa
             ndata=numobsa
             call count_obs(numobsa,maxlvl+2,ilat,ilon,larccld_in,nobs)
             write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
             write(lunout) ((larccld_in(k,i),k=1,maxlvl+2),i=1,numobsa)
          endif
          deallocate(larccld_in)
      endif
    endif
!
    call closbf(lunin)
    return
200 continue
    write(6,*) 'read_nasa_larc, Warning : cannot find LaRC data file'

end subroutine  read_nasa_larc
