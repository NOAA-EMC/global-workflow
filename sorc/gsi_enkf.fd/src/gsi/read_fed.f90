subroutine read_fed(nread,ndata,nodata,infile,obstype,lunout,twind,sis,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! ABSTRACT: 
!     This routine reads in netcdf or prepbufr flash-extent density (FED) data. 
!
! PROGRAM HISTORY LOG:
!    2018-07-25  Rong Kong (CAPS/OU) - modified based on read_radarref_mosaic.f90  
!    2019-09-20  Yaping Wang (CIMMS/OU)
!    2021-07-01  David Dowell (DCD; NOAA GSL) - added maximum flashes/min for observed FED
!
!    2023-10-18  Hongli Wang (NOAA GSL) 
!                   - cleanup code, removed hardcoded obs height (6500m)
!                   - use height fron obs file if they are avaiable, otherwise
!                       use default value or value from namelist variable r_hgt_fed 
!                   - return if NetCDF file open status /= nf90_noerror
!
!   input argument list:
!     infile   - unit from which to read observation information file
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
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,r_double,i_kind
  use constants, only: zero,one,deg2rad,r60inv
  use convinfo, only: nconvtype,icuse,ioctype
  use gsi_4dvar, only: iwinbgn
  use gridmod, only: tll2xy
  use mod_wrfmass_to_a, only: wrfmass_obs_to_a8
  use mpimod, only: npe
  use obsmod, only: perturb_obs,iadatemn,dofedoneob,oneoblat,oneoblon,r_hgt_fed

  use netcdf
  implicit none

  include 'netcdf.inc'
!
  character(len=*), intent(in)    :: infile,obstype
  integer(i_kind),  intent(in)    :: lunout
  integer(i_kind),  intent(inout) :: nread,ndata
  integer(i_kind),  intent(inout) :: nodata
  integer(i_kind),  dimension(npe) ,intent(inout) :: nobs
  real(r_kind),     intent(in   ) :: twind
  character(len=*), intent(in)    :: sis

! Declare local parameters
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: oe_fed = 1.0_r_kind
  real(r_kind),parameter:: fed_lowbnd = 0.1_r_kind    ! use fed == fed_lowbnd
  real(r_kind),parameter:: fed_lowbnd2 = 0.1_r_kind   ! use fed >= fed_lowbnd2
!  real(r_kind),parameter:: fed_highbnd = 18.0_r_kind  ! 18 flashes/min from Sebok and Back (2021, unpublished)
  real(r_kind),parameter:: fed_highbnd = 8.0_r_kind  ! 8 flashes/min from Back (2023) for regional FV3 tests

!
!  For fed observations
!
  integer(i_kind) nreal,nchanl

  integer(i_kind) ifn,i
 
  integer(i_kind) :: ilon,ilat

  logical :: fedobs, fedob
  real(r_kind),allocatable,dimension(:,:):: cdata_out
  real(r_kind) :: federr, thiserr
  real(r_kind) :: hgt_fed(1)

  real(r_kind) :: i_maxloc,j_maxloc,k_maxloc
  integer(i_kind) :: kint_maxloc
  real(r_kind) :: fed_max
  integer(i_kind) :: ndata2

  character(8) station_id
  real(r_double) :: rstation_id
  equivalence(rstation_id,station_id)

  integer(i_kind)  ::  maxlvl
  integer(i_kind)  ::  numfed,maxobs
  integer(i_kind)  ::  k

  real(r_kind),allocatable,dimension(:,:) :: fed3d_column  ! 3D fed in column
  real(r_kind),allocatable,dimension(:)   :: fed3d_hgt     ! fed height 
  real(r_kind),allocatable,dimension(:)   :: utime         ! time 

  integer(i_kind)  :: ikx

  character*128   :: myname='read_fed'

  real(r_kind) :: dlat, dlon              ! rotated corrdinate
  real(r_kind) :: dlat_earth, dlon_earth  ! in unit of degree
  real(r_kind) :: rlat00, rlon00          ! in unit of rad

  logical      :: l_psot_fed
  logical      :: l_latlon_fedobs
  logical      :: outside

!  for read netcdf   
  integer(i_kind)    :: sec70,mins_an
  integer(i_kind)    :: varID, ncdfID, status
  real(r_kind)       :: timeb,twindm,rmins_an,rmins_ob


    hgt_fed = r_hgt_fed 

    write(6,*) "r_kind=",r_kind
    l_psot_fed = .FALSE.
    l_latlon_fedobs = .TRUE.

    fedob = obstype == 'fed'
    if (fedob) then
       nreal=25
    else 
       write(6,*) ' illegal obs type in read_fed : obstype=',obstype
       call stop2(94)
    end if
    if(perturb_obs .and. fedob)nreal=nreal+1
    write(6,*)'read_fed: nreal=',nreal

    fedobs = .false.
    ikx=0
    do i=1,nconvtype
       if(trim(obstype) == trim(ioctype(i)) .and. abs(icuse(i))== 1) then
           fedobs=.true.
           ikx=i
           federr  = oe_fed          ! Obs error (flashes per minute)
           thiserr = federr
           exit                   ! Exit loop when finished with initial convinfo fields     
       else if (i == nconvtype ) then
          write(6,*) 'read_fed: Obs Type for fed is not in CONVINFO !'
          write(6,*) 'read_fed: PLEASE modify the CONVINFO file !'
          write(6,*) 'read_fed: abort read_fed !'
          return
       endif
    end do
    write(6,'(1x,A,A30,I4,A15,F7.3,A7)') &
       trim(myname),': fed in convinfo-->ikx=',ikx,' fed ob err:',thiserr," (fed)"

    nread=0
    ndata=0
    nchanl=0
    ifn = 15

   if (fedobs) then
       maxlvl= 1                          ! fed only has one level  
  
!!!! Start reading fed observations from NETCDF format data file 
      ! CHECK IF DATA FILE EXISTS
      ! OPEN NETCDF FILE
      status = nf90_open(TRIM(infile), NF90_NOWRITE, ncdfID)
      print*, '*** OPENING GOES FED OBS  NETCDF FILE: ', infile, status
      if(status/=nf90_noerr)return

      !------------------------
      ! Get Dimension Info (1-D)
      !-------------------------
      status = nf90_inq_varid( ncdfID, 'numobs', varID )
      status = nf90_get_var( ncdfID, varID, maxobs )

      !------------------------
      ! Allocate data arrays
      !-------------------------
      ALLOCATE( fed3d_column( 5, maxobs ) )
      allocate( fed3d_hgt(maxobs) )
      ALLOCATE( utime( 1 ) )  ! seconds since from 2000-01-01 12:00
      fed3d_hgt = -999.0_r_kind 

      !------------------------
      ! Get useful data arrays
      !-------------------------
      ! LON
      status = nf90_inq_varid( ncdfID, 'lon', varID )
      status = nf90_get_var( ncdfID, varID, fed3d_column(1, :) )
      ! LAT
      status = nf90_inq_varid( ncdfID, 'lat', varID )
      status = nf90_get_var( ncdfID, varID, fed3d_column(2, :) )
      ! FED value
      status = nf90_inq_varid( ncdfID, 'value', varID )
      status = nf90_get_var( ncdfID, varID, fed3d_column(3, :) )
      ! TIME
      status = nf90_inq_varid( ncdfID, 'time', varID )
      status = nf90_get_var( ncdfID, varID, utime )

      ! FED height, optional variable 
      status = nf90_inq_varid( ncdfID, 'height', varID )
      if(status==nf90_noerr)&
      status = nf90_get_var( ncdfID, varID, fed3d_hgt )

      ! CLOSE NETCDF FILE
      status = nf90_close( ncdfID )


      !-Obtain analysis time in minutes since reference date
      sec70 = 694267200.0  ! seconds since from 1978-01-01 00:00 to 2000-01-01 12:00    
                           ! because the official GOES prescribed epoch time for GLM data is 2000-01-01 12:00:00

      call w3fs21(iadatemn,mins_an)  !mins_an -integer number of mins snce 01/01/1978
      rmins_an=mins_an             !convert to real number

      ! SINCE ALL OBS WILL HAVE THE SAME TIME, CHECK TIME HERE:
      rmins_ob = ( utime(1) + sec70 )/60   !Convert to Minutes from seconds
      twindm = twind*60.    !Convert to Minutes from hours
      timeb = rmins_ob-rmins_an

      if(abs(timeb) > abs(twindm)) then
        print*, 'WARNING: ALL FED OBSERVATIONS OUTSIDE ASSIMILATION TIME WINDOW: ', timeb, twindm
      endif

      !time relative to the beginning of the da time window
      timeb=real(rmins_ob-iwinbgn,r_kind)

      numfed = maxobs
      do i=1,numfed
        if (fed3d_column( 3, i ) >= fed_lowbnd2 .or. fed3d_column( 3, i ) == fed_lowbnd ) then 
          ndata = ndata + 1
        end if
      end do

      write(6,*)'read_fed: total no. of obs = ',ndata
      nread=ndata
      nodata=ndata
!!! - Finished reading fed observations from NETCDF format data file

      allocate(cdata_out(nreal,ndata))
!
      do i=1,numfed
          do k=1,maxlvl
              if (fed3d_column(k+2,i) .gt. fed_highbnd) fed3d_column(k+2,i) = fed_highbnd 
          end do
      end do

      write(6,*) ' ------- check max and min value of OBS: bufr fed -------'
      write(6,*) ' level      maxval(fed)       minval(fed)'
      do k=1,maxlvl
          write(6,*) k,maxval(fed3d_column(k+2,:)),minval(fed3d_column(k+2,:))
      end do

      i_maxloc=-1.0
      j_maxloc=-1.0
      k_maxloc=-1.0
      kint_maxloc=-1
      fed_max=-999.99
      ndata2=0 

      ILOOP : &
      do i=1,numfed
        if(fed3d_hgt(i) > 0.0_r_kind)then
           hgt_fed=fed3d_hgt(i)
        else
           hgt_fed = r_hgt_fed
        end if
        do k=1,maxlvl
          if( fed3d_column(k+2,i) >= fed_lowbnd2 .or. fed3d_column(k+2,i) == fed_lowbnd .or. dofedoneob) then !Rong Kong
            dlon_earth = fed3d_column(1,i)                ! longitude (degrees) of observation
                                                       ! ilone=18    ! index of longitude (degrees)
            dlat_earth = fed3d_column(2,i)                ! latitude (degrees) of observation
                                                       ! ilate=19    ! index of latitude (degrees)

            if (dofedoneob) then
              dlat_earth=oneoblat
              dlon_earth=oneoblon
            endif

            !-Check format of longitude and correct if necessary
            if(dlon_earth>=r360) dlon_earth=dlon_earth-r360
            if(dlon_earth<zero ) dlon_earth=dlon_earth+r360
            if(dlon_earth>=r360 .or. dlat_earth >90.0_r_kind) cycle

            !-Convert back to radians                         
            rlon00 = dlon_earth*deg2rad
            rlat00 = dlat_earth*deg2rad
            call tll2xy(rlon00,rlat00,dlon,dlat,outside)
            if (dofedoneob) then
              if (outside) then
                write(6,*)'READ_FED: ONE OB OUTSIDE; STOP2(61) ',dlat_earth,dlon_earth
                call stop2(61)
              end if
            end if
            if (outside) cycle

            !If observation is outside the domain
            ! then cycle, but don't increase
            ! range right away.
            ! Domain could be rectangular, so ob
            ! may be out of
            ! range at one end, but not the
            ! other.   

            ndata2=ndata2+1
            cdata_out( 1,ndata2) = thiserr             ! obs error (flashes/min) - inflated/adjusted
                                                        
            cdata_out( 2,ndata2) = dlon                ! 
                                                       
            cdata_out( 3,ndata2) = dlat
                                                       
            cdata_out( 4,ndata2) = hgt_fed(k)          ! obs absolute height (m) above MSL
                                                       ! ipres=4     ! index of pressure
            cdata_out( 5,ndata2) = fed3d_column(k+2,i) ! FED value

            cdata_out( 6,ndata2) = rstation_id         ! station id (charstring equivalent to real double)
                                                       ! id=6        ! index of station id

            cdata_out( 7,ndata2) = timeb*r60inv        ! observation time in data array
                                                       ! itime=7     ! index of observation time in data array
            cdata_out( 8,ndata2) = ikx                 ! ob type
                                                       ! ikxx=8      ! index of ob type
            cdata_out( 9,ndata2) = thiserr*2.0_r_kind  ! max error
                                                       ! iqmax=9     ! index of max error
            cdata_out(10,ndata2) = 273.0_r_kind        ! dry temperature
                                                       ! itemp=10    ! index of dry temperature
            cdata_out(11,ndata2) = 1.0_r_kind          ! quality mark
                                                       ! iqc=11      ! index of quality mark
            cdata_out(12,ndata2) = thiserr             ! original-original obs error ratio 
                                                       ! ier2=12    ! index of original-original obs error ratio
            cdata_out(13,ndata2) = icuse(ikx)          ! index of use parameter
                                                       ! iuse=13     ! index of use parameter
            cdata_out(14,ndata2) = icuse(ikx)          ! dominant surface type
                                                       ! idomsfc=14  ! index of dominant surface type
            cdata_out(15,ndata2) = 273.0_r_kind        ! index of surface skin temperature
                                                       ! iskint=15   ! index of surface skin temperature
            cdata_out(16,ndata2) = 0.5_r_kind          ! 10 meter wind factor
                                                       ! iff10=16    ! index of 10 meter wind factor
            cdata_out(17,ndata2) = 0.5_r_kind          ! surface roughness
                                                       ! isfcr=17    ! index of surface roughness

            cdata_out(18,ndata2) = dlon_earth          ! longitude (degrees)

            cdata_out(19,ndata2) = dlat_earth          ! latitude (degrees)

            cdata_out(20,ndata2) = hgt_fed(k)          ! station elevation (m)
                                                       ! istnelv=20  ! index of station elevation (m)
            cdata_out(21,ndata2) = hgt_fed(k)          ! observation height (m)
                                                       ! iobshgt=21  ! index of observation height (m)
            cdata_out(22,ndata2) = hgt_fed(k)          ! surface height
                                                       ! izz=22      ! index of surface height
            cdata_out(23,ndata2) = fed3d_column(4,i)   ! i index of obs grid for bufr resolution (i.e.,8km)  

            cdata_out(24,ndata2) = fed3d_column(5,i)   ! j index of obs grid for bufr resolution

            cdata_out(25,ndata2) = hgt_fed(k)          ! data level category
                                                       ! icat =25    ! index of data level category
            if(perturb_obs .and. fedob)then
               cdata_out(26,ndata2) = 1.0_r_kind       ! obs perturbation
                                                       ! iptrb=26    ! index of q perturbation
            end if

            if( dofedoneob ) exit ILOOP

            if(fed3d_column(k+2,i) > fed_max)then
               kint_maxloc=k
               k_maxloc=real(k,r_kind)
               j_maxloc=fed3d_column(2,i)
               i_maxloc=fed3d_column(1,i)
               fed_max =fed3d_column(k+2,i)
            end if

          endif
        enddo ! k 
      enddo ILOOP ! i

!---all looping done now print diagnostic output
      write(6,*)'READ_FED: Reached eof on FED file'
      write(6,*)'READ_FED: # read in obs. number               =',nread
      write(6,*)'READ_FED: # read in obs. number for further processing  =',ndata2
!  write(6,*)'READ_FED: dlon_earth', cdata_out(18,10:15)

      ilon=2         ! array index for longitude
      ilat=3         ! array index for latitude  in obs information array
      ndata=ndata2
      nodata=ndata2

!---Write observations to scratch file---!

      call count_obs(ndata,nreal,ilat,ilon,cdata_out,nobs)
      write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,ndata
      write(lunout) ((cdata_out(k,i),k=1,nreal),i=1,ndata)

      deallocate(cdata_out)
      if (allocated(fed3d_column)) deallocate(fed3d_column)

      write(6,'(1x,A,F12.5,1x,A,3(1x,F8.3),1x,I4)') &
          'read_fed: max fed =',fed_max, '@ i j k =', &
          i_maxloc,j_maxloc,k_maxloc,kint_maxloc

   end if
   return

end subroutine read_fed
!
!
