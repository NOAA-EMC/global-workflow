subroutine read_radarref_directDA(nread,ndata,nodata,infile,obstype,lunout,twind,sis,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_radarref_directDA    Reading in reflectivity mosaic in RR grid
!
!   PRGMMR: Ming Hu          ORG: NP22        DATE: 2006-03-27
!
! ABSTRACT: 
!     This routine read in reflectivity mosaic data.  The data has already
!          been interpolated into analysis grid and in form of BUFR.
!
! PROGRAM HISTORY LOG:
!    2008-12-20  Hu  make it read in BUFR form reflectivity  data
!    2010-04-09  Hu  make changes based on current trunk style
!    2013-03-27  Hu  add code to map obs from WRF mass H grid to analysis grid
!    2015-02-23  Rancic/Thomas - add l4densvar to time window logical
!    2015-03-23  Su  -fix array size with maximum message and subset number from
!                        fixed number to dynamic allocated array
!    2016-09-xx  CAPS(G. Zhao) 
!                     - modified Ming Hu's read_radarref_mosaic for 
!                     - reading mosaic ref dbz for GSIv3.5 variational and
!                     - enkf analysis, not for gsd-cloud-analysis
!    2016-11-xx  CAPS(G. Zhao)
!                     - fix the bugs:
!                     - dbz bufr with scale=2 to have two more digits accuracy
!                     - station id
!                     - obs lat/lon in degree
!
!   input argument list:
!     infile   - unit from which to read mosaic information file
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
!   INPUT FILES:  refInGSI
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  Linux cluster(Wjet)
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,r_double,i_kind
  use constants, only: zero,one,rad2deg,deg2rad
  use convinfo, only: nconvtype,ctwind,icuse,ioctype
  use gsi_4dvar, only: l4dvar,l4densvar,winlen
  use gridmod, only: nlon,nlat,nlon_regional,nlat_regional,txy2ll,tll2xy
  use mod_wrfmass_to_a, only: wrfmass_obs_to_a8
  use mpimod, only: npe
  use obsmod, only: perturb_obs
  use directDA_radaruse_mod, only: oe_dbz, refl_lowbnd_dbz

  implicit none
!

  character(10),    intent(in)    :: infile,obstype
  integer(i_kind),  intent(in)    :: lunout
  integer(i_kind),  intent(inout) :: nread,ndata
  integer(i_kind),  intent(inout) :: nodata
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  real(r_kind),     intent(in   ) :: twind
  character(20),    intent(in)    :: sis

! Declare local parameters
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind

!
!  For reflectiivty mosaic
!
  integer(i_kind) nreal,nchanl

  integer(i_kind) ifn,i
 
  integer(i_kind) :: ilon,ilat

  logical :: nsslrefobs
  logical dbzob
  real(r_kind),allocatable,dimension(:,:):: cdata_out
  real(r_kind) :: dbzerr, thiserr
  real(r_kind) :: hgt_ref(33)
  data hgt_ref /  500.0_r_kind,   750.0_r_kind,  1000.0_r_kind,  1250.0_r_kind, &
                 1500.0_r_kind,  1750.0_r_kind,  2000.0_r_kind,  2250.0_r_kind, &
                 2500.0_r_kind,  2750.0_r_kind,  3000.0_r_kind,  3500.0_r_kind, &
                 4000.0_r_kind,  4500.0_r_kind,  5000.0_r_kind,  5500.0_r_kind, &
                 6000.0_r_kind,  6500.0_r_kind,  7000.0_r_kind,  7500.0_r_kind, &
                 8000.0_r_kind,  8500.0_r_kind,  9000.0_r_kind, 10000.0_r_kind, &
                11000.0_r_kind, 12000.0_r_kind, 13000.0_r_kind, 14000.0_r_kind, &
                15000.0_r_kind, 16000.0_r_kind, 17000.0_r_kind, 18000.0_r_kind, &
                19000.0_r_kind/

  real(r_kind) :: i_maxloc,j_maxloc,k_maxloc
  integer(i_kind) :: kint_maxloc
  real(r_kind) :: dbz_max
  integer(i_kind) :: ndata2

!
! for read in bufr 
!
  real(r_kind) :: hdr(5),obs(1,35)
  character(80):: hdrstr='SID XOB YOB DHR TYP'
  character(80):: obsstr='HREF'

  character(8) subset
  character(8) station_id
  real(r_double) :: rstation_id
  equivalence(rstation_id,station_id)
  integer(i_kind)  :: lunin,idate
  integer(i_kind)  :: ireadmg,ireadsb

  integer(i_kind)  ::  maxlvl
  integer(i_kind)  ::  numlvl,numref,nmsgmax,maxobs
  integer(i_kind)  ::  k,iret
  integer(i_kind)  ::  nmsg,ntb

  real(r_kind),allocatable,dimension(:,:) :: ref3d_column  ! 3D reflectivity in column

  integer(i_kind)  :: ikx
  real(r_kind)     :: timeo,t4dv

  character*128   :: myname='read_radarref_directDA'

  real(r_kind) :: dlat, dlon              ! rotated corrdinate
  real(r_kind) :: dlat_earth, dlon_earth  ! in unit of degree
  real(r_kind) :: rlat00, rlon00          ! in unit of rad

  logical      :: l_psot_dbz
  logical      :: l_latlon_dbzobs
  logical      :: outside

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!

    l_psot_dbz = .false.
    l_latlon_dbzobs = .true.

   dbzob = obstype == 'dbz'
   if(dbzob) then
      nreal=25
   else 
      write(6,*) ' illegal obs type in read_RadarRef_directDA : obstype=',obstype
      call stop2(94)
   end if
  if(perturb_obs .and. dbzob)nreal=nreal+1
  write(6,*)'read_RadarRef_directDA: nreal=',nreal

   nsslrefobs = .false.
   ikx=0
   do i=1,nconvtype
       if(trim(obstype) == trim(ioctype(i)) .and. abs(icuse(i))== 1) then
           nsslrefobs=.true.
           ikx=i
           dbzerr  = oe_dbz          ! Obs error (dB) for dBZ
           thiserr = dbzerr
           exit                   ! Exit loop when finished with initial convinfo fields     
       else if (i == nconvtype ) then
          write(6,*) 'read_RadarRef_directDA: Obs Type for dBZ is not in CONVINFO !'
          write(6,*) 'read_RadarRef_directDA: PLEASE modify the CONVINFO file !'
          write(6,*) 'read_RadarRef_directDA: abort read_RadarRef_directDA !'
          return
       endif
   end do
   write(6,'(1x,A,A30,I4,A15,F7.3,A7)') &
       trim(myname),': dbz in convinfo-->ikx=',ikx,' dbz ob err:',thiserr," (dbz)"

   nread=0
   ndata=0
   nchanl=0
   ifn = 15

   if(nsslrefobs) then
 !! get message and subset counts

       ! nmsgmax and maxobs are read in from BUFR data file, not pre-set.
      call getcount_bufr(infile,nmsgmax,maxobs)
      write(6,*)'read_radarref_directDA: nmsgmax=',nmsgmax,'    maxobs=',maxobs

!     read in ref obs in bufr code format
      lunin = 10            
      maxlvl= 33                          ! 33 levels (not old 31 levels)
      allocate(ref3d_column(maxlvl+2,maxobs))

      open  ( unit = lunin, file = trim(infile),form='unformatted',err=200)
      call openbf  ( lunin, 'IN', lunin )
      call datelen  ( 10 )

      nmsg=0
      ntb = 0

      ndata =0

      msg_report: do while (ireadmg(lunin,subset,idate) == 0)
         nmsg=nmsg+1
         if (nmsg>nmsgmax) then
            write(6,*)'read_radarref_directDA: messages exceed maximum ',nmsgmax
            call stop2(50)
         endif
         loop_report: do while (ireadsb(lunin) == 0)
            ntb = ntb+1
            if (ntb>maxobs) then
                write(6,*)'read_radarref_directDA: reports exceed maximum ',maxobs
                call stop2(50)
            endif

!    Extract type, date, and location information
            call ufbint(lunin,hdr,5,1,iret,hdrstr)
            if ( l_latlon_dbzobs ) then
	    	if(abs(hdr(3))>r90 .or. abs(hdr(2))>r360) cycle loop_report
           	if(hdr(2)== r360)hdr(2)=hdr(2)-r360
            	if(hdr(2) < zero)hdr(2)=hdr(2)+r360
            end if

! check time window in subset
            if (l4dvar.or.l4densvar) then
               t4dv=hdr(4)
               if (t4dv<zero .OR. t4dv>winlen) then
                  write(6,*)'read_radarref_directDA:      time outside window ',&
                       t4dv,' skip this report'
                  cycle loop_report
               endif
            else
               timeo=hdr(4)
               if (abs(timeo)>ctwind(ikx) .or. abs(timeo) > twind) then
                  write(6,*)'read_radarref_directDA:  time outside window ',&
                       timeo,' skip this report'
                  cycle loop_report
               endif
            endif
! read in observations
            call ufbint(lunin,obs,1,35,iret,obsstr)
            numlvl=min(iret,maxlvl)
            if (numlvl /= maxlvl) then
                write(6,*)' read_radarref_directDA: numlvl is not equalt to maxlvl:',numlvl,maxlvl
            end if

            if ( l_latlon_dbzobs ) then
                ref3d_column(1,ntb)=hdr(2)                   ! observation location,  earth lon
                ref3d_column(2,ntb)=hdr(3)                   ! observation location,  earth lat  
            else
                ref3d_column(1,ntb)=hdr(2)*10.0_r_kind       ! observation location, grid index i
                ref3d_column(2,ntb)=hdr(3)*10.0_r_kind       ! observation location, grid index j
            end if

            if (l_psot_dbz .and. .NOT. l_latlon_dbzobs ) then
                do k=1,numlvl
                    if (nint(ref3d_column(1,ntb)) == 175 .and. nint(ref3d_column(2,ntb)) == 105 .and.  &
                        nint(hgt_ref(k)) >= 100 ) then
                        write(6,*) 'read_radarref_directDA: single point/column obs run on grid: 175 105'
                        write(6,*) 'read_radarref_directDA: found the pseudo single(column) dbz obs:', &
                                    ref3d_column(1:2,ntb),hgt_ref(k)
                    else
                        obs(1,k) = -999.0_r_kind       ! reflectivity (column 33 levels)
                    end if
                end do
            end if

            do k=1,numlvl
                ref3d_column(2+k,ntb)=obs(1,k)             ! reflectivity (column 33 levels)
!               only use dbz obs > refl_lowbnd_dbz
                if (obs(1,k)>=refl_lowbnd_dbz) then
                    ndata = ndata + 1
                end if
            enddo

         enddo loop_report
      enddo msg_report

      write(6,*)'read_radarref_directDA: messages/reports = ',nmsg,'/',ntb
      ! numref is not obs number, it is obs report for each vertical profile
      numref=ntb
      write(6,*)'read_RadarRef_directDA: total No. of dBZ>0.0 = ',ndata
      nread=ndata
      nodata=ndata

!
! - Finished reading radar reflectivity mosaic observations from BUFR format data file
!

      if (ndata > 0 ) then
          allocate(cdata_out(nreal,ndata))
      end if
!
!  convert BUFR value of missing (-64) and no echo (-63) to cloud analysis
!  value of missing (-999.0) and no echo (-99.0)
!
      do i=1,numref
          do k=1,maxlvl
              if( abs(ref3d_column(k+2,i)+64.0_r_kind) <= 0.00001_r_kind) then
                  ref3d_column(k+2,i)=-999.0_r_kind
              elseif( abs(ref3d_column(k+2,i)+63.0_r_kind) <= 0.00001_r_kind) then
                  ref3d_column(k+2,i)=-99.0_r_kind
              end if
          end do
      end do

      write(6,*) ' ------- check max and min value of bufr radar reflectivity -------'
      write(6,*) ' level      maxval(dbz)       minval(dbz)'
      do k=1,maxlvl
          write(6,*) k,maxval(ref3d_column(k+2,:)),minval(ref3d_column(k+2,:))
      end do


      i_maxloc=-1.0_r_kind
      j_maxloc=-1.0_r_kind
      k_maxloc=-1.0_r_kind
      kint_maxloc=-1
      dbz_max=-999.99_r_kind
      ndata2=0 
      do i=1,numref
        do k=1,maxlvl
          if( ref3d_column(k+2,i) >= refl_lowbnd_dbz) then
            ndata2=ndata2+1

            cdata_out( 1,ndata2) = thiserr             ! reflectivity obs error (dB) - inflated/adjusted
                                                       ! ier=1       ! index of obs error
            cdata_out( 2,ndata2) = ref3d_column(1,i)   ! grid relative longitude
                                                       ! ilon=2      ! index of grid relative obs location (x)
            cdata_out( 3,ndata2) = ref3d_column(2,i)   ! grid relative latitude
                                                       ! ilat=3      ! index of grid relative obs location (y)
            if ( l_latlon_dbzobs ) then
                dlon_earth = ref3d_column(1,i)                   ! grid relative longitude
                rlon00     = dlon_earth * deg2rad                ! convert degree to radian
                dlat_earth = ref3d_column(2,i)                   ! grid relative latitude
                rlat00     = dlat_earth * deg2rad
                call tll2xy(rlon00,rlat00,dlon,dlat,outside)
                cdata_out( 2,ndata2) = dlon            ! grid relative longitude
                                                       ! ilon=2      ! index of grid relative obs location (x)
                cdata_out( 3,ndata2) = dlat            ! grid relative latitude
                                                       ! ilat=3      ! index of grid relative obs location (y)
            else
                cdata_out( 2,ndata2) = ref3d_column(1,i)   ! grid relative longitude
                                                       ! ilon=2      ! index of grid relative obs location (x)
                cdata_out( 3,ndata2) = ref3d_column(2,i)   ! grid relative latitude
                                                       ! ilat=3      ! index of grid relative obs location (y)
            end if

            cdata_out( 4,ndata2) = hgt_ref(k)          ! obs absolute height (m) above MSL
                                                       ! ipres=4     ! index of pressure
            cdata_out( 5,ndata2) = ref3d_column(k+2,i) ! radar reflectivity factor 
                                                       ! idbzob=5    ! index of dbz observation

            write(station_id,'(I8.8)') ndata2          ! int to char string
            cdata_out( 6,ndata2) = rstation_id         ! station id (charstring equivalent to real double)
                                                       ! id=6        ! index of station id

            cdata_out( 7,ndata2) = 0.0_r_kind          ! observation time in data array
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

            if ( l_latlon_dbzobs ) then
                cdata_out(18,ndata2) = ref3d_column(1,i)          ! longitude (degrees)
                                                       ! ilone=18    ! index of longitude (degrees)
                cdata_out(19,ndata2) = ref3d_column(2,i)          ! latitude (degrees)
                                                       ! ilate=19    ! index of latitude (degrees)
            else
                dlon = ref3d_column(1,i)                   ! grid relative longitude
                dlat = ref3d_column(2,i)                   ! grid relative latitude
                call txy2ll(dlon,dlat,rlon00,rlat00)       ! convert x-y grid units to earth lat-lon coordinates
                dlon_earth = rlon00 * rad2deg              ! convert radians to degree
                dlat_earth = rlat00 * rad2deg
                cdata_out(18,ndata2) = dlon_earth          ! longitude (degrees)
                                                       ! ilone=18    ! index of longitude (degrees)
                cdata_out(19,ndata2) = dlat_earth          ! latitude (degrees)
                                                       ! ilate=19    ! index of latitude (degrees)
            end if

            cdata_out(20,ndata2) = hgt_ref(k)          ! station elevation (m)
                                                       ! istnelv=20  ! index of station elevation (m)
            cdata_out(21,ndata2) = hgt_ref(k)          ! observation height (m)
                                                       ! iobshgt=21  ! index of observation height (m)
            cdata_out(22,ndata2) = hgt_ref(k)          ! surface height
                                                       ! izz=22      ! index of surface height
            cdata_out(23,ndata2) = ikx                 ! observation provider
                                                       ! iprvd=23    ! index of observation provider
            cdata_out(24,ndata2) = ikx                 ! observation subprovider
                                                       ! isprvd=24   ! index of observation subprovider
            cdata_out(25,ndata2) = hgt_ref(k)          ! data level category
                                                       ! icat =25    ! index of data level category
            if(perturb_obs .and. dbzob)then
               cdata_out(26,ndata2) = 1.0_r_kind       ! obs perturbation
                                                       ! iptrb=26    ! index of q perturbation
            end if

            if(ref3d_column(k+2,i) > dbz_max)then
               kint_maxloc=k
               k_maxloc=real(k,r_kind)
               j_maxloc=ref3d_column(2,i)
               i_maxloc=ref3d_column(1,i)
               dbz_max =ref3d_column(k+2,i)
            end if
          endif
        enddo
      enddo
      if(ndata /= ndata2) then
         write(6,*)'read_RadarRef_directDA: Error in counting dBZ>0.0dbz --> ndata  ndata2:',ndata,ndata2
         call stop2(111)
      end if

      ilon=2         ! array index for longitude
      ilat=3         ! array index for latitude  in obs information array
      nread=ndata
      nodata=ndata
      if(ndata > 0 ) then
         if(nlon==nlon_regional .and. nlat==nlat_regional) then
            call count_obs(ndata,nreal,ilat,ilon,cdata_out,nobs)
!           the binary file through lunout is to be read in subroutine disobs
            write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,ndata
            write(lunout) cdata_out

            write(6,*)'read_radarref_directDA: npe  nobs=',npe, nobs(1:npe), &
                ' total radarref_directDA=',sum(nobs(1:npe)),'   ndata=',ndata

         else
            write(6,*)' read_rdarRef_directDA: nlon /= nlon_regional and/or nlat/=nlat_regional:', &
                      nlon,nlat,'regional:',nlon_regional,nlat_regional
            write(6,*)' read_RadarRef_directDA:  abort and stop'
            call stop2(111)
         endif
      endif
      if (allocated(cdata_out)) deallocate(cdata_out)
      if (allocated(ref3d_column)) deallocate(ref3d_column)

      write(6,'(1x,A,F12.5,1x,A,3(1x,F8.3),1x,I4)') &
          'read_RadarRef_directDA: max dbz =',dbz_max, '@ i j k =', &
          i_maxloc,j_maxloc,k_maxloc,kint_maxloc

    endif
 
    call closbf(lunin)
    close(lunin)

    return

200 continue
    write(6,*) 'read_radarref_directDA, Warning : cannot find or open bufr radar dbz data file: ', trim(infile)

end subroutine read_radarref_directDA
!
!
