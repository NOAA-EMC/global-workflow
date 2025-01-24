subroutine read_gnssrspd(nread,ndata,nodata,infile,obstype,lunout,twind,sis,&
                       nobs)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_gnssrspd            read obs from gnssrspd bufr file
!   prgmmr: kapodaca          org: Spire Global, Inc.                date: 2022-03-12
!   Largely based on other read_* routines. For the  !Winds --- surface wind speed section
!   ~L438, we used the exixting read_fl_hdob.f90 routine
!
! abstract:  This routine reads GNSSRSPD L2 wind speed observations
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain

! program history log:
!   2022-03-12  k apodaca- initial coding
!
!   input argument list:
!     infile    - unit from which to read BUFR data
!     obstype   - observation type to process
!     lunout    - unit to which to write data for further processing
!     twind     - input group time window (hours)
!     sis       - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread     - number of type "obstype" observations read
!     nodata    - number of individual "obstype" observations read
!     ndata     - number of type "obstype" observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  
!
!$$$
     use kinds, only: r_single,r_kind,r_double,i_kind
     use constants, only: zero,one_tenth,one,two,ten,deg2rad,t0c,half,&
         three,four,rad2deg,r0_01,&
         r60inv,r10,r100,r2000,hvap
     use gridmod, only: diagnostic_reg,regional,nlon,nlat,&
         tll2xy,txy2ll,rotate_wind_ll2xy,rotate_wind_xy2ll,&
         rlats,rlons,twodvar_regional
     use convinfo, only: nconvtype, &
         icuse,ictype,ioctype
     use obsmod, only: ran01dom
     use obsmod, only: iadate,bmiss,offtime_data
     use gsi_4dvar, only: l4dvar,l4densvar,time_4dvar,winlen
     use qcmod, only: errormod
     use convthin, only: make3grids,del3grids
     use ndfdgrids,only: init_ndfdgrid,destroy_ndfdgrid,relocsfcob,adjust_error
     use deter_sfc_mod, only: deter_sfc_type,deter_sfc2
     use mpimod, only: npe
                                                                                                      
     implicit none

!    Declare passed variables
     character(len=*), intent(in   ) :: infile,obstype
     character(len=20),intent(in   ) :: sis
     integer(i_kind) , intent(in   ) :: lunout
     integer(i_kind) , dimension(npe), intent(inout) :: nobs
     integer(i_kind) , intent(inout) :: nread,ndata,nodata
     real(r_kind)    , intent(in   ) :: twind
   
!    Declare local variables
!    Logical variables
     logical :: outside 
     logical :: inflate_error
     logical :: lspdob

!    Character variables
     character(40) :: timestr,locstr,wndstr,oestr  
     character( 8) :: subset
     character( 8) :: c_prvstg,c_sprvstg
     character( 8) :: c_station_id
     character(10) date
!    Integer variables
     integer(i_kind), parameter :: mxib  = 31

     integer(i_kind) :: i,k
     integer(i_kind) :: ihh,idd,idate,im,iy
     integer(i_kind) :: lunin 
     integer(i_kind) :: ireadmg,ireadsb
     integer(i_kind) :: ilat,ilon 
     integer(i_kind) :: nlv
     integer(i_kind) :: nreal,nchanl
     integer(i_kind) :: idomsfc,isflg
     integer(i_kind) :: iout 
     integer(i_kind) :: nc,ncsave
     integer(i_kind) :: ntmatch,ntb
     integer(i_kind) :: nmsg   
     integer(i_kind) :: maxobs 
     integer(i_kind) :: itype,iecol
     integer(i_kind) :: qcm,lim_qm
     integer(i_kind) :: wspd_qm
     integer(i_kind) :: ntest,nvtest
     integer(i_kind) :: igood
     integer(i_kind) :: iuse
 
     integer(i_kind) :: idate5(5)
     integer(i_kind) :: minobs,minan

     integer(i_kind), allocatable,dimension(:) :: isort

!    Real variables
     real(r_kind), parameter :: r0_001  =  0.001_r_kind
     real(r_kind), parameter :: r1_2    =    1.2_r_kind
     real(r_kind), parameter :: r3_0    =    3.0_r_kind
     real(r_kind), parameter :: r0_7    =    0.7_r_kind
     real(r_kind), parameter :: r6      =    6.0_r_kind
     real(r_kind), parameter :: r50     =   50.0_r_kind
     real(r_kind), parameter :: r1200   = 1200.0_r_kind
     real(r_kind), parameter :: emerr   =    0.2_r_kind ! RH
     real(r_kind), parameter :: missing = 1.0e+11_r_kind
     real(r_kind), parameter :: r180    = 180.0_r_kind
     real(r_kind), parameter :: r360    = 360.0_r_kind

     real(r_kind) :: toff,t4dv
     real(r_kind) :: usage
     real(r_kind) :: woe,obserr
     real(r_kind) :: dlat,dlon,dlat_earth,dlon_earth
     real(r_kind) :: dlat_earth_deg,dlon_earth_deg
     real(r_kind) :: cdist,disterr,disterrmax,rlon00,rlat00
     real(r_kind) :: vdisterrmax
     real(r_kind) :: spdob
     real(r_kind) :: gob
     real(r_kind) :: dlnpsob
     real(r_kind) :: tdiff 
     real(r_kind) :: tsavg,ff10,sfcr,zz
     real(r_kind) :: errmin
     real(r_kind) :: log100  

     real(r_kind) :: obstime(6,1)
     real(r_kind) :: obsloc(2,1)
     real(r_kind) :: gnssrw(2,1)
      
     real(r_double) :: rstation_id
     real(r_double) :: r_prvstg(1,1),r_sprvstg(1,1)

     real(r_kind), allocatable,dimension(:,:) :: cdata_all,cdata_out

!    Equivalence to handle character names
     equivalence(r_prvstg(1,1),c_prvstg)
     equivalence(r_sprvstg(1,1),c_sprvstg)
     equivalence(rstation_id,c_station_id)

!    Data 
     !data timestr  / 'YEAR MNTH DAYS HOUR MINU SECO' /
     data timestr / 'DHR RPT' /
     !data locstr   / 'CLAT CLON' /
     data locstr   / 'XOB YOB' /
     !data wndstr   / 'WSPD' / !GNSSRSPD Wind speed
     data wndstr   / 'SOB' / !GNSSRSPD Wind speed
     data oestr   / 'WSU' / !GNSSRSPD Wind speed uncertainty/error 
     data lunin    / 13 /
 
!------------------------------------------------------------------------------------------------

     write(6,*)'READ_GNSSRSPD: begin to read gnssrspd satellite data ...'

!    Initialize parameters

!    Set common variables
     lspdob = obstype == 'gnssrspd'

     nreal  = 0
     iecol  = 0
     log100=log(100._r_kind)
 
 
     lim_qm = 4
     iecol=0
     if (lspdob) then
        nreal  = 23
        iecol  =  4  
        errmin = one
     else 
        write(6,*) ' illegal obs type in read_gnssrspd '
        call stop2(94)
     end if

     inflate_error = .true.

! Check if the obs type specified in the convinfo is in the fl hdob bufr file 
! If found, get the index (nc) from the convinfo for the specified type
ntmatch =  0
ncsave  =  0
do nc = 1, nconvtype
    if (trim(ioctype(nc)) == trim(obstype)) then 
        if (trim(ioctype(nc)) == 'gnssrspd' .and. ictype(nc) == 600 ) then
            ntmatch = ntmatch + 1
            ncsave  = nc
            itype   = ictype(nc)
        end if
    end if
end do
if (ntmatch == 0) then  ! Return if not specified in convinfo 
    write(6,*) ' READ_GNSSRSPD: No matching obstype found in obsinfo ', obstype
    return
else 
    nc = ncsave
    write(6,*) ' READ_GNSSRSPD: Processing GNSSRSPD data : ', ntmatch, nc, ioctype(nc), ictype(nc), itype 
end if


!------------------------------------------------------------------------------------------------

!    Go through the bufr file to find out how mant subsets to process
     nmsg   = 0
     maxobs = 0
     call closbf(lunin) 
     open(lunin,file=trim(infile),form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     
     loop_msg1: do while(ireadmg(lunin,subset,idate) >= 0)
        if(nmsg == 0) call time_4dvar(idate,toff)   ! time offset (hour)

        nmsg = nmsg+1
        loop_readsb1: do while(ireadsb(lunin) == 0)
           maxobs = maxobs+1     
        end do loop_readsb1
     end do loop_msg1
     call closbf(lunin)
     write(6,*) 'READ_GNSSRSPD: total number of data found in the bufr file ',maxobs,obstype      
     write(6,*) 'READ_GNSSRSPD: time offset is ',toff,' hours'

!---------------------------------------------------------------------------------------------------

!    Allocate array to hold data
     allocate(cdata_all(nreal,maxobs))
     allocate(isort(maxobs))

!    Initialize
     cdata_all = zero 
     isort     = 0
     nread     = 0
     nchanl    = 0
     ntest     = 0
     nvtest    = 0
     ilon      = 2 
     ilat      = 3 

!    Open bufr file again for reading
     call closbf(lunin)
     open(lunin,file=trim(infile),form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     ntb   = 0     
     igood = 0
!    Loop through BUFR file
     loop_msg2: do while(ireadmg(lunin,subset,idate) >= 0)
        loop_readsb2: do while(ireadsb(lunin) == 0)

           ntb = ntb+1

           c_station_id = subset

!          QC mark 9: will be monitored but not assimilated
!          QC mark 4: reject - will not be monitored nor assimilated 
!          QC mark 3: suspect
!          QC mark 2: neutral or not checked 
!          QC mark 1: good
!          QC mark 0: keep - will be always assimilated
           qcm     = 0 
           wspd_qm = 0 


!          Read observation time 
           call ufbint(lunin,obstime,2,1,nlv,timestr) 

! If date in gnssrspd file does not agree with analysis date, 
! print message and stop program execution.
              write(date,'( i10)') idate
              read (date,'(i4,3i2)') iy,im,idd,ihh
           if(offtime_data) then

!             in time correction for observations to account for analysis
              idate5(1)=iy
              idate5(2)=im
              idate5(3)=idd
              idate5(4)=ihh
              idate5(5)=0
              call w3fs21(idate5,minobs)    !  obs ref time in minutes relative to historic date
              idate5(1)=iadate(1)
              idate5(2)=iadate(2)
              idate5(3)=iadate(3)
              idate5(4)=iadate(4)
              idate5(5)=0
              call w3fs21(idate5,minan)    !  analysis ref time in minutes relative to historic date
!             Add obs reference time, then subtract analysis time to get obs time relative to analysis

              tdiff=float(minobs-minan)*r60inv

           else
              tdiff=zero
           end if

           t4dv = toff+obstime(1,1)

           if (l4dvar.or.l4densvar) then
              if (t4dv < zero .OR. t4dv > winlen) cycle loop_readsb2
           else
              if (abs(tdiff)>twind) cycle loop_readsb2
           endif
           nread = nread+1

           usage = zero                ! will be considered for assimilation
                                       ! subject to further QC in setupt subroutine
           iuse  = icuse(nc)           ! assimilation flag 
           if (iuse <=0) usage = r100  ! will be monitored but not assimilated

!          Read observation location (lat/lon degree) 
           call ufbint(lunin,obsloc,2,1,nlv,locstr)

           if (obsloc(1,1) == missing .or. abs(obsloc(1,1)) < -180.0_r_kind .or. &     
               obsloc(1,1) == missing .or. abs(obsloc(1,1)) >  180.0_r_kind .or. &
               obsloc(2,1) == missing .or. abs(obsloc(2,1)) <  -90.0_r_kind .or. &
               obsloc(2,1) == missing .or. abs(obsloc(2,1)) >   90.0_r_kind ) then 
               write(6,*) 'READ_GNSSRSPD: bad lon/lat values: ', obsloc(1,1),obsloc(2,1)              
               cycle loop_readsb2     
           endif
! GNSSRSPD BUFR longitudes are in the +/- 180 range, need to convert to 0 to 360
! deg
           if (obsloc(1,1) < 0.0_r_kind) obsloc(1,1) = obsloc(1,1) + 360.0_r_kind
           !if (obsloc(1,1) >= 0.0_r_kind .or. obsloc(1,1) <= 180.0_r_kind) obsloc(1,1) = obsloc(1,1) + 180.0_r_kind
 
           dlon_earth_deg = obsloc(1,1)
           dlat_earth_deg = obsloc(2,1)
           dlon_earth = obsloc(1,1)*deg2rad ! degree to radian
           dlat_earth = obsloc(2,1)*deg2rad ! degree to radian

!          Convert obs lat/lon to rotated coordinate and check 
!          if the obs is outside of the regional domain
           if (regional) then
              call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
              if (diagnostic_reg) then
                 call txy2ll(dlon,dlat,rlon00,rlat00)
                 ntest      = ntest+1
                 cdist      = sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                             (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
                 cdist      = max(-one,min(cdist,one))
                 disterr    = acos(cdist)*rad2deg
                 disterrmax = max(disterrmax,disterr)
              end if
              if(outside) cycle loop_readsb2
           else
              dlon = dlon_earth
              dlat = dlat_earth
              call grdcrd1(dlat,rlats,nlat,1)
              call grdcrd1(dlon,rlons,nlon,1)
           endif


!          Read surface wind speed [m/s] from GNSSRSPD 
           if (lspdob) then
              !usage=r100 
              !usage = zero  ! will be considered for assimilation

              ! Get Wind Speed observations from bufr file
              call ufbint(lunin,gnssrw,1,1,nlv,wndstr)
              spdob = gnssrw(1,1) ! surface wind speed 

              ! Don't permit observations with ws <= 1 m/s
              if (spdob <= one) cycle loop_readsb2
              if (spdob >= missing) cycle loop_readsb2

              ! Get observation error from bufr file
              call ufbint(lunin,gnssrw,1,1,nlv,oestr)
              obserr = max(gnssrw(1,1),1.5_r_kind) ! surface wind speed error
           endif


           if ( .not. twodvar_regional) then
              call deter_sfc_type(dlat_earth,dlon_earth,t4dv,isflg,tsavg)
           endif

           ! Get information from surface file necessary for conventional data
           call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)                                                                      
           ! Process data passed quality control 
           igood = igood + 1
           ndata = ndata + 1
           nodata = nodata + 1
           iout = ndata

!         Read extrapolated surface pressure [pa] and convert to [cb]
           dlnpsob = log100         ! default (1000mb)           

!-------------------------------------------------------------------------------------------------          

           ! Winds --- surface wind speed 
           if (lspdob) then
              woe = obserr
              !if (inflate_error) woe = woe*r3_0
              !if (inflate_error) woe = woe*r1_2
              !if (qcm > lim_qm ) woe = woe*1.0e6_r_kind
              cdata_all( 1,iout)=woe                    ! wind error
              cdata_all( 2,iout)=dlon                   ! grid relative longitude             
              cdata_all( 3,iout)=dlat                   ! grid relative latitude                  
              cdata_all( 4,iout)=dlnpsob                ! ln(surface pressure in cb)
              cdata_all( 5,iout)=spdob*sqrt(two)*half   ! u obs
              cdata_all( 6,iout)=spdob*sqrt(two)*half   ! v obs
              cdata_all( 7,iout)=rstation_id            ! station id
              cdata_all( 8,iout)=t4dv                   ! time
              cdata_all( 9,iout)=nc                     ! type
              cdata_all(10,iout)=r10                    !  elevation of observation ! 10-m wind       
              cdata_all(11,iout)=qcm                    !  quality mark 
              cdata_all(12,iout)=obserr                 !  original obs error 
              cdata_all(13,iout)=usage                  ! usage parameter 
              cdata_all(14,iout)=idomsfc                !  dominate surface type        
              cdata_all(15,iout)=tsavg                  ! skin temperature 
              cdata_all(16,iout)=ff10                   ! 10 meter wind factor     
              cdata_all(17,iout)=sfcr                   ! surface roughness 
              cdata_all(18,iout)=dlon_earth_deg         ! earth relative longitude (degree)                
              cdata_all(19,iout)=dlat_earth_deg         ! earth relative latitude (degree)                  
              cdata_all(20,iout)=gob                    !  station elevation (m)    
              cdata_all(21,iout)=zz                     !  terrain height at ob location        
              cdata_all(22,iout)=r_prvstg(1,1)          !  provider name 
              cdata_all(23,iout)=r_sprvstg(1,1)         !  subprovider name 
           endif 

        end do loop_readsb2
     end do loop_msg2

!    Close unit to bufr file
     call closbf(lunin)
 
!    Write header record and data to output file for further processing
     write(6,*) "READ_GNSSRSPD: nreal=",nreal," ndata=",ndata
     allocate(cdata_out(nreal,ndata))
     do i=1,ndata
        do k=1,nreal
           cdata_out(k,i)=cdata_all(k,i)
        end do
     end do
     deallocate(cdata_all)

     call count_obs(ndata,nreal,ilat,ilon,cdata_out,nobs)
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) cdata_out
     deallocate(cdata_out)
900  continue
     if(diagnostic_reg .and. ntest>0)  write(6,*)'READ_GNSSRSPD:  ',&
        'ntest,  disterrmax=', ntest,disterrmax
     if(diagnostic_reg .and. nvtest>0) write(6,*)'READ_GNSSRSPD:  ',&
        'nvtest,vdisterrmax=',ntest,vdisterrmax

     if (ndata == 0) then
        call closbf(lunin)
        write(6,*)'READ_GNSSRSPD: no data to process'
     endif
     write(6,*)'READ_GNSSRSPD: nreal=',nreal
     write(6,*)'READ_GNSSRSPD: ntb,nread,ndata,nodata=',ntb,nread,ndata,nodata

     call closbf(lunin)
     close(lunin)

!    End of routine
     return

end subroutine read_gnssrspd

