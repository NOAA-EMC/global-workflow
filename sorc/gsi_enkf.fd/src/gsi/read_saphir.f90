subroutine read_saphir(mype,val_tovs,ithin,isfcalc,&
     rmesh,jsatid,gstime,infile,lunout,obstype,&
     nread,ndata,nodata,twind,sis, &
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs,dval_use)
! subprogram:    read_saphir                 read bufr format saphir data
! prgmmr :   ejones          org: jcsda               date: 2015-01-02
! code copied from read_atms.f90
!
! abstract:  This routine reads BUFR format SAPHIR radiance (brightness
!            temperature) data. 
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain  
!
! program history log:
!  2015-01-02  ejones  - adapted from read_atms.f90
!  2015-09-17  Thomas  - add l4densvar and thin4d to data selection procedure
!  2016-03-09  ejones  - update mnemonics for operational SAPHIR bufr
!  2016-04-01  ejones  - add binning of fovs for scan angle bias correction 
!  2016-07-25  ejones  - remove binning of fovs
!  2016-10-05  acollard -Fix interaction with NSST and missing zenith angle issue.
!  2018-05-21  j.jin   - added time-thinning. Moved the checking of thin4d into satthin.F90.
!
!   input argument list:
!     mype     - mpi task id
!     val_tovs - weighting factor applied to super obs
!     ithin    - flag to thin data
!     isfcalc  - method to calculate surface fields within FOV
!                when one, calculate accounting for size/shape of FOV.
!                otherwise, use bilinear interpolation.
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window(hours)
!     sis      - sensor/instrument/satellite indicator
!     mype_root - "root" task for sub-communicator
!     mype_sub - mpi task id within sub-communicator
!     npe_sub  - number of data read tasks
!     mpi_comm_sub - sub-communicator for data read
!
!   output argument list:
!     nread    - number of BUFR ATMS 1b observations read
!     ndata    - number of BUFR ATMS 1b profiles retained for further processing
!     nodata   - number of BUFR ATMS 1b observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,destroygrids,checkob, &
      finalcheck,map2tgrid,score_crit
  use satthin, only: radthin_time_info,tdiff2crit
  use obsmod,  only: time_window_max
  use radinfo, only: iuse_rad,nusis,jpch_rad, &
      use_edges,radedge1,radedge2,radstart,radstep
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,tll2xy,txy2ll,rlats,rlons
  use constants, only: deg2rad,zero,one,two,three,rad2deg,r60inv
  use crtm_module, only : max_sensor_zenith_angle
  use calc_fov_crosstrk, only : instrument_init, fov_cleanup, fov_check
  use gsi_4dvar, only: l4dvar,iwinbgn,winlen,l4densvar
  use deter_sfc_mod, only: deter_sfc_fov,deter_sfc
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth,gsi_nstcoupler_deter
  use mpimod, only: npe
  use radiance_mod, only: rad_obs_type

  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: infile,obstype,jsatid
  character(len=20),intent(in  ) :: sis
  integer(i_kind) ,intent(in   ) :: mype,lunout,ithin
  integer(i_kind) ,intent(inout) :: isfcalc
  integer(i_kind) ,intent(inout) :: nread
  integer(i_kind) ,intent(  out) :: ndata,nodata
  real(r_kind)    ,intent(in   ) :: rmesh,gstime,twind
  real(r_kind)    ,intent(inout) :: val_tovs
  integer(i_kind) ,intent(in   ) :: mype_root
  integer(i_kind) ,intent(in   ) :: mype_sub
  integer(i_kind) ,intent(in   ) :: npe_sub
  integer(i_kind) ,intent(in   ) :: mpi_comm_sub
  integer(i_kind),dimension(npe)  ,intent(inout) :: nobs
  logical         ,intent(in   ) :: dval_use

! Declare local parameters

  character(8),parameter:: fov_flag="crosstrk"
  integer(i_kind),parameter:: n1bhdr=12
  integer(i_kind),parameter:: n2bhdr=4
  integer(i_kind),parameter:: maxobs = 5000000
  integer(i_kind),parameter:: max_chanl = 22
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind
  ! The next two are one minute in hours
  real(r_kind),parameter:: one_minute=0.01666667_r_kind
  real(r_kind),parameter:: minus_one_minute=-0.01666667_r_kind
  real(r_kind),parameter:: rato=1.1363987_r_kind ! ratio of satellite height to 
                                                 ! distance from Earth's centre

! Declare local variables
  logical               :: outside,iuse,assim,valid
  character(8)          :: subset
  character(80)         :: hdr1b,hdr2b

  integer(i_kind)       :: ireadsb,ireadmg
  integer(i_kind)       :: i,j,k,ntest,iob
  integer(i_kind)       :: iret,idate,nchanl,n,idomsfc(1)
  integer(i_kind)       :: kidsat,maxinfo
  integer(i_kind)       :: nmind,itx,nreal,nele,itt,num_obs
  integer(i_kind)       :: iskip 
  integer(i_kind)       :: lnbufr,ksatid,isflg  
  integer(i_kind)       :: ilat,ilon,nadir
  integer(i_kind),dimension(5):: idate5
  integer(i_kind)       :: instr,ichan
  integer(i_kind)       :: radedge_min, radedge_max
  integer(i_kind), POINTER :: ifov  

  real(r_kind)          :: sfcr 
  real(r_kind)          :: expansion
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts
  real(r_kind)           :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind)           :: zob,tref,dtw,dtc,tz_tr

  real(r_kind)           :: dlat,dlon,tdiff,panglr
  real(r_kind)           :: dlon_earth_deg,dlat_earth_deg 
  real(r_kind)           :: step,start,dist1
  real(r_kind),dimension(0:4)            :: rlndsea
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind), POINTER :: bt_in(:), crit1,rsat, t4dv, solzen, solazi
  real(r_kind), POINTER :: dlon_earth,dlat_earth,satazi, lza

  integer(i_kind), ALLOCATABLE, TARGET  :: ifov_save(:)
  integer(i_kind), ALLOCATABLE, TARGET :: it_mesh_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: rsat_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: t4dv_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: dlon_earth_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: dlat_earth_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: crit1_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: lza_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: satazi_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: solzen_save(:) 
  real(r_kind), ALLOCATABLE, TARGET :: solazi_save(:) 
  real(r_kind), ALLOCATABLE, TARGET :: bt_save(:,:)


  integer(i_kind),allocatable,dimension(:):: nrec
  real(r_double),allocatable,dimension(:) :: data1b8
  real(r_double),dimension(n1bhdr):: bfr1bhdr
  real(r_double),dimension(n2bhdr):: bfr2bhdr

  real(r_kind)          :: disterr,disterrmax,dlon00,dlat00
  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin
  integer(i_kind),pointer:: it_mesh => null()

!**************************************************************************

! Initialize variables

  maxinfo=31
  lnbufr = 15
  disterrmax=zero
  ntest=0
  ndata  = 0
  nodata  = 0
  nread  = 0

  ilon=3
  ilat=4

  if(nst_gsi>0) then
     call gsi_nstcoupler_skindepth(obstype,zob)
  endif

  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif
! Make thinning grids
  call makegrids(rmesh,ithin,n_tbin=n_tbin)

! Set nadir position
  nadir=65

! Set various variables depending on type of data to be read

  if (obstype /= 'saphir') then
     write(*,*) 'READ_SAPHIR called for obstype '//obstype//': RETURNING'
     return
  end if

  if(jsatid == 'meghat') then
     kidsat=440
  else 
     write(*,*) 'READ_SAPHIR: Unrecognized value for jsatid '//jsatid//': RETURNING'
     return
  end if

! look up info in the scaninfo file
  radedge_min = 0
  radedge_max = 1000
  do i=1,jpch_rad
     if (trim(nusis(i))==trim(sis)) then
        step  = radstep(i)
        start = radstart(i)
        if (radedge1(i)/=-1 .or. radedge2(i)/=-1) then
           radedge_min=radedge1(i)
           radedge_max=radedge2(i)
        end if
        exit 
     endif
  end do 

! IFSCALC setup
  nchanl=6
  if (dval_use) maxinfo = maxinfo+2 
  if (isfcalc==1) then
     instr=19                    ! This section isn't really updated.
     expansion=2.9_r_kind        ! use almost three for microwave sensors.
  endif
!   Set rlndsea for types we would prefer selecting
  rlndsea(0) = zero
  rlndsea(1) = 15._r_kind
  rlndsea(2) = 10._r_kind
  rlndsea(3) = 15._r_kind
  rlndsea(4) = 100._r_kind
     
! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.

  assim=.false.
  search: do i=1,jpch_rad
     if ((nusis(i)==sis) .and. (iuse_rad(i)>0)) then
        assim=.true.
        exit search
     endif
  end do search
  if (.not.assim) val_tovs=zero

! This may need updating, but 
! Initialize variables for use by FOV-based surface code.
  if (isfcalc == 1) then
     call instrument_init(instr,jsatid,expansion,valid)
     if (.not. valid) then
       if (assim) then
         write(6,*)'READ_SAPHIR:  ***ERROR*** IN SETUP OF FOV-SFC CODE. STOP'
         call stop2(71)
       else
         call fov_cleanup
         isfcalc = 0
         write(6,*)'READ_SAPHIR:  ***ERROR*** IN SETUP OF FOV-SFC CODE'
       endif
     endif
  endif

! Allocate arrays for BUFR I/O
  ALLOCATE(data1b8(nchanl))
  ALLOCATE(rsat_save(maxobs))
  ALLOCATE(t4dv_save(maxobs))
  ALLOCATE(ifov_save(maxobs))
  ALLOCATE(dlon_earth_save(maxobs))
  ALLOCATE(dlat_earth_save(maxobs))
  ALLOCATE(crit1_save(maxobs))
  ALLOCATE(it_mesh_save(maxobs))
  ALLOCATE(lza_save(maxobs))
  ALLOCATE(satazi_save(maxobs))
  ALLOCATE(solzen_save(maxobs)) 
  ALLOCATE(solazi_save(maxobs)) 
  ALLOCATE(bt_save(max_chanl,maxobs))

! Reopen unit to satellite bufr file
  iob=1
  open(lnbufr,file=trim(infile),form='unformatted',status = 'old',err = 500)

  call openbf(lnbufr,'IN',lnbufr)
  hdr1b ='SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH'
  hdr2b ='IANG SOZA BEARAZ SOLAZI'
!  mnemonics in non-operational SAPHIR bufr are a little different:
!  hdr1b ='SAID FOVN YEAR MONTH DAY HOUR MINU SECO CLATH CLONH'
!  hdr2b ='AGIND SOZA BEARAZ SOLAZI'  ! AGIND instead of SAZA

! Loop to read bufr file
  read_subset: do while(ireadmg(lnbufr,subset,idate)>=0 .AND. iob < maxobs)
     read_loop: do while (ireadsb(lnbufr)==0 .and. iob < maxobs)

        rsat       => rsat_save(iob)
        t4dv       => t4dv_save(iob)
        dlon_earth => dlon_earth_save(iob)
        dlat_earth => dlat_earth_save(iob)
        crit1      => crit1_save(iob)
        it_mesh    => it_mesh_save(iob)
        ifov       => ifov_save(iob)
        lza        => lza_save(iob)
        satazi     => satazi_save(iob)
        solzen     => solzen_save(iob)
        solazi     => solazi_save(iob)

        call ufbint(lnbufr,bfr1bhdr,n1bhdr,1,iret,hdr1b)

!       Extract satellite id.  If not the one we want, read next record
        rsat=bfr1bhdr(1) 
        ksatid=nint(bfr1bhdr(1))
        if(ksatid /= kidsat) cycle read_subset

!       Extract observation location and other required information
        if(abs(bfr1bhdr(9)) <= 90._r_kind .and. abs(bfr1bhdr(10)) <= r360)then
           dlat_earth = bfr1bhdr(9)
           dlon_earth = bfr1bhdr(10)
        else
           cycle read_loop
        end if
        if(dlon_earth<zero)  dlon_earth = dlon_earth+r360
        if(dlon_earth>=r360) dlon_earth = dlon_earth-r360


!       Extract date information.  If time outside window, skip this obs
        idate5(1) = bfr1bhdr(3) !year
        idate5(2) = bfr1bhdr(4) !month
        idate5(3) = bfr1bhdr(5) !day
        idate5(4) = bfr1bhdr(6) !hour
        idate5(5) = bfr1bhdr(7) !minute
        call w3fs21(idate5,nmind)
        t4dv= (real((nmind-iwinbgn),r_kind) + bfr1bhdr(8)*r60inv)*r60inv    ! add in seconds
        tdiff=t4dv+(iwinbgn-gstime)*r60inv
        if (l4dvar.or.l4densvar) then
           if (t4dv<minus_one_minute .OR. t4dv>winlen+one_minute) &
                cycle read_loop
        else
           if(abs(tdiff) > twind+one_minute) cycle read_loop
        endif
 
        crit0 = 0.01_r_kind
        timeinflat=two
        call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)

        call ufbint(lnbufr,bfr2bhdr,n2bhdr,1,iret,hdr2b)

        satazi=bfr2bhdr(3)
        if (abs(satazi) > r360) then
           satazi=zero
        endif

        ifov = nint(bfr1bhdr(2))
        lza = bfr2bhdr(1)*deg2rad      ! local zenith angle
        if(ifov <= 65)    lza=-lza


! compute look angle (panglr) and check against max angle
!        panglr=(start+real(ifov-1,r_kind)*step)*deg2rad
! Use this calculation for now:
        step = .6660465
        panglr = (42.96 - real(ifov-1,r_kind)*step)*deg2rad

        if(abs(lza)*rad2deg > MAX_SENSOR_ZENITH_ANGLE) then
          write(6,*)'READ_SAPHIR WARNING lza error ',lza,panglr
          cycle read_loop
        end if

!       solzen_save(iob)=bfr2bhdr(2) ! encoded as 0.1E+12 in BUFR file
        solzen_save(iob)=zero        ! set to 0.0 to bypass CRTM check
        solazi_save(iob)=bfr2bhdr(4) 

!       Read data record.  Increment data counter
        call ufbrep(lnbufr,data1b8,1,nchanl,iret,'TMBRST')

!       non-operational SAPHIR bufr:
!        call ufbrep(lnbufr,data1b8,1,nchanl,iret,'TMBR')

        bt_save(1:nchanl,iob) = data1b8(1:nchanl)

        iob=iob+1

     end do read_loop
  end do read_subset
  call closbf(lnbufr)
  close(lnbufr)
  deallocate(data1b8)
  
  num_obs = iob-1

500 continue
  if (num_obs <= 0) then
     write(*,*) 'READ_SAPHIR: No SAPHIR Data were read in'
     return
  end if

! Allocate arrays to hold all data for given satellite
  nreal = maxinfo + nstinfo
  nele  = nreal   + nchanl
  allocate(data_all(nele,itxmax),nrec(itxmax))

  nrec=999999
  ObsLoop: do iob = 1, num_obs  

     rsat       => rsat_save(iob)
     t4dv       => t4dv_save(iob)
     dlon_earth => dlon_earth_save(iob)
     dlat_earth => dlat_earth_save(iob)
     crit1      => crit1_save(iob)
     it_mesh    => it_mesh_save(iob)
     ifov       => ifov_save(iob)
     lza        => lza_save(iob)
     satazi     => satazi_save(iob)
     solzen     => solzen_save(iob)
     solazi     => solazi_save(iob)
     bt_in      => bt_save(1:nchanl,iob)
     
     dlat_earth_deg = dlat_earth
     dlon_earth_deg = dlon_earth
     dlat_earth = dlat_earth*deg2rad
     dlon_earth = dlon_earth*deg2rad   

!    Regional case
     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if(diagnostic_reg) then
           call txy2ll(dlon,dlat,dlon00,dlat00)
           ntest=ntest+1
           disterr=acos(sin(dlat_earth)*sin(dlat00)+cos(dlat_earth)*cos(dlat00)* &
                (sin(dlon_earth)*sin(dlon00)+cos(dlon_earth)*cos(dlon00)))*rad2deg
           disterrmax=max(disterrmax,disterr)
        end if
           
!       Check to see if in domain
        if(outside) cycle ObsLoop
           
!    Global case
     else
        dlat=dlat_earth
        dlon=dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif

! Check time window
     if (l4dvar.or.l4densvar) then
        if (t4dv<zero .OR. t4dv>winlen) cycle ObsLoop
     else
        tdiff=t4dv+(iwinbgn-gstime)*r60inv
        if(abs(tdiff) > twind) cycle ObsLoop
     endif
 
!    Map obs to thinning grid
     call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)
     if(.not. iuse)cycle ObsLoop

!
!    Check FOV and scan-edge usage
     if (.not. use_edges .and. (ifov < radedge_min .OR. ifov > radedge_max )) &
          cycle ObsLoop

     nread=nread+nchanl
     
!    Transfer observed brightness temperature to work array.  If any
!    temperature exceeds limits, reset observation to "bad" value
     iskip=0
     do j=1,nchanl
        if (bt_in(j) < tbmin .or. bt_in(j) > tbmax) then
           iskip = iskip + 1
        endif
     end do
     if (iskip >= nchanl) cycle ObsLoop

!    Determine surface properties based on 
!    sst and land/sea/ice mask   
!
!    isflg    - surface flag
!               0 sea
!               1 land
!               2 sea ice
!               3 snow
!               4 mixed                       

!    FOV-based surface code requires fov number.  if out-of-range, then
!    skip this ob.

     if (isfcalc == 1) then
        call fov_check(ifov,instr,ichan,valid)
        if (.not. valid) cycle ObsLoop

!    When isfcalc is one, calculate surface fields based on size/shape of fov.
!    Otherwise, use bilinear method.

        call deter_sfc_fov(fov_flag,ifov,instr,ichan,satazi,dlat_earth_deg,&
             dlon_earth_deg,expansion,t4dv,isflg,idomsfc(1), &
             sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
     else
        call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg, &
             idomsfc(1),sfcpct,ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)

           if(isflg/=0) cycle ObsLoop                     ! use data over water only

     endif



     crit1 = crit1 + rlndsea(isflg) + 10._r_kind*real(iskip,r_kind) + 0.01_r_kind * abs(zz)
     call checkob(dist1,crit1,itx,iuse)
     if(.not. iuse)cycle ObsLoop

!    Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
!     crit1 = crit1+pred     ! not using pred for now... -ej
     call finalcheck(dist1,crit1,itx,iuse)
     if(.not. iuse)cycle ObsLoop
     
!    interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
     if(nst_gsi>0) then
        tref  = ts(0)
        dtw   = zero
        dtc   = zero
        tz_tr = one
        if(sfcpct(0)>zero) then
           call gsi_nstcoupler_deter(dlat_earth,dlon_earth,t4dv,zob,tref,dtw,dtc,tz_tr)
        endif
     endif

! Re-calculate look angle
!     panglr=(start+real(ifov-1,r_kind)*step)*deg2rad
! Use this calculation for now:
        step = .6660465
        panglr = (42.96 - real(ifov-1,r_kind)*step)*deg2rad

!     Load selected observation into data array
              
     data_all(1 ,itx)= rsat                      ! satellite ID
     data_all(2 ,itx)= t4dv                      ! time
     data_all(3 ,itx)= dlon                      ! grid relative longitude
     data_all(4 ,itx)= dlat                      ! grid relative latitude
     data_all(5 ,itx)= lza                       ! local zenith angle
     data_all(6 ,itx)= satazi                    ! local azimuth angle
     data_all(7 ,itx)= panglr                    ! look angle
     data_all(8 ,itx)= ifov                      ! scan position
     data_all(9 ,itx)= solzen                    ! solar zenith angle
     data_all(10,itx)= solazi                    ! solar azimuth angle
     data_all(11,itx) = sfcpct(0)                ! sea percentage of
     data_all(12,itx) = sfcpct(1)                ! land percentage
     data_all(13,itx) = sfcpct(2)                ! sea ice percentage
     data_all(14,itx) = sfcpct(3)                ! snow percentage
     data_all(15,itx)= ts(0)                     ! ocean skin temperature
     data_all(16,itx)= ts(1)                     ! land skin temperature
     data_all(17,itx)= ts(2)                     ! ice skin temperature
     data_all(18,itx)= ts(3)                     ! snow skin temperature
     data_all(19,itx)= tsavg                     ! average skin temperature
     data_all(20,itx)= vty                       ! vegetation type
     data_all(21,itx)= vfr                       ! vegetation fraction
     data_all(22,itx)= sty                       ! soil type
     data_all(23,itx)= stp                       ! soil temperature
     data_all(24,itx)= sm                        ! soil moisture
     data_all(25,itx)= sn                        ! snow depth
     data_all(26,itx)= zz                        ! surface height
     data_all(27,itx)= idomsfc(1) + 0.001_r_kind ! dominate surface type
     data_all(28,itx)= sfcr                      ! surface roughness
     data_all(29,itx)= ff10                      ! ten meter wind factor
     data_all(30,itx) = dlon_earth_deg           ! earth relative longitude (deg)
     data_all(31,itx) = dlat_earth_deg           ! earth relative latitude (deg)

     if(dval_use) then     
        data_all(32,itx)= val_tovs
        data_all(33,itx)= itt
     endif
     
     if(nst_gsi>0) then
        data_all(maxinfo+1,itx) = tref            ! foundation temperature
        data_all(maxinfo+2,itx) = dtw             ! dt_warm at zob
        data_all(maxinfo+3,itx) = dtc             ! dt_cool at zob
        data_all(maxinfo+4,itx) = tz_tr           ! d(Tz)/d(Tr)
     endif

     do i=1,nchanl
        data_all(i+nreal,itx)=bt_in(i)
     end do
     nrec(itx)=iob

  end do ObsLoop

! DEAllocate I/O arrays
  DEALLOCATE(rsat_save)
  DEALLOCATE(t4dv_save)
  DEALLOCATE(ifov_save)
  DEALLOCATE(dlon_earth_save)
  DEALLOCATE(dlat_earth_save)
  DEALLOCATE(crit1_save)
  DEALLOCATE(it_mesh_save)
  DEALLOCATE(lza_save)
  DEALLOCATE(satazi_save)
  DEALLOCATE(solzen_save) 
  DEALLOCATE(solazi_save) 
  DEALLOCATE(bt_save)

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
       nele,itxmax,nread,ndata,data_all,score_crit,nrec)

  if(mype_sub==mype_root)then
     do n=1,ndata
        do i=1,nchanl
           if(data_all(i+nreal,n) > tbmin .and. &
                data_all(i+nreal,n) < tbmax)nodata=nodata+1
        end do
     end do

     if(dval_use .and. assim)then
        do n=1,ndata
           itt=nint(data_all(maxinfo,n))
           super_val(itt)=super_val(itt)+val_tovs
        end do
     endif
             
!    Write final set of "best" observations to output file
     call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  end if
     
! Deallocate local arrays
  deallocate(data_all,nrec)

! Deallocate satthin arrays
  call destroygrids

! Deallocate FOV surface code arrays and nullify pointers.
  if (isfcalc == 1) call fov_cleanup

  if(diagnostic_reg.and.ntest>0) write(6,*)'READ_SAPHIR:  ',&
     'mype,ntest,disterrmax=',mype,ntest,disterrmax

! End of routine
  return

end subroutine read_saphir
