subroutine read_goesndr(mype,val_goes,ithin,rmesh,jsatid,infile,&
     lunout,obstype,nread,ndata,nodata,twind,gstime,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs, &
     nrec_start,dval_use)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_goesndr                   read goes sounder data
!   prgmmr: yang             org: np23                date: 1998-05-15
!
! abstract:  This routine reads GOES sounder radiance (brightness
!            temperature) files.  Optionally, the data are thinned to 
!            a specified resolution using simple quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   1998-05-15 weiyu yang
!   1999-08-24 derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-05-28 kleist - update subroutine call
!   2004-06-16 treadon - update documentation
!   2004-07-23 derber - make changes to eliminate obs. earlier in thinning
!   2004-07-29 treadon - add only to module use, add intent in/out
!   2005-01-26 derber - land/sea determination and weighting for data selection
!   2005-07-08 derber - clean up, fix bugs, and improve observation selection
!   2005-09-08  derber - modify to use input group time window
!   2005-09-28  derber - modify to produce consistent surface info
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-11-22  derber  - include mean in bias correction
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-03-07  derber  - combine reading of 1x1 and 5x5 (prepbufr) files
!   2006-04-27  derber - clean up code
!   2006-05-19  eliu    - add logic to reset relative weight when all channels not used
!   2006-07-28  derber  - add solar and satellite azimuth angles remove isflg from output
!                       - add ability to read g9,g11,g13
!   2006-09-21  treadon - replace serial bufr i/o with parallel bufr i/o (mpi_io)
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2008-04-21  safford - rm unused vars
!   2009-04-18  woollen - improve mpi_io interface with bufrlib routines
!   2009-04-21  derber  - add ithin to call to makegrids
!   2009-09-01  li      - add to handle nst fields
!   2010-06-27  kokron  - added test for returned value of bmiss in hdr(15)
!   2010-06-29  zhu     - add newpc4pred option
!   2011-04-07  todling - newpc4pred now in radinfo
!   2011-04-08  li      - (1) use nst_gsi, nstinfo, fac_dtl, fac_tsl and add NSST vars
!                         (2) get zob, tz_tr (call skindepth and cal_tztr)
!                         (3) interpolate NSST Variables to Obs. location (call deter_nst)
!                         (4) add more elements (nstinfo) in data array
!   2011-08-01  lueken  - added module use deter_sfc_mod
!   2012-03-05  akella  - nst now controlled via coupler
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2013-01-26  parrish - question about bmiss and hdr(15).  debug compile execution on WCOSS failed.  
!                           code tests for bmiss==1e9, but a lot of hdr(15) values = 1e11, which
!                          causes integer overflow with current logic.  Made quick fix, but needs review.
!   2013-12-30  sienkiewicz - use BUFR library function 'ibfms' to check for missing value of hdr(15)
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2015-10-01  guo     - consolidate use of ob location (in deg)
!   2018-05-21  j.jin   - added time-thinning. Moved the checking of thin4d into satthin.F90.
!
!   input argument list:
!     mype     - mpi task id
!     val_goes - weighting factor applied to super obs
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window(hours)
!     gstime   - guess time
!     sis      - sensor/instrument/satellite indicator
!     mype_root - "root" task for sub-communicator
!     mype_sub - mpi task id within sub-communicator
!     npe_sub  - number of data read tasks
!     mpi_comm_sub - sub-communicator for data read
!     nrec_start - first subset with useful information
!
!   output argument list:
!     nread    - number of BUFR GOES sounder observations read
!     ndata    - number of BUFR GOES sounder profiles retained for further processing
!     nodata   - number of BUFR GOES sounder observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
      checkob,finalcheck,score_crit
  use satthin, only: radthin_time_info,tdiff2crit
  use obsmod,  only: time_window_max
  use obsmod, only: bmiss
  use radinfo, only: cbias,newchn,predx,iuse_rad,jpch_rad,nusis,ang_rad,air_rad,&
      newpc4pred
  use gridmod, only: diagnostic_reg,nlat,nlon,regional,tll2xy,txy2ll,rlats,rlons
  use constants, only: deg2rad,zero,rad2deg, r60inv,one,two
  use gsi_4dvar, only: l4dvar,l4densvar,time_4dvar,iwinbgn,winlen
  use deter_sfc_mod, only: deter_sfc
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter
  use mpimod, only: npe
! use radiance_mod, only: rad_obs_type

  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: infile,obstype,jsatid
  character(len=20),intent(in  ) :: sis
  integer(i_kind) ,intent(in   ) :: mype,lunout,ithin,nrec_start
  integer(i_kind) ,intent(inout) :: ndata,nodata,nread
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  real(r_kind)    ,intent(in   ) :: rmesh,twind,gstime
  real(r_kind)    ,intent(inout) :: val_goes
  integer(i_kind) ,intent(in   ) :: mype_root
  integer(i_kind) ,intent(in   ) :: mype_sub
  integer(i_kind) ,intent(in   ) :: npe_sub
  integer(i_kind) ,intent(in   ) :: mpi_comm_sub
  logical         ,intent(in   ) :: dval_use

! Declare local parameters
  integer(i_kind),parameter:: mfov=25   ! maximum number of fovs (currently 5x5)

  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind
  character(80),parameter:: hdstr = &
     'CLON CLAT ELEV SOEL BEARAZ SOLAZI SAID DINU YEAR MNTH DAYS HOUR MINU SECO ACAV' 
  character(80),parameter:: hdstr5 = &
     'XOB YOB ELEV SOEL BEARAZ SOLAZI SAID TYP ACAV DHR SID '
  character(80),parameter:: rbstr = 'TMBR'

! Declare local variables
  logical outside,iuse,g5x5,assim

  character(8)  subset

  integer(i_kind) kx,levs,ldetect
  integer(i_kind) lnbufr,nchanl,nreal,iret,ksatid,lsatid
  integer(i_kind) idate,maxinfo
  integer(i_kind) ilat,ilon,isflg,idomsfc
  integer(i_kind) itx,k,i,itt,iskip,l,ifov,n
  integer(i_kind) ichan8,ich8
  integer(i_kind) nele,iscan,nmind
  integer(i_kind) ntest,ireadsb,ireadmg,irec,next
  integer(i_kind),dimension(5):: idate5
  integer(i_kind),allocatable,dimension(:)::nrec
  integer(i_kind) ibfms         ! BUFR missing value function

  real(r_kind) dlon,dlat,emiss,sfcr
  real(r_kind) dlon_earth,dlat_earth
  real(r_kind) dlon_earth_deg,dlat_earth_deg
  real(r_kind) ch8,sstime
  real(r_kind) pred,crit1,tdiff,dist1,toff,t4dv
  real(r_kind) cdist,disterr,disterrmax,dlon00,dlat00,r01

  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr

  real(r_kind),allocatable,dimension(:,:):: data_all

  real(r_double),dimension(15):: hdr
  real(r_double),dimension(18):: grad
  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin,it_mesh


!**************************************************************************

! Start routine here.  Set constants.  Initialize variables
  maxinfo=31
  lnbufr = 10
  disterrmax=zero
  ntest  = 0
  ich8   = 8        !channel 8
  ndata  = 0
  nchanl = 18
  ifov = -999
  r01 = 0.01_r_kind

  ilon=3
  ilat=4

  if (nst_gsi > 0 ) then
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif

  rlndsea(0) = zero
  rlndsea(1) = 15._r_kind
  rlndsea(2) = 10._r_kind
  rlndsea(3) = 15._r_kind
  rlndsea(4) = 30._r_kind

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
  if (.not.assim) val_goes=zero


  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif
! Make thinning grids
  call makegrids(rmesh,ithin,n_tbin=n_tbin)
 
!  check to see if prepbufr file

  g5x5 = jsatid == 'g08_prep' .or. jsatid == 'g09_prep' .or.     &
         jsatid == 'g10_prep' .or. jsatid == 'g11_prep' .or.     &
         jsatid == 'g12_prep' .or. jsatid == 'g13_prep' .or.     &
         jsatid == 'g14_prep' .or. jsatid == 'g15_prep'

  if(g5x5)then
       if(jsatid=='g08_prep')lsatid=252
       if(jsatid=='g09_prep')lsatid=253
       if(jsatid=='g10_prep')lsatid=254
       if(jsatid=='g11_prep')lsatid=255
       if(jsatid=='g12_prep')lsatid=256
       if(jsatid=='g13_prep')lsatid=257
       if(jsatid=='g14_prep')lsatid=258
       if(jsatid=='g15_prep')lsatid=259
   else
       if(jsatid=='g08')lsatid=252
       if(jsatid=='g09')lsatid=253
       if(jsatid=='g10')lsatid=254
       if(jsatid=='g11')lsatid=255
       if(jsatid=='g12')lsatid=256
       if(jsatid=='g13')lsatid=257
       if(jsatid=='g14')lsatid=258
       if(jsatid=='g15')lsatid=259
       if(obstype == 'sndrd1')ldetect = 1
       if(obstype == 'sndrd2')ldetect = 2
       if(obstype == 'sndrd3')ldetect = 3
       if(obstype == 'sndrd4')ldetect = 4
  end if

! Set array index for surface-sensing channels
  ichan8  = newchn(sis, ich8)


! Open then read the bufr data
  open(lnbufr,file=trim(infile),form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)

! Time offset
  call time_4dvar(idate,toff)

! Allocate arrays to hold data
  if(dval_use) maxinfo = maxinfo + 2
  nreal  = maxinfo + nstinfo
  nele   = nreal   + nchanl
  allocate(data_all(nele,itxmax),nrec(itxmax))

! Big loop to read data file
  nrec=999999
  next=0
  irec=0
  read_subset: do while(ireadmg(lnbufr,subset,idate)>=0)
!    Time offset
     if(next == 0)call time_4dvar(idate,toff)
     irec=irec+1
     if(irec < nrec_start) cycle read_subset
     next=next+1
     if(next == npe_sub)next=0
     if(next/=mype_sub)cycle read_subset
     read_loop: do while (ireadsb(lnbufr)==0)

!       Extract type, date, and location information
        if(g5x5)then
!       Prepbufr file
           call ufbint(lnbufr,hdr,11,1,iret,hdstr5)
           kx = hdr(8)
           if(kx /= 164 .and. kx /= 165 .and. kx /= 174 .and. kx /= 175)cycle read_loop
!          If not goes data over ocean , read next bufr record
!          if(kx /= 174 .and. kx /= 175)cycle read_loop

           ksatid=nint(hdr(7))
!          if not proper satellite read next bufr record
           if (ksatid /= lsatid) cycle read_subset

!          Extract number of averaged FOVS
           ifov = hdr(9) ! number of averaged FOVS 
           if(ifov <= 3) cycle read_loop
!          Extract obs time difference. 
           tdiff=hdr(10)  ! relative obs time in hours
           t4dv=toff+tdiff
        else
!          GOES 1x1 or 5x5 file
           call ufbint(lnbufr,hdr,15,1,iret,hdstr)

           ksatid=hdr(7)   !bufr satellite id
!          if not proper satellite/detector read next bufr record
           if (ksatid /=lsatid) cycle read_loop
           if(obstype /= 'sndr')then
              if(ldetect /= nint(hdr(8)))cycle read_loop
           end if

!   test for case when hdr(15) comes back with bmiss signifying 1x1 data
           if (ibfms(hdr(15)) .eq. 1) then
              ifov = 0
           else ! 5x5 data
              ifov = nint(hdr(15)) ! number of averaged FOVS
              if(ifov < mfov .and. ifov > 0)then
                 if(ifov <= 3) cycle read_loop
              end if
           endif

!          Extract obs time.  If not within analysis window, skip obs
!          Extract date information.  If time outside window, skip this obs
           idate5(1) = hdr(9)  !year
           idate5(2) = hdr(10) !month
           idate5(3) = hdr(11) !day
           idate5(4) = hdr(12) !hour
           idate5(5) = hdr(13) !minute
           call w3fs21(idate5,nmind)
           sstime=real(nmind,r_kind) + hdr(14)*r60inv
           tdiff=(sstime-gstime)*r60inv
           t4dv=(real(nmind-iwinbgn,r_kind) + hdr(14)*r60inv)*r60inv
        end if

!       If not within analysis window, skip obs
        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           if (abs(tdiff)>twind) cycle read_loop
        endif

        if(abs(hdr(2))>90._r_kind .or. abs(hdr(1))>r360) cycle read_loop
!       Convert obs location to radians
        if (hdr(1)==r360) hdr(1)=zero
        if (hdr(1)< zero) hdr(1)=hdr(1)+r360

        dlon_earth_deg = hdr(1)
        dlat_earth_deg = hdr(2)
        dlon_earth = hdr(1)*deg2rad   !convert degrees to radians
        dlat_earth = hdr(2)*deg2rad 
        if(regional)then
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
           if(diagnostic_reg) then
              call txy2ll(dlon,dlat,dlon00,dlat00)
              ntest=ntest+1
              cdist=sin(dlat_earth)*sin(dlat00)+cos(dlat_earth)*cos(dlat00)* &
                   (sin(dlon_earth)*sin(dlon00)+cos(dlon_earth)*cos(dlon00))
              cdist=max(-one,min(cdist,one))
              disterr=acos(cdist)*rad2deg
              disterrmax=max(disterrmax,disterr)
           end if
      
!          Check to see if in domain
           if(outside) cycle read_loop

        else
           dlon = dlon_earth 
           dlat = dlat_earth 
           call grdcrd1(dlat,rlats,nlat,1)
           call grdcrd1(dlon,rlons,nlon,1)
        endif


!       Set common predictor parameters

        nread=nread+nchanl
        crit0=0.01_r_kind
        if(ifov < mfov .and. ifov > 0) crit0 = crit0+two*real(mfov-ifov,r_kind)
        timeinflat=6.0_r_kind
        call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)
        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)
        if(.not. iuse)cycle read_loop

!       Increment goes sounder data counter
!       Extract brightness temperatures
        call ufbint(lnbufr,grad,1,18,levs,rbstr)

        iskip = 0
        do l=1,nchanl

           if( grad(l) < tbmin .or. grad(l) > tbmax )then
              iskip = iskip + 1
              if(l == ich8)iskip = nchanl
           endif
        end do

        if( iskip >= nchanl )cycle read_loop

!    "Score" observation.   We use this information to id "best" obs.

!    Locate the observation on the analysis grid.  Get sst and land/sea/ice
!    mask.  

!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed 

        call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
           ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)


!       If not goes data over ocean , read next bufr record
        if(isflg /= 0) cycle read_loop

!   Following lines comment out because only using data over ocean
!       crit1 = crit1 + rlndsea(isflg)  
!       call checkob(dist1,crit1,itx,iuse)
!       if(.not. iuse)cycle read_loop

!       Set data quality predictor
        iscan   = nint(hdr(3))+0.001_r_kind   ! "scan" position
        if (newpc4pred) then
           ch8     = grad(ich8) -ang_rad(ichan8)*cbias(iscan,ichan8) -  &
                                 predx(1,ichan8)*air_rad(ichan8)
        else
           ch8     = grad(ich8) -ang_rad(ichan8)*cbias(iscan,ichan8) -  &
                                 r01*predx(1,ichan8)*air_rad(ichan8)
        end if
        emiss=0.992_r_kind-0.013_r_kind*(hdr(3)/65._r_kind)**3.5_r_kind-0.026_r_kind*(hdr(3)/65._r_kind)**7.0_r_kind
        pred = abs(ch8-tsavg*emiss)

!       Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
        crit1 = crit1+10.0_r_kind*pred  

        call finalcheck(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop
!
!       interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
!
        if ( nst_gsi > 0 ) then
          tref  = ts(0)
          dtw   = zero
          dtc   = zero
          tz_tr = one
          if ( sfcpct(0) > zero ) then
             call gsi_nstcoupler_deter(dlat_earth,dlon_earth,t4dv,zob,tref,dtw,dtc,tz_tr)
          endif
        endif

!       Transfer observation location and other data to local arrays

        data_all(1,itx) = ksatid                       ! satellite id
        data_all(2,itx) = t4dv                         ! time (hours)
        data_all(3,itx) = dlon                         ! grid relative longitude
        data_all(4,itx) = dlat                         ! grid relative latitude
        data_all(5,itx) = hdr(3)*deg2rad               ! satellite zenith angle
        data_all(6,itx) = hdr(5)                       ! satellite zenith angle
        data_all(7,itx) = zero                         ! local zenith angle
        data_all(8,itx) = iscan                        ! "scan" position
        data_all(9,itx) = hdr(4)                       ! solar zenith angle
        data_all(10,itx)= hdr(6)                       ! solar zenith angle
        data_all(11,itx) = sfcpct(0)                   ! sea percentage of
        data_all(12,itx) = sfcpct(1)                   ! land percentage
        data_all(13,itx) = sfcpct(2)                   ! sea ice percentage
        data_all(14,itx) = sfcpct(3)                   ! snow percentage
        data_all(15,itx)= ts(0)                        ! ocean skin temperature
        data_all(16,itx)= ts(1)                        ! land skin temperature
        data_all(17,itx)= ts(2)                        ! ice skin temperature
        data_all(18,itx)= ts(3)                        ! snow skin temperature
        data_all(19,itx)= tsavg                        ! average skin temperature
        data_all(20,itx)= vty                          ! vegetation type
        data_all(21,itx)= vfr                          ! vegetation fraction
        data_all(22,itx)= sty                          ! soil type
        data_all(23,itx)= stp                          ! soil temperature
        data_all(24,itx)= sm                           ! soil moisture
        data_all(25,itx)= sn                           ! snow depth
        data_all(26,itx)= zz                           ! surface height
        data_all(27,itx)= idomsfc + 0.001_r_kind       ! dominate surface type
        data_all(28,itx)= sfcr                         ! surface roughness
        data_all(29,itx)= ff10                         ! ten meter wind factor
        data_all(30,itx)= dlon_earth_deg               ! earth relative longitude (degrees)
        data_all(31,itx)= dlat_earth_deg               ! earth relative latitude (degrees)


        if(dval_use)then
          data_all(32,itx)= val_goes
          data_all(33,itx)= itt
        end if

        if ( nst_gsi > 0 ) then
          data_all(maxinfo+1,itx) = tref         ! foundation temperature
          data_all(maxinfo+2,itx) = dtw          ! dt_warm at zob
          data_all(maxinfo+3,itx) = dtc          ! dt_cool at zob
          data_all(maxinfo+4,itx) = tz_tr        ! d(Tz)/d(Tr)
        endif

        do k=1,nchanl
           data_all(k+nreal,itx)=grad(k)
        end do
        nrec(itx)=irec


     end do read_loop
  end do read_subset
  call closbf(lnbufr)
  close(lnbufr)


! If multiple tasks read input bufr file, allow each tasks to write out
! information it retained and then let single task merge files together

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
     nele,itxmax,nread,ndata,data_all,score_crit,nrec)


! Allow single task to check for bad obs, update superobs sum,
! and write out data to scratch file for further processing.
  if (mype_sub==mype_root.and.ndata>0) then

!    Identify "bad" observation (unreasonable brightness temperatures).
!    Update superobs sum according to observation location

     do n=1,ndata
        do i=1,nchanl
           if(data_all(i+nreal,n) > tbmin .and. &
              data_all(i+nreal,n) < tbmax)nodata=nodata+1
        end do
     end do
     if(dval_use .and. assim)then
        do n=1,ndata
           itt=nint(data_all(33,n))
           super_val(itt)=super_val(itt)+val_goes
        end do
     end if

!    Write final set of "best" observations to output file
     call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif

  deallocate(data_all,nrec) ! Deallocate data arrays
  call destroygrids    ! Deallocate satthin arrays

  if(diagnostic_reg .and. ntest>0 .and. mype_sub==mype_root) &
     write(6,*)'READ_GOESNDR:  mype,ntest,disterrmax=',&
     mype,ntest,disterrmax

  return

end subroutine read_goesndr
