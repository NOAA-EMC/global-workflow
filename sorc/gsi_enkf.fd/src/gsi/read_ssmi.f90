subroutine read_ssmi(mype,val_ssmi,ithin,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs, &
     nrec_start,dval_use)

!$$$  subprogram documentation block
! subprogram:    read_ssmi           read SSM/I  bufr1b data
!   prgmmr: okamoto          org: np23                date: 2003-12-27
!
! abstract:  This routine reads BUFR format SSM/I 1b radiance 
!            (brightness temperature) files.  Optionally, the data 
!            are thinned to a specified resolution using simple 
!            quality control (QC) checks.
!            QC performed in this subroutine are
!             1.obs time check  |obs-anal|<time_window
!             2.remove overlap orbit
!             3.climate check  reject for tb<tbmin or tb>tbmax 
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2003-12-27 okamoto
!   2005-09-08  derber - modify to use input group time window
!   2005-09-28  derber - modify to produce consistent surface info - clean up
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc, refs to sno,sli,sst,isli (not used)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-03-07  derber - correct error in nodata count
!   2006-04-27  derber - some efficiency modifications
!   2006-05-19  eliu    - add logic to reset relative weight when all channels not used
!   2006-07-28  derber  - add solar and satellite azimuth angles remove isflg from output
!   2006-08-25  treadon - replace serial bufr i/o with parallel bufr i/o (mpi_io)
!   2006-12-20  Sienkiewicz - add additional satellites f08 f10 f11
!                             set satellite zenith angle (avail. in Wentz data)
!                             85GHz workaround for f08
!   2007-03-01  Tremolet - tdiff definition had disappeared somehow
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2007-04-24  derber - define tdiff (was undefined)
!   2008-04-17  safford - rm unused vars
!   2009-04-18  woollen - improve mpi_io interface with bufrlib routines
!   2009-04-21  derber  - add ithin to call to makegrids
!   2011-04-08  li      - (1) use nst_gsi, nstinfo, fac_dtl, fac_tsl and add NSST vars
!                         (2) get zob, tz_tr (call skindepth and cal_tztr)
!                         (3) interpolate NSST Variables to Obs. location (call deter_nst)
!                         (4) add more elements (nstinfo) in data array
!   2011-08-01  lueken  - added module use deter_sfc_mod 
!   2012-03-05  akella  - nst now controlled via coupler
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2014-05-02  sienkiewicz- modify gross check screening to allow data to be used with bad ch6, if
!                              ch6 data has been turned off - only toss if do85GHz is true
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2015-10-01  guo     - consolidate use of ob location (in deg)
!   2018-05-21  j.jin   - added time-thinning. Moved the checking of thin4d into satthin.F90.
!
!   input argument list:
!     mype     - mpi task id
!     val_ssmi - weighting factor applied to super obs
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read  ex. 'f15'
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!     mype_root - "root" task for sub-communicator
!     mype_sub - mpi task id within sub-communicator
!     npe_sub  - number of data read tasks
!     mpi_comm_sub - sub-communicator for data read
!     nrec_start - first subset with useful information
!
!   output argument list:
!     nread    - number of BUFR SSM/I observations read (after eliminating orbit overlap)
!     ndata    - number of BUFR SSM/I profiles retained for further processing (thinned)
!     nodata   - number of BUFR SSM/I observations retained for further processing (thinned)
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
      checkob,finalcheck,score_crit
  use satthin, only: radthin_time_info,tdiff2crit
  use obsmod,  only: time_window_max
  use obsmod, only: bmiss
  use radinfo, only: iuse_rad,jpch_rad,nusis,nuchan
  use gridmod, only: diagnostic_reg,regional,rlats,rlons,nlat,nlon,&
      tll2xy,txy2ll
  use constants, only: deg2rad,rad2deg,zero,one,two,three,four,r60inv
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use deter_sfc_mod, only: deter_sfc
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter
  use mpimod, only: npe
! use radiance_mod, only: rad_obs_type

  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: infile,obstype,jsatid
  character(len=20),intent(in  ) :: sis
  integer(i_kind),intent(in   ) :: mype,lunout,ithin,nrec_start
  integer(i_kind),intent(in   ) :: mype_root
  integer(i_kind),intent(in   ) :: mype_sub
  integer(i_kind),intent(in   ) :: npe_sub
  integer(i_kind),intent(in   ) :: mpi_comm_sub
  real(r_kind)   ,intent(in   ) :: rmesh,gstime,twind
  real(r_kind)   ,intent(inout) :: val_ssmi

  integer(i_kind),dimension(npe),intent(inout):: nobs
  integer(i_kind),intent(inout):: nread

  integer(i_kind),intent(inout):: ndata,nodata
  logical        ,intent(in   ):: dval_use

! Declare local parameters
  integer(i_kind),parameter :: n1bhdr=14
  integer(i_kind),parameter :: maxchanl=30

  integer(i_kind),parameter :: ntime=8      !time header
  integer(i_kind),parameter :: nloc=5       !location dat used for ufbint()
  integer(i_kind),parameter :: maxscan=64   !possible max of scan positons
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=70.0_r_kind
  real(r_kind),parameter:: tbmax=320.0_r_kind
  character(80),parameter:: hdr1b='SAID YEAR MNTH DAYS HOUR MINU SECO ORBN'   !use for ufbint()
  character(40),parameter:: str1='CLATH CLONH SFTG POSN SAZA'   !use for ufbint() new
  character(40),parameter:: strx='CLAT  CLON  SFTG POSN SAZA'   !use for ufbint() old
  character(40),parameter:: str2='TMBR'                  !use for ufbrep()

! Declare local variables
  logical ssmi,assim
  logical outside,iuse

  character(8) subset

  integer(i_kind):: i,k,ntest,ireadsb,ireadmg,irec,next
  integer(i_kind):: iret,idate,nchanl
  integer(i_kind):: isflg,nreal,idomsfc
  integer(i_kind):: nmind,itx,nele,itt
  integer(i_kind):: iskip,maxinfo
  integer(i_kind):: lnbufr
  integer(i_kind):: ilat,ilon
  integer(i_kind),allocatable,dimension(:)::nrec

  real(r_kind) sfcr

  real(r_kind) pred
  real(r_kind) sstime,tdiff,t4dv
  real(r_kind) crit1,dist1
  real(r_kind),allocatable,dimension(:,:):: data_all

  real(r_kind) disterr,disterrmax,dlon00,dlat00,cdist

!  ---- bufr argument -----
  real(r_double),dimension(n1bhdr):: bfr1bhdr
  real(r_double),dimension(nloc,maxscan) :: midat  !location data from str1
  real(r_double),dimension(maxchanl*maxscan) :: mirad !TBB from str2



  integer(i_kind) :: nscan,jc,bufsat,js,ij,npos,n
  integer(i_kind),dimension(5):: iobsdate
  real(r_kind):: tb19v,tb22v,tb85v,si85,flgch,q19
  real(r_kind),dimension(maxchanl):: tbob
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr

  real(r_kind):: dlat,dlon,dlon_earth,dlat_earth
  real(r_kind):: dlon_earth_deg,dlat_earth_deg
  real(r_kind):: ssmi_def_ang,ssmi_zen_ang  ! default and obs SSM/I zenith ang
  logical  do85GHz, ch6, ch7
  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin,it_mesh

!**************************************************************************
! Initialize variables
  maxinfo=31
  lnbufr = 15
  disterrmax=zero
  ntest=0

  nchanl = 30
  ndata  = 0
  nodata = 0
  nread  = 0
  ssmi_def_ang = 53.1_r_kind

  ilon=3
  ilat=4

  if(nst_gsi>0) then
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif

! Set various variables depending on type of data to be read

  ssmi  = obstype  == 'ssmi'

  if ( ssmi ) then
     nscan  = 64   !for A-scan
!    nscan  = 128  !for B-scan
     nchanl = 7
     if(jsatid == 'f08')bufsat=241
     if(jsatid == 'f10')bufsat=243
     if(jsatid == 'f11')bufsat=244
     if(jsatid == 'f13')bufsat=246
     if(jsatid == 'f14')bufsat=247
     if(jsatid == 'f15')bufsat=248
     rlndsea(0) = zero
     rlndsea(1) = 30._r_kind
     rlndsea(2) = 30._r_kind
     rlndsea(3) = 30._r_kind
     rlndsea(4) = 100._r_kind
  end if

! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.

  assim=.false.
  ch6=.false.
  ch7=.false.
  search: do i=1,jpch_rad
     if (nusis(i)==sis) then
        if (iuse_rad(i)>=0) then
           if (iuse_rad(i)>0) assim=.true.
           if (nuchan(i)==6) ch6=.true.
           if (nuchan(i)==7) ch7=.true.
           if (assim.and.ch6.and.ch7) exit
        endif
     endif
  end do search
  if (.not.assim) val_ssmi=zero

  do85GHz = .not. assim .or. (ch6.and.ch7)

  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif
! Make thinning grids
  call makegrids(rmesh,ithin,n_tbin=n_tbin)

! Open unit to satellite bufr file
  open(lnbufr,file=trim(infile),form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)

! Allocate arrays to hold data
  if(dval_use)maxinfo=maxinfo+2
  nreal  = maxinfo + nstinfo
  nele   = nreal   + nchanl
  allocate(data_all(nele,itxmax),nrec(itxmax))

! Big loop to read data file
  nrec=999999
  irec=0
  next=0
  read_subset: do while(ireadmg(lnbufr,subset,idate)>=0)
     irec=irec+1
     if(irec < nrec_start) cycle read_subset
     next=next+1
     if(next == npe_sub)next=0
     if(next /= mype_sub)cycle
     read_loop: do while (ireadsb(lnbufr)==0)

! ----- Read header record to extract satid,time information  
!       SSM/I data are stored in groups of nscan, hence the loop.  
        call ufbint(lnbufr,bfr1bhdr,ntime,1,iret,hdr1b)

!       Extract satellite id.  If not the one we want, read next record
        if(nint(bfr1bhdr(1)) /= bufsat) cycle read_subset


!       calc obs seqential time  If time outside window, skip this obs
        iobsdate(1:5) = bfr1bhdr(2:6) !year,month,day,hour,min
        call w3fs21(iobsdate,nmind)
        t4dv=(real(nmind-iwinbgn,r_kind) + real(bfr1bhdr(7),r_kind)*r60inv)*r60inv
        sstime=real(nmind,r_kind) + real(bfr1bhdr(7),r_kind)*r60inv
        tdiff=(sstime-gstime)*r60inv
        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           if(abs(tdiff) > twind)  cycle read_loop
        endif

! ----- Read header record to extract obs location information  
!       SSM/I data are stored in groups of nscan, hence the loop.  

        call ufbint(lnbufr,midat,nloc,nscan,iret,str1)
        if(midat(1,1)>10e8) call ufbint(lnbufr,midat,nloc,nscan,iret,strx)


!---    Extract brightness temperature data.  Apply gross check to data. 
!       If obs fails gross check, reset to missing obs value.

        call ufbrep(lnbufr,mirad,1,nchanl*nscan,iret,str2)

        ij=0
        scan_loop:   do js=1,nscan


!          Regional case
           dlat_earth = midat(1,js)  !deg
           dlon_earth = midat(2,js)  !deg
           if(abs(dlat_earth)>90.0_r_kind .or. abs(dlon_earth)>r360) cycle scan_loop
           if(dlon_earth< zero) dlon_earth = dlon_earth+r360
           if(dlon_earth==r360) dlon_earth = dlon_earth-r360
           dlat_earth_deg = dlat_earth
           dlon_earth_deg = dlon_earth
           dlat_earth = dlat_earth*deg2rad
           dlon_earth = dlon_earth*deg2rad

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

!             Check to see if in domain
!cggg should this be cycle scan_loop?
              if(outside) cycle read_loop

!          Global case
           else
              dlat = dlat_earth  
              dlon = dlon_earth  
              call grdcrd1(dlat,rlats,nlat,1)
              call grdcrd1(dlon,rlons,nlon,1)
           endif

!  If available, set value of ssmi zenith angle
!
           if (midat(5,js) < bmiss ) then
              ssmi_zen_ang = midat(5,js)
           else
              ssmi_zen_ang = ssmi_def_ang
           endif

        
!          Transfer observed brightness temperature to work array.  
!          If any temperature exceeds limits, reset observation to "bad" value
!          mirad(1:maxchanl*nscan) => data1b(1:6+nchanl)
           iskip=0
           do jc=1,nchanl
              ij = ij+1
              if(mirad(ij)<tbmin .or. mirad(ij)>tbmax ) then
                 iskip = iskip + 1
                 if(jc == 1 .or. jc == 3)iskip=iskip+nchanl
                 if(jc == 6 .and. do85GHz)iskip=iskip+nchanl    ! skip on bad ch6 only if using 85GHz data
              else
                 nread=nread+1
              end if
              tbob(jc) = mirad(ij) 
  
           end do   !jc_loop
           if(iskip >= nchanl)  cycle scan_loop  !if all ch for any position is bad, skip 
           flgch = iskip*two   !used for thinning priority range 0-14

!          Map obs to thinning grid
           crit0 = 0.01_r_kind
           timeinflat=6.0_r_kind
           call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)
           call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)
           if(.not. iuse)cycle scan_loop


!          Locate the observation on the analysis grid.  Get sst and land/sea/ice
!          mask.  

!       isflg    - surface flag
!                  0 sea
!                  1 land
!                  2 sea ice
!                  3 snow
!                  4 mixed                     

           call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
              ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)

  
           crit1 = crit1 + rlndsea(isflg)
           call checkob(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle scan_loop

           if (do85GHz) then  ! do regular checks if 85 GHz available
  
!    ---- Set data quality predictor for initial qc -------------
!      -  simple si index : taken out from ssmiqc()
!            note! it exclude emission rain
              tb19v=tbob(1);  tb22v=tbob(3); tb85v=tbob(6)
              if(isflg/=0)  then !land+snow+ice
                 si85 = 451.9_r_kind - 0.44_r_kind*tb19v - 1.775_r_kind*tb22v + &
                    0.00574_r_kind*tb22v*tb22v - tb85v
              else    !sea
                 si85 = -174.4_r_kind + 0.715_r_kind*tb19v + 2.439_r_kind*tb22v -  &
                    0.00504_r_kind*tb22v*tb22v - tb85v
              end if

!             Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
              pred = abs(si85)*three  !range: 0 to 30

           else              ! otherwise do alternate check for rain

              if (isflg/=0) then     ! just try to scren out land pts.
                 pred = 50_r_kind
              else
                 tb19v=tbob(1);  tb22v=tbob(3);
                 if (tb19v < 288.0_r_kind .and. tb22v < 288.0_r_kind) then
                    q19 = -6.723_r_kind * ( log(290.0_r_kind - tb19v)  &
                         - 2.850_r_kind - 0.405_r_kind* log(290.0_r_kind - tb22v))
                    pred = min(75._r_kind * q19,50.)  ! scale 0.4mm -> pred ~ 30
                 else
                    pred = 50_r_kind      ! default if Tb 19/22 > 288
                 endif
              endif
           endif

!          Compute final "score" for observation.  All scores>=0.0.  
!          Lowest score is "best"
           crit1 = crit1 + pred 

           call finalcheck(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle scan_loop

           npos = (midat(4,js)-one)/four+one !original scan position 1.0->253.0 =4*(n-1)+1
!
!          interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
!
           if(nst_gsi>0) then
              tref  = ts(0)
              dtw   = zero
              dtc   = zero
              tz_tr = one
              if(sfcpct(0)>zero) then
                 call gsi_nstcoupler_deter(dlat_earth,dlon_earth,t4dv,zob,tref,dtw,dtc,tz_tr)
              endif
           endif

!          Transfer observation parameters to output array.  
           data_all( 1,itx) = bufsat              ! satellite id
           data_all( 2,itx) = t4dv                ! time diff between obs and anal (min)
           data_all( 3,itx) = dlon                ! grid relative longitude
           data_all( 4,itx) = dlat                ! grid relative latitude
           data_all( 5,itx) = ssmi_zen_ang*deg2rad ! local zenith angle (rad)
           data_all( 6,itx) = 999.00_r_kind       ! local azimuth angle (missing)
           data_all( 7,itx) = zero                ! look angle (rad)
!+>        data_all( 7,itx) =  45.0*deg2rad       ! look angle (rad)
           data_all( 8,itx) = npos                ! scan position 1->64
           data_all( 9,itx) = zero                ! solar zenith angle (deg) : not used
           data_all(10,itx) = 999.00_r_kind       ! solar azimuth angle (missing) : not used
           data_all(11,itx) = sfcpct(0)           ! sea percentage of
           data_all(12,itx) = sfcpct(1)           ! land percentage
           data_all(13,itx) = sfcpct(2)           ! sea ice percentage
           data_all(14,itx) = sfcpct(3)           ! snow percentage
           data_all(15,itx)= ts(0)                ! ocean skin temperature
           data_all(16,itx)= ts(1)                ! land skin temperature
           data_all(17,itx)= ts(2)                ! ice skin temperature
           data_all(18,itx)= ts(3)                ! snow skin temperature
           data_all(19,itx)= tsavg                ! average skin temperature
           data_all(20,itx)= vty                  ! vegetation type
           data_all(21,itx)= vfr                  ! vegetation fraction
           data_all(22,itx)= sty                  ! soil type
           data_all(23,itx)= stp                  ! soil temperature
           data_all(24,itx)= sm                   ! soil moisture
           data_all(25,itx)= sn                   ! snow depth
           data_all(26,itx)= zz                   ! surface height
           data_all(27,itx)= idomsfc + 0.001_r_kind ! dominate surface type
           data_all(28,itx)= sfcr                 ! surface roughness
           data_all(29,itx)= ff10                 ! ten meter wind factor
           data_all(30,itx)= dlon_earth_deg       ! earth relative longitude (degrees)
           data_all(31,itx)= dlat_earth_deg       ! earth relative latitude (degrees)
           if(dval_use)then
              data_all(32,itx)= val_ssmi
              data_all(33,itx)= itt
           end if

           if(nst_gsi>0) then
              data_all(maxinfo+1,itx) = tref       ! foundation temperature
              data_all(maxinfo+2,itx) = dtw        ! dt_warm at zob
              data_all(maxinfo+3,itx) = dtc        ! dt_cool at zob
              data_all(maxinfo+4,itx) = tz_tr      ! d(Tz)/d(Tr)
           endif

           do i=1,nchanl
              data_all(i+nreal,itx)=tbob(i)
           end do
           nrec(itx)=irec
        end do  scan_loop    !js_loop end

     end do read_loop
  end do read_subset
  call closbf(lnbufr)

! If multiple tasks read input bufr file, allow each tasks to write out
! information it retained and then let single task merge files together

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
     nele,itxmax,nread,ndata,data_all,score_crit,nrec)

  write(6,*) 'READ_SSMI: after combine_obs, nread,ndata is ',nread,ndata

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
           super_val(itt)=super_val(itt)+val_ssmi
        end do
     end if

!    Write final set of "best" observations to output file
     call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif

! Deallocate data arrays
  deallocate(data_all,nrec)


! Deallocate satthin arrays
1000 continue
  call destroygrids

  if(diagnostic_reg .and. ntest>0 .and. mype_sub==mype_root) &
     write(6,*)'READ_SSMI:  mype,ntest,disterrmax=',&
        mype,ntest,disterrmax

! End of routine
 return
end subroutine read_ssmi
