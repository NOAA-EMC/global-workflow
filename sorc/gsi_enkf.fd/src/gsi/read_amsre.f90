subroutine read_amsre(mype,val_amsre,ithin,isfcalc,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs,nrec_start,dval_use)

! subprogram:    read_amsre                  read bufr format amsre data
! prgmmr :   okamoto         org: np20                date: 2004-10-12
!
! abstract:  This routine reads BUFR format AQUA radiance (brightness
!            temperature) files.  Optionally, the data are thinned to 
!            a specified resolution using simple quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! Special Notes:
!     10/14/04  okamoto  looks like AMSRE overlap problem is not as bad as SSM/I
!
! program history log:
!   2004-10-12  okamoto
!   2005-10-07  Xu and Pawlak - modify the code related to ityp determination to
!                  use routine deter_ityp, created special notes section, removed
!                  GrADS specific code, fixed indentation
!   2005-10-10  treadon - replace deter_ityp with deter_sfc, modify rlndsea to be
!                         consistent with other read_* routines
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-10-20  kazumori - modify to read real AMSR-E bufr data and add zensun
!   2005-11-17  kazumori - add deter_sfc_amsre_low for AMSR-E low frequency channel
!   2005-11-29  parrish -  modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-02  kazumori - modify the threshold of surface determination and change
!                          the origin of satellite azimuth angle for diag file
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-04-26  kazumori - bug fix of order of polarization, timedif, tbmax
!   2006-05-17  kazumori - modify for new bufrtable change
!   2006-05-19  eliu    - add logic to reset relative weight when all channels not used
!   2006-08-05  kazumori - add good fov selection for amsre low channel data
!   2006-09-20  treadon - remove good fov selection for amsre low channel
!                         data in order to add mpi_io for data read
!   2006-10-22  kazumori - bug fix for the type of zensun subroutine argument
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2008-05-28  safford - rm unused vars
!   2009-04-18  woollen - improve mpi_io interface with bufrlib routines
!   2009-04-21  derber  - add ithin to call to makegrids
!   2009-09-01  li      - add to handle nst fields and read/use data over water only
!   2009-12-20  gayno - add option to calculate surface fields based on
!                       the size/shape of field of view.
!   2010-01-28  derber - move calculation of sun glint angle to setuprad
!   2010-02-25  collard - move where nread is calculated to before thinning
!                         step (more consistent with other obs).
!   2010-03-22  collard - ensure solar azimuth is in the range 0-360 degrees.
!   2011-04-08  li      - (1) use nst_gsi, nstinfo, fac_dtl, fac_tsl and add NSST vars
!                         (2) get zob, tz_tr (call skindepth and cal_tztr)
!                         (3) interpolate NSST Variables to Obs. location (call deter_nst)
!                         (4) add more elements (nstinfo) in data array
!   2011-08-01  lueken  - move deter_sfc_amsre_low to new module deter_sfc_mod,
!                         fix indentation
!   2011-09-13  gayno   - improve error handling for FOV-based sfc calculation
!                         (isfcalc=1)
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2013-02-13  eliu    - bug fix for solar zenith calculation 
!   2012-03-05  akella  - nst now controlled via coupler
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2015-10-01  guo     - consolidate use of ob location (in deg)
!   2018-05-21  j.jin    - added time-thinning. Moved the checking of thin4d into satthin.F90.
!
! input argument list:
!     mype     - mpi task id
!     val_amsre- weighting factor applied to super obs
!     ithin    - flag to thin data
!     isfcalc  - flag to specify method to calculate sfc fields within FOV
!                when set to one, account for size/shape of FOV.  otherwise
!                use bilinear interpolation.
!     rmesh    - thinning mesh size (km)
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
! output argument list:
!     nread    - number of BUFR AQUA observations read
!     ndata    - number of BUFR AQUA profiles retained for further processing
!     nodata   - number of BUFR AQUA observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
      checkob,finalcheck,score_crit
  use satthin, only: radthin_time_info,tdiff2crit
  use obsmod,  only: time_window_max
  use radinfo, only: iuse_rad,nusis,jpch_rad
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,rlats,rlons,&
      tll2xy
  use constants, only: deg2rad,zero,one,three,r60inv
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use calc_fov_conical, only: instrument_init
  use deter_sfc_mod, only: deter_sfc_fov,deter_sfc,deter_sfc_amsre_low
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter
  use mpimod, only: npe
! use radiance_mod, only: rad_obs_type

  implicit none

! Input variables
  character(len=*) ,intent(in   ) :: infile
  character(len=*) ,intent(in   ) :: obstype,jsatid
  integer(i_kind)  ,intent(in   ) :: mype,nrec_start
  integer(i_kind)  ,intent(inout) :: isfcalc
  integer(i_kind)  ,intent(in   ) :: ithin
  integer(i_kind)  ,intent(in   ) :: lunout
  real(r_kind)     ,intent(inout) :: val_amsre
  real(r_kind)     ,intent(in   ) :: gstime,twind
  real(r_kind)     ,intent(in   ) :: rmesh
  character(len=20),intent(in   ) :: sis
  integer(i_kind)  ,intent(in   ) :: mype_root
  integer(i_kind)  ,intent(in   ) :: mype_sub
  integer(i_kind)  ,intent(in   ) :: npe_sub
  integer(i_kind)  ,intent(in   ) :: mpi_comm_sub
  logical          ,intent(in   ) :: dval_use

! Output variables
  integer(i_kind)  ,intent(inout) :: nread
  integer(i_kind),dimension(npe)  ,intent(inout) :: nobs
  integer(i_kind)  ,intent(inout) :: ndata,nodata

! Number of channels for sensors in BUFR
  integer(i_kind),parameter :: N_AMSRCH  =  12
! integer(i_kind),parameter :: N_MAXCH   =  20
  integer(i_kind) :: said, AQUA_SAID  = 784  !WMO satellite identifier 
  integer(i_kind) :: siid, AMSRE_SIID = 345  !WMO instrument identifier 

! BUFR file sequencial number
  character(len=8)  :: subset
  character(len=4)  :: senname
  integer(i_kind)   :: lnbufr = 10
  integer(i_kind)   :: nchanl
  integer(i_kind)   :: iret,isflg,idomsfc(1)

! Work variables for time
  integer(i_kind)   :: idate
  integer(i_kind)   :: idate5(5)
  integer(i_kind)   :: nmind
  real(r_kind)      :: sstime, tdiff, t4dv

! Other work variables
  logical           :: outside,iuse,assim
  integer(i_kind)   :: nreal, kidsat
  integer(i_kind)   :: itx, k, nele, itt
  integer(i_kind)   :: ifov, ilat, ilon
  integer(i_kind)   :: i, l, n
  integer(i_kind),dimension(n_amsrch) :: kchamsre
  real(r_kind)     :: sfcr
  real(r_kind)     :: dlon, dlat
  real(r_kind)     :: dlon_earth,dlat_earth
  real(r_kind)     :: pred, crit1, dist1
  real(r_kind),allocatable,dimension(:,:):: data_all
  integer(i_kind),allocatable,dimension(:)::nrec
  integer(i_kind):: irec,next
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),dimension(0:3):: ts
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr

  character(len=7),parameter:: fov_flag="conical"
  character(len=3) :: fov_satid
  
  integer(i_kind) :: ichan, instr, idum

  logical :: valid

  real(r_kind) :: clath_sun_glint_calc , clonh_sun_glint_calc 
  real(r_kind) :: date5_4_sun_glint_calc
  real(r_kind) :: expansion, dlat_earth_deg, dlon_earth_deg

! Set standard parameters
  logical       :: amsre_low
  logical       :: amsre_mid
  logical       :: amsre_hig
  integer(i_kind) ntest,maxinfo
  integer(i_kind) :: nscan,iskip,kskip,kch,kchanl
! real(r_kind),parameter :: TEN      =  10._r_kind
! real(r_kind),parameter :: R45      =  45._r_kind
  real(r_kind),parameter :: R90      =  90._r_kind
! real(r_kind),parameter :: R180     = 180._r_kind
  real(r_kind),parameter :: R360     = 360._r_kind
  real(r_kind),parameter :: tbmin    = 70._r_kind
  real(r_kind),parameter :: tbmax    = 330._r_kind       !tbmax is larger than same as ssmiqc
  real(r_kind) disterrmax
  real(r_kind),dimension(N_AMSRCH) :: tbob_org
  real(r_kind) :: clath, clonh, fovn, saza, soza
  real(r_kind) :: flgch  !used for thinning priority  range:1-36

! AMSR-E-bufr
! BUFR format for AQUASPOT
! integer(i_kind),parameter :: N_AQUASPOT_LIST = 25

! BUFR format for AMSRSPOT
  integer(i_kind),parameter :: N_AMSRSPOT_LIST = 12

! BUFR format for AMSRCHAN
  integer(i_kind),parameter :: N_AMSRCHAN_LIST = 4

! Variables for BUFR IO
  real(r_double),dimension(3):: aquaspot_d
  real(r_double),dimension(12):: amsrspot_d
  real(r_double),dimension(4,12):: amsrchan_d
  real(r_double),dimension(2,5):: amsrdice_latlon
  real(r_double),dimension(20) :: amsrdice_tmbr

! ---- sun glint ----
  integer(i_kind) doy,mlen(12),mday(12),mon,m
  real(r_kind) bearaz,sun_zenith,sun_azimuth
  data  mlen/31,28,31,30,31,30, &
             31,31,30,31,30,31/ 

! Orbit
! logical :: remove_ovlporbit = .true. !looks like AMSRE overlap problem is not as bad as SSM/I 10/14/04  kozo
  integer(i_kind) :: orbit, old_orbit, iorbit, ireadsb, ireadmg
  real(r_kind) :: saz
  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin,it_mesh

! data selection

  maxinfo    =  31
! Initialize variables
  ilon = 3
  ilat = 4

  if (nst_gsi > 0 ) then
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif

  m = 0
  do mon=1,12 
     mday(mon) = m 
     m = m + mlen(mon) 
  end do 
  disterrmax=zero
  ntest = 0
  if(dval_use) maxinfo = maxinfo + 2
  nreal = maxinfo+nstinfo
  ndata = 0
  nodata = 0
  amsre_low=     obstype == 'amsre_low'
  amsre_mid=     obstype == 'amsre_mid'
  amsre_hig=     obstype == 'amsre_hig'
  orbit = -1
  old_orbit=-1
  iorbit = 0
  sstime = zero
  if(amsre_low)then
     kchanl=4
     kchamsre(1:4)=(/1,2,3,4/)
  else if(amsre_mid) then
     kchanl=6
     kchamsre(1:6)=(/5,6,7,8,9,10/)
  else if(amsre_hig)then
     kchanl=2
     kchamsre(1:2)=(/11,12/)
  end if

  if(amsre_low .or. amsre_mid .or. amsre_hig)then
     senname = 'AMSR'
     nchanl  = N_AMSRCH
     nscan  = 196  !for low frequency ch
!    nscan  = 392  !for 89.0GHz ch
     kidsat = 549
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
  endif

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
  if (.not.assim) val_amsre=zero

! note, fov-based surface code does not have equations for amsr-e, but
! the fov size/shape is similar to ssmi/s.  so call
! fov code using f16 specs.

  if(isfcalc==1)then
     instr=26
     fov_satid='f16'
     idum = -999  ! dummy variable for fov number. not used for conical instr.
     if(amsre_hig)then
        ichan=18
        expansion=1.5_r_kind
     elseif(amsre_mid)then
        ichan=3
        expansion=2.9_r_kind
     elseif(amsre_low)then
        ichan=12
        expansion=2.9_r_kind
     endif
     call instrument_init(instr, fov_satid, expansion, valid)
     if (.not. valid) then
       if (assim) then
         write(6,*)'READ_AMSRE:  ***ERROR*** IN SETUP OF FOV-SFC CODE. STOP'
         call stop2(71)
       else
         isfcalc = 0
         write(6,*)'READ_AMSRE:  ***ERROR*** IN SETUP OF FOV-SFC CODE'
       endif
    endif
  endif

  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif
! Make thinning grids
  call makegrids(rmesh,ithin,n_tbin=n_tbin)


! Open BUFR file
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)

! Allocate local array to contain observation information
  nele=nreal+nchanl
  allocate(data_all(nele,itxmax),nrec(itxmax))

! Big loop to read data file
  next=0
  nrec=999999
  irec=0
  read_msg: do while(ireadmg(lnbufr,subset,idate)>=0)
     irec=irec+1
     if(irec < nrec_start) cycle read_msg
     next=next+1
     if(next == npe_sub)next=0
     if(next /= mype_sub)cycle
     read_loop: do while (ireadsb(lnbufr)==0)

!    Retrieve bufr 1/4 :get aquaspot (said,orbn,soza)
        call ufbint(lnbufr,aquaspot_d,3,1,iret,'SAID ORBN SOZA')

        said = nint(aquaspot_d(1))
        if(said /= AQUA_SAID)  cycle read_loop

!       Retrieve bufr 2/4 :get amsrspot (siid,ymdhs,lat,lon)
        call ufbrep(lnbufr,amsrspot_d,N_AMSRSPOT_LIST,1,iret, &
           'SIID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA BEARAZ FOVN')

        siid = nint(amsrspot_d(1)) 
        if(siid /= AMSRE_SIID)  cycle read_loop

!       Check obs time
        idate5(1) = amsrspot_d(02)! year
        idate5(2) = amsrspot_d(03)! month
        idate5(3) = amsrspot_d(04)! day
        idate5(4) = amsrspot_d(05)! hour
        idate5(5) = amsrspot_d(06)! min
        if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
            idate5(2) < 1    .or. idate5(2) >   12 .or. &
            idate5(3) < 1    .or. idate5(3) >   31 .or. &
            idate5(4) < 0    .or. idate5(4) >   24 .or. &
            idate5(5) < 0    .or. idate5(5) >   60 )then
            write(6,*)'READ_AMSRE:  ### ERROR IN READING BUFR DATA:', &
              ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
            cycle read_loop
        endif
        call w3fs21(idate5,nmind)
        t4dv = (real((nmind-iwinbgn),r_kind) + amsrspot_d(7)*r60inv)*r60inv ! add in seconds
        sstime = real(nmind,r_kind) + amsrspot_d(7)*r60inv ! add in seconds
        tdiff  = (sstime - gstime)*r60inv
        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           if (abs(tdiff)>twind) cycle read_loop
        endif

!     --- Check observing position -----
        if(amsre_low .or. amsre_mid) then
           clath= amsrspot_d(08)
           clonh= amsrspot_d(09)
        else if(amsre_hig)then
           call ufbrep(lnbufr,amsrdice_latlon,2, 5,iret,'CLATH CLONH')
           clath = amsrdice_latlon(1,4)
           clonh = amsrdice_latlon(2,4)
        endif
        if( abs(clath) > R90  .or. abs(clonh) > R360 .or. &
          ( abs(clath) == R90 .and. clonh /= ZERO) )  then
!           write(6,*)'READ_AMSRE:  ### ERROR IN READING BUFR DATA:',&
!              ' STRANGE OBS POINT (LAT,LON):', clath, clonh
            cycle read_loop
        endif

!    Pick up every three scene  3,6,9,,,,195 (num=65)
!    because of too small scene size and too many scene numbers
!    (low-freq ch FOV are overlapped side by side)
        fovn = amsrspot_d(12)

     
!    Set position in a given region
        if(clonh >= R360)then
           clonh = clonh - R360
        else if(clonh < ZERO)then
           clonh = clonh + R360
        endif
     
!    If regional, map obs lat,lon to rotated grid.
        dlat_earth = clath * deg2rad
        dlon_earth = clonh * deg2rad
        dlat_earth_deg = clath
        dlon_earth_deg = clonh
        if(regional)then
        
!       Convert to rotated coordinate.  dlon centered on 180 (pi),
!       so always positive for limited area
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)

!       Check to see if in domain.  outside=.true. if dlon_earth,
!       dlat_earth outside domain, =.false. if inside
           if(outside) cycle read_loop

!    Global case 
        else
           dlat=dlat_earth
           dlon=dlon_earth
           call grdcrd1(dlat,rlats,nlat,1)
           call grdcrd1(dlon,rlons,nlon,1)
        endif

!    Sum number of read obs before thinning step.  Note that this number will contain
!    some observations that may be rejected later due to bad BTs.
        nread=nread+kchanl
    
        crit0 = 0.01_r_kind
        timeinflat=6.0_r_kind
        call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)
        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)
        if (.not.iuse) cycle read_loop

!    QC:  "Score" observation.  We use this information to identify "best" obs

!       Locate the observation on the analysis grid.  Get sst and land/sea/ice
!       mask   

!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed                       

!       When isfcalc is one, calculate surface fields based on size/shape of fov.
!       Otherwise, use bilinear interpolation.

        if (isfcalc==1)then
           call deter_sfc_fov(fov_flag,idum,instr,ichan,real(amsrspot_d(11),r_kind),dlat_earth_deg,&
              dlon_earth_deg,expansion,t4dv,isflg,idomsfc(1), &
              sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
        else
           call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc(1),sfcpct, &
              ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
           if (amsre_low) then
              call deter_sfc_amsre_low(dlat_earth,dlon_earth,isflg,sfcpct)
           endif
           if(isflg/=0) cycle read_loop                            ! use data over water only
        endif

        crit1 = crit1 +rlndsea(isflg)
        call checkob(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

!       Retrieve bufr 3/4 : get amsrchan (chnm,tbb)
        call ufbrep(lnbufr,amsrchan_d,N_AMSRCHAN_LIST,12,iret,'CHNM LOGRCW ACQF TMBR')

!       Retrieve bufr 4/4 : get amsrfovn (fovn)
        call ufbrep(lnbufr,amsrdice_tmbr,1,20,iret,'TMBR')

        tbob_org(1)=amsrchan_d(4,2)
        tbob_org(2)=amsrchan_d(4,1)
        tbob_org(3)=amsrchan_d(4,4)
        tbob_org(4)=amsrchan_d(4,3)
        tbob_org(5)=amsrchan_d(4,6)
        tbob_org(6)=amsrchan_d(4,5)
        tbob_org(7)=amsrchan_d(4,8)
        tbob_org(8)=amsrchan_d(4,7)
        tbob_org(9)=amsrchan_d(4,10)
        tbob_org(10)=amsrchan_d(4,9)
        tbob_org(11)=amsrdice_tmbr(18)
        tbob_org(12)=amsrdice_tmbr(17)

!       Set obs information

        iskip = 0
        do l=1,nchanl
           if(tbob_org(l)<tbmin .or. tbob_org(l)>tbmax)then
              iskip = iskip + 1
           end if
        end do
        kskip = 0
        do l=1,kchanl
           kch=kchamsre(l)
           if(tbob_org(kch)<tbmin .or. tbob_org(kch)>tbmax)then
              kskip = kskip + 1
           endif
        end do
        if(kskip == kchanl .or. iskip == nchanl) cycle read_loop
        flgch=iskip*three  !used for thin, range 0 to 36
        crit1 = crit1 + flgch

!    Set data quality predictor ***NEED TO COME UP WITH THIS***
        pred = zero


!    Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"

!    Map obs to grids
        crit1 = crit1+pred
        call finalcheck(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop
     
        soza = aquaspot_d(3)     !solar zenith angle
!    Check observational info 
        if( soza < -180._r_kind .or. soza > 180._r_kind )then
           write(6,*)'READ_AMSRE:  ### ERROR IN READING BUFR DATA:', &
              ' STRANGE OBS INFO(FOV,SAZA,SOZA):', fovn, saza, soza
           cycle read_loop
        endif

!  -------- Retreive Sun glint angle -----------
        doy = mday( int(idate5(2)) ) + int(idate5(3))
        if ((mod( int(idate5(1)),4)==0).and.( int(idate5(2)) > 2))  then 
           doy = doy + 1
        end if 

        ifov = nint(fovn)
        bearaz=amsrspot_d(11)-180.0_r_kind    

        clath_sun_glint_calc = clath
        clonh_sun_glint_calc = clonh
        if(clonh>180_r_kind) clonh_sun_glint_calc = clonh -360.0_r_kind
!       date5_4_sun_glint_calc = idate5(4)  
        date5_4_sun_glint_calc =  &                                                                                                
        real(idate5(4),r_kind)+real(idate5(5),r_kind)*r60inv+real(amsrspot_d(7),r_kind)*r60inv*r60inv   
     
        call zensun(doy,date5_4_sun_glint_calc,clath_sun_glint_calc,clonh_sun_glint_calc,sun_zenith,sun_azimuth)

        if(amsre_low .or. amsre_mid) then
           saz = 55.0_r_kind*deg2rad   ! satellite zenith angle (rad) 
        else if (amsre_hig) then
           saz = 54.5_r_kind*deg2rad   ! satellite zenith angle (rad) 
        end if
!          saz = amsrspot(10,1)*deg2rad   ! satellite zenith angle (rad) 
!             ==> not use this value but fixed values(55.0 deg)   10/12/04
!             because BUFR saza value looks strange (raging -3 to 25),
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

        data_all(1,itx) = 49                         ! satellite ID
        data_all(2,itx) = t4dv                       ! time diff (obs - anal) (hours)
        data_all(3,itx) = dlon                       ! grid relative longitude
        data_all(4,itx) = dlat                       ! grid relative latitude
        data_all(5,itx) = saz                        ! satellite zenith angle (rad)
        data_all(6,itx) = amsrspot_d(11)             ! satellite azimuth angle
        data_all(7,itx) = zero                       ! look angle (rad)
!       data_all(8,itx) = ifov                       ! fov number    1-196
        data_all(8,itx) = ifov/3 + 1                 ! fov number/3  1-65 !kozo
        data_all(9,itx) = sun_zenith                 ! solar zenith angle (deg)
        data_all(10,itx)= sun_azimuth                ! solar azimuth angle (deg)
        data_all(11,itx) = sfcpct(0)                 ! sea percentage of
        data_all(12,itx) = sfcpct(1)                 ! land percentage
        data_all(13,itx) = sfcpct(2)                 ! sea ice percentage
        data_all(14,itx) = sfcpct(3)                 ! snow percentage
        data_all(15,itx)= ts(0)                      ! ocean skin temperature
        data_all(16,itx)= ts(1)                      ! land skin temperature
        data_all(17,itx)= ts(2)                      ! ice skin temperature
        data_all(18,itx)= ts(3)                      ! snow skin temperature
        data_all(19,itx)= tsavg                      ! average skin temperature
        data_all(20,itx)= vty                        ! vegetation type
        data_all(21,itx)= vfr                        ! vegetation fraction
        data_all(22,itx)= sty                        ! soil type
        data_all(23,itx)= stp                        ! soil temperature
        data_all(24,itx)= sm                         ! soil moisture
        data_all(25,itx)= sn                         ! snow depth
        data_all(26,itx)= zz                         ! surface height
        data_all(27,itx)= idomsfc(1) + 0.001_r_kind  ! dominate surface type
        data_all(28,itx)= sfcr                       ! surface roughness
        data_all(29,itx)= ff10                       ! ten meter wind factor
        data_all(30,itx)= dlon_earth_deg             ! earth relative longitude (degrees)
        data_all(31,itx)= dlat_earth_deg             ! earth relative latitude (degrees)

        if(dval_use)then
           data_all(32,itx)= val_amsre
           data_all(33,itx)= itt
        end if

        if ( nst_gsi > 0 ) then
           data_all(maxinfo+1,itx) = tref                ! foundation temperature
           data_all(maxinfo+2,itx) = dtw                 ! dt_warm at zob
           data_all(maxinfo+3,itx) = dtc                 ! dt_cool at zob
           data_all(maxinfo+4,itx) = tz_tr               ! d(Tz)/d(Tr)
        endif

        do l=1,nchanl
           data_all(l+nreal,itx) = tbob_org(l)
        end do
        nrec(itx)=irec


     enddo read_loop
  enddo read_msg
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
           super_val(itt)=super_val(itt)+val_amsre

        end do
     end if

!    Write final set of "best" observations to output file
     call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif

  deallocate(data_all,nrec) ! Deallocate data arrays
  call destroygrids    ! Deallocate satthin arrays

  if(diagnostic_reg.and.ntest>0 .and. mype_sub==mype_root) &
     write(6,*)'READ_AMSRE:  ',&
        'mype,ntest,disterrmax=',mype,ntest,disterrmax

  return
end subroutine read_amsre

subroutine zensun(day,time,lat,lon,sun_zenith,sun_azimuth)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  zensun         make sun zenith and sun azimuth angle
!
!   prgmmr: Paul Ricchiazzi org: Earth Space Research Group,UCSB  date: 1992-10-23
!
! abstract: 
!       Compute solar position information as a function of
!      geographic coordinates, date and time.
!
! program history log:
!   2005-10-21  kazumori - reformatted for GSI
!   2013-02-13  eliu    - bug fix for solar zenith calculation 
!   2014-06-04  sienkiewicz - take out doy**3 and replace with simple
!                               linear interpolation of tables
!
!   input argument list:
!     day -     Julian day (positive scalar or vector)
!               (spring equinox =  80)
!               (summer solstice= 171)
!               (fall equinox   = 266)
!               (winter solstice= 356)
!     time -    Universal Time in hours (scalar or vector)
!     lat  -    geographic latitude of point on earth's surface (degrees)
!     lon  -    geographic longitude of point on earth's surface (degrees)
!
!   output argument list:
!     sun_zenith  - solar zenith angle
!     sun_azimuth - solar azimuth angle
!
!   comments:
!
!
!     PROCEDURE:
!
!  1. Calculate the subsolar point latitude and longitude, based on
!     DAY and TIME. Since each year is 365.25 days long the exact
!     value of the declination angle changes from year to year.  For
!     precise values consult THE AMERICAN EPHEMERIS AND NAUTICAL
!     ALMANAC published yearly by the U.S. govt. printing office.  The
!     subsolar coordinates used in this code were provided by a
!     program written by Jeff Dozier.
!
!  2. Given the subsolar latitude and longitude, spherical geometry is
!     used to find the solar zenith, azimuth and flux multiplier.
!
!  eqt = equation of time (minutes)  ! solar longitude correction = -15*eqt
!  dec = declination angle (degrees) = solar latitude
!
! LOWTRAN v7 data (25 points)
!     The LOWTRAN solar position data is characterized by only 25 points.
!     This should predict the subsolar angles within one degree.  For
!     increased accuracy add more data points.
!
!nday=[   1.,    9.,   21.,   32.,   44.,   60.,  91.,  121.,  141.,  152.,$
!       160.,  172.,  182.,  190.,  202.,  213., 244.,  274.,  305.,  309.,$
!       325.,  335.,  343.,  355.,  366.]
!
!eqt=[ -3.23, -6.83,-11.17,-13.57,-14.33,-12.63, -4.2,  2.83,  3.57,  2.45,$
!       1.10, -1.42, -3.52, -4.93, -6.25, -6.28,-0.25, 10.02, 16.35, 16.38,$
!       14.3, 11.27,  8.02,  2.32, -3.23]
!
!dec=[-23.07,-22.22,-20.08,-17.32,-13.62, -7.88, 4.23, 14.83, 20.03, 21.95,$
!      22.87, 23.45, 23.17, 22.47, 20.63, 18.23, 8.58, -2.88,-14.18,-15.45,$
!     -19.75,-21.68,-22.75,-23.43,-23.07]
!
! Analemma information from Jeff Dozier
!     This data is characterized by 74 points
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!
  use kinds, only: r_single,r_kind,i_kind
  use constants, only: deg2rad,rad2deg,one,r60inv,zero

  implicit none

  integer(i_kind), intent(in   ) :: day
  real(r_kind)   , intent(in   ) :: time,lat,lon

  real(r_kind)   , intent(  out) :: sun_zenith,sun_azimuth

  integer(i_kind)   di
  real(r_kind)      ut,noon
  real(r_kind)      y(5),y2(5),x(2,5),Tx(5,2),xTx(2,2),aTx(5,2),det
  real(r_kind)      tt,eqtime,decang,latsun,lonsun
  real(r_kind)      nday(74),eqt(74),dec(74)
  real(r_kind)      beta(2), beta2(2), a(2,2)
  real(r_kind)      t0,t1,p0,p1,zz,xx,yy

  data   nday/one,   6.0_r_kind,  11.0_r_kind,  16.0_r_kind,  21.0_r_kind,  26.0_r_kind,  &
                                    31.0_r_kind,  36.0_r_kind,  41.0_r_kind,  46.0_r_kind,&
             51.0_r_kind,  56.0_r_kind,  61.0_r_kind,  66.0_r_kind,  71.0_r_kind,  &
                76.0_r_kind,  81.0_r_kind,  86.0_r_kind,  91.0_r_kind,  96.0_r_kind,&
             101.0_r_kind, 106.0_r_kind, 111.0_r_kind, 116.0_r_kind, 121.0_r_kind, &
              126.0_r_kind, 131.0_r_kind, 136.0_r_kind, 141.0_r_kind, 146.0_r_kind,&
             151.0_r_kind, 156.0_r_kind, 161.0_r_kind, 166.0_r_kind, 171.0_r_kind, &
              176.0_r_kind, 181.0_r_kind, 186.0_r_kind, 191.0_r_kind, 196.0_r_kind,&
             201.0_r_kind, 206.0_r_kind, 211.0_r_kind, 216.0_r_kind, 221.0_r_kind, &
             226.0_r_kind, 231.0_r_kind, 236.0_r_kind, 241.0_r_kind, 246.0_r_kind,&
             251.0_r_kind, 256.0_r_kind, 261.0_r_kind, 266.0_r_kind, 271.0_r_kind, &
             276.0_r_kind, 281.0_r_kind, 286.0_r_kind, 291.0_r_kind, 296.0_r_kind,&
             301.0_r_kind, 306.0_r_kind, 311.0_r_kind, 316.0_r_kind, 321.0_r_kind, &
             326.0_r_kind, 331.0_r_kind, 336.0_r_kind, 341.0_r_kind, 346.0_r_kind,&
             351.0_r_kind, 356.0_r_kind, 361.0_r_kind, 366.0_r_kind/

  data  eqt/ -3.23_r_kind, -5.49_r_kind, -7.60_r_kind, -9.48_r_kind,-11.09_r_kind,&
             -12.39_r_kind,-13.34_r_kind,-13.95_r_kind,-14.23_r_kind,-14.19_r_kind,&
            -13.85_r_kind,-13.22_r_kind,-12.35_r_kind,-11.26_r_kind,-10.01_r_kind, &
            -8.64_r_kind, -7.18_r_kind, -5.67_r_kind, -4.16_r_kind, -2.69_r_kind,&
             -1.29_r_kind, -0.02_r_kind,  1.10_r_kind,  2.05_r_kind,  2.80_r_kind,  &
              3.33_r_kind,  3.63_r_kind,  3.68_r_kind,  3.49_r_kind,  3.09_r_kind,&
              2.48_r_kind,  1.71_r_kind,  0.79_r_kind, -0.24_r_kind, -1.33_r_kind, &
              -2.41_r_kind, -3.45_r_kind, -4.39_r_kind, -5.20_r_kind, -5.84_r_kind,&
             -6.28_r_kind, -6.49_r_kind, -6.44_r_kind, -6.15_r_kind, -5.60_r_kind, &
              -4.82_r_kind, -3.81_r_kind, -2.60_r_kind, -1.19_r_kind,  0.36_r_kind,&
              2.03_r_kind,  3.76_r_kind,  5.54_r_kind,  7.31_r_kind,  9.04_r_kind, &
             10.69_r_kind, 12.20_r_kind, 13.53_r_kind, 14.65_r_kind, 15.52_r_kind,&
             16.12_r_kind, 16.41_r_kind, 16.36_r_kind, 15.95_r_kind, 15.19_r_kind, &
             14.09_r_kind, 12.67_r_kind, 10.93_r_kind,  8.93_r_kind,  6.70_r_kind,&
              4.32_r_kind,  1.86_r_kind, -0.62_r_kind, -3.23_r_kind/

  data dec/ -23.06_r_kind,-22.57_r_kind,-21.91_r_kind,-21.06_r_kind,-20.05_r_kind,&
            -18.88_r_kind,-17.57_r_kind,-16.13_r_kind,-14.57_r_kind,-12.91_r_kind,&
            -11.16_r_kind, -9.34_r_kind, -7.46_r_kind, -5.54_r_kind, -3.59_r_kind,&
             -1.62_r_kind,  0.36_r_kind,  2.33_r_kind,  4.28_r_kind,  6.19_r_kind,&
              8.06_r_kind,  9.88_r_kind, 11.62_r_kind, 13.29_r_kind, 14.87_r_kind,&
             16.34_r_kind, 17.70_r_kind, 18.94_r_kind, 20.04_r_kind, 21.00_r_kind,&
             21.81_r_kind, 22.47_r_kind, 22.95_r_kind, 23.28_r_kind, 23.43_r_kind,&
             23.40_r_kind, 23.21_r_kind, 22.85_r_kind, 22.32_r_kind, 21.63_r_kind,&
             20.79_r_kind, 19.80_r_kind, 18.67_r_kind, 17.42_r_kind, 16.05_r_kind,&
             14.57_r_kind, 13.00_r_kind, 11.33_r_kind,  9.60_r_kind,  7.80_r_kind,&
              5.95_r_kind,  4.06_r_kind,  2.13_r_kind,  0.19_r_kind, -1.75_r_kind,&
             -3.69_r_kind, -5.62_r_kind, -7.51_r_kind, -9.36_r_kind,-11.16_r_kind,&
            -12.88_r_kind,-14.53_r_kind,-16.07_r_kind,-17.50_r_kind,-18.81_r_kind,&
            -19.98_r_kind,-20.99_r_kind,-21.85_r_kind,-22.52_r_kind,-23.02_r_kind,&
            -23.33_r_kind,-23.44_r_kind,-23.35_r_kind,-23.06_r_kind/

!
! compute the subsolar coordinates
!


  tt= mod(real((int(day)+time/24._r_kind-one)),365.25_r_single) + one  ! fractional day number
                                                                       ! with 12am 1jan = 1.
  do di = 1, 73
     if ((tt >= nday(di)) .and. (tt <= nday(di+1))) exit
  end do

!============== Perform a least squares regression on doy**3 ==============

  x(1,:) = one

  if ((di >= 3) .and. (di <= 72)) then
     y(:) = eqt(di-2:di+2)
     y2(:) = dec(di-2:di+2)

     x(2,:) = nday(di-2:di+2)**3
  end if
  if (di == 2) then
     y(1) = eqt(73)
     y(2:5) = eqt(di-1:di+2)
     y2(1) = dec(73)
     y2(2:5) = dec(di-1:di+2)

     x(2,1) = nday(73)**3
     x(2,2:5) = (365._r_kind+nday(di-1:di+2))**3
  end if
  if (di == 1) then
     y(1:2) = eqt(72:73)
     y(3:5) = eqt(di:di+2)
     y2(1:2) = dec(72:73)
     y2(3:5) = dec(di:di+2)

     x(2,1:2) = nday(72:73)**3
     x(2,3:5) = (365._r_kind+nday(di:di+2))**3
  end if
  if (di == 73) then
     y(1:4) = eqt(di-2:di+1)
     y(5) = eqt(2)
     y2(1:4) = dec(di-2:di+1)
     y2(5) = dec(2)

     x(2,1:4) = nday(di-2:di+1)**3
     x(2,5) = (365._r_kind+nday(2))**3
  end if
  if (di == 74) then
     y(1:3) = eqt(di-2:di)
     y(4:5) = eqt(2:3)
     y2(1:3) = dec(di-2:di)
     y2(4:5) = dec(2:3)

     x(2,1:3) = nday(di-2:di)**3
     x(2,4:5) = (365._r_kind+nday(2:3))**3
  end if

!  Tx = transpose(x)
  Tx(1:5,1)=x(1,1:5)
  Tx(1:5,2)=x(2,1:5)
!  xTx = MATMUL(x,Tx)
  xTx(1,1)=x(1,1)*Tx(1,1)+x(1,2)*Tx(2,1)+x(1,3)*Tx(3,1)+x(1,4)*Tx(4,1)+x(1,5)*Tx(5,1)
  xTx(1,2)=x(1,1)*Tx(1,2)+x(1,2)*Tx(2,2)+x(1,3)*Tx(3,2)+x(1,4)*Tx(4,2)+x(1,5)*Tx(5,2)
  xTx(2,1)=x(2,1)*Tx(1,1)+x(2,2)*Tx(2,1)+x(2,3)*Tx(3,1)+x(2,4)*Tx(4,1)+x(2,5)*Tx(5,1)
  xTx(2,2)=x(2,1)*Tx(1,2)+x(2,2)*Tx(2,2)+x(2,3)*Tx(3,2)+x(2,4)*Tx(4,2)+x(2,5)*Tx(5,2)

  det = xTx(1,1)*xTx(2,2) - xTx(1,2)*xTx(2,1)
  a(1,1) = xTx(2,2)/det
  a(1,2) = -xTx(1,2)/det
  a(2,1) = -xTx(2,1)/det
  a(2,2) = xTx(1,1)/det

!  aTx = MATMUL(Tx,a)
  aTx(1,1)=Tx(1,1)*a(1,1)+Tx(1,2)*a(2,1) 
  aTx(2,1)=Tx(2,1)*a(1,1)+Tx(2,2)*a(2,1) 
  aTx(3,1)=Tx(3,1)*a(1,1)+Tx(3,2)*a(2,1) 
  aTx(4,1)=Tx(4,1)*a(1,1)+Tx(4,2)*a(2,1) 
  aTx(5,1)=Tx(5,1)*a(1,1)+Tx(5,2)*a(2,1) 
  aTx(1,2)=Tx(1,1)*a(1,2)+Tx(1,2)*a(2,2) 
  aTx(2,2)=Tx(2,1)*a(1,2)+Tx(2,2)*a(2,2) 
  aTx(3,2)=Tx(3,1)*a(1,2)+Tx(3,2)*a(2,2) 
  aTx(4,2)=Tx(4,1)*a(1,2)+Tx(4,2)*a(2,2) 
  aTx(5,2)=Tx(5,1)*a(1,2)+Tx(5,2)*a(2,2) 

!  beta = MATMUL(y,aTx)
  beta(1) = y(1)*aTx(1,1)+y(2)*aTx(2,1)+y(3)*aTx(3,1)+y(4)*aTx(4,1)+y(5)*aTx(5,1)
  beta(2) = y(1)*aTx(1,2)+y(2)*aTx(2,2)+y(3)*aTx(3,2)+y(4)*aTx(4,2)+y(5)*aTx(5,2)

!  beta2 = MATMUL(y2,aTx)
  beta2(1) = y2(1)*aTx(1,1)+y2(2)*aTx(2,1)+y2(3)*aTx(3,1)+y2(4)*aTx(4,1)+y2(5)*aTx(5,1)
  beta2(2) = y2(1)*aTx(1,2)+y2(2)*aTx(2,2)+y2(3)*aTx(3,2)+y2(4)*aTx(4,2)+y2(5)*aTx(5,2)

!============== finished least squares regression on doy**3 ==============

! if ((di < 3) .or. (di > 72)) tt = tt + 365._r_kind

  eqtime=(beta(1) + beta(2)*tt**3)*r60inv
  decang=beta2(1) + beta2(2)*tt**3
  latsun=decang

  ut=time
  noon=12._r_kind-lon/15._r_kind                      ! universal time of noon

  lonsun=-15._r_kind*(ut-12._r_kind+eqtime)

  t0=(90._r_kind-lat)*deg2rad
  t1=(90._r_kind-latsun)*deg2rad

  p0=lon*deg2rad
  p1=lonsun*deg2rad

  zz=cos(t0)*cos(t1)+sin(t0)*sin(t1)*cos(p1-p0)
!  zz2=sin(t0)*sin(t1)+cos(t0)*cos(t1)*cos(p1-p0)
  xx=sin(t1)*sin(p1-p0)
  yy=sin(t0)*cos(t1)-cos(t0)*sin(t1)*cos(p1-p0)

  sun_zenith=90_r_kind-acos(zz)*rad2deg
  sun_azimuth=atan2(xx,yy)*rad2deg
  if (sun_azimuth < zero) sun_azimuth = sun_azimuth + 360.0_r_kind

  return
end subroutine zensun
