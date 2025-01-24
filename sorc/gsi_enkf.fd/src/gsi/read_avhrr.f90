subroutine read_avhrr(mype,val_avhrr,ithin,rmesh,jsatid,&
     gstime,infile,lunout,obstype,nread,ndata,nodata,twind,sis, &
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs, &
     nrec_start,dval_use)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_avhrr_gac                  read gac avhrr data
!   prgmmr: li, xu           org: np23                date: 2005-10-20
!
! abstract:  This routine reads BUFR format AVHRR GAC 1b radiance (brightness
!            temperature) files, which are bufrized from the NESDIS 1b data.  Optionally, the
!            data are thinned to a specified resolution using simple
!            quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2005-10-20  li
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-04-27  derber  - clean up code
!   2006-05-19  eliu    - add logic to reset relative weight when all channels not used
!   2006-07-28  derber  - add solar and satellite azimuth angles remove isflg from output
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2007-03-28  derber  - add CLAVR (CLAVR cloud flag)
!   2008-04-21  safford - rm unused vars and uses
!   2008-10-10  derber  - modify to allow mpi_io
!   2008-12-30  todling - memory leak fix (data_crit)
!   2009-04-21  derber  - add ithin to call to makegrids
!   2011-04-08  li      - (1) use nst_gsi, nstinfo, fac_dtl, fac_tsl and add NSST vars 
!                         (2) get zob, tz_tr (call skindepth and cal_tztr)
!                         (3) interpolate NSST Variables to Obs. location (call deter_nst)
!                         (4) add more elements (nstinfo) in data array
!                         (5) add observation scoring for thinning
!   2011-08-01  lueken  - added module use deter_sfc_mod, fixed indentation
!   2012-03-05  akella  - nst now controlled via coupler
!   2013-01-22  zhu     - add newpc4pred option
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2013-02-16  akella  - only 1 processor should check for bad obs and write out data_all 
!                         after call to combine_radobs. Clean up for retrieval case.
!                         Add call to checkob. Bug fix for scoring of obs, by including newchn,
!                         also add another ob scoring approach based on observed Tb only.
!                         add check: bufsat(jsatid) == satellite id
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2015-10-01  guo     - consolidate use of ob location (in deg)
!   2018-05-21  j.jin   - added time-thinning. Moved the checking of thin4d into satthin.F90.
!   2021-02-21  x.li    - modifying thinning scheme
!  
!
!   input argument list:
!     mype     - mpi task id
!     val_avhrr- weighting factor applied to super obs
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!     nrec_start - first subset with useful information
!
!   output argument list:
!     nread    - number of BUFR GAC AVHRR observations read
!     ndata    - number of BUFR GAC AVHRR profiles retained for further processing
!     nodata   - number of BUFR GAC AVHRR observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_single,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
                     checkob, finalcheck,score_crit
  use satthin, only: radthin_time_info,tdiff2crit
  use obsmod,  only: time_window_max
  use satthin, only: hsst
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,tll2xy,txy2ll,rlats,rlons
  use constants, only: deg2rad, zero, one, two, half, rad2deg, r60inv
! use radinfo, only: cbias,predx,air_rad,ang_rad,newpc4pred
  use radinfo, only: retrieval,iuse_rad,jpch_rad,nusis, &
                     newchn
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use deter_sfc_mod, only: deter_sfc
  use obsmod, only: bmiss
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter
  use mpimod, only: npe

! use radiance_mod, only: rad_obs_type
  implicit none


! Declare passed variables
  character(len=*), intent(in  ) :: infile,obstype,jsatid
  character(len=20),intent(in  ) :: sis
  integer(i_kind) ,intent(in   ) :: mype,lunout,ithin,nrec_start
  integer(i_kind) ,intent(inout) :: nread
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  integer(i_kind) ,intent(inout) :: ndata,nodata
  real(r_kind)    ,intent(in   ) :: rmesh,gstime,twind
  real(r_kind)    ,intent(inout) :: val_avhrr
  integer(i_kind) ,intent(in   ) :: mype_root
  integer(i_kind) ,intent(in   ) :: mype_sub
  integer(i_kind) ,intent(in   ) :: npe_sub
  integer(i_kind) ,intent(in   ) :: mpi_comm_sub
  logical         ,intent(in   ) :: dval_use

! Declare local parameters
  character(6),parameter:: file_sst='SST_AN'
  integer(i_kind),parameter:: mlat_sst = 3000
  integer(i_kind),parameter:: mlon_sst = 5000
  real(r_kind),parameter:: r6=6.0_r_kind
  real(r_kind),parameter:: scan_start=-52.612_r_kind, scan_inc=1.182_r_kind
  real(r_double),parameter:: r360=360.0_r_double
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind

  real(r_kind),parameter :: ngac=409.0_r_kind,nfov=90.0_r_kind,cut_spot=11.0_r_kind
  character(len=80),parameter ::  &
     headr='YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAID FOVN SAZA SOZA CLAVR'

! Declare local variables  
  logical outside,iuse,assim
  character(len=8) :: subset

  integer(i_kind) :: nmesh
  integer(i_kind) klon1,klatp1,klonp1,klat1
  integer(i_kind) nchanl,iret,ifov, ich4, ich_offset
! integer(i_kind) ich_win
  integer(i_kind) idate,maxinfo
  integer(i_kind) ilat,ilon
  integer(i_kind),dimension(5):: idate5
  integer(i_kind) nmind,isflg,idomsfc
  integer(i_kind) itx,k,i,bufsat,n
  integer(i_kind) nreal,nele,itt
  integer(i_kind) nlat_sst,nlon_sst
  integer(i_kind) ksatid

  real(r_kind) dlon,dlat,rsc
  real(r_kind) dlon_earth,dlat_earth,sfcr
  real(r_kind) dlon_earth_deg,dlat_earth_deg
  real(r_kind) w00,w01,w10,w11,dx1,dy1
  real(r_kind) pred,crit1,tdiff,sstime,dx,dy,dist1
  real(r_kind) dlat_sst,dlon_sst,sst_hires
  real(r_kind) t4dv

  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts
  real(r_kind),dimension(mlat_sst):: rlats_sst
  real(r_kind),dimension(mlon_sst):: rlons_sst
  real(r_kind),allocatable,dimension(:,:):: sst_an
  real(r_kind), allocatable, dimension(:,:) :: data_mesh
  real(r_kind), allocatable, dimension(:,:) :: data_all
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr
  real(r_kind) :: scan_pos,scan_angle,dfov,r01
! real(r_kind) :: ch_win,ch_win_flg

  real(r_double), dimension(13) :: hdr
  real(r_double), dimension(3,5) :: bufrf
  integer(i_kind) :: lnbufr,ireadsb,ireadmg,iskip,irec,next
  integer(i_kind), allocatable, dimension(:) :: nrec
  real(r_kind), allocatable, dimension(:) :: amesh
  real(r_kind), allocatable, dimension(:) :: hsst_thd

  real(r_kind) cdist,disterr,disterrmax,dlon00,dlat00,hsst_xy,dmsh
  integer(i_kind) ntest
  integer(i_kind) :: imesh,ndata_mesh,iele

! real(r_kind), dimension(3,2) :: bandcor_a,bandcor_b
! data bandcor_a/-1.70686_r_kind,-0.27201_r_kind,-0.30949_r_kind,-1.70388_r_kind,-0.43725_r_kind,-0.25342_r_kind/
! data bandcor_b/1.002629_r_kind,1.001207_r_kind,1.000989_r_kind,1.003049_r_kind,1.001395_r_kind,1.000944_r_kind/
  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin,it_mesh

!**************************************************************************

! Start routine here.  Set constants.  Initialize variables
  maxinfo = 33
  lnbufr = 10
  disterrmax=zero
  ntest=0
  ndata  = 0
  nodata  = 0
  nread   = 0
  nchanl = 3
  ich_offset    = 2               ! avhrr, channels 1 & 2 are skipped
  ich4          = ich_offset + 2
  r01 = 0.01_r_kind

  if ( rmesh == 888.0_r_kind ) then
     nmesh = 6
     allocate(amesh(nmesh),hsst_thd(0:nmesh))
!
!    assign thinning box sizes related berror correlation length thresholds
!
     amesh(1) = max(two,minval(hsst))
     amesh(nmesh) = min(100.0_r_kind,maxval(hsst))
     dmsh = (amesh(nmesh) - amesh(1))/real(nmesh-1)
     do imesh = 2, nmesh - 1
        amesh(imesh) = amesh(imesh-1) + dmsh
     enddo

     hsst_thd(0) = max(two,half*amesh(1))
     hsst_thd(nmesh) = 2000.0_r_kind
     do imesh = 1, nmesh - 1
        hsst_thd(imesh) = amesh(imesh) + half*dmsh 
     enddo

  else
     nmesh = 1
     allocate(amesh(nmesh),hsst_thd(0:nmesh))
     amesh(nmesh) = rmesh
     hsst_thd(0) = 2.0_r_kind
     hsst_thd(1) = 2000.0_r_kind
  endif

  dfov = (ngac - two*cut_spot - one)/nfov

  ilon=3
  ilat=4

  if(nst_gsi>0) then
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif

  rlndsea(0) = zero
  rlndsea(1) = 30._r_kind
  rlndsea(2) = 20._r_kind
  rlndsea(3) = 30._r_kind
  rlndsea(4) = 30._r_kind

                                        ! 205,206,207,208 or 209 for  NOAA-14,15,16,17, 18 and 19 respectively
  if(jsatid == 'n14')bufsat = 205
  if(jsatid == 'n15')bufsat = 206
  if(jsatid == 'n16')bufsat = 207
  if(jsatid == 'n17')bufsat = 208
  if(jsatid == 'n18')bufsat = 209
  if(jsatid == 'n19')bufsat = 223
  if(jsatid == 'metop-a')bufsat = 4
  if(jsatid == 'metop-b')bufsat = 3
  if(jsatid == 'metop-c')bufsat = 5

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
  if (.not.assim) val_avhrr=zero

  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif

! Read hi-res sst analysis
  if (retrieval) then
     allocate(sst_an(mlat_sst, mlon_sst))
     call rdgrbsst(file_sst,mlat_sst,mlon_sst,&
     sst_an,rlats_sst,rlons_sst,nlat_sst,nlon_sst)
  endif
     
! Allocate arrays to hold all data for given satellite
  if(dval_use) maxinfo = maxinfo + 2
  nreal = maxinfo + nstinfo
  nele  = nreal   + nchanl

  ndata = 0
  do imesh = 1, nmesh
!    Make thinning grids, including determination of itxmax
     call makegrids(amesh(imesh),ithin,n_tbin=n_tbin)

     allocate( data_mesh(nele,itxmax) )
     allocate( nrec(itxmax) )

     if ( imesh == 1 ) then
        allocate( data_all(nele,itxmax) )
     endif

     open(lnbufr,file=trim(infile),form='unformatted')         ! open bufr data file

!    Associate the tables file with the message file, and identify the 
!    latter to BUFRLIB software
     call openbf (lnbufr,'IN',lnbufr)

     next=0
     nrec=999999
     irec=0
!    Read BUFR AVHRR GAC 1b data
     read_msg: do while (ireadmg(lnbufr,subset,idate) >= 0)
        irec=irec+1
        if(irec < nrec_start) cycle read_msg
        next=next+1
        if(next == npe_sub)next=0
        if(next /= mype_sub)cycle
        read_loop: do while (ireadsb(lnbufr) == 0)
           call ufbint(lnbufr,hdr,13,1,iret,headr)
           call ufbrep(lnbufr,bufrf, 3,5,iret,'INCN ALBD TMBR')
           if(iret <= 0) cycle read_loop
           ksatid  = nint(hdr(9))                ! Extract satellite id from bufr file
           if(ksatid /= bufsat) cycle read_loop  ! If this sat is not the one we want, read next record
           if (hdr(10) <= real(cut_spot) .or. hdr(10) > real(ngac-cut_spot)) cycle read_loop! drop starting and ending pixels
           if (hdr(13) /= zero ) cycle read_loop ! toss pixel with CLAVR partly cloud flag

           iskip = 0
           do k=1,nchanl
              if(bufrf(3,ich_offset+k) < zero .or. bufrf(3,ich_offset+k) > tbmax) then
                 iskip=iskip+1
              end if
           end do
           if(iskip >= nchanl)cycle read_loop

!          Get lat/lon in degree          
           if (abs(hdr(7))>90.0_r_double .or. abs(hdr(8))>r360) cycle read_loop

           if (hdr(8)==r360) hdr(8)=hdr(8)-r360
           if (hdr(8)< zero) hdr(8)=hdr(8)+r360

           dlon_earth_deg = hdr(8)
           dlat_earth_deg = hdr(7)

!          Convert obs location to radians
           dlon_earth = hdr(8)*deg2rad   !convert degrees to radians
           dlat_earth = hdr(7)*deg2rad

!          Regional case
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
              if(outside) cycle read_loop

!          Global casea: Get relative lat/lon in analysis grids
           else
              dlat = dlat_earth
              dlon = dlon_earth
              call grdcrd1(dlat,rlats,nlat,1)
              call grdcrd1(dlon,rlons,nlon,1)
           endif

!          Interpolate hsst(nlat,nlon) to obs.location
           if ( rmesh == 888.0_r_kind ) then
           klon1=int(dlon); klat1=int(dlat)
           dx  =dlon-klon1; dy  =dlat-klat1
           dx1 =one-dx;         dy1 =one-dy
           w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

           klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
           if(klon1==0) klon1=nlon
           klatp1=min(nlat,klat1+1); klonp1=klon1+1
           if(klonp1==nlon+1) klonp1=1

           hsst_xy=w00*hsst(klat1,klon1 ) + w10*hsst(klatp1,klon1 ) + &
                   w01*hsst(klat1,klonp1) + w11*hsst(klatp1,klonp1)
!
!          Only process the obs. at the location where hsst is in the current processed range
!
           if ( hsst_xy < hsst_thd(imesh-1) .or. hsst_xy >= hsst_thd(imesh) ) cycle read_loop

           endif

!          Extract date information.  If time outside window, skip this obs
           idate5(1) = nint(hdr(1))    !year
           idate5(2) = nint(hdr(2))    !month
           idate5(3) = nint(hdr(3))    !day
           idate5(4) = nint(hdr(4))    !hour
           idate5(5) = nint(hdr(5))    !minute
           rsc       = hdr(6)          !second in real
           call w3fs21(idate5,nmind)
           t4dv=(real((nmind-iwinbgn),r_kind) + rsc*r60inv)*r60inv
           sstime=real(nmind,r_kind) + rsc*r60inv
           tdiff=(sstime-gstime)*r60inv

           if (l4dvar.or.l4densvar) then
              if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
           else
              if (abs(tdiff) > twind) cycle read_loop
           endif

           nread = nread + 1
!
!          map observations to the thinning grid 
!
           crit0 = 0.01_r_kind
           timeinflat=6.0_r_kind
           call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)
           call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)

           if(.not. iuse)cycle read_loop

!     "Score" observation.   We use this information to id "best" obs.

!     Locate the observation on the analysis grid.  Get sst and land/sea/ice
!     mask.  

!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed                          

           call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
              ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
!          if(isflg /= zero)  cycle read_loop
           if(sfcpct(0) < 0.15_r_kind)  cycle read_loop

           call checkob(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop
!
!       Get scan position (1 - 90) based on (409 - 2*cut_spot - 1) = 386 here,  GAC pixels
!       avhrr gac scan has 409 positions. we drop tails: 1- 11 & 399- 409 [the two ends]
!       here we linearly map pixels: 12- 398 to 1 - 90 scan positions
           if ( mod(hdr(10)-cut_spot,dfov) < half*dfov ) then
              scan_pos = real(nint((hdr(10)-cut_spot)/dfov) + 1) 
           else
              scan_pos = real(nint((hdr(10)-cut_spot)/dfov)) 
           endif

           if ( scan_pos > nfov ) scan_pos = nfov

           ifov = nint(scan_pos)
           scan_angle = (scan_start+real(ifov-1)*scan_inc)*deg2rad

!          Set common predictor parameters

!          Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"

!          if (newpc4pred) then
!             ch_win = bufrf(3,2+ich_win) -ang_rad(ich_win)*cbias(ifov,ich_win)- &
!                         predx(1,ich_win)*air_rad(ich_win)
!          else
!             ch_win = bufrf(3,2+ich_win) -ang_rad(ich_win)*cbias(ifov,ich_win)- &
!                         r01*predx(1,ich_win)*air_rad(ich_win)
!          end if
!          ch_win_flg = tsavg-ch_win
!          pred   = 10.0_r_kind*max(zero,ch_win_flg)

!          above commented calculation of pred uses tsavg (from bkg). There is no reason why
!          1. we should use bkg to SCORE an ob., 2. even if we do use bkg based tsavg, 
!          tsavg-ch_win could be misleading, if there are low clouds. 
!          instead we will TRY following simpler approach- so that ob with colder Tb gets a high score.
           pred   = (600.0_r_kind - bufrf(3,ich4)) * r01

           crit1=crit1+rlndsea(isflg)
           crit1 = crit1+pred  
           call finalcheck(dist1,crit1,itx,iuse)

           if(.not. iuse)cycle read_loop

           if (retrieval) then
!             Interpolate hi-res sst analysis to observation location
              dlat_sst = dlat_earth
              dlon_sst = dlon_earth
              call grdcrd1(dlat_sst,rlats_sst,nlat_sst,1)
              call grdcrd1(dlon_sst,rlons_sst,nlon_sst,1)

              klon1=int(dlon_sst); klat1=int(dlat_sst)
              dx  =dlon_sst-klon1; dy  =dlat_sst-klat1
              dx1 =one-dx;         dy1 =one-dy
              w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

              klat1=min(max(1,klat1),nlat_sst); klon1=min(max(0,klon1),nlon_sst)
              if(klon1==0) klon1=nlon_sst
              klatp1=min(nlat_sst,klat1+1); klonp1=klon1+1
              if(klonp1==nlon_sst+1) klonp1=1

              sst_hires=w00*sst_an(klat1,klon1 ) + w10*sst_an(klatp1,klon1 ) + &
                        w01*sst_an(klat1,klonp1) + w11*sst_an(klatp1,klonp1)
           else
              sst_hires=zero
           endif       !  if (retrieval) then
!
!       interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
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
           
!          Transfer information to work array
           data_mesh(1, itx) = hdr(9)                 ! satellite id (207 = NOAA-16, 208 = NOAA-17, 209 = NOAA-18)
           data_mesh(2, itx) = t4dv                   ! time (hours)
           data_mesh(3, itx) = dlon                   ! grid relative longitude
           data_mesh(4, itx) = dlat                   ! grid relative latitude
           data_mesh(5, itx) = hdr(11)*deg2rad        ! satellite zenith angle (radians)
           data_mesh(6, itx) = bmiss                  ! satellite azimuth angle
           data_mesh(7, itx) = scan_angle             ! scan angle
           data_mesh(8, itx) = scan_pos               ! scan position
           data_mesh(9, itx) = hdr(12)                ! solar zenith angle (radians)
           data_mesh(10,itx) = bmiss                  ! solar azimuth angle (radians)
           data_mesh(11,itx) = sfcpct(0)              ! sea percentage of
           data_mesh(12,itx) = sfcpct(1)              ! land percentage
           data_mesh(13,itx) = sfcpct(2)              ! sea ice percentage
           data_mesh(14,itx) = sfcpct(3)              ! snow percentage
           data_mesh(15,itx) = ts(0)                  ! ocean skin temperature (from surface file: sst_full)
           data_mesh(16,itx) = ts(1)                  ! land skin temperature (from surface file: sst_full)
           data_mesh(17,itx) = ts(2)                  ! ice skin temperature (from surface file: sst_full)
           data_mesh(18,itx) = ts(3)                  ! snow skin temperature (from surface file: sst_full)
           data_mesh(19,itx) = tsavg                  ! average skin temperature
           data_mesh(20,itx) = vty                    ! vegetation type
           data_mesh(21,itx) = vfr                    ! vegetation fraction
           data_mesh(22,itx) = sty                    ! soil type
           data_mesh(23,itx) = stp                    ! soil temperature
           data_mesh(24,itx) = sm                     ! soil moisture
           data_mesh(25,itx) = sn                     ! snow depth
           data_mesh(26,itx) = zz                     ! surface height
           data_mesh(27,itx) = idomsfc + 0.001_r_kind ! dominate surface type
           data_mesh(28,itx) = sfcr                   ! surface roughness
           data_mesh(29,itx) = ff10                   ! ten meter wind factor
           data_mesh(30,itx) = dlon_earth_deg         ! earth relative longitude (degrees)
           data_mesh(31,itx) = dlat_earth_deg         ! earth relative latitude (degrees)
           data_mesh(32,itx) = hdr(13)                ! CLAVR Cloud flag (only 0 = clear and 1 = probably clear included the data set used now)
           data_mesh(33,itx) = sst_hires              ! interpolated hires SST (deg K)
           if(dval_use)then
              data_mesh(34,itx) = val_avhrr              ! weighting factor applied to super obs
              data_mesh(35,itx) = itt                    !
           end if

           if(nst_gsi>0) then
              data_mesh(maxinfo+1,itx) = tref            ! foundation temperature
              data_mesh(maxinfo+2,itx) = dtw             ! dt_warm at zob
              data_mesh(maxinfo+3,itx) = dtc             ! dt_cool at zob
              data_mesh(maxinfo+4,itx) = tz_tr           ! d(Tz)/d(Tr)
           endif
   
           do k=1,nchanl
              data_mesh(k+nreal,itx)= bufrf(3,ich_offset+k) ! Tb for avhrr ch-3, ch-4 and ch-5; ich_offset is set to 2.
           end do
           nrec(itx)=irec

!       End of satellite read block

        enddo read_loop
     enddo read_msg
     call closbf(lnbufr)

     call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
          nele,itxmax,nread,ndata_mesh,data_mesh,score_crit,nrec)

     if ( nread > 0 ) then
        write(*,'(a,a10,I3,F6.1,3I10)') 'read_avhrr,satid,imesh,amesh,itxmax,nread,ndata_mesh : ',jsatid,imesh,amesh(imesh),itxmax,nread,ndata_mesh
     endif
!
!    get data_all by combining data from all thinning box sizes
!
     do n = 1, ndata_mesh
        ndata = ndata + 1
        do iele = 1, nele
           data_all(iele,ndata) = data_mesh(iele,n)
        enddo
     enddo

!    Deallocate arrays
     call destroygrids
     deallocate(data_mesh,nrec)

  enddo     ! do imesh = 1, nmesh
  deallocate(amesh,hsst_thd)

! Allow single task to check for bad obs, update superobs sum,
! and write out data to scratch file for further processing.
  if (mype_sub==mype_root.and.ndata>0) then
     do n=1,ndata
        do k=1,nchanl
           if(data_all(k+nreal,n) > tbmin .and. &
              data_all(k+nreal,n) < tbmax) nodata=nodata+1
        end do
     end do
     if(dval_use .and. assim)then
        do n=1,ndata
           itt=nint(data_all(35,n))
           super_val(itt)=super_val(itt)+val_avhrr
        end do
     end if
 
!    Write retained data to local file
     call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  endif

! Deallocate local arrays
  deallocate(data_all)
  if(retrieval) deallocate(sst_an)

  if(diagnostic_reg.and.ntest>0 .and. mype_sub==mype_root) &
     write(6,*)'READ_AVHRR:  ',&
     'mype,ntest,disterrmax=',mype,ntest,disterrmax

! End of routine
  return
end subroutine read_avhrr
