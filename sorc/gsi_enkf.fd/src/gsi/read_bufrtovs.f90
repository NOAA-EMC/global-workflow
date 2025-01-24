subroutine read_bufrtovs(mype,val_tovs,ithin,isfcalc,&
     rmesh,jsatid,gstime,infile,lunout,obstype,&
     nread,ndata,nodata,twind,sis, &
     mype_root,mype_sub,npe_sub,mpi_comm_sub, nobs, &
     nrec_start,nrec_start_ears,nrec_start_db,dval_use,radmod)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_bufrtovs                  read bufr tovs 1b data
!   prgmmr: treadon          org: np23                date: 2003-09-13
!
! abstract:  This routine reads BUFR format TOVS 1b radiance 
!            (brightness temperature) files.  Optionally, the data 
!            are thinned to a specified resolution using simple 
!            quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2003-09-13 treadon
!   2004-05-28 kleist  - update subroutine call
!   2004-06-16 treadon - update documentation
!   2004-07-23 derber  - make changes to eliminate obs. earlier in thinning
!   2004-07-29 treadon - add only to module use, add intent in/out
!   2004-10-15 derber  - various changes to "quality" prediction used 
!                        in data selection algorithm
!   2005-01-26 derber  - land/sea determination and weighting for data selection
!   2005-02-10 treadon - correct spelling in runtime print message; specify
!                        _r_kind precision for real constants
!   2005-07-06 derber  - add mhs and hirs/4 from NOAA-18, clean up code and 
!                        modify data selection criteria
!   2005-09-08  derber - modify to use input group time window
!   2005-09-28  derber - modify to produce consistent surface info 
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-10-24  derber - use more precise MHS scan properties (step & start)
!   2005-10-26  treadon - clean up formatting, add clarifying comments, correct
!                         ndata,ndata1 printout
!   2005-11-22  derber  - include mean in bias correction
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-02-11  liu - add ssu
!   2006-03-07  derber - correct error in nodata count
!   2006-04-27  derber - clean up code
!   2006-05-02  treadon - move close lnbufr to after 900 continue
!   2006-05-19  eliu    - add logic to reset relative weight when all channels not used
!   2006-07-28  derber  - add solar and satellite azimuth angles remove isflg from output
!   2007-01-18  derber - add kidsat for metop
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2007-08-31  h.liu - add kidsat for n05, n06, n07, n08, n09, n10, n11, n12,tiros-n
!   2008-05-27  safford - rm unused vars
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2008-10-14  derber  - properly use EARS data and use MPI_IO
!   2009-01-02  todling - remove unused vars
!   2009-01-09  gayno   - add option to calculate surface fields based on 
!                         size/shape of field of view.
!   2009-04-17  todling - zero out azimuth angle when unavailable (otherwise can't use old files)
!   2009-04-18  woollen - improve mpi_io interface with bufrlib routines
!   2009-04-21  derber  - add ithin to call to makegrids
!   2009-12-20  gayno - modify for updated version of FOV surface code which 
!                       calculates relative antenna power for some instruments.
!   2010-02-25  collard - changes to call to crtm_init for CRTM v2.0
!   2010-06-29  zhu     - add newpc4pred option
!   2010-07-12  zhu     - include global offset for amsua in bias correction for adp_anglebc option
!   2010-09-02  zhu     - add use_edges option
!   2010-10-12  zhu     - use radstep and radstart from radinfo
!   2011-04-07  todling - newpc4pred now in radinfo
!   2011-04-08  li      - (1) use nst_gsi, nstinfo, and add NSST vars
!                         (2) get zob, tz_tr (call skindepth and cal_tztr)
!                         (3) interpolate NSST Variables to Obs. location (call deter_nst)
!                         (4) add more elements (nstinfo) in data array
!   2011-05-05  todling - merge in Min-Jeong update for cloudy-radiance work
!   2011-05-20  mccarty - updated to read ATMS data
!   2011-07-04  todling  - fixes to run either single or double precision
!   2011-08-01  lueken  - removed deter_sfc subroutines, placed in new module deter_sfc_mod
!   2011-09-13  gayno - improve error handling for FOV-based sfc calculation
!                       (isfcalc=1)
!   2011-12-13  collard Replace find_edges code to speed up execution.
!   2011-12-14  collard Remove ATMS
!   2012-03-05  akella  - nst now controlled via coupler
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2013-12-20  zhu - change icw4crtm>0 to icw4crtm>10  (bug fix)
!   2014-01-31  mkim - added iql4crtm for all-sky mw radiance data assimilation 
!   2014-04-27  eliu/zhu - add thinning options for AMSU-A under allsky condition 
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2015-08-20  zhu - use centralized radmod for all-sky and aerosol usages in radiance assimilation
!   2016-04-28  jung - added logic for RARS and direct broadcast from NESDIS/UW
!   2016-10-20  collard - fix to allow monitoring and limited assimilation of spectra when key 
!                         channels are missing.
!   2018-04-19  eliu - allow data selection for precipitation-affected data 
!   2018-05-21  j.jin   - added time-thinning. Moved the checking of thin4d into satthin.F90.
!   2020-04021  s.sieron - change converting brightness temperatures to antenna temperautres to the
!                          other way around. added support for multiple SpcCoeff files and ACCoeffs
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
!     dval_use - logical for using dval
!     nrec_start - first subset with useful information
!     nrec_start_ears - first ears subset with useful information
!     nrec_start_db - first db subset with useful information
!
!   output argument list:
!     nread    - number of BUFR TOVS 1b observations read
!     ndata    - number of BUFR TOVS 1b profiles retained for further processing
!     nodata   - number of BUFR TOVS 1b observations retained for further processing
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
  use obsmod, only: time_window_max, ta2tb
  use radinfo, only: iuse_rad,newchn,cbias,predx,nusis,jpch_rad,air_rad,ang_rad, &
      use_edges,radedge1, radedge2, radstart,radstep,newpc4pred
  use radinfo, only: crtm_coeffs_path,adp_anglebc
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,tll2xy,txy2ll,rlats,rlons
  use constants, only: deg2rad,zero,one,two,three,five,rad2deg,r60inv,r1000,h300,r100
  use crtm_module, only: success, &
      crtm_kind => fp, &
      MAX_SENSOR_ZENITH_ANGLE
  use crtm_spccoeff, only: sc,crtm_spccoeff_load,crtm_spccoeff_destroy
  use ACCoeff_Define, only: ACCoeff_type
  use calc_fov_crosstrk, only : instrument_init, fov_cleanup, fov_check
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use antcorr_application, only: remove_antcorr, apply_antcorr
  use mpeu_util, only: getindex
  use deter_sfc_mod, only: deter_sfc_fov,deter_sfc
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter
  use mpimod, only: npe
  use radiance_mod, only: rad_obs_type
  use gsi_io, only: verbose
  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: infile,obstype,jsatid
  character(len=20),intent(in  ) :: sis
  integer(i_kind) ,intent(in   ) :: mype,lunout,ithin
  integer(i_kind) ,intent(in   ) :: nrec_start,nrec_start_ears,nrec_start_db
  integer(i_kind) ,intent(inout) :: isfcalc
  integer(i_kind) ,intent(inout) :: nread
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  integer(i_kind) ,intent(  out) :: ndata,nodata
  real(r_kind)    ,intent(in   ) :: rmesh,gstime,twind
  real(r_kind)    ,intent(inout) :: val_tovs
  integer(i_kind) ,intent(in   ) :: mype_root
  integer(i_kind) ,intent(in   ) :: mype_sub
  integer(i_kind) ,intent(in   ) :: npe_sub
  integer(i_kind) ,intent(in   ) :: mpi_comm_sub
  logical,         intent(in   ) :: dval_use
  type(rad_obs_type),intent(in ) :: radmod

! Declare local parameters

  character(8),parameter:: fov_flag="crosstrk"
  ! change from 13 to 14 for sacv
  integer(i_kind),parameter:: n1bhdr=14
  integer(i_kind),parameter:: n2bhdr=4
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind

! Declare local variables
  logical hirs,msu,amsua,amsub,mhs,hirs4,hirs3,hirs2,ssu
  logical outside,iuse,assim,valid

  character(len=40):: infile2
  character(len=8) :: subset
  character(len=80):: hdr1b,hdr2b

  integer(i_kind) ireadsb,ireadmg,irec,next,nrec_startx
  integer(i_kind) i,j,k,ifov,ntest,llll
  integer(i_kind) sacv
  integer(i_kind) iret,idate,nchanl,n,idomsfc(1)
  integer(i_kind) ich1,ich2,ich8,ich15,ich16,ich17
  integer(i_kind) kidsat,instrument,maxinfo
  integer(i_kind) nmind,itx,nreal,nele,itt,ninstruments
  integer(i_kind) iskip,ichan2,ichan1,ichan15
  integer(i_kind) lnbufr,ksatid,ichan8,isflg,ichan3,ich3,ich4,ich6
  integer(i_kind) ilat,ilon,ifovmod
  integer(i_kind),dimension(5):: idate5
  integer(i_kind) instr,ichan
  integer(i_kind) error_status,irecx,ierr
  integer(i_kind) radedge_min, radedge_max
  integer(i_kind),allocatable,dimension(:)::nrec
  character(len=20),dimension(1):: sensorlist

  real(r_kind) cosza,sfcr
  real(r_kind) ch1,ch2,ch3,ch8,d0,d1,d2,ch15,qval
  real(r_kind) ch1flg
  real(r_kind) expansion
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr

  real(r_kind) pred, pred_water, pred_not_water
  real(r_kind) rsat,dlat,panglr,dlon,rato,sstime,tdiff,t4dv
  real(r_kind) dlon_earth_deg,dlat_earth_deg,sat_aziang
  real(r_kind) dlon_earth,dlat_earth,r01
  real(r_kind) crit1,step,start,ch8flg,dist1
  real(r_kind) terrain,lza,df2,tt,lzaest
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),allocatable,dimension(:,:):: data_all

  real(crtm_kind),allocatable,dimension(:):: data1b4
  real(r_double),allocatable,dimension(:):: data1b8
  real(r_double),dimension(n1bhdr):: bfr1bhdr
  real(r_double),dimension(n2bhdr):: bfr2bhdr

  real(r_kind) disterr,disterrmax,cdist,dlon00,dlat00

  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin,it_mesh
  logical :: critical_channels_missing,quiet
  logical :: print_verbose

  logical :: spc_coeff_found
  integer(i_kind) :: spc_coeff_versions
  character(len=80) :: spc_filename
  type(ACCoeff_type),dimension(3) :: accoeff_sets

!**************************************************************************
! Initialize variables

  print_verbose=.false.
  if(verbose) print_verbose=.true.
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
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif

  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif
! Make thinning grids
  call makegrids(rmesh,ithin,n_tbin=n_tbin)

! Set various variables depending on type of data to be read

  hirs2 =    obstype == 'hirs2'
  hirs3 =    obstype == 'hirs3'
  hirs4 =    obstype == 'hirs4'
  hirs  =    hirs2 .or. hirs3 .or. hirs4
  msu   =    obstype == 'msu'
  amsua =    obstype == 'amsua'
  amsub =    obstype == 'amsub'
  mhs   =    obstype == 'mhs'
  ssu   =    obstype == 'ssu'

!  instrument specific variables
  d1 =  0.754_r_kind
  d2 = -2.265_r_kind 
  r01 = 0.01_r_kind
  ich1   = 1   !1
  ich2   = 2   !2
  ich3   = 3   !3
  ich4   = 4   !4
  ich6   = 6   !6
  ich8   = 8   !8
  ich15  = 15  !15
  ich16  = 16  !16
  ich17  = 17  !17
! Set array index for surface-sensing channels
  ichan1  = newchn(sis,ich1)
  ichan2  = newchn(sis,ich2)
  ichan3  = newchn(sis,ich3)
  if (hirs) then
     ichan8  = newchn(sis,ich8)
  endif
  if (amsua) ichan15 = newchn(sis,ich15)

  if(jsatid == 'n05')kidsat=705
  if(jsatid == 'n06')kidsat=706
  if(jsatid == 'n07')kidsat=707
  if(jsatid == 'tirosn')kidsat=708
  if(jsatid == 'n08')kidsat=200
  if(jsatid == 'n09')kidsat=201
  if(jsatid == 'n10')kidsat=202
  if(jsatid == 'n11')kidsat=203
  if(jsatid == 'n12')kidsat=204

  if(jsatid == 'n14')kidsat=205
  if(jsatid == 'n15')kidsat=206
  if(jsatid == 'n16')kidsat=207
  if(jsatid == 'n17')kidsat=208
  if(jsatid == 'n18')kidsat=209
  if(jsatid == 'n19')kidsat=223
  if(jsatid == 'metop-a')kidsat=4
  if(jsatid == 'metop-b')kidsat=3
  if(jsatid == 'metop-c')kidsat=5
  if(jsatid == 'npp')kidsat=224

  radedge_min = 0
  radedge_max = 1000
  do i=1,jpch_rad
     if (trim(nusis(i))==trim(sis)) then
        step  = radstep(i)
        start = radstart(i)
        if (radedge1(i)/=-1 .and. radedge2(i)/=-1) then
           radedge_min=radedge1(i)
           radedge_max=radedge2(i)
        end if
        exit 
     endif
  end do 

  rato=1.1363987_r_kind
  if ( hirs ) then
     nchanl=19
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 30._r_kind
     if (isfcalc == 1) then
        expansion=one  ! use one for ir sensors
        ichan=-999  ! not used for hirs
        if (hirs2) then
           if(kidsat==203.or.kidsat==205)then
              instr=5 ! hirs-2i on noaa 11 and 14
           elseif(kidsat==708.or.kidsat==706.or.kidsat==707.or. &
                  kidsat==200.or.kidsat==201.or.kidsat==202.or. &
                  kidsat==204)then
              instr=4 ! hirs-2 on tiros-n,noaa6-10,noaa12
           else  ! sensor/sat mismatch
              write(6,*) 'READ_BUFRTOVS:  *** WARNING: HIRS2 SENSOR/SAT MISMATCH'
              instr=5 ! set to something to prevent abort
           endif
        elseif (hirs4) then
           instr=8
        elseif (hirs3) then
           if(kidsat==206) then
              instr=6   ! noaa 15
           elseif(kidsat==207.or.kidsat==208)then
              instr=7   ! noaa 16/17
           else
              write(6,*) 'READ_BUFRTOVS:  *** WARNING: HIRS3 SENSOR/SAT MISMATCH'
              instr=7
           endif
        endif
     endif  ! isfcalc == 1
  else if ( msu ) then
     nchanl=4
     if (isfcalc==1) then
        instr=10
        ichan=-999
        expansion=2.9_r_kind
     endif
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = zero
     rlndsea(1) = 20._r_kind
     rlndsea(2) = 15._r_kind
     rlndsea(3) = 20._r_kind
     rlndsea(4) = 100._r_kind
  else if ( amsua ) then
     nchanl=15
     if (isfcalc==1) then
        instr=11
        ichan=15  ! pick a surface sens. channel
        expansion=2.9_r_kind ! use almost three for microwave sensors.
     endif
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
  else if ( amsub )  then
     nchanl=5
     if (isfcalc==1) then
        instr=12
        ichan=-999
        expansion=2.9_r_kind
     endif
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 20._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
  else if ( mhs )  then
     nchanl=5
     if (isfcalc==1) then
        instr=13
        ichan=1  ! all channels give similar result.
        expansion=2.9_r_kind
     endif
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 20._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
  else if ( ssu ) then
     nchanl=3
     if (isfcalc==1) then
        instr=9
        ichan=-999  ! not used for this sensor
        expansion=one
     endif
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 30._r_kind
  end if

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

! Initialize variables for use by FOV-based surface code.
  if (isfcalc == 1) then
     call instrument_init(instr,jsatid,expansion,valid)
     if (.not. valid) then
       if (assim) then
         write(6,*)'READ_BUFRTOVS:  ***ERROR*** IN SETUP OF FOV-SFC CODE. STOP'
         call stop2(71)
       else
         call fov_cleanup
         isfcalc = 0
         write(6,*)'READ_BUFRTOVS:  ***ERROR*** IN SETUP OF FOV-SFC CODE'
       endif
     endif
  endif

  if (isfcalc==1) then
    if (amsub.or.mhs)then
      rlndsea(4) = max(rlndsea(0),rlndsea(1),rlndsea(2),rlndsea(3))
    else
      rlndsea=0
    endif
  endif

! Allocate arrays to hold all data for given satellite
  if(dval_use) maxinfo=maxinfo+2
  nreal = maxinfo + nstinfo
  nele  = nreal   + nchanl
  hdr1b ='SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON CLATH CLONH HOLS SACV'
  hdr2b ='SAZA SOZA BEARAZ SOLAZI'
  allocate(data_all(nele,itxmax),data1b8(nchanl),data1b4(nchanl),nrec(itxmax))

  nrec = 999999
  next=0
  irec=0
! Big loop over standard data feed and possible ears/db data
! llll=1 is normal feed, llll=2 EARS/RARS data, llll=3 DB/UW data)
  ears_db_loop: do llll= 1, 3

     if(llll == 1)then
        nrec_startx=nrec_start
        infile2=trim(infile)         ! Set bufr subset names based on type of data to read
     elseif(llll == 2) then
        nrec_startx=nrec_start_ears
        infile2=trim(infile)//'ears' ! Set bufr subset names based on type of data to read
        if(amsua .and. kidsat >= 200 .and. kidsat <= 207) cycle ears_db_loop
     elseif(llll == 3) then
        nrec_startx=nrec_start_db
        infile2=trim(infile)//'_db'  ! Set bufr subset names based on type of data to read
        if(amsua .and. kidsat >= 200 .and. kidsat <= 207) cycle ears_db_loop
     end if

!    Reopen unit to satellite bufr file
     open(lnbufr,file=trim(infile2),form='unformatted',status = 'old',iostat=ierr)
     if(ierr /= 0) cycle ears_db_loop

     call openbf(lnbufr,'IN',lnbufr)

     ! support multiple spc coefficient files for any given sensor
     if(amsua .or. amsub .or. mhs)then
        quiet=.not.verbose
        spc_coeff_versions = 0
        spc_coeff_found = .true.
        do while (spc_coeff_found)
           if (spc_coeff_versions == 0) then
              sensorlist(1)=sis
           else
              i = spc_coeff_versions+1
              write(sensorlist(1),'(a,a,i1)') trim(sis),'_v',i
           end if

           if( crtm_coeffs_path /= "" ) then
              if(mype_sub==mype_root .and. print_verbose) write(6,*)'READ_BUFRTOVS: crtm_spccoeff_load() on path "'//trim(crtm_coeffs_path)//'"'
           end if

           spc_filename = trim(crtm_coeffs_path) // trim(sensorlist(1)) // '.SpcCoeff.bin'
           INQUIRE(FILE=trim(spc_filename), EXIST=spc_coeff_found)

           if (.NOT. spc_coeff_found) then
              if (spc_coeff_versions == 0) then
                 write(6,*)'READ_BUFRTOVS:  ***ERROR*** crtm_spccoeff_load error_status=',error_status,&
                    '   TERMINATE PROGRAM EXECUTION'
                 call stop2(71)
              else
                 write(6,*)'READ_BUFRTOVS:  ', spc_coeff_versions, ' versions of SpcCoeff found for ', trim(sis)
              end if
           else
              spc_coeff_versions = spc_coeff_versions+1

              if( crtm_coeffs_path /= "" ) then
                 error_status = crtm_spccoeff_load(sensorlist,&
                    File_Path = crtm_coeffs_path, quiet=quiet )
              else
                 error_status = crtm_spccoeff_load(sensorlist,quiet=quiet)
              endif
              if (error_status /= success) then
                 write(6,*)'READ_BUFRTOVS:  ***ERROR*** crtm_spccoeff_load error_status=',error_status,&
                    ' despite file ',trim(spc_filename),' existing,   TERMINATE PROGRAM EXECUTION'
                 call stop2(71)
              endif

              ninstruments = size(sc)
              instrument_loop: do n=1,ninstruments
                 if(sis == sc(n)%sensor_id)then
                    instrument=n
                    exit instrument_loop
                 end if
              end do instrument_loop
              if(instrument == 0)then
                 write(6,*)' failure to find instrument in read_bufrtovs ',sis
              end if

              accoeff_sets(spc_coeff_versions) = sc(instrument)%ac

              ! deallocate crtm info
              error_status = crtm_spccoeff_destroy()
              if (error_status /= success) &
                 write(6,*)'OBSERVER:  ***ERROR*** crtm_spccoeff_destroy error_status=',error_status
           end if
        end do

     end if

!    Loop to read bufr file
     irecx=0
     read_subset: do while(ireadmg(lnbufr,subset,idate)>=0)
        irecx=irecx+1
        if(irecx < nrec_startx) cycle read_subset
        irec=irec+1
        next=next+1
        if(next == npe_sub)next=0
        if(next/=mype_sub)cycle read_subset
        read_loop: do while (ireadsb(lnbufr)==0)

!          Read header record.  (llll=1 is normal feed, 2=EARS data, 3=DB data)
           call ufbint(lnbufr,bfr1bhdr,n1bhdr,1,iret,hdr1b)

!          Extract satellite id.  If not the one we want, read next record
           ksatid=nint(bfr1bhdr(1))
           if(ksatid /= kidsat) cycle read_subset
           rsat=bfr1bhdr(1) 

!          If msu, drop obs from first (1) and last (11) scan positions
           ifov = nint(bfr1bhdr(2))
           if (use_edges) then 
              if (msu .and. (ifov==1 .or. ifov==11)) cycle read_loop
           else if ((ifov < radedge_min .OR. ifov > radedge_max )) then
              cycle read_loop
           end if

           ! Check that ifov is not out of range of cbias dimension
           if (ifov < 1 .OR. ifov > 90) cycle read_loop

!          Extract observation location and other required information
           if(abs(bfr1bhdr(11)) <= 90._r_kind .and. abs(bfr1bhdr(12)) <= r360)then
              dlat_earth = bfr1bhdr(11)
              dlon_earth = bfr1bhdr(12)
           elseif(abs(bfr1bhdr(9)) <= 90._r_kind .and. abs(bfr1bhdr(10)) <= r360)then
              dlat_earth = bfr1bhdr(9)
              dlon_earth = bfr1bhdr(10)
           else
              cycle read_loop
           end if
           if(dlon_earth<zero)  dlon_earth = dlon_earth+r360
           if(dlon_earth>=r360) dlon_earth = dlon_earth-r360
           dlat_earth_deg = dlat_earth
           dlon_earth_deg = dlon_earth
           dlat_earth = dlat_earth*deg2rad
           dlon_earth = dlon_earth*deg2rad

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
              
!             Check to see if in domain
              if(outside) cycle read_loop

!          Global case
           else
              dlat=dlat_earth
              dlon=dlon_earth
              call grdcrd1(dlat,rlats,nlat,1)
              call grdcrd1(dlon,rlons,nlon,1)
           endif

!          Extract date information.  If time outside window, skip this obs
           idate5(1) = bfr1bhdr(3) !year
           idate5(2) = bfr1bhdr(4) !month
           idate5(3) = bfr1bhdr(5) !day
           idate5(4) = bfr1bhdr(6) !hour
           idate5(5) = bfr1bhdr(7) !minute
           call w3fs21(idate5,nmind)
           t4dv= (real((nmind-iwinbgn),r_kind) + bfr1bhdr(8)*r60inv)*r60inv    ! add in seconds
           sstime= real(nmind,r_kind) + bfr1bhdr(8)*r60inv    ! add in seconds
           tdiff=(sstime-gstime)*r60inv
           if (l4dvar.or.l4densvar) then
              if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
           else
              if(abs(tdiff) > twind) cycle read_loop
           endif

           nread=nread+nchanl

           terrain = 50._r_kind
           if(llll == 1)terrain = 0.01_r_kind*abs(bfr1bhdr(13))                   
           crit0 = 0.01_r_kind + terrain
           if (llll >  1 ) crit0 = crit0 + r100 * real(llll,r_kind)
           timeinflat=two
           call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)
           call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)
           if(.not. iuse)cycle read_loop

!          Extract satellite antenna corrections version number
           if (llll > 1) then
              sacv = nint(bfr1bhdr(14))
              if (sacv > spc_coeff_versions) then
                 write(6,*) 'READ_BUFRTOVS WARNING sacv greater than spc_coeff_versions',' ',jsatid,' ',obstype
              end if
           else ! normal feed doesn't have antenna correction, so set sacv to 0
              sacv = 0
           end if


           call ufbint(lnbufr,bfr2bhdr,n2bhdr,1,iret,hdr2b)

!          Set common predictor parameters
           ifovmod=ifov

!          Account for assymetry due to satellite build error
           if(hirs .and. ((jsatid == 'n16') .or. (jsatid == 'n17'))) &
              ifovmod=ifovmod+1

           panglr=(start+real(ifovmod-1,r_kind)*step)*deg2rad
           lzaest = asin(rato*sin(panglr))
           if( msu .or. hirs2 .or. ssu)then
              lza = lzaest
           else
              lza = bfr2bhdr(1)*deg2rad      ! local zenith angle
              if((amsua .and. ifovmod <= 15) .or.        &
                 (amsub .and. ifovmod <= 45) .or.        &
                 (mhs   .and. ifovmod <= 45) .or.        &
                 (hirs  .and. ifovmod <= 28) )   lza=-lza
           end if

!  Check for errors in satellite zenith angles 
           if(abs(lzaest-lza)*rad2deg > one) then
              write(6,*)' READ_BUFRTOVS WARNING uncertainty in lza ', &
                 lza*rad2deg,lzaest*rad2deg,sis,ifovmod,start,step
              cycle read_loop
           end if

           if(abs(lza)*rad2deg > MAX_SENSOR_ZENITH_ANGLE) then
              write(6,*)'READ_BUFRTOVS WARNING lza error ',bfr2bhdr(1),panglr
              cycle read_loop
           end if

           sat_aziang=bfr2bhdr(3)
           if (abs(sat_aziang) > r360) then
              sat_aziang=zero
!_RT              write(6,*) 'READ_BUFRTOVS: bad azimuth angle ',sat_aziang
!_RT              cycle read_loop
           endif

!          Read data record.  Increment data counter
!          TMBR is actually the antenna temperature for most microwave sounders.
!          Changed from accepting TMBR and converting TMBRST to
!          antenna temperature (remove_antcorr), to converting TMBR to
!          brightness temperature (add_antcorr) and accepting TMBRST
!

           if (ta2tb) then

              if (llll == 1) then
                 call ufbrep(lnbufr,data1b8,1,nchanl,iret,'TMBR')
                 if ( (amsua .or. amsub .or. mhs) .and. &
                      .not.(jsatid == 'n15' .or. jsatid == 'n16') )then
                    ! convert antenna temperature to brightness temperature,
                    ! unless the satellite is n15 or n16, because tranamsua
                    ! does this conversion because the coefficient files exist
                    ! for it to use
                    data1b4=data1b8
                    !call apply_antcorr(accoeff_sets(spc_coeff_versions),ifov,data1b4)
                    call apply_antcorr(accoeff_sets(1),ifov,data1b4)
                    do j=1,nchanl
                       if(data1b8(j) > r1000)then
                         data1b8(j) = 1000000._r_kind
                       else
                         data1b8(j) = data1b4(j)
                       end if
                    end do
                 end if
              else     ! EARS / DB
                 call ufbrep(lnbufr,data1b8,1,nchanl,iret,'TMBRST')
                 !if ( amsua .or. amsub .or. mhs .AND. sacv .ne. spc_coeff_versions)then
                 if ( amsua .or. amsub .or. mhs .AND. spc_coeff_versions /= 1 .AND. sacv /= 1)then
                    ! convert brightness temperature to antenna temperature using
                    ! the satellite antenna correction version (sacv) used by the
                    ! data originator,
                    ! then convert back to brightness temperature using the version
                    ! of parameters used by the CRTM
                    data1b4=data1b8
                    call remove_antcorr(accoeff_sets(sacv),ifov,data1b4)
                    !call apply_antcorr(accoeff_sets(spc_coeff_versions),ifov,data1b4)
                    call apply_antcorr(accoeff_sets(1),ifov,data1b4)
                    do j=1,nchanl
                       if(data1b8(j) > r1000) then
                         data1b8(j) = 1000000._r_kind
                       else
                         data1b8(j)=data1b4(j)
                       end if
                    end do
                 end if
              end if

           else

              if (llll == 1) then
                 call ufbrep(lnbufr,data1b8,1,nchanl,iret,'TMBR')
              else     ! EARS / DB
                 call ufbrep(lnbufr,data1b8,1,nchanl,iret,'TMBRST')
                 if ( amsua .or. amsub .or. mhs )then
                    data1b4=data1b8
                    call remove_antcorr(accoeff_sets(1),ifov,data1b4)
                    do j=1,nchanl
                       if(data1b8(j) > r1000)then
                         data1b8(j) = 1000000._r_kind
                       else
                         data1b8(j) = data1b4(j)
                       end if
                    end do
                 end if
              end if

           endif

!          Transfer observed brightness temperature to work array.  If any
!          temperature exceeds limits, reset observation to "bad" value
           iskip=0 
           critical_channels_missing = .false.
           do j=1,nchanl
              if (data1b8(j) < tbmin .or. data1b8(j) > tbmax) then
                 iskip = iskip + 1

!                Flag profiles where key channels are bad  
                 if(( msu  .and.  j == ich1) .or.                                 &
                    (amsua .and. (j == ich1 .or. j == ich2 .or. j == ich3 .or.    &
                                  j == ich4 .or. j == ich6 .or. j == ich15 )) .or.&
                    (hirs  .and. (j == ich8 )) .or.                               &
                    (amsub .and.  j == ich1) .or.                                 &
                    (mhs   .and. (j == ich1 .or. j == ich2)) ) critical_channels_missing = .true.
              endif
           end do
           if (iskip >= nchanl) cycle read_loop
!          Map obs to thinning grid
           crit1 = crit1 + 10._r_kind*real(iskip,r_kind)
           call checkob(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop

!          Determine surface properties based on 
!          sst and land/sea/ice mask   
!
!          isflg    - surface flag
!                     0 sea
!                     1 land
!                     2 sea ice
!                     3 snow
!                     4 mixed                       

!          FOV-based surface code requires fov number.  if out-of-range, then
!          skip this ob.

           if (isfcalc == 1) then
              call fov_check(ifov,instr,ichan,valid)
              if (.not. valid) cycle read_loop

!          When isfcalc is one, calculate surface fields based on size/shape of fov.
!          Otherwise, use bilinear method.

              call deter_sfc_fov(fov_flag,ifov,instr,ichan,sat_aziang,dlat_earth_deg,&
                 dlon_earth_deg,expansion,t4dv,isflg,idomsfc(1), &
                 sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
           else
              call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg, &
                 idomsfc(1),sfcpct,ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
           endif


           crit1 = crit1 + rlndsea(isflg) 
           call checkob(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop


           if (critical_channels_missing) then

             pred=1.0e8_r_kind

           else

!             Set data quality predictor
              if (msu) then
                 if (newpc4pred) then
                    ch1 = data1b8(ich1)-ang_rad(ichan1)*cbias(ifov,ichan1)- &
                         predx(1,ichan1)*air_rad(ichan1)
                 else
                    ch1 = data1b8(ich1)-ang_rad(ichan1)*cbias(ifov,ichan1)- &
                         r01*predx(1,ichan1)*air_rad(ichan1)
                 end if
                 ch1flg = tsavg-ch1
                 if(isflg == 0)then
                    pred = 100._r_kind-min(ch1flg,100.0_r_kind)
                 else
                    pred = abs(ch1flg)
                 end if
              else if (hirs) then
                 if (newpc4pred) then
                    ch8 = data1b8(ich8) -ang_rad(ichan8)*cbias(ifov,ichan8)- &
                         predx(1,ichan8)*air_rad(ichan8)
                 else
                    ch8 = data1b8(ich8) -ang_rad(ichan8)*cbias(ifov,ichan8)- &
                         r01*predx(1,ichan8)*air_rad(ichan8)
                 end if
                 ch8flg = tsavg-ch8
                 pred   = 10.0_r_kind*max(zero,ch8flg)
              else if (amsua) then
!                Remove angle dependent pattern (not mean)
                 if (adp_anglebc .and. newpc4pred) then
                    ch1 = data1b8(ich1)-ang_rad(ichan1)*cbias(ifov,ichan1) 
                    ch2 = data1b8(ich2)-ang_rad(ichan2)*cbias(ifov,ichan2) 
                    ch15= data1b8(ich15)-ang_rad(ichan15)*cbias(ifov,ichan15)
                 else
                    ch1 = data1b8(ich1)-ang_rad(ichan1)*cbias(ifov,ichan1)+ &
                         air_rad(ichan1)*cbias(15,ichan1)
                    ch2 = data1b8(ich2)-ang_rad(ichan2)*cbias(ifov,ichan2)+ &
                         air_rad(ichan2)*cbias(15,ichan2)   
                    ch15= data1b8(ich15)-ang_rad(ichan15)*cbias(ifov,ichan15)
                 end if
                 if (isflg == 0 .and. ch1<285.0_r_kind .and. ch2<285.0_r_kind) then
                    cosza = cos(lza)
                    d0    = 8.24_r_kind - 2.622_r_kind*cosza + 1.846_r_kind*cosza*cosza                                 
                    qval=cosza*(d0+d1*log(285.0_r_kind-ch1)+d2*log(285.0_r_kind-ch2))
                    if (radmod%lcloud_fwd) then
                       ! no preference in selecting clouds/precipitation
                       ! qval=zero 
                       ! favor non-precipitating clouds                                                   
                       qval=-113.2_r_kind+(2.41_r_kind-0.0049_r_kind*ch1)*ch1 +  &         
                            0.454_r_kind*ch2-ch15   
                       if (qval>=9.0_r_kind) then
                          qval=1000.0_r_kind*qval
                       else
                          qval=zero
                       end if
                       if (radmod%lprecip) qval=zero  
                       ! favor thinner clouds
                       ! cosza = cos(lza)
                       ! d0= 8.24_r_kind - 2.622_r_kind*cosza + 1.846_r_kind*cosza*cosza
                       ! qval=cosza*(d0+d1*log(285.0_r_kind-ch1)+d2*log(285.0_r_kind-ch2))
                       ! if (qval>0.2_r_kind) then
                       !    qval=1000.0_r_kind*qval
                       ! else
                       !    qval=zero
                       ! end if
                    end if
                    pred  = max(zero,qval)*100.0_r_kind
                 else
                    if (adp_anglebc .and. newpc4pred) then
                       ch3 = data1b8(ich3)-ang_rad(ichan3)*cbias(ifov,ichan3) 
                       ch15 = data1b8(ich15)-ang_rad(ichan15)*cbias(ifov,ichan15) 
                    else
                       ch3  = data1b8(ich3)-ang_rad(ichan3)*cbias(ifov,ichan3)+ &
                            air_rad(ichan3)*cbias(15,ichan3)   
                       ch15 = data1b8(ich15)-ang_rad(ichan15)*cbias(ifov,ichan15)+ &
                            air_rad(ichan15)*cbias(15,ichan15)
                    end if
                    pred = abs(ch1-ch15)
                    if(ch1-ch15 >= three) then
                       df2  = 5.10_r_kind +0.78_r_kind*ch1-0.96_r_kind*ch3
                       tt   = 168._r_kind-0.49_r_kind*ch15
                       if(ch1 > 261._r_kind .or. ch1 >= tt .or. & 
                            (ch15 <= 273._r_kind .and. df2 >= 0.6_r_kind))then
                          pred = 100._r_kind
                       end if
                    end if
                 endif
                 
!                 sval=-113.2_r_kind+(2.41_r_kind-0.0049_r_kind*ch1)*ch1 +  &
!                 0.454_r_kind*ch2-ch15
                 
              else if (amsub .or. mhs) then
                 if (newpc4pred) then
                    ch1 = data1b8(ich1)-ang_rad(ichan1)*cbias(ifov,ichan1)- &
                         predx(1,ichan1)*air_rad(ichan1)
                    ch2 = data1b8(ich2)-ang_rad(ichan2)*cbias(ifov,ichan2)- &
                         predx(1,ichan2)*air_rad(ichan2)
                 else
                    ch1 = data1b8(ich1)-ang_rad(ichan1)*cbias(ifov,ichan1)- &
                         r01*predx(1,ichan1)*air_rad(ichan1)
                    ch2 = data1b8(ich2)-ang_rad(ichan2)*cbias(ifov,ichan2)- &
                         r01*predx(1,ichan2)*air_rad(ichan2)
                 end if
                 pred_water = zero
                 if(sfcpct(0) > zero)then
                    cosza = cos(lza)
                    if(ch2 < h300)then 
                       pred_water = (0.13_r_kind*(ch1+33.58_r_kind*log(h300-ch2)- &
                            341.17_r_kind))*five
                    else
                       pred_water = 100._r_kind
                    end if
                 end if
                 pred_not_water = 42.72_r_kind + 0.85_r_kind*ch1-ch2
                 pred = (sfcpct(0)*pred_water) + ((one-sfcpct(0))*pred_not_water)
                 pred = max(zero,pred)
                 
              endif
              
           end if

!          Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
           crit1 = crit1+pred 
           call finalcheck(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop

!          interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
           if(nst_gsi>0) then
              tref  = ts(0)
              dtw   = zero
              dtc   = zero
              tz_tr = one
              if(sfcpct(0)>zero) then
                 call gsi_nstcoupler_deter(dlat_earth,dlon_earth,t4dv,zob,tref,dtw,dtc,tz_tr)
              endif
           endif


!          Load selected observation into data array
              
           data_all(1 ,itx)= rsat                      ! satellite ID
           data_all(2 ,itx)= t4dv                      ! time
           data_all(3 ,itx)= dlon                      ! grid relative longitude
           data_all(4 ,itx)= dlat                      ! grid relative latitude
           data_all(5 ,itx)= lza                       ! local zenith angle
           data_all(6 ,itx)= bfr2bhdr(3)               ! local azimuth angle
           data_all(7 ,itx)= panglr                    ! look angle
           data_all(8 ,itx)= ifov                      ! scan position
           data_all(9 ,itx)= bfr2bhdr(2)               ! solar zenith angle
           data_all(10,itx)= bfr2bhdr(4)               ! solar azimuth angle
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
           end if

           if(nst_gsi>0) then
              data_all(maxinfo+1,itx) = tref            ! foundation temperature
              data_all(maxinfo+2,itx) = dtw             ! dt_warm at zob
              data_all(maxinfo+3,itx) = dtc             ! dt_cool at zob
              data_all(maxinfo+4,itx) = tz_tr           ! d(Tz)/d(Tr)
           endif

           do i=1,nchanl
              data_all(i+nreal,itx)=data1b8(i)
           end do
           nrec(itx)=irec

!       End of bufr read loops
        enddo read_loop
     enddo read_subset
     call closbf(lnbufr)
     close(lnbufr)

  end do ears_db_loop
  deallocate(data1b8,data1b4)

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
     nele,itxmax,nread,ndata,data_all,score_crit,nrec)

! 
  if(mype_sub==mype_root)then
     do n=1,ndata
        do i=1,nchanl
           if(data_all(i+nreal,n) > tbmin .and. &
              data_all(i+nreal,n) < tbmax)nodata=nodata+1
        end do
     end do
     if(dval_use .and. assim)then
        do n=1,ndata
           itt=nint(data_all(33,n))
           super_val(itt)=super_val(itt)+val_tovs
        end do
     end if

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

  if(diagnostic_reg.and.ntest>0) write(6,*)'READ_BUFRTOVS:  ',&
     'mype,ntest,disterrmax=',mype,ntest,disterrmax

! End of routine
  return

end subroutine read_bufrtovs
