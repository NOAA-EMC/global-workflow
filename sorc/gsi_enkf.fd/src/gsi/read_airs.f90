subroutine read_airs(mype,val_airs,ithin,isfcalc,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs,nrec_start,dval_use)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_airs                  read bufr format airs data
! prgmmr :   tahara          org: np20                date: 2002-12-03
!
! abstract:  This routine reads BUFR format AQUA radiance (brightness
!            temperature) files.  Optionally, the data are thinned to 
!            a specified resolution using simple quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2002-12-03  tahara  - read aqua data in new bufr format
!   2004-05-28  kleist  - subroutine call update
!   2004-06-16  treadon - update documentation
!   2004-07-23  derber  - make changes to eliminate obs. earlier in thinning
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-08-25  eliu    - added option to read separate bufr table
!   2004-10-15  derber  - increase weight given to surface channel check
!                         in AIRS data selection algorithm
!   2005-01-26  derber - land/sea determination and weighting for data selection
!   2005-07-07  derber - clean up code and improve selection criteria
!   2005-09-08  derber - modify to use input group time window
!   2005-09-28  derber - modify to produce consistent surface info
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-11-22  derber  - include mean in bias correction
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-03-07  derber - correct error in nodata count
!   2006-03-09  jung - correct sat zenith angle error (used before defined)
!   2006-04-21  keyser/treadon - modify ufbseq calls to account for change
!                                in NCEP bufr sequence for AIRS data
!   2006-05-19  eliu   - add logic to reset relative weight when all channels not used
!   2006-07-28  derber - modify reads so ufbseq not necessary
!                      - add solar and satellite azimuth angles remove isflg from output
!   2006-08-25  treadon - replace serial bufr i/o with parallel bufr i/o (mpi_io)
!   2006-12-15  todling - trim table filename (also made shorter word!)
!   2007-01-17  liu     - fix in channel numbering in weight reset logics
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2008-04-21  safford - rm unused vars and uses
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2009-01-09  gayno   - new option to calculate surface fields within FOV
!                         based on its size/shape
!   2009-04-18  woollen - improve mpi_io interface with bufrlib routines
!   2009-04-21  derber  - add ithin to call to makegrids
!   2009-09-01  li      - add to handle nst fields
!   2009-12-20  gayno - method to calculate surface fields within FOV
!                       based on its size/shape now calculates antenna 
!                       power for some instruments. 
!   2010-07-12  zhu   - include global offset in amsua bc for adp_anglebc option
!   2010-09-02  zhu   - add use_edges option
!   2010-10-12  zhu   - use radstep and radstart from radinfo
!   2011-04-07  todling - newpc4pred now in radinfo
!   2011-04-08  li      - (1) use nst_gsi, nstinfo, fac_dtl, fac_tsl and add NSST vars
!                         (2) get zob, tz_tr (call skindepth and cal_tztr)
!                         (3) interpolate NSST Variables to Obs. location (call deter_nst)
!                         (4) add more elements (nstinfo) in data array
!   2011-04-15  jung  - added use of acqf flag from bufr file to reject bad channels
!   2011-08-01  lueken  - added module use deter_sfc_mod and fixed indentation
!   2011-09-13  gayno - improve error handling for FOV-based sfc calculation
!                       (isfcalc=1)
!   2011-12-13  collard Replace find_edges code to speed up execution.
!   2012-03-05  akella  nst now controlled via coupler
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2015-10-22  Jung    - added logic to allow subset changes based on the satinfo file
!   2018-05-21  j.jin   - added time-thinning. Moved the checking of thin4d into satthin.F90.
!
!   input argument list:
!     mype     - mpi task id
!     val_airs - weighting factor applied to super obs
!     ithin    - flag to thin data
!     isfcalc  - specify method to calculate surface fields within FOV
!                when set to one, integrate surface info across FOV based
!                on its size/shape.  when not one, use bilinear interpolation.
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - sensor/instrument/satellite indicator
!     mype_root - "root" task for sub-communicator
!     mype_sub - mpi task id within sub-communicator
!     npe_sub  - number of data read tasks
!     mpi_comm_sub - sub-communicator for data read
!     dval_use - logical for using dval (val_airs)
!     nrec_start - first subset with useful information
!
!   output argument list:
!     nread    - number of BUFR AQUA observations read
!     ndata    - number of BUFR AQUA profiles retained for further processing
!     nodata   - number of BUFR AQUA observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
! Use modules
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
      finalcheck,checkob,score_crit
  use satthin,   only: radthin_time_info,tdiff2crit
  use obsmod,    only: time_window_max
  use radinfo, only: cbias,newchn,iuse_rad,nusis,jpch_rad,ang_rad, &
      nuchan, adp_anglebc,use_edges,radedge1,radedge2, &
      radstep,radstart,newpc4pred
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,&
      tll2xy,txy2ll,rlats,rlons
  use constants, only: zero,deg2rad,one,three,five,rad2deg,r60inv
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use calc_fov_crosstrk, only : instrument_init, fov_cleanup, fov_check
  use deter_sfc_mod, only: deter_sfc_fov,deter_sfc
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter
  use mpimod, only: npe
  use gsi_io, only: verbose
! use radiance_mod, only: rad_obs_type

  implicit none

! BUFR format for AQUASPOT 
! Input variables
  integer(i_kind)  ,intent(in   ) :: mype,nrec_start
  real(r_kind)     ,intent(in   ) :: twind
  integer(i_kind)  ,intent(in   ) :: ithin
  integer(i_kind)  ,intent(inout) :: isfcalc
  character(len=*) ,intent(in   ) :: jsatid
  character(len=*) ,intent(in   ) :: infile
  character(len=*) ,intent(in   ) :: obstype
  real(r_kind)     ,intent(in   ) :: gstime
  integer(i_kind)  ,intent(in   ) :: lunout
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
  integer(i_kind)  ,intent(  out) :: ndata,nodata
  
! Input/Output variables
  real(r_kind)     ,intent(inout) :: val_airs

! BUFR file sequencial number
  character(len=512)  :: table_file
  integer(i_kind)     :: lnbufr = 10
  integer(i_kind)     :: lnbufrtab = 11
  integer(i_kind)     :: irec,next

! Variables for BUFR IO    
  real(r_double) :: crchn_reps
  real(r_double),dimension(2) :: aquaspot
  real(r_double),dimension(12,3) :: allspot
  real(r_double),allocatable,dimension(:,:) :: allchan
  integer(i_kind),allocatable, dimension(:) :: chan_map
  integer(i_kind) :: bufr_size
  
  real(r_kind)      :: step, start
  character(len=8)  :: subset
  character(len=4)  :: senname
  character(len=80) :: allspotlist
  integer(i_kind)   :: iret, ireadmg,ireadsb


! Work variables for time
  integer(i_kind)   :: idate
  integer(i_kind)   :: idate5(5)
  real(r_kind)      :: sstime, tdiff, t4dv
  integer(i_kind)   :: nmind


! Other work variables
  integer(i_kind)  :: nreal, isflg
  integer(i_kind)  :: itx, k, nele, itt, n,ix
  integer(i_kind)  :: qc_1, qc_2, qc_3, qc_15    ! amsua quality control bufr_index values
  real(r_kind)     :: chsstf,chsst,chsst_all,sfcr
  real(r_kind)     :: ch15, ch3, df2, tt
  real(r_kind)     :: dlon, dlat
  real(r_kind)     :: dlon_earth,dlat_earth, lza
  real(r_kind)     :: pred, crit1, qval, ch1, ch2, d0, cosza, dist1
  real(r_kind)     :: sat_zenang, sol_zenang, sat_aziang, sol_aziang
  real(r_kind)     :: ch8ch18, ch8ch19, ch18ch19, tmpinv
  real(r_kind)     :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind)     :: zob,tref,dtw,dtc,tz_tr
  real(r_kind),dimension(0:4) :: rlndsea
  real(r_kind),dimension(0:3) :: sfcpct
  real(r_kind),dimension(0:3) :: ts

  integer(i_kind)  :: ifov, ioff, instr, ichan
  logical          :: outside,iuse,assim,lluse,valid
  integer(i_kind)  :: i, l, iskip
  integer(i_kind),allocatable,dimension(:)::nrec
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind) :: dlat_earth_deg, dlon_earth_deg, expansion
  integer(i_kind):: idomsfc(1)
  integer(i_kind):: radedge_min, radedge_max, maxinfo
  integer(i_kind)   :: subset_start, subset_end, satinfo_nchan
  integer(i_kind)   :: bufr_start, bufr_end, bufr_nchan
  integer(i_kind),allocatable, dimension(:) :: bufr_index
  integer(i_kind),allocatable, dimension(:) :: bufr_chan_test


! Set standard parameters
  character(8),parameter:: fov_flag="crosstrk"
  real(r_kind),parameter:: R90    =  90._r_kind
  real(r_kind),parameter:: R360   = 360._r_kind
  real(r_kind),parameter:: d1     = 0.754_r_kind
  real(r_kind),parameter:: d2     = -2.265_r_kind
  real(r_kind),parameter:: tbmin  = 50._r_kind
  real(r_kind),parameter:: tbmax  = 550._r_kind
  integer(i_kind),parameter :: ilon = 3
  integer(i_kind),parameter :: ilat = 4

  real(r_kind) cdist,disterr,disterrmax,dlon00,dlat00
  integer(i_kind) ntest

  logical           :: airs, amsua, hsb, airstab
  real(r_kind)      :: ptime,timeinflat,crit0
  integer(i_kind)   :: ithin_time,n_tbin,it_mesh
  logical print_verbose


  print_verbose = .false.
  if(verbose)print_verbose=.true.
! Initialize variables
  maxinfo    =  31
  disterrmax=zero
  ntest=0
  if(dval_use) maxinfo = maxinfo+2
  nreal  = maxinfo+nstinfo
  ndata = 0
  nodata = 0
  airs=      obstype == 'airs'
  amsua=     obstype == 'amsua'
  hsb=       obstype == 'hsb'

  if (nst_gsi > 0 ) then
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif

  radedge_min = 0
  radedge_max = 1000

! find the airs/amsu/hsb offset in the jpch_rad list.  This is for the isue flag
! and count the number of channels in the satinfo file.
  ioff=jpch_rad
  subset_start = 0
  subset_end = 0
  assim = .false.
  do i=1,jpch_rad
     if (trim(nusis(i))==trim(sis)) then
        ioff = min(ioff,i) ! airs/amsu/hsp offset
        if (subset_start == 0) then
           step  = radstep(i)
           start = radstart(i)
           if (radedge1(i)/=-1 .and. radedge2(i)/=-1) then
              radedge_min=radedge1(i)
              radedge_max=radedge2(i)
           end if
           subset_start = i
        endif
        if (iuse_rad(i) > 0) assim = .true.    ! Are any of the airs/amsu/hsb channels being used?
        subset_end = i
     endif
  end do 
  satinfo_nchan = subset_end - subset_start + 1
  allocate(bufr_index(satinfo_nchan)) 
  ioff = ioff -1

! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.
  if ( .not. assim) val_airs=zero

  if(airs)then
     ix=1
     senname = 'AIRS'
     allocate( chan_map(2378))
     if(isfcalc==1) then
        instr=17
        ichan=-999  ! not used for airs
        expansion=one ! use one for ir sensors
     endif
     if (mype_sub==mype_root .and. print_verbose) &
        write(6,*)'READ_AIRS:  airs offset ',ioff
  else if(amsua)then
     ix=2
     senname = 'AMSU'
     allocate( chan_map(15))
     if(isfcalc==1) then
        instr=11
        ichan=15  ! for now pick a surface channel
        expansion=2.9_r_kind ! use almost three for microwave
     endif
     if (mype_sub==mype_root .and. print_verbose) &
        write(6,*)'READ_AIRS:  amsu offset ',ioff
  else if(hsb)then
     ix=3
     senname = 'HSB'
     if(isfcalc==1) then
        instr=12 ! similar to amsu-b according to tom kleespies
        ichan=-999 ! not used for hsb
        expansion=2.9_r_kind ! use almost three for microwave
     endif
  endif

  allspotlist='SIID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA BEARAZ FOVN'

! Calculate parameters needed for FOV-based surface calculation.
  if (isfcalc == 1) then
     call instrument_init(instr,jsatid,expansion,valid)
     if (.not. valid) then
       if (assim) then
         write(6,*)'READ_AIRS:  ***ERROR*** IN SETUP OF FOV-SFC CODE. STOP'
         call stop2(71)
       else
         call fov_cleanup
         isfcalc = 0
         write(6,*)'READ_AIRS:  ***ERROR*** IN SETUP OF FOV-SFC CODE'
       endif
     endif
  endif

  if(isfcalc==1)then
    rlndsea = zero
  else
    if (airs)then
      rlndsea(0) = zero                       
      rlndsea(1) = 10._r_kind
      rlndsea(2) = 15._r_kind
      rlndsea(3) = 10._r_kind
      rlndsea(4) = 30._r_kind
    elseif (amsua.or.hsb)then
      rlndsea(0) = zero
      rlndsea(1) = 15._r_kind
      rlndsea(2) = 20._r_kind
      rlndsea(3) = 15._r_kind
      rlndsea(4) = 100._r_kind
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
  open(lnbufr,file=trim(infile),form='unformatted')

! Open BUFR table
  table_file = 'airs_bufr.table'      ! make table file name
  inquire(file=table_file,exist=airstab)
  if (airstab) then
     if (mype_sub==mype_root .and. print_verbose) &
        write(6,*)'READ_AIRS:  Reading BUFR Table A file: ',trim(table_file)
     open(lnbufrtab,file=trim(table_file))
     call openbf(lnbufr,'IN',lnbufrtab)
  else
     call openbf(lnbufr,'IN',lnbufr)
  endif
  call datelen(10)

! Allocate arrays to hold data
! The number of channels in obtained from the satinfo file being used.
  nele=nreal+satinfo_nchan
  allocate(data_all(nele,itxmax),nrec(itxmax))
  allocate(allchan(3,1))     ! actual values set after ireadsb
  allocate(bufr_chan_test(1))! actual values set after ireadsb

! Big loop to read data file
  nrec=999999
  next=0
  irec=0
  read_subset: do while(ireadmg(lnbufr,subset,idate)>=0)
     irec=irec+1
     if(irec < nrec_start) cycle read_subset
     next=next+1
     if(next == npe_sub)next=0
     if(next /= mype_sub)cycle read_subset

     read_loop: do while (ireadsb(lnbufr)==0)

!       Get the size of the channels and radiance (allchan) array
        call ufbint(lnbufr,crchn_reps,1,1,iret, '(SCBTSEQN)')
        bufr_nchan = int(crchn_reps) + 24  ! 4 AIRS imager + 15 amsu + 5 HSB
                                           ! (In the bufr format HSB has 5 not 4
                                           ! channels!) 

        bufr_size = size(allchan,2)
        if ( bufr_size /= bufr_nchan ) then
!          Allocate the arrays needed for the channel and radiance array
           deallocate(allchan, bufr_chan_test)
           allocate(allchan(3,bufr_nchan))
           allocate(bufr_chan_test(bufr_nchan))
           bufr_chan_test(:)=0
        endif

!       Read AIRSSPOT , AMSUSPOT and HSBSPOT
!       AIRS > ix = 1, AMSU > ix = 2, HSB > ix = 3
        call ufbrep(lnbufr,allspot,12,3,iret,allspotlist)

        if(iret /= 3) cycle read_loop

        sat_aziang=allspot(11,ix)
        if (abs(sat_aziang) > r360) then
!          write(6,*)  'READ_AIRS: bad azimuth angle ',sat_aziang
           cycle read_loop
        endif

!       Remove data on edges
        ifov = nint( allspot(12,ix) )
        if (.not. use_edges .and. &
             (ifov < radedge_min .OR. ifov > radedge_max )) cycle read_loop

!       Check observational info
        sat_zenang  = allspot(10,ix) 
        if( ifov < 0 .or. ifov > 100 .or. abs(sat_zenang) > 360._r_kind ) then
           write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
              ' STRANGE OBS INFO(FOV,SAZA):', allspot(12,ix), allspot(10,ix)
           cycle read_loop
        endif

        dlat_earth = allspot(8,ix)
        dlon_earth = allspot(9,ix)
!       Check observing position
        if( abs(dlat_earth) > R90  .or. abs(dlon_earth) > R360 .or. &
           (abs(dlat_earth) == R90 .and. dlon_earth /= ZERO) )then
!          write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
!             ' STRANGE OBS POINT (LAT,LON):', dlat_earth, dlon_earth
           cycle read_loop
        endif

!       Retrieve observing position
        if(dlon_earth >= R360)then
           dlon_earth = dlon_earth - R360
        else if(dlon_earth < ZERO)then
           dlon_earth = dlon_earth + R360
        endif

        dlat_earth_deg = dlat_earth
        dlon_earth_deg = dlon_earth

        dlat_earth = dlat_earth * deg2rad
        dlon_earth = dlon_earth * deg2rad


!       If regional, map obs lat,lon to rotated grid.
        if(regional)then

!       Convert to rotated coordinate.  dlon centered on 180 (pi),
!       so always positive for limited area
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

!       Check to see if in domain.  outside=.true. if dlon_earth,
!       dlat_earth outside domain, =.false. if inside
           if(outside) cycle read_loop

!       Gobal case 
        else
           dlat = dlat_earth
           dlon = dlon_earth
           call grdcrd1(dlat,rlats,nlat,1)
           call grdcrd1(dlon,rlons,nlon,1)
        endif

!       Check obs time
        idate5(1) = nint(allspot(2,ix)) ! year
        idate5(2) = nint(allspot(3,ix)) ! month
        idate5(3) = nint(allspot(4,ix)) ! day
        idate5(4) = nint(allspot(5,ix)) ! hour
        idate5(5) = nint(allspot(6,ix)) ! minute

        if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
            idate5(2) < 1    .or. idate5(2) >   12 .or. &
            idate5(3) < 1    .or. idate5(3) >   31 .or. &
            idate5(4) <0     .or. idate5(4) >   24 .or. &
            idate5(5) <0     .or. idate5(5) >   60 )then

            write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
               ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
            cycle read_loop

        endif

!       Retrieve obs time
        call w3fs21(idate5,nmind)
        t4dv = (real((nmind-iwinbgn),r_kind) + real(allspot(7,ix),r_kind)*r60inv)*r60inv ! add in seconds
        sstime = real(nmind,r_kind) + real(allspot(7,ix),r_kind)*r60inv ! add in seconds
        tdiff = (sstime - gstime)*r60inv

        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           if (abs(tdiff)>twind) cycle read_loop
        endif

!       Increment nread ounter by satinfo_nchan
        nread = nread + satinfo_nchan

        crit0 = 0.01_r_kind
        timeinflat=6.0_r_kind
        call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)
        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)
        if(.not. iuse)cycle read_loop

!      "Score" observation.  We use this information to identify "best" obs
!       Locate the observation on the analysis grid.  Get sst and land/sea/ice
!       mask.  
!        isflg    - surface flag
!                   0 sea
!                   1 land
!                   2 sea ice
!                   3 snow
!                   4 mixed 

!       The field of view number is used when calculating the surface fields
!       based on the fov's size/shape.  if it is out-of-range, skip ob.

        if (isfcalc == 1) then
           call fov_check(ifov,instr,ichan,valid)
           if (.not. valid) cycle read_loop

!       When isfcalc is one, calculate surface fields based on the fov's size/shape.
!       Otherwise, use bilinear interpolation.

           call deter_sfc_fov(fov_flag,ifov,instr,ichan,sat_aziang,dlat_earth_deg, &
              dlon_earth_deg,expansion,t4dv,isflg,idomsfc(1), &
              sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
        else
           call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg, &
              idomsfc(1),sfcpct,ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
        endif

        crit1 = crit1 + rlndsea(isflg)
        call checkob(one,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

!       Set common predictor parameters
        sat_zenang  = sat_zenang  * deg2rad
        sat_aziang  = allspot(11,ix)  

!       Read AQUASPOT
        call ufbint(lnbufr,aquaspot,2,1,iret,'SOZA SOLAZI')
        sol_zenang = aquaspot(1)

!       Read the channel numbers, quality flags, and brightness temperatures
        call ufbrep(lnbufr, allchan,3,bufr_nchan,iret,'CHNM ACQF TMBR')
        if( iret /= bufr_nchan)then
           write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
              iret, ' TEMPERATURE CH DATA IS READ INSTEAD OF ',bufr_nchan
           cycle read_loop
        endif

!       Coordinate bufr channels with satinfo file channels
!       Order in bufr file is airs, hsb, and amsua
        if (airs ) then 
           bufr_start = 1
           bufr_end = bufr_nchan - 24  ! 4 airs visible, 15 amsu chans, 5 hsb chans
           
           do i=1, bufr_end
!             endif
              chan_map(int(allchan(1,i))) = i    ! map channel number position into chan_map
           end do
        elseif (amsua ) then
           bufr_start = bufr_nchan - 19 ! 15 amsua + 5 hsb + 1
           bufr_end = bufr_nchan  - 5   ! 5 hsb
           do i=1, 15
              !  Force allchan values for AMSU to be between 1 and 15 (sometimes they are
              !  28-42!)
              chan_map(i) = i+bufr_start-1
              allchan(1,i+bufr_start-1)=i
           end do
        elseif (hsb) then
           bufr_start = bufr_nchan - 4  ! 5 hsb chans + 1
           bufr_end = bufr_nchan        ! Fin
        endif
        do i=bufr_start, bufr_end
           chan_map(int(allchan(1,i))) = i    ! map channel number position into
        end do

!       Coordinate bufr channels with satinfo file channels
!       If this is the first time or a change in the bufr channels is detected, sync with satinfo file
        if (ANY(int(allchan(1,bufr_start:bufr_end)) /= bufr_chan_test(bufr_start:bufr_end))) then
           bufr_index(:) = 0
           bufr_chans: do l=bufr_start, bufr_end
              bufr_chan_test(l) = int(allchan(1,l))                          ! Copy this bufr channel selection into array for comparison to next profile
              satinfo_chans: do i=1,satinfo_nchan                            ! Loop through sensor (airs) channels in the satinfo file
                 if ( nuchan(ioff+i) == bufr_chan_test(l) ) then             ! Channel found in both bufr and stainfo file
                    bufr_index(i) = l
                    exit satinfo_chans                                       ! go to next bufr channel
                 endif
              end do  satinfo_chans
           end do bufr_chans
        end if

!       Channel based quality control
        if(amsua)then

           qc_1 = newchn(sis,1)
           qc_2 = newchn(sis,2)
           qc_3 = newchn(sis,3)
           qc_15 = newchn(sis,15)

           if(ifov <= 15)sat_zenang = -sat_zenang

           if (adp_anglebc .and. newpc4pred) then
              ch1 = allchan(3,chan_map(1))-ang_rad(qc_1)*cbias(ifov,qc_1) 
              ch2 = allchan(3,chan_map(2))-ang_rad(qc_2)*cbias(ifov,qc_2) 
              ch3 = allchan(3,chan_map(3))-ang_rad(qc_3)*cbias(ifov,qc_3) 
              ch15= allchan(3,chan_map(15))-ang_rad(qc_15)*cbias(ifov,qc_15) 
           else
              ch1    = allchan(3,chan_map(1))-ang_rad(qc_1)* &
                 (cbias(ifov,qc_1 )-cbias(15,qc_1 ))
              ch2    = allchan(3,chan_map(2))-ang_rad(qc_2 )* &
                 (cbias(ifov,qc_2)-cbias(15,qc_2 ))
              ch3    = allchan(3,chan_map(3))-ang_rad(qc_3 )* &
                 (cbias(ifov,qc_3 )-cbias(15,qc_3 ))
              ch15   = allchan(3,chan_map(15))-ang_rad(qc_15)* &
                 (cbias(ifov,qc_15)-cbias(15,qc_15))
           end if
           if (isflg == 0 .and. ch1<285.0_r_kind .and. ch2<285.0_r_kind) then
              cosza = cos(sat_zenang)
              d0  =8.24_r_kind - 2.622_r_kind*cosza + 1.846_r_kind*cosza*cosza
              qval=cosza*(d0+d1*log(285.0_r_kind-ch1)+d2*log(285.0_r_kind-ch2))
              pred=max(zero,qval)*100.0_r_kind
           else
             tt=168._r_kind-0.49_r_kind*ch15
              df2 = 5.10_r_kind +0.78_r_kind*ch1-0.96_r_kind*ch3
              pred=zero
              if(ch1-ch15 >= three)then
                 if(ch1 > 261._r_kind .or. ch1 >= tt .or. &
                   (ch15 <= 273._r_kind .and. df2 >= 0.6_r_kind))then
                    pred=100._r_kind
                 end if
              end if
           endif

        elseif( airs ) then

           chsst_all=zero  ! value weighted according to surface type
           if ( sfcpct(0) > zero ) then
! cloud checks over ocean
              chsst = 8.28206_r_kind - 0.97957_r_kind * allchan(3,chan_map(791)) + 0.60529_r_kind * &  ! AIRS science team
                 allchan(3,chan_map(914)) + 1.74444_r_kind * allchan(3,chan_map(1285)) &            ! SST calculation for
                 - .40379_r_kind * allchan(3,chan_map(1301))                                           ! AIRS data
! 917 cm-1 minus 2500 cm-1 cloud test valid at night for land/ocean:
! beyond threshold, negative >> cirrus (ice), positive >> stratus (water)
! 917 cm-1 minus 2664 cm-1 cloud test valid at night for land/ocean:
! beyond threshold, negative >> cirrus ( ice), positive >> stratus (water)
! 2500 cm-1 minus 2664 cm-1 cloud test valid at night for land/ocean:
! sensitivity test li, Jun et al. (2000) JAM
              ch8ch18 = abs(allchan(3,chan_map(787)) - allchan(3,chan_map(2197)) - .10_r_kind)
              ch8ch19 = abs(allchan(3,chan_map(787)) - allchan(3,chan_map(2377)) + .39_r_kind)
              ch18ch19 = abs(allchan(3,chan_map(2197)) - allchan(3,chan_map(2377)) + .49_r_kind)
              if (sol_zenang > 89.0_r_kind .and. ch8ch18 < .75_r_kind .and. ch8ch19 < .55_r_kind .and. &
                 ch18ch19 < .50_r_kind .and. (chsst-tsavg) > -6.0_r_kind) then
                 chsst = tsavg
              endif
              chsst_all=chsst_all + chsst*sfcpct(0)
           endif ! water
           if ( sfcpct(1) > zero ) then
! cloud checks over land
              chsst = allchan(3,chan_map(587))
              ch8ch18 = abs(allchan(3,chan_map(787)) - allchan(3,chan_map(2197)) - .39_r_kind)
              ch8ch19 = abs(allchan(3,chan_map(787)) - allchan(3,chan_map(2377)) + .13_r_kind)
              ch18ch19 = abs(allchan(3,chan_map(2197)) - allchan(3,chan_map(2377)) + .52_r_kind)
              if (sol_zenang > 89.0_r_kind .and. ch8ch18 < .75_r_kind .and. ch8ch19 < .70_r_kind .and. &
                 ch18ch19 < .55_r_kind .and. (chsst-tsavg) > -10.0_r_kind) then
                 chsst = tsavg
              endif
              chsst_all=chsst_all+ sfcpct(1)*chsst
           endif  ! bare land
           if ( sfcpct(2) > zero .or. sfcpct(3) > zero ) then

! cloud checks over snow and ice
! 801 cm-1 minus 1103 cm-1 test:
! less than -0.05 >> ice cloud; greater than 1.0 >> water cloud
! 965 cm-1 minus 1103 cm-1 test:
! greater than 1.0 >> water cloud
! these tests should not be solar zenigh angle dependent.
! Holz and Ackerman 2006 AMS Sat Conf.

              chsst = allchan(3,chan_map(870))
              ch8ch18 = allchan(3,chan_map(475)) - allchan(3,chan_map(1199))
              ch8ch19 = allchan(3,chan_map(914)) - allchan(3,chan_map(1199))
              if (ch8ch18 > -.05_r_kind .and. ch8ch18 < one .and. &
                  ch8ch19 > -.05_r_kind .and. ch8ch19 < one .and. &
                 chsst < 263.0_r_kind) then
                 chsst = tsavg
              endif
              if ( allchan(3,chan_map(300)) > allchan(3,chan_map(299)) .and. &
                   allchan(3,chan_map(355)) > allchan(3,chan_map(338)) .and. &
                   allchan(3,chan_map(1565)) > allchan(3,chan_map(1545)) .and. &
                   allchan(3,chan_map(1708)) > allchan(3,chan_map(1717))) then
                 tmpinv = allchan(3,chan_map(198))
                 l = chan_map(201)
                 do k = l, chan_map(787)
                    if ( allchan(3,k) > tmpinv ) then
                       tmpinv = allchan(3,k)
                       l = k
                    endif
                 end do
                 if ( tmpinv > allchan(3,chan_map(787)) + five) then
                    chsst = tsavg
                 endif
              endif
              chsst_all = chsst_all + (sfcpct(2)+sfcpct(3))*chsst
           endif  ! snow or sea ice
           chsstf = tsavg-chsst_all
           chsstf = max(zero,chsstf)
           pred = 15._r_kind*chsstf

           if(ifov <= 45)sat_zenang = -sat_zenang

        end if


!       Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
        crit1 = crit1+pred 
        if (pred == zero) then
          call checkob(dist1,crit1,itx,iuse)
        else
          call checkob(one,crit1,itx,iuse)
        endif
        if(.not. iuse)cycle read_loop

!       check for missing channels (if key channel reject)
        iskip = 0
        skip_loop: do l=1, satinfo_nchan
           if ( bufr_index(l) == 0 ) cycle skip_loop
           lluse = iuse_rad(ioff+l) >= 0
           if( lluse .and. (allchan(3,bufr_index(l)) < tbmin .or. allchan(3,bufr_index(l)) > tbmax) ) then
              iskip = iskip + 1
              if(airs) then
                 if( bufr_index(l) == chan_map(914) ) cycle read_loop
              else if(amsua)then
                 if (bufr_index(l) == 1 .or. bufr_index(l) == 2 .or. bufr_index(l) == 3 .or. &
                     bufr_index(l) == 4 .or. bufr_index(l) == 6 .or. bufr_index(l) == 15 ) cycle read_loop
              else
                 if(bufr_index(l) == 1 .or. bufr_index(l) == 2)cycle read_loop
              end if
           endif
        end do skip_loop

        if( iskip >= satinfo_nchan )cycle read_loop

!       Map obs to grids
        if (chsst > tsavg) then
           call finalcheck(dist1,crit1,itx,iuse)
        else
           call finalcheck(one,crit1,itx,iuse)
        endif
        if(.not. iuse) cycle read_loop

!       Replace popped AIRS channel Tb with zero
        if (airs) then
           do l=1, bufr_end
              if ( allchan(2,l) /= zero ) allchan(3,l) = zero
           end do
        endif

        sol_aziang = aquaspot(2)
        lza = (start + real(ifov-1,r_kind)*step)*deg2rad
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

        data_all(1,itx) = 49                        ! satellite ID (temp. 49)
        data_all(2,itx) = t4dv                      ! time diff (obs-anal) (hrs)
        data_all(3,itx) = dlon                      ! grid relative longitude
        data_all(4,itx) = dlat                      ! grid relative latitude
        data_all(5,itx) = sat_zenang                ! satellite zenith angle (rad)
        data_all(6,itx) = sat_aziang                ! satellite azimuth angle (deg)
        data_all(7,itx) = lza                       ! look angle (rad)
        data_all(8,itx) = ifov                      ! fov number
        data_all(9,itx) = sol_zenang                ! solar zenith angle (deg)
        data_all(10,itx)= sol_aziang                ! solar azimuth angle (deg)
        data_all(11,itx) = sfcpct(0)                ! sea percentage of
        data_all(12,itx) = sfcpct(1)                ! land percentage
        data_all(13,itx) = sfcpct(2)                ! sea ice percentage
        data_all(14,itx) = sfcpct(3)                ! snow percentage
        data_all(15,itx)= ts(0)                     ! ocean temperature at zob
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
        data_all(30,itx)= dlon_earth_deg            ! earth relative longitude (degrees)
        data_all(31,itx)= dlat_earth_deg            ! earth relative latitude (degrees)

        if(dval_use) then
           data_all(32,itx)= val_airs
           data_all(33,itx)= itt
        end if

        if ( nst_gsi > 0 ) then
           data_all(maxinfo+1,itx) = tref            ! foundation temperature
           data_all(maxinfo+2,itx) = dtw             ! dt_warm at zob
           data_all(maxinfo+3,itx) = dtc             ! dt_cool at zob
           data_all(maxinfo+4,itx) = tz_tr           ! d(Tz)/d(Tr)
        endif

!       Put satinfo defined channel temperature into data array
        do i=1, satinfo_nchan
           if ( bufr_index(i) /= 0 ) then
              data_all(i+nreal,itx) = allchan(3,bufr_index(i))   ! brightness temerature
           else
              data_all(i+nreal,itx) = tbmin
           endif
        end do
        nrec(itx)=irec

     enddo read_loop

  enddo read_subset
  deallocate(allchan, chan_map, bufr_chan_test)
  call closbf(lnbufr)  ! Close bufr file
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
        do i=1, satinfo_nchan
           if(data_all(i+nreal,n) > tbmin .and. &
              data_all(i+nreal,n) < tbmax)nodata=nodata+1
        end do
     end do

     if(dval_use .and. assim)then
        do n=1,ndata
           itt=nint(data_all(33,n))
           super_val(itt)=super_val(itt)+val_airs
        end do
     end if

!    Write final set of "best" observations to output file
     call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
     write(lunout) obstype,sis,nreal,satinfo_nchan,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif


  deallocate(data_all,nrec) ! Deallocate data arrays
  deallocate(bufr_index)
  call destroygrids    ! Deallocate satthin arrays

! deallocate arrays and nullify pointers.
  if (isfcalc == 1) call fov_cleanup

  if(diagnostic_reg .and. ntest > 0 .and. mype_sub==mype_root) &
     write(6,*)'READ_AIRS:  mype,ntest,disterrmax=',&
     mype,ntest,disterrmax

  return
end subroutine read_airs
