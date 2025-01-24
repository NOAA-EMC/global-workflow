subroutine read_iasi(mype,val_iasi,ithin,isfcalc,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs, &
     nrec_start,nrec_start_ears,nrec_start_db,dval_use)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_iasi                  read bufr format iasi data
! prgmmr :   tahara          org: np20                date: 2002-12-03
!
! abstract:  This routine reads BUFR format radiance 
!            files.  Optionally, the data are thinned to 
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
!   2006-05-19  eliu    - add logic to reset relative weight when all channels not used
!   2006-07-28  derber - modify reads so ufbseq not necessary
!                      - add solar and satellite azimuth angles remove isflg from output
!   2006-08-25  treadon - replace serial bufr i/o with parallel bufr i/o (mpi_io)
!   2008-11-28  todling - measure time from beginning of assimilation window
!   2009-04-18  woollen - improve mpi_io interface with bufrlib routines
!   2009-04-21  derber  - add ithin to call to makegrids
!   2009-09-01  li      - add to handle nst fields
!   2009-12-28  gayno - add option to calculate surface characteristics using
!                       method that accounts for the size/shape of the fov.
!   2010-02-25  collard - changes to call to crtm_init for CRTM v2.0
!   2010-09-02  zhu     - add use_edges option
!   2010-10-12  zhu     - use radstep and radstart from radinfo
!   2011-04-08  li      - (1) use nst_gsi, nstinfo, and add NSST vars 
!                         (2) get zob, tz_tr (call skindepth and cal_tztr)
!                         (3) interpolate NSST Variables to Obs. location (call deter_nst)
!                         (4) add more elements (nstinfo) in data array
!   2011-07-04  todling  - fixes to run either single or double precision
!   2011-08-01  lueken  - add module use deter_sfc_mod, fixed indentation
!   2011-09-13  gayno   - improve error handling for FOV-based sfc calculation
!                         (isfcalc=1)
!   2011-12-13  collard - Replace find_edges code to speed up execution.
!   2012-03-05  akella  - nst now controlled via coupler
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2013-02-26  collard - fix satid issues for MetOp-B and MetOp-C
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2015-10-22  Jung    - added logic to allow subset changes based on the satinfo file
!   2016-04-28  jung - added logic for RARS and direct broadcast from NESDIS/UW
!   2018-05-21  j.jin   - added time-thinning. Moved the checking of thin4d into satthin.F90.
!   2022-04-29  Jung/Collard - allow thinning based on clear sky if AVHRR is missing
!
!   input argument list:
!     mype     - mpi task id
!     val_iasi - weighting factor applied to super obs
!     ithin    - flag to thin data
!     isfcalc  - when set to one, calculate surface characteristics using
!                method that accounts for the size/shape of the fov. 
!                when not one, calculate surface characteristics using
!                bilinear interpolation.
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite id
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
!     nrec_start - first subset with useful information
!     nrec_start_ears - first ears subset with useful information
!     nrec_start_db - first db subset with useful information
!
!   output argument list:
!     nread    - number of BUFR IASI observations read
!     ndata    - number of BUFR IASI profiles retained for further processing
!     nodata   - number of BUFR IASI observations retained for further processing
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
  use satthin, only: radthin_time_info,tdiff2crit
  use obsmod,  only: time_window_max
  use radinfo, only:iuse_rad,nuchan,nusis,jpch_rad,crtm_coeffs_path,use_edges, &
      radedge1,radedge2,radstart,radstep
  use crtm_module, only: success, &
      crtm_kind => fp
  use crtm_planck_functions, only: crtm_planck_temperature
  use crtm_spccoeff, only: sc,crtm_spccoeff_load,crtm_spccoeff_destroy
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,&
      tll2xy,txy2ll,rlats,rlons
  use constants, only: zero,deg2rad,rad2deg,r60inv,one,ten,r100
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use calc_fov_crosstrk, only: instrument_init, fov_check, fov_cleanup
  use deter_sfc_mod, only: deter_sfc,deter_sfc_fov
  use obsmod, only: bmiss
  use gsi_nstcouplermod, only:nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter
  use mpimod, only: npe
  use gsi_io, only: verbose
  use qcmod,  only: iasi_cads
! use radiance_mod, only: rad_obs_type

  implicit none

! BUFR format for IASISPOT 
! Input variables
  integer(i_kind)  ,intent(in   ) :: mype,nrec_start,nrec_start_ears,nrec_start_db
  integer(i_kind)  ,intent(in   ) :: ithin
  integer(i_kind)  ,intent(inout) :: isfcalc
  integer(i_kind)  ,intent(in   ) :: lunout
  integer(i_kind)  ,intent(in   ) :: mype_root
  integer(i_kind)  ,intent(in   ) :: mype_sub
  integer(i_kind)  ,intent(in   ) :: npe_sub
  integer(i_kind)  ,intent(in   ) :: mpi_comm_sub  
  character(len=*) ,intent(in   ) :: infile
  character(len=*) ,intent(in   ) :: jsatid
  character(len=*) ,intent(in   ) :: obstype
  character(len=20),intent(in   ) :: sis
  real(r_kind)     ,intent(in   ) :: twind
  real(r_kind)     ,intent(inout) :: val_iasi
  real(r_kind)     ,intent(in   ) :: gstime
  real(r_kind)     ,intent(in   ) :: rmesh
  logical          ,intent(in   ) :: dval_use

! Output variables
  integer(i_kind)  ,intent(inout) :: nread
  integer(i_kind),dimension(npe)  ,intent(inout) :: nobs
  integer(i_kind)  ,intent(  out) :: ndata,nodata
  

! BUFR file sequencial number
!  character(len=512)  :: table_file
  integer(i_kind)     :: lnbufr = 10

! Variables for BUFR IO    
  real(r_double) :: crchn_reps
  real(r_double),dimension(5)  :: linele
  real(r_double),dimension(13) :: allspot
  real(r_double),allocatable,dimension(:,:) :: allchan
  real(r_double),dimension(3,10):: cscale 
  real(r_double),dimension(7):: cloud_frac
  integer(i_kind) :: bufr_size
  
  real(r_kind)      :: step, start,step_adjust
  character(len=8)  :: subset
  character(len=4)  :: senname
  character(len=80) :: allspotlist
  character(len=40) :: infile2
  integer(i_kind)   :: iret,ireadsb,ireadmg,irec,next, nrec_startx
  integer(i_kind),allocatable,dimension(:) :: nrec


! Work variables for time
  integer(i_kind)   :: idate
  integer(i_kind)   :: idate5(5)
  real(r_kind)      :: sstime, tdiff, t4dv
  integer(i_kind)   :: nmind


! Other work variables
  real(r_kind)     :: piece
  real(r_kind)     :: rsat, dlon, dlat
  real(r_kind)     :: dlon_earth,dlat_earth,dlon_earth_deg,dlat_earth_deg
  real(r_kind)     :: lza, lzaest,sat_height_ratio
  real(r_kind)     :: pred, crit1, dist1
  real(r_kind)     :: sat_zenang
  real(crtm_kind)  :: radiance
  real(r_kind)     :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr
  real(r_kind)     :: zob,tref,dtw,dtc,tz_tr
  real(r_kind),dimension(0:4) :: rlndsea
  real(r_kind),dimension(0:3) :: sfcpct
  real(r_kind),dimension(0:3) :: ts
  real(r_kind),dimension(10) :: sscale
  real(crtm_kind),allocatable,dimension(:) :: temperature
  real(r_kind),allocatable,dimension(:) :: scalef
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind) cdist,disterr,disterrmax,dlon00,dlat00

  logical          :: outside,iuse,assim,valid
  logical          :: iasi,quiet,cloud_info

  integer(i_kind)  :: ifov, instr, iscn, ioff, sensorindex_iasi
  integer(i_kind)  :: i, j, l, iskip, ifovn, bad_line, ksatid, kidsat, llll
  integer(i_kind)  :: nreal, isflg
  integer(i_kind)  :: itx, k, nele, itt, n
  integer(i_kind):: iexponent,maxinfo, bufr_nchan, dval_info
  integer(i_kind):: idomsfc(1)
  integer(i_kind):: ntest
  integer(i_kind):: error_status, irecx,ierr
  integer(i_kind):: radedge_min, radedge_max
  integer(i_kind)   :: subset_start, subset_end, satinfo_nchan, sc_chan, bufr_chan
  integer(i_kind)   :: sfc_channel_index
  integer(i_kind),allocatable, dimension(:) :: channel_number, sc_index, bufr_index
  integer(i_kind),allocatable, dimension(:) :: bufr_chan_test
  character(len=20),allocatable, dimension(:):: sensorlist

! Imager clouser information for CADS
  integer(i_kind)              :: sensorindex_imager, cads_info
  integer(i_kind),dimension(7) :: imager_cluster_index
  logical                      :: imager_coeff
  logical,dimension(7)         :: imager_cluster_flag
  character(len=80)            :: spc_filename
  real(r_kind),dimension(33,7) :: imager_info
  real(r_kind),dimension(7)    :: imager_cluster_size
  real(r_kind),dimension(2)    :: imager_mean, imager_std_dev

! Set standard parameters
  character(8),parameter:: fov_flag="crosstrk"
  integer(i_kind),parameter:: sfc_channel=1271
  integer(i_kind),parameter:: ichan=-999  ! fov-based surface code is not channel specific for iasi 
  real(r_kind),parameter:: expansion=one         ! exansion factor for fov-based surface code.
                                                 ! use one for ir sensors.
  real(r_kind),parameter:: R90    =  90._r_kind
  real(r_kind),parameter:: R360   = 360._r_kind
  real(r_kind),parameter:: tbmin  = 50._r_kind
  real(r_kind),parameter:: tbmax  = 550._r_kind
  real(r_kind),parameter:: earth_radius = 6371000._r_kind
  integer(i_kind),parameter :: ilon = 3
  integer(i_kind),parameter :: ilat = 4
  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin,it_mesh,jstart
  logical print_verbose

  print_verbose=.false.
  if(verbose)print_verbose=.true.

! Initialize variables
  maxinfo    =  31
  disterrmax=zero
  ntest=0
  dval_info = 0
  if(dval_use) dval_info = 2
  cads_info = 0
  if(iasi_cads) cads_info = 23
  nreal  = maxinfo + cads_info + dval_info + nstinfo

  ndata = 0
  nodata = 0
  iasi=      obstype == 'iasi'

  bad_line=-1

  if (nst_gsi > 0 ) then
    call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif

  if(jsatid == 'metop-a')kidsat=4
  if(jsatid == 'metop-b')kidsat=3
  if(jsatid == 'metop-c')kidsat=5
 
!  write(6,*)'READ_IASI: mype, mype_root,mype_sub, npe_sub,mpi_comm_sub', &
!          mype, mype_root,mype_sub,mpi_comm_sub

  radedge_min = 0
  radedge_max = 1000

! Find the iasi offset in the jpch_rad list.  This is for the iuse flag
! and count the number of cahnnels in the satinfo file 
  ioff=jpch_rad
  subset_start = 0
  subset_end = 0
  assim = .false.
  do i=1,jpch_rad
     if (trim(nusis(i))==trim(sis)) then
        ioff = min(ioff,i)   ! iasi offset
        if (subset_start == 0) then
          step  = radstep(i)
          start = radstart(i)
          if (radedge1(i)/=-1 .and. radedge2(i)/=-1) then
             radedge_min=radedge1(i)
             radedge_max=radedge2(i)
          end if
          subset_start = i
        endif
        if (iuse_rad(i) > 0) assim = .true.  ! Are any of the IASI channels being used?
        subset_end = i 
     endif
  end do 
  satinfo_nchan = subset_end - subset_start + 1
  allocate(channel_number(satinfo_nchan))
  allocate(sc_index(satinfo_nchan))
  allocate(bufr_index(satinfo_nchan)) 
  ioff = ioff - 1 

  step_adjust = 0.625_r_kind
! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.
  if (.not.assim) val_iasi=zero

  if (mype_sub==mype_root)write(6,*)'READ_IASI:  iasi offset ',ioff

  senname = 'IASI'
  
  allspotlist= &
   'SAID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA BEARAZ SOZA SOLAZI'

! load spectral coefficient structure  
  quiet=.not. verbose

  imager_coeff = .false.
  spc_filename =trim(crtm_coeffs_path)//'avhrr3_'//trim(jsatid)//'.SpcCoeff.bin'
  inquire(file=trim(spc_filename), exist=imager_coeff)
  if ( imager_coeff ) then
    allocate( sensorlist(2))
    sensorlist(1) = sis
    sensorlist(2) = 'avhrr3_'//trim(jsatid)
  else
    allocate( sensorlist(1))
    sensorlist(1) = sis
  endif

  if( crtm_coeffs_path /= "" ) then
     if(mype_sub==mype_root .and. print_verbose) write(6,*)'READ_IASI: crtm_spccoeff_load() on path "'//trim(crtm_coeffs_path)//'"'
     error_status = crtm_spccoeff_load(sensorlist,&
        File_Path = crtm_coeffs_path,quiet=quiet )
  else
     error_status = crtm_spccoeff_load(sensorlist,quiet=quiet)
  endif

  if (error_status /= success) then
     write(6,*)'READ_IASI:  ***ERROR*** crtm_spccoeff_load error_status=',error_status,&
        '   TERMINATE PROGRAM EXECUTION'
     call stop2(71)
  endif

!  find IASI sensorindex
  sensorindex_iasi = 0
  if ( sc(1)%sensor_id(1:4) == 'iasi' ) then
     sensorindex_iasi = 1
  else
     write(6,*)'READ_IASI: ***ERROR*** sensorindex_iasi not set  NO IASI DATA USED'
     write(6,*)'READ_IASI: We are looking for ', sc(1)%sensor_id, '   TERMINATE PROGRAM EXECUTION'
     call stop2(71)
  end if

!  find imager sensorindex
  sensorindex_imager = 0
  if ( iasi_cads .and. imager_coeff ) then
     if ( sc(2)%sensor_id(1:4) == 'avhr' ) then
        sensorindex_imager = 2
        imager_coeff = .true.
     else
        write(6,*)'READ_IASI: ***ERROR*** sensorindex_imager is not set  NO IASI DATA USED'
        write(6,*)'READ_IASI: We are looking for ', sc(2)%sensor_id
        imager_coeff = .false.
     end if
  else
     imager_coeff = .false.
  end if

! Find the channels being used (from satinfo file) in the spectral coef. structure.
  do i=subset_start,subset_end
     channel_number(i -subset_start +1) = nuchan(i)
  end do
  sc_index(:) = 0
  satinfo_chan: do i=1,satinfo_nchan
     spec_coef: do l=1,sc(1)%n_channels
        if ( channel_number(i) == sc(sensorindex_iasi)%sensor_channel(l) ) then
           sc_index(i) = l
           exit spec_coef
        endif
     end do spec_coef
  end do  satinfo_chan

! Calculate parameters needed for FOV-based surface calculation.
  if (isfcalc==1)then
     instr=18
     call instrument_init(instr, jsatid, expansion, valid)
     if (.not. valid) then
        if (assim) then 
           write(6,*)'READ_IASI:  ***ERROR*** IN SETUP OF FOV-SFC CODE. STOP'
           call stop2(71)
        else
           call fov_cleanup
           isfcalc = 0
           write(6,*)'READ_IASI:  ***ERROR*** IN SETUP OF FOV-SFC CODE'
        endif
     endif
  endif

  if (isfcalc==1)then
     rlndsea = zero
  else
     rlndsea(0) = zero                       
     rlndsea(1) = 10._r_kind
     rlndsea(2) = 15._r_kind
     rlndsea(3) = 10._r_kind
     rlndsea(4) = 30._r_kind
  endif

  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif
! Make thinning grids
  call makegrids(rmesh,ithin,n_tbin=n_tbin)

! Allocate arrays to hold data
! The number of channels in obtained from the satinfo file being used.
  nele=nreal+satinfo_nchan
  allocate(data_all(nele,itxmax),nrec(itxmax))
  allocate(temperature(1))   ! dependent on # of channels in the bufr file
  allocate(allchan(2,1))     ! actual values set after ireadsb
  allocate(bufr_chan_test(1))! actual values set after ireadsb
  allocate(scalef(1))

! Big loop to read data file
  next=0
  irec=0
  nrec=999999
! Big loop over standard data feed and possible rars/db data
! llll=1 is normal feed, llll=2 RARS/EARS data, llll=3 DB/UW data)
  ears_db_loop: do llll= 1, 3

     if(llll == 1)then
        nrec_startx=nrec_start
        infile2=trim(infile)         ! Set bufr subset names based on type of data to read
     elseif(llll == 2) then
        nrec_startx=nrec_start_ears
        infile2=trim(infile)//'ears' ! Set bufr subset names based on type of data to read
     elseif(llll == 3) then
        nrec_startx=nrec_start_db
        infile2=trim(infile)//'_db'  ! Set bufr subset names based on type of data to read
     end if

!    Open BUFR file
     open(lnbufr,file=trim(infile2),form='unformatted',status='old',iostat=ierr)

     if(ierr /= 0) cycle ears_db_loop
!    Open BUFR table
     call openbf(lnbufr,'IN',lnbufr)
     call datelen(10)

     irecx = 0
     read_subset: do while(ireadmg(lnbufr,subset,idate)>=0)
        irecx = irecx + 1
        if(irecx < nrec_startx) cycle read_subset
        irec = irec + 1
        next=next+1
        if(next == npe_sub)next=0
        if(next /= mype_sub)cycle read_subset

        read_loop: do while (ireadsb(lnbufr)==0)

!          Get the size of the channels and radiance (allchan) array
           call ufbint(lnbufr,crchn_reps,1,1,iret,'(IASICHN)')
           bufr_nchan = int(crchn_reps)

           bufr_size = size(temperature,1)
           if ( bufr_size /= bufr_nchan ) then ! Re-allocation if number of channels has changed
!             Allocate the arrays needed for the channel and radiance array
              deallocate(temperature,allchan,bufr_chan_test,scalef)
              allocate(temperature(bufr_nchan))   ! dependent on # of channels in the bufr file
              allocate(allchan(2,bufr_nchan))
              allocate(bufr_chan_test(bufr_nchan))
              allocate(scalef(bufr_nchan))
              bufr_chan_test(:)=0
           endif       !  allocation if

!          Read IASI FOV information
           call ufbint(lnbufr,linele,5,1,iret,'FOVN SLNM QGFQ SELV SAID')

!          Extract satellite id.  If not the one we want, read next subset
           ksatid=nint(linele(5))
           if(ksatid /= kidsat) cycle read_loop

           if ( linele(3) /= zero) cycle read_loop  ! problem with profile (QGFQ)

!          zenith angle/scan spot mismatch, reject entire line
           if ( bad_line == nint(linele(2))) then
              cycle read_loop
           else
              bad_line = -1
           endif

           ifov = nint(linele(1))               ! field of view

!          IASI fov ranges from 1 to 120.   Current angle dependent bias
!          correction has a maximum of 90 scan positions.   Geometry
!          of IASI scan allows us to remap 1-120 to 1-60.   Variable
!          ifovn below contains the remapped IASI fov.  This value is
!          passed on to and used in setuprad
           ifovn = (ifov-1)/2 + 1

!          Remove data on edges
           if (.not. use_edges .and. &
             (ifovn < radedge_min .OR. ifovn > radedge_max )) cycle read_loop

!          Check field of view (FOVN) and satellite zenith angle (SAZA)
           iscn = nint(linele(2))               ! scan line
           if( ifov <= 0 .or. ifov > 120) then
              write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                 ' STRANGE OBS INFO(FOVN,SLNM):', ifov, iscn
              cycle read_loop
           endif

           call ufbint(lnbufr,allspot,13,1,iret,allspotlist)
           if(iret /= 1) cycle read_loop


!          Check observing position
           dlat_earth = allspot(8)   ! latitude
           dlon_earth = allspot(9)   ! longitude
           if( abs(dlat_earth) > R90  .or. abs(dlon_earth) > R360 .or. &
              (abs(dlat_earth) == R90 .and. dlon_earth /= ZERO) )then
              write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                 ' STRANGE OBS POINT (LAT,LON):', dlat_earth, dlon_earth
              cycle read_loop
           endif

!          Retrieve observing position
           if(dlon_earth >= R360)then
              dlon_earth = dlon_earth - R360
           else if(dlon_earth < ZERO)then
              dlon_earth = dlon_earth + R360
           endif

           dlat_earth_deg = dlat_earth
           dlon_earth_deg = dlon_earth
           dlat_earth = dlat_earth * deg2rad
           dlon_earth = dlon_earth * deg2rad

!          If regional, map obs lat,lon to rotated grid.
           if(regional)then

!             Convert to rotated coordinate.  dlon centered on 180 (pi),
!             so always positive for limited area
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

!             Check to see if in domain.  outside=.true. if dlon_earth,
!             dlat_earth outside domain, =.false. if inside
              if(outside) cycle read_loop

!          Global case 
           else
              dlat = dlat_earth
              dlon = dlon_earth
              call grdcrd1(dlat,rlats,nlat,1)
              call grdcrd1(dlon,rlons,nlon,1)
           endif

!          Check obs time
           idate5(1) = nint(allspot(2)) ! year
           idate5(2) = nint(allspot(3)) ! month
           idate5(3) = nint(allspot(4)) ! day
           idate5(4) = nint(allspot(5)) ! hour
           idate5(5) = nint(allspot(6)) ! minute

           if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
              idate5(2) < 1    .or. idate5(2) >   12 .or. &
              idate5(3) < 1    .or. idate5(3) >   31 .or. &
              idate5(4) <0     .or. idate5(4) >   24 .or. &
              idate5(5) <0     .or. idate5(5) >   60 )then

              write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                 ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
              cycle read_loop

           endif

!          Retrieve obs time
           call w3fs21(idate5,nmind)
           t4dv = (real(nmind-iwinbgn,r_kind) + real(allspot(7),r_kind)*r60inv)*r60inv ! add in seconds
           sstime = real(nmind,r_kind) + real(allspot(7),r_kind)*r60inv ! add in seconds
           tdiff = (sstime - gstime)*r60inv

           if (l4dvar.or.l4densvar) then
              if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
           else
              if (abs(tdiff)>twind) cycle read_loop
           endif

!          Increment nread counter by satinfo_nchan
           nread = nread + satinfo_nchan

           crit0 = 0.01_r_kind
           if( llll > 1 ) crit0 = crit0 + r100 * real(llll,r_kind)
           timeinflat=6.0_r_kind
           call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)
           call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)

           if(.not. iuse)cycle read_loop

!          Observational info
           sat_zenang  = allspot(10)            ! satellite zenith angle

!          Check  satellite zenith angle (SAZA)
           if(sat_zenang > 90._r_kind ) then
              write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                 ' STRANGE OBS INFO(FOVN,SLNM,SAZA,BEARAZ):', ifov, iscn, allspot(10),allspot(11)
              cycle read_loop
           endif
           if ( ifov <= 60 ) sat_zenang = -sat_zenang

!          Compare IASI satellite scan angle and zenith angle
           piece = -step_adjust
           if ( mod(ifovn,2) == 1) piece = step_adjust
           lza = ((start + real((ifov-1)/4,r_kind)*step) + piece)*deg2rad
           sat_height_ratio = (earth_radius + linele(4))/earth_radius
           lzaest = asin(sat_height_ratio*sin(lza))*rad2deg
           if (abs(sat_zenang - lzaest) > one) then
              write(6,*)' READ_IASI WARNING uncertainty in lza ', &
                 lza*rad2deg,sat_zenang,sis,ifov,start,step,allspot(11),allspot(12),allspot(13)
              bad_line = iscn
              cycle read_loop
           endif


!          "Score" observation.  We use this information to identify "best" obs
!          Locate the observation on the analysis grid.  Get sst and land/sea/ice
!          mask.  
!          isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed 

!          When using FOV-based surface code, must screen out obs with bad fov numbers.
           if (isfcalc == 1) then
              call fov_check(ifov,instr,ichan,valid)
              if (.not. valid) cycle read_loop

!          When isfcalc is set to one, calculate surface fields using size/shape of fov.
!          Otherwise, use bilinear interpolation.

              call deter_sfc_fov(fov_flag,ifov,instr,ichan,real(allspot(11),r_kind),dlat_earth_deg, &
                              dlon_earth_deg,expansion,t4dv,isflg,idomsfc(1), &
                              sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
           else
              call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc(1),sfcpct, &
                 ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
           endif

!          Set common predictor parameters
           crit1 = crit1 + rlndsea(isflg)
 
           call checkob(one,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop

!          Clear Amount  (percent clear)
!          Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
           pred = r100
           cloud_info = .false.
           call ufbrep(lnbufr,cloud_frac,1,7,iret,'FCPH')
           if (iret == 7 .and. cloud_frac(1) <= r100 .and. cloud_frac(1) >= zero) then
              pred = r100 - cloud_frac(1)
              cloud_info = .true.
           endif

           crit1 = crit1 + pred
           call checkob(one,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop

           call ufbseq(lnbufr,cscale,3,10,iret,'IASIL1CB')
           if(iret /= 10) then
              write(6,*) 'READ_IASI  read scale error ',iret
              cycle read_loop
           end if

!          The scaling factors are as follows, cscale(1) is the start channel number,
!                                     cscale(2) is the end channel number,
!                                     cscale(3) is the exponent scaling factor
!          In our case (616 channels) there are 10 groups of cscale (dimension :: cscale(3,10))
!          The units are W/m2..... you need to convert to mW/m2.... (subtract 5 from cscale(3)
           do i=1,10  ! convert exponent scale factor to int and change units
              if(cscale(3,i) < bmiss) then
                iexponent = -(nint(cscale(3,i)) - 5)
                sscale(i)=ten**iexponent
              else 
                sscale(i)=0.0_r_kind
              endif
           end do

!          Read IASI channel number(CHNM) and radiance (SCRA)
           call ufbseq(lnbufr,allchan,2,bufr_nchan,iret,'IASICHN')
           jstart=1
           scalef=one
           do i=1,bufr_nchan
               scaleloop: do j=jstart,10
                  if(allchan(1,i) >= cscale(1,j) .and. allchan(1,i) <= cscale(2,j))then
                     scalef(i) = sscale(j)
                     jstart=j
                     exit scaleloop
                  end if
               end do scaleloop
           end do
         
           if (iret /= bufr_nchan) then
              write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                 iret, ' CH DATA IS READ INSTEAD OF ',bufr_nchan
              cycle read_loop
           endif

!          Coordinate bufr channels with satinfo file channels
!          If this is the first time or a change in the bufr channels is detected, sync with satinfo file
           if (ANY(int(allchan(1,:)) /= bufr_chan_test(:))) then
              sfc_channel_index = 0
              bufr_index(:) = 0
              bufr_chans: do l=1,bufr_nchan
                 bufr_chan_test(l) = int(allchan(1,l))                      ! Copy this bufr channel selection into array for comparison to next profile
                 satinfo_chans: do i=1,satinfo_nchan                        ! Loop through sensor (iasi) channels in the satinfo file
                    if ( channel_number(i) == int(allchan(1,l)) ) then      ! Channel found in both bufr and satinfo file
                       bufr_index(i) = l
                       if ( channel_number(i) == sfc_channel) sfc_channel_index = l
                       exit satinfo_chans                                   ! go to next bufr channel
                    endif
                 end do  satinfo_chans
              end do bufr_chans
           endif

           if (sfc_channel_index == 0) then
             write(6,*)'READ_IASI: ***ERROR*** SURFACE CHANNEL USED FOR QC WAS NOT FOUND'
             cycle read_loop
           endif

!$omp parallel do schedule(dynamic,1) private(i,sc_chan,bufr_chan,radiance)
           channel_loop: do i=1,satinfo_nchan
              sc_chan = sc_index(i)
              if ( bufr_index(i) == 0 ) cycle channel_loop
              bufr_chan = bufr_index(i)
!             check that channel number is within reason
              if (( allchan(2,bufr_chan) > zero .and. allchan(2,bufr_chan) < 99999._r_kind)) then  ! radiance bounds
                radiance = allchan(2,bufr_chan)*scalef(bufr_chan)
                call crtm_planck_temperature(sensorindex_iasi,sc_chan,radiance,temperature(bufr_chan))
              else
                 temperature(bufr_chan) = tbmin
              endif
           end do channel_loop

!          Check for reasonable temperature values
           iskip = 0
           skip_loop: do i=1,satinfo_nchan
              if ( bufr_index(i) == 0 ) cycle skip_loop
              bufr_chan = bufr_index(i)
              if(temperature(bufr_chan) <= tbmin .or. temperature(bufr_chan) > tbmax ) then
                 temperature(bufr_chan) = min(tbmax,max(tbmin,temperature(bufr_chan)))
                 if(iuse_rad(ioff+i) >= 0)iskip = iskip + 1
              endif
           end do skip_loop

           if(iskip > 0)then
              if(print_verbose)write(6,*) ' READ_IASI : iskip > 0 ',iskip
              cycle read_loop 
           end if

!          crit1=crit1 + ten*real(iskip,r_kind)

!          If the surface channel exists (~960.0 cm-1) and the imager cloud information is missing, use an
!          estimate of the surface temperature to determine if the profile may be clear.
           if (.not. cloud_info) then
              pred = tsavg*0.98_r_kind - temperature(sfc_channel_index)
              pred = max(pred,zero)
              crit1=crit1 + pred
           endif

!          Map obs to grids
           if (pred == zero) then
              call finalcheck(dist1,crit1,itx,iuse)
           else
              call finalcheck(one,crit1,itx,iuse)
           endif
           if(.not. iuse)cycle read_loop

!   Read the imager cluster information for the Cloud and Aerosol Detection Software.
!   Only channels 4 and 5 are used.

           if ( iasi_cads ) then
             call ufbseq(lnbufr,imager_info,33,7,iret,'IASIL1CS')
             if (iret == 7 .and. imager_info(3,1) <= 100.0_r_kind .and. &
                  sum(imager_info(3,:)) > zero .and. imager_coeff ) then   ! if imager cluster info exists
               imager_mean = zero
               imager_std_dev = zero
               imager_cluster_flag = .TRUE.
               imager_cluster_size = imager_info(3,1:7)
               imager_cluster_size(:) = imager_cluster_size(:) / sum(imager_cluster_size(:))

!  Order clusters from largest (1) to smallest (7)
               imager_cluster_sort: do i=1,7
                 j = maxloc(imager_cluster_size,dim=1,mask=imager_cluster_flag)
                 imager_cluster_index(i) = j
                 imager_cluster_flag(j) = .FALSE.
               end do imager_cluster_sort

!   Convert from radiance to brightness temperature for mean and standard deviation used by CADS.
!   Imager cluster info added to data_all array

               imager_cluster_info: do j=1,7
                 i = imager_cluster_index(j)

!   If the cluster size, or radiance values of channel 4 or 5 are zero, do not compute statistics for that cluster
                 if ( imager_cluster_size(i) > zero .and. imager_info(26,i) > zero .and. imager_info(31,i) > zero ) then
                   data_all(maxinfo+j,itx) =  imager_cluster_size(i)                   ! Imager cluster fraction

                   iexponent = -(nint(imager_info(25,i))-5 )                           ! channel 4 radiance for each cluster.
                   imager_info(26,i) =  imager_info(26,i) * (ten ** iexponent)

                   iexponent = -(nint(imager_info(27,i))-5 )                           ! channel 4 radiance std dev for each cluster.
                   imager_info(28,i) =  imager_info(28,i) * (ten ** iexponent)

                   iexponent = -(nint(imager_info(30,i))-5 )                           ! channel 5 radiance for each cluster
                   imager_info(31,i) =  imager_info(31,i) * (ten ** iexponent)

                   iexponent = -(nint(imager_info(32,i))-5 )                           ! channel 5 radiance std dev for each cluser.
                   imager_info(33,i) =  imager_info(33,i) * (ten ** iexponent)

                   call crtm_planck_temperature(sensorindex_imager,2,imager_info(26,i),data_all(maxinfo+7+j,itx))
                   data_all(maxinfo+7+j,itx) = max(data_all(maxinfo+7+j,itx),zero)
                   call crtm_planck_temperature(sensorindex_imager,3,imager_info(31,i),data_all(maxinfo+14+j,itx))
                   data_all(maxinfo+14+j,itx) = max(data_all(maxinfo+14+j,itx),zero)
                 else                                                                  ! something is wrong
                   data_all(maxinfo+j,itx) = zero                                      ! set everything to zero
                   data_all(maxinfo+7+j,itx) = zero
                   data_all(maxinfo+14+j,itx) = zero
                 endif

               end do imager_cluster_info

! Compute cluster averages for each channel

               imager_mean(1) = sum(imager_cluster_size(:) * imager_info(26,:))        ! Channel 4 radiance cluster average
               imager_std_dev(1) = sum(imager_cluster_size(:) * (imager_info(26,:)**2 + imager_info(28,:)**2)) - imager_mean(1)**2
               imager_std_dev(1) = sqrt(max(imager_std_dev(1),zero))                   ! Channel 4 radiance RMSE
               if ( imager_mean(1) > zero .and. imager_std_dev(1) > zero ) then
                 call crtm_planck_temperature(sensorindex_imager,2,(imager_std_dev(1) + imager_mean(1)),imager_std_dev(1))
                 call crtm_planck_temperature(sensorindex_imager,2,imager_mean(1),imager_mean(1))    ! Channel 4 average BT
                 imager_std_dev(1) = imager_std_dev(1) - imager_mean(1)                ! Channel 4 BT std dev
                 data_all(maxinfo+22,itx) = imager_std_dev(1)
               else
                 data_all(maxinfo+22,itx) = zero
               endif

               imager_mean(2) = sum(imager_cluster_size(:) * imager_info(31,:))        ! Channel 5 radiance cluster average
               imager_std_dev(2) = sum(imager_cluster_size(:) * (imager_info(31,:)**2 + imager_info(33,:)**2)) - imager_mean(1)**2
               imager_std_dev(2) = sqrt(max(imager_std_dev(1),zero))                   ! Channel 5 radiance RMSE
               if ( imager_mean(2) > zero .and. imager_std_dev(2) > zero ) then 
                 call crtm_planck_temperature(sensorindex_imager,3,(imager_std_dev(2) + imager_mean(2)),imager_std_dev(2))
                 call crtm_planck_temperature(sensorindex_imager,3,imager_mean(2),imager_mean(2))     ! Channel 5 average BT
                 imager_std_dev(2) = imager_std_dev(2) - imager_mean(2)                ! Channel 5 BT std dev
                 data_all(maxinfo+23,itx) = imager_std_dev(2)
               else
                 data_all(maxinfo+23,itx) = zero
               endif

             else  ! Imager cluster information is missing.  Set everything to zero
               data_all(maxinfo+1 : maxinfo+cads_info,itx) = zero
             endif
           endif ! iasi_cads = .true.
!
!          interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
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

           rsat=allspot(1) 
           data_all(1,itx) = rsat                      ! satellite ID 
           data_all(2,itx) = t4dv                      ! time diff (obs-anal) (hrs)
           data_all(3,itx) = dlon                      ! grid relative longitude
           data_all(4,itx) = dlat                      ! grid relative latitude
           data_all(5,itx) = sat_zenang*deg2rad        ! satellite zenith angle (rad)
           data_all(6,itx) = allspot(11)               ! satellite azimuth angle (deg)
           data_all(7,itx) = lza                       ! look angle (rad)
           data_all(8,itx) = ifovn                     ! fov number
           data_all(9,itx) = allspot(12)               ! solar zenith angle (deg)
           data_all(10,itx)= allspot(13)               ! solar azimuth angle (deg)
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
           data_all(30,itx)= dlon_earth_deg            ! earth relative longitude (degrees)
           data_all(31,itx)= dlat_earth_deg            ! earth relative latitude (degrees)

           if(dval_use)then
              data_all(maxinfo+cads_info+1,itx)= val_iasi
              data_all(maxinfo+cads_info+2,itx)= itt
           end if

           if ( nst_gsi > 0 ) then
              data_all(maxinfo+cads_info+dval_info+1,itx) = tref         ! foundation temperature
              data_all(maxinfo+cads_info+dval_info+2,itx) = dtw          ! dt_warm at zob
              data_all(maxinfo+cads_info+dval_info+3,itx) = dtc          ! dt_cool at zob
              data_all(maxinfo+cads_info+dval_info+4,itx) = tz_tr        ! d(Tz)/d(Tr)
           endif

!          Put satinfo defined channel temperatures into data array
           do l=1,satinfo_nchan
              i = bufr_index(l)
              if(bufr_index(l) /= 0)then
                 data_all(l+nreal,itx) = temperature(i)   ! brightness temerature
              else
                 data_all(l+nreal,itx) = tbmin
              end if
           end do
           nrec(itx)=irec

        enddo read_loop

     enddo read_subset

     call closbf(lnbufr)
     close(lnbufr)

  end do ears_db_loop

  deallocate(temperature, allchan, bufr_chan_test,scalef)
  deallocate(channel_number,sc_index)
  deallocate(bufr_index)
! deallocate crtm info
  error_status = crtm_spccoeff_destroy()
  if (error_status /= success) &
    write(6,*)'OBSERVER:  ***ERROR*** crtm_destroy error_status=',error_status

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
        do i=1,satinfo_nchan
           if(data_all(i+nreal,n) > tbmin .and. &
              data_all(i+nreal,n) < tbmax)nodata=nodata+1
        end do
     end do

     if(dval_use .and. assim)then
        do n=1,ndata
          itt=nint(data_all(33,n))
          super_val(itt)=super_val(itt)+val_iasi
        end do
     end if

!    Write final set of "best" observations to output file
     call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
     write(lunout) obstype,sis,nreal,satinfo_nchan,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif


  deallocate(data_all,nrec) ! Deallocate data arrays
  call destroygrids    ! Deallocate satthin arrays

! Deallocate arrays and nullify pointers.
  if(isfcalc == 1) call fov_cleanup

  if(diagnostic_reg .and. ntest > 0 .and. mype_sub==mype_root) &
     write(6,*)'READ_IASI:  mype,ntest,disterrmax=',&
        mype,ntest,disterrmax
  
  return
end subroutine read_iasi
