subroutine read_ssmis(mype,val_ssmis,ithin,isfcalc,rmesh,jsatid,gstime,&
           infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
           mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs, &
           nrec_start,dval_use)

! subprogram:    read_ssmis            read ssmis data
! prgmmr: okamoto          org: np23                date: 2005-01-05
!
! abstract:  This routine reads BUFR format SSM/IS radiance 
!     (brightness temperature) files.  Optionally, the data 
!     are thinned to a specified resolution using simple 
!     quality control checks.
!         1) obs time check  |obs-anal|<time_window;
!         2) remove overlap orbit; 
!         3) climate check  reject for tb<tbmin or tb>tbmax
!
!     When running the gsi in regional mode, the code only
!     retains those observations that fall within the regional
!     domain
!
! program history log:
!   2005-01-05 okamoto 
!    2005-10-07 Xu & Pawlak - modify the code related to ityp determination to
!                     use routine  deter_ityp, added values for constants 
!                     rlndsea for four ssmis instruments, fixed indentation
!   2005-10-10 treadon - replace deter_ityp with deter_sfc, modify rlndsea to be
!                        consistent with other read_* routines
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2005-12-15  treadon - patch to constrain ssmi_img scan positions to be in 1-90 range
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-04-27  derber  - some efficiency modifications
!   2006-05-19  eliu    - add logic to reset relative weight when all channels not used
!   2006-07-28  derber  - add solar and satellite azimuth angles remove isflg from output
!   2006-08-25  treadon - replace serial bufr i/o with parallel bufr i/o (mpi_io)
!   2007-01-24  kazumori- modify to read UKMO preprocessed SSMIS data
!   2008-04-08  Yan     - fix bug in calculation of ifov for UPP SSMIS data
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2008-05-27  safford - rm unused vars and uses
!   2009-01-09  gayno   - new option to calculate surface fields within FOV
!                         (when isfcalc flag set to one)
!   2009-04-18  woollen - improve mpi_io interface with bufrlib routines
!   2009-04-21  derber  - add ithin to call to makegrids
!   2011-04-08  li      - (1) use nst_gsi, nstinfo, fac_dtl, fac_tsl and add NSST vars
!                         (2) get zob, tz_tr (call skindepth and cal_tztr)
!                         (3) interpolate NSST Variables to Obs. location (call deter_nst)
!                         (4) add more elements (nstinfo) in data array
!   2011-08-01  lueken  - added module use deter_sfc_mod
!   2011-09-02  gayno - add processing of future satellites for FOV-based
!                       surface field calculation and improved its error handling
!                       (isfcalc=1)
!   2011-12-10  eliu  - add handling and call to zensun to calculate solar zenith/azimuth angles;
!                       fix UTC hour used by zensun
!   2012-01-10  eliu  - add handling to do spatial averaging (noise reduction) for 
!                       observed brightness temperatures 
!   2012-03-05  akella  - nst now controlled via coupler
!   2012-07-10  sienkiewicz  add control for choosing noise reduction method  0=no smoothing
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2013-01-26  parrish - WCOSS debug compile error--change mype from intent(inout) to intent(in)
!   2014-12-03  derber remove unused variables
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2018-05-21  j.jin   - added time-thinning. Moved the checking of thin4d into satthin.F90.
!
! input argument list:
!     mype     - mpi task id
!     val_ssmis- weighting factor applied to super obs
!     ithin    - flag to thin data
!     isfcalc  - flag to specify method to calculate sfc fields within FOV
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read  ex.15
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
!     nread    - number of BUFR MI 1b observations read
!     ndata    - number of BUFR MI 1b profiles retained for further processing
!     nodata   - number of BUFR MI 1b observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!$$$ end documentation block

  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
      checkob,finalcheck,score_crit
  use satthin, only: radthin_time_info,tdiff2crit
  use obsmod,  only: time_window_max
  use radinfo, only: ssmis_method
  use radinfo, only: iuse_rad,newchn,nusis,jpch_rad,&   
      use_edges,radedge1,radedge2       
  use gridmod, only: diagnostic_reg,regional,rlats,rlons,nlat,nlon,&
      tll2xy,txy2ll
  use constants, only: deg2rad,rad2deg,zero,half,one,two,four,r60inv
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use calc_fov_conical, only: instrument_init
  use deter_sfc_mod, only: deter_sfc,deter_sfc_fov
  use gsi_nstcouplermod, only: nst_gsi,nstinfo 
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter
  use ssmis_spatial_average_mod, only : ssmis_spatial_average 
  use m_sortind
  use mpimod, only: npe
! use radiance_mod, only: rad_obs_type
 
  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: infile,obstype,jsatid
  character(len=20),intent(in  ) :: sis
  real(r_kind)    ,intent(in   ) :: rmesh,gstime,twind
  real(r_kind)    ,intent(inout) :: val_ssmis
  integer(i_kind) ,intent(in   ) :: mype,nrec_start
  integer(i_kind) ,intent(inout) :: lunout,ithin,isfcalc
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  integer(i_kind) ,intent(inout) :: nread
  integer(i_kind) ,intent(inout) :: ndata,nodata
  integer(i_kind) ,intent(in   ) :: mype_root
  integer(i_kind) ,intent(in   ) :: mype_sub
  integer(i_kind) ,intent(in   ) :: npe_sub
  integer(i_kind) ,intent(in   ) :: mpi_comm_sub
  logical         ,intent(in   ) :: dval_use

! Declare local variables
  character(7),parameter    :: fov_flag="conical"
  integer(i_kind),parameter :: maxchanl  =  24
  integer(i_kind),parameter :: mxscen_img = 180   !img
  integer(i_kind),parameter :: mxscen_env = 90    !env
  integer(i_kind),parameter :: mxscen_las = 60    !las
  integer(i_kind),parameter :: mxscen_uas = 30    !uas
  integer(i_kind),parameter :: maxobs = 800000
  real(r_kind),parameter    :: r360=360.0_r_kind
  real(r_kind),parameter    :: tbmin=70.0_r_kind
  real(r_kind),parameter    :: tbmax=320.0_r_kind
  real(r_kind),parameter    :: one_minute=0.01666667_r_kind

  logical :: do_noise_reduction
  logical :: ssmis_las,ssmis_uas,ssmis_img,ssmis_env,ssmis
  logical :: outside,iuse,assim,valid

  character(len=8):: subset

  integer(i_kind) :: i,k,ifovoff,ntest
  integer(i_kind) :: nlv,idate,nchanl,nreal
  integer(i_kind) :: n,ireadsb,ireadmg,irec
  integer(i_kind) :: nmind,itx,nele,itt
  integer(i_kind) :: iskip
  integer(i_kind) :: lnbufr,isflg,idomsfc(1)
  integer(i_kind) :: ilat,ilon
  integer(i_kind) :: nscan,jc,bufsat,incangl,said
  integer(i_kind) :: nfov_bad
  integer(i_kind) :: ichan, instr
  integer(i_kind) :: radedge_min, radedge_max  
  integer(i_kind) :: iobs,num_obs,method,iret
  integer(i_kind) :: irain
  integer(i_kind) :: doy,mon,m
  integer(i_kind) :: ibfms,maxinfo

! integer(i_kind),pointer :: ifov,iscan,iorbn,inode
  integer(i_kind),pointer :: ifov,inode

  integer(i_kind),allocatable        :: sorted_index(:)
  integer(i_kind),allocatable,target :: ifov_save(:)
! integer(i_kind),allocatable,target :: iscan_save(:)
! integer(i_kind),allocatable,target :: iorbn_save(:)
  integer(i_kind),allocatable,target :: inode_save(:)

  integer(i_kind),dimension(12):: mlen,mday
  integer(i_kind),dimension(5) :: iobsdate
  integer(i_kind),allocatable  :: nrec(:)  

  real(r_kind) :: sfcr,r07
! real(r_kind) :: pred
  real(r_kind) :: tdiff,dist1
! real(r_kind) :: step,start 
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr
  real(r_kind) :: disterr,disterrmax,cdist,dlon00,dlat00
  real(r_kind) :: fovn,sscan,orbn,rainf
! real(r_kind) :: sort_time1, sort_time2   
  real(r_kind) :: flgch
  real(r_kind) :: clat,clon
  real(r_kind) :: dlat,dlon
  real(r_kind) :: dlon_earth_deg,dlat_earth_deg,expansion,sat_aziang
  real(r_kind) :: utc_hour,sun_zenith,sun_azimuth

  real(r_double),dimension(7)         :: bufrinit
  real(r_double),dimension(3,5)       :: bufrymd
  real(r_double),dimension(2,2)       :: bufrhm
  real(r_double),dimension(2,29)      :: bufrloc
  real(r_double),dimension(1,maxchanl):: bufrtbb
  
  real(r_double) :: rnode

  real(r_kind),dimension(0:3) :: sfcpct
  real(r_kind),dimension(0:4) :: rlndsea
  real(r_kind),dimension(0:3) :: ts

  real(r_kind),pointer :: bt_in(:)
  real(r_kind),pointer :: crit1,rsat,t4dv,solzen,solazi,dlon_earth,dlat_earth,satazi,lza

  integer(i_kind),allocatable,target :: it_mesh_save(:)
  real(r_kind),allocatable,target :: rsat_save(:)
  real(r_kind),allocatable,target :: t4dv_save(:)
  real(r_kind),allocatable,target :: dlon_earth_save(:)
  real(r_kind),allocatable,target :: dlat_earth_save(:)
  real(r_kind),allocatable,target :: crit1_save(:)
  real(r_kind),allocatable,target :: lza_save(:)
  real(r_kind),allocatable,target :: satazi_save(:)
  real(r_kind),allocatable,target :: solzen_save(:)
  real(r_kind),allocatable,target :: solazi_save(:)
  real(r_kind),allocatable,target :: bt_save(:,:)
  real(r_kind),allocatable        :: relative_time_in_seconds(:)
  real(r_kind),allocatable        :: data_all(:,:)

  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin
  integer(i_kind),pointer:: it_mesh => null()

! For solar zenith/azimuth angles calculation
  data  mlen/31,28,31,30,31,30, &
             31,31,30,31,30,31/

!----------------------------------------------------------------------
! Initialize variables
  maxinfo   =  31
  m = 0
  do mon=1,12
     mday(mon) = m
     m = m + mlen(mon)
  end do

  do_noise_reduction = .true.
  if (ssmis_method .eq. 0) do_noise_reduction = .false.
  
  nchanl     = maxchanl
  disterrmax = zero
  ntest      = 0
  ndata      = 0
  nodata     = 0
  nread      = 0
  nfov_bad   = 0
  ilon       = 3
  ilat       = 4
  lnbufr     = 15
  r07        = 0.7_r_kind * deg2rad
  if (nst_gsi > 0 ) then
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
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
  if (.not.assim) val_ssmis=zero

  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif
! Make thinning grids
  call makegrids(rmesh,ithin,n_tbin=n_tbin)

! Set various variables depending on type of data to be read
  ssmis_uas= obstype == 'ssmis_uas'
  ssmis_las= obstype == 'ssmis_las'
  ssmis_img= obstype == 'ssmis_img'
  ssmis_env= obstype == 'ssmis_env'
  ssmis    = obstype == 'ssmis'
  
! Define WMO satellite number 
  bufsat = 0                                
  if (trim(sis) == 'ssmis_f16') bufsat=249  
  if (trim(sis) == 'ssmis_f17') bufsat=285  
  if (trim(sis) == 'ssmis_f18') bufsat=286 
  if (trim(sis) == 'ssmis_f19') bufsat=287 

  write(6,*) 'READ_SSMIS: reading bufsat = ', bufsat, trim(sis)

! Humidity imager:180
  if(ssmis)then
     nscan   = mxscen_las
     ifovoff = 270
     incangl = 53.0_r_kind
  else if(ssmis_img) then
     nscan   = mxscen_img
     ifovoff = 0
     incangl = 53.0_r_kind
! env:90
  else if(ssmis_env) then
     nscan   = mxscen_env
     ifovoff = 180
     incangl = 53.1_r_kind
! las:60
  else if(ssmis_las) then
     nscan   = mxscen_las
     ifovoff = 270
     incangl = 53.0_r_kind
! uas:30
  else if(ssmis_uas) then
     nscan   = mxscen_uas
     ifovoff = 330
     incangl = 52.4_r_kind
  end if

! Initialize variables for use by FOV-based surface code 
  if (isfcalc == 1) then
     instr=25  ! circular fov, use as default
     if (trim(jsatid) == 'f16') instr=26
     if (trim(jsatid) == 'f17') instr=27
     if (trim(jsatid) == 'f18') instr=28
     if (trim(jsatid) == 'f19') instr=29
!    right now, all ssmis data is mapped to a common fov -
!    that of the las channels.
     ichan = 1
     expansion = 2.9_r_kind
     sat_aziang = 90.0_r_kind  ! 'fill' value; need to get this from file
     call instrument_init(instr, jsatid, expansion, valid)
     if (.not. valid) then
       if (assim) then
         write(6,*)'READ_SSMIS:  ***ERROR*** IN SETUP OF FOV-SFC CODE. STOP'
         call stop2(71)
       else
         isfcalc = 0
         write(6,*)'READ_SSMIS:  ***ERROR*** IN SETUP OF FOV-SFC CODE'
       endif
    endif
  endif

  radedge_min = 0
  radedge_max = 1000
  do i=1,jpch_rad
     if (trim(nusis(i))==trim(sis)) then
!       step  = radstep(i)
!       start = radstart(i)
        if (radedge1(i)/=-1 .or. radedge2(i)/=-1) then
           radedge_min=radedge1(i)
           radedge_max=radedge2(i)
        end if
        exit
     endif
  end do

  rlndsea(0) = zero
  rlndsea(1) = 15._r_kind
  rlndsea(2) = 10._r_kind
  rlndsea(3) = 15._r_kind
  rlndsea(4) = 100._r_kind

! Allocate arrays for BUFR I/O
  allocate(ifov_save(maxobs))
! allocate(iscan_save(maxobs))
! allocate(iorbn_save(maxobs))
  allocate(inode_save(maxobs))
  allocate(rsat_save(maxobs))
  allocate(t4dv_save(maxobs))
  allocate(dlon_earth_save(maxobs))
  allocate(dlat_earth_save(maxobs))
  allocate(crit1_save(maxobs))
  allocate(it_mesh_save(maxobs))
  allocate(lza_save(maxobs))
  allocate(satazi_save(maxobs))
  allocate(solzen_save(maxobs))
  allocate(solazi_save(maxobs))
  allocate(bt_save(maxchanl,maxobs))

  inode_save = 0 

! Read in data from bufr into arrays first      
! Open unit to satellite bufr file
  iobs=1
  open(lnbufr,file=trim(infile),form='unformatted',status='old',err=500)  
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)

! Loop to read bufr file
  irec=0   
  read_subset: do while(ireadmg(lnbufr,subset,idate)>=0 .and. iobs < maxobs)
     irec = irec + 1
     if(irec < nrec_start) cycle read_subset
     read_loop: do while(ireadsb(lnbufr)==0 .and. iobs < maxobs)

        rsat        => rsat_save(iobs)
        t4dv        => t4dv_save(iobs)
        dlon_earth  => dlon_earth_save(iobs)
        dlat_earth  => dlat_earth_save(iobs)
        crit1       => crit1_save(iobs)
        it_mesh     => it_mesh_save(iobs)
        ifov        => ifov_save(iobs)
!       iscan       => iscan_save(iobs)
!       iorbn       => iorbn_save(iobs)
        inode       => inode_save(iobs)
        lza         => lza_save(iobs)
        satazi      => satazi_save(iobs)
        solzen      => solzen_save(iobs)
        solazi      => solazi_save(iobs)
 
!       BUFR read 1/3
        call ufbint(lnbufr,bufrinit,7,1,nlv, &
                    "SAID SECO SLNM FOVN SFLG RFLAG ORBN" )

!       Extract satellite id.  If not the one we want, read next record
        said = nint( bufrinit(1))  
        if( said /= bufsat) cycle read_subset

        rainf = bufrinit(6)
        irain = nint(rainf)

!       Rain check (-1=indeterminate 0=no rain 1=rain)
        if(irain == 1 .or. irain < 0) cycle read_loop    ! rain check

        rsat=bufsat

        fovn  = bufrinit(4)
        sscan  = bufrinit(3)
        orbn  = bufrinit(7)
        ifov  = nint(fovn)
!       iscan = nint(sscan)
!       iorbn = nint(orbn)
  

!       if not doing noise reduction, try reading node information
        if ( .not. do_noise_reduction ) then
           inode = 1000
           call ufbint(lnbufr,rnode,1,1,nlv, 'STKO')
           if (ibfms(rnode) == 0) then
              if (rnode == 1.) inode = -1
              if (rnode == 0.) inode = 1
           end if
        end if
        
!       Check date/time
!       BUFR read 2/3 --- read in observation date/time
        call ufbrep(lnbufr,bufrymd,3,5,nlv,"YEAR MNTH DAYS" )
        call ufbrep(lnbufr,bufrhm, 2,2,nlv,"HOUR MINU" )

!       Calc obs seqential time  If time outside window, skip this obs
        iobsdate(1:3) = bufrymd(1:3,1) !year,month,day for scan start time  kozo
        iobsdate(4:5) = bufrhm(1:2,1)  !hour,min for scan start time  kozo

        call w3fs21(iobsdate,nmind)
        t4dv=(real(nmind-iwinbgn,r_kind) + real(bufrinit(2),r_kind)*r60inv)*r60inv
        tdiff=t4dv+(iwinbgn-gstime)*r60inv
        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           if(abs(tdiff) > twind+one_minute) cycle read_loop
        endif

        crit0 = 0.01_r_kind
        timeinflat=6.0_r_kind
        call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)

!       Extract obs location, TBB, other information
!       BUFR read 3/3 --- read in observation lat/lon
        call ufbrep(lnbufr,bufrloc,  2,29,      nlv,"CLAT CLON" )
!       call ufbrep(lnbufr,bufrinfo, 1,3,       nlv,"SELV" )
!       call ufbrep(lnbufr,bufrlleaa,2,28,      nlv,"RAIA BEARAZ" )

        dlat_earth = bufrloc(1,1)  !degrees
        dlon_earth = bufrloc(2,1)  !degrees
        if(abs(dlat_earth)>90.0_r_kind .or. abs(dlon_earth)>r360) cycle read_loop
        if(dlon_earth <  zero) dlon_earth = dlon_earth+r360
        if(dlon_earth == r360) dlon_earth = dlon_earth-r360

        lza    = incangl    ! conical scanning instrument has fixed local zenith angle 
        satazi = 0          ! currently missing from bufr

!       Calculate solar zenith/azimuth angle
        clat = dlat_earth
        clon = dlon_earth
        if(clon > 180_r_kind) clon = clon-360.0_r_kind
        utc_hour = real(iobsdate(4),r_kind)+real(iobsdate(5),r_kind)*r60inv+real(bufrinit(2),r_kind)*r60inv*r60inv
!       utc_hour = real(iobsdate(4),r_kind)    !orig

        doy = mday( int(iobsdate(2)) ) + int(iobsdate(3))
        if ((mod( int(iobsdate(1)),4)==0).and.( int(iobsdate(2)) > 2))  then
           doy = doy + 1
        end if

        call zensun(doy,utc_hour,clat,clon,sun_zenith,sun_azimuth) ! output solar zenith angles are between -90 and 90
        sun_zenith = 90.-sun_zenith                                ! make sure solar zenith angles are between 0 and 180
        solzen     = sun_zenith 
        solazi     = sun_azimuth
           
!       Check Tb
!       Transfer observed brightness temperature to work array.

        call ufbrep(lnbufr,bufrtbb,  1,maxchanl,nlv,"TMBR" )  
        bt_save(1:maxchanl,iobs) = bufrtbb(1,1:maxchanl)
        
        iobs=iobs+1 

     end do read_loop
  end do read_subset
  call closbf(lnbufr)
  close(lnbufr)

  num_obs = iobs-1

500 continue  
  write(*,*) 'READ_SSMIS: num_obs  = ', num_obs, num_obs*nchanl
  if (num_obs <= 0 ) then
     write(*,*) 'READ_SSMIS: No ', trim(sis),  &
               ' data read in at mype mype_sub ', mype, mype_sub
     return 
  endif

  if (do_noise_reduction) then 

!    call cpu_time(sort_time1)
     write(*,*) 'READ_SSMIS: num_obs  = ', num_obs, num_obs*nchanl

!    Sort time in ascending order and get sorted index 
!    relative_time_in_seconds referenced at the beginning of the assimilation window
     allocate(relative_time_in_seconds(num_obs))  
     allocate(sorted_index(num_obs))
     relative_time_in_seconds  = 3600.0_r_kind*t4dv_save(1:num_obs)  
     sorted_index              = sortind(relative_time_in_seconds)  
     
!    Sort data according to observation time in ascending order  
     relative_time_in_seconds(1:num_obs) = relative_time_in_seconds(sorted_index)
     rsat_save(1:num_obs)                = rsat_save(sorted_index)
     t4dv_save(1:num_obs)                = t4dv_save(sorted_index)
     dlon_earth_save(1:num_obs)          = dlon_earth_save(sorted_index)
     dlat_earth_save(1:num_obs)          = dlat_earth_save(sorted_index)
     crit1_save(1:num_obs)               = crit1_save(sorted_index)
     it_mesh_save(1:num_obs)             = it_mesh_save(sorted_index)
     ifov_save(1:num_obs)                = ifov_save(sorted_index)
!    iscan_save(1:num_obs)               = iscan_save(sorted_index)
!    iorbn_save(1:num_obs)               = iorbn_save(sorted_index)
     lza_save(1:num_obs)                 = lza_save(sorted_index)
     satazi_save(1:num_obs)              = satazi_save(sorted_index)
     solzen_save(1:num_obs)              = solzen_save(sorted_index)
     solazi_save(1:num_obs)              = solazi_save(sorted_index)
     bt_save(:,1:num_obs)                = bt_save(:,sorted_index)

!    call cpu_time(sort_time2)
!    write(*,*)'READ_SSMIS: cpu_time (sorting)  ', sort_time2-sort_time1
!    write(*,*)'READ_SSMIS: min/max time        ', minval(relative_time_in_seconds(1:num_obs)), &
!                                                  maxval(relative_time_in_seconds(1:num_obs))  
!    write(*,*)'READ_SSMIS: min/max lat         ', minval(dlat_earth_save(1:num_obs)), &
!                                                  maxval(dlat_earth_save(1:num_obs))  
!    write(*,*)'READ_SSMIS: min/max lon         ', minval(dlon_earth_save(1:num_obs)), &
!                                                  maxval(dlon_earth_save(1:num_obs))  
!    write(*,*)'READ_SSMIS: min/max iscan_save  ', minval(iscan_save(1:num_obs)), & 
!                                                  maxval(iscan_save(1:num_obs))  
!    write(*,*)'READ_SSMIS: min/max ifov_save   ', minval(ifov_save(1:num_obs)), &
!                                                  maxval(ifov_save(1:num_obs))  
!    write(*,*)'READ_SSMIS: min/max bt_save     ', minval(bt_save(:,1:num_obs)), &
!                                                  maxval(bt_save(:,1:num_obs))  

!========================================================================================================================

!    Do SSMIS spatial averaging  
!    method=1 --- simply averaging over circular domains centered on each field of view
!    method=2 --- similar to 1 (for testing only)
!    method=3 --- AAPP package 

     method = ssmis_method
     write(*,*) 'READ_SSMIS: Calling ssmis_spatial_average, method =', method

     call ssmis_spatial_average(bufsat,method,num_obs,nchanl, & 
                                ifov_save,inode_save,relative_time_in_seconds,  & 
                                dlat_earth_save,dlon_earth_save, &
                                bt_save(1:nchanl,1:num_obs),iret)  ! inout 
     if (iret /= 0) then
        write(*,*) 'Error calling ssmis_spatial_average from READ_SSMIS'
        return
     endif

     if (num_obs > 0) then 
        deallocate(sorted_index)
        deallocate(relative_time_in_seconds)
     endif

  endif ! do_noise_reduction

!========================================================================================================================

! Complete thinning and QC steps for SSMIS
! Write header record to scratch file.  Also allocate array
! to hold all data for given satellite
  if(dval_use) maxinfo = maxinfo+2
  nreal  = maxinfo + nstinfo
  nele   = nreal   + nchanl
  allocate(data_all(nele,itxmax),nrec(itxmax)) 

  nrec=999999  
  obsloop: do iobs = 1, num_obs

     rsat       => rsat_save(iobs) 
     t4dv       => t4dv_save(iobs) 
     dlon_earth => dlon_earth_save(iobs) 
     dlat_earth => dlat_earth_save(iobs) 
     crit1      => crit1_save(iobs) 
     it_mesh    => it_mesh_save(iobs) 
     ifov       => ifov_save(iobs) 
     inode      => inode_save(iobs) 
     lza        => lza_save(iobs) 
     satazi     => satazi_save(iobs) 
     solzen     => solzen_save(iobs) 
     solazi     => solazi_save(iobs) 
     bt_in      => bt_save(1:nchanl,iobs) 

     if (inode == 0) cycle obsloop   ! this indicate duplicated data
     if (.not. use_edges .and. (ifov < radedge_min .or. ifov > radedge_max)) &
       cycle obsloop

     dlat_earth_deg = dlat_earth 
     dlon_earth_deg = dlon_earth 
     dlat_earth     = dlat_earth*deg2rad 
     dlon_earth     = dlon_earth*deg2rad 

!    Regional case
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

!       Check to see if in domain
        if(outside) cycle obsloop

!    Global case
     else
        dlat=dlat_earth
        dlon=dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif

!    Check time window
     tdiff=t4dv+(iwinbgn-gstime)*r60inv
     if (l4dvar.or.l4densvar) then
        if (t4dv<zero .OR. t4dv>winlen) cycle obsloop 
     else
        if(abs(tdiff) > twind) cycle ObsLoop
     endif

!    Map obs to thinning grid
     call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)
     if(.not. iuse)cycle obsloop


     nread=nread+nchanl

!    Transfer observed brightness temperature to work array.
!    If any temperature exceeds limits, reset observation
!    to "bad" value
     iskip=0
     do jc=1,nchanl
        if(bt_in(jc)<tbmin .or. bt_in(jc)>tbmax) then
              iskip = iskip + 1
        endif
     end do
     if(iskip>=nchanl) cycle obsloop !if all ch for any position are bad, skip

     flgch = iskip*two   !used for thinning priority range 0-14
     crit1 = crit1 + flgch 
     call checkob(dist1,crit1,itx,iuse)
     if (.not. iuse) cycle obsloop

!    Locate the observation on the analysis grid.  Get sst and land/sea/ice
!    mask.
!    isflg    - surface flag
!               0 sea
!               1 land
!               2 sea ice
!               3 snow
!               4 mixed
!     sfcpct(0:3)- percentage of 4 surface types
!                (0) - sea percentage
!                (1) - land percentage
!                (2) - sea ice percentage
!                (3) - snow percentage

!    FOV-based surface code requires fov number; if out-of-range, then 
!    skip this obs.

     if (isfcalc==1) then
        call deter_sfc_fov(fov_flag,ifov,instr,ichan,sat_aziang,dlat_earth_deg,&
             dlon_earth_deg,expansion,t4dv,isflg,idomsfc(1), &
             sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
     else
        call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc(1),sfcpct, &
           ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
     endif ! isfcalc==1

     crit1 = crit1 + rlndsea(isflg)
!    call checkob(dist1,crit1,itx,iuse)
!    if(.not. iuse)cycle obsloop

!    Set common predictor parameters
!    pred = zero

!    Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
!    crit1 = crit1+pred

     call finalcheck(dist1,crit1,itx,iuse)
     if(.not. iuse)cycle obsloop

!
!    interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
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

!    Load selected observation into data array  

     data_all( 1,itx)= rsat                      ! satellite id
     data_all( 2,itx)= t4dv                      ! time diff between obs and anal (min)
     data_all( 3,itx)= dlon                      ! grid relative longitude
     data_all( 4,itx)= dlat                      ! grid relative latitude
     data_all( 5,itx)= incangl*deg2rad           ! local zenith angle (rad)
     data_all( 6,itx)= inode                     ! local azimuth angle (missing)   ** AS/DS node infomation for SSMIS 
     data_all( 7,itx)= zero                      ! look angle (rad)   
     data_all( 8,itx)= ifov                      ! FOV scan position
!    data_all( 9,itx)= zero                      ! solar zenith angle (deg)  : not used for MW-RT calc
!    data_all(10,itx)= zero                      ! solar azimuth angle (deg) : not used for MW-RT calc
     data_all( 9,itx)= solzen                    ! calculated solar zenith angle (deg)    
     data_all(10,itx)= solazi                    ! calculated solar azimuth angle (deg) 
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
        data_all(32,itx)=val_ssmis
        data_all(33,itx)=itt
     end if

     if ( nst_gsi > 0 ) then
        data_all(maxinfo+1,itx) = tref         ! foundation temperature
        data_all(maxinfo+2,itx) = dtw          ! dt_warm at zob
        data_all(maxinfo+3,itx) = dtc          ! dt_cool at zob
        data_all(maxinfo+4,itx) = tz_tr        ! d(Tz)/d(Tr)
     endif

     do jc=1,nchanl
        data_all(nreal+jc,itx) = bt_in(jc)
     end do
     nrec(itx)=iobs 

  end do obsloop

! Deallocate I/O arrays
  deallocate(rsat_save)
  deallocate(ifov_save)
! deallocate(iscan_save)
  deallocate(inode_save)
! deallocate(iorbn_save)
  deallocate(t4dv_save)
  deallocate(dlon_earth_save)
  deallocate(dlat_earth_save)
  deallocate(crit1_save)
  deallocate(it_mesh_save)
  deallocate(lza_save)
  deallocate(satazi_save)
  deallocate(solzen_save)
  deallocate(solazi_save)
  deallocate(bt_save)

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
           super_val(itt)=super_val(itt)+val_ssmis
        end do
     end if

!    Write final set of "best" observations to output file
     call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)

  endif

! Deallocate local arrays
  deallocate(data_all,nrec) 

! Deallocate satthin arrays
1000 continue
  call destroygrids

  if(diagnostic_reg .and. ntest>0 .and. mype_sub==mype_root) &
     write(6,*)'READ_SSMIS:  mype,ntest,disterrmax=',&
     mype,ntest,disterrmax
  if (nfov_bad>0) &
     write(6,*)'READ_SSMIS(',obstype,'):  found ',nfov_bad,' questionable fov'
  
! End of routine
  return

end subroutine read_ssmis
