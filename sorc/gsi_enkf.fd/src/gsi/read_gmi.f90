subroutine read_gmi(mype,val_gmi,ithin,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs,dval_use)

!$$$  subprogram documentation block
! subprogram:    read_gmi           read  GMI  bufr data
!   prgmmr: j.jin    copied from read_tmi.f90    date: 2014-5-08
!
! abstract:  This routine reads BUFR format  GMI radiance 
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
!   2014-03-29  j.jin   - read gmi.
!   2014-05-08  j.jin   - copy/separate from read_tmi.f90. Needs clean up.
!   2014-09-03  j.jin   - read GMI 1CR data, obstype=gmi. Increase the size 
!                         of data_all for channel 10-13 geo-information.
!   2014-12-15  ejones  - add rfi check
!   2015-03-02  ejones  - add logical "use_swath_edge" to allow user to choose 
!                         whether obs are used at swath edges where there are 
!                         no TBs for ch10-13. If the logical is set to true, the missing
!                         observations for ch10-13 are given a TB of 500K, and
!                         these obs will be removed by the gross check elsewhere
!                         in the GSI, allowing the observations from ch1-9
!                         through. If the logical is set to false, the swath
!                         edge obs are skipped in the read loop. 
!   2015-09-17  Thomas  - add l4densvar and thin4d to data selection procedure
!   2015-10-01  guo     - Fixed dlxx_earth_deg, to avoid truncation errors
!   2016-03-04  ejones  - add spatial averaging capability (use SSMI/S spatial averaging)
!   2016-04-29  ejones  - update some mnemonics for expected operational bufr
!                         tanks
!   2016-07-25  ejones  - increase maxobs, remove fov binning, make most arrays
!                         static
!   2016-03-11  guo     - Refixed dlxx_earth_deg, for the new dlxx_earth_save(:).
!   2016-03-22  j.jin   - Set a range (0-360 degree) for satellite and Sun azimuth
!                         angles.
!   2016-10-05  acollard -Fix interaction with NSST.
!   2017-01-03  todling - treat save arrays as allocatable
!   2017-08-03  j.jin   - Re-implement the writing out geoinformation for GMI channel 10-13.
!                         The information is needed for the processing of 1CR data, and 
!                         should not have beend taken out.  Note: Use the same 
!                         sun_zenith and sun_azimuth angles for ch10-13 as for ch1-9. 
!                       - Check bufr formats while reading because of different formats
!                         at GMAO and NOAA. Eventually the research bufr data set will be
!                         the same at the operational one.
!   2017-08-10  j.jin   - Bug fix: crit1 should not have been initialized as zero (when thin4d=True).
!   2017-08-19  j.jin   - Keep the binning of ifov by 3 independent of adp_anglebc=True or False.
!   2018-05-21  j.jin   - Added time-thinning.
!
!   input argument list:
!     mype     - mpi task id
!     val_gmi - weighting factor applied to super obs
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
!
!   output argument list:
!     nread    - number of BUFR  observations read (after eliminating orbit overlap)
!     ndata    - number of BUFR  profiles retained for further processing (thinned)
!     nodata   - number of BUFR  observations retained for further processing (thinned)
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!
! Note:
!   2013-10-21  j.jin   - there is not a procedure for isfcalc.
!                         (isfcalc - specifies method to determine surface fields
!                         within a FOV. When it is equal to one, integrate
!                         model fields over a FOV. When it is not equal to one, bilinearly
!                         interpolate model fields at a FOV center.)
!
!$$$  end documentation block
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
      checkob,finalcheck,score_crit
  use satthin, only: radthin_time_info,tdiff2crit
  use obsmod, only: time_window_max
  use radinfo, only: iuse_rad,jpch_rad,nusis,use_edges, &
                     radedge1,radedge2,gmi_method
  use gridmod, only: diagnostic_reg,regional,rlats,rlons,nlat,nlon,&
      tll2xy,txy2ll
  use constants, only: deg2rad,rad2deg,zero,one,two,three,four,r60inv,rearth
  use gsi_4dvar, only: l4dvar,iwinbgn,winlen,l4densvar
  use deter_sfc_mod, only: deter_sfc,deter_sfc_gmi
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter
  use ssmis_spatial_average_mod, only : ssmis_spatial_average
  use m_sortind
  use mpimod, only: npe

  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: infile,obstype,jsatid
  character(len=*),intent(in   ) :: sis
  integer(i_kind), intent(in   ) :: mype,lunout,ithin
  integer(i_kind), intent(in   ) :: mype_root
  integer(i_kind), intent(in   ) :: mype_sub
  integer(i_kind), intent(in   ) :: npe_sub
  integer(i_kind), intent(in   ) :: mpi_comm_sub
  real(r_kind)   , intent(in   ) :: rmesh,gstime,twind
  real(r_kind)   , intent(inout) :: val_gmi
  integer(i_kind),intent(inout)  :: nread
  integer(i_kind),intent(inout)  :: ndata,nodata
  integer(i_kind),dimension(npe)  ,intent(inout) :: nobs
  logical         ,intent(in   ) :: dval_use

! Declare local parameters
  logical                   :: use_swath_edge
  integer(i_kind)           :: maxinfo
  integer(i_kind),parameter   :: maxchanl=13, ngs=2

  integer(i_kind),dimension(maxchanl)    :: tbmin                 ! different tbmin for the channels.
  !real(r_double),dimension(maxchanl)     :: mirad,gmichq,gmirfi   ! TBB from strtmbr
  real(r_double),dimension(maxchanl)     :: mirad,var_check1,var_check2   ! TBB from strtmbr
  logical                   :: use_gmichq

  real(r_double)            :: fovn,slnm      ! FOVN 
  real(r_kind),parameter    :: r360=360.0_r_kind
  character(80),parameter   :: satinfo='SAID SIID OGCE GSES SACV'          !use for ufbint()
  character(80),parameter   :: hdr1b='YEAR MNTH DAYS HOUR MINU SECO ORBN'  !use for ufbint()
  integer(i_kind),parameter :: ntime=7, ninfo=5                                     !time header
  real(r_kind)              :: tbmax, satinfo_v(ninfo)
  real(r_double),dimension(ntime):: bfr1bhdr

  integer(i_kind),parameter :: nloc=3                                      !location dat used for ufbint()
  real(r_double),dimension(nloc) :: midat                                  !location data from 

  character(40),parameter   :: strloc='CLATH CLONH'                        !use for ufbint() 
  character(40),parameter   :: strsaza='SAZA'                              !use for ufbint() 
  real(r_double)            :: pixelloc(2)                                 !location data
  character(40),parameter   :: strtmbr='TMBR', strfovn='FOVN'              !use for ufbrep()  
  character(40),parameter   :: strslnm='SLNM'
  character(8)              :: subset
  real(r_kind), parameter   :: bmiss=990_r_kind   ! miss values are 999 in bufr
                                                  ! undefined value is 1.0e+11 in bufr data files.

!  character(40),parameter   :: str_angls='SAMA SZA SMA SGA'  ! non-operational bufr
  character(40),parameter   :: str_angls='SOLAZI SOZA SMA SSGA' 
  integer(i_kind),parameter :: n_angls=4
  real(r_double),dimension(n_angls,ngs)  :: val_angls
  real(r_double),dimension(ngs)          :: pixelsaza

! Declare local variables
  logical        :: assim,outside,iuse
  logical        :: do_noise_reduction

  integer(i_kind):: i,k,ntest,ireadsb,ireadmg,irec,next,j
  integer(i_kind):: iret,idate,nchanl,nchanla
  integer(i_kind):: isflg,nreal,idomsfc
  integer(i_kind):: nmind,itx,nele,itt
  integer(i_kind):: iskip
  integer(i_kind):: lnbufr
  integer(i_kind):: ilat,ilon

  real(r_kind) :: sfcr
  real(r_kind) :: sstime,tdiff
  real(r_kind) :: dist1    
  real(r_kind),allocatable,dimension(:,:):: data_all
  integer(i_kind),allocatable,dimension(:)::nrec

  real(r_kind) :: disterr,disterrmax,dlon00,dlat00

  integer(i_kind) :: jc,bufsat,n    
  integer(i_kind),dimension(5):: iobsdate
  integer(i_kind):: method,iobs,num_obs
  integer(i_kind),parameter   :: maxobs=6000000
  !-- integer(i_kind),parameter   :: nscan=74       ! after binning ifov, 221/3 + 1
  integer(i_kind),parameter   :: nscan=221

  real(r_kind):: flgch
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr
  real(r_kind):: dlat,dlon        
  real(r_kind):: sat_def_ang,sat_def_ang2    
  real(r_kind),allocatable        :: relative_time_in_seconds(:)

  real(r_kind) :: dlon_earth_deg,dlat_earth_deg
  real(r_kind),pointer :: t4dv,dlon_earth,dlat_earth,crit1
  real(r_kind),pointer :: sat_zen_ang,sat_zen_ang2,sat_azimuth_ang,sat_azimuth_ang2
  real(r_kind),pointer :: sat_scan_ang,sat_scan_ang2
  real(r_kind),pointer :: tbob(:)
  integer(i_kind),pointer :: ifov,iscan,iorbn,inode   

  integer(i_kind),allocatable        :: sorted_index(:)

  integer(i_kind),target,allocatable,dimension(:) :: ifov_save
  integer(i_kind),target,allocatable,dimension(:) :: iscan_save
  integer(i_kind),target,allocatable,dimension(:) :: iorbn_save
  integer(i_kind),target,allocatable,dimension(:) :: inode_save
  integer(i_kind),target,allocatable,dimension(:) :: it_mesh_save
  real(r_kind),target,allocatable,dimension(:)    :: dlon_earth_save
  real(r_kind),target,allocatable,dimension(:)    :: dlat_earth_save
  real(r_kind),target,allocatable,dimension(:)    :: sat_zen_ang_save,sat_azimuth_ang_save,sat_scan_ang_save
  real(r_kind),target,allocatable,dimension(:)    :: sat_zen_ang2_save,sat_azimuth_ang2_save,sat_scan_ang2_save
  real(r_kind),target,allocatable,dimension(:)    :: t4dv_save
  real(r_kind),target,allocatable,dimension(:)    :: crit1_save
  real(r_kind),target,allocatable,dimension(:,:)  :: tbob_save
  real(r_kind),target,allocatable,dimension(:)    :: sun_zenith_save,sun_azimuth_ang_save


! ---- sun glint ----
  integer(i_kind):: doy,mday(12),mon,m,mlen(12)
  real(r_kind)   :: time_4_sun_glint_calc,clath_sun_glint_calc,clonh_sun_glint_calc
  real(r_kind),pointer :: sun_zenith,sun_azimuth_ang

  data  mlen/31,28,31,30,31,30, &
             31,31,30,31,30,31/

  integer(i_kind) :: pos_max      
  integer(i_kind),allocatable       :: pos_statis(:)
  integer(i_kind),allocatable       :: npos_all(:,:)

! ---- skip some obs at the beginning and end of a scan ----
  integer(i_kind):: radedge_min,radedge_max,iscan_pos,iedge_log,j2
  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin
  integer(i_kind),pointer:: it_mesh => null()

!**************************************************************************

! Initialize variables
  call init_(maxchanl,maxobs)
  use_swath_edge = .false.

  do_noise_reduction = .true.
  if (gmi_method == 0) do_noise_reduction = .false.

  lnbufr = 15
  disterrmax=zero
  ntest=0
  iscan_pos = 8     ! id in data_all for scan positions
  iedge_log  = 32    ! id in data_all for log if obs is to be obleted beause of locating near scan edges.

  ndata  = 0
  nodata = 0
  nread  = 0
  sat_def_ang =52.8_r_kind   ! default TMI/GMI satellite zenith angle.
  sat_def_ang2=49.2_r_kind   ! default GMI channel 10-13 satellite zenith angle.

  ilon=3 
  ilat=4

  if(nst_gsi>0) then   
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif
  m = 0
  do mon=1,12
     mday(mon) = m
     m = m + mlen(mon)
  end do

! Set various variables depending on type of data to be read
                                        !     (for grouping the obs at a position)
  if(jsatid == 'gpm')bufsat=288         ! Satellite ID (WMO as of 03Jun2014)
  tbmax = 320.0_r_kind                  ! one value for all tmi channels (see data document).

  maxinfo=37
  if(dval_use) maxinfo = maxinfo+2
  nchanl = 13                         ! 13 channls
  nchanla = 9                         ! first 9 channels
  tbmin = (/50,50,50,50,50,50,50,50,50,70,70,70,70/)             !

  rlndsea(0) = zero
  rlndsea(1) = 30._r_kind
  rlndsea(2) = 30._r_kind
  rlndsea(3) = 30._r_kind
  rlndsea(4) = 100._r_kind

! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.

  radedge_min = 0
  radedge_max = 1000
  assim=.false.
  search: do i=1,jpch_rad
    if (trim(nusis(i))==trim(sis)) then
        if (radedge1(i)/=-1 .and. radedge2(i)/=-1) then
           radedge_min=radedge1(i)
           radedge_max=radedge2(i)
        end if
        if (iuse_rad(i)>=0) then
           if (iuse_rad(i)>0) assim=.true.
           if (assim) exit
        endif
     endif
  end do search
  if (.not.assim) val_gmi=zero


  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif
! Make thinning grids
  call makegrids(rmesh,ithin,n_tbin=n_tbin)

  inode_save = 0

! Open unit to satellite bufr file
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)

!This block may be needed if used at GMAO, for its gmi data.
!Extract satellite id from the 1st MG.  If it is not the one we want, exit reading.
  call readmg(lnbufr, subset, iret, idate)
  rd_loop: do while (ireadsb(lnbufr)==0)

    call ufbint(lnbufr,satinfo_v,ninfo,1,iret,satinfo)
    if(nint(satinfo_v(1)) /= bufsat) then 
      write(6,*) 'READ_GMI: Bufr satellie ID SAID', nint(satinfo_v(1)), &
                 ' does not match ', bufsat
      go to 690   ! skip to the end of read_subset block
    endif
  enddo rd_loop

! Big loop to read data file
  next=0
  irec=0
  iobs=1

  read_subset: do while(ireadmg(lnbufr,subset,idate)>=0) ! GMI scans
     irec=irec+1
     next=next+1
     if(next == npe_sub)next=0
     if(next /= mype_sub)cycle
     read_loop: do while (ireadsb(lnbufr)==0)            ! GMI pixels

        call ufbint(lnbufr,satinfo_v,ninfo,1,iret,satinfo)
        if(nint(satinfo_v(1)) /= bufsat) then 
           write(6,*) 'READ_GMI: Bufr satellie ID SAID', nint(satinfo_v(1)), &
                 ' does not match ', bufsat
           cycle read_loop
        end if
        t4dv        => t4dv_save(iobs)
        dlon_earth  => dlon_earth_save(iobs)
        dlat_earth  => dlat_earth_save(iobs)
        crit1       => crit1_save(iobs)
        it_mesh     => it_mesh_save(iobs)
        ifov        => ifov_save(iobs)
        iscan       => iscan_save(iobs)
        iorbn       => iorbn_save(iobs)
        inode       => inode_save(iobs)
        sat_zen_ang         => sat_zen_ang_save(iobs)
        sat_zen_ang2        => sat_zen_ang2_save(iobs)
        sat_azimuth_ang     => sat_azimuth_ang_save(iobs)
        sat_azimuth_ang2    => sat_azimuth_ang2_save(iobs)
        sat_scan_ang        => sat_scan_ang_save(iobs)
        sat_scan_ang2       => sat_scan_ang2_save(iobs)
        sun_zenith          => sun_zenith_save(iobs)
        sun_azimuth_ang     => sun_azimuth_ang_save(iobs)
        call ufbrep(lnbufr,fovn,1, 1,iret, strfovn)
        call ufbrep(lnbufr,slnm,1, 1,iret, strslnm)
        ifov  = nint(fovn)
        !-- ifov  = ifov/3_i_kind + 1.0_r_kind
        iscan = nint(slnm)
        if (.not. use_edges .and. &
             (ifov < radedge_min .OR. ifov > radedge_max )) then
             cycle read_loop             
        endif                            

! ----- extract time information  
        call ufbint(lnbufr,bfr1bhdr,ntime,1,iret,hdr1b)

!       calc obs seqential time. If time is outside window, skip this obs
        iobsdate(1:5) = bfr1bhdr(1:5) !year,month,day,hour,min
        call w3fs21(iobsdate,nmind)
        t4dv=(real(nmind-iwinbgn,r_kind) + real(bfr1bhdr(6),r_kind)*r60inv)*r60inv
        sstime=real(nmind,r_kind) + real(bfr1bhdr(6),r_kind)*r60inv
        tdiff=(sstime-gstime)*r60inv
        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           if(abs(tdiff) > twind) then 
             cycle read_loop             
           endif                        
        endif

        crit0=0.01_r_kind
        timeinflat=6.0_r_kind
        call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)

! ----- Read header record to extract obs location information  
          call ufbint(lnbufr,midat,nloc,1,iret,'SCLAT SCLON HMSL')
          call ufbrep(lnbufr,var_check1,1,nchanl,iret,'GMICHQ')
          !call ufbrep(lnbufr,gmirfi,1,nchanl,iret,'GMIRFI')
          call ufbrep(lnbufr,pixelsaza,1,ngs,iret,'SAZA')
          call ufbrep(lnbufr,val_angls,n_angls,ngs,iret,'BEARAZ SOZA SOLAZI SSGA')
          call ufbint(lnbufr,pixelloc,2, 1,iret,'CLATH CLONH')

        if (any(var_check1 < 99999999999_r_double)) then   ! 100000000000 seems to be the missing value
           use_gmichq = .true.
           call ufbrep(lnbufr,var_check2,1,nchanl,iret,'GMIRFI')
        else
           use_gmichq = .false.
           call ufbrep(lnbufr,var_check1,1,nchanl,iret,'VIIRSQ')
           call ufbrep(lnbufr,var_check2,1,nchanl,iret,'TPQC2')
        endif

!---    Extract brightness temperature data.  Apply gross check to data. 
!       If obs fails gross check, reset to missing obs value.
        call ufbrep(lnbufr,mirad,1,nchanl,iret,strtmbr)

        dlat_earth = pixelloc(1)  !deg
        dlon_earth = pixelloc(2)  !deg
        if(abs(dlat_earth)>90.0_r_kind .or. abs(dlon_earth)>r360) then 
          cycle read_loop             
        endif                        

        if(dlon_earth< zero) dlon_earth = dlon_earth+r360
        if(dlon_earth==r360) dlon_earth = dlon_earth-r360

!       If available, set value of zenith angle
        if (pixelsaza(1) < bmiss ) then
           sat_zen_ang = pixelsaza(1)*deg2rad
        else
           sat_zen_ang = sat_def_ang*deg2rad
        endif
        sat_azimuth_ang = val_angls(1,1)   
        sun_zenith      = val_angls(2,1)
        sun_azimuth_ang = val_angls(3,1)
        sat_scan_ang = asin( sin(sat_zen_ang)*rearth/(rearth+midat(3)) )
        if (pixelsaza(ngs) < bmiss ) then
          sat_zen_ang2 = pixelsaza(ngs)*deg2rad
        else
          sat_zen_ang2 = sat_def_ang2*deg2rad
        endif
        sat_scan_ang2 = asin( sin(sat_zen_ang2)*rearth/(rearth+midat(3)) )
        sat_azimuth_ang2 = val_angls(1,ngs)     

           !  -------- Retreive Sun glint angle -----------
        clath_sun_glint_calc = pixelloc(1)
        clonh_sun_glint_calc = pixelloc(2)
        if(clonh_sun_glint_calc > 180._r_kind) clonh_sun_glint_calc = clonh_sun_glint_calc - 360.0_r_kind
        doy = mday(iobsdate(2)) + iobsdate(3)
        if ( (mod(iobsdate(1),4)==0) .and. (iobsdate(2)>2) ) then
           doy = doy + 1
        end if
        time_4_sun_glint_calc = bfr1bhdr(4)+bfr1bhdr(5)*r60inv+bfr1bhdr(6)*r60inv*r60inv
        call zensun(doy,time_4_sun_glint_calc,clath_sun_glint_calc,clonh_sun_glint_calc,sun_zenith,sun_azimuth_ang)
        ! output solar zenith angles are between -90 and 90
        ! make sure solar zenith angles are between 0 and 180
        sun_zenith = 90.0_r_kind-sun_zenith
        ! Make sure satellite's and Sun's azimuth angles are within 0-360 degree.
        if( sat_azimuth_ang < 0_r_kind ) sat_azimuth_ang = sat_azimuth_ang + 360_r_kind
        if( sun_azimuth_ang < 0_r_kind ) sun_azimuth_ang = sun_azimuth_ang + 360_r_kind
!        if( sat_azimuth_ang2< 0_r_kind ) sat_azimuth_ang2= sat_azimuth_ang2+ 360_r_kind
        
!          If use_swath_edge is true, set missing ch10-13 TBs to 500, so they
!          can be tossed in gross check while ch1-9 TBs go through. If
!          use_swath_edge is false, skip these obs 

        do jc=10,nchanl
           if(mirad(jc)>1000.0_r_kind) then         
              if(use_swath_edge) then
                mirad(jc) = 500.0_r_kind !-replace missing tbs(ch10-13, swath edge)
              else
                cycle read_loop   ! skip obs 
              endif
           endif
        enddo

        iskip=0
        do jc=1, nchanla    ! only does such check the first 9 channels for GMI 1C-R data
           if( mirad(jc)<tbmin(jc) .or. mirad(jc)>tbmax ) then
              iskip = iskip+1
           !endif
           !if(use_gmichq) then
           elseif (use_gmichq) then
              if (var_check1(jc) < -0.5_r_kind .or. var_check1(jc) > 1.5_r_kind .or. &
                  var_check2(jc) > 0.0_r_kind) then
                 iskip = iskip+1
              else
                 nread=nread+1
              endif
           else
              if (var_check1(jc) < -0.5_r_kind .or. var_check1(jc) > 0.5_r_kind .or. &
                  var_check2(jc) > 0.0_r_kind) then
                 iskip = iskip+1
              else
                 nread=nread+1
              endif
           endif
           !if( mirad(jc)<tbmin(jc) .or. mirad(jc)>tbmax .or. &
           !   gmichq(jc) < -0.5_r_kind .or. gmichq(jc) > 1.5_r_kind .or. & 
           !   gmirfi(jc)>0.0_r_kind) then ! &
           !   iskip = iskip + 1
           !else
           !   nread=nread+1
           !end if
        enddo

        if(iskip == nchanla) then 
          cycle read_loop             
        endif                        
        tbob_save(1:maxchanl,iobs) = mirad 
        nread=nread + (nchanl - nchanla)

        flgch = 0

        iobs=iobs+1
        if(iobs>maxobs) exit
     end do read_loop
  end do read_subset
690 continue
  call closbf(lnbufr)
  close(lnbufr)
  
  num_obs=iobs-1
  if( mype_sub==mype_root) write(6,*) 'READ_GMI: do_noise_reduction=', do_noise_reduction
  if (do_noise_reduction) then

!    Sort time in ascending order and get sorted index
!    relative_time_in_seconds referenced at the beginning of the assimilation
!    window
     allocate(relative_time_in_seconds(num_obs))
     allocate(sorted_index(num_obs))
     relative_time_in_seconds  = 3600.0_r_kind*t4dv_save(1:num_obs)
     sorted_index              = sortind(relative_time_in_seconds)

!    Sort data according to observation time in ascending order
     relative_time_in_seconds(1:num_obs) = relative_time_in_seconds(sorted_index)
     t4dv_save(1:num_obs)                = t4dv_save(sorted_index)
     dlon_earth_save(1:num_obs)          = dlon_earth_save(sorted_index)
     dlat_earth_save(1:num_obs)          = dlat_earth_save(sorted_index)
     crit1_save(1:num_obs)               = crit1_save(sorted_index)
     it_mesh_save(1:num_obs)             = it_mesh_save(sorted_index)
     ifov_save(1:num_obs)                = ifov_save(sorted_index)
     iscan_save(1:num_obs)               = iscan_save(sorted_index)
     iorbn_save(1:num_obs)               = iorbn_save(sorted_index)
     sat_zen_ang_save(1:num_obs)         = sat_zen_ang_save(sorted_index)
     sat_zen_ang2_save(1:num_obs)        = sat_zen_ang2_save(sorted_index)
     sat_azimuth_ang_save(1:num_obs)     = sat_azimuth_ang_save(sorted_index)
     sat_azimuth_ang2_save(1:num_obs)    = sat_azimuth_ang2_save(sorted_index)
     sun_zenith_save(1:num_obs)          = sun_zenith_save(sorted_index)
     sun_azimuth_ang_save(1:num_obs)     = sun_azimuth_ang_save(sorted_index)
     tbob_save(:,1:num_obs)              = tbob_save(:,sorted_index)

!    Do spatial averaging using SSMIS spatial averaging

     method = gmi_method
     write(6,*) 'READ_GMI: Calling ssmis_spatial_average, method =', method

     call ssmis_spatial_average(bufsat,method,num_obs,nchanl, &
                                ifov_save,inode_save,relative_time_in_seconds, &
                                dlat_earth_save,dlon_earth_save, &
                                tbob_save(1:nchanl,1:num_obs),iret)  ! inout

     if (iret /= 0) then
        write(6,*) 'Error calling ssmis_spatial_average from READ_GMI'
        return
     endif

     if (num_obs > 0) then
        deallocate(sorted_index)
        deallocate(relative_time_in_seconds)
     endif

  endif ! do_noise_reduction
!========================================================================================================================

! Complete thinning for GMI
! Write header record to scratch file.  Also allocate array
! to hold all data for given satellite
  nreal  = maxinfo + nstinfo
  nele   = nreal   + nchanl
  allocate(data_all(nele,itxmax),nrec(itxmax))

  nrec=999999
  if (mype==0) write(*,*) 'read_gmi num_obs before thinning: ', num_obs
  obsloop: do iobs = 1, num_obs

     t4dv        => t4dv_save(iobs)
     dlon_earth  => dlon_earth_save(iobs)
     dlat_earth  => dlat_earth_save(iobs)
     crit1       => crit1_save(iobs)
     it_mesh     => it_mesh_save(iobs)
     ifov        => ifov_save(iobs)
     iscan       => iscan_save(iobs)
     iorbn       => iorbn_save(iobs)
     inode       => inode_save(iobs)
     sat_zen_ang         => sat_zen_ang_save(iobs)
     sat_zen_ang2        => sat_zen_ang2_save(iobs)
     sat_azimuth_ang     => sat_azimuth_ang_save(iobs)
     sat_azimuth_ang2    => sat_azimuth_ang2_save(iobs)
     sat_scan_ang        => sat_scan_ang_save(iobs)
     sat_scan_ang2       => sat_scan_ang2_save(iobs)
     sun_zenith          => sun_zenith_save(iobs)
     sun_azimuth_ang     => sun_azimuth_ang_save(iobs)
     tbob                => tbob_save(1:nchanl,iobs)

     if (do_noise_reduction) then 
        if (inode == 0) cycle obsloop   ! this indicate duplicated data
     endif 

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
           disterr=acos(sin(dlat_earth)*sin(dlat00)+cos(dlat_earth)*cos(dlat00)*&
                   (sin(dlon_earth)*sin(dlon00)+cos(dlon_earth)*cos(dlon00)))*rad2deg
           disterrmax=max(disterrmax,disterr)
        end if

!       Check to see if in domain
        if(outside) cycle obsloop

!    Global case
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif

!   Check time
    if (l4dvar.or.l4densvar) then
        if (t4dv<zero .OR. t4dv>winlen) cycle obsloop
    else
        tdiff=t4dv+(iwinbgn-gstime)*r60inv
        if(abs(tdiff) > twind) cycle obsloop
    endif

!   Map obs to thinning grid
    call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)

    if(.not. iuse) then
      cycle obsloop
    endif

!   Check TBs again
    iskip=0
    do jc=1, nchanla    ! only does such check the first 9 channels for GMI 1C-R data
       if( tbob(jc)<tbmin(jc) .or. tbob(jc)>tbmax )then
            iskip = iskip + 1
       endif
    end do
    if(iskip>=nchanl) cycle obsloop !if all ch for any position are bad, skip

! if the obs is far from the grid box center, do not use it.
    if(ithin /= 0) then
       if(.not. regional .and. dist1 > 0.75_r_kind) cycle obsloop
    endif

    crit1 = crit1 + 10._r_kind * real(iskip,r_kind)
    call checkob(dist1,crit1,itx,iuse)
    if(.not. iuse) then
       cycle obsloop
    endif

!   Locate the observation on the analysis grid.  Get sst and
!   land/sea/ice mask.

!       isflg    - surface flag
!                  0 sea
!                  1 land
!                  2 sea ice
!                  3 snow
!                  4 mixed

    call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
         ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
    call deter_sfc_gmi(dlat_earth,dlon_earth,isflg)


!   Only keep obs over ocean    - ej
    if(isflg /= 0) cycle obsloop

    crit1 = crit1 + rlndsea(isflg)
    call checkob(dist1,crit1,itx,iuse)
    if(.not. iuse) then
       cycle obsloop
    endif

    call finalcheck(dist1,crit1,itx,iuse)
    if(.not. iuse) then
       cycle obsloop
    endif

!   interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
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

!   Transfer observation parameters to output array.
    data_all( 1,itx) = bufsat              ! satellite id
    data_all( 2,itx) = t4dv                ! time diff between obs and anal (min)
    data_all( 3,itx) = dlon                ! grid relative longitude
    data_all( 4,itx) = dlat                ! grid relative latitude
    data_all( 5,itx) = sat_zen_ang         ! local (satellite) zenith angle (radians)
    data_all( 6,itx) = sat_azimuth_ang     ! local (satellite) azimuth_ang angle (degrees)
    data_all( 7,itx) = sat_scan_ang        ! scan(look) angle (rad)
    data_all( 8,itx) = ifov                ! scan position
    data_all( 9,itx) = sun_zenith          ! solar zenith angle (deg)
    data_all(10,itx) = sun_azimuth_ang     ! solar azimuth_ang angle (deg)
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
    data_all(iedge_log,itx) = 0            ! =0, not to be obsoleted at scan edges
    data_all(33,itx) = sat_zen_ang2        ! local (satellite) zenith angle (radians)
    data_all(34,itx) = sat_azimuth_ang2    ! local (satellite) azimuth_ang angle (degrees)
    data_all(35,itx) = sat_scan_ang2       ! scan(look) angle (rad)
    data_all(36,itx) = sun_zenith          ! solar zenith angle (deg)
    data_all(37,itx) = sun_azimuth_ang     ! solar azimuth_ang angle (deg)

    if(dval_use) then
       data_all(maxinfo-1,itx)= val_gmi
       data_all(maxinfo,itx)= itt
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
  end do obsloop

! If multiple tasks read input bufr file, allow each tasks to write out
! information it retained and then let single task merge files together
  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
     nele,itxmax,nread,ndata,data_all,score_crit,nrec)
  if( mype_sub==mype_root) write(6,*) 'READ_GMI: after combine_obs, nread,ndata is ',nread,ndata

!=========================================================================================================
  if( use_edges .and. (radedge_min > 1 .or. radedge_max < nscan).and. mype_sub==mype_root )then !nscan instead of ang_nn
    ! Obsolete some obs at the beginning and end positions of a scan by flagging
    !       obs at these positions with negative NPOS values.
    ! Note: This is an arbitary process. Just want to phase out part of these obs
    !       at the scan edges in the QC process (qc_ssmi, ifail_scanedge_qc=58).
    !       However, there is not a known quality issue at the edge of scans.
    !       JJJ, 2/12/2014
     pos_max=ndata  
     allocate(npos_all(pos_max,nscan))
     allocate(pos_statis(nscan))
     npos_all = 0
     pos_statis = 0
     do n=1,ndata
        i = nint(data_all(iscan_pos,n))
        pos_statis(i) = pos_statis(i) + 1
        npos_all(pos_statis(i), i) = n
     enddo

     do n=1, ndata
        i = nint(data_all(iscan_pos,n))
        if(i < radedge_min .or. i > radedge_max) then
          data_all(iedge_log,n) = 1     ! assume all at scan edges at the beginning.
        endif
     enddo
     if( radedge_min > 1 )then
       pos_max = sum(pos_statis(radedge_min : (radedge_min+1)))/2 
       do i=radedge_min-1, 1, -1
         if(pos_max==0) then
           j2=1
         else
           j2=nint(real(pos_statis(i),r_kind)/pos_max)
           j2=max(1,j2)
         endif
         do j=1,pos_statis(i),j2
           n = npos_all(j,i)
           data_all(iedge_log,n)= 0     ! flag back
         enddo      
       enddo
     endif

     if( radedge_max < nscan )then
       pos_max = sum(pos_statis((radedge_max-1) : radedge_max))/2 
       do i=radedge_max+1,nscan
         if(pos_max==0) then
           j2=1
         else
           j2=nint(real(pos_statis(i),r_kind)/pos_max)
           j2=max(1,j2)
         endif
         do j=1,pos_statis(i),j2
           n = npos_all(j,i)
           data_all(iedge_log,n)= 0     ! flag back
         enddo      
       enddo
     endif

     ! new pos_statis
     pos_statis=0
     do n=1,ndata
        i = nint(data_all(iscan_pos,n))
        if(data_all(iedge_log,n)>0) cycle
        pos_statis(i) = pos_statis(i) + 1
     enddo
     write(6,*) 'READ_', trim(obstype), ': after obsolete_obs near edges, ndata ', sum(pos_statis)
     deallocate(npos_all)
     deallocate(pos_statis)
  endif ! use_edges, but flag part of obs at the scan edges with negative FOV values.
!=========================================================================================================

! Allow single task to check for bad obs, update superobs sum,
! and write out data to scratch file for further processing.
  if (mype_sub==mype_root.and.ndata>0) then

!    Identify "bad" observation (unreasonable brightness temperatures).
!    Update superobs sum according to observation location

     do n=1,ndata
        do i=1,nchanl
           if(data_all(i+nreal,n) > tbmin(i) .and. &
              data_all(i+nreal,n) < tbmax)nodata=nodata+1
        end do
     end do

     if(dval_use .and. assim)then
        do n=1,ndata
           itt=nint(data_all(maxinfo,n))
           super_val(itt)=super_val(itt)+val_gmi 
        end do
     endif

!    Write final set of "best" observations to output file
     call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif

! Deallocate data arrays
  deallocate(nrec)
  deallocate(data_all)


! Deallocate satthin arrays
1000 continue
  call destroygrids
  call clean_

  if(diagnostic_reg .and. ntest>0 .and. mype_sub==mype_root) &
     write(6,*)'READ_GMI:  mype,ntest,disterrmax=',&
        mype,ntest,disterrmax

! End of routine
 return
 contains
 subroutine init_(maxchanl,maxobs)
  integer(i_kind),intent(in) :: maxchanl,maxobs
  allocate(ifov_save(maxobs))
  allocate(iscan_save(maxobs))
  allocate(iorbn_save(maxobs))
  allocate(inode_save(maxobs))
  allocate(dlon_earth_save(maxobs))
  allocate(dlat_earth_save(maxobs))
  allocate(sat_zen_ang_save(maxobs),sat_azimuth_ang_save(maxobs),sat_scan_ang_save(maxobs))
  allocate(sat_zen_ang2_save(maxobs),sat_azimuth_ang2_save(maxobs),sat_scan_ang2_save(maxobs))
  allocate(t4dv_save(maxobs))
  allocate(crit1_save(maxobs))
  allocate(it_mesh_save(maxobs))
  allocate(tbob_save(maxchanl,maxobs))
  allocate(sun_zenith_save(maxobs),sun_azimuth_ang_save(maxobs))
 end subroutine init_
 subroutine clean_
  deallocate(sun_zenith_save,sun_azimuth_ang_save)
  deallocate(tbob_save)
  deallocate(crit1_save)
  deallocate(it_mesh_save)
  deallocate(t4dv_save)
  deallocate(sat_zen_ang2_save,sat_azimuth_ang2_save,sat_scan_ang2_save)
  deallocate(sat_zen_ang_save,sat_azimuth_ang_save,sat_scan_ang_save)
  deallocate(dlat_earth_save)
  deallocate(dlon_earth_save)
  deallocate(inode_save)
  deallocate(iorbn_save)
  deallocate(iscan_save)
  deallocate(ifov_save)
 end subroutine clean_

end subroutine read_gmi


