subroutine read_amsr2(mype,val_amsr2,ithin,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs)

! subprogram:    read_amsr2                  read bufr format amsr2 data
!   prgmmr: ejones         copied from read_amsre.f90         date: 2014-03-15
! abstract:  This routine reads BUFR format AMSR2 radiance (brightness
!            temperature) files that contain the first 14 channels of AMSR2 
!            (all at the same resolution).
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2014-03-15  ejones   - read amsr2
!   2015-09-17  Thomas   - add l4densvar and thin4d to data selection procedure
!   2015-09-30  ejones   - modify solar angle info passed for calculating
!                          sunglint in QC routine, get rid of old sun glint calc
!   2016-03-11  j. guo   - Fixed {dlat,dlon}_earth_deg in the obs data stream
!   2016-03-21  ejones   - add spatial averaging capability (use SSMI/S spatial averaging)
!   2016-05-05  ejones   - remove isfcalc; no procedure exists for this for
!                          AMSR2
!   2016-07-25  ejones   - made most allocatable arrays static
!   2016-09-20  j. guo   - Refixed dlxx_earth_deg, for the new dlxx_earth_save(:).
!   2017-01-03  todling  - treat save arrays as allocatable
!   2018-05-21  j.jin    - added time-thinning. Moved the checking of thin4d into satthin.F90.
! 
!
! input argument list:
!     mype     - mpi task id
!     val_amsre- weighting factor applied to super obs
!     ithin    - flag to thin data
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
!
! output argument list:
!     nread    - number of BUFR AMSR2 observations read
!     ndata    - number of BUFR AMSR2 profiles retained for further processing
!     nodata   - number of BUFR AMSR2 observations retained for further processing
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
  use radinfo, only: iuse_rad,nusis,jpch_rad,amsr2_method 
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,rlats,rlons,&
      tll2xy
  use constants, only: deg2rad,zero,one,three,r60inv,two
  use gsi_4dvar, only: l4dvar, iwinbgn, winlen, l4densvar
  use calc_fov_conical, only: instrument_init
  use deter_sfc_mod, only: deter_sfc_fov,deter_sfc
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter
  use ssmis_spatial_average_mod, only : ssmis_spatial_average
  use m_sortind
  use mpimod, only: npe
! use radiance_mod, only: rad_obs_type

  implicit none

! Input variables
  character(len=*) ,intent(in   ) :: infile
  character(len=*) ,intent(in   ) :: obstype,jsatid
  integer(i_kind)  ,intent(in   ) :: mype
  integer(i_kind)  ,intent(in   ) :: ithin
  integer(i_kind)  ,intent(in   ) :: lunout
  real(r_kind)     ,intent(inout) :: val_amsr2
  real(r_kind)     ,intent(in   ) :: gstime,twind
  real(r_kind)     ,intent(in   ) :: rmesh
  character(len=20),intent(in   ) :: sis
  integer(i_kind)  ,intent(in   ) :: mype_root
  integer(i_kind)  ,intent(in   ) :: mype_sub
  integer(i_kind)  ,intent(in   ) :: npe_sub
  integer(i_kind)  ,intent(in   ) :: mpi_comm_sub

! Output variables
  integer(i_kind)  ,intent(inout) :: nread
  integer(i_kind)  ,intent(inout) :: ndata,nodata
integer(i_kind),dimension(npe)  ,intent(inout) :: nobs

! Number of channels for sensors in BUFR
  integer(i_kind),parameter :: N_AMSRCH  =  14  ! only channels 1-14 processed
  integer(i_kind) :: said, bufsat = 122 !WMO sat id 
  integer(i_kind) :: siid, AMSR2_SIID = 478   !WMO instrument identifier 
  integer(i_kind),parameter :: maxinfo    =  33

! BUFR file sequencial number
  character(len=8)  :: subset
  character(len=4)  :: senname
  integer(i_kind)   :: lnbufr = 10
  integer(i_kind)   :: nchanl
  integer(i_kind)   :: iret,isflg,idomsfc(1)
  integer(i_kind),parameter  :: kchanl=14

! Work variables for time
  integer(i_kind)   :: idate
  integer(i_kind)   :: idate5(5)
  integer(i_kind)   :: nmind
  real(r_kind)      :: sstime, tdiff   

! Other work variables
  logical           :: outside,iuse,assim
  logical           :: do_noise_reduction
  integer(i_kind)   :: nreal, kidsat
  integer(i_kind)   :: itx, nele, itt, k
  integer(i_kind)   :: ilat, ilon   
  integer(i_kind)   :: i, l, n
  integer(i_kind),dimension(n_amsrch) :: kchamsr2
  real(r_kind)     :: sfcr
  real(r_kind)     :: dlon, dlat
  real(r_kind)     :: dist1   
  real(r_kind),allocatable,dimension(:,:):: data_all
  integer(i_kind),allocatable,dimension(:)::nrec
  integer(i_kind):: irec,next
  integer(i_kind):: method,iobs,num_obs   
  integer(i_kind),parameter  :: maxobs=2e7

  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),dimension(0:3):: ts
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr

  character(len=7),parameter:: fov_flag="conical"
  
  real(r_kind),allocatable        :: relative_time_in_seconds(:)

  real(r_kind) :: dlat_earth_deg, dlon_earth_deg
  real(r_kind),pointer :: t4dv,dlon_earth,dlat_earth,crit1
  real(r_kind),pointer :: sat_zen_ang,sat_az_ang    
  real(r_kind),pointer :: sun_zen_ang,sun_az_ang
  real(r_kind),pointer :: tbob(:)

  integer(i_kind),pointer :: ifov,iscan,iorbn,inode    

  integer(i_kind),allocatable        :: sorted_index(:)
  integer(i_kind),target,allocatable,dimension(:) :: ifov_save
  integer(i_kind),target,allocatable,dimension(:) :: iscan_save
  integer(i_kind),target,allocatable,dimension(:) :: iorbn_save
  integer(i_kind),target,allocatable,dimension(:) :: inode_save
  integer(i_kind),target,allocatable,dimension(:) :: it_mesh_save
  real(r_kind),target,allocatable,dimension(:) :: dlon_earth_save
  real(r_kind),target,allocatable,dimension(:) :: dlat_earth_save
  real(r_kind),target,allocatable,dimension(:) :: sat_zen_ang_save,sat_az_ang_save
  real(r_kind),target,allocatable,dimension(:) :: sun_zen_ang_save,sun_az_ang_save
  real(r_kind),target,allocatable,dimension(:) :: t4dv_save
  real(r_kind),target,allocatable,dimension(:) :: crit1_save
  real(r_kind),target,allocatable,dimension(:,:) :: tbob_save

! Set standard parameters
  integer(i_kind) ntest
  integer(i_kind) :: nscan,iskip,kskip,kch  
  real(r_kind),parameter :: R90      =  90._r_kind
  real(r_kind),parameter :: R360     = 360._r_kind
  real(r_kind),parameter :: tbmin    = 3._r_kind           
  real(r_kind),parameter :: tbmax    = 340._r_kind         

  real(r_kind) :: clath, clonh, sun_el_ang, fovn         

! BUFR format for AMSRSPOT
  integer(i_kind),parameter :: N_AMSRSPOT_LIST = 13

! BUFR format for AMSRCHAN
  integer(i_kind),parameter :: N_AMSRCHAN_LIST = 3

! Variables for BUFR IO
  real(r_double),dimension(4):: gcomspot_d
  real(r_double),dimension(13):: amsrspot_d               
  real(r_double),dimension(3,14):: amsrchan_d             

  integer(i_kind) :: ireadsb, ireadmg 
  real(r_kind),parameter:: one_minute=0.01666667_r_kind
  real(r_kind),parameter:: minus_one_minute=-0.01666667_r_kind

  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin
  integer(i_kind),pointer:: it_mesh => null()
! ----------------------------------------------------------------------
! Initialize variables

  call init_(kchanl,maxobs)
  do_noise_reduction = .true.
  if (amsr2_method == 0) do_noise_reduction = .false.

  ilon = 3
  ilat = 4

  if (nst_gsi > 0 ) then
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif

  ntest = 0
  nreal = maxinfo+nstinfo
  ndata = 0
  nodata = 0
  nread = 0
  sstime = zero
  kchamsr2(1:14)=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14/)

  senname = 'AMSR'
  nchanl  = N_AMSRCH
  nscan  = 243  
  kidsat = 549
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
  if (.not.assim) val_amsr2=zero

  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif
! Make thinning grids
  call makegrids(rmesh,ithin,n_tbin=n_tbin)

  inode_save = 0

! Open BUFR file
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)

! Big loop to read data file
  next=0
  irec=0
  iobs=1
  do while(ireadmg(lnbufr,subset,idate)>=0)
     irec=irec+1
     next=next+1
     if(next == npe_sub)next=0
     if(next /= mype_sub)cycle
     read_loop: do while (ireadsb(lnbufr)==0)

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
        sat_az_ang          => sat_az_ang_save(iobs)
        sun_zen_ang         => sun_zen_ang_save(iobs)
        sun_az_ang          => sun_az_ang_save(iobs)

!    Retrieve bufr 1/4 :get gcomspot (said,orbn,sun_az_ang,sun_el_ang)
        call ufbint(lnbufr,gcomspot_d,4,1,iret,'SAID ORBN SOLAZI SOEL')    

        said = nint(gcomspot_d(1))
        if(said /= bufsat)    cycle read_loop

        iorbn = nint(gcomspot_d(2))
        sun_az_ang = gcomspot_d(3)

!       Retrieve bufr 2/4 :get amsrspot (siid,ymdhs,lat,lon,angles,fov,scanl)
        call ufbrep(lnbufr,amsrspot_d,N_AMSRSPOT_LIST,1,iret, &
           'SIID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH AANG IANG FOVN SLNM')

        siid = nint(amsrspot_d(1)) 
        if(siid /= AMSR2_SIID)   cycle read_loop

        fovn = amsrspot_d(12)
        iscan = amsrspot_d(13)
        
        ifov = nint(fovn)
        sat_az_ang = amsrspot_d(10)
        sat_zen_ang = 55.0_r_kind*deg2rad


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
            write(6,*)'READ_AMSR2:  ### ERROR IN READING BUFR DATA:', &
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

        crit0 = 0.01_r_kind
        timeinflat=6.0_r_kind
        call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)

!     --- Check observing position -----
        clath= amsrspot_d(08)
        clonh= amsrspot_d(09)

        if( abs(clath) > R90  .or. abs(clonh) > R360 .or. &
          ( abs(clath) == R90 .and. clonh /= ZERO) )  then
           write(6,*)'READ_AMSR2:  ### ERROR IN READING BUFR DATA:',&
              ' STRANGE OBS POINT (LAT,LON):', clath, clonh
              cycle read_loop
        endif

     
!    Set position in a given region
        if(clonh >= R360)then
           clonh = clonh - R360
        else if(clonh < ZERO)then
           clonh = clonh + R360
        endif
      
!    If regional, map obs lat,lon to rotated grid.
        dlat_earth = clath !* deg2rad
        dlon_earth = clonh !* deg2rad

!!       Retrieve bufr 3/4 : get amsrchan 
        call ufbrep(lnbufr,amsrchan_d,3,14,iret,'SCCF ACQF TMBR')   

!       Set check for TBs outside of limits
        iskip = 0
        do l=1,nchanl
           if(amsrchan_d(3,l)<tbmin .or. amsrchan_d(3,l)>tbmax)then
              iskip = iskip + 1
           end if
        end do
        kskip = 0
        do l=1,kchanl
           kch=kchamsr2(l)
           if(amsrchan_d(3,kch)<tbmin .or. amsrchan_d(3,kch)>tbmax)then
              kskip = kskip + 1
           else
              nread=nread+1
           endif
        end do
        if(kskip == kchanl .or. iskip == nchanl) cycle read_loop

        tbob_save(1,iobs)=amsrchan_d(3,1)
        tbob_save(2,iobs)=amsrchan_d(3,2)
        tbob_save(3,iobs)=amsrchan_d(3,3)
        tbob_save(4,iobs)=amsrchan_d(3,4)
        tbob_save(5,iobs)=amsrchan_d(3,5)
        tbob_save(6,iobs)=amsrchan_d(3,6)
        tbob_save(7,iobs)=amsrchan_d(3,7)
        tbob_save(8,iobs)=amsrchan_d(3,8)
        tbob_save(9,iobs)=amsrchan_d(3,9)
        tbob_save(10,iobs)=amsrchan_d(3,10)
        tbob_save(11,iobs)=amsrchan_d(3,11)
        tbob_save(12,iobs)=amsrchan_d(3,12)
        tbob_save(13,iobs)=amsrchan_d(3,13)
        tbob_save(14,iobs)=amsrchan_d(3,14)

        nread=nread+kchanl

        sun_zen_ang = gcomspot_d(3)     !solar azimuth angle
        sun_el_ang = gcomspot_d(4)       !solar elevation angle

!    Check observational info 

        if( sun_el_ang < -180._r_kind .or. sun_el_ang > 180._r_kind )then
           write(6,*)'READ_AMSR2:  ### ERROR IN READING BUFR DATA:', &
              ' STRANGE OBS INFO(FOV,SOLAZI,SOEL):', ifov, sun_az_ang, sun_el_ang
           cycle read_loop       
        endif
!    make solar azimuth angles from -180 to 180 degrees
        if (sun_az_ang > 180.0_r_kind) then
           sun_az_ang=sun_az_ang-360.0_r_kind
        endif

!    calculate solar zenith angle (used in QC for sun glint)
        sun_zen_ang = 90.0_r_kind - sun_el_ang

!       check to make sure sun zenith is between 0 and 180
        if (sun_zen_ang < 0.0_r_kind) then
          sun_zen_ang=90.0_r_kind-sun_zen_ang
        endif
        sat_zen_ang = amsrspot_d(11)*deg2rad    ! satellite zenith/incidence angle(rad)

        iobs=iobs+1
        if (iobs > maxobs) then
           write(6,*)'read_amsr2:  ***ERROR*** iobs= ',iobs,' > maxobs= ',maxobs
           call stop2(56)
        endif

     enddo read_loop
  enddo
  call closbf(lnbufr)
  close(lnbufr)

  num_obs=iobs-1

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
     sat_az_ang_save(1:num_obs)          = sat_az_ang_save(sorted_index)
     sun_zen_ang_save(1:num_obs)         = sun_zen_ang_save(sorted_index)
     sun_az_ang_save(1:num_obs)          = sun_az_ang_save(sorted_index)
     tbob_save(:,1:num_obs)              = tbob_save(:,sorted_index)

!    Do spatial averaging using SSMIS spatial averaging

     method = amsr2_method
     write(6,*) 'READ_AMSR2: Calling ssmis_spatial_average, method =', method

     call ssmis_spatial_average(bufsat,method,num_obs,nchanl, &
                                ifov_save,inode_save,relative_time_in_seconds,&
                                dlat_earth_save,dlon_earth_save, &
                                tbob_save(1:nchanl,1:num_obs),iret)  ! inout

     if (iret /= 0) then
        write(6,*) 'Error calling ssmis_spatial_average from READ_AMSR2'
        return
     endif

     if (num_obs > 0) then
        deallocate(sorted_index)
        deallocate(relative_time_in_seconds)
     endif

  endif ! do_noise_reduction

!========================================================================================================================

! Complete thinning for AMSR2
! Write header record to scratch file.  Also allocate array
! to hold all data for given satellite
  nreal  = maxinfo + nstinfo
  nele   = nreal   + nchanl
  allocate(data_all(nele,itxmax),nrec(itxmax))

  nrec=999999
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
     sat_az_ang          => sat_az_ang_save(iobs)
     sun_zen_ang             => sun_zen_ang_save(iobs)
     sun_az_ang          => sun_az_ang_save(iobs)
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
    tdiff=t4dv+(iwinbgn-gstime)*r60inv
    if (l4dvar) then
        if (t4dv<zero .OR. t4dv>winlen) cycle obsloop
    else
        if(abs(tdiff) > twind) cycle obsloop
    endif

!   Map obs to thinning grid
    call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)
    if(.not. iuse) then
      cycle obsloop
    endif

!   Check TBs again
    iskip = 0
    do l=1,nchanl
        if(tbob(l)<tbmin .or. tbob(l)>tbmax)then
           iskip = iskip + 1
        end if
    end do
    kskip = 0
    do l=1,kchanl
        kch=kchamsr2(l)
        if(tbob(kch)<tbmin .or. tbob(kch)>tbmax)then
           kskip = kskip + 1
        endif
    end do
    if(kskip == kchanl .or. iskip == nchanl) cycle obsloop

!   if the obs is far from the grid box center, do not use it.
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

    call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc(1),sfcpct, &
         ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)

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

!       interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
        if ( nst_gsi > 0 ) then
           tref  = ts(0)
           dtw   = zero
           dtc   = zero
           tz_tr = one
           if ( sfcpct(0) > zero ) then
             call gsi_nstcoupler_deter(dlat_earth,dlon_earth,t4dv,zob,tref,dtw,dtc,tz_tr)
           endif
        endif

        data_all(1,itx) = bufsat                     ! satellite ID
        data_all(2,itx) = t4dv                       ! time diff (obs - anal) (hours)
        data_all(3,itx) = dlon                       ! grid relative longitude
        data_all(4,itx) = dlat                       ! grid relative latitude
        data_all(5,itx) = sat_zen_ang                ! satellite zenith angle (rad)
        data_all(6,itx) = sat_az_ang                 ! satellite azimuth angle
        data_all(7,itx) = zero                       ! look angle (rad)
        data_all(8,itx) = ifov                       ! scan position
        data_all(9,itx) = sun_zen_ang                ! solar zenith angle (deg)
        data_all(10,itx)= sun_az_ang                 ! solar azimuth angle (deg)
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

        data_all(32,itx)= val_amsr2
        data_all(33,itx)= itt

        if ( nst_gsi > 0 ) then
           data_all(maxinfo+1,itx) = tref                ! foundation temperature
           data_all(maxinfo+2,itx) = dtw                 ! dt_warm at zob
           data_all(maxinfo+3,itx) = dtc                 ! dt_cool at zob
           data_all(maxinfo+4,itx) = tz_tr               ! d(Tz)/d(Tr)
        endif

        do l=1,nchanl
           data_all(l+nreal,itx) = tbob(l)
        end do
        nrec(itx)=irec

     enddo obsloop

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
        itt=nint(data_all(maxinfo,n))
        super_val(itt)=super_val(itt)+val_amsr2

     end do

!    Write final set of "best" observations to output file
     call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  endif

  call clean_
  deallocate(data_all,nrec) ! Deallocate data arrays
  call destroygrids    ! Deallocate satthin arrays
  if(diagnostic_reg.and.ntest>0 .and. mype_sub==mype_root) &
     write(6,*)'READ_AMSR2:  ',&
        'mype,ntest=',mype,ntest

  return

  contains
  subroutine init_(kchanl,maxobs)
  integer(i_kind),intent(in) :: kchanl,maxobs

  allocate(ifov_save(maxobs))
  allocate(iscan_save(maxobs))
  allocate(iorbn_save(maxobs))
  allocate(inode_save(maxobs))
  allocate(dlon_earth_save(maxobs))
  allocate(dlat_earth_save(maxobs))
  allocate(sat_zen_ang_save(maxobs),sat_az_ang_save(maxobs))
  allocate(sun_zen_ang_save(maxobs),sun_az_ang_save(maxobs))
  allocate(t4dv_save(maxobs))
  allocate(crit1_save(maxobs))
  allocate(it_mesh_save(maxobs))
  allocate(tbob_save(kchanl,maxobs))

  end subroutine init_

  subroutine clean_

  deallocate(tbob_save)
  deallocate(crit1_save)
  deallocate(it_mesh_save)
  deallocate(t4dv_save)
  deallocate(sun_zen_ang_save,sun_az_ang_save)
  deallocate(sat_zen_ang_save,sat_az_ang_save)
  deallocate(dlat_earth_save)
  deallocate(dlon_earth_save)
  deallocate(inode_save)
  deallocate(iorbn_save)
  deallocate(iscan_save)
  deallocate(ifov_save)

  end subroutine clean_

end subroutine read_amsr2

