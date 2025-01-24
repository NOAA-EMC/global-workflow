subroutine read_atms(mype,val_tovs,ithin,isfcalc,&
     rmesh,jsatid,gstime,infile,lunout,obstype,&
     nread,ndata,nodata,twind,sis, &
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs, &
     nrec_start,nrec_start_ears,nrec_start_db,dval_use,radmod)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_atms                  read atms 1b data
!   prgmmr: collard          org: np23                date: 2011-12-07
!
! abstract:  This routine reads BUFR format ATMS radiance 
!            (brightness temperature) files. Optionally the data
!            are filtered using the AAPP filtering code. This requires
!            This code to differ from read_bufrtovs in that all the
!            data needs to be read in together before it is processed further. 
!
!            Also optionally, the data 
!            are thinned to a specified resolution using simple 
!            quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!  2011-12-06  Original version based on r16656 version of read_bufrtovs.  A. Collard
!  2012-03-05  akella  - nst now controlled via coupler
!  2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!  2013-12-20  eliu - change icw4crtm>0 to icw4crtm>10 (bug fix))
!  2014-01-31  mkim - add iql4crtm and set qval= 0 for all-sky mw data assimilation
!  2015-02-23  Rancic/Thomas - add thin4d to time window logical
!  2015-08-20  zhu - add radmod for all-sky and aerosol usages in radiance assimilation
!  2016-04-28  jung - added logic for RARS and direct broadcast from NESDIS/UW
!  2016-10-20  collard - fix to allow monitoring and limited assimilation of spectra when key 
!                         channels are missing.
!  2016-10-25  zhu - add changes for assimilating radiances affected by non-precipitating clouds
!  2018-02-05  collard - get orbit height from BUFR file
!  2018-04-19  eliu - allow data selection for precipitation-affected data 
!  2018-05-21  j.jin  - added time-thinning, to replace thin4d
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
!     nrec_start - first subset with useful information
!     nrec_start_ears - first ears subset with useful information
!     nrec_start_db - first db subset with useful information
!
!   output argument list:
!     nread    - number of BUFR ATMS 1b observations read
!     ndata    - number of BUFR ATMS 1b profiles retained for further processing
!     nodata   - number of BUFR ATMS 1b observations retained for further processing
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
  use obsmod,  only: time_window_max, ta2tb
  use radinfo, only: iuse_rad,newchn,cbias,nusis,jpch_rad,air_rad,ang_rad, &
      use_edges,radedge1,radedge2,nusis,radstart,radstep,newpc4pred,maxscan
  use radinfo, only: adp_anglebc
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,tll2xy,txy2ll,rlats,rlons
  use constants, only: deg2rad,zero,one,two,three,rad2deg,r60inv,r100,rearth_equator
  use crtm_module, only : max_sensor_zenith_angle
  use calc_fov_crosstrk, only : instrument_init, fov_cleanup, fov_check
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use deter_sfc_mod, only: deter_sfc_fov,deter_sfc
  use atms_spatial_average_mod, only : atms_spatial_average
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth,gsi_nstcoupler_deter
  use mpimod, only: npe
  use radiance_mod, only: rad_obs_type

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
  logical         ,intent(in   ) :: dval_use
  type(rad_obs_type),intent(in ) :: radmod

! Declare local parameters

  character(8),parameter:: fov_flag="crosstrk"
  integer(i_kind),parameter:: n1bhdr=13
  integer(i_kind),parameter:: n2bhdr=4
  integer(i_kind),parameter:: maxobs = 800000
  integer(i_kind),parameter:: max_chanl = 22
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind
  ! The next two are one minute in hours
  real(r_kind),parameter:: one_minute=0.01666667_r_kind
  real(r_kind),parameter:: minus_one_minute=-0.01666667_r_kind

! Declare local variables
  logical outside,iuse,assim,valid

  character(40) :: infile2
  character(8) subset
  character(80) hdr1b,hdr2b

  integer(i_kind) ireadsb,ireadmg,nrec_startx
  integer(i_kind) i,j,k,ntest,iob,llll
  integer(i_kind) iret,idate,nchanl,n,idomsfc(1)
  integer(i_kind) ich1,ich2,ich8,ich15,ich16,ich17
  integer(i_kind) kidsat,maxinfo
  integer(i_kind) nmind,itx,nreal,nele,itt,num_obs
  integer(i_kind) iskip,ichan2,ichan1,ichan16,ichan17
  integer(i_kind) lnbufr,ksatid,isflg,ichan3,ich3,ich4,ich6
  integer(i_kind) ilat,ilon, ifovmod, nadir
  integer(i_kind),dimension(5):: idate5
  integer(i_kind) instr,ichan
  integer(i_kind):: ierr
  integer(i_kind):: radedge_min, radedge_max
  integer(i_kind), POINTER :: ifov
  integer(i_kind), TARGET :: ifov_save(maxobs)
  integer(i_kind), ALLOCATABLE :: IScan(:)

  real(r_kind) cosza,sfcr
  real(r_kind) ch1,ch2,ch3,d0,d1,d2,ch16,qval
  real(r_kind) expansion
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr
  real(r_kind) :: satellite_height, rato

  real(r_kind) pred
  real(r_kind) dlat,panglr,dlon,tdiff
  real(r_kind) dlon_earth_deg,dlat_earth_deg,r01
  real(r_kind) step,start,dist1
  real(r_kind) tt,lzaest
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),allocatable :: Relative_Time_In_Seconds(:)
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind), POINTER :: bt_in(:), crit1,rsat, t4dv, solzen, solazi
  real(r_kind), POINTER :: dlon_earth,dlat_earth,satazi, lza

  integer(i_kind), ALLOCATABLE, TARGET :: it_mesh_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: rsat_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: t4dv_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: dlon_earth_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: dlat_earth_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: crit1_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: lza_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: satazi_save(:)
  real(r_kind), ALLOCATABLE, TARGET :: solzen_save(:) 
  real(r_kind), ALLOCATABLE, TARGET :: solazi_save(:) 
  real(r_kind), ALLOCATABLE, TARGET :: bt_save(:,:)

  integer(i_kind),allocatable,dimension(:):: nrec
  real(r_double),allocatable,dimension(:):: data1b8
  real(r_double),dimension(n1bhdr):: bfr1bhdr
  real(r_double),dimension(n2bhdr):: bfr2bhdr

  real(r_kind) cdist,disterr,disterrmax,dlon00,dlat00

  logical :: critical_channels_missing
  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin
  integer(i_kind),pointer :: it_mesh => null()

!**************************************************************************
! Initialize variables

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
     call gsi_nstcoupler_skindepth(obstype,zob)
  endif

  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif
! Make thinning grids
  call makegrids(rmesh,ithin,n_tbin=n_tbin)

! Set nadir position based on value of maxscan
  if (maxscan < 96) then
     ! For ATMS when using the old style satang files, 
     ! we shift the FOV number down by three as we can only use
     ! 90 of the 96 positions right now because of the scan bias limitation.
     nadir=45
  else
     nadir=48
  endif

! Set various variables depending on type of data to be read

  if (obstype /= 'atms') then
     write(6,*) 'READ_ATMS called for obstype '//obstype//': RETURNING'
     return
  end if

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

  ichan16 = newchn(sis,ich16)
  ichan17 = newchn(sis,ich17)

  if(jsatid == 'npp') then
     kidsat=224
  elseif (jsatid == 'n20') then
     kidsat = 225
  elseif (jsatid == 'n21') then
     kidsat = 226
  else 
     write(6,*) 'READ_ATMS: Unrecognized value for jsatid '//jsatid//': RETURNING'
     return
  end if

  radedge_min = 0
  radedge_max = 1000
  do i=1,jpch_rad
     if (trim(nusis(i))==trim(sis)) then
        step  = radstep(i)
        start = radstart(i)
        if (radedge1(i)/=-1 .or. radedge2(i)/=-1) then
           radedge_min=radedge1(i)
           radedge_max=radedge2(i)
        end if
        exit 
     endif
  end do 

! Allocate arrays to hold all data for given satellite
  nchanl=22
  if(dval_use) maxinfo = maxinfo+2
  nreal = maxinfo + nstinfo
  nele  = nreal   + nchanl
  allocate(data_all(nele,itxmax),nrec(itxmax))
  nrec=999999

! IFSCALC setup
  if (isfcalc==1) then
     instr=20                    
     ichan=16                    ! pick a surface sens. channel
     expansion=2.9_r_kind        ! use almost three for microwave sensors.
  endif
!   Set rlndsea for types we would prefer selecting
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
  if (.not.assim) val_tovs=zero

! Initialize variables for use by FOV-based surface code.
  if (isfcalc == 1) then
     call instrument_init(instr,jsatid,expansion,valid)
     if (.not. valid) then
       if (assim) then
         write(6,*)'READ_ATMS:  ***ERROR*** IN SETUP OF FOV-SFC CODE. STOP'
         call stop2(71)
       else
         call fov_cleanup
         isfcalc = 0
         write(6,*)'READ_ATMS:  ***ERROR*** IN SETUP OF FOV-SFC CODE'
       endif
     endif
  endif

! This eventually needs to be spit between MHS and AMSU-A like channels
  if (isfcalc==1) then
!   if (amsub.or.mhs)then
!     rlndsea(4) = max(rlndsea(0),rlndsea(1),rlndsea(2),rlndsea(3))
!   else
      rlndsea=0
!   endif
  endif

! Allocate arrays for BUFR I/O
  ALLOCATE(data1b8(nchanl))
  ALLOCATE(rsat_save(maxobs))
  ALLOCATE(t4dv_save(maxobs))
  ALLOCATE(dlon_earth_save(maxobs))
  ALLOCATE(dlat_earth_save(maxobs))
  ALLOCATE(crit1_save(maxobs))
  ALLOCATE(it_mesh_save(maxobs))
  ALLOCATE(lza_save(maxobs))
  ALLOCATE(satazi_save(maxobs))
  ALLOCATE(solzen_save(maxobs)) 
  ALLOCATE(solazi_save(maxobs)) 
  ALLOCATE(bt_save(max_chanl,maxobs))

  iob=1
! Big loop over standard data feed and possible rars/db data
! llll=1 normal feed, llll=2 RARS/EARS data, llll=3 DB/UW data
  ears_db_loop: do llll= 1, 3

     if(llll == 1)then
        nrec_startx = nrec_start
        infile2 = trim(infile)         ! Set bufr subset names based on type of data to read
     elseif(llll == 2) then
        nrec_startx = nrec_start_ears
        infile2 = trim(infile)//'ears' ! Set bufr subset names based on type of data to read
     elseif(llll == 3) then
        nrec_startx = nrec_start_db
        infile2 = trim(infile)//'_db'  ! Set bufr subset names based on type of data to read
     end if

!    Reopen unit to satellite bufr file
     open(lnbufr,file=trim(infile2),form='unformatted',status = 'old', &
         iostat = ierr)
     if(ierr /= 0) cycle ears_db_loop

     call openbf(lnbufr,'IN',lnbufr)
     hdr1b ='SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON CLATH CLONH HMSL'
     hdr2b ='SAZA SOZA BEARAZ SOLAZI'
   
!    Loop to read bufr file
     read_subset: do while(ireadmg(lnbufr,subset,idate)>=0 .AND. iob < maxobs)

        read_loop: do while (ireadsb(lnbufr)==0 .and. iob < maxobs)

           rsat       => rsat_save(iob)
           t4dv       => t4dv_save(iob)
           dlon_earth => dlon_earth_save(iob)
           dlat_earth => dlat_earth_save(iob)
           crit1      => crit1_save(iob)
           it_mesh    => it_mesh_save(iob)
           ifov       => ifov_save(iob)
           lza        => lza_save(iob)
           satazi     => satazi_save(iob)
           solzen     => solzen_save(iob)
           solazi     => solazi_save(iob)

!          inflate selection value for ears_db data
           crit0 = 0.01_r_kind
           if ( llll > 1 ) crit0 = crit0 + r100 * real(llll,r_kind)

           call ufbint(lnbufr,bfr1bhdr,n1bhdr,1,iret,hdr1b)

!          Extract satellite id.  If not the one we want, read next record
           rsat=bfr1bhdr(1) 
           ksatid=nint(bfr1bhdr(1))
           if(ksatid /= kidsat) cycle read_subset

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


!          Extract date information.  If time outside window, skip this obs
           idate5(1) = bfr1bhdr(3) !year
           idate5(2) = bfr1bhdr(4) !month
           idate5(3) = bfr1bhdr(5) !day
           idate5(4) = bfr1bhdr(6) !hour
           idate5(5) = bfr1bhdr(7) !minute
           call w3fs21(idate5,nmind)
           t4dv= (real((nmind-iwinbgn),r_kind) + bfr1bhdr(8)*r60inv)*r60inv    ! add in seconds
           tdiff=t4dv+(iwinbgn-gstime)*r60inv

           if (l4dvar.or.l4densvar) then
              if (t4dv<minus_one_minute .OR. t4dv>winlen+one_minute) &
                  cycle read_loop
           else
              if(abs(tdiff) > twind+one_minute) cycle read_loop
           endif

           timeinflat=two
           call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)
 
           call ufbint(lnbufr,bfr2bhdr,n2bhdr,1,iret,hdr2b)

           satazi=bfr2bhdr(3)
           if (abs(satazi) > r360) then
              satazi=zero
           endif

           ifov = nint(bfr1bhdr(2))
           lza = bfr2bhdr(1)*deg2rad      ! local zenith angle
           if(ifov <= 48)    lza=-lza

           panglr=(start+real(ifov-1,r_kind)*step)*deg2rad
           satellite_height=bfr1bhdr(13)
!          Ensure orbit height is reasonable
           if (satellite_height < 780000.0_r_kind .OR. &
              satellite_height > 900000.0_r_kind) satellite_height = 824000.0_r_kind
           rato = one + satellite_height/rearth_equator
           lzaest = asin(rato*sin(panglr))

           if(abs(lza)*rad2deg > MAX_SENSOR_ZENITH_ANGLE) then
              write(6,*)'READ_ATMS WARNING lza error ',lza,panglr
              cycle read_loop
           end if

!          Check for errors in satellite zenith angles 
           if(abs(lzaest-lza)*rad2deg > one) then
              write(6,*)' READ_ATMS WARNING uncertainty in lza ', &
              lza*rad2deg,lzaest*rad2deg,sis,ifov,start,step
              cycle read_loop
           end if

           solzen_save(iob)=bfr2bhdr(2) 
           solazi_save(iob)=bfr2bhdr(4) 

!          Read data record.  Increment data counter
!          TMBR is actually the antenna temperature for most microwave sounders but for
!          ATMS it is stored in TMANT.
!          ATMS is assumed not to come via EARS
           if (ta2tb) then
              call ufbrep(lnbufr,data1b8,1,nchanl,iret,'TMBR')
           else
              call ufbrep(lnbufr,data1b8,1,nchanl,iret,'TMANT')
           endif

           bt_save(1:nchanl,iob) = data1b8(1:nchanl)

           iob=iob+1

        end do read_loop
     end do read_subset
     call closbf(lnbufr)
     close(lnbufr)
  end do ears_db_loop
  deallocate(data1b8)

  num_obs = iob-1

  if (num_obs <= 0) then
     write(6,*) 'READ_ATMS: No ATMS Data were read in'
     return
  end if

! Call filtering code 

  ALLOCATE(Relative_Time_In_Seconds(Num_Obs))
  ALLOCATE(IScan(Num_Obs))
  Relative_Time_In_Seconds = 3600.0_r_kind*T4DV_Save(1:Num_Obs)
! write(6,*) 'Calling ATMS_Spatial_Average'
  CALL ATMS_Spatial_Average(Num_Obs, NChanl, IFOV_Save(1:Num_Obs), &
       Relative_Time_In_Seconds, BT_Save(1:nchanl,1:Num_Obs), IScan, IRet)
! write(6,*) 'ATMS_Spatial_Average Called with IRet=',IRet
  DEALLOCATE(Relative_Time_In_Seconds)
  
  IF (IRet /= 0) THEN
     write(6,*) 'Error Calling ATMS_Spatial_Average from READ_ATMS'
     RETURN
  END IF

! Complete Read_ATMS thinning and QC steps

  ObsLoop: do iob = 1, num_obs  

     rsat       => rsat_save(iob)
     t4dv       => t4dv_save(iob)
     dlon_earth => dlon_earth_save(iob)
     dlat_earth => dlat_earth_save(iob)
     crit1      => crit1_save(iob)
     it_mesh    => it_mesh_save(iob)
     ifov       => ifov_save(iob)
     lza        => lza_save(iob)
     satazi     => satazi_save(iob)
     solzen     => solzen_save(iob)
     solazi     => solazi_save(iob)
     bt_in      => bt_save(1:nchanl,iob)
     
     dlat_earth_deg = dlat_earth
     dlon_earth_deg = dlon_earth
     dlat_earth = dlat_earth*deg2rad
     dlon_earth = dlon_earth*deg2rad   

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
        if(outside) cycle ObsLoop
           
!    Global case
     else
        dlat=dlat_earth
        dlon=dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif

! Check time window
     if (l4dvar.or.l4densvar) then
        if (t4dv<zero .OR. t4dv>winlen) cycle ObsLoop
     else
        tdiff=t4dv+(iwinbgn-gstime)*r60inv
        if(abs(tdiff) > twind) cycle ObsLoop
     endif
 
!    Map obs to thinning grid
     call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)
     if(.not. iuse)cycle ObsLoop

!
!    Check FOV and scan-edge usage
     if (.not. use_edges .and. (ifov < radedge_min .OR. ifov > radedge_max )) &
          cycle ObsLoop

     if (maxscan < 96) then
       ! For ATMS when using the old style satang files, 
       ! we shift the FOV number down by three as we can only use
       ! 90 of the 96 positions right now because of the scan bias limitation.
       ifovmod=ifov-3
       ! Check that ifov is not out of range of cbias dimension
       if (ifovmod < 1 .OR. ifovmod > 90) cycle ObsLoop
     else
       ! This line is for consistency with previous treatment
       if (ifov < 4 .OR. ifov > 93) cycle ObsLoop
       ifovmod=ifov
     endif

     nread=nread+nchanl
     
!    Transfer observed brightness temperature to work array.  If any
!    temperature exceeds limits, reset observation to "bad" value
     iskip=0
     critical_channels_missing = .false.
     do j=1,nchanl
        if (bt_in(j) < tbmin .or. bt_in(j) > tbmax) then
           iskip = iskip + 1
           
!          Flag profiles where key channels are bad  
           if((j == ich1 .or. j == ich2 .or. &
                j == ich16 .or. j == ich17)) critical_channels_missing = .true.
        endif
     end do
     if (iskip >= nchanl) cycle ObsLoop

!    Determine surface properties based on 
!    sst and land/sea/ice mask   
!
!    isflg    - surface flag
!               0 sea
!               1 land
!               2 sea ice
!               3 snow
!               4 mixed                       

!    FOV-based surface code requires fov number.  if out-of-range, then
!    skip this ob.

     if (isfcalc == 1) then
        call fov_check(ifov,instr,ichan,valid)
        if (.not. valid) cycle ObsLoop

!    When isfcalc is one, calculate surface fields based on size/shape of fov.
!    Otherwise, use bilinear method.

        call deter_sfc_fov(fov_flag,ifov,instr,ichan,satazi,dlat_earth_deg,&
             dlon_earth_deg,expansion,t4dv,isflg,idomsfc(1), &
             sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
     else
        call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg, &
             idomsfc(1),sfcpct,ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
     endif

     crit1 = crit1 + rlndsea(isflg) + 10._r_kind*real(iskip,r_kind) + 0.01_r_kind * abs(zz)
     call checkob(dist1,crit1,itx,iuse)
     if(.not. iuse)cycle ObsLoop

           if (critical_channels_missing) then

              pred=1.0e8_r_kind

           else

!    Set data quality predictor
!    Simply modify the AMSU-A-Type calculations and use them for all ATMS channels.
!    Remove angle dependent pattern (not mean).
              if (adp_anglebc .and. newpc4pred) then
                 ch1 = bt_in(ich1)-ang_rad(ichan1)*cbias(ifovmod,ichan1)
                 ch2 = bt_in(ich2)-ang_rad(ichan2)*cbias(ifovmod,ichan2)
                 ch16= bt_in(ich16)-ang_rad(ichan16)*cbias(ifovmod,ichan16)
              else
                 ch1 = bt_in(ich1)-ang_rad(ichan1)*cbias(ifovmod,ichan1)+ &
                      air_rad(ichan1)*cbias(nadir,ichan1)
                 ch2 = bt_in(ich2)-ang_rad(ichan2)*cbias(ifovmod,ichan2)+ &
                      air_rad(ichan2)*cbias(nadir,ichan2)   
                 ch16= bt_in(ich16)-ang_rad(ichan16)*cbias(ifovmod,ichan16)+ &
                      air_rad(ichan16)*cbias(nadir,ichan16)
              end if
              if (isflg == 0 .and. ch1<285.0_r_kind .and. ch2<285.0_r_kind) then
                 cosza = cos(lza)
                 if (radmod%lcloud_fwd) then
                    qval=-113.2_r_kind+(2.41_r_kind-0.0049_r_kind*ch1)*ch1 +  &
                               0.454_r_kind*ch2-ch16
                    if (qval>=9.0_r_kind) then
                       qval=1000.0_r_kind*qval
                    else
                       qval  = zero
                    end if
                    if (radmod%lprecip) qval=zero 
                 else 
                    d0    = 8.24_r_kind - 2.622_r_kind*cosza + 1.846_r_kind*cosza*cosza
                    qval  = cosza*(d0+d1*log(285.0_r_kind-ch1)+d2*log(285.0_r_kind-ch2))
                 endif
                 pred  = max(zero,qval)*100.0_r_kind
              else
!          This is taken straight from AMSU-A even though Ch 3 has a different polarisation
!          and ATMS Ch16 is at a slightly different frequency to AMSU-A Ch 15.
                 if (adp_anglebc .and. newpc4pred) then
                    ch3 = bt_in(ich3)-ang_rad(ichan3)*cbias(ifovmod,ichan3)
                 else
                    ch3  = bt_in(ich3)-ang_rad(ichan3)*cbias(ifovmod,ichan3)+ &
                         air_rad(ichan3)*cbias(nadir,ichan3)   
                 end if
                 pred = abs(ch1-ch16)
                 if(ch1-ch16 >= three) then
                    tt   = 168._r_kind-0.49_r_kind*ch16
                    if(ch1 > 261._r_kind .or. ch1 >= tt .or. &
                         (ch16 <= 273._r_kind))then
                       pred = 100._r_kind
                    end if
                 end if
              endif
           end if

!    Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
     crit1 = crit1+pred 
     call finalcheck(dist1,crit1,itx,iuse)
     if(.not. iuse)cycle ObsLoop
     
!    interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
     if(nst_gsi>0) then
        tref  = ts(0)
        dtw   = zero
        dtc   = zero
        tz_tr = one
        if(sfcpct(0)>zero) then
           call gsi_nstcoupler_deter(dlat_earth,dlon_earth,t4dv,zob,tref,dtw,dtc,tz_tr)
        endif
     endif

! Re-calculate look angle
     panglr=(start+real(ifov-1,r_kind)*step)*deg2rad


!     Load selected observation into data array
              
     data_all(1 ,itx)= rsat                      ! satellite ID
     data_all(2 ,itx)= t4dv                      ! time
     data_all(3 ,itx)= dlon                      ! grid relative longitude
     data_all(4 ,itx)= dlat                      ! grid relative latitude
     data_all(5 ,itx)= lza                       ! local zenith angle
     data_all(6 ,itx)= satazi                    ! local azimuth angle
     data_all(7 ,itx)= panglr                    ! look angle
     data_all(8 ,itx)= ifovmod                   ! scan position
     data_all(9 ,itx)= solzen                    ! solar zenith angle
     data_all(10,itx)= solazi                    ! solar azimuth angle
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
        data_all(i+nreal,itx)=bt_in(i)
     end do
     nrec(itx)=iob

  end do ObsLoop


  DEALLOCATE(iscan)
! DEAllocate I/O arrays
  DEALLOCATE(rsat_save)
  DEALLOCATE(t4dv_save)
  DEALLOCATE(dlon_earth_save)
  DEALLOCATE(dlat_earth_save)
  DEALLOCATE(crit1_save)
  DEALLOCATE(it_mesh_save)
  DEALLOCATE(lza_save)
  DEALLOCATE(satazi_save)
  DEALLOCATE(solzen_save) 
  DEALLOCATE(solazi_save) 
  DEALLOCATE(bt_save)

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

  if(diagnostic_reg.and.ntest>0) write(6,*)'READ_ATMS:  ',&
     'mype,ntest,disterrmax=',mype,ntest,disterrmax

! End of routine
  return

end subroutine read_atms
