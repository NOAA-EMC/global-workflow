subroutine read_cris(mype,val_cris,ithin,isfcalc,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs, &
     nrec_start,nrec_start_ears,nrec_start_db,dval_use)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_cris                  read bufr format cris data
! prgmmr :   mccarty          org: gmao                date: 2011-05-18
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
!   2010-10-12  zhu     - use radstep and radstart from radinfo
!   2011-05-18  mccarty - read_cris copied from read_iasi r10572 
!   2011-07-04  todling  - fixes to run either single or double precision
!   2011-08-01  lueken  - added module use deter_sfc_mod
!   2011-09-13  gayno - improve error handling for FOV-based sfc calculation
!                       (isfcalc=1)
!   2011-12-13  collard Replace find_edges code to speed up execution.
!   2012-03-05  akella  - nst now controlled via coupler
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2013-01-27  parrish - assign initial value to pred (to allow successful debug compile on WCOSS)
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2015-09-04  Jung    - Added mods for CrIS full spectral resolution (FSR).
!   2016-04-28  jung - added logic for RARS and direct broadcast from NESDIS/UW
!   2016-06-03  Collard - Added changes to allow for historical naming conventions
!
!   input argument list:
!     mype     - mpi task id
!     val_cris - weighting factor applied to super obs
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
!     nread    - number of BUFR CRIS observations read
!     ndata    - number of BUFR CRIS profiles retained for further processing
!     nodata   - number of BUFR CRIS observations retained for further processing
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
  use radinfo, only:iuse_rad,nuchan,nusis,jpch_rad,crtm_coeffs_path,use_edges, &
               radedge1,radedge2,radstart,radstep
  use crtm_module, only: success, &
      crtm_kind => fp,  max_sensor_zenith_angle
  use crtm_spccoeff, only: sc,crtm_spccoeff_load,crtm_spccoeff_destroy
  use crtm_planck_functions, only: crtm_planck_temperature
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,&
      tll2xy,txy2ll,rlats,rlons
  use constants, only: zero,deg2rad,rad2deg,r60inv,one,ten,r100
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,thin4d
  use calc_fov_crosstrk, only: instrument_init, fov_check, fov_cleanup
  use deter_sfc_mod, only: deter_sfc_fov,deter_sfc
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth,gsi_nstcoupler_deter
  use mpimod, only: npe

  implicit none


! Number of channels for sensors in BUFR


! BUFR format for CRISSPOT 
! Input variables
  integer(i_kind)  ,intent(in   ) :: mype,nrec_start,nrec_start_ears,nrec_start_db
  integer(i_kind)  ,intent(in   ) :: ithin
  integer(i_kind)  ,intent(inout) :: isfcalc
  integer(i_kind)  ,intent(in   ) :: lunout
  integer(i_kind)  ,intent(in   ) :: mype_root
  integer(i_kind)  ,intent(in   ) :: mype_sub
  integer(i_kind)  ,intent(in   ) :: npe_sub
  integer(i_kind)  ,intent(in   ) :: mpi_comm_sub  
  character(len=*), intent(in   ) :: infile
  character(len=10),intent(in   ) :: jsatid
  character(len=*), intent(in   ) :: obstype
  character(len=20),intent(in   ) :: sis
  real(r_kind)     ,intent(in   ) :: twind
  real(r_kind)     ,intent(inout) :: val_cris
  real(r_kind)     ,intent(in   ) :: gstime
  real(r_kind)     ,intent(in   ) :: rmesh
  logical          ,intent(in   ) :: dval_use

! Output variables
  integer(i_kind)  ,intent(inout) :: nread
  integer(i_kind)  ,dimension(npe), intent(inout) :: nobs
  integer(i_kind)  ,intent(  out) :: ndata,nodata
  

! BUFR file sequencial number
  integer(i_kind)     :: lnbufr = 10

! Variables for BUFR IO    
  real(r_double) :: rchar_mtyp
  real(r_double),dimension(5)  :: linele
  real(r_double),dimension(13) :: allspot
  real(r_double),allocatable,dimension(:,:) :: allchan
!  real(r_double),dimension(6):: cloud_frac
  character(len=3) :: char_mtyp
  
  real(r_kind)      :: step, start
  character(len=8)  :: subset
  character(len=4)  :: senname
  character(len=80) :: allspotlist
  character(len=40) :: infile2
  integer(i_kind)   :: jstart, kidsat, ksatid
  integer(i_kind)   :: iret,ireadsb,ireadmg,irec,next, nrec_startx
  integer(i_kind)   :: bufr_nchan,maxinfo
  integer(i_kind),allocatable,dimension(:)::nrec


! Work variables for time
  integer(i_kind)   :: idate
  integer(i_kind)   :: idate5(5)
  real(r_kind)      :: sstime, tdiff, t4dv
  integer(i_kind)   :: nmind, sfc_channel_index
  integer(i_kind)   :: subset_start, subset_end, satinfo_nchan, sc_chan, bufr_chan
  integer(i_kind),allocatable, dimension(:) :: channel_number, sc_index, bufr_index
  integer(i_kind),allocatable,dimension(:):: bufr_chan_test


! Other work variables
  real(r_kind)     :: dlon, dlat
  real(r_kind)     :: dlon_earth,dlat_earth,dlon_earth_deg,dlat_earth_deg
  real(r_kind)     :: rsat
  real(r_kind)     :: timedif, pred, crit1, dist1
  real(r_kind)     :: sat_zenang, sat_look_angle, look_angle_est
  real(crtm_kind)  :: radiance
  real(r_kind)     :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr
  real(r_kind)     :: zob,tref,dtw,dtc,tz_tr
  real(r_kind),dimension(0:4) :: rlndsea
  real(r_kind),dimension(0:3) :: sfcpct
  real(r_kind),dimension(0:3) :: ts
  real(crtm_kind),allocatable,dimension(:) :: temperature
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind) cdist,disterr,disterrmax,dlon00,dlat00,r01

  logical          :: outside,iuse,assim,valid
  logical          :: cris

  integer(i_kind)  :: ifov, ifor, iscn, instr, ioff, ilat, ilon, sensorindex
  integer(i_kind)  :: i, l, iskip, bad_line, llll
  integer(i_kind)  :: nreal, isflg
  integer(i_kind)  :: itx, k, nele, itt, n
  integer(i_kind):: idomsfc(1)
  integer(i_kind):: ntest
  integer(i_kind):: error_status, irecx,ierr
  integer(i_kind):: radedge_min, radedge_max
  integer(i_kind):: bufr_size
  character(len=20),dimension(1):: sensorlist


! Set standard parameters
  character(8),parameter:: fov_flag="crosstrk"
  integer(i_kind),parameter:: ichan=-999  ! fov-based surface code is not channel specific for cris 
  real(r_kind),parameter:: expansion=one         ! exansion factor for fov-based surface code.
                                                 ! use one for ir sensors.
  real(r_kind),parameter:: R90    =  90._r_kind
  real(r_kind),parameter:: R360   = 360._r_kind
  real(r_kind),parameter:: tbmin  = 50._r_kind
  real(r_kind),parameter:: tbmax  = 550._r_kind
  real(r_kind),parameter:: rato   = 0.87997285_r_kind 

! Initialize variables
  maxinfo    =  31
  disterrmax=zero
  ntest=0
  if(dval_use) maxinfo = maxinfo + 2
  nreal  = maxinfo + nstinfo
  ndata = 0
  nodata = 0
  cris= obstype == 'cris' .or. obstype == 'cris-fsr'
  r01=0.01_r_kind

  ilon=3
  ilat=4
  bad_line=-1

  if (jsatid == 'npp') then
     kidsat = 224
  elseif (jsatid == 'n20') then
     kidsat = 225
  elseif (jsatid == 'n21') then
     kidsat = 226
  else 
     write(*,*) 'READ_CrIS: Unrecognized value for jsatid '//jsatid//': RETURNING'
     return
  end if

  if (nst_gsi > 0 ) then
    call gsi_nstcoupler_skindepth(trim(obstype),zob)
  endif

!  write(6,*)'READ_CRIS: mype, mype_root,mype_sub, npe_sub,mpi_comm_sub', &
!          mype, mype_root,mype_sub,mpi_comm_sub

  radedge_min = 0
  radedge_max = 1000

! Find the 'cris' offset in the jpch_rad list.  This is for the iuse flag
! and count the number of cahnnels in the satinfo file for this sensor (cris, cris-fsr)
  ioff=jpch_rad
  subset_start = 0
  subset_end = 0
  assim = .false.
  do i=1,jpch_rad
     if (trim(nusis(i))==trim(sis)) then
        ioff = min(ioff,i)    ! cris offset
        if (subset_start == 0) then
          step  = radstep(i)
          start = radstart(i)
          if (radedge1(i)/=-1 .and. radedge2(i)/=-1) then
            radedge_min=radedge1(i)
            radedge_max=radedge2(i)
          endif
          subset_start = i
        endif 
        if (iuse_rad(i) >0) assim = .true.  ! Are any of the CrIS channels being used?
        subset_end = i
     endif
  end do
  satinfo_nchan = subset_end - subset_start + 1
  allocate(channel_number(satinfo_nchan))
  allocate(sc_index(satinfo_nchan))
  allocate(bufr_index(satinfo_nchan)) 
  ioff=ioff-1

! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.
  if (.not. assim) val_cris=zero

  if (mype_sub==mype_root)write(6,*)'READ_CRIS:  ',nusis(ioff+1),' offset ',ioff

  senname = 'CRIS'
  
  allspotlist= &
     'SAID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA BEARAZ SOZA SOLAZI'

! Load spectral coefficient structure  
  sensorlist(1)=sis
  if( crtm_coeffs_path /= "" ) then
     if(mype_sub==mype_root) write(6,*)'READ_CRIS: crtm_spccoeff_load() on path "'//trim(crtm_coeffs_path)//'"'
     error_status = crtm_spccoeff_load(sensorlist,&
        File_Path = crtm_coeffs_path )
  else
     error_status = crtm_spccoeff_load(sensorlist)
  endif

  if (error_status /= success) then
     write(6,*)'READ_CRIS:  ***ERROR*** crtm_spccoeff_load error_status=',error_status,&
        '   TERMINATE PROGRAM EXECUTION'
     call stop2(71)
  endif

! Find the channels being used (from satinfo file) in the spectral coef. structure.
  do i=subset_start,subset_end
     channel_number(i -subset_start +1) = nuchan(i)
  end do
  sc_index(:) = 0
  satinfo_chan: do i=1,satinfo_nchan
     spec_coef: do l=1,sc(1)%n_channels
        if ( channel_number(i) == sc(1)%sensor_channel(l) ) then
           sc_index(i) = l
           exit spec_coef
        endif
     end do spec_coef
  end do  satinfo_chan

!  find CRIS sensorindex. 
  sensorindex = 0
  if ( sc(1)%sensor_id(1:4) == 'cris' )then
     sensorindex = 1
  else
     write(6,*)'READ_CRIS: ***ERROR*** sensorindex not set  NO CRIS DATA USED'
     write(6,*)'READ_CRIS: We are looking for ', sc(1)%sensor_id, '   TERMINATE PROGRAM EXECUTION'
     call stop2(71)
  end if

! Calculate parameters needed for FOV-based surface calculation.
  if (isfcalc==1)then
     instr=17     ! CrIS is similar to AIRS for this purpose. 
     call instrument_init(instr, jsatid, expansion, valid)
     if (.not. valid) then
        if (assim) then
           write(6,*)'READ_CRIS:  ***ERROR*** IN SETUP OF FOV-SFC CODE. STOP'
           call stop2(71)
        else
           call fov_cleanup
           isfcalc = 0
           write(6,*)'READ_CRIS:  ***ERROR*** IN SETUP OF FOV-SFC CODE'
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

! Make thinning grids
  call makegrids(rmesh,ithin)

! Allocate arrays to hold data
! The number of channels is obtained from the satinfo file being used.
  nele=nreal+satinfo_nchan
  allocate(data_all(nele,itxmax),nrec(itxmax))
  allocate(temperature(1))   ! actual values set after ireadsb
  allocate(allchan(2,1))     ! actual values set after ireadsb
  allocate(bufr_chan_test(1))! actual values set after ireadsb

! Big loop to read data file
  next=0
  irec=0
  nrec = 99999
! Big loop over standard data feed and possible rars/db data
! llll=1 is normal feed, llll=2 RARS data, llll=3 DB/UW data)
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
     call closbf(lnbufr)
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

!          Check for data / sensor resolution mis-match 
           call ufbint(lnbufr,rchar_mtyp,1,1,iret,'MTYP')
           char_mtyp = transfer(rchar_mtyp,char_mtyp)
           if ( char_mtyp == 'FSR' .and. sis(1:8) /= 'cris-fsr') cycle read_subset
           if ( char_mtyp /= 'FSR' .and. sis(1:8) == 'cris-fsr') cycle read_subset

!          Get the size of the channels and radiance (allchan) array and
!          Read FOV information
           if (char_mtyp == 'FSR') then
              call ufbint(lnbufr,linele,5,1,iret,'FOVN SLNM QMRKH FORN  (CRCHNM)')
           else
              call ufbint(lnbufr,linele,5,1,iret,'FOVN SLNM QMRKH FORN  (CRCHN)')
           endif
           bufr_nchan = int(linele(5))

           bufr_size = size(temperature,1)
           if ( bufr_size /= bufr_nchan ) then    ! allocation if
!             Allocate the arrays needed for the channel and radiance array
              deallocate(temperature, allchan, bufr_chan_test)
              allocate(temperature(bufr_nchan))   ! dependent on # of channels in the bufr file
              allocate(allchan(2,bufr_nchan))
              allocate(bufr_chan_test(bufr_nchan))
           endif    ! allocation if

!          CRIS field-of-view ranges from 1 to 9, corresponding to the 9 sensors measured
!          per field-of-regard.  The field-of-regard ranges from 1 to 30.  For reference, FOV 
!          pattern within the FOR is :
!                FOV#      7 8 9|7 8 9
!                FOV#      4 5 6|4 5 6
!                FOV#      1 2 3|1 2 3 (spacecraft velocity up the screen)
!                ----------------------
!                FOR#        x    x+1
!          FORs are scanned from left limb (FOR=1) to right limb (FOR=30)
!
!          For now, we will simply choose IFOV=5.  See Fig. 58 of CrIS SDR ATBD (Rev. D) for a picture.

!          Only use central IFOV
           ifov = nint(linele(1))               ! field of view
           if (ifov /= 5) cycle read_loop

           ifor = nint(linele(4))               ! field of regard

!          Remove data on edges
           if (.not. use_edges .and. &
              (ifor < radedge_min .OR. ifor > radedge_max )) cycle read_loop

!          Top level QC check:
           if ( linele(3) /= zero) cycle read_loop  ! problem with profile (QMRKH)
                                                 ! May want to eventually set to 
                                                 ! QMRHK <= 1, as data is, and I
                                                 ! quote, 'slightly suspect'

!          Zenith angle/scan spot mismatch, reject entire line
           if ( bad_line == nint(linele(2))) then
              cycle read_loop
           else
              bad_line = -1
           endif

!          Check that the number of channels in the BUFR file is what we are expecting
!          Number of channels in the BUFR file must be less than or equal to the number of channels
!          in the spectral coefficient file.
           if (nint(linele(5)) > sc(1) % n_channels) then 
              if (mype_sub==mype_root) write(6,*)'READ_CRIS:  ***ERROR*** CrIS BUFR contains ',&
                 nint(linele(5)),' channels, but CRTM expects ',sc(1) % n_channels
              exit read_subset
           endif 
       
           iscn = nint(linele(2))               ! scan line
!          Check field of view (FOVN), field-of-regard (FORN), and satellite zenith angle (SAZA)
           if( ifov < 1 .or. ifov > 9  .or. & ! FOVN not betw. 1 & 9
              ifor < 1 .or. ifor > 30 )then  ! FORN not betw. 1 & 30
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                ' STRANGE OBS INFO(FOVN,FORN,SLNM):', ifov, ifor, iscn
              cycle read_loop
           endif

!          Read satellite id, lat/lon and other profile specific information
           call ufbint(lnbufr,allspot,13,1,iret,allspotlist)
           if(iret /= 1) cycle read_loop

!          Extract satellite id.  If not the one we want, read next record
           ksatid=nint(allspot(1))
           if(ksatid /= kidsat) cycle read_loop
           rsat=allspot(1) 

!          Check observing position
           dlat_earth = allspot(8)   ! latitude
           dlon_earth = allspot(9)   ! longitude
           if( abs(dlat_earth) > R90  .or. abs(dlon_earth) > R360 .or. &
              (abs(dlat_earth) == R90 .and. dlon_earth /= ZERO) )then
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
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

!          Convert to rotated coordinate.  dlon centered on 180 (pi),
!          so always positive for limited area
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

              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
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

!          Increment nread counter by bufr_nchan    (should be changed to number of channels in satinfo file? (satinfo_nchan))
           nread = nread + satinfo_nchan

           if (thin4d) then
              crit1 = 0.01_r_kind
           else
              timedif = 6.0_r_kind*abs(tdiff)        ! range:  0 to 18
              crit1 = 0.01_r_kind+timedif
           endif
           if( llll > 1 ) crit1 = crit1 + r100 * float(llll)
           call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
           if(.not. iuse)cycle read_loop

!          Observational info
           sat_zenang  = allspot(10)            ! satellite zenith angle

!          Check satellite zenith angle (SAZA)
           if(sat_zenang > 90._r_kind ) then
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                 ' STRANGE OBS INFO(SAZA):', allspot(10)
              cycle read_loop
           endif
           if ( ifor <= 15 ) sat_zenang = -sat_zenang

!          Compare CRIS satellite scan angle and zenith angle
 
           look_angle_est = (start + float(ifor)*step)*deg2rad
           sat_look_angle=asin(rato*sin(sat_zenang*deg2rad))

           if(abs(sat_look_angle)*rad2deg > MAX_SENSOR_ZENITH_ANGLE) then
              write(6,*)'READ_CRIS WARNING lza error ',sat_look_angle,look_angle_est
              cycle read_loop
           end if

           if (ifov == 5 .and. abs(sat_look_angle - look_angle_est)*rad2deg > one) then
              write(6,*)' READ_CRIS WARNING uncertainty in look angle ', &
                  look_angle_est*rad2deg,sat_look_angle*rad2deg,sat_zenang,sis,ifor,start,step,allspot(11),allspot(12),allspot(13)
              bad_line = iscn
              cycle read_loop
           endif

           pred = 100.0_r_kind    ! pred needs to have a value for WCOSS debug execution to work.

!          As cloud_frac is missing from BUFR, use proxy of warmest fov over
!          non ice surfaces.  Fixed channels (assuming the 399 set) for now.
!          This is moved to below where the radiances are read in.

!        if ( pred < zero .or. pred > 100.0_r_kind ) pred = 100.0_r_kind
           crit1 = crit1 + pred
           call checkob(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop

!          "Score" observation.  We use this information to identify "best" obs
!          Locate the observation on the analysis grid.  Get sst and land/sea/ice
!          mask.  
!          isflg    - surface flag
!                     0 sea
!                     1 land
!                     2 sea ice
!                     3 snow
!                     4 mixed 

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
           call checkob(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop

!          CrIS data read radiance values and channel numbers

!          Read CRIS channel number(CHNM) and radiance (SRAD)
           call ufbint(lnbufr,allchan,2,bufr_nchan,iret,'SRAD CHNM')
           if( iret /= bufr_nchan)then
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                iret, ' CH DATA IS READ INSTEAD OF ',bufr_nchan
              cycle read_loop
           endif
 
!          Coordinate bufr channels with satinfo file channels
!          If this is the first time or a change in the bufr channels is detected, sync with satinfo file
           if (ANY(int(allchan(2,:)) /= bufr_chan_test(:))) then
              sfc_channel_index = 0                                         ! surface channel used for qc and thinning test
              bufr_index(:) = 0
              bufr_chans: do l=1,bufr_nchan
                 bufr_chan_test(l) = int(allchan(2,l))                      ! Copy this bufr channel selection into array for comparison to next profile
                 satinfo_chans: do i=1,satinfo_nchan                        ! Loop through sensor (cris) channels in the satinfo file
                    if ( channel_number(i) == int(allchan(2,l)) ) then      ! Channel found in both bufr and satinfo file
                       bufr_index(i) = l
                       if ( channel_number(i) == 501 ) sfc_channel_index = l
                       exit satinfo_chans                                   ! go to next bufr channel
                    endif
                 end do  satinfo_chans
              end do bufr_chans
           end if 

           if ( sfc_channel_index == 0 ) then
              write(6,*)'READ_CRIS:  ***ERROR*** SURFACE CHANNEL USED FOR QC WAS NOT FOUND'
              cycle read_loop
           endif 

           iskip = 0
           jstart=1
!$omp parallel do schedule(dynamic,1) private(i,sc_chan,bufr_chan,radiance)
           channel_loop: do i=1,satinfo_nchan
              sc_chan = sc_index(i)
              if ( bufr_index(i) == 0 ) cycle channel_loop
              bufr_chan = bufr_index(i)
!             Check that channel radiance is within reason and channel number is consistent with CRTM initialisation
!             Negative radiance values are entirely possible for shortwave channels due to the high noise, but for
!             now such spectra are rejected.  
              if (( allchan(1,bufr_chan) > zero .and. allchan(1,bufr_chan) < 99999._r_kind)) then    ! radiance bounds
                 radiance = allchan(1,bufr_chan) * 1000.0_r_kind    ! Conversion from W to mW
                 call crtm_planck_temperature(sensorindex,sc_chan,radiance,temperature(bufr_chan))  ! radiance to BT calculation
              else           ! error with channel number or radiance
                 temperature(bufr_chan) = tbmin
              endif
           end do channel_loop

!          Check for reasonable temperature values
           skip_loop: do i=1,satinfo_nchan
              if ( bufr_index(i) == 0 ) cycle skip_loop
              bufr_chan = bufr_index(i)
              if(temperature(bufr_chan) <= tbmin .or. temperature(bufr_chan) > tbmax ) then
                 temperature(bufr_chan) = min(tbmax,max(zero,temperature(bufr_chan)))
                 if(iuse_rad(ioff+i) >= 0)iskip = iskip + 1
              endif
           end do skip_loop

           if(iskip > 0)write(6,*) ' READ_CRIS : iskip > 0 ',iskip
!          if( iskip >= 10 )cycle read_loop 

           crit1=crit1 + ten*float(iskip)

!          Cloud / clear tests.
!          Cloud information  may be missing depending on how the VIIRS granules align
!          with the CrIS granules.  
!          Cloud Amount, TOCC is percent cloudy, HOCT is cloud height in meters 
!JAJ        call ufbint(lnbufr,cloud_frac,2,1,iret,'TOCC HOCT')
!JAJ        if ( cloud_frac(1) <= 100.0_r_kind .and. cloud_frac(1) >= 0.0_r_kind) then
!          Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
!           pred = cloud_frac(1) / 10.0_r_kind
!JAJ           pred = cloud_frac(2) / 100.0_r_kind
!JAJ           crit1 = crit1 + pred

!JAJ        else
!       If cloud_frac is missing from BUFR, use proxy of warmest fov over 
!       non ice surfaces.  Fixed channels (assuming the 399 set) for now.
!       This is moved to below where the radiances are read in.

              if (sfcpct(0)+sfcpct(1) > 0.9) &
                 crit1=crit1+(320.0_r_kind-temperature(sfc_channel_index))
 
!JAJ        endif ! clearest FOV check

!          Map obs to grids
           call finalcheck(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop
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

           data_all(1,itx) = rsat                   ! satellite ID
           data_all(2,itx) = t4dv                   ! time diff (obs-anal) (hrs)
           data_all(3,itx) = dlon                   ! grid relative longitude
           data_all(4,itx) = dlat                   ! grid relative latitude
           data_all(5,itx) = sat_zenang*deg2rad     ! satellite zenith angle (rad)
           data_all(6,itx) = allspot(11)            ! satellite azimuth angle (deg)
           data_all(7,itx) = sat_look_angle         ! look angle (rad)
           data_all(8,itx) = ifor                   ! field of regard
           data_all(9,itx) = allspot(12)            ! solar zenith angle (deg)
           data_all(10,itx)= allspot(13)            ! solar azimuth angle (deg)
           data_all(11,itx)= sfcpct(0)              ! sea percentage of
           data_all(12,itx)= sfcpct(1)              ! land percentage
           data_all(13,itx)= sfcpct(2)              ! sea ice percentage
           data_all(14,itx)= sfcpct(3)              ! snow percentage
           data_all(15,itx)= ts(0)                  ! ocean skin temperature
           data_all(16,itx)= ts(1)                  ! land skin temperature
           data_all(17,itx)= ts(2)                  ! ice skin temperature
           data_all(18,itx)= ts(3)                  ! snow skin temperature
           data_all(19,itx)= tsavg                  ! average skin temperature
           data_all(20,itx)= vty                    ! vegetation type
           data_all(21,itx)= vfr                    ! vegetation fraction
           data_all(22,itx)= sty                    ! soil type
           data_all(23,itx)= stp                    ! soil temperature
           data_all(24,itx)= sm                     ! soil moisture
           data_all(25,itx)= sn                     ! snow depth
           data_all(26,itx)= zz                     ! surface height
           data_all(27,itx)= idomsfc(1) + 0.001_r_kind ! dominate surface type
           data_all(28,itx)= sfcr                   ! surface roughness
           data_all(29,itx)= ff10                   ! ten meter wind factor
           data_all(30,itx)= dlon_earth*rad2deg     ! earth relative longitude (degrees)
           data_all(31,itx)= dlat_earth*rad2deg     ! earth relative latitude (degrees)

           if(dval_use) then
              data_all(32,itx)= val_cris
              data_all(33,itx)= itt
           end if

           if ( nst_gsi > 0 ) then
              data_all(maxinfo+1,itx) = tref         ! foundation temperature
              data_all(maxinfo+2,itx) = dtw          ! dt_warm at zob
              data_all(maxinfo+3,itx) = dtc          ! dt_cool at zob
              data_all(maxinfo+4,itx) = tz_tr        ! d(Tz)/d(Tr)
           endif

!          Put satinfo defined channel temperatures into data array
           do l=1,satinfo_nchan
              i = bufr_index(l)
              if ( bufr_index(l) /= 0 ) then
                 data_all(l+nreal,itx) = temperature(i) ! brightness temperature
              else
                 data_all(l+nreal,itx) = tbmin
              endif
           end do
           nrec(itx)=irec


        enddo read_loop

     enddo read_subset

     call closbf(lnbufr)

  end do ears_db_loop

  deallocate(temperature, allchan, bufr_chan_test)
! deallocate crtm info
  error_status = crtm_spccoeff_destroy()
  if (error_status /= success) &
     write(6,*)'OBSERVER:  ***ERROR*** crtm_spccoeff_destroy error_status=',error_status

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
           super_val(itt)=super_val(itt)+val_cris
        end do
     end if

!    Write final set of "best" observations to output file
     call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
     write(lunout) obstype,sis,nreal,satinfo_nchan,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif


  deallocate(data_all,nrec) ! Deallocate data arrays
  deallocate(channel_number,sc_index)
  deallocate(bufr_index)
  call destroygrids    ! Deallocate satthin arrays

! Deallocate arrays and nullify pointers.
  if(isfcalc == 1) call fov_cleanup

  if(diagnostic_reg .and. ntest > 0 .and. mype_sub==mype_root) &
     write(6,*)'READ_CRIS:  mype,ntest,disterrmax=',&
        mype,ntest,disterrmax
  
  return
end subroutine read_cris
