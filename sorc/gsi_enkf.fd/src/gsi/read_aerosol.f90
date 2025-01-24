subroutine read_aerosol(nread,ndata,nodata,jsatid,infile,gstime,lunout, &
           obstype,twind,sis,ithin,rmesh, &
           mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_aerosol                    read aerosol data
!   prgmmr: hchuang     org: np23                date: 2009-01-26
!
! abstract:  This routine reads MODIS aerosol total column AOD observations.
!            ONLY total column values are read in.  The routine has
!            the ability to read both IEEE and BUFR format MODIS
!            as well as BUFR format VIIRS 
!            aerosol data files.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2009-04-08  Huang   - modified from read_ozone to read in MODIS AEROSOL data
!   2010-10-20  hclin   - modified for total aod in channels
!   2011-01-05  hclin   - added three more BUFR records (STYP DBCF QAOD)
!   2011-08-01  lueken  - changed F90 to f90 (no machine logic)
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2015-10-01  guo      - calc ob location once in deg
!   2018-05-21  j.jin    - added time-thinning. Moved the checking of thin4d into satthin.F90.
!   2019-03-22  martin  - add VIIRS BUFR capability based on code from S-W Wei and  Q. Zhao
!   2022-05-24  R.Li/H.Wang Add VIIRS AOD with a new Bufr Table. 
!
!   input argument list:
!     obstype  - observation type to process
!     jsatid   - satellite id to read
!     infile   - unit from which to read aerosol data
!     gstime   - analysis time in minutes from reference date
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     mype     - mpi task id
!     mype_root - "root" task for sub-communicator
!     mype_sub - mpi task id within sub-communicator
!     npe_sub  - number of data read tasks
!     mpi_comm_sub - sub-communicator for data read
!
!   output argument list:
!     nread    - number of modis aerosol observations read
!     ndata    - number of modis aerosol profiles retained for further processing
!     nodata   - number of modis aerosol observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  IBM AIX Cirrus
!
!$$$
  use kinds,     only: r_kind, r_double, i_kind
  use gridmod,   only: nlat, nlon, regional, tll2xy, rlats, rlons
  use chemmod,   only: aod_qa_limit, luse_deepblue
  use constants, only: deg2rad, zero, one, two, three, four, five, r0_01, r60inv
  use obsmod,    only: rmiss_single
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use satthin,   only: itxmax,makegrids,destroygrids,checkob, &
      finalcheck,map2tgrid,score_crit
  use satthin,   only: radthin_time_info,tdiff2crit
  use obsmod,    only: time_window_max
  use mpimod, only: npe
  implicit none
!
! Declare local parameters
  real(r_kind), parameter :: r360 = 360.0_r_kind
!
! Declare passed variables
!
  character(len=*),intent(in)    :: obstype, infile, jsatid
  character(len=20),intent(in)   :: sis
  integer(i_kind), intent(in)    :: lunout, ithin
  integer(i_kind), intent(inout) :: nread
  integer(i_kind),dimension(npe), intent(inout) :: nobs
  integer(i_kind), intent(inout) :: ndata, nodata
  integer(i_kind) ,intent(in)    :: mype_root
  integer(i_kind) ,intent(in)    :: mype_sub
  integer(i_kind) ,intent(in)    :: npe_sub
  integer(i_kind) ,intent(in)    :: mpi_comm_sub
  real(r_kind),    intent(in)    :: gstime, twind, rmesh
!
! Declare local variables
!
  logical :: outside, iuse, new_viirsTab
  
  character (len= 8) :: subset
  character (len=10) :: date

  integer(i_kind) :: naerodat
  integer(i_kind) :: idate, jdate, ksatid, iy, iret, im, ihh, idd
  integer(i_kind) :: lunin = 10
  integer(i_kind) :: nmind, i, n, j
  integer(i_kind) :: k, ilat, ilon, nreal, nchanl
  integer(i_kind) :: kidsat
  integer(i_kind), dimension(5) :: idate5
!
!| NC008041 | SAID    AEROSOL  CLONH   CLATH YYMMDD  HHMMSS  SOZA  SOLAZI       |
!| NC008041 | SCATTA  OPTD  AEROTP                                              |
!
!| YYMMDD   | YEAR    MNTH    DAYS                                              |
!|          |                                                                   |
!| HHMMSS   | HOUR    MINU    SECO                                              |
!
!    SAID    Satellite identifier code table (eg, 783 == 'TERRA')
!    AEROSOL Aerosol Optical Depth (AOD) source code table (eg, 5 == 'AATSR' )
!    YEAR    Year                               
!    MNTH    Month                              
!    DAYS    Day                                
!    HOUR    Hour                               
!    MINU    Minute                             
!    SECO    Second                             
!    CLATH   Latitude (high accuracy)     degree (5 decimal precision)
!    CLONH   Longitude (high accuracy)    degree (5 decimal precision)
!    SOLAZI  Solar azimuth                degree (2 decimal precision)
!    SOZA    Solar zenith angle           degree (2 decimal precision)
!    OPTD    Optical depth                numeric
!    SCATTA  Scattering angle             degree (2 decimal precsion)
!    AEROTP  Aerosol type land            code table (eg, 1 == 'DUST', 2 == 'SULFATE')
!
!    0-15-195 - AEROTP (Aerosol land type)
!
!    CODE  DESCRIPTION
!    ====  ===========
!    0     Mixed
!    1     Dust
!    2     Sulfate
!    3     Smoke
!    4     Heavy absorbing smoke
!    5-14  Reserved
!    15    Missing value
!
  character (len= 4) :: aerostr  = 'OPTD'
  character (len=53) :: aerogstr = &
      'SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI'

! VIIRS AOD code
  character (len= 9) :: vaodchstr  = 'CHWL AOTH'
  character (len=69) :: vaodgstr = &
      'SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI RSST VAOTQ QPLR'

! new viirs table
  character (len=69) :: vaodgstr1 = &
      'SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI RSST AOTQ RETRQ'

  integer(i_kind), parameter :: mxib  = 20,imax=6
  integer(i_kind) :: nib
  integer(i_kind) :: ibit(mxib)

  integer(i_kind) :: itx, itt, irec

  real(r_kind) :: tdiff, sstime, dlon, dlat, t4dv, crit1, dist1
  real(r_kind) :: slons0, slats0, rsat, solzen, azimuth, dlat_earth, dlon_earth
  real(r_kind) :: dlat_earth_deg, dlon_earth_deg
  real(r_kind) :: styp, dbcf, qaod, smask, qcall
  real(r_kind) :: qcall_limit  ! qcall >= qcall_limit will be retained

  real(r_kind),dimension(0:6):: rlndsea

  real(r_kind), allocatable, dimension(:,:) :: aeroout
  real(r_kind), allocatable, dimension(:)   :: dataaod
  integer(i_kind),allocatable,dimension(:)  :: nrec
  real(r_double), dimension( 10) :: hdraerog
  real(r_double)                 :: aod_550
  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin,it_mesh
! for VIIRS
  real(r_double),dimension(13)   :: hdrvaodg
  real(r_double),dimension(2,12) :: vaodch
  real(r_double)                 :: aod_lb,aod_ub

!**************************************************************************
! Set constants.  Initialize variables
  rsat=999._r_kind
  ! output position of LON and LAT
  ilon=3
  ilat=4
  nread = 0
  ndata = 0
  nodata = 0

  ! Set rlndsea for types we would prefer selecting
  rlndsea(0) = zero        ! styp 0: water
  rlndsea(1) = 15._r_kind  ! styp 1: coast
  rlndsea(2) = 20._r_kind  ! styp 2: desert
  rlndsea(3) = 10._r_kind  ! styp 3: land
  rlndsea(4) = 25._r_kind  ! styp 4: deep blue
  rlndsea(5) = 30._r_kind  ! styp 5: nnr ocean
  rlndsea(6) = 35._r_kind  ! styp 6: nnr land


  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif
! Make thinning grids
  call makegrids(rmesh,ithin,n_tbin=n_tbin)

  if ( obstype == 'modis_aod' ) then
!
     open(lunin,file=trim(infile),form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     call readmg(lunin,subset,idate,iret)

     if ( iret == 0 ) then
!
        if (subset == 'NC008041') then
           write(6,*)'READ_AEROSOL: MODIS data type, subset = ',subset
           !          Set dependent variables and allocate arrays
           nreal=11   !9
           nchanl=20 ! 19 + 1 additional vis channel in CRTM coeff file
           naerodat=nreal+nchanl
           allocate (aeroout(naerodat,itxmax),nrec(itxmax))
           allocate (dataaod(nchanl))

           iy = 0
           im = 0
           idd= 0
           ihh= 0
           write(date,'( i10)') idate
           read (date,'(i4,3i2)') iy,im,idd,ihh
           write(6,'(''READ_AEROSOL: aerosol bufr file '',a,''  date is '',i4,4i2.2,a)')trim(infile),iy,im,idd,ihh

           nrec=999999
           irec=0
           read_modis: do
              irec=irec+1
              call readsb(lunin,iret)
              if (iret/=0) then
                 call readmg(lunin,subset,jdate,iret)
                 if (iret/=0) exit read_modis
                 cycle read_modis
              endif
     
              !    extract header information
              call ufbint(lunin,hdraerog,10,1,iret,aerogstr)
              rsat = hdraerog(1); ksatid=rsat

              if ( jsatid == 'terra' ) kidsat = 783
              if ( jsatid == 'aqua'  ) kidsat = 784

              if ( ksatid /= kidsat  ) cycle read_modis

              !    Convert observation location to radians
              slats0= hdraerog(2)
              slons0= hdraerog(3)
              if(slons0< zero) slons0=slons0+r360
              if(slons0>=r360) slons0=slons0-r360
              dlat_earth_deg = slats0
              dlon_earth_deg = slons0
              dlat_earth = slats0 * deg2rad
              dlon_earth = slons0 * deg2rad

              if(regional)then
                 call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
                 if(outside) cycle read_modis
              else
                 dlat = dlat_earth
                 dlon = dlon_earth
                 call grdcrd1(dlat,rlats,nlat,1)
                 call grdcrd1(dlon,rlons,nlon,1)
              endif

              solzen  = hdraerog(9)
              azimuth = hdraerog(10)

              !    Convert observation time to relative time
              idate5(1) = hdraerog(4)  !year
              idate5(2) = hdraerog(5)  !month
              idate5(3) = hdraerog(6)  !day
              idate5(4) = hdraerog(7)  !hour
              idate5(5) = hdraerog(8)  !minute

              !    extract total column aod 1 value 'OPTD' as defined in aerostr
              call ufbint(lunin,aod_550,1,1,iret,aerostr)

              call w3fs21(idate5,nmind)
              t4dv=real((nmind-iwinbgn),r_kind)*r60inv
              sstime=real(nmind,r_kind)
              tdiff=(sstime-gstime)*r60inv

              if (l4dvar.or.l4densvar) then
                 if(t4dv<zero .OR. t4dv>winlen) cycle read_modis
              else
                 if ( abs(tdiff) > twind ) cycle read_modis
              end if

             
              nread = nread + 1   !nread = nread + nchanl

              if ( aod_550 > 1.0e+10_r_double ) cycle read_modis

              ! extract STYP, DBCF, and QAOD
              ! these are missing from the 008041 bufr files
              styp = rmiss_single
              dbcf = rmiss_single
              qaod = zero 

              if ( .not. luse_deepblue .and. nint(styp)==4 ) cycle read_modis
              if ( qaod > aod_qa_limit ) cycle read_modis

              ! Map obs to thinning grid
              crit0 = 0.01_r_kind 
              timeinflat=two
              call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)
              call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)
              if ( .not. iuse ) cycle read_modis

              if ( (styp > rmiss_single) .and. (styp >= zero .and. styp <= four) ) then
                 crit1 = crit1 + rlndsea(nint(styp))
              end if
              if ( (qaod > rmiss_single) .and. (qaod >= aod_qa_limit .and. qaod <= three) ) then
                 crit1 = crit1 + 10.0_r_kind*(four-qaod)
              end if
              call checkob(dist1,crit1,itx,iuse)
              if ( .not. iuse ) cycle read_modis

              ! Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
              call finalcheck(dist1,crit1,itx,iuse)
              if ( .not. iuse ) cycle read_modis

              dataaod = rmiss_single
              dataaod(4) = aod_550

              aeroout( 1,itx) = rsat
              aeroout( 2,itx) = tdiff
              aeroout( 3,itx) = dlon               ! grid relative longitude
              aeroout( 4,itx) = dlat               ! grid relative latitude
              aeroout( 5,itx) = dlon_earth_deg     ! earth relative longitude (degrees)
              aeroout( 6,itx) = dlat_earth_deg     ! earth relative latitude (degrees)
              aeroout( 7,itx) = qaod               ! total column AOD error flag
              aeroout( 8,itx) = solzen             ! solar zenith angle
              aeroout( 9,itx) = azimuth            ! solar azimuth angle
              aeroout(10,itx) = styp               ! surface type
              aeroout(11,itx) = dbcf               ! deep blue confidence flag
              do i = 1, nchanl
                 aeroout(i+nreal,itx) = dataaod(i)
              end do
              nrec(itx)=irec
       
           end do read_modis

           call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
              naerodat,itxmax,nread,ndata,aeroout,score_crit,nrec)

           if ( mype_sub == mype_root ) then
              do n = 1, ndata
                 do i = 1, nchanl
                    if ( aeroout(i+nreal,n) > rmiss_single ) nodata = nodata + 1
                 end do
              end do
              ! Write final set of "best" observations to output file
              call count_obs(ndata,naerodat,ilat,ilon,aeroout,nobs)
              write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
              write(lunout) ((aeroout(k,n),k=1,naerodat),n=1,ndata)
           end if

           ! Deallocate local arrays
           deallocate(aeroout,nrec)
           deallocate(dataaod)

           ! End of MODIS bufr block
        else       ! subset /= NC008041
           write(6,*)'READ_AEROSOL:  *** WARNING: unknown aerosol data type, subset=',subset
           write(6,*)' infile=',infile, ', lunin=',lunin, ', obstype=',obstype,', jsatid=',jsatid
           write(6,*)' SKIP PROCESSING OF THIS MODIS FILE'
        endif

     else          ! read subset iret /= 0
        write(6,*)'READ_AEROSOL:  *** WARNING: read subset error, obstype=',obstype,', iret=',iret
     end if
     call closbf(lunin)
     close(lunin)
  else if ( obstype == 'viirs_aod' ) then
     new_viirsTab=.false.
     open(lunin,file=trim(infile),form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     call readmg(lunin,subset,idate,iret)

     if ( iret == 0 ) then

        if (subset == 'NC008043') then
           write(6,*)'READ_AEROSOL: VIIRS AOD data type, subset = ',subset
           !          Set dependent variables and allocate arrays
           nreal=11 !10
           nchanl=11
           naerodat=nreal+nchanl
           allocate (aeroout(naerodat,itxmax),nrec(itxmax))
           allocate (dataaod(nchanl))

           iy = 0
           im = 0
           idd= 0
           ihh= 0
           write(date,'( i10)') idate
           read (date,'(i4,3i2)') iy,im,idd,ihh
           write(6,'(''READ_AEROSOL: aerosol bufr file '',a,''  date is '',i4,3i2.2)') trim(infile),iy,im,idd,ihh

!          set qcall_limit
           if (idate >= 2018021300) then
              qcall_limit = aod_qa_limit + r0_01 ! for the viirs data after 2018/02/13
              new_viirsTab=.true.                ! for the viirs data 2019/08-09 used in WF1 project
           else
              qcall_limit = aod_qa_limit - r0_01
           end if

!          set valid range of AOD to ingest
           aod_lb = zero
           aod_ub = five
           nrec=999999
           irec=0
           read_viirs: do
              irec=irec+1
              call readsb(lunin,iret)
              if (iret/=0) then
                 call readmg(lunin,subset,jdate,iret)
                 if (iret/=0) exit read_viirs
                 cycle read_viirs
              endif

              !    extract header information
              if (.not. new_viirsTab) then
                call ufbint(lunin,hdrvaodg,13,1,iret,vaodgstr)
              else
                call ufbint(lunin,hdrvaodg,13,1,iret,vaodgstr1)
              endif
              rsat = hdrvaodg(1); ksatid=rsat

              if ( jsatid == 'NPP' .or. jsatid == 'npp' ) kidsat = 224
              if ( jsatid == 'N20' .or. jsatid == 'n20' ) kidsat = 225

              if ( ksatid /= kidsat  ) cycle read_viirs

              !    Convert observation location to radians
              slats0= hdrvaodg(2)
              slons0= hdrvaodg(3)
              if(slons0< zero) slons0=slons0+r360
              if(slons0>=r360) slons0=slons0-r360
              dlat_earth_deg = slats0
              dlon_earth_deg = slons0
              dlat_earth = slats0 * deg2rad
              dlon_earth = slons0 * deg2rad

              if(regional)then
                 call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
                 if(outside) cycle read_viirs
              else
                 dlat = dlat_earth
                 dlon = dlon_earth
                 call grdcrd1(dlat,rlats,nlat,1)
                 call grdcrd1(dlon,rlons,nlon,1)
              endif

              solzen  = hdrvaodg(9)
              azimuth = hdrvaodg(10)

              if (.not. new_viirsTab) then
              ! SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI RSST VAOTQ QPLR
                smask = zero                          ! over water
                if (nint(hdrvaodg(11)) > 0) then      ! over land: RSST
                  smask = one                         ! dark surface
                  call upftbv(lunin,"VAOTQ",hdrvaodg(12),mxib,ibit,nib) 
                  if (nib > 0) then
                    if(any(ibit(1:nib) == 6)) then
                       smask = two                  !  bright surface
                    endif
                  endif
                endif

              else
              ! SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI RSST AOTQ RETRQ

                do j=1,imax
                   ibit(j)=IBits(int(hdrvaodg(12)),j-1,1)   ! IBits: right justified
                enddo

                if ( ibit(1)==1 .or. ibit(3)==1 ) then
                  smask=zero
                else if ( ibit(4)==1 .or. ibit(5)==1 ) then
                  smask=one
                else if ( ibit(2)==1 .or. ibit(6)==1 ) then
                  smask=two
                else
                  write(6,*)'VIIRS unknown surface type:  ',hdrvaodg(12)                   
                endif

              endif

              qcall   = hdrvaodg(13) 

              !    Convert observation time to relative time
              idate5(1) = hdrvaodg(4)  !year
              idate5(2) = hdrvaodg(5)  !month
              idate5(3) = hdrvaodg(6)  !day
              idate5(4) = hdrvaodg(7)  !hour
              idate5(5) = hdrvaodg(8)  !minute

              call w3fs21(idate5,nmind)
              t4dv=real((nmind-iwinbgn),r_kind)*r60inv
              sstime=real(nmind,r_kind)
              tdiff=(sstime-gstime)*r60inv


              if (l4dvar.or.l4densvar) then
                 if(t4dv<zero .OR. t4dv>winlen) cycle read_viirs
              else
                 if ( abs(tdiff) > twind ) cycle read_viirs
              end if

              nread = nread + 1   !nread = nread + nchanl

              if (idate >= 2018021300) then
                 if ( qcall > qcall_limit ) cycle read_viirs
              else
                 if ( qcall < qcall_limit ) cycle read_viirs
              end if

              !    extract VAODCH pairs 'CHWL AOTH' as defined in vaodchstr
              call ufbrep(lunin,vaodch,2,12,iret,vaodchstr)
              aod_550 = vaodch(2,12)

              if ( aod_550 < aod_lb .OR. aod_550 >= aod_ub ) cycle read_viirs
                              

              ! Map obs to thinning grid
              crit0 = 0.01_r_kind 
              timeinflat=two
              call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)
              call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)

              if ( .not. iuse ) cycle read_viirs

              crit1 = crit1 + 10.0_r_kind*smask
              ! is below needed now because of the change in QC flags? CRM
              if (idate >= 2018021300) then
                crit1 = crit1 + 10.0_r_kind*(four+qcall)
              else
                crit1 = crit1 + 10.0_r_kind*(four-qcall)
              end if
              call checkob(dist1,crit1,itx,iuse)

              if ( .not. iuse ) cycle read_viirs

              ! Compute "score" for observation.  All scores>=0.0.  Lowest score
              ! is "best"
              call finalcheck(dist1,crit1,itx,iuse)

              if ( .not. iuse ) cycle read_viirs

              dataaod = rmiss_single
              dataaod(4) = aod_550

              aeroout( 1,itx) = rsat
              aeroout( 2,itx) = tdiff
              aeroout( 3,itx) = dlon               ! grid relative longitude
              aeroout( 4,itx) = dlat               ! grid relative latitude
              aeroout( 5,itx) = dlon_earth_deg     ! earth relative longitude (degrees)
              aeroout( 6,itx) = dlat_earth_deg     ! earth relative latitude (degrees)
              aeroout( 7,itx) = qcall              ! total column AOD error flag
              aeroout( 8,itx) = solzen             ! solar zenith angle
              aeroout( 9,itx) = azimuth            ! solar azimuth angle
              aeroout(10,itx) = smask              ! surface type mask
              aeroout(11,itx) = hdrvaodg(11) 
              do i = 1, nchanl
                 aeroout(i+nreal,itx) = dataaod(i) ! RL: aeroout(14,itx) =aod_550
              enddo
              nrec(itx)=irec
           end do read_viirs

           call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
              naerodat,itxmax,nread,ndata,aeroout,score_crit,nrec)

           if ( mype_sub == mype_root ) then
              do n = 1, ndata
                 do i = 1, nchanl
                    if ( aeroout(i+nreal,n) > rmiss_single ) nodata = nodata + 1
                 end do
              end do
           
              ! Write final set of "best" observations to output file
              call count_obs(ndata,naerodat,ilat,ilon,aeroout,nobs)
              write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
              write(lunout) ((aeroout(k,n),k=1,naerodat),n=1,ndata)
           end if

           ! Deallocate local arrays
           deallocate(aeroout,nrec)
           deallocate(dataaod)

           ! End of VIIRS AOD bufr block

        else       ! subset /= NC008043
           write(6,*)'READ_AEROSOL:  *** WARNING: unknown aerosol data type, subset=',subset
           write(6,*)' infile=',infile, ', lunin=',lunin, ', obstype=',obstype,', jsatid=',jsatid
           write(6,*)' SKIP PROCESSING OF THIS VIIRS_AOD FILE'
        endif

     else          ! read subset iret /= 0
        write(6,*)'READ_AEROSOL:  *** WARNING: read subset error, obstype=',obstype,', iret=',iret
     end if
     call closbf(lunin)
     close(lunin)

  else             ! obstype /= 'modis' or 'viirs'
     write(6,*)'READ_AEROSOL:  *** WARNING: unknown aerosol input type, obstype=',obstype
  endif

  ! Deallocate satthin arrays
  call destroygrids

end subroutine read_aerosol
