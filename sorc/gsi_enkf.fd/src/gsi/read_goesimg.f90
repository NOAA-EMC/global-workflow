subroutine read_goesimg(mype,val_img,ithin,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis, &
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs, &
     nrec_start,dval_use)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_goesimg                    read goes imager data
!   prgmmr: su, xiujuan      org: np23                date: 2002-02-26
!
! abstract:  This routine reads GOES imager radiance (brightness
!            temperature) files.  Optionally, the data are thinned to
!            a specified resolution using simple quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2002-02-26 su, x.  
!   2004-05-28 kleist  - update subroutine call
!   2004-06-16 treadon - update documentation
!   2004-07-23 derber - make changes to eliminate obs. earlier in thinning
!   2004-07-29  treadon - abonl  to module use, add intent in/out
!   2005-01-26  derber - land/sea determination and weighting for data selection
!   2005-09-08  derber - modify to use input group time window
!   2005-09-28  derber - modify to produce consistent surface info
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-03  derber  - add new obs control
!   2006-04-27  derber - clean up code
!   2006-05-19  eliu    - add logic to reset relative weight when all channels not used
!   2006-06-19  kleist - correct bug in global grid relative dlat,dlon
!   2006-07-28  derber  - add solar and satellite azimuth angles remove isflg from output
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2008-10-14  derber - allow mpi_io
!   2009-04-21  derber  - add ithin to call to makegrids
!   2011-04-08  li      - (1) use nst_gsi, nstinfo, fac_dtl, fac_tsl and add NSST vars
!                         (2) get zob, tz_tr (call skindepth and cal_tztr)
!                         (3) interpolate NSST Variables to Obs. location (call deter_nst)
!                         (4) add more elements (nstinfo) in data array
!   2011-08-01  lueken  - added module use deter_sfc_mod, fix indentation
!   2012-03-05  akella  - nst now controlled via coupler
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!   2015-10-01  guo     - consolidate use of ob location (in deg)
!   2018-05-21  j.jin   - added time-thinning. Moved the checking of thin4d into satthin.F90.
!
!   input argument list:
!     mype     - mpi task id
!     val_img  - weighting factor applied to super obs
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
!     nread    - number of BUFR GOES imager observations read
!     ndata    - number of BUFR GOES imager profiles retained for further processing
!     nodata   - number of BUFR GOES imager observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
      checkob,finalcheck,score_crit
  use satthin, only: radthin_time_info,tdiff2crit
  use obsmod,  only: time_window_max
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,txy2ll,tll2xy,rlats,rlons
  use constants, only: deg2rad,zero,one,rad2deg,r60inv,r60
  use radinfo, only: iuse_rad,jpch_rad,nusis
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use deter_sfc_mod, only: deter_sfc
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter
  use mpimod, only: npe
! use radiance_mod, only: rad_obs_type
  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: infile,obstype,jsatid
  character(len=20),intent(in  ) :: sis
  integer(i_kind) ,intent(in   ) :: mype,lunout,ithin,nrec_start
  integer(i_kind) ,intent(inout) :: ndata,nodata
  integer(i_kind) ,intent(inout) :: nread
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  real(r_kind)    ,intent(in   ) :: rmesh,gstime,twind
  real(r_kind)    ,intent(inout) :: val_img
  integer(i_kind) ,intent(in   ) :: mype_root
  integer(i_kind) ,intent(in   ) :: mype_sub
  integer(i_kind) ,intent(in   ) :: npe_sub
  integer(i_kind) ,intent(in   ) :: mpi_comm_sub
  logical         ,intent(in   ) :: dval_use

! Declare local parameters
  integer(i_kind),parameter:: nimghdr=13
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind
  character(80),parameter:: hdrgoes  = &            ! goes imager header
        'SAID YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SAZA SOZA BEARAZ SOLAZI'

! Declare local variables
  logical outside,iuse,assim

  character(8) subset

  integer(i_kind) nchanl,ilath,ilonh,ilzah,iszah,irec,next
  integer(i_kind) nmind,lnbufr,idate,ilat,ilon,maxinfo
  integer(i_kind) ireadmg,ireadsb,iret,nreal,nele,itt
  integer(i_kind) itx,i,k,isflg,kidsat,n,iscan,idomsfc
  integer(i_kind) idate5(5)
  integer(i_kind),allocatable,dimension(:)::nrec

  real(r_kind) dg2ew,sstime,tdiff,t4dv,sfcr
  real(r_kind) dlon,dlat,crit1,dist1
  real(r_kind) dlon_earth,dlat_earth
  real(r_kind) dlon_earth_deg,dlat_earth_deg
  real(r_kind) pred
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr
  real(r_kind),allocatable,dimension(:,:):: data_all

  real(r_double),dimension(nimghdr) :: hdrgoesarr       !  goes imager header
  real(r_double),dimension(3,6) :: dataimg              !  goes imager data

  real(r_kind) cdist,disterr,disterrmax,dlon00,dlat00
  integer(i_kind) ntest
  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin,it_mesh


!**************************************************************************
! Initialize variables
  maxinfo=31
  lnbufr = 10
  disterrmax=zero
  ntest=0
  dg2ew = r360*deg2rad

  ilon=3
  ilat=4

  if (nst_gsi > 0 ) then
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif

  rlndsea(0) = zero
  rlndsea(1) = 15._r_kind
  rlndsea(2) = 10._r_kind
  rlndsea(3) = 15._r_kind
  rlndsea(4) = 30._r_kind

  ndata=0
  nodata=0
  nchanl=4       ! the channel number
  ilath=8        ! the position of latitude in the header
  ilonh=9        ! the position of longitude in the header
  ilzah=10       ! satellite zenith angle
  iszah=11       ! solar zenith angle

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
  if (.not.assim) val_img=zero


  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif
! Make thinning grids
  call makegrids(rmesh,ithin,n_tbin=n_tbin)


! Open bufr file.
  open(lnbufr,file=trim(infile),form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)
  if(jsatid == 'g08') kidsat = 252
  if(jsatid == 'g09') kidsat = 253
  if(jsatid == 'g10') kidsat = 254
  if(jsatid == 'g11') kidsat = 255
  if(jsatid == 'g12') kidsat = 256
  if(jsatid == 'g13') kidsat = 257
  if(jsatid == 'g14') kidsat = 258
  if(jsatid == 'g15') kidsat = 259

! Allocate arrays to hold all data for given satellite
  if(dval_use) maxinfo = maxinfo + 2
  nreal = maxinfo + nstinfo
  nele  = nreal   + nchanl
  allocate(data_all(nele,itxmax),nrec(itxmax))

  next=0
  nrec=999999
  irec=0

! Big loop over bufr file
  read_msg: do while(IREADMG(lnbufr,subset,idate) >= 0)
     irec=irec+1
     if(irec < nrec_start)cycle read_msg
     next=next+1
     if(next == npe_sub)next=0
     if(next /= mype_sub)cycle
     read_loop: do while (IREADSB(lnbufr) == 0)

!       Read through each reacord
        call ufbint(lnbufr,hdrgoesarr,nimghdr,1,iret,hdrgoes)
        if(hdrgoesarr(1) /= kidsat) cycle read_loop
        call ufbrep(lnbufr,dataimg,3,6,iret,'TMBRST NCLDMNT SDTB')
        nread=nread+nchanl
!      first step QC filter out data with less clear sky fraction
        if (hdrgoesarr(1) == 256_r_double .and. dataimg(2,3) < 70.0_r_kind) cycle read_loop
        if (hdrgoesarr(1) == 254_r_double .and. dataimg(2,3) < 40.0_r_kind) cycle read_loop
        if (hdrgoesarr(ilzah) >r60) cycle read_loop


!       Compare relative obs time with window.  If obs 
!       falls outside of window, don't use this obs
        idate5(1) = hdrgoesarr(2)     !year
        idate5(2) = hdrgoesarr(3)     ! month
        idate5(3) = hdrgoesarr(4)     ! day
        idate5(4) = hdrgoesarr(5)     ! hours
        idate5(5) = hdrgoesarr(6)     ! minutes
        call w3fs21(idate5,nmind)
        t4dv = (real((nmind-iwinbgn),r_kind) + real(hdrgoesarr(7),r_kind)*r60inv)*r60inv
        sstime = real(nmind,r_kind) + real(hdrgoesarr(7),r_kind)*r60inv
        tdiff=(sstime-gstime)*r60inv

        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           if (abs(tdiff)>twind) cycle read_loop
        endif

!       Convert obs location from degrees to radians
        if (hdrgoesarr(ilonh)>=r360) hdrgoesarr(ilonh)=hdrgoesarr(ilonh)-r360
        if (hdrgoesarr(ilonh)< zero) hdrgoesarr(ilonh)=hdrgoesarr(ilonh)+r360

        dlon_earth_deg=hdrgoesarr(ilonh)
        dlat_earth_deg=hdrgoesarr(ilath)
        dlon_earth=hdrgoesarr(ilonh)*deg2rad
        dlat_earth=hdrgoesarr(ilath)*deg2rad

!       If regional, map obs lat,lon to rotated grid.
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

!          Check to see if in domain.  outside=.true. if dlon_earth,
!          dlat_earth outside domain, =.false. if inside
           if(outside) cycle read_loop

!       Global case
        else
           dlon=dlon_earth
           dlat=dlat_earth
           call grdcrd1(dlat,rlats,nlat,1)
           call grdcrd1(dlon,rlons,nlon,1)
        endif

        crit0 = 0.01_r_kind
        timeinflat=6.0_r_kind
        call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)
        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)
        if(.not. iuse)cycle read_loop


!       Locate the observation on the analysis grid.  Get sst and land/sea/ice
!       mask.  

!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed                         


        call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
            ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)


!       Set common predictor parameters

        crit1=crit1+rlndsea(isflg)
        call checkob(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

!       Set data quality predictor 
        pred =(10.0_r_kind-dataimg(2,1)/10.0_r_kind)+dataimg(3,3)*10.0_r_kind  ! clear sky and
                                                                 ! bt std as quality indicater

!       Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
        crit1 = crit1+pred 
        call finalcheck(dist1,crit1,itx,iuse)
        if(.not. iuse) cycle read_loop

!       Map obs to grids
        if(hdrgoesarr(1) == 256_r_double) then
           dataimg(1,5)=dataimg(1,6)              ! use  brightness tem. 6 not 5
           dataimg(3,5)=dataimg(3,6)              ! use BT tem. var. 6 not 5 
        endif
        iscan = nint(hdrgoesarr(ilzah))+1.001_r_kind ! integer scan position
        
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

!       Transfer information to work array
        data_all( 1,itx) = hdrgoesarr(1)              ! satellite id
        data_all( 2,itx) = t4dv                       ! analysis relative time
        data_all( 3,itx) = dlon                       ! grid relative longitude
        data_all( 4,itx) = dlat                       ! grid relative latitude
        data_all( 5,itx) = hdrgoesarr(ilzah)*deg2rad  ! satellite zenith angle (radians)
        data_all( 6,itx) = hdrgoesarr(12)             ! satellite azimuth angle (radians)
        data_all( 7,itx) = dataimg(2,1)               ! clear sky amount
        data_all( 8,itx) = iscan                      ! integer scan position
        data_all( 9,itx) = hdrgoesarr(iszah)          ! solar zenith angle
        data_all(10,itx) = hdrgoesarr(13)             ! solar azimuth angle
        data_all(11,itx) = sfcpct(0)                  ! sea percentage of
        data_all(12,itx) = sfcpct(1)                  ! land percentage
        data_all(13,itx) = sfcpct(2)                  ! sea ice percentage
        data_all(14,itx) = sfcpct(3)                  ! snow percentage
        data_all(15,itx)= ts(0)                       ! ocean skin temperature
        data_all(16,itx)= ts(1)                       ! land skin temperature
        data_all(17,itx)= ts(2)                       ! ice skin temperature
        data_all(18,itx)= ts(3)                       ! snow skin temperature
        data_all(19,itx)= tsavg                       ! average skin temperature
        data_all(20,itx)= vty                         ! vegetation type
        data_all(21,itx)= vfr                         ! vegetation fraction
        data_all(22,itx)= sty                         ! soil type
        data_all(23,itx)= stp                         ! soil temperature
        data_all(24,itx)= sm                          ! soil moisture
        data_all(25,itx)= sn                          ! snow depth
        data_all(26,itx)= zz                          ! surface height
        data_all(27,itx)= idomsfc + 0.001_r_kind      ! dominate surface type
        data_all(28,itx)= sfcr                        ! surface roughness
        data_all(29,itx)= ff10                        ! ten meter wind factor
        data_all(30,itx)= dlon_earth_deg              ! earth relative longitude (degrees)
        data_all(31,itx)= dlat_earth_deg              ! earth relative latitude (degrees)

        if(dval_use)then
           data_all(32,itx) = val_img
           data_all(33,itx) = itt
        end if

        if ( nst_gsi > 0 ) then
           data_all(maxinfo+1,itx) = tref         ! foundation temperature
           data_all(maxinfo+2,itx) = dtw          ! dt_warm at zob
           data_all(maxinfo+3,itx) = dtc          ! dt_cool at zob
           data_all(maxinfo+4,itx) = tz_tr        ! d(Tz)/d(Tr)
        endif

!       Transfer observation location and other data to local arrays

        do k=1,nchanl
           data_all(k+31,itx)=dataimg(3,k+1)
           data_all(k+nreal,itx)=dataimg(1,k+1)
        end do
        nrec(itx)=irec

     enddo read_loop
  enddo read_msg

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
     nele,itxmax,nread,ndata,data_all,score_crit,nrec)

! If no observations read, jump to end of routine.
  if (mype_sub==mype_root.and.ndata>0) then

     do n=1,ndata
        do k=1,nchanl
           if(data_all(k+nreal,n) > tbmin .and. &
              data_all(k+nreal,n) < tbmax)nodata=nodata+1
       end do
     end do
     if(dval_use .and. assim)then
        do n=1,ndata
          itt=nint(data_all(maxinfo,n))
          super_val(itt)=super_val(itt)+val_img
        end do
     end if

! Write final set of "best" observations to output file
     call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)

  endif

! Deallocate local arrays
  deallocate(data_all,nrec)

! Deallocate satthin arrays
900 continue
  call destroygrids
  call closbf(lnbufr)
  close(lnbufr)

  if(diagnostic_reg.and.ntest>0) write(6,*)'READ_GOESIMG:  ',&
     'mype,ntest,disterrmax=',mype,ntest,disterrmax

  return
end subroutine read_goesimg
