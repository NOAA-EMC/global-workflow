subroutine read_abi(mype,val_abi,ithin,rmesh,jsatid,&
     gstime,infile,lunout,obstype,nread,ndata,nodata,twind,sis, &
     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs, &
     nrec_start,dval_use)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_abi                  read abi bufr data
!   prgmmr: liu, haixia             org: np23                date: 2018-02-21
!
! abstract:  This routine reads BUFR format ABI 1b radiance (brightness
!            temperature) files, which are bufrized from the NESDIS 1b data.  Optionally, the
!            data are thinned to a specified resolution using simple
!            quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2018-02-21  hliu start the read_abi routine
!
!   input argument list:
!     mype     - mpi task id
!     val_abi  - weighting factor applied to super obs
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
!     nread    - number of BUFR ABI 1b observations read
!     ndata    - number of BUFR ABI 1b profiles retained for further processing
!     nodata   - number of BUFR ABI 1b observations retained for further processing
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
  use constants, only: deg2rad,zero,one,rad2deg,r60inv
  use obsmod, only: bmiss
  use radinfo, only: iuse_rad,jpch_rad,nusis
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use deter_sfc_mod, only: deter_sfc
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_skindepth, gsi_nstcoupler_deter
  use mpimod, only: npe
  implicit none

! Declare passed variables
  character(len=*),intent(in):: infile,obstype,jsatid
  character(len=20),intent(in):: sis
  integer(i_kind),intent(in):: mype,lunout,ithin,nrec_start
  integer(i_kind),intent(inout):: ndata,nodata
  integer(i_kind),intent(inout):: nread
  integer(i_kind),dimension(npe),intent(inout):: nobs
  real(r_kind),intent(in):: rmesh,gstime,twind
  real(r_kind),intent(inout):: val_abi
  integer(i_kind),intent(in) :: mype_root
  integer(i_kind),intent(in) :: mype_sub
  integer(i_kind),intent(in) :: npe_sub
  integer(i_kind),intent(in) :: mpi_comm_sub
  logical        ,intent(in) :: dval_use

! Declare local parameters
  real(r_kind),parameter:: r70=70.0_r_kind
  real(r_kind),parameter:: r65=65.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind

! Declare local variables
  logical outside,iuse,assim,clrsky,allsky

  character(8) subset,subcsr,subasr
  character(80):: hdrabi              ! abi header

  integer(i_kind) nchanl,ilath,ilonh,ilzah,iszah,irec,next,ilazi,isazi
  integer(i_kind) nmind,lnbufr,idate,ilat,ilon,nhdr,nchn,ncld,nbrst,jj
  integer(i_kind) ireadmg,ireadsb,iret,nreal,nele,itt
  integer(i_kind) itx,i,k,isflg,kidsat,n,iscan,idomsfc
  integer(i_kind) idate5(5),maxinfo
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
  real(r_kind),allocatable,dimension(:,:):: data_all

  real(r_kind),allocatable,dimension(:):: hdr                        !  abi imager header
  real(r_kind),allocatable,dimension(:,:):: dataabi1,dataabi2,dataabi,dataabi3        !  abi imager data
  real(r_kind) rclrsky,rcldfrc
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr

  real(r_kind) cdist,disterr,disterrmax,dlon00,dlat00
  integer(i_kind) ntest
  real(r_kind)    :: ptime,timeinflat,crit0
  integer(i_kind) :: ithin_time,n_tbin,it_mesh

  logical :: allchnmiss

!**************************************************************************
! Initialize variables
  maxinfo=32
  lnbufr = 10
  disterrmax=zero
  ntest=0
  dg2ew = r360*deg2rad

  ilon=3
  ilat=4

  if (nst_gsi > 0 ) then
     call gsi_nstcoupler_skindepth(obstype, zob)         ! get penetration depth (zob) for the obstype
  endif

! HLIU: NEED TO confirm
  rlndsea(0) = zero
  rlndsea(1) = 15._r_kind
  rlndsea(2) = 10._r_kind
  rlndsea(3) = 15._r_kind
  rlndsea(4) = 30._r_kind

  nread=0
  ndata=0
  nodata=0
  nchanl=10                    ! total # of IR channels

  ilath=8                      ! the position of latitude in the header
  ilonh=9                      ! the position of longitude in the header
  ilzah=10                     ! satellite zenith angle
  ilazi=11                     ! satellite azimuth angle
  iszah=12                     ! solar zenith angle
  isazi=13                     ! solar azimuth angle
  subcsr='NC021046'            ! sub message
  subasr='NC021045'            ! sub message

! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.

  assim=.false.
  search: do i=1,jpch_rad
     if ((trim(nusis(i))==trim(sis)) .and. (iuse_rad(i)>0)) then
        assim=.true.
        exit search
     endif
  end do search
  if (.not.assim) val_abi=zero

! Open bufr file.
  open(lnbufr,file=trim(infile),form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)
  call readmg(lnbufr,subset,idate,iret)

! Check the data set
  if( iret/=0) then
     write(6,*) 'READ_ABI: SKIP PROCESSING OF ABI FILE'
     write(6,*) 'infile=', lnbufr, infile
     return
  endif

  clrsky=.false.
  allsky=.false.
  if(subset == subcsr) then
     clrsky=.true.
  elseif(subset == subasr) then
     allsky=.true.
  else
     write(6,*) 'READ_ABI: SKIP PROCESSING OF ABI FILE'
     write(6,*) 'infile=', lnbufr, infile,' subset=', subset
     return
  endif

  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if( ptime > 0.0_r_kind) then
     n_tbin=nint(2*time_window_max/ptime)
  else
     n_tbin=1
  endif
! Make thinning grids
  call makegrids(rmesh,ithin,n_tbin=n_tbin)

! Set BUFR string based on abi data set
  hdrabi='SAID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA BEARAZ SOZA SOLAZI'
  nhdr=13
  if (clrsky) then
     nchn=10
     ncld=nchn
     nbrst=nchn
  else if (allsky) then
     nchn=10
     ncld=2
     nbrst=nchn*6                ! channel dependent: all, clear, cloudy, low, middle and high clouds
  endif
  allocate(dataabi(1,4))         ! CLDMNT for ASR: not channel dependent
  allocate(dataabi1(1,ncld))     ! NCLDMNT: 2 for ASR, not channel dependent; ncld for CSR, chn dependent
  allocate(dataabi2(1,nbrst))    ! BT: channel dependent: all, clear, cloudy, low, middle and high clouds
  allocate(dataabi3(1,nbrst))    ! SDTB: channel dependent: all, clear, cloudy, low, middle and high clouds
  allocate(hdr(nhdr))


! Allocate arrays to hold all data for given satellite
  maxinfo=maxinfo+nchanl
  if(dval_use) maxinfo = maxinfo + 2
  nreal = maxinfo + nstinfo
  nele  = nreal   + nchanl
  allocate(data_all(nele,itxmax),nrec(itxmax))


!  Reopen unit to bufr file
  call closbf(lnbufr)
  close(lnbufr)
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  if(jsatid == 'gr' .or. jsatid == 'g16') then
     kidsat = 270
  elseif (jsatid == 'g17') then
     kidsat = 271
  elseif (jsatid == 'g18') then 
     kidsat = 272
  else
     write(6,*) 'READ_ABI: Unrecognized value for jsatid '//jsatid//': RETURNING'
     return
  end if


  nrec=999999
  irec=0
  next=0
! Big loop over bufr file
  read_msg: do while (ireadmg(lnbufr,subset,idate) >= 0)
     irec=irec+1
     if(irec < nrec_start) cycle read_msg
     next=next+1
     if(next == npe_sub)next=0     
     if(next /= mype_sub)cycle

     read_loop: do while (ireadsb(lnbufr) == 0)

!       Read through each record
        call ufbint(lnbufr,hdr,nhdr,1,iret,hdrabi)
        if(nint(hdr(1)) /= kidsat) cycle read_loop
!       remove the obs whose satellite zenith angles larger than 65 degree
        if ( hdr(ilzah) > r65 ) then
          cycle read_loop
        end if

!       Convert obs location from degrees to radians
        if (hdr(ilonh)>=r360) hdr(ilonh)=hdr(ilonh)-r360
        if (hdr(ilonh)< zero) hdr(ilonh)=hdr(ilonh)+r360

        dlon_earth_deg=hdr(ilonh)
        dlat_earth_deg=hdr(ilath)
        dlon_earth=hdr(ilonh)*deg2rad
        dlat_earth=hdr(ilath)*deg2rad

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

!       Compare relative obs time with window.  If obs 
!       falls outside of window, don't use this obs
        idate5(1) = hdr(2)     ! year
        idate5(2) = hdr(3)     ! month
        idate5(3) = hdr(4)     ! day
        idate5(4) = hdr(5)     ! hours
        idate5(5) = hdr(6)     ! minutes
        call w3fs21(idate5,nmind)
        t4dv = (real((nmind-iwinbgn),r_kind) + real(hdr(7),r_kind)*r60inv)*r60inv
        sstime = real(nmind,r_kind) + real(hdr(7),r_kind)*r60inv
        tdiff=(sstime-gstime)*r60inv
!    remove the tdiff QC check
        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           if (abs(tdiff)>twind) cycle read_loop
        endif

        crit0 = 0.01_r_kind
        timeinflat=6.0_r_kind
        call tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)
        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh=it_mesh)
        if(.not. iuse)cycle read_loop

        nread=nread+nchanl

        rcldfrc=bmiss
        if(clrsky) then 
           call ufbrep(lnbufr,dataabi1,1,ncld,iret,'NCLDMNT')
           rclrsky=bmiss
!          dataabi1(1,2) is high-peaking water vapor channel
!          for ABI CSR, clear-sky percentage are the same for all the channels
           if(dataabi1(1,2)>= zero .and. dataabi1(1,2) <= 100.0_r_kind ) then
              rclrsky=dataabi1(1,2)
!             first QC filter out data with less clear sky fraction
              if ( rclrsky < r70 ) cycle read_loop
           end if
        else if(allsky) then
           call ufbrep(lnbufr,dataabi1,1,2,iret,'NCLDMNT')
           rclrsky=dataabi1(1,1)  !clear-sky percentage
!          rclrsky=dataabi1(1,2)  !clear-sky percentage over sea
           call ufbrep(lnbufr,dataabi,1,4,iret,'CLDMNT')
           if (dataabi(1,1)>= zero .and. dataabi(1,1) <= 100.0_r_kind ) then
              rcldfrc=dataabi(1,1)   !total cloud 
           end if
        end if

        call ufbrep(lnbufr,dataabi2,1,nbrst,iret,'TMBRST')
        call ufbrep(lnbufr,dataabi3,1,nbrst,iret,'SDTB')
 
!       toss data if SDTB>1.3 
        if(clrsky) then
          do i=1,nbrst
            if(i==2 .or. i==3 .or. i==4) then   ! 3 water-vapor channels
              if(dataabi3(1,i)>1.3_r_kind) cycle read_loop
            end if
          end do
        end if

        allchnmiss=.true.
        do n=1,nchn
           if(clrsky) then
             if(dataabi2(1,n)<500.0_r_kind)  then
                allchnmiss=.false.
             end if
           else if(allsky) then
             jj=(n-1)*6+1
             if(dataabi2(1,jj)<500.0_r_kind) then
                allchnmiss=.false.
             end if
           end if
        end do
        if(allchnmiss) then
          cycle read_loop
        end if

!       Locate the observation on the analysis grid.  Get sst and land/sea/ice
!       mask.  

!       isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed                         


        call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
           ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)

        crit1=crit1+rlndsea(isflg)
        call checkob(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop


!       Set common predictor parameters
        if(clrsky) then
!         use NCLDMNT from chn7 (10.8 micron) as a QC predictor
!         add SDTB from chn7 as QC predictor
          pred=10.0_r_kind-dataabi1(1,7)/10.0_r_kind+dataabi3(1,7)*10.0_r_kind
        else
          pred=zero
        end if
!            
!       Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"

        crit1 = crit1+pred  
        call finalcheck(dist1,crit1,itx,iuse)

        if(.not. iuse)cycle read_loop

        iscan = nint(hdr(ilzah))+1.001_r_kind ! integer scan position HLIU check this
 
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

      ndata=ndata+1

!       Transfer information to work array
        data_all( 1,itx) = hdr(1)                     ! satellite id
        data_all( 2,itx) = t4dv                       ! analysis relative time
        data_all( 3,itx) = dlon                       ! grid relative longitude
        data_all( 4,itx) = dlat                       ! grid relative latitude
        data_all( 5,itx) = hdr(ilzah)*deg2rad         ! satellite zenith angle (radians)
        data_all( 6,itx) = hdr(ilazi)*deg2rad         ! satellite azimuth angle (radians)
        data_all( 7,itx) = rclrsky                    ! clear sky amount
        data_all( 8,itx) = iscan                      ! integer scan position
        data_all( 9,itx) = hdr(iszah)                 ! solar zenith angle
        data_all(10,itx) = hdr(isazi)                 ! solar azimuth angle
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
        data_all(30,itx) = dlon_earth_deg             ! earth relative longitude (degrees)
        data_all(31,itx) = dlat_earth_deg             ! earth relative latitude (degrees)
        data_all(32,itx) = rcldfrc                    ! total cloud fraction from ABIASR
        do k=1,nchanl
           if(clrsky) then
             data_all(32+k,itx) = dataabi3(1,k)       ! BT standard deviation from ABICSR
           else if(allsky) then
             jj=(k-1)*6+1
             data_all(32+k,itx) = dataabi3(1,jj)      ! BT standard deviation from ABIASR 
           end if
        end do

        if(dval_use)then
           data_all(maxinfo-1,itx) = val_abi
           data_all(maxinfo,itx) = itt
        end if

        if ( nst_gsi > 0 ) then
           data_all(maxinfo+1,itx) = tref         ! foundation temperature
           data_all(maxinfo+2,itx) = dtw          ! dt_warm at zob
           data_all(maxinfo+3,itx) = dtc          ! dt_cool at zob
           data_all(maxinfo+4,itx) = tz_tr        ! d(Tz)/d(Tr)
        endif

        do k=1,nchanl
           if (clrsky) then
              data_all(k+nreal,itx)=dataabi2(1,k)       ! for chn7,8,9,10,11,12,13,14,15,16
           else if (allsky) then
              jj=(k-1)*6+1
              data_all(k+nreal,itx)=dataabi2(1,jj)      ! all-sky radiance for chn 4,5,6,7,8,9,10,11
           end if
        end do
        nrec(itx)=irec

!    End of satellite read block
     enddo read_loop
  enddo read_msg

  call closbf(lnbufr)
  close(lnbufr)

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
     nele,itxmax,nread,ndata,data_all,score_crit,nrec)

! Allow single task to check for bad obs, update superobs sum,
! and write out data to scratch file for further processing.
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
          super_val(itt)=super_val(itt)+val_abi
       end do
    end if

!   Write retained data to local file
    call count_obs(ndata,nele,ilat,ilon,data_all,nobs)
    write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
    write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)

  endif

! Deallocate local arrays
  deallocate(data_all,nrec)
  deallocate(hdr,dataabi2,dataabi1,dataabi,dataabi3)

! Deallocate satthin arrays
  call destroygrids

! Print data counts
! write(6,9000) infile,sis,nread,rmesh,ndata
!9000 format(' READ_ABI:  infile=',a10,&
!       '   sis=',a20,&
!       '   nread=',i10, &
!       '   rmesh=',f7.3,'   ndata=',i10)

  if(diagnostic_reg.and.ntest>0) write(6,*)'READ_ABI:  ',&
     'mype,ntest,disterrmax=',mype,ntest,disterrmax

! End of routine
  return
end subroutine read_abi
