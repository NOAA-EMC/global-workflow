subroutine read_modsbufr(nread,ndata,nodata,gstime,infile,obstype,lunout, &
          twindin,sis,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_modsbufr                read sst obs from modsbufr file (based on MODS)
!   prgmmr: Xu Li          org: np22                date: 2005-10-20
!
! abstract:  This routine reads conventional sst data from modsbufr
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2006-02-03  derber   - modify for new obs control and obs count
!   2006-02-08  derber   - modify to use new convinfo module
!   2006-02-24  derber   - modify to take advantage of convinfo module
!   2006-04-12  treadon  - remove unused variables from gridmod module
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2008-04-18  safford  - rm unused vars and uses
!   2011-04-08  li      - (1) use nst_gsi, nstinfo, fac_dtl, fac_tsl and add NSST vars
!                         (2) get zob, tz_tr (call skindepth and cal_tztr)
!                         (3) interpolate NSST Variables to Obs. location (call deter_nst)
!                         (4) add more elements (nstinfo) in data array
!   2011-08-01  lueken  - added module use deter_sfc_mod, fixed indentation
!   2012-03-05  akella  - nst now controlled via coupler
!   2012-08-29  akella  - (1) fix accumulation of ndata and nodata so that it is consistent with data_all
!                       - (2) use t4dv rather than tdiff in calls to deter_sfc
!                       - (3) use tsavg that is computed at observation depth
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2014-1-28   xli     - modify NSST related tz
!   2015-02-23  Rancic/Thomas - add l4densvar to time window logical
!   2015-10-01  guo     - consolidate use of ob location (in deg
!
!   input argument list:
!     infile   - unit from which to read BUFR data
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     twindin  - input group time window (hours)
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!     nodata   - number of individual "obstype" observations retained for further processing
!     sis      - satellite/instrument/sensor indicator
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind,r_single
  use mpimod, only: mype
  use constants, only: zero,one_tenth,quarter,half,one,deg2rad,&
      two,three,four,rad2deg,r60inv
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,&
      tll2xy,txy2ll,rlats,rlons
  use convinfo, only: nconvtype,ctwind, &
      ncmiter,ncgroup,ncnumgrp,icuse,ictype
  use obsmod, only: oberrflg,bmiss
  use insitu_info, only: n_comps,n_scripps,n_triton,n_3mdiscus,cid_mbuoy,n_ship,ship
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use deter_sfc_mod, only: deter_sfc
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_deter
  use mpimod, only: npe
  implicit none

! Declare passed variables
  character(len=*),intent(in):: infile,obstype
  character(len=20),intent(in):: sis
  integer(i_kind),intent(in):: lunout
  integer(i_kind),intent(inout):: nread,ndata,nodata
  integer(i_kind),dimension(npe),intent(inout):: nobs
  real(r_kind),intent(in):: gstime,twindin

! Declare local parameters
  integer(i_kind),parameter:: maxinfo = 18
  real(r_double),parameter:: d250 = 250.0_r_double
  real(r_double),parameter:: d350 = 350.0_r_double
  real(r_kind),parameter:: r0_2  = 0.20_r_kind
  real(r_kind),parameter:: r0_45 = 0.45_r_kind
  real(r_kind),parameter:: r0_6  = 0.60_r_kind
  real(r_kind),parameter:: r1_2  = 1.20_r_kind
  real(r_kind),parameter:: r1_5  = 1.50_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind

! Declare local variables
  logical outside

  integer(i_kind) lunin,i,maxobs
  integer(i_kind) idate,iret,k
  integer(i_kind) kx,nreal,nchanl,ilat,ilon
  integer(i_kind) sstq,nmind
  integer(i_kind):: isflg,idomsfc

  integer(i_kind) :: ireadmg,ireadsb,klev
  integer(i_kind), dimension(5) :: idate5
  character(len=8)  :: subset
  character(len=8)  :: crpid
  character(len=80) :: headr
  character(len=5)  :: cid
  real(r_double), dimension(9) :: hdr(9)
  real(r_double), dimension(2,255) :: tpf
  real(r_double)  :: msst,sst
  equivalence (crpid,hdr(9))

  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts

  real(r_kind) :: tdiff,sstime,usage,sfcr,tsavg,ff10,t4dv
  real(r_kind) :: vty,vfr,sty,stp,sm,sn,zz
  real(r_kind) :: dlat,dlon,sstoe,dlat_earth,dlon_earth
  real(r_kind) :: dlat_earth_deg,dlon_earth_deg
  real(r_kind) :: zob,tref,dtw,dtc,tz_tr,tz

  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  integer(i_kind) ntest

  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_single),allocatable::etabl(:,:,:)
  integer(i_kind) ietabl,lcount,itypex
  integer(i_kind) l,m,ikx
  integer(i_kind) n,cid_pos,ship_mod
  real(r_kind) terrmin,werrmin,perrmin,qerrmin,pwerrmin

  data headr/'YEAR MNTH DAYS HOUR MINU CLATH CLONH SELV RPID'/
  data lunin / 10 /
!**************************************************************************
! Initialize variables
  disterrmax=zero
  ntest=0
  maxobs=2e6
  nread=0
  ndata=0
  nodata=0
  nchanl=0
  ilon=2
  ilat=3

  nreal=maxinfo+nstinfo

  allocate(data_all(nreal,maxobs))
  data_all = zero


  if(oberrflg)then
     allocate(etabl(300,33,6))
     ietabl=19
     open(ietabl,file='errtable',form='formatted')
     rewind ietabl
     etabl=1.e9_r_kind
     lcount=0
     do l=1,300
        read(ietabl,100,end=120,err=120)itypex
100     format(1x,i3)
        lcount=lcount+1
        do k=1,33
           read(ietabl,110)(etabl(itypex,k,m),m=1,6)
110        format(1x,6e12.5)
        end do
     end do
120  continue
     if(lcount<=0) then
        write(6,*)'READ_MODSBUFR:  ***WARNING*** obs error table not available to 3dvar.'
        oberrflg=.false.
     end if
     close(ietabl)

!    Set lower limits for observation errors
     terrmin=half
     werrmin=one
     perrmin=half
     qerrmin=one_tenth
     pwerrmin=one
     
  endif

! Open, then read date from bufr data
  open(lunin,file=trim(infile),form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)
       
! READING EACH REPORT FROM BUFR
       
  do while (ireadmg(lunin,subset,idate) == 0)
     read_loop: do while (ireadsb(lunin) == 0)
        call ufbint(lunin,hdr,9,1,iret,headr)

!          Measurement types
!             0       Ship intake
!             1       Bucket
!             2       Hull contact sensor
!             3       Reversing Thermometer
!             4       STD/CTD sensor
!             5       Mechanical BT
!             6       Expendable BT
!             7       Digital BT
!             8       Thermistor chain
!             9       Infra-red scanner
!             10      Micro-wave scanner
!             11-14   Reserved
! data headr/'YEAR MNTH DAYS HOUR MINU CLATH CLONH SELV RPID'/
!
!     Determine measurement type
!

        if ( trim(subset) == 'SHIPS' .or. trim(subset) == 'MBUOY' .or. &
             trim(subset) == 'LCMAN' ) then
           call ufbint(lunin,msst,1,1,iret,'MSST')          ! for ships, fixed buoy and lcman
           call ufbint(lunin,sst,1,1,iret,'SST1')           ! read SST
        elseif ( trim(subset) == 'DBUOY' ) then
           msst = 11.0_r_kind                                      ! for drifting buoy, assign to be 11
           call ufbint(lunin,sst,1,1,iret,'SST1')
        elseif ( trim(subset) == 'TESAC' ) then
           msst = 12.0_r_kind                                      ! for ARGO, assign to be 12
           call ufbint(lunin,tpf,2,255,klev,'DBSS STMP')    ! read T_Profile
           if ( tpf(1,1) < 5.0_r_kind ) then
              sst = tpf(2,1)
           else
              sst = bmiss
           endif
        elseif ( trim(subset) == 'BATHY' ) then
           msst = 13.0_r_kind                                      ! for BATHY, assign to be 13
           call ufbint(lunin,tpf,2,255,klev,'DBSS STMP')           ! read T_Profile
           if ( tpf(1,1) < 5.0_r_kind ) then
              sst = tpf(2,1)
           else
              sst = bmiss
           endif
        elseif ( trim(subset) == 'TRKOB' ) then
           msst = 14.0_r_kind                                      ! for TRKOB, assign to be 14
           call ufbint(lunin,tpf,2,255,klev,'DBSS STMP')           ! read T_Profile
           if ( tpf(1,1) < 1.0_r_kind ) then
              sst = tpf(2,1)
           else
              sst = bmiss
           endif
        elseif ( trim(subset) == 'TIDEG' ) then
           msst = 15.0_r_kind                                      ! for TIDEG, assign to be 15
           call ufbint(lunin,sst,1,1,iret,'SST1')                  ! read SST
        elseif ( trim(subset) == 'CSTGD' ) then
           msst = 16.0_r_kind                                      ! for CSTGD, assign to be 16
           call ufbint(lunin,sst,1,1,iret,'SST1')                  ! read SST
        endif

        nread = nread + 1

        if (  sst > d250 .and. sst < d350 ) then

           cid = trim(crpid)
!          Extract type, date, and location information
           if(hdr(7) >= r360)  hdr(7) = hdr(7) - r360
           if(hdr(7) <  zero)  hdr(7) = hdr(7) + r360

           dlon_earth_deg=hdr(7)
           dlat_earth_deg=hdr(6)
           dlon_earth=hdr(7)*deg2rad
           dlat_earth=hdr(6)*deg2rad

           if(regional)then
              call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
              if(diagnostic_reg) then
                 call txy2ll(dlon,dlat,rlon00,rlat00)
                 ntest=ntest+1
                 cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                      (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
                 cdist=max(-one,min(cdist,one))
                 disterr=acos(cdist)*rad2deg
                 disterrmax=max(disterrmax,disterr)
              end if
              if(outside) cycle read_loop    ! check to see if outside regional domain
           else
              dlat = dlat_earth
              dlon = dlon_earth
              call grdcrd1(dlat,rlats,nlat,1)
              call grdcrd1(dlon,rlons,nlon,1)
           endif

!          Extract date information.  If time outside window, skip this obs
           idate5(1) = nint(hdr(1))    !year
           idate5(2) = nint(hdr(2))    !month
           idate5(3) = nint(hdr(3))    !day
           idate5(4) = nint(hdr(4))    !hour
           idate5(5) = nint(hdr(5))    !minute
 
!          determine platform (ships, dbuoy, fbuoy or lcman and so on) dependent zob and obs. error
!
           if ( trim(subset) == 'SHIPS' ) then                                            ! ships
              ship_mod = 0
              do n = 1, n_ship
                 if ( crpid == trim(ship%id(n)) ) then
                    ship_mod = 1
                    zob = ship%depth(n)
                    if ( trim(ship%sensor(n)) == 'BU' ) then
                       kx = 181
                       sstoe = r1_5
                    elseif ( trim(ship%sensor(n)) == 'C' ) then
                       kx = 182
                       sstoe = two
                    elseif ( trim(ship%sensor(n)) == 'HC' ) then
                       kx = 183
                       sstoe = two
                    elseif ( trim(ship%sensor(n)) == 'BTT' ) then
                       kx = 184
                       sstoe = two
                    elseif ( trim(ship%sensor(n)) == 'HT' ) then
                       kx = 185
                       sstoe = two
                    elseif ( trim(ship%sensor(n)) == 'RAD' ) then
                       kx = 186
                       sstoe = two
                    elseif ( trim(ship%sensor(n)) == 'TT' ) then
                       kx = 187
                       sstoe = two
                    elseif ( trim(ship%sensor(n)) == 'OT' ) then
                       kx = 188
                       sstoe = two
                    else
                       kx = 189
                       sstoe = three
                    endif
                 endif
              enddo

              if ( ship_mod == 0 ) then
                 if ( msst == two ) then                                  ! positive or zero bucket
                    kx = 181
                    sstoe = two
                    zob = one
                 elseif ( msst == zero .or. msst == one ) then            ! positive/negative/zero intake
                    kx = 182
                    sstoe = 2.5_r_kind
                    zob = three
                 else
                    kx = 189
                    zob = 2.5_r_kind
                    sstoe = three
                 endif
              endif


           elseif ( trim(subset) == 'DBUOY'  ) then

              cid_pos = 0

              do n = 1, n_3mdiscus
                 if ( cid == cid_mbuoy(n) ) then
                    cid_pos = n
                 endif
              enddo
 
              if ( cid_pos >= 1 .and. cid_pos <= n_comps ) then                                                  ! COMPS moored buoy

                 zob = r1_2
                 kx = 192
                 sstoe = half

              elseif ( cid_pos > n_scripps .and. cid_pos <= n_triton ) then                   ! Triton buoy
 
                 zob = r1_5
                 kx = 194
                 sstoe = 0.4_r_kind
 
              elseif ( cid_pos == 0 ) then
 
                 zob = r0_2
                 if ( cid(3:3) == '5' .or. cid(3:3) == '6' .or. cid(3:3) == '7' .or. cid(3:3) == '8' .or. cid(3:3) == '9' ) then
                    kx = 190
                    sstoe = r0_6
                 elseif ( cid(3:3) == '0' .or. cid(3:3) == '1' .or. cid(3:3) == '2' .or. cid(3:3) == '3' .or. cid(3:3) == '4') then
                    kx = 191
                    sstoe = half
                 endif

              endif

           elseif ( trim(subset) == 'MBUOY' ) then

              cid_pos = 0

              do n = 1, n_3mdiscus
                 if ( cid == cid_mbuoy(n) ) then
                    cid_pos = n
                 endif
              enddo

              if ( cid_pos >= 1 .and. cid_pos <= n_comps ) then                            ! COMPS moored buoy
                 zob = r1_2
                 kx = 192
                 sstoe = one
              elseif ( cid_pos > n_comps .and. cid_pos <= n_scripps ) then                    ! SCRIPPS moored buoy
                 zob = r0_45
                 kx = 193
                 sstoe = 1.5_r_kind
              elseif ( cid_pos > n_scripps .and. cid_pos <= n_triton ) then                   ! Triton buoy
                 zob = r1_5
                 kx = 194
                 sstoe = 0.4_r_kind
              elseif ( cid_pos > n_triton .and. cid_pos <= n_3mdiscus ) then                  ! Moored buoy with 3-m discus
                 zob = r0_6
                 kx = 195
                 sstoe = 1.5_r_kind
              elseif ( cid_pos == 0 ) then                                                    ! All other moored buoys (usually with 1-m observation depth)
                 zob = one
                 kx = 196
                 sstoe = one
              endif

           elseif ( trim(subset) == 'LCMAN' ) then                                            ! lcman
              zob = one
              kx = 197
              sstoe = 2.5_r_kind
           elseif ( trim(subset) == 'TESAC' ) then                                            ! ARGO
              if (  tpf(1,1) >= one .and.  tpf(1,1) < 5.0_r_kind ) then
                 zob = tpf(1,1)
              elseif (  tpf(1,1) >= zero .and. tpf(1,1) < one ) then
                 zob = one
              endif
              kx = 198
              sstoe = 2.5_r_kind 
           elseif ( trim(subset) == 'BATHY' ) then                                            ! ARGO
              if (  tpf(1,1) >= one .and.  tpf(1,1) < 5.0_r_kind ) then
                 zob = tpf(1,1)
              elseif (  tpf(1,1) >= zero .and. tpf(1,1) < one ) then
                 zob = one
              endif
              kx = 199
              sstoe = half
           elseif ( trim(subset) == 'TRKOB' ) then                                            ! trkob
              zob = one
              kx = 200
              sstoe = two
           elseif ( trim(subset) == 'TIDEG' ) then                                            ! tideg
              zob = one
              kx = 201
              sstoe = two
           elseif ( trim(subset) == 'CSTGD' ) then                                            ! cstgd
              zob = one
              kx = 202
              sstoe = two
           endif
!
!          Determine usage
!
           ikx = 0
           do i = 1, nconvtype
              if(kx == ictype(i) .and. abs(icuse(i))== 1) ikx=i
           end do

           if(ikx == 0) cycle read_loop             ! not ob type used

           call w3fs21(idate5,nmind)
           t4dv=real((nmind-iwinbgn),r_kind)*r60inv  ! no information in obs bufr file about seconds.
!
           if (l4dvar.or.l4densvar) then
              if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
           else
              sstime=real(nmind,r_kind)
              tdiff=(sstime-gstime)*r60inv
              if(abs(tdiff)>twindin .or. abs(tdiff)>ctwind(ikx)) cycle read_loop ! outside time window
           endif
 
!          If running in 2d-var (surface analysis) mode, check to see if observation
!          is surface type.  If not, read next observation report from bufr file
!          if ( twodvar_regional .and. &
!             (kx<180 .or. kx>289 .or. (kx>202 .and. kx<280)) ) cycle read_loop

           usage = zero
           if (   icuse(ikx) < 0 ) usage = 100.0_r_kind
           if ( ncnumgrp(ikx) > 0 ) then                                ! cross validation on
              if (mod(ndata+1,ncnumgrp(ikx))== ncgroup(ikx)-1) usage=ncmiter(ikx)
           end if

!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed
!      sfcpct(0:3)- percentage of 4 surface types
!                 (0) - sea percentage
!                 (1) - land percentage
!                 (2) - sea ice percentage
!                 (3) - snow percentage

           call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
                          ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)

           if( idomsfc /= zero)  cycle read_loop                         ! use data over water only

           nodata = nodata + 1
           ndata = ndata + 1
           if(ndata > maxobs) then
              write(6,*)'READ_MODSBUFR:  ***WARNING*** ndata > maxobs for ',obstype
              ndata = maxobs
           end if

!
!          interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
!
           if(nst_gsi>0) then
              tref  = tsavg
              dtw   = zero
              dtc   = zero
              tz_tr = one
              if(isflg == zero) then
                 call gsi_nstcoupler_deter(dlat_earth,dlon_earth,t4dv,zob,tref,dtw,dtc,tz_tr)
                 tz = tref
                 if ( nst_gsi > 2 ) then
                    tz = tref+dtw-dtc            ! Tz: Background temperature at depth of zob
                 endif
              endif

           endif

           nodata = nodata + 1
           ndata = ndata + 1
           if(ndata > maxobs) then
              write(6,*)'READ_MODSBUFR:  ***WARNING*** ndata > maxobs for ',obstype
              ndata = maxobs
           end if

           data_all(1,ndata)  = sstoe                   ! sst error
           data_all(2,ndata)  = dlon                    ! grid relative longitude
           data_all(3,ndata)  = dlat                    ! grid relative latitude
           data_all(4,ndata)  = sst                     ! sst obs
           data_all(5,ndata)  = hdr(9)                  ! station id
           data_all(6,ndata)  = t4dv                    ! time
           data_all(7,ndata)  = ikx                     ! type
           data_all(8,ndata)  = sstoe*three             ! max error
           data_all(9,ndata)  = zob                     ! depth of measurement
           data_all(10,ndata) = kx                      ! measurement type
           data_all(11,ndata) = sstq                    ! quality mark
           data_all(12,ndata) = sstoe                   ! original obs error
           data_all(13,ndata) = usage                   ! usage parameter
           data_all(14,ndata) = idomsfc+0.001_r_kind    ! dominate surface type
           data_all(15,ndata) = tz                      ! Tz: Background temperature at depth of zob
           data_all(16,ndata) = dlon_earth_deg          ! earth relative longitude (degrees)
           data_all(17,ndata) = dlat_earth_deg          ! earth relative latitude (degrees)
           data_all(18,ndata) = hdr(8)                  ! station elevation

           if(nst_gsi>0) then
              data_all(maxinfo+1,ndata) = tref           ! foundation temperature
              data_all(maxinfo+2,ndata) = dtw            ! dt_warm at zob
              data_all(maxinfo+3,ndata) = dtc            ! dt_cool at zob
              data_all(maxinfo+4,ndata) = tz_tr          ! d(Tz)/d(Tr)
           endif

        end if                                               ! if (  sst > d250 .and. sst < d350 ) then
     enddo read_loop
  enddo
!
!   End of bufr read loop
       
! Normal exit
1000 continue

  if ( ndata > 0 ) then 
    ! Write header record and data to output file for further processing
    call count_obs(ndata,nreal,ilat,ilon,data_all,nobs)
    write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
    write(lunout) ((data_all(k,i),k=1,nreal),i=1,ndata)
  endif

! Deallocate local arrays
  deallocate(data_all)

  write(6,*) 'read_modsbufr: mype = ', mype, 'nread =', nread, 'ndata =', ndata, 'nodata =', nodata

! Close unit to bufr file
1020 continue
  if (oberrflg) deallocate(etabl)
  call closbf(lunin)
  close(lunin)

  if(regional)then
    if(diagnostic_reg.and.ntest > 0) write(6,*)'READ_MODSBUFR:  ',&
     'ntest,disterrmax=',ntest,disterrmax
  endif

! End of routine
  return
end subroutine read_modsbufr
