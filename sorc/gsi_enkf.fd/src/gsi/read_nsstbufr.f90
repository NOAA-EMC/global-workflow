subroutine read_nsstbufr(nread,ndata,nodata,gstime,infile,obstype,lunout, &
          twindin,sis,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_nsstbufr                read sst obs from nsstbufr file 
!   prgmmr: Xu Li          org: np22                date: 2012-01-04
!
! abstract:  This routine reads conventional sst data from nsstbufr
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2012-01-04  li      - modified based on read_nsstbufr.f90
!   2015-03-06  Thomas  - added l4densvar logical to remove thinning in time
!   2015-05-30  Li      - Modify to use deter_sfc instead of deter_sfc2
!   2015-06-01  Li      - Modify to make it work when nst_gsi = 0 and nsstbufr data file exists
!   2016-03-11  j. guo  - Fixed {dlat,dlon}_earth_deg in the obs data stream
!   2019-01-15  Li      - modify to handle dbuoyb (NC001102) and mbuoyb (NC001103)
!   2021-02-15  Li      - modify to handle bufr ships, restricted (nc001101,shipsb) and
!                         unrestricted (NC001113, shipub), and bufr land based lcman (nc001104)
!   2021-09-09  Li      - modify to handle bufr ships, restricted (nc001013,shipsu)
!   2021-12-14  Li      - modify to handle bufr Saildrone sea water temperature Obs. (001120)
!   2022-01-12  Li      - modify to handle bufr subpfl sea water temperature (Argo & Glider) Obs. (031005)
!   2022-01-12  Li      - Modify the maxmum depth to be 20 m for the shallowest T of the T-Profile Obs. 
!  
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
  use constants, only: zero,one_tenth,quarter,half,one,deg2rad,&
      two,three,four,rad2deg,r60inv
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,&
      tll2xy,txy2ll,rlats,rlons
  use convinfo, only: nconvtype,ctwind, &
      ncmiter,ncgroup,ncnumgrp,icuse,ictype
  use obsmod, only: oberrflg
  use insitu_info, only: n_comps,n_scripps,n_triton,n_3mdiscus,cid_mbuoy,cid_mbuoyb,n_ship,ship
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use deter_sfc_mod, only: deter_sfc,deter_sfc2
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use gsi_nstcouplermod, only: gsi_nstcoupler_deter
  use mpimod, only: npe
  implicit none

! Declare passed variables
  character(len=*),intent(in):: infile,obstype
  character(len=*),intent(in):: sis
  integer(i_kind),intent(in):: lunout
  integer(i_kind),intent(inout):: nread,ndata,nodata
  integer(i_kind),dimension(npe),intent(inout):: nobs
  real(r_kind),intent(in):: gstime,twindin

! Declare local parameters
  integer(i_kind),parameter:: maxinfo = 18
  real(r_double),parameter:: d250 = 250.0_r_double
  real(r_double),parameter:: d350 = 350.0_r_double
  real(r_kind),parameter:: r0_1  = 0.10_r_kind
  real(r_kind),parameter:: r0_15 = 0.15_r_kind
  real(r_kind),parameter:: r0_2  = 0.20_r_kind
  real(r_kind),parameter:: r0_4  = 0.40_r_kind
  real(r_kind),parameter:: r0_45 = 0.45_r_kind
  real(r_kind),parameter:: r0_6  = 0.60_r_kind
  real(r_kind),parameter:: r1_2  = 1.20_r_kind
  real(r_kind),parameter:: r1_5  = 1.50_r_kind
  real(r_kind),parameter:: r24   = 24.0_r_kind
  real(r_kind),parameter:: r60   = 60.0_r_kind
  real(r_kind),parameter:: r90   = 90.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind

  real(r_kind),parameter:: bmiss = 1.0E11_r_kind

! Declare local variables
  logical outside

  integer(i_kind) lunin,i,maxobs
  integer(i_kind) idate,iret,k
  integer(i_kind) kx,nreal,nchanl,ilat,ilon
  integer(i_kind) nmind
  integer(i_kind):: idomsfc,isflg

  integer(i_kind) :: ireadmg,ireadsb,klev,msub,nmsub
  integer(i_kind), dimension(5) :: idate5
  character(len=8)  :: subset
  character(len=8)  :: crpid
  character(len=80) :: headr
  character(len=5)  :: cid
  real(r_double), dimension(7) :: hdr
  real(r_double), dimension(4) :: loc
  real(r_double), dimension(2,255) :: tpf
  real(r_double), dimension(2,65535) :: tpf2
  real(r_double), dimension(3,65535) :: tpf3
  real(r_double)  :: msst,sst
  equivalence (crpid,hdr(7))

  real(r_kind),dimension(0:3):: ts
  real(r_kind),dimension(0:3):: sfcpct

  real(r_single), dimension(65535) :: apres
  real(r_kind) :: tdiff,sstime,usage,sfcr,t4dv,rsc
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind) :: dlat,dlon,sstoe,dlat_earth,dlon_earth
  real(r_kind) :: dlat_earth_deg,dlon_earth_deg
  real(r_kind) :: pres1,zob,tz,tref,dtw,dtc,tz_tr

  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  real(r_double) :: clath,clonh,r_rpid
  integer(i_kind) ntest

  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_single),allocatable::etabl(:,:,:)
  integer(i_kind) ietabl,lcount,itypex
  integer(i_kind) l,m,ikx,ibfms
  integer(i_kind) n,cid_pos,ship_mod
  real(r_kind) terrmin,werrmin,perrmin,qerrmin,pwerrmin

  data headr/'YEAR MNTH DAYS HOUR MINU SELV RPID'/
  data lunin / 10 /
!**************************************************************************
! Initialize variables
  disterrmax=zero
  ntest=0
  maxobs=2e6
  ndata=0
  nodata=0
  nchanl=0
  ilon=2
  ilat=3

  nreal=maxinfo+nstinfo

  allocate(data_all(nreal,maxobs))


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
        write(6,*)'READ_NSSTBUFR:  ***WARNING*** obs error table not available to 3dvar.'
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
  open(lunin,file=infile,form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)
       
! READING EACH REPORT FROM BUFR
       
  do while (ireadmg(lunin,subset,idate) == 0)
     msub = nmsub(lunin)

!           case ( 'NC001001' ) ; ctyp='ships       '
!           case ( 'NC001101' ) ; ctyp='shipsb      '
!           case ( 'NC001113' ) ; ctyp='shipub      '
!           case ( 'NC001104' ) ; ctyp='Land_lcman  '
!           case ( 'NC001002' ) ; ctyp='dbuoy       '
!           case ( 'NC001003' ) ; ctyp='dbuoyb      '
!           case ( 'NC001003' ) ; ctyp='mbuoy       '
!           case ( 'NC001103' ) ; ctyp='mbuoyb      '
!           case ( 'NC001120' ) ; ctyp='saildron    '
!           case ( 'NC001004' ) ; ctyp='lcman       '
!           case ( 'NC001005' ) ; ctyp='tideg       '
!           case ( 'NC001007' ) ; ctyp='cstgd       '
!           case ( 'NC031001' ) ; ctyp='bathy       '
!           case ( 'NC031002' ) ; ctyp='tesac       '
!           case ( 'NC031003' ) ; ctyp='trkob       '
!           case ( 'NC031005' ) ; ctyp='argo_glider '

     read_loop: do while (ireadsb(lunin) == 0)
        call ufbint(lunin,hdr,7,1,iret,headr)

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
        if ( ( trim(subset) == 'NC001003' ) .or. &            ! MBUOY
             ( trim(subset) == 'NC001004' ) .or. &            ! LCMAN
             ( trim(subset) == 'NC001104' ) .or. &            ! bufr land based LCMAN
             ( trim(subset) == 'NC001001' ) .or. &            ! SHIPS
             ( trim(subset) == 'NC001101' ) .or. &            ! bufr SHIPS, restricted
             ( trim(subset) == 'NC001013' ) .or. &            ! bufr SHIPS, unrestricted
             ( trim(subset) == 'NC001113' ) ) then            ! bufr SHIPS, unrestricted
           call ufbint(lunin,msst,1,1,iret,'MSST')            ! for ships, fixed buoy and lcman
           call ufbint(lunin,sst,1,1,iret,'SST1')             ! read SST
        elseif ( trim(subset) == 'NC001103' ) then            ! MBUOYB
           msst = 0.0_r_kind                                  ! for mbuoyb, assign to be 0
           call ufbint(lunin,sst,1,1,iret,'SST0')
        elseif ( trim(subset) == 'NC001002' ) then            ! DBUOY
           msst = 11.0_r_kind                                 ! for drifting buoy, assign to be 11
           call ufbint(lunin,sst,1,1,iret,'SST1')
        elseif ( trim(subset) == 'NC001102' ) then            ! DBUOYB
           msst = 11.0_r_kind                                 ! for drifting buoyb, assign to be 11
           call ufbint(lunin,sst,1,1,iret,'SST0')
        elseif ( trim(subset) == 'NC001120' ) then            ! SailDrone
           msst = 11.0_r_kind                                 ! for SailDrone obs, assign to be 11
           call ufbint(lunin,sst,1,1,iret,'SST0')
!
!          get station ID of SailDrone Obs. 
!
           call ufbint(lunin,r_rpid,1,1,iret,'WMOP')
           write(crpid,'(I7)') int(r_rpid)
        elseif ( trim(subset) == 'NC031002' ) then            ! TESAC
           msst = 12.0_r_kind                                 ! for ARGO, assign to be 12
           call ufbint(lunin,tpf2,2,65535,klev,'DBSS STMP')   ! read T_Profile
           if ( tpf2(1,1) < 20.0_r_kind ) then
              sst = tpf2(2,1)
           else
              sst = bmiss
           endif
        elseif ( trim(subset) == 'NC031001' ) then            ! BATHY
           msst = 13.0_r_kind                                 ! for BATHY, assign to be 13
           call ufbint(lunin,tpf2,2,65535,klev,'DBSS STMP')   ! read T_Profile

           if ( tpf2(1,1) <= 20.0_r_kind ) then
              sst = tpf2(2,1)
           else
              sst = bmiss
           endif
        elseif ( trim(subset) == 'NC031003' ) then            ! TRKOB
           msst = 14.0_r_kind                                 ! for TRKOB, assign to be 14
           call ufbint(lunin,tpf,2,255,klev,'DBSS STMP')      ! read T_Profile
           if ( tpf(1,1) < 1.0_r_kind ) then
              sst = tpf(2,1)
           else
              sst = bmiss
           endif
        elseif ( trim(subset) == 'NC031005' ) then            ! Argo or Glider
           msst = 12.0_r_kind                                 ! for ARGO, assign to be 12
           call ufbint(lunin,tpf3,3,65535,klev,'SSTH SALNH WPRES') ! get klev,T,water pres & salinity

           call ufbint(lunin,r_rpid,1,1,iret,'WMOP')
           write(crpid,'(I7)') int(r_rpid)

           apres(:) = real(tpf3(3,:))

           pres1 = minval(apres)/10000.0_r_kind   ! converting wpres from Pa to dbar for minmum depth 
                                                  ! Not always layer 1 
!
!          get zob in meters
!
           zob = ((-3.434e-12_r_kind*pres1+1.113e-7_r_kind)*pres1+0.712953_r_kind)*pres1 & 
                 + 14190.7_r_kind*log(one+1.83e-5_r_kind*pres1)
           zob = (zob/(980.0_r_kind+1.113e-4*pres1))*1000.0_r_kind
!
!          assign sst if z1 <= 20 m
!
           if ( zob <= 20.0_r_kind ) then
              sst = tpf3(1,minloc(apres,dim=1))
           else
              sst = bmiss
           endif

        elseif ( trim(subset) == 'NC001005' ) then            ! TIDEG
           msst = 15.0_r_kind                                 ! for TIDEG, assign to be 15
           call ufbint(lunin,sst,1,1,iret,'SST1')             ! read SST
        elseif ( trim(subset) == 'NC001007' ) then            ! CSTGD
           msst = 16.0_r_kind                                 ! for CSTGD, assign to be 16
           call ufbint(lunin,sst,1,1,iret,'SST1')             ! read SST
        else
           cycle read_loop
        endif

          call ufbint(lunin,loc,4,1,iret,'CLAT CLATH CLON CLONH')
          clath=loc(1) ; if ( ibfms(loc(2)).eq.0 ) clath=loc(2)
          clonh=loc(3) ; if ( ibfms(loc(4)).eq.0 ) clonh=loc(4)

        nread = nread + 1

        if (  sst > d250 .and. sst < d350 ) then

           cid = trim(crpid)
!          Extract type, date, and location information
           if(clonh >= r360)  clonh = clonh - r360
           if(clonh <  zero)  clonh = clonh + r360

!          Check for valid latitude and longitude
           if (abs(clonh) > r360) cycle read_loop
           if (abs(clath) > r90 ) cycle read_loop

           dlon_earth_deg = clonh
           dlat_earth_deg = clath
           dlon_earth=clonh*deg2rad
           dlat_earth=clath*deg2rad

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
           rsc       = hdr(6)          !second in real

           if ( rsc > 60.0_r_kind .or. rsc < zero ) rsc = zero   !second in real

           call w3fs21(idate5,nmind)
           sstime=real(nmind,r_kind)
 
           tdiff=(sstime-gstime)*r60inv

!      
!          determine platform (ships, dbuoy, fbuoy or lcman and so on) dependent zob and obs. error
!
           if ( trim(subset) == 'NC001001' .or. trim(subset) == 'NC001101' .or. &
                trim(subset) == 'NC001013' .or. trim(subset) == 'NC001113' ) then            ! SHIPS
              ship_mod = 0
              do n = 1, n_ship
                 if ( crpid == trim(ship%id(n)) ) then
                    ship_mod = 1
                    zob = ship%depth(n)
                    if ( trim(ship%sensor(n)) == 'BU' ) then
                       kx = 181
                       sstoe = 0.75_r_kind
                    elseif ( trim(ship%sensor(n)) == 'C' ) then
                       kx = 182
                       sstoe = one
                    elseif ( trim(ship%sensor(n)) == 'HC' ) then
                       kx = 183
                       sstoe = one
                    elseif ( trim(ship%sensor(n)) == 'BTT' ) then
                       kx = 184
                       sstoe = one
                    elseif ( trim(ship%sensor(n)) == 'HT' ) then
                       kx = 185
                       sstoe = one
                    elseif ( trim(ship%sensor(n)) == 'RAD' ) then
                       kx = 186
                       sstoe = one
                    elseif ( trim(ship%sensor(n)) == 'TT' ) then
                       kx = 187
                       sstoe = one
                    elseif ( trim(ship%sensor(n)) == 'OT' ) then
                       kx = 188
                       sstoe = one
                    else
                       kx = 189
                       sstoe = two
                    endif
                 endif
              enddo

              if ( ship_mod == 0 ) then
                 if ( msst == two ) then                                  ! positive or zero bucket
                    kx = 181
                    sstoe = 0.75_r_kind
                    zob = one
                 elseif ( msst == zero .or. msst == one ) then            ! positive/negative/zero intake
                    kx = 182
                    sstoe = one
                    zob = three
                 else
                    kx = 189
                    sstoe = two
                    zob = 2.5_r_kind
                 endif
              endif


           elseif ( trim(subset) == 'NC001002'  ) then                        ! DBUOY

              cid_pos = 0

              do n = 1, n_3mdiscus
                 if ( cid == cid_mbuoy(n) ) then
                    cid_pos = n
                 endif
              enddo
 
              if ( cid_pos >= 1 .and. cid_pos <= n_comps ) then                ! COMPS moored buoy

                 zob = r1_2
                 kx = 192
                 sstoe = 0.75_r_kind

              elseif ( cid_pos > n_scripps .and. cid_pos <= n_triton ) then    ! Triton buoy
 
                 zob = r1_5
                 kx = 194
                 sstoe = half
 
              elseif ( cid_pos == 0 ) then
 
                 zob = r0_2
                 if ( cid(3:3) == '5' .or. cid(3:3) == '6' .or. cid(3:3) == '7' .or. cid(3:3) == '8' .or. cid(3:3) == '9' ) then
                    kx = 190
                    sstoe = half
                 elseif ( cid(3:3) == '0' .or. cid(3:3) == '1' .or. cid(3:3) == '2' .or. cid(3:3) == '3' .or. cid(3:3) == '4') then
                    kx = 191
                    sstoe = half
                 endif

              endif

           elseif ( trim(subset) == 'NC001102'  ) then                           ! DBUOYB
              zob = r0_2
              kx = 190
              sstoe = half
           elseif ( trim(subset) == 'NC001003' ) then                            ! MBUOY

              cid_pos = 0

              do n = 1, n_3mdiscus
                 if ( cid == cid_mbuoy(n) ) then
                    cid_pos = n
                 endif
              enddo

              if ( cid_pos >= 1 .and. cid_pos <= n_comps ) then                  ! COMPS moored buoy
                 zob = r1_2
                 kx = 192
                 sstoe = 0.75_r_kind
              elseif ( cid_pos > n_comps .and. cid_pos <= n_scripps ) then       ! SCRIPPS moored buoy
                 zob = r0_45
                 kx = 193
                 sstoe = 0.75_r_kind
              elseif ( cid_pos > n_scripps .and. cid_pos <= n_triton ) then      ! Triton buoy
                 zob = r1_5
                 kx = 194
                 sstoe = half
              elseif ( cid_pos > n_triton .and. cid_pos <= n_3mdiscus ) then     ! Moored buoy with 3-m discus
                 zob = r0_6
                 kx = 195
                 sstoe = 0.75_r_kind
              elseif ( cid_pos == 0 ) then                                       ! All other moored buoys (usually with 1-m observation depth)
                 zob = one
                 kx = 196
                 sstoe = 0.75_r_kind
              endif

           elseif ( trim(subset) == 'NC001103' ) then                            ! MBUOYB

              cid_pos = 0

              do n = 1, n_3mdiscus
                 if ( cid == cid_mbuoyb(n) ) then
                    cid_pos = n
                 endif
              enddo

              if ( cid_pos >= 1 .and. cid_pos <= n_comps ) then                  ! COMPS moored buoyb
                 zob = r1_2
                 kx = 192
                 sstoe = 0.75_r_kind
              elseif ( cid_pos > n_comps .and. cid_pos <= n_scripps ) then       ! SCRIPPS moored buoyb
                 zob = r0_45
                 kx = 193
                 sstoe = 0.75_r_kind
              elseif ( cid_pos > n_scripps .and. cid_pos <= n_triton ) then      ! Triton buoyb
                 zob = r1_5
                 kx = 194
                 sstoe = half
              elseif ( cid_pos > n_triton .and. cid_pos <= n_3mdiscus ) then     ! Moored buoyb with 3-m discus
                 zob = r0_6
                 kx = 195
                 sstoe = 0.75_r_kind
              elseif ( cid_pos == 0 ) then                                       ! All other moored buoysb (usually with 1-m observation depth)
                 zob = one
                 kx = 196
                 sstoe = 0.75_r_kind
              endif
           elseif ( trim(subset) == 'NC001120' ) then            ! SailDrone
             zob = r0_6
             kx = 190                         ! classify saildrone to be drifting buoy type
             sstoe = half
           elseif ( trim(subset) == 'NC001004' .or. trim(subset) == 'NC001104' .or. &    ! LCMAN
                    trim(subset) == 'NC031003' .or. &                                    ! TRKOB
                    trim(subset) == 'NC001005' .or. trim(subset) == 'NC001007' ) then    ! TIDEG, CSTGD
              zob = one
              kx = 197
              sstoe = one
           elseif ( trim(subset) == 'NC031002' ) then                            ! TESAC
              if (  tpf2(1,1) >= one .and.  tpf2(1,1) < 20.0_r_kind ) then
                 zob = tpf2(1,1)
              elseif (  tpf2(1,1) >= zero .and. tpf2(1,1) < one ) then
                 zob = one
              endif
              kx = 198   
              sstoe = one
           elseif ( trim(subset) == 'NC031005' ) then                            ! ARGO/Glider
              kx = 199                                                           ! classify argo & glider to be bathy type
              sstoe = r0_6
           elseif ( trim(subset) == 'NC031001' ) then                            ! BATHY
              if (  tpf2(1,1) >= one .and.  tpf2(1,1) <= 20.0_r_kind ) then
                 zob = tpf2(1,1)
              elseif (  tpf2(1,1) >= zero .and. tpf2(1,1) < one ) then
                 zob = one
              endif
              kx = 199
              sstoe = half
           else
              write(*,*) 'unrecognized data type, set kx = 0'
              kx = 0
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
           t4dv=(real((nmind-iwinbgn),r_kind) + rsc*r60inv)*r60inv
!
           if (l4dvar.or.l4densvar) then
              if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
           else
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

           call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
                          ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
           if(sfcpct(0) == zero)  cycle read_loop

           nodata = nodata + 1
           ndata = ndata + 1
           if(ndata > maxobs) then
              write(6,*)'READ_NSSTBUFR:  ***WARNING*** ndata > maxobs for ',obstype
              ndata = maxobs
           end if
!
!          interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
!
           if(nst_gsi > 0) then
              call gsi_nstcoupler_deter(dlat_earth,dlon_earth,t4dv,zob,tref,dtw,dtc,tz_tr)
              tz = tref
              if (nst_gsi > 2 ) then
                tz = tref+dtw-dtc            ! Tz: Background temperature at depth of zob
              endif
           else
              tref  = ts(0)
              dtw   = zero
              dtc   = zero
              tz_tr = one
              tz    = ts(0)
           endif

           data_all(1,ndata)  = sstoe                   ! sst error
           data_all(2,ndata)  = dlon                    ! grid relative longitude
           data_all(3,ndata)  = dlat                    ! grid relative latitude
           data_all(4,ndata)  = sst                     ! sst obs
           data_all(5,ndata)  = hdr(7)                  ! station id
           data_all(6,ndata)  = t4dv                    ! time
           data_all(7,ndata)  = ikx                     ! type
           data_all(8,ndata)  = ts(0)                   ! open water temperature
           data_all(9,ndata)  = zob                     ! depth of measurement
           data_all(10,ndata) = kx                      ! measurement type
           data_all(11,ndata) = sfcpct(0)               ! open water percentage
           data_all(12,ndata) = sstoe                   ! original sst error
           data_all(13,ndata) = usage                   ! usage parameter
           data_all(14,ndata) = idomsfc+0.001_r_kind    ! dominate surface type
           data_all(15,ndata) = tz                      ! Tz: Background temperature at depth of zob
           data_all(16,ndata) = dlon_earth_deg          ! earth relative longitude (degrees)
           data_all(17,ndata) = dlat_earth_deg          ! earth relative latitude (degrees)
           data_all(18,ndata) = hdr(6)                  ! station elevation
 

           if(nst_gsi>0) then
              data_all(maxinfo+1,ndata) = tref          ! foundation temperature
              data_all(maxinfo+2,ndata) = dtw           ! dt_warm at zob
              data_all(maxinfo+3,ndata) = dtc           ! dt_cool at zob
              data_all(maxinfo+4,ndata) = tz_tr         ! d(Tz)/d(Tr)
           endif

        end if                                          ! if (  sst > d250 .and. sst < d350 ) then
     enddo read_loop
  enddo
!
!   End of bufr read loop
       
! Normal exit
1000 continue

! Write header record and data to output file for further processing
  call count_obs(ndata,nreal,ilat,ilon,data_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((data_all(k,i),k=1,nreal),i=1,ndata)

  write(*,*) 'read_nsstbufr : ',nreal,nchanl,nread,ndata

! Close unit to bufr file
1020 continue
  deallocate(data_all)
  if (oberrflg) deallocate(etabl)
  call closbf(lunin)
  close(lunin)

  if(diagnostic_reg.and.ntest > 0) write(6,*)'READ_NSSTBUFR:  ',&
     'ntest,disterrmax=',ntest,disterrmax


! End of routine
  return
end subroutine read_nsstbufr
