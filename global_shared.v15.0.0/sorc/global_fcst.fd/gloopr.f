       subroutine gloopr
     &    (trie_ls,trio_ls,
     &     ls_node,ls_nodes,max_ls_nodes,
     &     lats_nodes_a,global_lats_a,
     &     lats_nodes_r,global_lats_r,
     &     lonsperlar,
     &     epse,epso,epsedn,epsodn,
     &     snnp1ev,snnp1od, plnev_r,plnod_r,
     &     pddev_r,pddod_r,
     &     phour,deltim,
     &     xlon,xlat,coszdg,coszen,
     &     slmsk,snwdph,sncovr,snoalb,zorl,tsea,hprime,
     &     sfalb,
     &     alvsf,alnsf ,alvwf ,alnwf,facsf ,facwf,cv,cvt ,
     &     cvb  ,swh,swhc,hlw,hlwc,sfcnsw,sfcdlw,
     &     fice ,tisfc, sfcdsw, sfcemis,                ! for sea-ice - xw nov04
     &     tsflw,fluxr, phy_f3d,phy_f2d,slag,sdec,cdec,kdt,
     &     global_times_r)
!!
!
!   program modification log:
!      apr 2012  - y-t hou     -  modified radiation calling interval
!             parameters fhswr/fhlwr.  changed units from hour to second
!             (in multiple of model time step length), thus radiation can
!             be invoked more frequently than the minimum of 1 hr limit.
!                              -  moved radiation initialization part to
!             the beginning of model initialization process. in the time
!             loop, added a subroutine "radupdate" to updae radiation related
!             external data sets (such as aerosols, gases, astronomy, etc.)
!             moved computation of coszen to inside subrouine "grrad"
!      nov 2012  - y-t hou     - modified many physics/radiation control
!             parameters through module 'physpara' which contains the use of
!             module 'machine'.
!      may 2013 s. mooorthi - removed fpkapx
!      jul 2013 r sun  - to use pdf cloud and convective cld cover and water 

      use physpara
      use physcons, fv => con_fvirt, rerth => con_rerth, rocp =>con_rocp

      use module_radiation_driver,   only : radupdate, grrad

      use module_radsw_parameters,  only : topfsw_type, sfcfsw_type
      use module_radlw_parameters,  only : topflw_type, sfcflw_type
!
!! ---  for optional spectral band heating outputs
!!    use module_radsw_parameters,   only : nbdsw
!!    use module_radlw_parameters,   only : nbdlw
!
      use resol_def
      use layout1
      use layout_grid_tracers
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def					! hmhj
      use tracer_const						! hmhj
      use d3d_def , only : cldcov
      use mersenne_twister, only : random_setseed, random_index,        &
     &                             random_stat
!
      implicit none
      include 'mpif.h'
!
      real (kind=kind_phys), parameter :: qmin =1.0e-10
      real (kind=kind_evod), parameter :: typical_pgr = 95.0
      real (kind=kind_evod), parameter :: cons0 = 0.0

!  --- ...  inputs:
      integer, intent(in) :: ls_node(ls_dim,3), ls_nodes(ls_dim,nodes), &
     &                       max_ls_nodes(nodes), lats_nodes_r(nodes),  &
     &                       global_lats_r(latr), lonsperlar(latr)

      integer, intent(in) :: lats_nodes_a(nodes), global_lats_a(latg)

      real(kind=kind_evod), dimension(len_trie_ls), intent(in) ::       &
     &                       epse, epsedn, snnp1ev

      real(kind=kind_evod), dimension(len_trio_ls), intent(in) ::       &
     &                       epso, epsodn, snnp1od

      real(kind=kind_evod), intent(in) :: plnev_r(len_trie_ls, latr2),  &
     &                                    plnod_r(len_trio_ls, latr2)
      real(kind=kind_evod), intent(in) :: pddev_r(len_trie_ls, latr2),  &
     &                                    pddod_r(len_trio_ls, latr2)

      real (kind=kind_phys), dimension(lonr,lats_node_r), intent(in) :: &
     &                       xlon, xlat, slmsk, snwdph, zorl, tsea,     &
     &                       alvsf, alnsf, alvwf, alnwf, facsf, facwf,  &
     &                       cv, cvt, cvb, fice, tisfc, sncovr, snoalb

      real (kind=kind_phys), intent(in) ::                              &
     &                    hprime(lonr,nmtvr,lats_node_r), phour,deltim
!
!  --- ...  input and output:
      real (kind=kind_phys), intent(inout) ::                           &
     &                    phy_f3d(lonr,levs,num_p3d,lats_node_r),       &
     &                    phy_f2d(lonr,num_p2d,lats_node_r)
      real(kind=kind_evod), intent(inout) ::                            &
     &                    trie_ls(len_trie_ls,2,11*levs+3*levh+6),      &
     &                    trio_ls(len_trio_ls,2,11*levs+3*levh+6)
      integer              ipt_ls                                       ! hmhj
      real(kind=kind_evod) reall                                        ! hmhj
      real(kind=kind_evod) rlcs2(jcap1)                                 ! hmhj

      real (kind=kind_phys), intent(inout) ::                           &
     &                    fluxr (lonr,nfxr,lats_node_r)

      integer, intent(in) :: kdt

!  --- ...  outputs:
      real(kind=kind_evod), intent(inout) ::                            &
     &                    global_times_r(latr,nodes)

      real (kind=kind_phys), intent(out) ::                             &
     &                    swh(lonr,levs,lats_node_r),                   &
     &                    swhc(lonr,levs,lats_node_r),                  &
     &                    hlwc(lonr,levs,lats_node_r),                  &
     &                    hlw(lonr,levs,lats_node_r)

      real (kind=kind_phys),dimension(lonr,lats_node_r), intent(out) :: &
     &                    coszdg, coszen, sfcnsw, sfcdlw, tsflw,        &
     &                    sfcdsw, sfalb, sfcemis

      real (kind=kind_phys), intent(out) :: slag, sdec, cdec

!! --- ...  optional spectral band heating rates
!!    real (kind=kind_phys), optional, intent(out) ::                   &
!!   &                 htrswb(ngptc,levs,nbdsw,nblck,lats_node_r),      &
!!   &                 htrlwb(ngptc,levs,nbdlw,nblck,lats_node_r)

!  --- ...  locals:
      real(kind=kind_evod) ::                                           &
     &                    for_gr_r_1(LONRX,LOTS,LATS_DIM_R),            &
     &                    dyn_gr_r_1(lonrx,lotd,lats_dim_r),            &
     &                    for_gr_r_2(LONR ,LOTS,LATS_DIM_R),            &
     &                    dyn_gr_r_2(lonr ,lotd,lats_dim_r)

      real(kind=kind_phys) :: prsl(ngptc,levs),  prslk(ngptc,levs),     &
     &                        prsi(ngptc,levp1), prsik(ngptc,levp1),    &
     &                        hlwc_v(ngptc,levs), swhc_v(ngptc,levs),   &
     &                        hlw_v(ngptc,levs), swh_v(ngptc,levs)

      real (kind=kind_phys) ::                                          &
     &                      gt(ngptc,levr),   gq(ngptc),                &
     &                      gr(ngptc,levr),   gr1(ngptc,levr,ntrac-1),  &
     &                      gtv(ngptc,levr)

      real (kind=kind_phys), allocatable ::  sumq(:,:), xcp(:,:),       &
     &                                       gtvx(:,:), gtvy(:,:),      &
     &                                                  gd(:,:),        &
!    &                                       vvel(:,:), gd(:,:),        &
     &                                       gu(:,:),   gv1(:,:),       &
     &                                       gphi(:),   glam(:)

      real (kind=kind_phys) :: f_ice(ngptc,levs), f_rain(ngptc,levs),   &
     &                         r_rime(ngptc,levs)

      real (kind=kind_phys) :: deltaq(ngptc,levs),cnvw(ngptc,levs)
      real (kind=kind_phys) :: cnvc(ngptc,levs ), vvel(ngptc,levs)

      real (kind=kind_phys) :: cldcov_v(ngptc,levs), fluxr_v(ngptc,nfxr)
      real (kind=kind_phys) :: flgmin_v(ngptc), work1, work2

      real (kind=kind_phys) :: coszen_v(ngptc), coszdg_v(ngptc),        &
     &                         sinlat_v(ngptc), coslat_v(ngptc)

      real (kind=kind_phys) :: rinc(5), dtsw, dtlw, solcon, raddt, solhr

      integer :: njeff, lon, lan, lat, lons_lat, lmax
      integer :: idat(8), jdat(8)

!  ---  variables of instantaneous calculated toa/sfc radiation fluxes
      type (topfsw_type), dimension(ngptc) :: topfsw
      type (sfcfsw_type), dimension(ngptc) :: sfcfsw

      type (topflw_type), dimension(ngptc) :: topflw
      type (sfcflw_type), dimension(ngptc) :: sfcflw

!  ---  variables used for random number generator (thread safe mode)
      type (random_stat) :: stat
      integer :: numrdm(lonr*latr*2), ixseed(lonr,lats_node_r,2)
      integer :: ipseed, icsdlw(ngptc), icsdsw(ngptc)
      integer, parameter :: ipsdlim = 1.0e8      ! upper limit for random seeds

!  --- ...  control parameters:
!           (some of the them may be moved into model namelist)

!  ---  ictm=yyyy#, controls time sensitive external data (e.g. co2, solcon, aerosols, etc)
!     integer, parameter :: ictm =   -2 ! same as ictm=0, but add seasonal cycle from
!                                       ! climatology. no extrapolation.
!     integer, parameter :: ictm =   -1 ! use user provided external data set for the
!                                       ! forecast time. no extrapolation.
!     integer, parameter :: ictm =    0 ! use data at initial cond time, if not
!                                       ! available, use latest, no extrapolation.
!!    integer, parameter :: ictm =    1 ! use data at the forecast time, if not
!                                       ! available, use latest and extrapolation.
!     integer, parameter :: ictm =yyyy0 ! use yyyy data for the forecast time,
!                                       ! no further data extrapolation.
!     integer, parameter :: ictm =yyyy1 ! use yyyy data for the fcst. if needed, do
!                                       ! extrapolation to match the fcst time.

!  ---  isol controls solar constant data source
!!    integer, parameter :: isol = 0   ! use prescribed solar constant
!     integer, parameter :: isol = 1   ! use varying solar const with 11-yr cycle

!  ---  ico2 controls co2 data source for radiation
!     integer, parameter :: ico2 = 0   ! prescribed global mean value (old opernl)
!!    integer, parameter :: ico2 = 1   ! use obs co2 annual mean value only
!     integer, parameter :: ico2 = 2   ! use obs co2 monthly data with 2-d variation

!  ---  ialb controls surface albedo for sw radiation
!!    integer, parameter :: ialb = 0   ! use climatology alb, based on sfc type
!     integer, parameter :: ialb = 1   ! use modis derived alb (to be developed)

!  ---  iems controls surface emissivity and sfc air/ground temp for lw radiation
!        ab: 2-digit control flags. a-for sfc temperature;  b-for emissivity
!!    integer, parameter :: iems = 00  ! same air/ground temp; fixed emis = 1.0
!!    integer, parameter :: iems = 01  ! same air/ground temp; varying veg typ based emis
!!    integer, parameter :: iems = 10  ! diff air/ground temp; fixed emis = 1.0
!!    integer, parameter :: iems = 11  ! diff air/ground temp; varying veg typ based emis

!  ---  iaer  controls aerosols scheme selections
!     integer, parameter :: iaer = abc --> abc are three digits integer numbers
!                                          to control aerosol effect
!     a: stratospheric-volcanic forcing;  b: lw radiation;  c: sw radiation.
!      a=0: no stratospheric-volcanic aerosol effect;   =1: include the effect.
!      b=0: no lw tropospheric aerosols; =1: lw compute 1 bnd; =2: lw compute multi bnd.
!      c=0: no sw tropospheric aerosols; =1: sw compute multi band.

!  ---  iovr controls cloud overlapping method in radiation:
!     integer, parameter :: iovr_sw = 0  ! sw: random overlap clouds
!!    integer, parameter :: iovr_sw = 1  ! sw: max-random overlap clouds

!     integer, parameter :: iovr_lw = 0  ! lw: random overlap clouds
!!    integer, parameter :: iovr_lw = 1  ! lw: max-random overlap clouds

!  ---  isubc controls sub-column cloud approximation in radiation:
!!    integer, parameter :: isubc_sw = 0 ! sw: without sub-col clds approx
!     integer, parameter :: isubc_sw = 1 ! sw: sub-col clds with prescribed seeds
!     integer, parameter :: isubc_sw = 2 ! sw: sub-col clds with random seeds

!!    integer, parameter :: isubc_lw = 0 ! lw: without sub-col clds approx
!     integer, parameter :: isubc_lw = 1 ! lw: sub-col clds with prescribed seeds
!     integer, parameter :: isubc_lw = 2 ! lw: sub-col clds with random seeds

!  ---  iflip indicates model vertical index direction:
!     integer, parameter :: iflip = 0    ! virtical profile index from top to bottom
!!    integer, parameter :: iflip = 1    ! virtical profile index from bottom to top
!
!    the following parameters are from gbphys

      real (kind=kind_phys), parameter :: dxmax=-16.118095651,          &
     &                dxmin=-9.800790154, dxinv=1.0/(dxmax-dxmin)

      integer :: dimg
      integer :: i, j, k, n, nt
      integer :: kdtphi,kdtlam,ks                                ! hmhj
      integer :: dbgu

      logical :: change
      logical, save :: first
      data  first / .true. /

!  ---  for debug test use
!     real (kind=kind_phys) :: temlon, temlat, alon, alat
      integer :: ipt
      logical :: lprnt

!  ---  timers:
!     real*8 :: rtc, timer1, timer2
!
!===> *** ...  begin here
!
!$$$      integer                lots,lotd,lota
!$$$      parameter            ( lots = 5*levs+1*levh+3 )
!$$$      parameter            ( lotd = 6*levs+2*levh+0 )
!$$$      parameter            ( lota = 3*levs+1*levh+1 )

      integer              ksd,ksplam,kspphi,ksq,ksr,kst
      integer              ksu,ksv,ksz,item,jtem

!     real(kind=kind_evod) spdlat(levs,lats_dim_r)
!moor real(kind=kind_phys) slk(levs)
!     real(kind=kind_evod) spdmax_node (levs)
!     real(kind=kind_evod) spdmax_nodes(levs,nodes)
!!
!!................................................................
!!  syn(1, 0*levs+0*levh+1, lan)  ze
!!  syn(1, 1*levs+0*levh+1, lan)  di
!!  syn(1, 2*levs+0*levh+1, lan)  te
!!  syn(1, 3*levs+0*levh+1, lan)  rq
!!  syn(1, 3*levs+1*levh+1, lan)  q
!!  syn(1, 3*levs+1*levh+2, lan)  dpdlam
!!  syn(1, 3*levs+1*levh+3, lan)  dpdphi
!!  syn(1, 3*levs+1*levh+4, lan)  uln
!!  syn(1, 4*levs+1*levh+4, lan)  vln
!!................................................................
!!  dyn(1, 0*levs+0*levh+1, lan)  d(t)/d(phi)
!!  dyn(1, 1*levs+0*levh+1, lan)  d(rq)/d(phi)
!!  dyn(1, 1*levs+1*levh+1, lan)  d(t)/d(lam)
!!  dyn(1, 2*levs+1*levh+1, lan)  d(rq)/d(lam)
!!  dyn(1, 2*levs+2*levh+1, lan)  d(u)/d(lam)
!!  dyn(1, 3*levs+2*levh+1, lan)  d(v)/d(lam)
!!  dyn(1, 4*levs+2*levh+1, lan)  d(u)/d(phi)
!!  dyn(1, 5*levs+2*levh+1, lan)  d(v)/d(phi)
!!................................................................
!!  anl(1, 0*levs+0*levh+1, lan)  w     dudt
!!  anl(1, 1*levs+0*levh+1, lan)  x     dvdt
!!  anl(1, 2*levs+0*levh+1, lan)  y     dtdt
!!  anl(1, 3*levs+0*levh+1, lan)  rt    drdt
!!  anl(1, 3*levs+1*levh+1, lan)  z     dqdt
!!................................................................
!!
!$$$      parameter(ksz     =0*levs+0*levh+1,
!$$$     x          ksd     =1*levs+0*levh+1,
!$$$     x          kst     =2*levs+0*levh+1,
!$$$     x          ksr     =3*levs+0*levh+1,
!$$$     x          ksq     =3*levs+1*levh+1,
!$$$     x          ksplam  =3*levs+1*levh+2,
!$$$     x          kspphi  =3*levs+1*levh+3,
!$$$     x          ksu     =3*levs+1*levh+4,
!$$$     x          ksv     =4*levs+1*levh+4)
!!
!$$$      parameter(kdtphi  =0*levs+0*levh+1,
!$$$     x          kdrphi  =1*levs+0*levh+1,
!$$$     x          kdtlam  =1*levs+1*levh+1,
!$$$     x          kdrlam  =2*levs+1*levh+1,
!$$$     x          kdulam  =2*levs+2*levh+1,
!$$$     x          kdvlam  =3*levs+2*levh+1,
!$$$     x          kduphi  =4*levs+2*levh+1,
!$$$     x          kdvphi  =5*levs+2*levh+1)
!!
!$$$      parameter(kau     =0*levs+0*levh+1,
!$$$     x          kav     =1*levs+0*levh+1,
!$$$     x          kat     =2*levs+0*levh+1,
!$$$     x          kar     =3*levs+0*levh+1,
!$$$     x          kap     =3*levs+1*levh+1)
!!
!$$$      integer   p_gz,p_zem,p_dim,p_tem,p_rm,p_qm
!$$$      integer   p_ze,p_di,p_te,p_rq,p_q,p_dlam,p_dphi,p_uln,p_vln
!$$$      integer   p_w,p_x,p_y,p_rt,p_zq
!$$$      parameter(p_gz  = 0*levs+0*levh+1,  !      gze/o(lnte/od,2),
!$$$     x          p_zem = 0*levs+0*levh+2,  !     zeme/o(lnte/od,2,levs),
!$$$     x          p_dim = 1*levs+0*levh+2,  !     dime/o(lnte/od,2,levs),
!$$$     x          p_tem = 2*levs+0*levh+2,  !     teme/o(lnte/od,2,levs),
!$$$     x          p_rm  = 3*levs+0*levh+2,  !      rme/o(lnte/od,2,levh),
!$$$     x          p_qm  = 3*levs+1*levh+2,  !      qme/o(lnte/od,2),
!$$$     x          p_ze  = 3*levs+1*levh+3,  !      zee/o(lnte/od,2,levs),
!$$$     x          p_di  = 4*levs+1*levh+3,  !      die/o(lnte/od,2,levs),
!$$$     x          p_te  = 5*levs+1*levh+3,  !      tee/o(lnte/od,2,levs),
!$$$     x          p_rq  = 6*levs+1*levh+3,  !      rqe/o(lnte/od,2,levh),
!$$$     x          p_q   = 6*levs+2*levh+3,  !       qe/o(lnte/od,2),
!$$$     x          p_dlam= 6*levs+2*levh+4,  !  dpdlame/o(lnte/od,2),
!$$$     x          p_dphi= 6*levs+2*levh+5,  !  dpdphie/o(lnte/od,2),
!$$$     x          p_uln = 6*levs+2*levh+6,  !     ulne/o(lnte/od,2,levs),
!$$$     x          p_vln = 7*levs+2*levh+6,  !     vlne/o(lnte/od,2,levs),
!$$$     x          p_w   = 8*levs+2*levh+6,  !       we/o(lnte/od,2,levs),
!$$$     x          p_x   = 9*levs+2*levh+6,  !       xe/o(lnte/od,2,levs),
!$$$     x          p_y   =10*levs+2*levh+6,  !       ye/o(lnte/od,2,levs),
!$$$     x          p_rt  =11*levs+2*levh+6,  !      rte/o(lnte/od,2,levh),
!$$$     x          p_zq  =11*levs+3*levh+6)  !      zqe/o(lnte/od,2)
!!
!     print *,' in gloopr vertcoord_id =',vertcoord_id

      ksz     =0*levs+0*levh+1
      ksd     =1*levs+0*levh+1
      kst     =2*levs+0*levh+1
      ksq     =3*levs+0*levh+1
      ksplam  =3*levs+0*levh+2
      kspphi  =3*levs+0*levh+3
      ksu     =3*levs+0*levh+4
      ksv     =4*levs+0*levh+4
      ksr     =5*levs+0*levh+4

      kdtphi  =0*levs+0*levh+1                          ! hmhj
      kdtlam  =1*levs+1*levh+1                          ! hmhj

      idat = 0
      idat(1) = idate(4)
      idat(2) = idate(2)
      idat(3) = idate(3)
      idat(5) = idate(1)
      rinc = 0.
      rinc(2) = fhour
      call w3movdat(rinc, idat, jdat)

      if (first) then
        if( thermodyn_id.eq.3 ) then
          if (.not. allocated(xcp)) allocate (xcp(ngptc,levr))
          if (.not. allocated(sumq)) allocate (sumq(ngptc,levr))
        endif

        if( ntcw <= 0 ) then
          if( gen_coord_hybrid .and. vertcoord_id == 3.) then
            if (.not. allocated(gtvx)) allocate (gtvx(ngptc,levs))
            if (.not. allocated(gtvy)) allocate (gtvy(ngptc,levs))
          endif

          if (.not. allocated(gu))   allocate (gu(ngptc,levs))
          if (.not. allocated(gv1))  allocate (gv1(ngptc,levs))
          if (.not. allocated(gd))   allocate (gd(ngptc,levs))
!         if (.not. allocated(vvel)) allocate (vvel(ngptc,levs))
          if (.not. allocated(gphi)) allocate (gphi(ngptc))
          if (.not. allocated(glam)) allocate (glam(ngptc))
        endif

        first = .false.
      endif         ! end_if_first

!  --- ...  update time varying radiation related quantities

!     dtsw  = 3600.0 * fhswr
!     dtlw  = 3600.0 * fhlwr
      dtsw  = fhswr                  ! fhswr is in sec
      dtlw  = fhlwr                  ! fhlwr is in sec
      raddt = min(dtsw, dtlw)

      if ( fhour > 0.0 ) then
        solhr = mod(float(jdat(5)),24.0) ! hour after 00z at current fcst time
      else
        solhr = idate(1)                 ! initial time
      endif

      call radupdate                                                    &
!  ---  inputs:
     &     ( idat, jdat, dtsw, deltim, lsswr, me,                       &
!  ---  outputs:
     &       slag, sdec, cdec, solcon                                   &
     &     )

!  --- ...  generate 2-d random seeds array for sub-grid cloud-radiation

      if ( isubc_lw==2 .or. isubc_sw==2 ) then
        ipseed = mod(nint(100.0*sqrt(fhour*3600)), ipsdlim) + 1 + ipsd0

        call random_setseed                                             &
!  ---  inputs:
     &     ( ipseed,                                                    &
!  ---  outputs:
     &       stat                                                       &
     &      )
        call random_index                                               &
!  ---  inputs:
     &     ( ipsdlim,                                                   &
!  ---  outputs:
     &       numrdm, stat                                               &
     &     )

        do k = 1, 2
          do j = 1, lats_node_r
            lat = global_lats_r(ipt_lats_node_r-1+j)

            do i = 1, lonr
              ixseed(i,j,k) = numrdm(i+(lat-1)*lonr+(k-1)*latr)
            enddo
          enddo
        enddo
      endif

!  --- ...  spectrum to grid transformation for radiation calculation.

      call delnpe(trie_ls(1,1,p_q   ), trio_ls(1,1,p_dphi),             &
     &            trie_ls(1,1,p_dlam), epse,epso,ls_node)

      call delnpo(trio_ls(1,1,p_q   ), trie_ls(1,1,p_dphi),             &
     &            trio_ls(1,1,p_dlam), epse,epso,ls_node)
!     print *,' after delnpeo'


!$omp parallel do shared(trie_ls,trio_ls)
!$omp+shared(epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!$omp+private(k)

      do k = 1, levs
        call dezouv(trie_ls(1,1,p_di +k-1), trio_ls(1,1,p_ze +k-1),     &
     &              trie_ls(1,1,p_uln+k-1), trio_ls(1,1,p_vln+k-1),     &
     &              epsedn,epsodn,snnp1ev,snnp1od,ls_node)

        call dozeuv(trio_ls(1,1,p_di +k-1), trie_ls(1,1,p_ze +k-1),     &
     &              trio_ls(1,1,p_uln+k-1), trie_ls(1,1,p_vln+k-1),     &
     &              epsedn,epsodn,snnp1ev,snnp1od,ls_node)
      enddo

      dimg=0

      call sumfln_slg_gg(trie_ls(1,1,p_ze),                             &
     &                   trio_ls(1,1,p_ze),                             &
     &                   lat1s_r,plnev_r,plnod_r,                       &
     &                   5*levs+3,ls_node,latr2,                        &
     &                   lats_dim_r,lots,for_gr_r_1,                    &
     &                   ls_nodes,max_ls_nodes,                         &
     &                   lats_nodes_r,global_lats_r,                    &
     &                   lats_node_r,ipt_lats_node_r,lon_dim_r,         &
     &                   lonsperlar,lonrx,latr,0)

      if ( .not. gg_tracers ) then
!  --- ...  tracers grid values will be set from layout_grid_traces

        call sumfln_slg_gg(trie_ls(1,1,p_rq),                           &
     &                     trio_ls(1,1,p_rq),                           &
     &                     lat1s_r,plnev_r,plnod_r,                     &
     &                     levh,ls_node,latr2,                          &
     &                     lats_dim_r,lots,for_gr_r_1,                  &
     &                     ls_nodes,max_ls_nodes,                       &
     &                     lats_nodes_r,global_lats_r,                  &
     &                     lats_node_r,ipt_lats_node_r,lon_dim_r,       &
     &                     lonsperlar,lonrx,latr,5*levs+3)
      endif

!     print*,'completed call to sumfln'


      if ( vertcoord_id == 3. ) then

        do lan = 1, lats_node_r
          lat = global_lats_r(ipt_lats_node_r-1+lan)
          lmax = min(jcap,lonsperlar(lat)/2)

          if ( (lmax+1)*2+1 .le. lonsperlar(lat)+2 ) then
            do k = levs+1, 4*levs+2*levh
               do i = (lmax+1)*2+1, lonsperlar(lat)+2
                  dyn_gr_r_1(i,k,lan) = cons0    !constant
               enddo
            enddo
          endif
        enddo

        call sumder2_slg(trie_ls(1,1,p_te),                             &! hmhj
     &                   trio_ls(1,1,p_te),                             &! hmhj
     &                   lat1s_r,pddev_r,pddod_r,                       &! hmhj
     &                   levs,ls_node,latr2,                            &! hmhj
     &                   lats_dim_r,lotd,                               &! hmhj
     &                   dyn_gr_r_1,                                    &! hmhj
     &                   ls_nodes,max_ls_nodes,                         &! hmhj
     &                   lats_nodes_r,global_lats_r,                    &! hmhj
     &                   lats_node_r,ipt_lats_node_r,lon_dim_r,         &! hmhj
     &                   lonsperlar,lonrx,latr,0)                        ! hmhj

      endif     ! vertcoord_id=3

      if (gg_tracers .and. shuff_lats_r) then
        print*,' gloopr mpi_tracers_a_to_b shuff_lats_r',shuff_lats_r

        call mpi_tracers_a_to_b(rgt_a,lats_nodes_a,global_lats_a,       &
     &              for_gr_r_2(1,1,1),lats_nodes_r,global_lats_r,ksr,0)
      endif  ! gg_tracers_and_shuff_lats_r
 
      do lan = 1, lats_node_r
!       timer1 = rtc()
        lat = global_lats_r(ipt_lats_node_r-1+lan)
        lons_lat = lonsperlar(lat)

        if ( gen_coord_hybrid .and. vertcoord_id==3. ) then
          lmax   = min(jcap,lons_lat/2)                                  ! hmhj
          ipt_ls = min(lat,latr-lat+1)                                   ! hmhj

          do i=1,lmax+1                                                  ! hmhj
            if ( ipt_ls .ge. lat1s_r(i-1) ) then                         ! hmhj
              reall    = i-1                                             ! hmhj
              rlcs2(i) = reall*rcs2_r(ipt_ls)/rerth                      ! hmhj
            else                                                         ! hmhj
              rlcs2(i) = cons0     !constant                             ! hmhj
            endif                                                        ! hmhj
          enddo                                                          ! hmhj

!$omp parallel do private(k,i,item,jtem)
          do k = 1, levs                                                 ! hmhj
            item = kdtlam-1+k
            jtem = kst   -1+k

            do i = 1, lmax+1                                             ! hmhj
              dyn_gr_r_1(i+i-1,item,lan) = -for_gr_r_1(i+i,jtem,lan)    &
     &                                   *  rlcs2(i)                     ! hmhj
              dyn_gr_r_1(i+i,item,lan)   =  for_gr_r_1(i+i-1,jtem,lan)  &
     &                                   *  rlcs2(i)                     ! hmhj
            enddo                                                        ! hmhj
          enddo                                                          ! hmhj
        endif       ! gc and vertcoord_id=3

!       print*,' beginning call four2grid',lan
        call four_to_grid(for_gr_r_1(1,1,lan),for_gr_r_2(1,1,lan),      &
     &                    lon_dim_r,lonr,lons_lat,5*levs+3)

!       print*,' after first call four2grid',lan

        if ( gg_tracers ) then
!  --- ...  set tracers grid values from layout_grid_tracers

          if ( .not. shuff_lats_r ) then
!  --- ...  set for_gr_r_2 to rgt_a  from gloopa

            do nt = 1, ntrac
!$omp parallel do private(k,i,item,jtem)
              do k = 1, levs
                item = ksr - 1 + k +(nt-1)*levs
                jtem = lats_node_a+1-lan

                do i = 1, min(lonf,lons_lat)
                  for_gr_r_2(i,item,lan) = rgt_a(i,k,jtem,nt)
                enddo
              enddo
            enddo
          endif ! not_shuff_lats_r

        else
!         print *,' begin second call to four_to_grid in gloopr'

          call four_to_grid(for_gr_r_1(1,ksr,lan),for_gr_r_2(1,ksr,lan),&
     &                      lon_dim_r,lonr,lons_lat,levh)
        endif
!       print *,' after second call to four_to_grid in gloopr'

        if ( gen_coord_hybrid .and. vertcoord_id == 3. ) then            ! hmhj
          call four_to_grid(dyn_gr_r_1(1,1,lan),dyn_gr_r_2(1,1,lan),    &! hmhj
     &                      lon_dim_r,lonr,lons_lat,levs)                ! hmhj

          call four_to_grid(dyn_gr_r_1(1,kdtlam,lan),                   &! hmhj
     &                      dyn_gr_r_2(1,kdtlam,lan),                   &! hmhj
     &                      lon_dim_r,lonr,lons_lat,levs)                ! hmhj
        endif       ! gc and vertcoord_id=3

!       print*,' completed call four2grid lan=',lan

        if ( .not. gen_coord_hybrid ) then                               ! hmhj
          do k = 1, levs
            item = ksr-1+k
            jtem = kst-1+k

            do j = 1, lons_lat
              if (for_gr_r_2(j,item,lan) <= qmin) then
                 for_gr_r_2(j,item,lan) = qmin
              endif

              for_gr_r_2(j,jtem,lan) = for_gr_r_2(j,jtem,lan)           &
     &                               / (1.0 + fv*for_gr_r_2(j,item,lan))
            enddo
          enddo

!         print *,' now do sfc pressure for lan=',lan

          do j = 1, lons_lat
            for_gr_r_2(j,ksq,lan) = exp( for_gr_r_2(j,ksq,lan) )
          enddo

!         print *,' after sfc pressure for lan=',lan
        endif   ! not_gen_coord_hybrid

!       timer2 = rtc()
!       global_times_r(lat,me+1) = timer2 - timer1

!       print*,'timeloopr',me,timer1,timer2,global_times_r(lat,me+1)
      enddo   ! do_lan_loop


!  --- ...  starting latitude loop

      do lan = 1, lats_node_r

        lat = global_lats_r(ipt_lats_node_r-1+lan)
        lons_lat = lonsperlar(lat)

!       print *,' lan=',lan

!$omp parallel do schedule(dynamic,1) private(lon,i,j,k)
!$omp+private(vvel,gu,gv1,gd,gt,gr,gr1,gq,gphi,glam)
!$omp+private(gtv,gtvx,gtvy,sumq,xcp)
!$omp+private(cldcov_v,fluxr_v,f_ice,f_rain,r_rime)
!$omp+private(deltaq,cnvw,cnvc)
!$omp+private(coszen_v,coszdg_v,sinlat_v,coslat_v)
!$omp+private(prslk,prsl,prsik,prsi,flgmin_v,hlw_v,swh_v)
!$omp+private(hlwc_v,swhc_v)
!$omp+private(njeff,n,item,jtem,ks,work1,work2)
!$omp+private(icsdsw,icsdlw,topfsw,sfcfsw,topflw,sfcflw)
!$omp+private(lprnt,ipt,dbgu)

        do lon = 1, lons_lat, ngptc

          njeff   = min(ngptc,lons_lat-lon+1)

          dbgu = 300 + lon
!         print *,' ngptc=',ngptc,' dbgu=',dbgu
!         write(dbgu,*)' dbgu=',dbgu,' lon=',lon,' lan=',lan
          lprnt = .false.

!  --- ...  for debug test
!         alon = 139.20
!         alat = 74.71
!         alon = 97.5
!         alat = -6.66
          ipt = 1
!         do i = 1, njeff
!           item = lon + i - 1
!           temlon = xlon(item,lan) * 57.29578
!           if (temlon < 0.0) temlon = temlon + 360.0
!           temlat = xlat(item,lan) * 57.29578
!           lprnt = abs(temlon-364.5) < 0.5 .and. abs(temlat-65.7) < 0.1&
!    &             .and. kdt > 13
!           lprnt = abs(temlon-alon) < 0.1 .and. abs(temlat-alat) < 0.1 &
!    &             .and. kdt > 0
!           if ( lprnt ) then
!             ipt = item
!             print *,' ipt=',ipt,' lon=',lon,' lan=',lan
!             exit
!           endif
!         enddo
!         lprnt = .false.

          if (ntcw <= 0) then
            do k = 1, levs
              do j = 1, njeff
                jtem = lon-1+j
                gu (j,k)  = for_gr_r_2(jtem,ksu-1+k,lan)
                gv1(j,k)  = for_gr_r_2(jtem,ksv-1+k,lan)
                gd (j,k)  = for_gr_r_2(jtem,ksd-1+k,lan)
              enddo
            enddo
          endif

          if ( gen_coord_hybrid ) then                                   ! hmhj

            do k=1,levr                                                  ! hmhj
              do j=1,njeff                                               ! hmhj
                jtem = lon-1+j
                gtv (j,k) = for_gr_r_2(jtem,kst-1+k,lan)
                gr  (j,k) = max(qmin, for_gr_r_2(jtem,ksr-1+k,lan))
              enddo                                                      ! hmhj
            enddo

            if ( vertcoord_id == 3. .and. ntcw <= 0 ) then
              do k=1,levs                                                ! hmhj
                do j=1,njeff                                             ! hmhj
                  jtem = lon-1+j
                  gtvx(j,k) = dyn_gr_r_2(jtem,kdtlam-1+k,lan)
                  gtvy(j,k) = dyn_gr_r_2(jtem,kdtphi-1+k,lan)
                enddo                                                    ! hmhj
              enddo
            endif

            if ( thermodyn_id == 3 ) then  ! get dry temperature from enthalpy
              do k=1,levr						! hmhj
                do j=1,njeff			    			! hmhj
                  sumq(j,k) = 0.0					! hmhj
                  xcp(j,k)  = 0.0					! hmhj
                enddo
              enddo

              do i=1,ntrac						! hmhj
                if( cpi(i).ne.0.0 ) then				! hmhj
                  ks = ksr+(i-1)*levs					! hmhj

                  do k=1,levr						! hmhj
                    item = ks-1+k

                    do j=1,njeff					! hmhj
                      jtem = lon-1+j
                      sumq(j,k) = sumq(j,k) + for_gr_r_2(jtem,item,lan)	! hmhj
                      xcp(j,k)  = xcp(j,k)                              &
     &                          + cpi(i)*for_gr_r_2(jtem,item,lan)	! hmhj
                    enddo						! hmhj
                  enddo							! hmhj
                endif							! hmhj
              enddo							! hmhj

              do k=1,levr						! hmhj
                do j=1,njeff						! hmhj
                  xcp(j,k) = (1.-sumq(j,k))*cpi(0) + xcp(j,k)  		! hmhj
                  gt(j,k)  = gtv(j,k) / xcp(j,k) 			! hmhj
                enddo							! hmhj
              enddo							! hmhj
            else if( thermodyn_id <= 1 ) then ! get dry temperature from virtual temp
              do k=1,levr                                               ! hmhj
                do j=1,njeff                                            ! hmhj
                  gt(j,k) = gtv(j,k) / (1.+fv*gr(j,k))  	        ! hmhj
                enddo                                                   ! hmhj
              enddo							! hmhj
            else                              ! get dry temperature from dry temp
              do k=1,levr                                               ! hmhj
                do j=1,njeff                                            ! hmhj
                  gt(j,k) = gtv(j,k)                                    ! hmhj
                enddo                                                   ! hmhj
              enddo 
            endif

          else    ! if gen_coord_hybrid                                 ! hmhj

            do k = 1, levr
              do j = 1, njeff
                 jtem = lon-1+j
                 gt(j,k) = for_gr_r_2(jtem,kst-1+k,lan)
                 gr(j,k) = for_gr_r_2(jtem,ksr-1+k,lan)
              enddo
            enddo

          endif   ! if gen_coord_hybrid

!  --- ...  remaining tracers

          do n = 1, ntrac-1
            do k = 1, levr
              item = ksr-1+k+n*levs

              do j = 1, njeff
                gr1(j,k,n) = for_gr_r_2(lon-1+j,item,lan)
              enddo
            enddo
          enddo

          if (ntcw > 0) then
            do j = 1, njeff
              gq  (j) = for_gr_r_2(lon-1+j,ksq,lan)
            enddo
          else
            do j = 1, njeff
              jtem = lon-1+j
              gq  (j) = for_gr_r_2(jtem,ksq   ,lan)
              gphi(j) = for_gr_r_2(jtem,kspphi,lan)
              glam(j) = for_gr_r_2(jtem,ksplam,lan)
            enddo
          endif

          if(n3dzhaocld >0) then
           if(kdt == 1.or.(kdt == nsdfi+1.and.fhdfi == 0.)) then
            do k = 1, levs
              do j = 1, njeff
                 jtem = lon-1+j
                 phy_f3d(jtem,k,1,lan) = gt(j,k)
                 phy_f3d(jtem,k,2,lan) = gr(j,k)
                 phy_f3d(jtem,k,3,lan) = gt(j,k)
                 phy_f3d(jtem,k,4,lan) = gr(j,k)
              enddo
            enddo 
            do j=1,njeff
               jtem = lon + j - 1
               phy_f2d(jtem,2,lan) = gq(j) * 1000.0
               phy_f2d(jtem,3,lan) = gq(j) * 1000.0  
            enddo
           endif 
          endif 

!  --- ...  vertical structure variables:   del,si,sl,prslk,prdel

          if ( gen_coord_hybrid ) then                                   ! hmhj

            call  hyb2press_gc(njeff,ngptc,gq,gtv,prsi,prsl,prsik,prslk) ! hmhj

            if (ntcw <= 0)                                              &
     &      call omegtes_gc(njeff,ngptc,rcs2_r(min(lat,latr-lat+1)),    &! hmhj
     &                     gq,gphi,glam,gtv,gtvx,gtvy,gd,gu,gv1,vvel)    ! hmhj

          elseif (hybrid) then

            call  hyb2press(njeff,ngptc,gq, prsi, prsl,prsik, prslk)

            if (ntcw <= 0)                                              &
     &      call omegtes(njeff,ngptc,rcs2_r(min(lat,latr-lat+1)),       &
     &                   gq,gphi,glam,gd,gu,gv1,vvel)

          endif

          if (levr .lt. levs) then
            do j=1,njeff
              prsi(j,levr+1)  = prsi(j,levp1)
              prsl(j,levr)    = (prsi(j,levp1) + prsi(j,levr)) * 0.5
              prsik(j,levr+1) = prslk(j,levp1)
              prslk(j,levr)   = (prsl(j,levr)*0.01) ** rocp
            enddo
          endif

          do k=1,nfxr
            do j=1,njeff
              fluxr_v(j,k) = fluxr(lon+j-1,k,lan)
            enddo
          enddo

          do j = 1, njeff
            sinlat_v(j) = sinlat_r(lat)
            coslat_v(j) = coslat_r(lat)
          enddo

          if (n3dfercld >0) then
            do k = 1, levr
              do j = 1, njeff
                jtem = lon+j-1
                f_ice (j,k) = phy_f3d(jtem,k,1,lan)
                f_rain(j,k) = phy_f3d(jtem,k,2,lan)
                r_rime(j,k) = phy_f3d(jtem,k,3,lan)
              enddo
            enddo

            work1 = (log(coslat_r(lat) / (lons_lat*latg)) - dxmin)*dxinv
            work1 = max(0.0, min(1.0,work1))
            work2 = flgmin(1)*work1 + flgmin(2)*(1.0-work1)
            do j=1,njeff
              flgmin_v(j) = work2
            enddo
          else
            do j=1,njeff
              flgmin_v(j) = 0.0
            enddo
          endif

          do k = 1, levr
            do j = 1, njeff
              if(n3dzhaocld >0 .and. n3dcldpdf >0) then
                jtem = lon+j-1
                deltaq(j,k) = phy_f3d(jtem,k,n3dzhaocld+1,lan)
                cnvw(j,k)   = phy_f3d(jtem,k,n3dzhaocld+2,lan)
                cnvc(j,k)   = phy_f3d(jtem,k,n3dzhaocld+3,lan)
              else
                deltaq(j,k) = 0.0
                cnvw(j,k)   = 0.0
                cnvc(j,k)   = 0.0
              endif
            enddo
          enddo

!  --- ...  assign random seeds for sw and lw radiations

          if ( isubc_lw==2 .or. isubc_sw==2 ) then
            do j = 1, njeff
              icsdsw(j) = ixseed(lon+j-1,lan,1)
              icsdlw(j) = ixseed(lon+j-1,lan,2)
            enddo
          endif

!  --- ...  calling radiation driver

!         lprnt = me .eq. 0 .and. kdt .ge. 120
!         if (lprnt) then
!         if (kdt .gt. 85) then
!         print *,' calling grrad for me=',me,' lan=',lan,' lat=',lat   &
!    &,           ' num_p3d=',num_p3d
!         if (lan == 47) print *,' gt=',gt(1,:)
!         if (kdt > 3) call mpi_quit(5555)
!
!         if (lprnt) print *,' in grrad tsea=',tsea(ipt,lan)

          call grrad                                                    &
!  ---  inputs:
     &     ( prsi,prsl,prslk,gt,gr,gr1,vvel,slmsk(lon,lan),             &
     &       xlon(lon,lan),xlat(lon,lan),tsea(lon,lan),                 &
     &       snwdph(lon,lan),sncovr(lon,lan),snoalb(lon,lan),           &
     &       zorl(lon,lan),hprime(lon,1,lan),                           &
     &       alvsf(lon,lan),alnsf(lon,lan),alvwf(lon,lan),              &
     &       alnwf(lon,lan),facsf(lon,lan),facwf(lon,lan),              &
     &       fice(lon,lan),tisfc(lon,lan),                              &
     &       sinlat_v,coslat_v,solhr,jdat,solcon,                       &
     &       cv(lon,lan),cvt(lon,lan),cvb(lon,lan),                     &
     &       f_ice,f_rain,r_rime,flgmin_v,                              &
     &       icsdsw,icsdlw,ntcw-1,ncld,ntoz-1,ntrac-1,nfxr,             &
     &       dtlw,dtsw,lsswr,lslwr,lssav,                               &
     &       ngptc,njeff,levr, me, lprnt,ipt,kdt,deltaq,sup,cnvw,cnvc,  &
!  ---  outputs:
     &       swh_v,topfsw,sfcfsw,sfalb(lon,lan),coszen_v,coszdg_v,      &
     &       hlw_v,topflw,sfcflw,tsflw(lon,lan),                        &
     &       sfcemis(lon,lan),cldcov_v,                                 &
!  ---  input/output:
     &       fluxr_v,                                                   &
!    &       fluxr_v,dbgu                                               &
     &       htrlw0=hlwc_v,htrsw0=swhc_v                                &
     &       )
!         print *, 'after grrad lw',
!    &    minval(hlw_v),maxval(hlw_v),minval(hlwc_v),maxval(hlwc_v)
!         print *, 'after grrad sw',
!    &    minval(swh_v),maxval(swh_v),minval(swhc_v),maxval(swhc_v)

          do j = 1, njeff
            coszen(lon+j-1,lan) = coszen_v(j)
            coszdg(lon+j-1,lan) = coszdg_v(j)
          enddo

!  ---  check print
!         if (lprnt) print *,' returned from grrad for me=',me,' lan=', &
!    &      lan,' lat=',lat,' kdt=',kdt,' tsea=',tsea(ipt,lan)
!         print *,' end gloopr hlw=',hlw(lon,:,lan),' lan=',lan
!
!         if (lprnt) print *,' swh_vg=',swh_v(ipt,:)

          if (lssav) then
            if (ldiag3d) then
              do k=1,levr
                do j=1,njeff
                  cldcov(lon+j-1,k,lan) = cldcov(lon+j-1,k,lan)         &
     &                                  + cldcov_v(j,k) * raddt
                enddo
              enddo
            endif
          endif

          do k=1,nfxr
            do j=1,njeff
              fluxr(lon+j-1,k,lan) = fluxr_v(j,k)
            enddo
          enddo

!  --- ...  radiation fluxes and heating rates

          if (lsswr) then
            do i = 1, njeff
              jtem = lon + i - 1
              sfcdsw(jtem,lan) = sfcfsw(i)%dnfxc
              sfcnsw(jtem,lan) = sfcfsw(i)%dnfxc - sfcfsw(i)%upfxc
            enddo

            do k=1,levr
              do j=1,njeff
                jtem = lon + j - 1
                swh(jtem,k,lan) = swh_v(j,k)
                swhc(jtem,k,lan) = swhc_v(j,k)
              enddo
            enddo

            if (levr < levs) then
              do k=levr+1,levs
                do j=1,njeff
                  jtem = lon + j - 1
                  swh(jtem,k,lan) = swh_v(j,levr)
                  swhc(jtem,k,lan) = swhc_v(j,levr)
                enddo
              enddo
            endif
          endif    ! end if_lsswr_block

          if (lslwr) then
            do i = 1, njeff
              jtem = lon + i - 1
              sfcdlw(jtem,lan) = sfcflw(i)%dnfxc
            enddo

            do k=1,levr
              do j=1,njeff
                jtem = lon + j - 1
                hlw(jtem,k,lan) = hlw_v(j,k)
                hlwc(jtem,k,lan) = hlwc_v(j,k)
              enddo
            enddo

            if (levr < levs) then
              do k=levr+1,levs
                do j=1,njeff
                  jtem = lon + j - 1
                  hlw(jtem,k,lan) = hlw_v(j,levr)
                  hlwc(jtem,k,lan) = hlwc_v(j,levr)
                enddo
              enddo
            endif
          endif    ! end if_lslwr_block

!  ---  check print
!         if (lat == 45 .and. me == 0 .and. lon == 1) then
!           print *,' after grrad hlw_v=',hlw_v(1,:)
!           print *,' after grrad swh_v=',swh_v(1,:)
!         endif
!         if (lprnt) print *,' hlwg=',hlw(lon+ipt-1,:,lan)
!         if (lprnt) print *,' swhg=',swh(lon+ipt-1,:,lan)
!         if (lprnt) print *,' swh_vg=',swh_v(ipt,:)
 
!         print *,' completed grrad for lan=',lan,' istrt=',istrt
        enddo    ! do_lon_loop

      enddo    ! do_lan_loop

      return
      end subroutine gloopr
