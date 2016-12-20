      module module_ras
      use machine , only : kind_phys
      use physcons, grav => con_g, cp => con_cp, alhl => con_hvap       &
     &,             alhf => con_hfus, rgas => con_rd, rkap => con_rocp  &
     &,             nu => con_fvirt
      implicit none
      save
!
      integer, parameter :: nrcmax=32 ! maximum # of random clouds per 1200s

      real (kind=kind_phys), parameter :: delt_c=1800.0/3600.0          &
!     adjustment time scales in hrs for deep and shallow clouds
!    &,                                   adjts_d=3.0, adjts_s=0.5
!    &,                                   adjts_d=2.5, adjts_s=0.5
     &,                                   adjts_d=2.0, adjts_s=0.5
!
      logical,               parameter :: fix_ncld_hr=.true.
!
      real(kind=kind_phys), parameter  :: zero=0.0, half=0.5,  one=1.0, &
     &                                    four_p2=4.e2, quart=0.25,     &
     &                                    four=4.,  one_m10=1.e-10,     &
     &                                    one_m6=1.e-6, one_m5=1.e-5,   &
     &                                    one_m2=1.e-2, one_m1=1.e-1
!
      real(kind=kind_phys), parameter  :: cmb2pa = 100.0  ! conversion from mb to pa
!
      real(kind=kind_phys), parameter  ::                               &
     &           onebg   = one / grav,    gravcon = cmb2pa * onebg      &
     &,          gravfac = grav / cmb2pa, elocp   = alhl / cp           &
     &,          elfocp  = (alhl+alhf) / cp                             &
     &,          cmpor   = cmb2pa / rgas                                &
     &,          zfac    = 0.28888889e-4 * onebg
!
!     logical, parameter :: advcld=.true., advups=.true., advtvd=.false.
      logical, parameter :: advcld=.true., advups=.false., advtvd=.true.
!     logical, parameter :: advcld=.true., advups=.false.,advtvd=.false.
!
      real(kind=kind_phys)  rhmax,  qudfac, quad_lam, rhram, testmb     &
     &,                     tstmbi, hcritd, dd_dp,    rknob,  afc, eknob&
     &,                     shalfac,hcrits

!     parameter (dd_dp=1000.0, rknob=1.0, eknob=1.0)   ! no downdraft!
      parameter (dd_dp=500.0,  rknob=1.0, eknob=1.0)
!     parameter (dd_dp=500.0,  rknob=2.0, eknob=1.0)
!
      parameter (rhmax=1.0   )   ! max relative humidity
      parameter (quad_lam=1.0)   ! mask for quadratic lambda
!     parameter (rhram=0.15)     ! pbl relative humidity ramp
      parameter (rhram=0.05)     ! pbl relative humidity ramp
      parameter (hcritd=4000.0)  ! critical moist static energy
      parameter (hcrits=2000.0)  ! critical moist static energy

!     parameter (hpert_fac=1.01) ! perturbation on hbl when ctei=.true.
!     parameter (hpert_fac=1.005)! perturbation on hbl when ctei=.true.
!     parameter (hpert_fac=1.00) ! perturbation on hbl when ctei=.true.
!     parameter (qudfac=quad_lam*half, shalfac=1.0)
!     parameter (qudfac=quad_lam*half, shalfac=2.0)
      parameter (qudfac=quad_lam*half, shalfac=3.0)
!     parameter (qudfac=quad_lam*0.25)    ! yogesh's
      parameter (testmb=0.1, tstmbi=one/testmb)
!
      real(kind=kind_phys) almin1, almin2, almax
      real(kind=kind_phys) facdt
!
!     parameter (almin1=0.00e-6, almin2=2.50e-5, almax=1.0e-2)
      parameter (almin1=0.00e-6, almin2=0.00e-5, almax=1.0e-2)
!     parameter (almin1=0.00e-6, almin2=4.00e-5, almax=1.0e-2)
!cnt  parameter (almin1=0.00e-6, almin2=2.50e-5, almax=5.0e-3)
!
!     real(kind=kind_phys), parameter :: bldmax = 200.0
      real(kind=kind_phys), parameter :: bldmax = 300.0
!!    real(kind=kind_phys), parameter :: bldmax = 350.0
!
      real(kind=kind_phys) c0, c0i, qi0, qw0, c00, c00i, dlq_fac
      parameter (qi0=1.0e-5, qw0=1.0e-5)
!     parameter (qi0=1.0e-4, qw0=1.0e-5) ! 20050509
!     parameter (qi0=1.0e-5, qw0=1.0e-6)
!!!   parameter (c0i=1.0e-3)
      parameter (c00i=1.0e-3)
!     parameter (c0=1.0e-3)
!     parameter (c0=1.5e-3)
!!!   parameter (c0=2.0e-3)
      parameter (c00=2.0e-3)
!
      real(kind=kind_phys) tf, tcr, tcrf, tcl
!     parameter (tf=130.16, tcr=160.16, tcrf=1.0/(tcr-tf),tcl=2.0)
!     parameter (tf=230.16, tcr=260.16, tcrf=1.0/(tcr-tf))
      parameter (tf=233.16, tcr=263.16, tcrf=1.0/(tcr-tf),tcl=2.0)
!
!     for tilting angle specification
!
      real(kind=kind_phys) refp(6), refr(6), tlac(8), plac(8), tlbpl(7) &
     &,                    drdp(5), vtp
!
      data plac/100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0/
      data tlac/ 35.0,  25.0,  20.0,  17.5,  15.0,  12.5,  10.0,  7.5/
      data refp/500.0, 300.0, 250.0, 200.0, 150.0, 100.0/
      data refr/ 1.0,   2.0,  3.0,   4.0,   6.0,   8.0/
!
      real(kind=kind_phys) ac(16), ad(16)
!
      integer, parameter :: nqrp=500001
      real(kind=kind_phys) c1xqrp, c2xqrp, tbqrp(nqrp), tbqra(nqrp)     &
     &,                    tbqrb(nqrp)
!
      integer, parameter :: nvtp=10001
      real(kind=kind_phys) c1xvtp, c2xvtp, tbvtp(nvtp)
!
      contains
!
      subroutine set_ras_afc(dt)
      implicit none
      real(kind=kind_phys) dt
!     afc = -(1.04e-4*dt)*(3600./dt)**0.578
      afc = -(1.01097e-4*dt)*(3600./dt)**0.57777778
      end subroutine set_ras_afc

      subroutine ras_init(levs,  me)
!
      implicit none
!
      integer levs
!
      real(kind=kind_phys) actp,   facm, tem,  actop
      integer              i, l, me
      parameter (actp=1.7,   facm=1.00)
!
      real(kind=kind_phys) ph(15),    a(15)
!
      data ph/150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0    &
     &,       550.0, 600.0, 650.0, 700.0, 750.0, 800.0, 850.0/
!
       data a/ 1.6851, 1.1686, 0.7663, 0.5255, 0.4100, 0.3677           &
     &,       0.3151, 0.2216, 0.1521, 0.1082, 0.0750, 0.0664            &
     &,       0.0553, 0.0445, 0.0633/
!
      logical first
      data first/.true./
!
      if (first) then
!                                   set critical workfunction arrays
        actop = actp*facm
        do l=1,15
          a(l) = a(l)*facm
        enddo
        do l=2,15
          tem   = 1.0 / (ph(l) - ph(l-1))
          ac(l) = (ph(l)*a(l-1) - ph(l-1)*a(l)) * tem
          ad(l) = (a(l) - a(l-1)) * tem
        enddo
        ac(1)  = actop
        ac(16) = a(15)
        ad(1)  = 0.0
        ad(16) = 0.0
!
        call setqrp
        call setvtp
!
        do i=1,7
          tlbpl(i) = (tlac(i)-tlac(i+1)) / (plac(i)-plac(i+1))
        enddo
        do i=1,5
          drdp(i)  = (refr(i+1)-refr(i)) / (refp(i+1)-refp(i))
        enddo
!
        vtp    = 36.34*sqrt(1.2)* (0.001)**0.1364
!
        if (me .eq. 0) print *,' no downdraft for cloud types'          &
     &,        ' detraining within the bottom ',dd_dp,' hpa layers'
!
        first = .false.
      endif
!
      end subroutine ras_init
      end module module_ras
!
      module module_rascnv
!
      use machine , only : kind_phys
      implicit none
      save
!
      logical revap, cumfrc
      logical wrkfun, calkbl, crtfun, updret, botop, vsmooth

      real(kind=kind_phys), parameter :: frac=0.5                       &
     &,                                  rhfacs=0.70, rhfacl=0.70       &
     &,                                  face=5.0,    delx=10000.0      &
     &,                                  ddfac=face*delx*0.001          &
     &,                                  max_neg_bouy=0.15
!    &,                                  max_neg_bouy=0.25

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    real(kind=kind_phys) frac, crtmsf, max_neg_bouy, rhfacs, rhfacl   &
!!   &,                    face, delx,   ddfac
!     parameter (frac=0.1, crtmsf=0.0)
!     parameter (frac=0.25, crtmsf=0.0)
!!    parameter (frac=0.5, crtmsf=0.0)
!     parameter (max_neg_bouy=0.15, revap=.true., cumfrc=.false.)
!     parameter (max_neg_bouy=0.15, revap=.true., cumfrc=.true.)
!     parameter (max_neg_bouy=0.10, revap=.true., cumfrc=.true.)
!     parameter (max_neg_bouy=0.20, revap=.true., cumfrc=.true.)
!!    parameter (max_neg_bouy=0.25, revap=.true., cumfrc=.true.)
!     parameter (max_neg_bouy=0.30, revap=.true., cumfrc=.true.)
!!    parameter (max_neg_bouy=0.05, revap=.true., cumfrc=.true.)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      parameter (                   revap=.true., cumfrc=.true.)
      parameter (wrkfun = .false.,  updret = .false., vsmooth=.false.)
!     parameter (crtfun = .true.,   calkbl = .false., botop=.true.)
      parameter (crtfun = .true.,   calkbl = .true., botop=.true.)
!
!!    parameter (rhfacs=0.70, rhfacl=0.70)
!     parameter (rhfacs=0.75, rhfacl=0.75)
!     parameter (rhfacs=0.85, rhfacl=0.85)
!     parameter (rhfacs=0.80, rhfacl=0.80)   ! august 26, 2008
!     parameter (rhfacs=0.80, rhfacl=0.85)
!!    parameter (face=5.0, delx=10000.0, ddfac=face*delx*0.001)
!
!     real (kind=kind_phys), parameter :: pgftop=0.7, pgfbot=0.3        &
!     real (kind=kind_phys), parameter :: pgftop=0.75, pgfbot=0.35      &
!    for pressure gradient force in momentum mixing
!     real (kind=kind_phys), parameter :: pgftop=0.80, pgfbot=0.30      &
!    no pressure gradient force in momentum mixing
      real (kind=kind_phys), parameter :: pgftop=0.0, pgfbot=0.0        &
!     real (kind=kind_phys), parameter :: pgftop=0.55, pgfbot=0.55      &
     &,          pgfgrad=(pgfbot-pgftop)*0.001
!
      end module module_rascnv
!
!
      subroutine rascnv(im,    ix,     k,      dt,    dtf,  rannum      &
     &,                 tin,   qin,    uin,    vin,   ccin,  trac       &
     &,                 prsi,  prsl,   prsik,  prslk, phil,  phii       &
     &,                 kpbl,  cdrag,  rainc,  kbot,  ktop,  kcnv       &
     &,                 ddvel, flipv,  facmb,  me,    garea, lmh, ccwfac&
     &,                 nrcm,  rhc,    ud_mf, dd_mf,  det_mf, dlqfac    &
     &,                 lprnt, ipr, kdt, fscav)
!    &,                 lprnt, ipr, kdt, fscav, ctei_r, ctei_rm)
!
!*********************************************************************
!*********************************************************************
!************         relaxed arakawa-schubert      ******************
!************             parameterization          ******************
!************          plug compatible driver       ******************
!************               23 may 2002             ******************
!************                                       ******************
!************               developed by            ******************
!************                                       ******************
!************             shrinivas moorthi         ******************
!************                                       ******************
!************                  emc/ncep             ******************
!*********************************************************************
!*********************************************************************
!
!
      use machine , only : kind_phys
      use module_ras, dpd => dd_dp
      use module_rascnv
      implicit none
!
      logical flipv, lprnt
!
!      input
!
      integer im, ix, k, ncrnd, me, trac, ipr, nrcm
      integer kbot(im), ktop(im), kcnv(im), kpbl(im), lmh(im), kdt
!
      real(kind=kind_phys) tin(ix,k),     qin(ix,k),  uin(ix,k)         &
     &,                    vin(ix,k),     prsi(ix,k+1)                  &
     &,                    prsik(ix,k+1), prsl(ix,k), prslk(ix,k+1)     &
     &,                    phil(ix,k),    phii(ix,k+1),ccwfac(im)       &
     &,                    ccin(ix,k,trac+2)                            &
!    &,                    prsik(ix,k+1), clt(ix,k)                     &
     &,                    rainc(im),     cdrag(im),  ddvel(im)         &
     &,                    rannum(ix,nrcm),dlqfac                       &
     &,                    ud_mf(im,k), dd_mf(im,k), det_mf(im,k)
      real(kind=kind_phys) dt, facmb, garea(im), dtf, rhc(im,k)         &
!
!     added for aerosol scavenging for gocart
!
      real(kind=kind_phys), intent(in) :: fscav(trac)

!    &,                                   ctei_r(im), ctei_rm
!
!     locals
!
!     real(kind=kind_phys) rain,     toi(k), qoi(k),  uvi(k,trac+2)     &
      real(kind=kind_phys) rain,     toi(k), qoi(k)                     &
     &,                    tcu(k),   qcu(k), pcu(k),   clw(k), cli(k)   &
     &,                    qii(k),   qli(k), prs(k+1), psj(k+1)         &
     &,                    phi_l(k), phi_h(k+1)                         &
     &,                    wfnc,     flx(k+1), flxd(k+1)
      real(kind=kind_phys) tla,clwmin
      integer icm,irnd,ib

      parameter (icm=100, clwmin=1.0e-10)
      integer  ic(icm)
!
      real(kind=kind_phys), allocatable ::  alfint(:,:), uvi(:,:)
     &,                                     trcfac(:,:), rcu(:,:)
      real(kind=kind_phys)            prsm(k),  psjm(k)                 &
     &,                    alfind(k), rhc_l(k), dtvd(2,4)
!    &,                    dpi(k),    psjp(k+1)              
      real(kind=kind_phys) tem, sgc, ccwf, tem1, tem2
!
      integer              kcr,  kfx, ncmx, nc,  ktem, i,   l, lm1      &
     &,                    ntrc, ll,   km1, kp1,  ipt, kbl, n           &
     &,                    lmhij, krmin, krmax, kfmax, kblmx
!     integer              ia
      real(kind=kind_phys) sgcs(k,im)
!
      logical  dndrft, lprint
!     logical  dndrft, lprint, ctei
!
!  scavenging related parameters
!
      real                fscav_(trac+2)  ! fraction scavenged per km
!
      fscav_ = 0.0                        ! by default no scavenging
      if (trac > 0) then
        do i=1,trac
          fscav_(i) = fscav(i)
        enddo
      endif
!
      km1    = k - 1
      kp1    = k + 1
!
      dlq_fac = dlqfac
      tem     = 1.0 + dlq_fac
      c0      = c00  * tem
      c0i     = c00i * tem
!
      ntrc = trac
      if (cumfrc) then
        ntrc = ntrc + 2
      endif
      if (ntrc > 0) then
        if (.not. allocated(trcfac)) allocate (trcfac(k,ntrc))
        if (.not. allocated(uvi)) allocate (uvi(k,ntrc))
        if (.not. allocated(rcu)) allocate (rcu(k,ntrc))
        do n=1, ntrc
          do l=1,k
            trcfac(l,n) = 1.0         !  for other tracers
          enddo
        enddo
      endif
!
      if (.not. allocated(alfint)) allocate(alfint(k,ntrc+4))
!
      call set_ras_afc(dt)
!
      do ipt=1,im

        ccwf = 0.5
        if (ccwfac(ipt) >= 0.0) ccwf = ccwfac(ipt)

!
!       ctei = .false.
!       if (ctei_r(ipt) > ctei_rm) ctei = .true.
!

        do l=1,k
          ud_mf(ipt,l)  = 0.0
          dd_mf(ipt,l)  = 0.0
          det_mf(ipt,l) = 0.0
        enddo
!
!     compute ncrnd  : here lmh is the number of layers above the
!                      bottom surface.  for sigma coordinate lmh=k.
!                      if flipv is true, then input variables are from bottom
!                      to top while ras goes top to bottom
!
        lmhij = lmh(ipt)
        if (flipv) then
           ll  = kp1 - lmhij
           tem = 1.0 / prsi(ipt,ll)
        else
           ll  = lmhij
           tem = 1.0 / prsi(ipt,ll+1)
        endif
        krmin = 1
        krmax = km1
        kfmax = krmax
        kblmx = 1
        do l=1,lmhij-1
          ll = l
          if (flipv) ll = kp1 -l ! input variables are bottom to top!
          sgc = prsl(ipt,ll) * tem
          sgcs(l,ipt) = sgc
          if (sgc .le. 0.050) krmin = l
!         if (sgc .le. 0.700) krmax = l
!         if (sgc .le. 0.800) krmax = l
          if (sgc .le. 0.760) krmax = l
!         if (sgc .le. 0.930) kfmax = l
          if (sgc .le. 0.970) kfmax = l    ! commented on 20060202
!         if (sgc .le. 0.700) kblmx = l    ! commented on 20101015
          if (sgc .le. 0.600) kblmx = l    ! 
!         if (sgc .le. 0.650) kblmx = l    ! commented on 20060202
        enddo
        krmin = max(krmin,2)

!     if (lprnt .and. ipt .eq. ipr) print *,' krmin=',krmin,' krmax=',
!    &krmax,' kfmax=',kfmax,' lmhij=',lmhij,' tem=',tem
!
        if (fix_ncld_hr) then
!!!       ncrnd = min(nrcmax, (krmax-krmin+1)) * (dtf/1200) + 0.50001
          ncrnd = min(nrcmax, (krmax-krmin+1)) * (dtf/1200) + 0.10001
!!        ncrnd = min(nrcmax, (krmax-krmin+1)) * (dtf/600) + 0.50001
!         ncrnd = min(nrcmax, (krmax-krmin+1)) * (dtf/360) + 0.50001
!    &                                         + 0.50001
!         ncrnd = min(nrcmax, (krmax-krmin+1)) * min(1.0,dtf/360) + 0.1
          facdt = delt_c / dt
        else
          ncrnd = min(nrcmax, (krmax-krmin+1))
          facdt = 1.0 / 3600.0
        endif
        ncrnd   = min(nrcm,max(ncrnd, 1))
!
        kcr     = min(lmhij,krmax)
        ktem    = min(lmhij,kfmax)
        kfx     = ktem - kcr

!     if(lprnt)print*,' enter rascnv k=',k,' ktem=',ktem,' lmhij='
!    &,                 lmhij
!    &,               ' krmax=',krmax,' kfmax=',kfmax
!    &,               ' kcr=',kcr, ' cdrag=',cdrag(ipr)
 
        if (kfx .gt. 0) then
           if (botop) then
              do nc=1,kfx
                ic(nc) = ktem + 1 - nc
              enddo
           else
              do nc=kfx,1,-1
               ic(nc) = ktem + 1 - nc
              enddo
           endif
        endif
!
        ncmx  = kfx + ncrnd
        if (ncrnd .gt. 0) then
          do i=1,ncrnd
            irnd = (rannum(ipt,i)-0.0005)*(kcr-krmin+1)
            ic(kfx+i) = irnd + krmin
          enddo
        endif
!
!     ia = 1
!
!     print *,' in rascnv: k=',k,'lat=',lat,' lprnt=',lprnt
!     if (lprnt) then
!        if (me .eq. 0) then
!        print *,' tin',(tin(ia,l),l=k,1,-1)
!        print *,' qin',(qin(ia,l),l=k,1,-1)
!     endif
!
!
        lprint = lprnt .and. ipt .eq. ipr
!       lprint = lprnt
        do l=1,k
          ll = l
          if (flipv) ll = kp1 -l ! input variables are bottom to top!
          clw(l)     = 0.0       ! assumes initial value of cloud water
          cli(l)     = 0.0       ! assumes initial value of cloud ice
                                 ! to be zero i.e. no environmental condensate!!!
          qii(l)     = 0.0
          qli(l)     = 0.0
!                          initialize heating, drying, cloudiness etc.
          tcu(l)     = 0.0
          qcu(l)     = 0.0
          pcu(l)     = 0.0
          flx(l)     = 0.0
          flxd(l)    = 0.0
          rcu(l,1)   = 0.0
          rcu(l,2)   = 0.0
!                          transfer input prognostic data into local variable
          toi(l)     = tin(ipt,ll)
          qoi(l)     = qin(ipt,ll)
!
          if (ntrc > trac) then               ! cumfrc is true 
            uvi(l,trac+1) = uin(ipt,ll)
            uvi(l,trac+2) = vin(ipt,ll)
          endif
!
          if (trac > 0) then                  ! tracers such as o3, dust etc
            do n=1,trac
              uvi(l,n) = ccin(ipt,ll,n+2)
              if (abs(uvi(l,n)) < 1.0e-20) uvi(l,n) = 0.0
            enddo
          endif
!
        enddo
        flx(k+1)  = 0.0
        flxd(k+1) = 0.0
!
        if (ccin(ipt,1,2) .le. -999.0) then  ! input ice/water are together 
          do l=1,k
            ll = l
            if (flipv) ll = kp1 -l ! input variables are bottom to top!
              tem = ccin(ipt,ll,1)                                      &
     &            * max(zero, min(one, (tcr-toi(l))*tcrf))
              ccin(ipt,ll,2) = ccin(ipt,ll,1) - tem
              ccin(ipt,ll,1) = tem
          enddo
        endif
        if (advcld) then
          do l=1,k
            ll = l
            if (flipv) ll = kp1 -l ! input variables are bottom to top!
            qii(l) = ccin(ipt,ll,1)
            qli(l) = ccin(ipt,ll,2)
          enddo
        endif
!
        kbl  = kpbl(ipt)
        if (flipv) kbl  = max(min(k, kp1-kpbl(ipt)), k/2)
        rain = 0.0
!
        do l=1,kp1
          ll = l
          if (flipv) ll = kp1 + 1 - l      ! input variables are bottom to top!
          prs(ll)   = prsi(ipt, l) * facmb ! facmb is for conversion to mb
          psj(ll)   = prsik(ipt,l)
          phi_h(ll) = phii(ipt,l)
        enddo
!
        do l=1,k
          ll = l
          if (flipv) ll = kp1 - l          ! input variables are bottom to top!
          prsm(ll)  = prsl(ipt, l) * facmb ! facmb is for conversion to mb
          psjm(ll)  = prslk(ipt,l)
          phi_l(ll) = phil(ipt,l)
          rhc_l(ll) = rhc(ipt,l)
        enddo
!
!     if (lprnt .and. ipt == ipr) print *,' phi_h=',phi_h(:)
!     if(lprint) print *,' prs=',prs
!     if(lprint) print *,' prsm=',prsm
!     if (lprint) then
!        print *,' qns=',qns(ia),' qoi=',qn0(ia,k),'qin=',qin(ia,1)
!        if (me .eq. 0) then
!        print *,' toi',(tn0(ia,l),l=1,k)
!        print *,' qoi',(qn0(ia,l),l=1,k),' kbl=',kbl
!     endif
!
!
!       do l=k,kctop(1),-1
!!        dpi(l)  = 1.0 / (prs(l+1) - prs(l))
!       enddo
!
!     print *,' ipt=',ipt

        if (advups) then               ! for first order upstream for updraft
          alfint(:,:) = 1.0
        elseif (advtvd) then           ! tvd flux limiter scheme for updraft
          alfint(:,:) = 1.0
          l   = krmin
          lm1 = l - 1
          dtvd(1,1) = cp*(toi(l)-toi(lm1)) + phi_l(l)-phi_l(lm1)        &
     &              + alhl*(qoi(l)-qoi(lm1))
          dtvd(1,2) = qoi(l) - qoi(lm1)
          dtvd(1,3) = qli(l) - qli(lm1)
          dtvd(1,4) = qii(l) - qii(lm1)
          do l=krmin+1,k
            lm1 = l - 1

!     print *,' toi=',toi(l),toi(lm1),' phi_l=',phi_l(l),phi_l(lm1)
!    &,' qoi=',qoi(l),qoi(lm1),' cp=',cp,' alhl=',alhl

            dtvd(2,1)   = cp*(toi(l)-toi(lm1)) + phi_l(l)-phi_l(lm1)    &
     &                  + alhl*(qoi(l)-qoi(lm1))

!     print *,' l=',l,' dtvd=',dtvd(:,1)

            if (abs(dtvd(2,1)) > 1.0e-10) then
              tem1        = dtvd(1,1) / dtvd(2,1)
              tem2        = abs(tem1)
              alfint(l,1) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for h
            endif

!     print *,' alfint=',alfint(l,1),' l=',l,' ipt=',ipt

            dtvd(1,1)   = dtvd(2,1)
!
            dtvd(2,2)   = qoi(l) - qoi(lm1)

!     print *,' l=',l,' dtvd2=',dtvd(:,2)

            if (abs(dtvd(2,2)) > 1.0e-10) then
              tem1        = dtvd(1,2) / dtvd(2,2)
              tem2        = abs(tem1)
              alfint(l,2) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for q
            endif
            dtvd(1,2)   = dtvd(2,2)
!
            dtvd(2,3)   = qli(l) - qli(lm1)

!     print *,' l=',l,' dtvd3=',dtvd(:,3)

            if (abs(dtvd(2,3)) > 1.0e-10) then
              tem1        = dtvd(1,3) / dtvd(2,3)
              tem2        = abs(tem1)
              alfint(l,3) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for ql
            endif
            dtvd(1,3)   = dtvd(2,3)
!
            dtvd(2,4)   = qii(l) - qii(lm1)

!     print *,' l=',l,' dtvd4=',dtvd(:,4)

            if (abs(dtvd(2,4)) > 1.0e-10) then
              tem1        = dtvd(1,4) / dtvd(2,4)
              tem2        = abs(tem1)
              alfint(l,4) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for qi
            endif
            dtvd(1,4)   = dtvd(2,4)
          enddo
!
          if (ntrc > 0) then
            do n=1,ntrc
              l = krmin
              dtvd(1,1)   = uvi(l,n) - uvi(l-1,n)
              do l=krmin+1,k
                dtvd(2,1)     = uvi(l,n) - uvi(l-1,n)

!     print *,' l=',l,' dtvdn=',dtvd(:,1),' n=',n,' l=',l

                if (abs(dtvd(2,1)) > 1.0e-10) then
                  tem1          = dtvd(1,1) / dtvd(2,1)
                  tem2          = abs(tem1)
                  alfint(l,n+4) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2) ! for tracers
                endif
                dtvd(1,1)     = dtvd(2,1)
              enddo
            enddo
          endif
        else
          alfint(:,:) = 0.5              ! for second order scheme
        endif
        alfind(:)   = 0.5
!
!     print *,' after alfint for ipt=',ipt

! resolution dependent press grad correction momentum mixing

        if (cumfrc) then
          do l=krmin,k
            tem = 1.0 - max(pgfbot, min(pgftop, pgftop+pgfgrad*prsm(l)))
            trcfac(l,trac+1) = tem
            trcfac(l,trac+2) = tem
          enddo
        endif
!
!       lprint = lprnt .and. ipt .eq. ipr

!     if (lprint) then
!       print *,' trcfac=',trcfac(krmin:k,1+trac)
!       print *,' alfint=',alfint(krmin:k,1)
!       print *,' alfinq=',alfint(krmin:k,2)
!       print *,' alfini=',alfint(krmin:k,4)
!       print *,' alfinu=',alfint(krmin:k,5)
!     endif
!
        if (calkbl) kbl = k
        do nc=1,ncmx
!
          ib = ic(nc)
          if (ib > kbl) cycle

!         lprint = lprnt .and. ipt .eq. ipr
!         lprint = lprnt .and. ipt .eq. ipr .and. ib .eq. 41
!
          dndrft = dpd > 0.0
!
!         if (lprint) print *,' calling cloud type ib=',ib,' kbl=',kbl
!    *,   ' kpbl=',kpbl,' alfint=',alfint,' frac=',frac
!    *,   ' ntrc=',ntrc,' ipt=',ipt
!
!****************************************************************************
!         if (advtvd) then           ! tvd flux limiter scheme for updraft
!           l   = ib
!           lm1 = l - 1
!           dtvd(1,1) = cp*(toi(l)-toi(lm1)) + phi_l(l)-phi_l(lm1)
!    &                + alhl*(qoi(l)-qoi(lm1))
!           dtvd(1,2) = qoi(l) - qoi(lm1)
!           dtvd(1,3) = qli(l) - qli(lm1)
!           dtvd(1,4) = qii(l) - qii(lm1)
!           do l=ib+1,k
!             lm1 = l - 1
!             dtvd(2,1)   = cp*(toi(l)-toi(lm1)) + phi_l(l)-phi_l(lm1)
!    &                    + alhl*(qoi(l)-qoi(lm1))
!             if (abs(dtvd(2,1)) > 1.0e-10) then
!               tem1        = dtvd(1,1) / dtvd(2,1)
!               tem2        = abs(tem1)
!               alfint(l,1) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for h
!             endif
!             dtvd(1,1)   = dtvd(2,1)
!
!             dtvd(2,2)   = qoi(l) - qoi(lm1)
!             if (abs(dtvd(2,2)) > 1.0e-10) then
!               tem1        = dtvd(1,2) / dtvd(2,2)
!               tem2        = abs(tem1)
!               alfint(l,2) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for q
!             endif
!             dtvd(1,2)   = dtvd(2,2)
!
!             dtvd(2,3)   = qli(l) - qli(lm1)
!             if (abs(dtvd(2,3)) > 1.0e-10) then
!               tem1        = dtvd(1,3) / dtvd(2,3)
!               tem2        = abs(tem1)
!               alfint(l,3) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for ql
!             endif
!             dtvd(1,3)   = dtvd(2,3)
!
!             dtvd(2,4)   = qii(l) - qii(lm1)
!             if (abs(dtvd(2,4)) > 1.0e-10) then
!               tem1        = dtvd(1,4) / dtvd(2,4)
!               tem2        = abs(tem1)
!               alfint(l,4) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for qi
!             endif
!             dtvd(1,4)   = dtvd(2,4)
!           enddo
!
!           if (ntrc > 0) then
!             do n=1,ntrc
!               l = ib
!               dtvd(1,1)   = uvi(l,n) - uvi(l-1,n)
!               do l=ib+1,k
!                 dtvd(2,1)     = uvi(l,n) - uvi(l-1,n)
!                 if (abs(dtvd(2,1)) > 1.0e-10) then
!                   tem1        = dtvd(1,1) / dtvd(2,1)
!                   tem2          = abs(tem1)
!                   alfint(l,n+4) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2) ! for tracers
!                 endif
!                 dtvd(1,1)     = dtvd(2,1)
!               enddo
!             enddo
!           endif
!         endif
!****************************************************************************
!
!     if (lprint) then
!       ia = ipt
!       print *,' toi=',(toi(ia,l),l=1,k)
!       print *,' qoi=',(qoi(ia,l),l=1,k),' kbl=',kbl
!       print *,' toi=',(toi(l),l=1,k)
!       print *,' qoi=',(qoi(l),l=1,k),' kbl=',kbl
!       print *,' prs=',(prs(l),l=1,k)
!     endif
!
          wfnc = 0.0
          do l=ib,k+1
            flx(l) = 0.0
            flxd(l)= 0.0
          enddo
!
!     if(lprint)then
!       print *,' calling cloud type ib= ', ib,' dt=',dt,' k=',k
!    &,   'ipt=',ipt
!       print *,' toi=',(toi(l),l=ib,k)
!       print *,' qoi=',(qoi(l),l=ib,k)
!       print *,' qliin=',qli
!       print *,' qiiin=',qii
!     endif
!
          tla = -10.0
!
          call cloud(lmhij, ib, ntrc, kblmx                             &
     &,              frac,  max_neg_bouy, vsmooth                       &
     &,              revap, wrkfun, calkbl, crtfun, dndrft, lprint      &
     &,              dt, kdt, tla, dpd                                  &
     &,              alfint, rhfacl, rhfacs, garea(ipt)                 &
     &,              ccwf,   cdrag(ipt), trcfac                         & 
     &,              alfind, rhc_l, phi_l, phi_h, prs, prsm,sgcs(1,ipt) &
     &,              toi, qoi, uvi, qli, qii, kbl, ddvel(ipt)           &
     &,              tcu, qcu, rcu, pcu, flx, flxd, rain, wfnc, fscav_  &
     &               )
!    &,              ctei)

!     if (lprint) then
!       print *,' rain=',rain,' ipt=',ipt
!       print *,' after calling cloud type ib= ', ib                    &
!    &,' rain=',rain,' prskd=',prs(ib),' qli=',qli(ib),' qii=',qii(ib)
!       print *,' phi_h=',phi_h(k-5:k+1)
!       print *,' toi=',(toi(l),l=1,k),' me=',me,' ib=',ib
!       print *,' qoi=',(qoi(l),l=1,k)
!       print *,' qliou=',qli
!       print *,' qiiou=',qii
!     endif
!
          do l=ib,k
            ll = l
            if (flipv) ll  = kp1 -l    ! input variables are bottom to top!
            ud_mf(ipt,ll)  = ud_mf(ipt,ll)  + flx(l+1)
            dd_mf(ipt,ll)  = dd_mf(ipt,ll)  + flxd(l+1)
          enddo
          ll = ib
          if (flipv) ll  = kp1 - ib
          det_mf(ipt,ll) = det_mf(ipt,ll) + flx(ib)
! 
!
!   warining!!!!
!   ------------
!   by doing the following, cloud does not contain environmental
!   condensate!
!
          if (.not. advcld) then
            do l=1,k
              clw(l ) = clw(l) + qli(l)
              cli(l ) = cli(l) + qii(l)
              qli(l)  = 0.0
              qii(l)  = 0.0
            enddo
          endif
!
        enddo                        ! end of the nc loop!
!
        rainc(ipt) = rain * 0.001    ! output rain is in meters

!     if (lprint) then
!       print*,' convective precip=',rain*86400/dt,' mm/day'
!    1,               ' ipt=',ipt
!        print *,' toi',(tn0(imax,l),l=1,k)
!        print *,' qoi',(qn0(imax,l),l=1,k)
!     endif
!
        do l=1,k
          ll = l
          if (flipv) ll  = kp1 - l
          tin(ipt,ll)    = toi(l)                   ! temperature
          qin(ipt,ll)    = qoi(l)                   ! specific humidity
          uin(ipt,ll)    = uvi(l,trac+1)            ! u momentum
          vin(ipt,ll)    = uvi(l,trac+2)            ! v momentum
          if (trac > 0) then
            do n=1,trac
              ccin(ipt,ll,n+2) = uvi(l,n)           ! tracers
            enddo
          endif
        enddo
        if (advcld) then
          do l=1,k
            ll = l
            if (flipv) ll  = kp1 - l
            ccin(ipt,ll,1) = qii(l)          ! cloud ice
            ccin(ipt,ll,2) = qli(l)          ! cloud water
          enddo
        else
          do l=1,k
            ll = l
            if (flipv) ll  = kp1 - l
            ccin(ipt,ll,1) = ccin(ipt,ll,1) + cli(l)
            ccin(ipt,ll,2) = ccin(ipt,ll,2) + clw(l)
          enddo
        endif
!
        ktop(ipt) = kp1
        kbot(ipt) = 0

        kcnv(ipt) = 0

        do l=lmhij-1,1,-1
          if (sgcs(l,ipt) < 0.93 .and. tcu(l) .ne. 0.0) then
!         if (sgcs(l,ipt) < 0.90 .and. tcu(l) .ne. 0.0) then
!         if (sgcs(l,ipt) < 0.85 .and. tcu(l) .ne. 0.0) then
!         if (sgcs(l,ipt) < 0.70 .and. tcu(l) .ne. 0.0) then
!         if (sgcs(l,ipt) < 0.60 .and. tcu(l) .ne. 0.0) then
!         if (tcu(l) .ne. 0.0) then
             kcnv(ipt) = 1
          endif
!  new test for convective clouds ! added in 08/21/96
          if (clw(l)+cli(l) .gt. 0.0 .or.                               &
     &        qli(l)+qii(l) .gt. clwmin) ktop(ipt) = l
        enddo
        do l=1,km1
          if (clw(l)+cli(l) .gt. 0.0 .or.                               &
     &        qli(l)+qii(l) .gt. clwmin) kbot(ipt) = l
        enddo
        if (flipv) then
          ktop(ipt) = kp1 - ktop(ipt)
          kbot(ipt) = kp1 - kbot(ipt)
        endif
!
!     if (lprint) then
!        print *,' tin',(tin(ia,l),l=k,1,-1)
!        print *,' qin',(qin(ia,l),l=k,1,-1)
!     endif
!
!     velocity scale from the downdraft!
!
        ddvel(ipt) = ddvel(ipt) * ddfac * grav / (prs(k+1)-prs(k))
!
      enddo                            ! end of the ipt loop!

      deallocate (alfint,uvi,trcfac,rcu)
!
      return
      end
      subroutine crtwrk(pl, ccwf, acr)
      use machine , only : kind_phys
      use module_ras , only : ac, ad
      implicit none
!
      real(kind=kind_phys) pl, ccwf, acr
      integer iwk
!
      iwk = pl * 0.02 - 0.999999999
      iwk = max(1, min(iwk,16))
      acr = (ac(iwk) + pl * ad(iwk)) * ccwf
!
      return
      end
      subroutine cloud(                                                 &
     &                  k, kd, ntrc, kblmx                              &
     &,                 fracbl, max_neg_bouy, vsmooth                   &
     &,                 revap, wrkfun, calkbl, crtfun, dndrft, lprnt    &
     &,                 dt, kdt, tla, dpd                               &
     &,                 alfint, rhfacl, rhfacs, garea, ccwf, cd, trcfac &
     &,                 alfind, rhc_ls, phil, phih, prs, prsm, sgcs     &
     &,                 toi, qoi, roi,  qli, qii, kpbl, dsfc            &
     &,                 tcu, qcu, rcu, pcu, flx, flxd, cup, wfnc,fscav_ &
     &                  )
!    &,                 ctei)

!
!***********************************************************************
!******************** relaxed  arakawa-schubert ************************
!****************** plug compatible scalar version *********************
!************************ subroutine cloud  ****************************
!************************  october 2004     ****************************
!********************  version 2.0  (modified) *************************
!************* shrinivas.moorthi@noaa.gov (301) 763 8000(x7233) ********
!***********************************************************************
!*reference:
!-----------
!     noaa technical report nws/ncep 99-01:
!     documentation of version 2 of relaxed-arakawa-schubert
!     cumulus parameterization with convective downdrafts, june 1999.
!     by s. moorthi and m. j. suarez.
!
!***********************************************************************
!
!===>    updates cloud tendencies due to a single cloud
!===>    detraining at level kd.
!
!***********************************************************************
!
!===>  toi(k)     inout   temperature             kelvin
!===>  qoi(k)     inout   specific humidity       non-dimensional
!===>  roi(k,ntrc)inout   tracer                  arbitrary
!===>  qli(k)     inout   liquid water            non-dimensional
!===>  qii(k)     inout   ice                     non-dimensional

!===>  prs(k+1)   input   pressure @ edges        mb
!===>  prsm(k)    input   pressure @ layers       mb
!===>  sgcs(k)    input   local sigma
!===>  phih(k+1)  input   geopotential @ edges  in mks units
!===>  phil(k)    input   geopotential @ layers in mks units
!===>  prj(k+1)   input   (p/p0)^kappa  @ edges   non-dimensional
!===>  prjm(k)    input   (p/p0)^kappa  @ layers  non-dimensional

!===>  k          input   the rise & the index of the subcloud layer
!===>  kd         input   detrainment level ( 1<= kd < k )          
!===>  ntrc       input   number of tracers. may be zero.
!===>  kblmx      input   highest level the pbl can take
!===>  dndrft     input   logical .true. or .false.
!===>  dpd        input   minumum cloud depth for downdrfat computation hpa
!
!===>  tcu(k  )   update  temperature tendency       deg
!===>  qcu(k  )   update  water vapor tendency       (g/g)
!===>  rcu(k,ntrc)update  tracer tendencies          nd
!===>  pcu(k-1)   update  precip @ base of layer     kg/m^2
!===>  flx(k  )   update  mass flux @ top of layer   kg/m^2
!===>  cup        update  precipitation at the surface kg/m^2
!
      use machine , only : kind_phys
      use module_ras
      implicit none
!
!  input arguments

!     logical revap, dndrft, wrkfun, calkbl, crtfun, calcup, ctei
      logical revap, dndrft, wrkfun, calkbl, crtfun, calcup
      logical vsmooth, lprnt
      integer k, kd, ntrc, kblmx


      real(kind=kind_phys) toi(k),    qoi(k ),  prs(k+1),  prsm(k)      &
     &,                    qli(k),    qii(k),   phih(k+1), phil(k)      &
     &,                    roi(k,ntrc), sgcs(k)
      real(kind=kind_phys) cd,        dsfc
      integer kpbl,   kbl,     kb1, kdt

      real(kind=kind_phys) fracbl, max_neg_bouy,                        &
     &                     alfint(k,ntrc+4), rhfacl, rhfacs, garea, ccwf
      real(kind=kind_phys) dpd, alfind(k), rhc_ls(k)
      real(kind=kind_phys) trcfac(k,ntrc)
 
!  update arguments

      real(kind=kind_phys) tcu(k),   qcu(k),    rcu(k,ntrc)             &
     &,                    tcd(k),   qcd(k),    pcu(k)                  &
     &,                    flx(k+1), flxd(k+1), cup

!  temporary work space

      real(kind=kind_phys) hol(kd:k),  qol(kd:k),   gaf(kd:k+1)         &
     &,                    hst(kd:k),  qst(kd:k),   tol(kd:k)           &
     &,                    gmh(kd:k),  gms(kd:k+1), gam(kd:k+1)         &
     &,                    akt(kd:k),  akc(kd:k),   bkc(kd:k)           &
     &,                    ltl(kd:k),  rnn(kd:k),   fco(kd:k)           &
     &,                                pri(kd:k)                        &
     &,                    qil(kd:k),  qll(kd:k)                        &
     &,                    zet(kd:k),  xi(kd:k),    rns(kd:k)           &
     &,                    q0u(kd:k),  q0d(kd:k),   vtf(kd:k)           &
     &,                    dlb(kd:k+1),dlt(kd:k+1), eta(kd:k+1)         &
     &,                    prl(kd:k+1)                                  &
     &,                    cil(kd:k),  cll(kd:k),   etai(kd:k)          &
     &,                    dlq(kd:k)

      real(kind=kind_phys) alm,   det,    hcc,  clp                     &
     &,                    hsu,   hsd,    qtl,  qtv                     &
     &,                    akm,   wfn,    hos,  qos                     &
     &,                    amb,   tx1,    tx2,  tx3                     &
     &,                    tx4,   tx5,    qis,  qls                     &
     &,                    hbl,   qbl,    rbl(ntrc)                     &
     &,                    qlb,   qib,    pris                          &
     &,                    wfnc,  acr                                   &
     &,                    rhc                                          &
     &,                    hstkd, qstkd,  ltlkd, q0ukd, q0dkd, dlbkd    &
     &,                    qtp, qw00, qi00, qrbkd                       &
     &,                    hstold, rel_fac, prism

      real(kind=kind_phys) fscav_(ntrc)

      real(kind=kind_phys) wrk1(kd:k), wrk2(kd:k)


      logical ep_wfn, cnvflg

      logical lowest

      real(kind=kind_phys) tl, pl, ql, qs, dqs, st1, tau,               &
     &                     qtvp, hb, qb,                                &
     &                     hccp, ds, dh, ambmax, x00, epp, qtlp,        &
     &                     dpi, dphib, dphit, del_eta, detp,            &
     &                     tem, tem1, tem2, tem3, tem4,                 &
     &                     st2, st3, st4, st5,                          &
     &                     errmin, tem5,                                &
     &                     tem6, hbd, qbd, st1s, shal_fac, hmax, hmin,  &
     &                     dhdpmn, dhdp(kd:k)
      parameter (errmin=0.0001)
      integer i, l,  n,  kd1, ii                                        &
     &,       kp1, km1, ktem
     &,       kblh, kblm, kblpmn, kmax, kmaxm1, kmaxp1, klcl, kmin, kmxb
!     integer kbls

      real avt, avq, avr, avh
!

!     real(kind=kind_phys), parameter :: rainmin=1.0e-9
      real(kind=kind_phys), parameter :: rainmin=1.0e-8

      real(kind=kind_phys) clfrac, dt, clf

      real(kind=kind_phys) actevap,deltaq,potevap                       &
     &,                    teq,qsteq,dqdt,qeq
!
!     temporary workspace and parameters needed for downdraft
!
      real(kind=kind_phys) tla
!
      real(kind=kind_phys) buy(kd:k+1), qrb(kd:k),   qrt(kd:k)          &
     &,                    etd(kd:k+1), hod(kd:k+1), qod(kd:k+1)        &
     &,                    ghd(kd:k),   gsd(kd:k),   evp(kd:k)          &
     &,                    etz(kd:k),   cldfr(kd:k), etzi(kd:k-1)       &
     &,                    train, dof, cldfrd                           &
     &,                    dpneg, hcrit
      integer idh, lcon
      logical ddft
!
!  scavenging related parameters
!
      real                delzkm          ! layer thickness in km
      real                fnoscav         ! fraction of tracer *not* scavenged
!
!***********************************************************************
!
      do l=1,k
        tcd(l) = 0.0
        qcd(l) = 0.0
      enddo
!
      kp1     = k  + 1
      km1     = k  - 1
      kd1     = kd + 1
!
!     if (lprnt) then
!       print *,' in cloud for kd=',kd
!       print *,' prs=',prs(kd:k+1)
!       print *,' phil=',phil(kd:k)
!       print *,' phih=',phih(1:k+1),' kdt=',kdt
!       print *,' phih=',phih(kd:k+1)
!       print *,' toi=',toi
!       print *,' qoi=',qoi
!     endif
!
      cldfrd   = 0.0
      dof      = 0.0
      prl(kp1) = prs(kp1)
!
      do l=kd,k
        rnn(l) = 0.0
        zet(l) = 0.0
        xi(l)  = 0.0
!
        tol(l) = toi(l)
        qol(l) = qoi(l)
        prl(l) = prs(l)
        buy(l) = 0.0
        cll(l) = qli(l)
        cil(l) = qii(l)
      enddo
!
      if (vsmooth) then
        do l=kd,k
          wrk1(l) = tol(l)
          wrk2(l) = qol(l)
        enddo
        do l=kd1,km1
          tol(l) = 0.25*wrk1(l-1) + 0.5*wrk1(l) + 0.25*wrk1(l+1)
          qol(l) = 0.25*wrk2(l-1) + 0.5*wrk2(l) + 0.25*wrk2(l+1)
        enddo
      endif
!
      do l=kd, k
        dpi    = one / (prl(l+1) - prl(l))
        pri(l) = gravfac * dpi
!
        pl     = prsm(l)
        tl     = tol(l)

        akt(l) = (prl(l+1) - pl) * dpi
!
        call qsatcn(tl, pl, qs, dqs)
!       call qsatcn(tl, pl, qs, dqs,lprnt)
!
        qst(l) = qs
        gam(l) = dqs * elocp
        st1    = one + gam(l)
        gaf(l) = (one/alhl) * gam(l)/st1
 
        ql     = max(min(qs*rhmax,qol(l)), one_m10)
        qol(l) = ql
 
        tem    = cp * tl
        ltl(l) = tem * st1 / (one+nu*(qst(l)+tl*dqs))
        vtf(l) = 1.0 + nu * ql
        eta(l) = one / (ltl(l) * vtf(l))

        hol(l) = tem + ql * alhl
        hst(l) = tem + qs * alhl
!
      enddo
!
      eta(k+1) = zero
      gms(k)   = zero
!
      akt(kd)  = half
      gms(kd)  = zero
!
      clp      = zero
!
      gam(k+1) = gam(k)
      gaf(k+1) = gaf(k)
!
      do l=k,kd1,-1
        dphib  = phil(l) - phih(l+1)
        dphit  = phih(l) - phil(l)
!
        dlb(l) = dphib * eta(l)
        dlt(l) = dphit * eta(l)
!
        qrb(l) = dphib
        qrt(l) = dphit
!
        eta(l) = eta(l+1) + dphib

        hol(l) = hol(l) + eta(l)
        hstold = hst(l)
        hst(l) = hst(l) + eta(l)
!
        eta(l) = eta(l) + dphit
      enddo
!
!     for the cloud top layer
!
      l = kd

      dphib  = phil(l) - phih(l+1)
!
      dlb(l) = dphib * eta(l)
!
      qrb(l) = dphib
      qrt(l) = dphib
!
      eta(l) = eta(l+1) + dphib

      hol(l) = hol(l) + eta(l)
      hst(l) = hst(l) + eta(l)
!
!     if (kd .eq. 12) then
!       if (lprnt) then
!         print *,' in cloud for kd=',kd,' k=',k
!         print *,' l=',l,' hol=',hol(l),' hst=',hst(l)
!         print *,' tol=',tol
!         print *,' qol=',qol
!         print *,' hol=',hol
!         print *,' hst=',hst
!       endif
!     endif
!
!     to determine kbl internally -- if kbl is defined externally
!     the following two loop should be skipped
!
!     if (lprnt) print *,' calkbl=',calkbl

      hcrit = hcritd
      if (sgcs(kd) > 0.65) hcrit = hcrits
      if (calkbl) then
         ktem = max(kd+1, kblmx)
         hmin = hol(k)
         kmin = k
         do l=km1,kd,-1
           if (hmin > hol(l)) then
             hmin = hol(l)
             kmin = l
           endif
         enddo
         if (kmin == k) return
         hmax = hol(k)
         kmax = k
         do l=km1,ktem,-1
           if (hmax < hol(l)) then
             hmax = hol(l)
             kmax = l
           endif
         enddo
         kmxb = kmax
         if (kmax < kmin) then
           kmax = k
           kmxb = k
           hmax = hol(kmax)
         elseif (kmax < k) then
           do l=kmax+1,k
             if (abs(hol(kmax)-hol(l)) > 0.5 * hcrit) then
               kmxb = l - 1
               exit
             endif
           enddo
         endif
         kmaxm1 = kmax - 1
         kmaxp1 = kmax + 1
         kblpmn = kmax
!
         dhdp(kmax:k) = 0.0
         dhdpmn = dhdp(kmax)
         do l=kmaxm1,ktem,-1
           dhdp(l) = (hol(l)-hol(l+1)) / (prl(l+2)-prl(l))
           if (dhdp(l) < dhdpmn) then
             dhdpmn = dhdp(l)
             kblpmn = l + 1
           elseif (dhdp(l) > 0.0 .and. l <= kmin) then
             exit
           endif
         enddo
         kbl = kmax
         if (kblpmn < kmax) then
           do l=kblpmn,kmaxm1
             if (hmax-hol(l) < 0.5*hcrit) then
               kbl = l
               exit
             endif
           enddo
         endif
       
!     if(lprnt) print *,' kbl=',kbl,' kbls=',kbls,' kmax=',kmax
!
         klcl = kd1
         if (kmax > kd1) then
           do l=kmaxm1,kd1,-1
             if (hmax > hst(l)) then
               klcl = l+1
               exit
             endif
           enddo
         endif
!        if(lprnt) print *,' klcl=',klcl,' ii=',ii
!        if (klcl == kd .or. klcl < ktem) return

!        this is to handle mid-level convection from quasi-uniform h

         if (kmax < kmxb) then
           kmax   = max(kd1, min(kmxb,k))
           kmaxm1 = kmax - 1
           kmaxp1 = kmax + 1
         endif


!        if (prl(kmaxp1) - prl(klcl) > 250.0 ) return

         ii  = max(kbl,kd1)
         kbl = max(klcl,kd1)
         tem = min(50.0,max(10.0,(prl(kmaxp1)-prl(kd))*0.10))
         if (prl(kmaxp1) - prl(ii) > tem .and. ii > kbl) kbl = ii

!        if(lprnt) print *,' kbl2=',kbl,' ii=',ii

         if (kbl .ne. ii) then
           if (prl(kmaxp1)-prl(kbl) > bldmax) kbl = max(kbl,ii)
         endif
         if (kbl < ii) then
           if (hol(ii)-hol(ii-1) > 0.5*hcrit) kbl = ii
         endif

!        if (prl(kbl) - prl(klcl) > 300.0 ) return
         if (prl(kbl) - prl(klcl) > 250.0 ) return
!
         kbl  = min(kmax, max(kbl,kblmx))
!        kbl  = min(kblh,kbl)
!!!
!        tem1 = max(prl(k+1)-prl(k),                                    &
!    &                     min((prl(kbl) - prl(kd))*0.05, 10.0))
!!   &                     min((prl(kbl) - prl(kd))*0.05, 20.0))
!!   &                     min((prl(kbl) - prl(kd))*0.05, 30.0))
!        if (prl(k+1)-prl(kbl) .lt. tem1) then
!          ktem = max(kd+1, kblmx)
!          do l=k,ktem,-1
!            tem = prl(k+1) - prl(l)
!            if (tem .gt. tem1) then
!              kbl = min(kbl,l)
!              exit
!            endif
!          enddo
!        endif
!        if (kbl == kblmx .and. kmax >= k-1) kbl = k - 1
!!!

         kpbl = kbl

!     if(lprnt)print*,' 1st kbl=',kbl,' kblmx=',kblmx,' kd=',kd
!     if(lprnt)print*,' tx3=',tx3,' tx1=',tx1,' tem=',tem
!    1,               ' hcrit=',hcrit

      else
         kbl  = kpbl
!     if(lprnt)print*,' 2nd kbl=',kbl
      endif

!     if(lprnt)print*,' after calkbl l=',l,' hol=',hol(l)
!    1,               ' hst=',hst(l)
!
      kbl      = min(kmax,max(kbl,kd+2))
      kb1      = kbl - 1
!!
!     if (lprnt) print *,' kbl=',kbl,' prlkbl=',prl(kbl),prl(k+1)

      if(prl(kmaxp1)-prl(kbl) .gt. bldmax .or. kb1 .le. kd) then
        return
      endif
!
!     if (lprnt) print *,' kbl=',kbl
!     write(0,*)' kbl=',kbl,' kmax=',kmax,' kmaxp1=',kmaxp1,' k=',k
!
      pris     = one / (prl(k+1)-prl(kbl))
      prism    = one / (prl(kmaxp1)-prl(kbl))
      tx1      = eta(kbl)
!
      gms(kbl) = 0.0
      xi(kbl)  = 0.0
      zet(kbl) = 0.0
!
      shal_fac = 1.0
!     if (prl(kbl)-prl(kd) < 300.0 .and. kmax == k) shal_fac = shalfac
      if (prl(kbl)-prl(kd) < 350.0 .and. kmax == k) shal_fac = shalfac
      do l=kmax,kd,-1
        if (l >= kbl) then
          eta(l) = (prl(kmaxp1)-prl(l)) * prism
        else
          zet(l) = (eta(l) - tx1) * onebg
          xi(l)  =  zet(l) * zet(l) * (qudfac*shal_fac)
          eta(l) =  zet(l) - zet(l+1)
          gms(l) =  xi(l)  - xi(l+1)
        endif
!       if (lprnt) print *,' l=',l,' eta=',eta(l),' kbl=',kbl
      enddo
      if (kmax < k) then
        do l=kmaxp1,kp1
          eta(l) = 0.0
        enddo
      endif
!
      hbl = hol(kmax) * eta(kmax)
      qbl = qol(kmax) * eta(kmax)
      qlb = cll(kmax) * eta(kmax)
      qib = cil(kmax) * eta(kmax)
      tx1 = qst(kmax) * eta(kmax)
!
      do l=kmaxm1,kbl,-1
         tem = eta(l) - eta(l+1)
         hbl = hbl + hol(l) * tem
         qbl = qbl + qol(l) * tem
         qlb = qlb + cll(l) * tem
         qib = qib + cil(l) * tem
         tx1 = tx1 + qst(l) * tem
      enddo

!     if (ctei .and. sgcs(kd) > 0.65) then
!        hbl = hbl * hpert_fac
!        qbl = qbl * hpert_fac
!     endif

!     if (lprnt) print *,' hbl=',hbl,' qbl=',qbl
!                                   find min value of hol in tx2
      tx2 = hol(kd)
      idh = kd1
      do l=kd1,kb1
        if (hol(l) .lt. tx2) then
           tx2 = hol(l)
           idh = l             ! level of minimum moist static energy!
        endif
      enddo
      idh = 1
      idh = max(kd1, idh)
!
      tem1 = hbl - hol(kd)
      tem  = hbl - hst(kd1) - ltl(kd1) * nu *(qol(kd1)-qst(kd1))
      lowest = kd .eq. kb1

      lcon = kd
      do l=kb1,kd1,-1
        if (hbl >= hst(l)) then
          lcon = l
          exit
        endif
      enddo
!
      if (lcon == kd .or. kbl <= kd .or. prl(kbl)-prsm(lcon) > 150.0)   &
     &                                    return
!
      tx1    = rhfacs - qbl / tx1       !     average rh

      cnvflg = (tem .gt. zero .or. (lowest .and. tem1 .ge. zero))       &
     &         .and. (tx1 .lt. rhram)

!     if(lprnt) print *,' cnvflg=',cnvflg,' tem=',tem,' tem1=',tem1
!    &,' tx1=',tx1,' rhram=',rhram,' kbl=',kbl,' kd=',kd,' lowest='
!    &,lowest,' rhfacs=',rhfacs,' ltl=',ltl(kd1),' qol=',qol(kd1)
!    &,' qst=',qst(kd1),' hst=',hst(kd1),' nu=',nu
!     if(lprnt .and. (.not. cnvflg)) print *,' tx1=',tx1,' rhfacs='
!    &,rhfacs, ' tem=',tem,' hst=',hst(kd1)

      if (.not. cnvflg) return
!
      rhc    = max(zero, min(one, exp(-20.0*tx1) ))
!
      if (ntrc > 0) then
        do n=1,ntrc
          rbl(n) = roi(kmax,n) * eta(kmax)
        enddo
        do n=1,ntrc
          do l=kmaxm1,kbl,-1
            rbl(n) = rbl(n) + roi(l,n)*(eta(l)-eta(l+1))
          enddo
        enddo
      endif
!
      tx4    = 0.0
      tx5    = 0.0
!
      tx3      = qst(kbl) - gaf(kbl) * hst(kbl)
      do l=kbl,k
        qil(l) = max(zero, min(one, (tcr-tcl-tol(l))*tcrf))
      enddo
!
      do l=kb1,kd1,-1
        tem      = qst(l) - gaf(l) * hst(l)
        tem1     = (tx3 + tem) * 0.5
        st2      = (gaf(l)+gaf(l+1)) * 0.5
!
        fco(l+1) =            tem1 + st2 * hbl

!     if(lprnt) print *,' fco=',fco(l+1),' tem1=',tem1,' st2=',st2
!    &,' hbl=',hbl,' tx3=',tx3,' tem=',tem,' gaf=',gaf(l),' l=',l

        rnn(l+1) = zet(l+1) * tem1 + st2 * tx4
        gmh(l+1) = xi(l+1)  * tem1 + st2 * tx5
!
        tx3      = tem
        tx4      = tx4 + eta(l) * hol(l)
        tx5      = tx5 + gms(l) * hol(l)
!
        qil(l)   = max(zero, min(one, (tcr-tcl-tol(l))*tcrf))
        qll(l+1) = (0.5*alhf) * st2 * (qil(l)+qil(l+1)) + one
      enddo
!
!     for the cloud top -- l=kd
!
      l = kd
!
      tem      = qst(l) - gaf(l) * hst(l)
      tem1     = (tx3 + tem) * 0.5
      st2      = (gaf(l)+gaf(l+1)) * 0.5
!
      fco(l+1) =            tem1 + st2 * hbl
      rnn(l+1) = zet(l+1) * tem1 + st2 * tx4
      gmh(l+1) = xi(l+1)  * tem1 + st2 * tx5
!
      fco(l)   = tem + gaf(l) * hbl
      rnn(l)   = tem * zet(l) + (tx4 + eta(l)*hol(l)) * gaf(l)
      gmh(l)   = tem * xi(l)  + (tx5 + gms(l)*hol(l)) * gaf(l)
!
!   replace fco for the bottom
!
      fco(kbl) = qbl
      rnn(kbl) = 0.0
      gmh(kbl) = 0.0
!
      qil(kd)  =  max(zero, min(one, (tcr-tcl-tol(kd))*tcrf))
      qll(kd1) = (0.5*alhf) * st2 * (qil(kd) + qil(kd1)) + one
      qll(kd ) = alhf * gaf(kd) * qil(kd) + one
!
!     if (lprnt) then
!       print *,' fco=',fco(kd:kbl)
!       print *,' qil=',qil(kd:kbl)
!       print *,' qll=',qll(kd:kbl)
!     endif
!
      st1  = qil(kd)
      st2  = c0i * st1
      tem  = c0  * (1.0-st1)
      tem2 = st2*qi0 + tem*qw0
!
      do l=kd,kb1
         tx2    = akt(l) * eta(l)
         tx1    = tx2 * tem2
         q0u(l) = tx1
         fco(l) = fco(l+1) - fco(l) + tx1
         rnn(l) = rnn(l+1) - rnn(l)                                     &
     &          + eta(l)*(qol(l)+cll(l)+cil(l)) + tx1*zet(l)
         gmh(l) = gmh(l+1) - gmh(l)                                     &
     &          + gms(l)*(qol(l)+cll(l)+cil(l)) + tx1*xi(l)
!
         tem1   = (1.0-akt(l)) * eta(l)

!     if(lprnt) print *,' qll=',qll(l),' st2=',st2,' tem=',tem
!    &,' tx2=',tx2,' akt=',akt(l),' eta=',eta(l)

         akt(l) = qll(l)   + (st2 + tem) * tx2

!     if(lprnt) print *,' akt==',akt(l),' l==',l

         akc(l) = 1.0 / akt(l)
!
         st1    = 0.5 * (qil(l)+qil(l+1))
         st2    = c0i * st1
         tem    = c0  * (1.0-st1)
         tem2   = st2*qi0 + tem*qw0
!
         bkc(l) = qll(l+1) - (st2 + tem) * tem1
!
         tx1    = tem1*tem2
         q0d(l) = tx1
         fco(l) = fco(l) + tx1
         rnn(l) = rnn(l) + tx1*zet(l+1)
         gmh(l) = gmh(l) + tx1*xi(l+1)
      enddo

!     if(lprnt) print *,' akt=',akt(kd:kb1)
!     if(lprnt) print *,' akc=',akc(kd:kb1)

      qw00 = qw0
      qi00 = qi0
      ii = 0
  777 continue
!
!     if (lprnt) print *,' after 777 ii=',ii,' ep_wfn=',ep_wfn
!
      ep_wfn = .false.
      rnn(kbl) = 0.0
      tx3      = bkc(kb1) * (qib + qlb)
      tx4      = 0.0
      tx5      = 0.0
      do l=kb1,kd1,-1
        tem    = bkc(l-1)       * akc(l)
!     if (lprnt) print *,' tx3=',tx3,' fco=',fco(l),' akc=',akc(l)
!    &,' bkc=',bkc(l-1), ' l=',l
        tx3    = (tx3 + fco(l)) * tem
        tx4    = (tx4 + rnn(l)) * tem
        tx5    = (tx5 + gmh(l)) * tem
      enddo
      if (kd .lt. kb1) then
         hsd   = hst(kd1) + ltl(kd1) *  nu *(qol(kd1)-qst(kd1))
      else
         hsd   = hbl
      endif
!
!     if (lprnt) print *,' tx3=',tx3,' fco=',fco(kd),' akc=',akc(kd)

      tx3 = (tx3 + fco(kd)) * akc(kd)
      tx4 = (tx4 + rnn(kd)) * akc(kd)
      tx5 = (tx5 + gmh(kd)) * akc(kd)
      alm = alhf*qil(kd) - ltl(kd) * vtf(kd)
!
      hsu = hst(kd) + ltl(kd) * nu * (qol(kd)-qst(kd))

!     if (lprnt) print *,' hsu=',hsu,' hst=',hst(kd),
!    &' ltl=',ltl(kd),' qol=',qol(kd),' qst=',qst(kd)
!
!===> vertical integrals needed to compute the entrainment parameter
!
      tx1 = alm * tx4
      tx2 = alm * tx5

      do l=kd,kb1
        tau = hol(l) - hsu
        tx1 = tx1 + tau * eta(l)
        tx2 = tx2 + tau * gms(l)
      enddo
!
!     modify hsu to include cloud liquid water and ice terms
!
!     if (lprnt) print *,' hsu=',hsu,' alm=',alm,' tx3=',tx3

      hsu    = hsu - alm * tx3
!
      clp    = zero
      alm    = -100.0
      hos    = hol(kd)
      qos    = qol(kd)
      qis    = cil(kd)
      qls    = cll(kd)
      cnvflg = hbl .gt. hsu .and. abs(tx1) .gt. 1.0e-4

!     if (lprnt) print *,' ii=',ii,' cnvflg=',cnvflg,' hsu=',hsu
!    &,' hbl=',hbl,' tx1=',tx1,' hsd=',hsd


!***********************************************************************


       st1  = half*(hsu + hsd)
       if (cnvflg) then
!
!  standard case:
!   cloud can be neutrally bouyant at middle of level kd w/ +ve lambda.
!   epp < .25 is required to have real roots.
!
       clp = 1.0
       st2 = hbl - hsu

!     if(lprnt) print *,' tx2=',tx2,' tx1=',tx1,' st2=',st2
!
       if (tx2 .eq. 0.0) then
         alm = - st2 / tx1
         if (alm .gt. almax) alm = -100.0
       else
         x00 = tx2 + tx2
         epp = tx1 * tx1 - (x00+x00)*st2
         if (epp .gt. 0.0) then
           x00  = 1.0 / x00
           tem  = sqrt(epp)
           tem1 = (-tx1-tem)*x00
           tem2 = (-tx1+tem)*x00
           if (tem1 .gt. almax) tem1 = -100.0
           if (tem2 .gt. almax) tem2 = -100.0
           alm  = max(tem1,tem2)

!     if (lprnt) print *,' tem1=',tem1,' tem2=',tem2,' alm=',alm
!    &,' tx1=',tx1,' tem=',tem,' epp=',epp,' x00=',x00,' st2=',st2

         endif
       endif

!     if (lprnt) print *,' almf=',alm,' ii=',ii,' qw00=',qw00
!    &,' qi00=',qi00
!
!  clip case:
!   non-entrainig cloud detrains in lower half of top layer.
!   no clouds are allowed to detrain below the top layer.
!
       elseif ( (hbl .le. hsu) .and.                                    &
     &          (hbl .gt. st1   )     ) then
         alm = zero
!        clp = (hbl-st1) / (hsu-st1)    ! commented on jan 16, 2010
       endif
!
      cnvflg = .true.
      if (almin1 .gt. 0.0) then
        if (alm .ge. almin1) cnvflg = .false.
      else
        lowest   = kd .eq. kb1
        if ( (alm .gt. zero) .or.                                       &
     &      (.not. lowest .and. alm .eq. zero) ) cnvflg = .false.
      endif
!
!===>  if no sounding meets second condition, return
!
      if (cnvflg) then
         if (ii .gt. 0 .or. (qw00 .eq. 0.0 .and. qi00 .eq. 0.0)) return
         clp = 1.0
         ep_wfn = .true.
         go to 888
      endif
!
!     if (lprnt) print *,' hstkd=',hst(kd),' qstkd=',qst(kd)
!    &,' ii=',ii,' clp=',clp

      st1s = one
      if(clp.gt.zero .and. clp.lt.one) then
        st1     = half*(one+clp)
        st2     = one - st1
        st1s    = st1
        hstkd   = hst(kd)
        qstkd   = qst(kd)
        ltlkd   = ltl(kd)
        q0ukd   = q0u(kd)
        q0dkd   = q0d(kd)
        dlbkd   = dlb(kd)
        qrbkd   = qrb(kd)
!
        hst(kd) = hst(kd)*st1 + hst(kd1)*st2
        hos     = hol(kd)*st1 + hol(kd1)*st2
        qst(kd) = qst(kd)*st1 + qst(kd1)*st2
        qos     = qol(kd)*st1 + qol(kd1)*st2
        qls     = cll(kd)*st1 + cll(kd1)*st2
        qis     = cil(kd)*st1 + cil(kd1)*st2
        ltl(kd) = ltl(kd)*st1 + ltl(kd1)*st2
!
        dlb(kd) = dlb(kd)*clp
        qrb(kd) = qrb(kd)*clp
        eta(kd) = eta(kd)*clp
        gms(kd) = gms(kd)*clp
        q0u(kd) = q0u(kd)*clp
        q0d(kd) = q0d(kd)*clp
      endif
!
!
!***********************************************************************
!
!    critical workfunction is included in this version
!
      acr = 0.0
      tem = prl(kd1) - (prl(kd1)-prl(kd)) * clp * half
      tx1 = prl(kbl) - tem
      tx2 = min(900.0,max(tx1,100.0))
      tem1    = log(tx2*0.01) / log(10.0)
      if ( kdt == 1 ) then
        rel_fac = (dt * facdt)  / (tem1*12.0 + (1-tem1)*3.0)
      else
        rel_fac = (dt * facdt) / (tem1*adjts_d + (1-tem1)*adjts_s)
      endif
!
!     rel_fac = max(zero, min(one,rel_fac))
      rel_fac = max(zero, min(half,rel_fac))
      
      if (crtfun) then
        call crtwrk(tem, ccwf, st1)
        acr = tx1 * st1
      endif
!
!===>  normalized massflux
!
!  eta is the thickness coming in and the mass flux going out.
!  gms is the thickness of the square; it is later reused for gamma_s
!
!     eta(k) = one

      do l=kb1,kd,-1
        eta(l)  = eta(l+1) + alm * (eta(l) + alm * gms(l))
      enddo
      do l=kd,kbl
        etai(l) = 1.0 / eta(l)
      enddo

!     if (lprnt) print *,' eta=',eta,' ii=',ii,' alm=',alm
!
!===>  cloud workfunction
!
      wfn    = zero
      akm    = zero
      det    = zero
      hcc    = hbl
      cnvflg = .false.
      qtl    = qst(kb1) - gaf(kb1)*hst(kb1)
      tx1    = hbl
!
      qtv    = qbl
      det    = qlb + qib
!
      tx2    = 0.0
      dpneg  = 0.0
!
      do l=kb1,kd1,-1
         del_eta = eta(l) - eta(l+1)
         hccp = hcc + del_eta*hol(l)
!
         qtlp = qst(l-1) - gaf(l-1)*hst(l-1)
         qtvp = 0.5 * ((qtlp+qtl)*eta(l)                                &
     &              + (gaf(l)+gaf(l-1))*hccp)
         st1  = eta(l)*q0u(l) + eta(l+1)*q0d(l)
         detp = (bkc(l)*det - (qtvp-qtv)                                &
     &        + del_eta*(qol(l)+cll(l)+cil(l)) + st1)  * akc(l)

!     if(lprnt) print *,' detp=',detp,' bkc=',bkc(l),' det=',det
!     if (lprnt .and. kd .eq. 15) 
!    &          print *,' detp=',detp,' bkc=',bkc(l),' det=',det
!    &,' qtvp=',qtvp,' qtv=',qtv,' del_eta=',del_eta,' qol='
!    &,qol(l),' st1=',st1,' akc=',akc(l)
!
         tem1   = akt(l)   - qll(l)
         tem2   = qll(l+1) - bkc(l)
         rns(l) = tem1*detp  + tem2*det - st1

         qtp    = 0.5 * (qil(l)+qil(l-1))
         tem2   = min(qtp*(detp-eta(l)*qw00),                           &
     &               (1.0-qtp)*(detp-eta(l)*qi00))
         st1    = min(tx2,tem2)
         tx2    = tem2
!
         if (rns(l) .lt. zero .or. st1 .lt. zero) ep_wfn = .true.
         if (detp <= zero) cnvflg = .true.

         st1  = hst(l) - ltl(l)*nu*(qst(l)-qol(l))


         tem2 = hccp   + detp   * qtp * alhf
!
!     if(lprnt) print *,' hst=',hst(l),' ltl=',ltl(l),' nu=',nu
!     if (lprnt .and. kd .eq. 15) 
!    &          print *,' hst=',hst(l),' ltl=',ltl(l),' nu=',nu
!    &,' qst=',qst(l),' qol=',qol(l),' hccp=',hccp,' detp=',detp
!    *,' qtp=',qtp,' alhf=',alhf,' vtf=',vtf(l)

         st2  = ltl(l) * vtf(l)
         tem5 = cll(l) + cil(l)
         tem3 = (tx1  - eta(l+1)*st1 - st2*(det-tem5*eta(l+1))) * dlb(l)
         tem4 = (tem2 - eta(l  )*st1 - st2*(detp-tem5*eta(l)))  * dlt(l)
!
!     if (lprnt) then
!     if (lprnt .and. kd .eq. 12) then 
!       print *,' tem3=',tem3,' tx1=',tx1,' st1=',st1,' eta1=',eta(l+1)
!    &, ' st2=',st2,' det=',det,' tem5=',tem5,' dlb=',dlb(l)
!       print *,' tem4=',tem4,' tem2=',tem2,' detp=',detp
!    &, ' eta=',eta(l),' dlt=',dlt(l),' rns=',rns(l),' l=',l
!       print *,' bt1=',tem3/(eta(l+1)*qrb(l))
!    &,         ' bt2=',tem4/(eta(l)*qrt(l))
!      endif

         st1  = tem3 + tem4

!     if (lprnt) print *,' wfn=',wfn,' st1=',st1,' l=',l,' ep_wfn=',
!    &ep_wfn,' akm=',akm

         wfn = wfn + st1       
         akm = akm - min(st1,zero)

!     if (lprnt) print *,' wfn=',wfn,' akm=',akm

         if (st1 .lt. zero .and. wfn .lt. zero) then
           dpneg = dpneg + prl(l+1) - prl(l)
         endif

         buy(l) = 0.5 * (tem3/(eta(l+1)*qrb(l)) + tem4/(eta(l)*qrt(l)))
!
         hcc = hccp
         det = detp
         qtl = qtlp
         qtv = qtvp
         tx1 = tem2

      enddo

      del_eta = eta(kd) - eta(kd1)
      hccp    = hcc + del_eta*hos
!
      qtlp    = qst(kd) - gaf(kd)*hst(kd)
      qtvp    = qtlp*eta(kd) + gaf(kd)*hccp
      st1     = eta(kd)*q0u(kd) + eta(kd1)*q0d(kd)
      detp    = (bkc(kd)*det - (qtvp-qtv)                               &
     &        + del_eta*(qos+qls+qis) + st1) * akc(kd)
!
      tem1    = akt(kd)  - qll(kd)
      tem2    = qll(kd1) - bkc(kd)
      rns(kd) = tem1*detp  + tem2*det - st1
!
      if (rns(kd) .lt. zero) ep_wfn = .true.
      if (detp <= zero) cnvflg = .true.
!
  888 continue

!     if (lprnt) print *,' ep_wfn=',ep_wfn,' ii=',ii,' rns=',rns(kd)
!    &,' clp=',clp,' hst(kd)=',hst(kd)

      if (ep_wfn) then
        if ((qw00 .eq. 0.0 .and. qi00 .eq. 0.0)) return
        if (ii .eq. 0) then
          ii  = 1
          if (clp .gt. 0.0 .and. clp .lt. 1.0) then
            hst(kd) = hstkd
            qst(kd) = qstkd
            ltl(kd) = ltlkd
            q0u(kd) = q0ukd
            q0d(kd) = q0dkd
            dlb(kd) = dlbkd
            qrb(kd) = qrbkd
          endif
          do l=kd,kb1
            fco(l) = fco(l) - q0u(l) - q0d(l)
            rnn(l) = rnn(l) - q0u(l)*zet(l) - q0d(l)*zet(l+1)
            gmh(l) = gmh(l) - q0u(l)*xi(l)  - q0d(l)*zet(l+1)
            eta(l) = zet(l) - zet(l+1)
            gms(l) = xi(l)  - xi(l+1)
            q0u(l) = 0.0
            q0d(l) = 0.0
          enddo
          qw00 = 0.0
          qi00 = 0.0

!     if (lprnt) print *,' returning to 777 : ii=',ii,' qw00=',qw00,qi00
!    &,' clp=',clp,' hst(kd)=',hst(kd)

          go to 777
        else
          cnvflg = .true.
        endif
      endif
!
!
!     st1 = 0.5 * (hst(kd)  - ltl(kd)*nu*(qst(kd)-qos)
!    &          +  hst(kd1) - ltl(kd1)*nu*(qst(kd1)-qol(kd1)))
!
      st1 = hst(kd)  - ltl(kd)*nu*(qst(kd)-qos)
      st2 = ltl(kd)  * vtf(kd)
      tem5 = (qls + qis) * eta(kd1)
      st1  = half * (tx1-eta(kd1)*st1-st2*(det-tem5))*dlb(kd)
!
!     if (lprnt) print *,' st1=',st1,' st2=',st2,' ltl=',ltl(kd)
!    *,ltl(kd1),' qos=',qos,qol(kd1)

      wfn = wfn + st1
      akm = akm - min(st1,zero)   ! commented on 08/26/02 - does not include top
!

      buy(kd) = st1 / (eta(kd1)*qrb(kd))
!
!     if (lprnt) print *,' wfn=',wfn,' akm=',akm,' st1=',st1
!    &,' dpneg=',dpneg

      det = detp
      hcc = hccp
      akm = akm / wfn


!***********************************************************************
!
!     if only to calculate workfunction save it and return
!
      if (wrkfun) then
        if (wfn .ge. 0.0) wfnc = wfn
        return
      elseif (.not. crtfun) then
        acr = wfnc
      endif
!
!===>  third check based on cloud workfunction
!
      calcup = .false.

      tem  =  max(0.05, min(cd*200.0, max_neg_bouy))
      if (wfn .gt. acr .and.  (.not. cnvflg)                             &
!    & .and. dpneg .lt. 100.0  .and. akm .le. tem) then
     & .and. dpneg .lt. 150.0  .and. akm .le. tem) then
!    & .and. dpneg .lt. 200.0  .and. akm .le. tem) then
!
        calcup = .true.
      endif

!     if (lprnt) print *,' calcup=',calcup,' akm=',akm,' tem=',tem
!    *,' cnvflg=',cnvflg,' clp=',clp,' rhc=',rhc,' cd=',cd,' acr=',acr
!
!===>  if no sounding meets third condition, return
!
      if (.not. calcup) return
!
! this is for not ll - 20050601
      if (almin2 .ne. 0.0) then
        if (almin1 .ne. almin2) st1 = 1.0 / max(one_m10,(almin2-almin1))
        if (alm .lt. almin2) then
           clp = clp * max(0.0, min(1.0,(0.3 + 0.7*(alm-almin1)*st1)))
!          clp = clp * max(0.0, min(1.0,(0.2 + 0.8*(alm-almin1)*st1)))
!          clp = clp * max(0.0, min(1.0,(0.1 + 0.9*(alm-almin1)*st1)))
        endif
      endif
!
!     if (lprnt) print *,' clp=',clp
!
      clp = clp * rhc
      dlq = 0.0
      tem = 1.0 / (1.0 + dlq_fac)
      do l=kd,kb1
        rnn(l) = rns(l) * tem
        dlq(l) = rns(l) * tem * dlq_fac
      enddo
      do l=kbl,k 
        rnn(l) = 0.0 
      enddo
!     if (lprnt) print *,' rnn=',rnn
!
!     if downdraft is to be invoked, do preliminary check to see
!     if enough rain is available and then call ddrft.
!
      ddft = .false.
      if (dndrft) then
!
        train = 0.0
        if (clp .gt. 0.0) then
          do l=kd,kb1
            train = train + rnn(l)
          enddo
        endif

        pl = (prl(kd1) + prl(kd))*half
        tem = prl(k+1)*(1.0-dpd*0.001)
        if (train .gt. 1.0e-4 .and. pl .le. tem) ddft  = .true.
!
      endif
!
!     if (lprnt) then
!       print *,' before calling ddrft kd=',kd,' ddft=',ddft
!    &,                  ' pl=',pl,' train=',train
!       print *,' buy=',(buy(l),l=kd,kb1)
!     endif

      if (ddft) then ! downdraft scheme based on (cheng and arakawa, 1997)
        call ddrft(                                                     &
     &              k, kd                                               &
     &,             tla, alfind                                         &
     &,             tol, qol, hol,   prl, qst, hst, gam, gaf            &
!    &,             tol, qol, hol,   prl, qst, hst, gam, gaf, hbl, qbl  &
     &,             qrb, qrt, buy,   kbl, idh, eta, rnn, etai           &
     &,             alm, wfn, train, ddft                               &
     &,             etd, hod, qod,   evp, dof, cldfr, etz               &
     &,             gms, gsd, ghd,   lprnt)               

      endif
!
!  no downdraft case (including case with no downdraft solution)
!  ---------------------------------------------------------
!
      if (.not. ddft) then
        do l=kd,k+1
          etd(l) = 0.0
          hod(l) = 0.0
          qod(l) = 0.0
        enddo
        do l=kd,k
          evp(l) = 0.0
          etz(l) = 0.0
        enddo

      endif

!     if (lprnt) print *,' hod=',hod
!     if (lprnt) print *,' etd=',etd
!
!
!===> calculate gammas  i.e. tendencies per unit cloud base massflux
!           includes downdraft terms!

      avh = 0.0

!
!     fraction of detrained condensate evaporated
!
!     tem1 = max(zero, min(half, (prl(kd)-four_p2)*one_m2))
!     tem1 = max(zero, min(half, (prl(kd)-300.0)*0.005))
      tem1 = 0.0
!     tem1 = 1.0
!     if (kd1 .eq. kbl) tem1 = 0.0
!
      tem2    = 1.0 - tem1
      tem = det * qil(kd)


      st1 = (hcc+alhf*tem-eta(kd)*hst(kd)) / (1.0+gam(kd))
      ds  = eta(kd1) * (hos- hol(kd)) - alhl*(qos - qol(kd))
      dh  = eta(kd1) * (hos- hol(kd))


      gms(kd) = (ds + st1 - tem1*det*alhl-tem*alhf) * pri(kd)
      gmh(kd) = pri(kd) * (hcc-eta(kd)*hos + dh)


!     if (lprnt) print *,' gmhkd=',gmh(kd),' gmskd=',gms(kd)
!    &,' det=',det,' tem=',tem,' tem1=',tem1,' tem2=',tem2
!
!      tendency for suspended environmental ice and/or liquid water
!
      qll(kd) = (tem2*(det-tem) + eta(kd1)*(qls-cll(kd))                &
     &        + (1.0-qil(kd))*dlq(kd) - eta(kd)*qls ) * pri(kd)

      qil(kd) =     (tem2*tem + eta(kd1)*(qis-cil(kd))                  &
     &        + qil(kd)*dlq(kd) - eta(kd)*qis ) * pri(kd)
!
      ghd(kd) = 0.0
      gsd(kd) = 0.0
!
      do l=kd1,k
         st1 = one - alfint(l,1)
         st2 = one - alfint(l,2)
         st3 = one - alfint(l,3)
         st4 = one - alfint(l,4)
         st5 = one - alfind(l)
         hb       = alfint(l,1)*hol(l-1) + st1*hol(l)
         qb       = alfint(l,2)*qol(l-1) + st2*qol(l)

         tem      = alfint(l,4)*cil(l-1) + st4*cil(l)
         tem2     = alfint(l,3)*cll(l-1) + st3*cll(l)
 
         tem1     = eta(l) * (tem - cil(l))
         tem3     = eta(l) * (tem2 - cll(l))

         hbd      = alfind(l)*hol(l-1) + st5*hol(l)
         qbd      = alfind(l)*qol(l-1) + st5*qol(l)

         tem5     = etd(l) * (hod(l) - hbd)
         tem6     = etd(l) * (qod(l) - qbd)
!
         dh       = eta(l) * (hb - hol(l)) + tem5
         ds       = dh - alhl * (eta(l) * (qb - qol(l)) + tem6)

         gmh(l)   = dh * pri(l)
         gms(l)   = ds * pri(l)

!     if (lprnt) print *,' gmh=',gmh(l),' gms=',gms(l)
!    &,' dh=',dh,' ds=',ds,' qb=',qb,' qol=',qol(l),' eta=',eta(l)
!    &,' hb=',hb,' hol=',hol(l),' l=',l,' hod=',hod(l)
!    &,' etd=',etd(l),' qod=',qod(l),' tem5=',tem5,' tem6=',tem6
!
         ghd(l)   = tem5 * pri(l)
         gsd(l)   = (tem5 - alhl * tem6) * pri(l)
!
         qll(l)   = (tem3 + (1.0-qil(l))*dlq(l)) * pri(l)
         qil(l)   = (tem1 + qil(l)*dlq(l)) * pri(l)

         tem1     = eta(l) * (cil(l-1) - tem)
         tem3     = eta(l) * (cll(l-1) - tem2)

         dh       = eta(l) * (hol(l-1) - hb) - tem5
         ds       = dh - alhl * eta(l) * (qol(l-1) - qb)                &
     &                 + alhl * (tem6 - evp(l-1))

         gmh(l-1) = gmh(l-1) + dh * pri(l-1)
         gms(l-1) = gms(l-1) + ds * pri(l-1)
!
!     if (lprnt) print *,' gmh1=',gmh(l-1),' gms1=',gms(l-1)
!    &,' dh=',dh,' ds=',ds,' qb=',qb,' qol=',qol(l-1)
!    &,' hb=',hb,' hol=',hol(l-1),' evp=',evp(l-1)
!
         ghd(l-1) = ghd(l-1) - tem5 * pri(l-1)
         gsd(l-1) = gsd(l-1) - (tem5-alhl*(tem6-evp(l-1))) * pri(l-1)

         qil(l-1) = qil(l-1) + tem1 * pri(l-1)
         qll(l-1) = qll(l-1) + tem3 * pri(l-1)


!     if (lprnt) print *,' gmh=',gmh(l),' gms=',gms(l)
!    &,' dh=',dh,' ds=',ds,' qb=',qb,' qol=',qol(l),' eta=',eta(l)
!    &,' hb=',hb,' hol=',hol(l),' l=',l
!
        avh = avh + gmh(l-1)*(prs(l)-prs(l-1))

      enddo
!
      hbd    = hol(k)
      qbd    = qol(k)
      tem5   =  etd(k+1) * (hod(k+1) - hbd)
      tem6   =  etd(k+1) * (qod(k+1) - qbd)
      dh     = - tem5
      ds     = dh  + alhl * tem6
      tem1   = dh * pri(k)
      tem2   = (ds - alhl * evp(k)) * pri(k)
      gmh(k) = gmh(k) + tem1
      gms(k) = gms(k) + tem2
      ghd(k) = ghd(k) + tem1
      gsd(k) = gsd(k) + tem2

!     if (lprnt) print *,' gmhk=',gmh(k),' gmsk=',gms(k)
!    &,' tem1=',tem1,' tem2=',tem2,' dh=',dh,' ds=',ds
!
      avh    = avh + gmh(k)*(prs(kp1)-prs(k))
!
      tem4   = - gravfac * pris
      tx1    = dh * tem4
      tx2    = ds * tem4
!
      do l=kbl,k
        gmh(l) = gmh(l) + tx1
        gms(l) = gms(l) + tx2
        ghd(l) = ghd(l) + tx1
        gsd(l) = gsd(l) + tx2
!
        avh = avh + tx1*(prs(l+1)-prs(l))
      enddo

!
!     if (lprnt) then
!        print *,' gmh=',gmh
!        print *,' gms=',gms(kd:k)
!     endif
!
!***********************************************************************
!***********************************************************************

!===>  kernel (akm) calculation begins

!===>  modify sounding with unit mass flux
!
      do l=kd,k

         tem1   = gmh(l)
         tem2   = gms(l)
         hol(l) = hol(l) +  tem1*testmb
         qol(l) = qol(l) + (tem1-tem2)  * (testmb/alhl)
         hst(l) = hst(l) +  tem2*(one+gam(l))*testmb
         qst(l) = qst(l) +  tem2*gam(l)*(testmb/alhl)
         cll(l) = cll(l) + qll(l) * testmb
         cil(l) = cil(l) + qil(l) * testmb
      enddo
!
      if (alm .gt. 0.0) then
        hos     = hos + gmh(kd)  * testmb
        qos     = qos + (gmh(kd)-gms(kd)) * (testmb/alhl)
        qls     = qls + qll(kd) * testmb
        qis     = qis + qil(kd) * testmb
      else
        st2 = 1.0 - st1s
        hos     = hos + (st1s*gmh(kd)+st2*gmh(kd1))  * testmb
        qos     = qos + (st1s * (gmh(kd)-gms(kd))                       &
     &                +  st2  * (gmh(kd1)-gms(kd1))) * (testmb/alhl)
        hst(kd) = hst(kd) + (st1s*gms(kd)*(one+gam(kd))                 &
     &                    +  st2*gms(kd1)*(one+gam(kd1))) * testmb
        qst(kd) = qst(kd) + (st1s*gms(kd)*gam(kd)                       &
     &                    +  st2*gms(kd1)*gam(kd1)) * (testmb/alhl)

        qls     = qls + (st1s*qll(kd)+st2*qll(kd1)) * testmb
        qis     = qis + (st1s*qil(kd)+st2*qil(kd1)) * testmb
      endif

!
      tem = prl(kmaxp1) - prl(kmax)
      hbl = hol(kmax) * tem
      qbl = qol(kmax) * tem
      qlb = cll(kmax) * tem
      qib = cil(kmax) * tem
      do l=kmaxm1,kbl,-1
        tem = prl(l+1) - prl(l)
        hbl = hbl + hol(l) * tem
        qbl = qbl + qol(l) * tem
        qlb = qlb + cll(l) * tem
        qib = qib + cil(l) * tem
      enddo
      hbl = hbl * prism
      qbl = qbl * prism
      qlb = qlb * prism
      qib = qib * prism

!     if (ctei .and. sgcs(kd) > 0.65) then
!        hbl = hbl * hpert_fac
!        qbl = qbl * hpert_fac
!     endif

!     if (lprnt) print *,' hbla=',hbl,' qbla=',qbl

!***********************************************************************

!===>  cloud workfunction for modified sounding, then kernel (akm)
!
      akm = zero
      tx1 = zero
      qtl = qst(kb1) - gaf(kb1)*hst(kb1)
      qtv = qbl
      hcc = hbl
      tx2 = hcc
      tx4 = (alhf*0.5)*max(zero,min(one,(tcr-tcl-tol(kb1))*tcrf))
!
      qtv = qbl
      tx1 = qib + qlb
!

      do l=kb1,kd1,-1
         del_eta = eta(l) - eta(l+1)
         hccp = hcc + del_eta*hol(l)
!
         qtlp = qst(l-1) - gaf(l-1)*hst(l-1)
         qtvp = 0.5 * ((qtlp+qtl)*eta(l) + (gaf(l)+gaf(l-1))*hccp)

         detp = (bkc(l)*tx1 - (qtvp-qtv)                                &
     &        +  del_eta*(qol(l)+cll(l)+cil(l))                         &
     &        +  eta(l)*q0u(l) + eta(l+1)*q0d(l)) * akc(l)
         if (detp .le. zero) cnvflg = .true.

         st1  = hst(l) - ltl(l)*nu*(qst(l)-qol(l))

         tem2 = (alhf*0.5)*max(zero,min(one,(tcr-tcl-tol(l-1))*tcrf))
         tem1 = hccp + detp * (tem2+tx4)

         st2  = ltl(l) * vtf(l)
         tem5 = cll(l) + cil(l)
         akm  = akm +                                                   &
     &     (  (tx2  -eta(l+1)*st1-st2*(tx1-tem5*eta(l+1))) * dlb(l)     &
     &      + (tem1 -eta(l  )*st1-st2*(detp-tem5*eta(l)))  * dlt(l) )
!
         hcc  = hccp
         tx1  = detp
         tx2  = tem1
         qtl  = qtlp
         qtv  = qtvp
         tx4  = tem2
      enddo
!
      if (cnvflg) return
!
!  eventhough we ignore the change in lambda, we still assume
!  that the cloud-top contribution is zero; as though we still
!  had non-bouyancy there.
!
!
      st1 = hst(kd)  - ltl(kd)*nu*(qst(kd)-qos)
      st2 = ltl(kd)  * vtf(kd)
      tem5 = (qls + qis) * eta(kd1)
      akm  = akm + half * (tx2-eta(kd1)*st1-st2*(tx1-tem5)) * dlb(kd)
!
      akm = (akm - wfn) * (one/testmb)


!***********************************************************************

!===>   mass flux

      tem2 = rel_fac
!
      amb = - (wfn-acr) / akm
!
!     if(lprnt) print *,' wfn=',wfn,' acr=',acr,' akm=',akm             &
!    &,' amb=',amb,' kd=',kd,' cldfrd=',cldfrd,' tem2=',tem2            &
!    &,' rel_fac=',rel_fac,' prskd=',prs(kd)

!===>   relaxation and clipping factors
!
      amb = amb * clp * tem2

!!!   if (ddft) amb = min(amb, one/cldfrd)
       
!===>   sub-cloud layer depth limit on mass flux

      ambmax = (prl(kmaxp1)-prl(kbl))*(fracbl*gravcon)
      amb    = max(min(amb, ambmax),zero)


!     if(lprnt) print *,' amb=',amb,' clp=',clp,' ambmax=',ambmax
!***********************************************************************
!*************************results***************************************
!***********************************************************************

!===>  precipitation and clw detrainment
!
      avt = 0.0
      avq = 0.0
      avr = dof

!
      dsfc = dsfc + amb * etd(k) * (1.0/dt)
!
!     do l=kbl,kd,-1
      do l=k,kd,-1
          pcu(l) = pcu(l) + amb*rnn(l)      !  (a40)
          avr    = avr + rnn(l)
!     if(lprnt) print *,' avr=',avr,' rnn=',rnn(l),' l=',l
      enddo
!
!===> temparature and q change and cloud mass flux due to cloud type kd
!
      tx1  = amb * (one/cp)
      tx2  = amb * (one/alhl)
      do l=kd,k
        st1    = gms(l)*tx1
        toi(l) = toi(l) + st1
        tcu(l) = tcu(l) + st1
        tcd(l) = tcd(l) + gsd(l) * tx1
!
        st1 = st1 - (alhl/cp) * (qil(l) + qll(l)) * amb

        avt = avt + st1 * (prs(l+1)-prs(l))

        flx(l)  = flx(l)  + eta(l)*amb
        flxd(l) = flxd(l) + etd(l)*amb
!
        qii(l)  = qii(l) + qil(l) * amb
        tem     = 0.0

        qli(l)  = qli(l) + qll(l) * amb + tem

        st1     = (gmh(l)-gms(l)) * tx2

        qoi(l)  = qoi(l) + st1
        qcu(l)  = qcu(l) + st1
        qcd(l)  = qcd(l) + (ghd(l)-gsd(l)) * tx2
!
        avq = avq + (st1+(qll(l)+qil(l))*amb) * (prs(l+1)-prs(l))
!       avq = avq + st1 * (prs(l+1)-prs(l))
!       avr = avr + (qll(l) + qil(l)*(1+alhf/alhl))
!       avr = avr + (qll(l) + qil(l))
!    *                  * (prs(l+1)-prs(l)) * gravcon

!     if(lprnt) print *,' avr=',avr,' qll=',qll(l),' l=',l
!    &,' qil=',qil(l)

      enddo
      avr = avr * amb
!
!      correction for negative condensate!
!     if (advcld) then
!       do l=kd,k
!         if (qli(l) .lt. 0.0) then
!           qoi(l) = qoi(l) + qli(l)
!           toi(l) = toi(l) - (alhl/cp) * qli(l)
!           qli(l) = 0.0
!         endif
!         if (qii(l) .lt. 0.0) then
!           qoi(l) = qoi(l) + qii(l)
!           toi(l) = toi(l) - ((alhl+alhf)/cp) * qii(l)
!           qii(l) = 0.0
!         endif
!       enddo
!     endif

!
!
!     if (lprnt) then
!       print *,' for kd=',kd
!       avt = avt * cp * 100.0*86400.0 / (alhl*dt*grav)
!       avq = avq *  100.0*86400.0 / (dt*grav)
!       avr = avr * 86400.0 / dt
!       print *,' avt=',avt,' avq=',avq,' avr=',avr,' avh='
!    *   ,avh,' alm=',alm,' ddft=',ddft,' kd=',kd
!    &,' toik-',toi(k),' toik-1=',toi(k-1),' toik-2=',toi(k-2)
!        if (kd .eq. 12 .and. .not. ddft) stop
!       if (avh .gt. 0.1 .or. abs(avt+avq) .gt. 1.0e-5 .or.
!    &      abs(avt-avr) .gt. 1.0e-5 .or. abs(avr+avq) .gt. 1.0e-5) stop
!
!     if (lprnt) then
!       print *,' for kd=',kd
!       print *,' tcu=',(tcu(l),l=kd,k)
!       print *,' qcu=',(qcu(l),l=kd,k)
!     endif
!
      tx1 = 0.0
      tx2 = 0.0
!
      if (revap) then !     reevaporation of falling convective rain
!
       tem = 0.0
       do l=kd,kbl
         if (l .lt. idh .or. (.not. ddft)) then
           tem = tem + amb * rnn(l)
         endif
       enddo
       tem = tem + amb * dof
       tem = tem * (3600.0/dt)
!!!!   tem1 = max(1.0, min(100.0,sqrt((5.0e10/max(garea,one)))))
!      tem1 = max(1.0, min(100.0,(7.5e10/max(garea,one))))
!      tem1 = max(1.0, min(100.0,(5.0e10/max(garea,one))))
!      tem1 = max(1.0, min(100.0,(4.0e10/max(garea,one))))
!!     tem1 = sqrt(max(1.0, min(100.0,(4.0e10/max(garea,one))))) ! 20100902
       tem1 = sqrt(max(1.0, min(100.0,(6.25e10/max(garea,one))))) ! 20110530

!      if (lprnt) print *,' clfr0=',clf(tem),' tem=',tem,' tem1=',tem1

!      clfrac = max(zero, min(one, rknob*clf(tem)*tem1))
!      clfrac = max(zero, min(0.25, rknob*clf(tem)*tem1))
!      clfrac = max(zero, min(half, rknob*clf(tem)*tem1))
       clfrac = max(zero, min(quart, rknob*clf(tem)*tem1))

!      if (lprnt) then
!        print *,' cldfrd=',cldfrd,' amb=',amb,' clfrac=',clfrac
!        print *,' tx3=',tx3,' etakd=',eta(kd),' pri=',pri(kd)
!        print *,' rnn=',rnn(kd:k)
!      endif
!
!cnt   do l=kd,k
       do l=kd,kbl         ! testing on 20070926
!                                                 for l=kd,k
         if (l .ge. idh .and. ddft) then
           tx2    = tx2 + amb * rnn(l)
           cldfrd = min(amb*cldfr(l), clfrac)
         else
           tx1 = tx1 + amb * rnn(l)
         endif
         tx4 = zfac * phil(l)
         tx4 = (one - tx4 * (one - half*tx4)) * afc
!
         if (tx1 .gt. 0. .or. tx2 .gt. 0.0) then
          teq     = toi(l)
          qeq     = qoi(l)
          pl      = 0.5 * (prl(l+1)+prl(l))

          st1     = max(zero, min(one, (tcr-teq)*tcrf))
          st2     = st1*elfocp + (1.0-st1)*elocp

          call qsatcn ( teq,pl,qsteq,dqdt)
!         call qsatcn ( teq,pl,qsteq,dqdt,.false.)
!
          deltaq = 0.5 * (qsteq*rhc_ls(l)-qeq) / (1.+st2*dqdt)
!
          qeq    = qeq + deltaq
          teq    = teq - deltaq*st2
!
          tem1   = max(zero, min(one, (tcr-teq)*tcrf))
          tem2   = tem1*elfocp + (1.0-tem1)*elocp

          call qsatcn ( teq,pl,qsteq,dqdt)
!         call qsatcn ( teq,pl,qsteq,dqdt,.false.)
!
          deltaq = (qsteq*rhc_ls(l)-qeq) / (1.+tem2*dqdt)
!
          qeq    = qeq + deltaq
          teq    = teq - deltaq*tem2

          if (qeq .gt. qoi(l)) then
            potevap = (qeq-qoi(l))*(prl(l+1)-prl(l))*gravcon

            tem4    = 0.0
            if (tx1 .gt. 0.0)                                           &
     &      tem4    = potevap * (1. - exp( tx4*tx1**0.57777778 ) )
!    &      tem4    = potevap * (1. - exp( afc*tx4*sqrt(tx1) ) )
            actevap = min(tx1, tem4*clfrac)

!     if(lprnt) print *,' l=',l,' actevap=',actevap,' tem4=',tem4,
!    &' clfrac='
!    &,clfrac,' potevap=',potevap,'efac=',afc*sqrt(tx1*tem3)
!    &,' tx1=',tx1

            if (tx1 .lt. rainmin*dt) actevap = min(tx1, potevap)
!
            tem4    = 0.0
            if (tx2 .gt. 0.0)                                           &
     &      tem4    = potevap * (1. - exp( tx4*tx2**0.57777778 ) )
!    &      tem4    = potevap * (1. - exp( afc*tx4*sqrt(tx2) ) )
            tem4    = min(min(tx2, tem4*cldfrd), potevap-actevap)
            if (tx2 .lt. rainmin*dt) tem4 = min(tx2, potevap-actevap)
!
            tx1     = tx1 - actevap
            tx2     = tx2 - tem4
            st1     = (actevap+tem4) * pri(l)
            qoi(l)  = qoi(l) + st1
            qcu(l)  = qcu(l) + st1
!

            st1     = st1 * elocp
            toi(l)  = toi(l) - st1 
            tcu(l)  = tcu(l) - st1
          endif
         endif
       enddo
!
       cup = cup + tx1 + tx2 + dof * amb
      else
       do l=kd,k
         tx1 = tx1 + amb * rnn(l)
       enddo
       cup = cup + tx1 + dof * amb
      endif

!     if (lprnt) print *,' tx1=',tx1,' tx2=',tx2,' dof=',dof
!    &,' cup=',cup*86400/dt,' amb=',amb
!    &,' amb=',amb,' cup=',cup,' clfrac=',clfrac,' cldfrd=',cldfrd
!    &,' ddft=',ddft,' kd=',kd,' kbl=',kbl,' k=',k
!
!    convective transport (mixing) of passive tracers
!
      if (ntrc > 0) then
        do l=kd,k-1
          if (etz(l) .ne. zero) etzi(l) = one / etz(l)
        enddo
        do n=1,ntrc        ! tracer loop ; first two are u and v

          do l=kd,k
            hol(l) = roi(l,n)
          enddo
!
          hcc     = rbl(n)
          hod(kd) = hol(kd)
!      compute downdraft properties for the tracer
          do l=kd1,k
            st1 = one - alfind(l)
            hb  = alfind(l)  * hol(l-1) + st1 * hol(l)
            if (etz(l-1) .ne. zero) then
              tem = etzi(l-1)
              if (etd(l)  > etd(l-1)) then
                hod(l) = (etd(l-1)*(hod(l-1)-hol(l-1))                  &
     &                 +  etd(l)  *(hol(l-1)-hb) +  etz(l-1)*hb) * tem
              else
                hod(l) = (etd(l-1)*(hod(l-1)-hb) + etz(l-1)*hb) * tem
              endif
            else
              hod(l) = hb
            endif
          enddo
             
          do l=kb1,kd,-1
            hcc = hcc + (eta(l)-eta(l+1))*hol(l)
          enddo
!
!         scavenging -- fscav   - fraction scavenged [km-1]
!                       delz    - distance from the entrainment to detrainment layer [km]
!                       fnoscav - the fraction not scavenged
!                                 following liu et al. [jgr,2001] eq 1

          if (fscav_(n) > 0.0) then
            delzkm = ( phil(kd) - phih(kd1) ) *(onebg*0.001)
            fnoscav = exp(- fscav_(n) * delzkm)
          else
            fnoscav = 1.0
          endif

          gmh(kd) = pri(kd) * (hcc-eta(kd)*hol(kd)) * trcfac(kd,n)      &
     &                                              * fnoscav
          do l=kd1,k
            if (fscav_(n) > 0.0) then
              delzkm = ( phil(kd) - phih(l+1) ) *(onebg*0.001)
              fnoscav = exp(- fscav_(n) * delzkm)
            endif
            st1      = one - alfint(l,n+4)
            st2      = one - alfind(l)
            hb       = alfint(l,n+4) * hol(l-1) + st1 * hol(l)
            hbd      = alfind(l) * hol(l-1) + st2 * hol(l)
            tem5     = etd(l)    * (hod(l) - hbd)
            dh       = eta(l)    * (hb - hol(l))   * fnoscav + tem5
            gmh(l  ) = dh * pri(l) * trcfac(l,n)
            dh       = eta(l)    * (hol(l-1) - hb) * fnoscav - tem5
            gmh(l-1) = gmh(l-1)  + dh * pri(l-1) * trcfac(l,n)
          enddo
!
          do l=kd,k
            st1      = gmh(l)*amb
            roi(l,n) = hol(l)   + st1
            rcu(l,n) = rcu(l,n) + st1
          enddo
        enddo                             ! tracer loop ntrc
      endif

!     if (lprnt) print *,' toio=',toi
!     if (lprnt) print *,' qoio=',qoi

      return
      end

      subroutine ddrft(                                                 &
     &                  k, kd                                           &
     &,                 tla, alfind                                     &
     &,                 tol, qol, hol, prl, qst, hst, gam, gaf          &
!    &,                 tol, qol, hol, prl, qst, hst, gam, gaf, hbl, qbl&
     &,                 qrb, qrt, buy, kbl, idh, eta, rnn, etai         &
     &,                 alm, wfn, train, ddft                           &
     &,                 etd, hod, qod, evp, dof, cldfrd, wcb            &
     &,                 gms, gsd, ghd,lprnt)                   

!
!***********************************************************************
!******************** cumulus downdraft subroutine *********************
!****************** based on cheng and arakawa (1997)  ****** **********
!************************ subroutine ddrft  ****************************
!*************************  october 2004  ******************************
!***********************************************************************
!***********************************************************************
!************* shrinivas.moorthi@noaa.gov (301) 763 8000(x7233) ********
!***********************************************************************
!***********************************************************************
!23456789012345678901234567890123456789012345678901234567890123456789012
!
!===>  tol(k)     input   temperature            kelvin
!===>  qol(k)     input   specific humidity      non-dimensional

!===>  prl(k+1)   input   pressure @ edges       mb

!===>  k     input   the rise & the index of the subcloud layer
!===>  kd    input   detrainment level ( 1<= kd < k )          
!     
      use machine , only : kind_phys
      use module_ras
      implicit none
!
!  input arguments
!
      integer k, kd
      real(kind=kind_phys) alfind(k)
      integer kbl, kb1

      logical skpdd, skpup

      real(kind=kind_phys) hol(kd:k),   qol(kd:k),   gaf(kd:k+1)        &
     &,                    hst(kd:k),   qst(kd:k),   tol(kd:k)          &
     &,                    buy(kd:k+1), qrb(kd:k),   qrt(kd:k)          &
     &,                    gam(kd:k+1), rnn(kd:k),   rns(kd:k)                       &
     &,                    eta(kd:k+1), prl(kd:k+1), etai(kd:k)
!
!     real(kind=kind_phys)    hbl,     qbl,        pris                 &
!    &,                       train,   wfn,        alm
      real(kind=kind_phys)    train,   wfn,        alm
!
!     temporary work space
!
      real(kind=kind_phys) gms(kd:k+1), tx7
      real(kind=kind_phys) tx1,    tx2,  tx3, tx4                       &
     &,                    tx5,    tx6,  tx8, tx9
      logical cnvflg

      real(kind=kind_phys) st1                                          &
     &,                    qqq, picon, piinv, del_eta                   &
     &,                    tem, tem1, tem2, tem3, tem4, st2             &
     &,                    errmin, errmi2, errh, errw, erre             &
     &,                    tem6
      integer i, l,  n, kd1, ii                                         &
     &,       kp1, km1, ktem, kk, kk1, lm1, ll, lp1                     &
     &,       ntla

!
      integer, parameter :: numtla=2
!     integer, parameter :: numtla=4
      parameter (errmin=0.0001, errmi2=0.1*errmin)
!     parameter (errmin=0.00001, errmi2=0.1*errmin)
!
      real(kind=kind_phys) tla,    stla,  ctl2, ctl3
      real(kind=kind_phys) gmf,    pi,    onpg, vtpexp                  &
     &,    rpart,  qrmin,  aa1,    bb1,   cc1,  dd1                     &
     &,    wc2min, wcmin,  wcbase, f2,    f3,   f5, gmf1, gmf5          &
     &,    qraf,   qrbf,   del_tla               
!    &,    sialf
!
      parameter (onpg=1.0+0.5, gmf=1.0/onpg, rpart=0.0)
!     parameter (onpg=1.0+0.5, gmf=1.0/onpg, rpart=1.0)
!     parameter (onpg=1.0+0.5, gmf=1.0/onpg, rpart=0.5)
!     parameter (aa1=1.0, bb1=1.5, cc1=1.1, dd1=0.85, f3=cc1, f5=2.5)
!     parameter (aa1=2.0, bb1=1.5, cc1=1.1, dd1=0.85, f3=cc1, f5=2.5)
      parameter (aa1=1.0, bb1=1.0, cc1=1.0, dd1=1.0, f3=cc1,  f5=1.0)
      parameter (qrmin=1.0e-6, wc2min=0.01, gmf1=gmf/aa1, gmf5=gmf/f5)
!     parameter (qrmin=1.0e-6, wc2min=1.00, gmf1=gmf/aa1, gmf5=gmf/f5)
!     parameter (sialf=0.5)
!
      parameter (pi=3.1415926535897931, piinv=1.0/pi)
      integer itr, itrmu, itrmd, itrmin, itrmnd
!     parameter (itrmu=25, itrmd=25, itrmin=7)
      parameter (itrmu=25, itrmd=25, itrmin=12, itrmnd=12)
!     parameter (itrmu=25, itrmd=25, itrmin=12)
!     parameter (itrmu=14, itrmd=18, itrmin=7)
!     parameter (itrmu=10, itrmd=10, itrmin=5)
      real(kind=kind_phys) qrp(kd:k+1), wvl(kd:k+1), al2
      real(kind=kind_phys) wvlo(kd:k+1)
!
      real(kind=kind_phys) rnf(kd:k),   etd(kd:k+1), wcb(kd:k)          &
     &,                    hod(kd:k+1), qod(kd:k+1), evp(kd:k)          &
     &,                    ror(kd:k+1), stlt(kd:k)                      &
     &,                    ghd(kd:k),   gsd(kd:k),   cldfrd(kd:k)       &
     &,                    rnt,        rnb                              &
     &,                    errq,       rntp
      integer idw, idh, idn(k), idnm
!     real(kind=kind_phys) em(k*k), elm(k)
      real(kind=kind_phys) edz, ddz, ce, qhs, fac, facg, asin,          &
     &                     rsum1, rsum2
!     real(kind=kind_phys) cee
      logical ddft, ddlgk
!
      real(kind=kind_phys) aa(kd:k,kd:k+1), qw(kd:k,kd:k)               &
     &,                    bud(kd:k), vt(2), vrw(2), trw(2)             &
     &,                    gqw(kd:k)                                    &
     &,                    qa(3),     wa(3),    dof, dofw               &
     &,                    qrpi(kd:k)
!    &,                    gqw(kd:k), wcb(kd:k)

!***********************************************************************

      real(kind=kind_phys) qrpf, vtpf
      logical lprnt

!     if(lprnt) print *,' k=',k,' kd=',kd,' in downdrft'

      kd1    = kd + 1
      kp1    = k  + 1
      km1    = k  - 1
      kb1    = kbl - 1
!
!     vtp    = 36.34*sqrt(1.2)* (0.001)**0.1364
      vtpexp = -0.3636
!     piinv  = 1.0 / pi
      picon  = pi * onebg * 0.5
!
!     compute rain water budget of the updraft (cheng and arakawa, 1997)
!
      cldfrd = 0.0
      rntp   = 0.0
      dof    = 0.0
      errq   = 10.0
      rnb    = 0.0
      rnt    = 0.0
      tx2    = prl(kbl)
!
      tx1      = (prl(kd) + prl(kd1)) * 0.5
      ror(kd)  = cmpor*tx1 / (tol(kd)*(1.0+nu*qol(kd)))
!     gms(kd)  = vtp * ror(kd) ** vtpexp
      gms(kd)  = vtp * vtpf(ror(kd))
!
      qrp(kd)  = qrmin
!
      tem      = tol(k) * (1.0 + nu * qol(k))
      ror(k+1) = 0.5 * cmpor * (prl(k+1)+prl(k)) / tem
      gms(k+1) = vtp * vtpf(ror(k+1))
      qrp(k+1) = qrmin
!
      kk = kbl
      do l=kd1,k
        tem = 0.5 * (tol(l)+tol(l-1))                                   &
     &      * (1.0 + (0.5*nu) * (qol(l)+qol(l-1)))
        ror(l) = cmpor * prl(l) / tem
!       gms(l) = vtp * ror(l) ** vtpexp
        gms(l) = vtp * vtpf(ror(l))
        qrp(l) = qrmin
        if (buy(l) .le. 0.0 .and. kk .eq. kbl) then
          kk = l
        endif
      enddo
      if (kk .ne. kbl) then
        do l=kk,kbl
          buy(l) = 0.9 * buy(l-1)
        enddo
      endif
!
      do l=kd,k
        qrpi(l) = buy(l)
      enddo
      do l=kd1,kb1
        buy(l) = 0.25 * (qrpi(l-1)+qrpi(l)+qrpi(l)+qrpi(l+1))
      enddo
      
!
!     call angrad(tx1, alm, stla, ctl2, al2, pi, tla, tx2, wfn, tx3)
      tx1 = 1000.0 + tx1 - prl(k+1)
      call angrad(tx1, alm,  al2, tla, tx2, wfn, tx3)
!
!    following ucla approach for rain profile
!
      f2      = 2.0*bb1*onebg/(pi*0.2)
      wcmin   = sqrt(wc2min)
      wcbase  = wcmin
!
!     del_tla = tla * 0.2
!     del_tla = tla * 0.25
      del_tla = tla * 0.3
      tla     = tla - del_tla
!
      do l=kd,k
        rnf(l)   = 0.0
        rns(l)   = 0.0
        wvl(l)   = 0.0
        stlt(l)  = 0.0
        gqw(l)   = 0.0
        qrp(l)   = qrmin
        do n=kd,k
          qw(n,l) = 0.0
        enddo
      enddo
!
!-----qw(n,l) = d(w(n)*w(n))/dqr(l)
!
      kk = kbl
      qw(kd,kd)  = -qrb(kd)  * gmf1
      ghd(kd)    = eta(kd)   * eta(kd)
      gqw(kd)    = qw(kd,kd) * ghd(kd)
      gsd(kd)    = etai(kd)  * etai(kd)
!
      gqw(kk)    = - qrb(kk-1) * (gmf1+gmf1)
!
      wcb(kk)    = wcbase * wcbase

      tx1        = wcb(kk)
      gsd(kk)    = 1.0
      ghd(kk)    = 1.0
!
      tem        = gmf1 + gmf1
      do l=kb1,kd1,-1
         ghd(l)  = eta(l)  * eta(l)
         gsd(l)  = etai(l) * etai(l)
         gqw(l)  = - ghd(l) * (qrb(l-1)+qrt(l)) * tem
         qw(l,l) = - qrt(l) * tem
!
         st1     = 0.5 * (eta(l) + eta(l+1))
         tx1     = tx1 + buy(l) * tem * (qrb(l)+qrt(l)) * st1 * st1
         wcb(l)  = tx1 * gsd(l)
      enddo
!
      tem1        = (qrb(kd) + qrt(kd1) + qrt(kd1)) * gmf1
      gqw(kd1)    = - ghd(kd1) * tem1
      qw(kd1,kd1) = - qrt(kd1) * tem
      st1         = 0.5 * (eta(kd) + eta(kd1))
      wcb(kd)     = (tx1 + buy(kd)*tem*qrb(kd)*st1*st1) * gsd(kd)
!
      do l=kd1,kbl
        do n=kd,l-1
           qw(n,l) = gqw(l) * gsd(n)
        enddo
      enddo
      qw(kbl,kbl) = 0.0
!
      do ntla=1,numtla
!
!       if (errq .lt. 1.0 .or. tla .gt. 45.0) cycle
        if (errq .lt. 0.1 .or. tla .gt. 45.0) cycle
!
        tla = tla + del_tla
        stla = sin(tla*pi/180.0)
        ctl2 = 1.0 - stla * stla
!
!       if (lprnt) print *,' tla=',tla,' al2=',al2,' ptop='
!    &,0.5*(prl(kd)+prl(kd1)),' ntla=',ntla,' f2=',f2,' stla=',stla
!       if (lprnt) print *,' buy=',(buy(l),l=kd,kbl)
!
        stla = f2     * stla * al2
        ctl2 = dd1    * ctl2
        ctl3 = 0.1364 * ctl2
!
        do l=kd,k
          rnf(l)   = 0.0
          wvl(l)   = 0.0
          stlt(l)  = 0.0
          qrp(l)   = qrmin
        enddo
        wvl(kbl)   = wcbase
        stlt(kbl)  = 1.0 / wcbase
!
        do l=kd,k+1
          do n=kd,k
            aa(n,l) = 0.0
          enddo
        enddo
!
        skpup = .false.
!
        do itr=1,itrmu               ! rain profile iteration starts!
          if (.not. skpup) then
             wvlo = wvl
!
!-----calculating the vertical velocity
!
            tx1      = 0.0
            qrpi(kbl) = 1.0 / qrp(kbl)
            do l=kb1,kd,-1
              tx1     = tx1    + qrp(l+1) * gqw(l+1)
              st1     = wcb(l) + qw(l,l)  * qrp(l)                      &
     &                         + tx1      * gsd(l)
              if (st1 .gt. wc2min) then
!               wvl(l)  = sqrt(st1)
                wvl(l)  = 0.5 * (sqrt(st1) + wvl(l))
!               if (itr .eq. 1) wvl(l) = wvl(l) * 0.25
              else

!       if (lprnt)  print *,' l=',l,' st1=',st1,' wcb=',wcb(l),' qw='
!    &,qw(l,l),' qrp=',qrp(l),' tx1=',tx1,' gsd=',gsd(l),' ite=',itr

!               wvl(l) = 0.5*(wcmin+wvl(l))
                wvl(l) = 0.5*(wvl(l) + wvl(l+1))
                qrp(l) = 0.5*((wvl(l)*wvl(l)-wcb(l)-tx1*gsd(l))/qw(l,l) &
     &                      + qrp(l))
!!              wvl(l) = 0.5 * (wvl(l) + wvl(l+1))
              endif
!             wvl(l)  = 0.5 * (wvl(l) + wvlo(l))
!             wvl(l)  = sqrt(max(st1,wc2min))
              wvl(l)  = max(wvl(l), wcbase)
              stlt(l) = 1.0 / wvl(l)
              qrpi(l) = 1.0 / qrp(l)
            enddo
!
!       if (lprnt) then
!         print *,' itr=',itr,' itrmu=',itrmu
!         print *,' wvl=',(wvl(l),l=kd,kbl)
!         print *,' qrp=',(qrp(l),l=kd,kbl)
!         print *,' qrpi=',(qrpi(l),l=kd,kbl)
!         print *,' rnf=',(rnf(l),l=kd,kbl)
!       endif
!
!-----calculating trw, vrw and of
!
!           vt(1)   = gms(kd) * qrp(kd)**0.1364
            vt(1)   = gms(kd) * qrpf(qrp(kd))
            trw(1)  = eta(kd) * qrp(kd) * stlt(kd)
            tx6     = trw(1) * vt(1)
            vrw(1)  = f3*wvl(kd) - ctl2*vt(1)
            bud(kd) = stla * tx6 * qrb(kd) * 0.5
            rnf(kd) = bud(kd)
            dof     = 1.1364 * bud(kd) * qrpi(kd)
            dofw    = -bud(kd) * stlt(kd)
!
            rnt     = trw(1) * vrw(1)
            tx2     = 0.0
            tx4     = 0.0
            rnb     = rnt
            tx1     = 0.5
            tx8     = 0.0
!
            if (rnt .ge. 0.0) then
              tx3 = (rnt-ctl3*tx6) * qrpi(kd)
              tx5 = ctl2 * tx6 * stlt(kd)
            else
              tx3 = 0.0
              tx5 = 0.0
              rnt = 0.0
              rnb = 0.0
            endif
!
            do l=kd1,kb1
              ktem    = max(l-2, kd)
              ll      = l - 1
! 
!             vt(2)   = gms(l) * qrp(l)**0.1364
              vt(2)   = gms(l) * qrpf(qrp(l))
              trw(2)  = eta(l) * qrp(l) * stlt(l)
              vrw(2)  = f3*wvl(l) - ctl2*vt(2)
              qqq     = stla * trw(2) * vt(2)
              st1     = tx1  * qrb(ll)
              bud(l)  = qqq * (st1 + qrt(l))
!
              qa(2)   = dof
              wa(2)   = dofw
              dof     = 1.1364 * bud(l) * qrpi(l)
              dofw    = -bud(l) * stlt(l)
!
              rnf(ll) = rnf(ll) + qqq * st1
              rnf(l)  =           qqq * qrt(l)
!
              tem3    = vrw(1) + vrw(2)
              tem4    = trw(1) + trw(2)
!
              tx6     = .25 * tem3 * tem4
              tem4    = tem4 * ctl3
!
!-----by qr above
!
!             tem1    = .25*(trw(1)*tem3 - tem4*vt(1))*tx7
              tem1    = .25*(trw(1)*tem3 - tem4*vt(1))*qrpi(ll)
              st1     = .25*(trw(1)*(ctl2*vt(1)-vrw(2))                 &
     &                     * stlt(ll) + f3*trw(2))
!-----by qr below
              tem2    = .25*(trw(2)*tem3 - tem4*vt(2))*qrpi(l)
              st2     = .25*(trw(2)*(ctl2*vt(2)-vrw(1))                 &
     &                     * stlt(l)  + f3*trw(1))
!
!      from top to  the kbl-2 layer
!
              qa(1)   = tx2
              qa(2)   = qa(2) + tx3 - tem1
              qa(3)   = -tem2
!
              wa(1)   = tx4
              wa(2)   = wa(2) + tx5 - st1
              wa(3)   = -st2
!
              tx2     = tem1
              tx3     = tem2
              tx4     = st1
              tx5     = st2
!
              vt(1)   = vt(2)
              trw(1)  = trw(2)
              vrw(1)  = vrw(2)
!
              if (wvl(ktem) .eq. wcmin) wa(1) = 0.0
              if (wvl(ll)   .eq. wcmin) wa(2) = 0.0
              if (wvl(l)    .eq. wcmin) wa(3) = 0.0
              do n=ktem,kbl
                aa(ll,n) = (wa(1)*qw(ktem,n) * stlt(ktem)               &
     &                   +  wa(2)*qw(ll,n)   * stlt(ll)                 &
     &                   +  wa(3)*qw(l,n)    * stlt(l) ) * 0.5
              enddo
              aa(ll,ktem) = aa(ll,ktem) + qa(1)
              aa(ll,ll)   = aa(ll,ll)   + qa(2)
              aa(ll,l)    = aa(ll,l)    + qa(3)
              bud(ll)     = (tx8 + rnn(ll)) * 0.5                       &
     &                      - rnb + tx6 - bud(ll)
              aa(ll,kbl+1) = bud(ll)
              rnb = tx6
              tx1 = 1.0
              tx8 = rnn(ll)
            enddo
            l  = kbl
            ll = l - 1
!           vt(2)   = gms(l) * qrp(l)**0.1364
            vt(2)   = gms(l) * qrpf(qrp(l))
            trw(2)  = eta(l) * qrp(l) * stlt(l)
            vrw(2)  = f3*wvl(l) - ctl2*vt(2)
            st1     = stla * trw(2) * vt(2) * qrb(ll)
            bud(l)  = st1

            qa(2)   = dof
            wa(2)   = dofw
            dof     = 1.1364 * bud(l) * qrpi(l)
            dofw    = -bud(l) * stlt(l)
!
            rnf(ll) = rnf(ll) + st1
!
            tem3    = vrw(1) + vrw(2)
            tem4    = trw(1) + trw(2)
!
            tx6     = .25 * tem3 * tem4
            tem4    = tem4 * ctl3
!
!-----by qr above
!
            tem1    = .25*(trw(1)*tem3 - tem4*vt(1))*qrpi(ll)
            st1     = .25*(trw(1)*(ctl2*vt(1)-vrw(2))                   &
     &                  * stlt(ll) + f3*trw(2))
!-----by qr below
            tem2    = .25*(trw(2)*tem3 - tem4*vt(2))*qrpi(l)
            st2     = .25*(trw(2)*(ctl2*vt(2)-vrw(1))                   &
     &                   * stlt(l)  + f3*trw(1))
!
!      for the layer next to the top of the boundary layer
!
            qa(1)   = tx2
            qa(2)   = qa(2) + tx3 - tem1
            qa(3)   = -tem2
!
            wa(1)   = tx4
            wa(2)   = wa(2) + tx5 - st1
            wa(3)   = -st2
!
            tx2     = tem1
            tx3     = tem2
            tx4     = st1
            tx5     = st2
!
            idw     = max(l-2, kd)
!
            if (wvl(idw) .eq. wcmin) wa(1) = 0.0
            if (wvl(ll)  .eq. wcmin) wa(2) = 0.0
            if (wvl(l)   .eq. wcmin) wa(3) = 0.0
!
            kk = idw
            do n=kk,l
              aa(ll,n) = (wa(1)*qw(kk,n) * stlt(kk)                     &
     &                 +  wa(2)*qw(ll,n) * stlt(ll)                     &
     &                 +  wa(3)*qw(l,n)  * stlt(l) ) * 0.5

            enddo
!
            aa(ll,idw) = aa(ll,idw) + qa(1)
            aa(ll,ll)  = aa(ll,ll)  + qa(2)
            aa(ll,l)   = aa(ll,l)   + qa(3)
            bud(ll)    = (tx8+rnn(ll)) * 0.5 - rnb + tx6 - bud(ll)
!
            aa(ll,l+1) = bud(ll)
!
            rnb        = trw(2) * vrw(2)
!
!      for the top of the boundary layer
!
            if (rnb .lt. 0.0) then
               kk    = kbl
               tem   = vt(2) * trw(2)
               qa(2) = (rnb - ctl3*tem) * qrpi(kk)
               wa(2) = ctl2 * tem * stlt(kk)
            else
               rnb   = 0.0
               qa(2) = 0.0
               wa(2) = 0.0
            endif
!
            qa(1) = tx2
            qa(2) = dof + tx3 - qa(2)
            qa(3) = 0.0
!
            wa(1) = tx4
            wa(2) = dofw + tx5 - wa(2)
            wa(3) = 0.0
!
            kk = kbl
            if (wvl(kk-1) .eq. wcmin) wa(1) = 0.0
            if (wvl(kk)   .eq. wcmin) wa(2) = 0.0
!
            do ii=1,2
               n = kk + ii - 2
               aa(kk,n) = (wa(1)*qw(kk-1,n) * stlt(kk-1)                &
     &                  +  wa(2)*qw(kk,n)   * stlt(kk)) * 0.5
            enddo
            fac = 0.5
            ll  = kbl
            l   = ll + 1
            lm1 = ll - 1
            aa(ll,lm1)  = aa(ll,lm1) + qa(1)
            aa(ll,ll)   = aa(ll,ll)  + qa(2)
            bud(ll)     = 0.5*rnn(lm1) - tx6 + rnb - bud(ll)
            aa(ll,ll+1) = bud(ll)
!
!-----solving the budget equations for dqr
!
            do l=kd1,kbl
              lm1  = l - 1
              cnvflg = abs(aa(lm1,lm1)) .lt. abs(aa(l,lm1))
              do  n=lm1,kbl+1
                 if (cnvflg) then
                    tx1       = aa(lm1,n)
                    aa(lm1,n) = aa(l,n)
                    aa(l,n)   = tx1
                 endif
              enddo
              tx1 = aa(l,lm1) / aa(lm1,lm1)
              do  n=l,kbl+1
                aa(l,n) = aa(l,n) - tx1 * aa(lm1,n)
              enddo
            enddo     
!
!-----back substitution and check if the solution converges
!
            kk = kbl
            kk1 = kk + 1
            aa(kk,kk1) = aa(kk,kk1) / aa(kk,kk)      !   qr correction !
            tx2        = abs(aa(kk,kk1)) * qrpi(kk)  !   error measure !
!     if (lprnt) print *,' tx2a=',tx2,' aa1=',aa(kk,kk1)
!    &,' qrpi=',qrpi(kk)
!
            kk = kbl + 1
            do l=kb1,kd,-1
               lp1   = l + 1
               tx1  = 0.0
               do n=lp1,kbl
                 tx1  = tx1 + aa(l,n) * aa(n,kk)
               enddo
               aa(l,kk) = (aa(l,kk) - tx1) / aa(l,l)       ! qr correction !
               tx2      = max(tx2, abs(aa(l,kk))*qrpi(l))  ! error measure !

!     if (lprnt) print *,' tx2b=',tx2,' aa1=',aa(l,kk)
!    &,' qrpi=',qrpi(l),' l=',l

            enddo
!
!           tem = 0.5
            if (tx2 .gt. 1.0 .and. abs(errq-tx2) .gt. 0.1) then
              tem = 0.5
!!          elseif (tx2 .lt. 0.1) then
!!            tem = 1.2
            else
              tem = 1.0
            endif
!
            do l=kd,kbl
!              qrp(l) = max(qrp(l)+aa(l,kbl+1), qrmin)
               qrp(l) = max(qrp(l)+aa(l,kbl+1)*tem, qrmin)
            enddo
!
!       if (lprnt) print *,' itr=',itr,' tx2=',tx2

            if (itr .lt. itrmin) then
               tem = abs(errq-tx2) 
               if (tem .ge. errmi2 .and. tx2 .ge. errmin) then 
                 errq  = tx2                              ! further iteration !
               else 
                 skpup = .true.                           ! converges      !
                 errq  = 0.0                              ! rain profile exists!
!     if (lprnt) print *,' here1',' tem=',tem,' tx2=',tx2,' errmi2=',
!    *errmi2,' errmin=',errmin
               endif 
            else
               tem = errq - tx2
!              if (tem .lt. zero .and. errq .gt. 0.1) then
               if (tem .lt. zero .and. errq .gt. 0.5) then
!              if (tem .lt. zero .and.                                    &
!    &            (ntla .lt. numtla .or. errq .gt. 0.5)) then
!     if (lprnt) print *,' tx2=',tx2,' errq=',errq,' tem=',tem
                 skpup = .true.                           ! no convergence !
                 errq = 10.0                              ! no rain profile!
!!!!           elseif (abs(tem).lt.errmi2 .or. tx2.lt.errmin) then
               elseif (tx2.lt.errmin) then
                 skpup = .true.                           ! converges      !
                 errq = 0.0                               ! rain profile exists!
!     if (lprnt) print *,' here2'
               elseif (tem .lt. zero .and. errq .lt. 0.1) then
                 skpup = .true.
!                if (ntla .eq. numtla .or. tem .gt. -0.003) then
                   errq  = 0.0
!                else
!                  errq = 10.0
!                endif
               else
                 errq = tx2                               ! further iteration !
!     if (lprnt) print *,' itr=',itr,' errq=',errq
!              if (itr .eq. itrmu .and. errq .gt. errmin*10             &
!    &            .and. ntla .eq. 1) errq = 10.0 
               endif
            endif
!
!         if (lprnt) print *,' errq=',errq

          endif                                           ! skpup  endif!
!
        enddo                                          ! end of the itr loop!!
!
!     if(lprnt) then
!       print *,' qrp=',(qrp(l),l=kd,kbl)
!       print *,'rnf=',(rnf(l),l=kd,kbl),' rnt=',rnt,' rnb=',rnb
!    &,' errq=',errq
!     endif
!
        if (errq .lt. 0.1) then
          ddft = .true.
          rnb  = - rnb
   !      do l=kd1,kb1-1
   !        if (wvl(l)-wcbase .lt. 1.0e-9) ddft = .false.
   !      enddo
        else
          ddft = .false.
        endif
!
!     caution !! below is an adjustment to rain flux to maintain
!                conservation of precip!
!
        if (ddft) then
          tx1 = 0.0
          do l=kd,kb1
            tx1 = tx1 + rnf(l)
          enddo
!     if (lprnt) print *,' tx1+rnt+rnb=',tx1+rnt+rnb, ' train=',train
          tx1 = train / (tx1+rnt+rnb)
          if (abs(tx1-1.0) .lt. 0.2) then
             rnt = max(rnt*tx1,zero)
             rnb = rnb * tx1
          else
             ddft = .false.
             errq = 10.0
          endif
        endif
      enddo                                          ! end of ntla loop
!
      dof = 0.0
      if (.not. ddft) return     ! rain profile did not converge!
!

      do l=kd,kb1
         rnf(l) = rnf(l) * tx1

      enddo
!     if (lprnt) print *,' train=',train
!     if (lprnt) print *,' rnf=',rnf
!
!     adjustment is over
!
!     downdraft
!
      do l=kd,k
        wcb(l) = 0.0
      enddo
!
      skpdd = .not. ddft
!
      errq  = 10.0
      if (.not. skpdd) then
!
!     calculate downdraft properties
!

        kk = max(kb1,kd1)
        do l=kk,k
          stlt(l) = stlt(l-1)
        enddo
        tem1 = 1.0 / bb1
!
        do l=kd,k
          if (l .le. kbl) then
            tem     = stla * tem1
            stlt(l) = eta(l) * stlt(l) * tem / ror(l)
          else
            stlt(l) = 0.0
          endif
        enddo
!       if (lprnt) print *,' stlt=',stlt

        rsum1 = 0.0
        rsum2 = 0.0

!
        idn      = 99
        do l=kd,k+1
          etd(l)  = 0.0
          wvl(l)  = 0.0
!         qrp(l)  = 0.0
        enddo
        do l=kd,k
          evp(l)   = 0.0
          buy(l)   = 0.0
          qrp(l+1) = 0.0
        enddo
        hod(kd)  = hol(kd)
        qod(kd)  = qol(kd)
        tx1      = 0.0                               ! sigma at the top
!!!     tx1      = stlt(kd)*qrb(kd)*one              ! sigma at the top
!       tx1      = min(stlt(kd)*qrb(kd)*one, one)    ! sigma at the top
!       tx1      = min(stlt(kd)*qrb(kd)*0.5, one)    ! sigma at the top
        rntp     = 0.0
        tx5      = tx1
        qa(1)    = 0.0
!     if(lprnt) print *,' stlt=',stlt(kd),' qrb=',qrb(kd)
!    *,' tx1=',tx1,' ror=',ror(kd),' gms=',gms(kd),' rpart=',rpart
!    *,' rnt=',rnt
!
!       here we assume rpart of detrained rain rnt goes to pd
!
        if (rnt .gt. 0.0) then
          if (tx1 .gt. 0.0) then
            qrp(kd) = (rpart*rnt / (ror(kd)*tx1*gms(kd)))               &
     &                                          ** (1.0/1.1364)
           else
             tx1 = rpart*rnt / (ror(kd)*gms(kd)*qrp(kd)**1.1364)
           endif
            rntp    = (1.0 - rpart) * rnt
            buy(kd) = - ror(kd) * tx1 * qrp(kd)
        else
          qrp(kd) = 0.0
        endif
!
!     l-loop for the downdraft iteration from kd1 to k+1 (bottom surface)
!
!     bud(kd) = ror(kd)
      idnm = 1
      do l=kd1,k+1

          qa(1) = 0.0
          ddlgk = idn(idnm) .eq. 99
          if (.not. ddlgk) cycle
          if (l .le. k) then
            st1   = 1.0 - alfind(l)
            wa(1) = alfind(l)*hol(l-1) + st1*hol(l)
            wa(2) = alfind(l)*qol(l-1) + st1*qol(l)
            wa(3) = alfind(l)*tol(l-1) + st1*tol(l)
            qa(2) = alfind(l)*hst(l-1) + st1*hst(l)
            qa(3) = alfind(l)*qst(l-1) + st1*qst(l)
          else
            wa(1) = hol(k)
            wa(2) = qol(k)
            wa(3) = tol(k)
            qa(2) = hst(k)
            qa(3) = qst(k)
          endif
!
          fac = 2.0
          if (l .eq. kd1) fac = 1.0

          facg    = fac * 0.5 * gmf5     !  12/17/97
!
!         ddlgk   =  idn(idnm) .eq. 99
          bud(kd) = ror(l)

!         if (ddlgk) then
            tx1    = tx5
            wvl(l) = max(wvl(l-1),one_m1)

            qrp(l) = max(qrp(l-1),qrp(l))
!
!           vt(1)  = gms(l-1) * qrp(l-1) ** 0.1364
            vt(1)  = gms(l-1) * qrpf(qrp(l-1))
            rnt    = ror(l-1) * (wvl(l-1)+vt(1))*qrp(l-1)
!     if(lprnt) print *,' l=',l,' qa=',qa(1), ' tx1rnt=',rnt*tx1,
!    *' wvl=',wvl(l-1)
!    *,' qrp=',qrp(l-1),' tx5=',tx5,' tx1=',tx1,' rnt=',rnt

!

!           tem    = max(alm, 2.5e-4) * max(eta(l), 1.0)
            tem    = max(alm,one_m6) * max(eta(l), one)
!           tem    = max(alm, 1.0e-5) * max(eta(l), 1.0)
            trw(1) = picon*tem*(qrb(l-1)+qrt(l-1))
            trw(2) = 1.0 / trw(1)
!
            vrw(1) = 0.5 * (gam(l-1) + gam(l))
            vrw(2) = 1.0 / (vrw(1) + vrw(1))
!
            tx4    =  (qrt(l-1)+qrb(l-1))*(onebg*fac*500.00*eknob)
!
            dofw   = 1.0 / (wa(3) * (1.0 + nu*wa(2)))      !  1.0 / tvbar!
!
            etd(l) = etd(l-1)
            hod(l) = hod(l-1)
            qod(l) = qod(l-1)
!
            errq   = 10.0

!
            if (l .le. kbl) then
              tx3 = stlt(l-1) * qrt(l-1) * (0.5*fac)
              tx8 = stlt(l)   * qrb(l-1) * (0.5*fac)
              tx9 = tx8 + tx3
            else
              tx3 = 0.0
              tx8 = 0.0
              tx9 = 0.0
            endif
!
            tem  = wvl(l-1) + vt(1)
            if (tem .gt. 0.0) then
              tem1 = 1.0 / (tem*ror(l-1))
              tx3 = vt(1) * tem1 * ror(l-1) * tx3
              tx6 = tx1 * tem1
            else
              tx6 = 1.0
            endif
!         endif
!
          if (l .eq. kd1) then
            if (rnt .gt. 0.0) then
              tem    = max(qrp(l-1),qrp(l))
              wvl(l) = tx1 * tem * qrb(l-1)*(facg*5.0)
            endif
            wvl(l) = max(one_m2, wvl(l))
            trw(1) = trw(1) * 0.5
            trw(2) = trw(2) + trw(2)
          else
            if (ddlgk) evp(l-1) = evp(l-2)
          endif
!
!       no downdraft above level idh
!

          if (l .lt. idh) then

            etd(l)   = 0.0
            hod(l)   = wa(1)
            qod(l)   = wa(2)
            evp(l-1) = 0.0
            wvl(l)   = 0.0
            qrp(l)   = 0.0
            buy(l)   = 0.0
            tx5      = tx9
            errq     = 0.0
            rntp     = rntp + rnt * tx1
            rnt      = 0.0
            wcb(l-1) = 0.0
          endif
!         bud(kd) = ror(l)
!
!       iteration loop for a given level l begins
!
!         if (lprnt) print *,' tx8=',tx8,' tx9=',tx9,' tx5=',tx5
!    &,                      ' tx1=',tx1
          do itr=1,itrmd
!
!           cnvflg =  ddlgk .and. (errq .gt. errmin)
            cnvflg =  errq .gt. errmin
            if (cnvflg) then
!
!             vt(1)  = gms(l) * qrp(l) ** 0.1364
              vt(1)  = gms(l) * qrpf(qrp(l))
              tem    =  wvl(l) + vt(1)
!
              if (tem .gt. 0.0) then
                st1    = ror(l) * tem * qrp(l) + rnt
                if (st1 .ne. 0.0) st1 = 2.0 * evp(l-1) / st1
                tem1   = 1.0 / (tem*ror(l))
                tem2   = vt(1) * tem1 * ror(l) * tx8
              else
                tem1   = 0.0
                tem2   = tx8
                st1    = 0.0
              endif
!     if (lprnt) print *,' st1=',st1,' tem=',tem,' ror=',ror(l)
!    &,' qrp=',qrp(l),' rnt=',rnt,' ror1=',ror(l-1),' wvl=',wvl(l)
!    &,' wvl1=',wvl(l-1),' tem2=',tem2,' vt=',vt(1),' tx3=',tx3
!
              st2 = tx5
              tem = ror(l)*wvl(l) - ror(l-1)*wvl(l-1)
              if (tem .gt. 0.0) then
                tx5 = (tx1 - st1 + tem2 + tx3)/(1.0+tem*tem1)
              else
                tx5 = tx1 - tem*tx6 - st1 + tem2 + tx3
              endif
              tx5   = max(tx5,zero)
              tx5 = 0.5 * (tx5 + st2)
!
!             qqq = 1.0 + tem * tem1 * (1.0 - sialf)
!
!             if (qqq .gt. 0.0) then
!               tx5   = (tx1 - sialf*tem*tx6 - st1 + tem2 + tx3) / qqq
!             else
!               tx5   = (tx1 - tem*tx6 - st1 + tem2 + tx3)
!             endif
!
!     if(lprnt) print *,' tx51=',tx5,' tx1=',tx1,' st1=',st1,' tem2='
!     if(tx5 .le. 0.0 .and. l .gt. kd+2)
!    * print *,' tx51=',tx5,' tx1=',tx1,' st1=',st1,' tem2='
!    *,tem2,' tx3=',tx3,' tem=',tem,' tem1=',tem1,' wvl=',wvl(l-1),
!    &wvl(l),' l=',l,' itr=',itr,' evp=',evp(l-1),' vt=',vt(1)
!    *,' qrp=',qrp(l),' rnt=',rnt,' kd=',kd
!     if (lprnt) print *,' etd=',etd(l),' wvl=',wvl(l)
!    &,' trw=',trw(1),trw(2),' ror=',ror(l),' wa=',wa


!
              tem1   = etd(l)
              etd(l) = ror(l) * tx5 * max(wvl(l),zero)
!
              if (etd(l) .gt. 0.0) etd(l) = 0.5 * (etd(l) + tem1)
!

              del_eta = etd(l) - etd(l-1)

!               tem       = del_eta * trw(2)
!               tem2      = max(min(tem, 1.0), -1.0)
!               if (abs(tem) .gt. 1.0 .and. etd(l) .gt. 0.0 ) then
!                 del_eta = tem2 * trw(1)
!                 etd(l)  = etd(l-1) + del_eta
!               endif
!               if (wvl(l) .gt. 0.0) tx5 = etd(l) / (ror(l)*wvl(l))
!
                erre  = etd(l) - tem1
!
                tem  = max(abs(del_eta), trw(1))
                tem2 = del_eta / tem
                tem1 = sqrt(max((tem+del_eta)*(tem-del_eta),zero))
!               tem1 = sqrt(max((trw(1)+del_eta)*(trw(1)-del_eta),0.0))

                edz  = (0.5 + asin(tem2)*piinv)*del_eta + tem1*piinv

              ddz   = edz - del_eta
              wcb(l-1) = etd(l) + ddz
!
              tem1  = hod(l)
              if (del_eta .gt. 0.0) then
                qqq    = 1.0 / (etd(l) + ddz)
                hod(l) = (etd(l-1)*hod(l-1) + del_eta*hol(l-1)          &
     &                                            + ddz*wa(1)) * qqq
                qod(l) = (etd(l-1)*qod(l-1) + del_eta*qol(l-1)          &
     &                                            + ddz*wa(2)) * qqq
              elseif((etd(l-1) + edz) .gt. 0.0) then
                qqq    = 1.0 / (etd(l-1) + edz)
                hod(l) = (etd(l-1)*hod(l-1) + edz*wa(1)) * qqq
                qod(l) = (etd(l-1)*qod(l-1) + edz*wa(2)) * qqq
              endif
              errh  = hod(l) - tem1
              errq  = abs(errh/hod(l))  + abs(erre/max(etd(l),one_m5))
!     if (lprnt) print *,' errqp=',errq,' errh=',errh,' hod=',hod(l)
!    &,' erre=',erre,' etd=',etd(l),' del_eta=',del_eta
              dof   = ddz
              vt(2) = qqq

!
              ddz  = dof
              tem4 = qod(l)
              tem1 = vrw(1)
!
              qhs  = qa(3) + 0.5 * (gaf(l-1)+gaf(l))                    &
     &                           * (hod(l)-qa(2))
!
!                                           first iteration       !
!
              st2  = prl(l) * (qhs + tem1 * (qhs-qod(l)))
              tem2 = ror(l) * qrp(l)
              call qrabf(tem2,qraf,qrbf)
              tem6 = tx5 * (1.6 + 124.9 * qraf) * qrbf * tx4
!
              ce   = tem6 * st2 / ((5.4e5*st2 + 2.55e6)*(etd(l)+ddz))
!
              tem2   = - ((1.0+tem1)*(qhs+ce) + tem1*qod(l))
              tem3   = (1.0 + tem1) * qhs * (qod(l)+ce)
              tem    = max(tem2*tem2 - 4.0*tem1*tem3,zero)
              qod(l) = max(tem4, (- tem2 - sqrt(tem)) * vrw(2))
!

!
!                                            second iteration   !
!
              st2  = prl(l) * (qhs + tem1 * (qhs-qod(l)))
              ce   = tem6 * st2 / ((5.4e5*st2 + 2.55e6)*(etd(l)+ddz))
!             cee  = ce * (etd(l)+ddz)
!


              tem2   = - ((1.0+tem1)*(qhs+ce) + tem1*tem4)
              tem3   = (1.0 + tem1) * qhs * (tem4+ce)
              tem    = max(tem2*tem2 - 4.0*tem1*tem3,zero)
              qod(l) = max(tem4, (- tem2 - sqrt(tem)) * vrw(2))
!                                              evaporation in layer l-1
!

              evp(l-1) = (qod(l)-tem4) * (etd(l)+ddz)
!                                              calculate pd (l+1/2)
              qa(1)    = tx1*rnt + rnf(l-1) - evp(l-1)
!
!     if(lprnt) print *,' etd=',etd(l),' tx5=',tx5,' rnt=',rnt
!    *,' rnf=',rnf(l-1),' evp=',evp(l-1),' itr=',itr,' l=',l

!
              if (qa(1) .gt. 0.0) then
              if (etd(l) .gt. 0.0) then
                tem    = qa(1) / (etd(l)+ror(l)*tx5*vt(1))
                qrp(l) = max(tem,zero)
              elseif (tx5 .gt. 0.0) then
                qrp(l) = (max(zero,qa(1)/(ror(l)*tx5*gms(l))))           &
     &                                          ** (1.0/1.1364)
              else
                qrp(l) = 0.0
              endif
              else
                qrp(l) = 0.5 * qrp(l)
              endif
!                                              compute buoyancy
              tem1   = wa(3)+(hod(l)-wa(1)-alhl*(qod(l)-wa(2)))         &
     &                                                  * (1.0/cp)
!             if (lprnt) print *,' tem1=',tem1,' wa3=',wa(3),' hod='
!    &,hod(l),' wa1=',wa(1),' qod=',qod(l),' wa2=',wa(2),' alhl=',alhl
!    &,' cmpor=',cmpor,' dofw=',dofw,' prl=',prl(l),' qrp=',qrp(l)
              tem1   = tem1 * (1.0 + nu*qod(l))
              ror(l) = cmpor * prl(l) / tem1
              tem1   = tem1 * dofw
!!!           tem1   = tem1 * (1.0 + nu*qod(l)) * dofw

              buy(l) = (tem1 - 1.0 - qrp(l)) * ror(l) * tx5
!                                              compute w (l+1/2)

              tem1   = wvl(l)
!             if (etd(l) .gt. 0.0) then
              wvl(l) = vt(2) * (etd(l-1)*wvl(l-1) - facg                &
     &                 * (buy(l-1)*qrt(l-1)+buy(l)*qrb(l-1)))
!
!             if (lprnt) print *,' wvl=',wvl(l),'vt2=',vt(2),' buy1='
!    &,buy(l-1),' buy=',buy(l),' qrt1=',qrt(l-1),' qrb1=',qrb(l-1)
!    &,' etd1=',etd(l-1),' wvl1=',wvl(l-1)
!             endif
!
              if (wvl(l) .lt. 0.0) then
!               wvl(l) = max(wvl(l), 0.1*tem1)
!               wvl(l) = 0.5*tem1
!               wvl(l) = 0.1*tem1
!               wvl(l) = 0.0
                wvl(l) = 1.0e-10
              else
                wvl(l) = 0.5*(wvl(l)+tem1)
              endif

!
!             wvl(l) = max(0.5*(wvl(l)+tem1), 0.0)

              errw   = wvl(l) - tem1
!
              errq   = errq + abs(errw/max(wvl(l),one_m5))

!     if (lprnt) print *,' errw=',errw,' wvl=',wvl(l)
!     if(lprnt .or. tx5 .eq. 0.0) then
!     if(tx5 .eq. 0.0 .and. l .gt. kbl) then
!        print *,' errq=',errq,' itr=',itr,' l=',l,' wvl=',wvl(l)
!    &,' tx5=',tx5,' idnm=',idnm,' etd1=',etd(l-1),' etd=',etd(l)
!    &,' kbl=',kbl
!     endif
!
!     if(lprnt) print *,' itr=',itr,' itrmnd=',itrmnd,' itrmd=',itrmd
!             if (itr .ge. min(itrmin,itrmd/2)) then
              if (itr .ge. min(itrmnd,itrmd/2)) then
!     if(lprnt) print *,' itr=',itr,' etd1=',etd(l-1),' errq=',errq
                if (etd(l-1) .eq. 0.0 .and. errq .gt. 0.2) then
!     if(lprnt) print *,' bud=',bud(kd),' wa=',wa(1),wa(2)
                  ror(l)   = bud(kd)
                  etd(l)   = 0.0
                  wvl(l)   = 0.0
                  errq     = 0.0
                  hod(l)   = wa(1)
                  qod(l)   = wa(2)
!                 tx5      = tx1 + tx9
                  if (l .le. kbl) then
                    tx5      = tx9
                  else
                    tx5 = (stlt(kb1) * qrt(kb1)                         &
     &                  +  stlt(kbl) * qrb(kb1)) * (0.5*fac)
                  endif

!     if(lprnt) print *,' tx1=',tx1,' rnt=',rnt,' rnf=',rnf(l-1)
!    *,' evp=',evp(l-1),' l=',l

                  evp(l-1) = 0.0
                  tem      = max(tx1*rnt+rnf(l-1),zero)
                  qa(1)    = tem - evp(l-1)
!                 if (qa(1) .gt. 0.0) then

!     if(lprnt) print *,' ror=',ror(l),' tx5=',tx5,' tx1=',tx1
!    *,' tx9=',tx9,' gms=',gms(l),' qa=',qa(1)
!     if(lprnt) call mpi_quit(13)
!     if (tx5 .eq. 0.0 .or. gms(l) .eq. 0.0)
!     if (lprnt) 
!    *  print *,' atx5=',tx5,' gms=',gms(l),' ror=',ror(l)
!    *,' l=',l,' qa=',qa(1),' tx1=',tx1,' tx9=',tx9
!    *,' kbl=',kbl,' etd1=',etd(l-1),' idnm=',idnm,' idn=',idn(idnm)
!    *,' errq=',errq

                  qrp(l)   = (qa(1) / (ror(l)*tx5*gms(l)))              &
     &                                            ** (1.0/1.1364)
!                 endif
                  buy(l)   = - ror(l) * tx5 * qrp(l)
                  wcb(l-1) = 0.0
                endif
!
                del_eta = etd(l) - etd(l-1)
                if(del_eta .lt. 0.0 .and. errq .gt. 0.1) then
                  ror(l)   = bud(kd)
                  etd(l)   = 0.0
                  wvl(l)   = 0.0
!!!!!             tx5      = tx1 + tx9
                  cldfrd(l-1) = tx5
!
                  del_eta  = - etd(l-1)
                  edz      = 0.0
                  ddz      = -del_eta
                  wcb(l-1) = ddz
!
                  hod(l)   = hod(l-1)
                  qod(l)   = qod(l-1)
!
                  tem4     = qod(l)
                  tem1     = vrw(1)
!
                  qhs      = qa(3) + 0.5 * (gaf(l-1)+gaf(l))            &
     &                                   * (hod(l)-qa(2))

!
!                                           first iteration       !
!
                  st2  = prl(l) * (qhs + tem1 * (qhs-qod(l)))
                  tem2 = ror(l) * qrp(l-1)
                  call qrabf(tem2,qraf,qrbf)
                  tem6 = tx5 * (1.6 + 124.9 * qraf) * qrbf * tx4
!
                  ce   = tem6*st2/((5.4e5*st2 + 2.55e6)*(etd(l)+ddz))
!

                  tem2   = - ((1.0+tem1)*(qhs+ce) + tem1*qod(l))
                  tem3   = (1.0 + tem1) * qhs * (qod(l)+ce)
                  tem    = max(tem2*tem2 -four*tem1*tem3,zero)
                  qod(l) = max(tem4, (- tem2 - sqrt(tem)) * vrw(2))
!
!                                            second iteration   !
!
                  st2  = prl(l) * (qhs + tem1 * (qhs-qod(l)))
                  ce   = tem6*st2/((5.4e5*st2 + 2.55e6)*(etd(l)+ddz))
!                 cee  = ce * (etd(l)+ddz)
!


                  tem2   = - ((1.0+tem1)*(qhs+ce) + tem1*tem4)
                  tem3   = (1.0 + tem1) * qhs * (tem4+ce)
                  tem    = max(tem2*tem2 -four*tem1*tem3,zero)
                  qod(l) = max(tem4, (- tem2 - sqrt(tem)) * vrw(2))

!                                              evaporation in layer l-1
!
                  evp(l-1) = (qod(l)-tem4) * (etd(l)+ddz)

!                                               calculate pd (l+1/2)
!                 rnn(l-1) = tx1*rnt + rnf(l-1) - evp(l-1)

                  qa(1)    = tx1*rnt + rnf(l-1)
                  evp(l-1) = min(evp(l-1), qa(1))
                  qa(1)    = qa(1) - evp(l-1)
                  qrp(l)   = 0.0

!
!     if (tx5 .eq. 0.0 .or. gms(l) .eq. 0.0)
!     if (lprnt)
!    *  print *,' btx5=',tx5,' gms=',gms(l),' ror=',ror(l)
!    *,' l=',l,' qa=',qa(1),' tx1=',tx1,' tx9=',tx9
!    *,' kbl=',kbl,' etd1=',etd(l-1),' del_eta=',del_eta
!    &,' evp=',evp(l-1)
!
!                 if (qa(1) .gt. 0.0) then
!!                  rns(l-1) = qa(1)
!!!                 tx5      = tx9
!                   qrp(l) = (qa(1) / (ror(l)*tx5*gms(l)))              &
!    &                                         ** (1.0/1.1364)
!                 endif
!                 errq   = 0.0
!                                              compute buoyancy
!                 tem1   = wa(3)+(hod(l)-wa(1)-alhl*(qod(l)-wa(2)))     &
!    &                                                  * (1.0/cp)
!                 tem1   = tem1 * (1.0 + nu*qod(l)) * dofw
!                 buy(l) = (tem1 - 1.0 - qrp(l)) * ror(l) * tx5
!
!                 if (qa(1) .gt. 0.0) rns(l) = qa(1)

                  if (l .le. k) then
                     rns(l) = qa(1)
                     qa(1)  = 0.0
                  endif
                  tx5      = tx9
                  errq     = 0.0
                  qrp(l)   = 0.0
                  buy(l)   = 0.0
!
                endif
              endif
            endif
!
          enddo                ! end of the iteration loop  for a given l!
          if (l .le. k) then
            if (etd(l-1) .eq. 0.0                                       &
     &         .and. errq .gt. 0.1 .and. l .le. kbl) then
!!!  &         .and. errq .gt. errmin*10.0 .and. l .le. kbl) then
!    &         .and. errq .gt. errmin*10.0) then
               ror(l)   = bud(kd)
               hod(l)   = wa(1)
               qod(l)   = wa(2)
               tx5      =       tx9     ! does not make too much difference!
!              tx5      = tx1 + tx9
               evp(l-1) = 0.0
!              evp(l-1) = cee * (1.0 - qod(l)/qa(3))
               qa(1)    = tx1*rnt + rnf(l-1)
               evp(l-1) = min(evp(l-1), qa(1))
               qa(1)    = qa(1) - evp(l-1)

!              qrp(l)   = 0.0
!              if (tx5 .eq. 0.0 .or. gms(l) .eq. 0.0) then
!                print *,' ctx5=',tx5,' gms=',gms(l),' ror=',ror(l)     &
!    &,          ' l=',l,' qa=',qa(1),' tx1=',tx1,' tx9=',tx9           &
!    &,          ' kbl=',kbl,' etd1=',etd(l-1),' del_eta=',del_eta
!              endif
!              if (qa(1) .gt. 0.0) then

                 qrp(l) = (qa(1) / (ror(l)*tx5*gms(l)))                 &
     &                                         ** (1.0/1.1364)
!              endif
               etd(l)   = 0.0
               wvl(l)   = 0.0
               st1      = 1.0 - alfind(l)

               errq     = 0.0
               buy(l)   = - ror(l) * tx5 * qrp(l)
               wcb(l-1) = 0.0
            endif
          endif
!
          ll = min(idn(idnm), k+1)
          if (errq .lt. 1.0 .and. l .le. ll) then
            if (etd(l-1) .gt. 0.0 .and. etd(l) .eq. 0.0) then
             idn(idnm) = l
             wvl(l)    = 0.0
             if (l .lt. kbl .or. tx5 .gt. 0.0) idnm  = idnm + 1
             errq      = 0.0
            endif
            if (etd(l) .eq. 0.0 .and. l .gt. kbl) then
              idn(idnm) = l
              if (tx5 .gt. 0.0) idnm  = idnm + 1
            endif
          endif

!         if (lprnt) then
!           print *,' errq=',errq,' idn=',idn(idnm),' idnm=',idnm
!           print *,' l=',l,' qrp=',qrp(l),' etd=',etd(l),' qa=',qa(1)
!    *,' evp=',evp(l-1),' rnf=',rnf(l-1)
!         endif

! 
!     if downdraft properties are not obtainable, (i.e.solution does
!      not converge) , no downdraft is assumed
!
!         if (errq .gt. errmin*100.0 .and. idn(idnm) .eq. 99)           &
          if (errq .gt. 0.1 .and. idn(idnm) .eq. 99)                    &
     &                          ddft = .false.
!
!
          dof = 0.0
          if (.not. ddft) return
!
!         if (ddlgk .or. l .le. idn(idnm)) then
!           rsum2 = rsum2 + evp(l-1)
!           print *,' rsum1=',rsum1,' rsum2=',rsum2,' l=',l,' qa=',qa(1)&
!    &,   ' evp=',evp(l-1)
!         else
!           rsum1 = rsum1 + rnf(l-1)
!           print *,' rsum1=',rsum1,' rsum2=',rsum2,' l=',l,' rnf=',    &
!     &     rnf(l-1)
!         endif

        enddo                      ! end of the l loop of downdraft !

        tx1 = 0.0

        dof = qa(1)
!
!       print *,' dof=',dof,' rntp=',rntp,' rnb=',rnb
!       print *,' total=',(rsum1+dof+rntp+rnb)

      endif                       ! skpdd endif
!

      rnn(kd) = rntp
      tx1     = evp(kd)
      tx2     = rntp + rnb + dof

!     if (lprnt) print *,' tx2=',tx2
      ii = idh
      if (ii .ge. kd1+1) then
         rnn(kd)   = rnn(kd) + rnf(kd)
         tx2       = tx2 + rnf(kd)
         rnn(ii-1) = 0.0
         tx1       = evp(ii-1)
      endif
!     if (lprnt) print *,' tx2=',tx2,' idnm=',idnm,' idn=',idn(idnm)
      do l=kd,k
        ii = idh

        if (l .gt. kd1 .and. l .lt. ii) then
          rnn(l-1) = rnf(l-1)
          tx2      = tx2 + rnn(l-1)
        elseif (l .ge. ii .and. l .lt. idn(idnm)) then
          rnn(l)   = rns(l)
          tx2      = tx2 + rnn(l)
          tx1      = tx1 + evp(l)
        elseif (l .ge. idn(idnm)) then
          etd(l+1) = 0.0
          hod(l+1) = 0.0
          qod(l+1) = 0.0
          evp(l)   = 0.0
          rnn(l)   = rnf(l) + rns(l)
          tx2      = tx2    + rnn(l)
        endif
!     if (lprnt) print *,' tx2=',tx2,' l=',l,' rnn=',rnn(l)
      enddo
!
!      for downdraft case the rain is that falls thru the bottom

      l = kbl

      rnn(l)    = rnn(l) + rnb
      cldfrd(l) = tx5

!
!     caution !! below is an adjustment to rain flux to maintain
!                conservation of precip!

!
!     if (lprnt) print *,' train=',train,' tx2=',tx2,' tx1=',tx1

      if (tx1 .gt. 0.0) then
        tx1 = (train - tx2) / tx1
      else
        tx1 = 0.0
      endif

      do l=kd,k
        evp(l) = evp(l) * tx1
      enddo
!
!***********************************************************************
!***********************************************************************

      return
      end

      subroutine qsatcn(tt,p,q,dqdt)
!     subroutine qsatcn(tt,p,q,dqdt,lprnt)

      use machine , only : kind_phys
      use funcphys , only : fpvs
      use physcons, rv => con_rv, cvap => con_cvap, cliq => con_cliq    &
     &,             csol => con_csol, ttp => con_ttp, hvap => con_hvap  &
     &,             hfus => con_hfus, eps => con_eps, epsm1 => con_epsm1
      implicit none
!
      real(kind=kind_phys) tt, p, q, dqdt
!
      real(kind=kind_phys) rvi, facw, faci, hsub, tmix, den
      real(kind=kind_phys) zero,one,one_m10
      parameter (rvi=1.0/rv)
      parameter (facw=cvap-cliq, faci=cvap-csol)
      parameter (hsub=hvap+hfus, tmix=ttp-20.0, den=1.0/(ttp-tmix))
      parameter (zero=0.,one=1.,one_m10=1.e-10)
!     logical lprnt
!
      real(kind=kind_phys) es, d, hlorv, w
!
!     es    = 10.0 * fpvs(tt)                ! fpvs is in centibars!
      es    = 0.01 * fpvs(tt)                ! fpvs is in pascals!
      d     = 1.0 / max(p+epsm1*es,one_m10)
!
      q     = min(eps*es*d, one)
!
      w     = max(zero, min(one, (tt - tmix)*den))
      hlorv = ( w      * (hvap + facw * (tt-ttp))                       &
     &       + (1.0-w) * (hsub + faci * (tt-ttp)) ) * rvi
      dqdt  = p * q * hlorv *  d / (tt*tt)
!
      return
      end

      subroutine angrad( pres, alm,  al2, tla, prb, wfn, ufn)
      use machine , only : kind_phys
      use module_ras , only : refp, refr, tlac, plac, tlbpl, drdp, almax
      implicit none

      real(kind=kind_phys) pres                                         &
     &,                    alm,  al2,  tla,  tem                        &
     &,                    prb,  wfn,  ufn
!
      integer i
!
      if (tla .lt. 0.0) then
          if (pres .le. plac(1)) then
            tla = tlac(1)
          elseif (pres .le. plac(2)) then
            tla = tlac(2) + (pres-plac(2))*tlbpl(1)
          elseif (pres .le. plac(3)) then
            tla = tlac(3) + (pres-plac(3))*tlbpl(2)
          elseif (pres .le. plac(4)) then
            tla = tlac(4) + (pres-plac(4))*tlbpl(3)
          elseif (pres .le. plac(5)) then
            tla = tlac(5) + (pres-plac(5))*tlbpl(4)
          elseif (pres .le. plac(6)) then
            tla = tlac(6) + (pres-plac(6))*tlbpl(5)
          elseif (pres .le. plac(7)) then
            tla = tlac(7) + (pres-plac(7))*tlbpl(6)
          elseif (pres .le. plac(8)) then
            tla = tlac(8) + (pres-plac(8))*tlbpl(7)
          else
            tla = tlac(8)
          endif
      endif
        if (pres .ge. refp(1)) then
          tem = refr(1)
        elseif (pres .ge. refp(2)) then
          tem = refr(1) + (pres-refp(1)) * drdp(1)
        elseif (pres .ge. refp(3)) then
          tem = refr(2) + (pres-refp(2)) * drdp(2)
        elseif (pres .ge. refp(4)) then
          tem = refr(3) + (pres-refp(3)) * drdp(3)
        elseif (pres .ge. refp(5)) then
          tem = refr(4) + (pres-refp(4)) * drdp(4)
        elseif (pres .ge. refp(6)) then
          tem = refr(5) + (pres-refp(5)) * drdp(5)
        else
          tem = refr(6)
        endif
!
        tem = 2.0e-4 / tem
        al2 = min(4.0*tem, max(alm, tem))
!
      return
      end
      subroutine setqrp
      use machine , only : kind_phys
      use module_ras , only : nqrp,c1xqrp,c2xqrp,tbqrp,tbqra,tbqrb
      implicit none

      real(kind=kind_phys) tem2,tem1,x,xinc,xmax,xmin
      integer jx
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     xmin   = 1.0e-6
      xmin   = 0.0
      xmax   = 5.0
      xinc   = (xmax-xmin)/(nqrp-1)
      c2xqrp = 1.0/xinc
      c1xqrp = 1.0 - xmin*c2xqrp
      tem1   = 0.001 ** 0.2046
      tem2   = 0.001 ** 0.525
      do jx=1,nqrp
        x         = xmin + (jx-1)*xinc
        tbqrp(jx) =        x ** 0.1364
        tbqra(jx) = tem1 * x ** 0.2046
        tbqrb(jx) = tem2 * x ** 0.525
      enddo    
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
      function qrpf(qrp)
!
      use machine , only : kind_phys
      use module_ras , only : nqrp,c1xqrp,c2xqrp,tbqrp,tbqra,tbqrb
      implicit none

      real(kind=kind_phys) qrp, qrpf, xj, real_nqrp, one
      parameter (one=1.0)
      integer jx
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      real_nqrp = real(nqrp)
      xj   = min(max(c1xqrp+c2xqrp*qrp,one),real_nqrp)
!     xj   = min(max(c1xqrp+c2xqrp*qrp,one),float(nqrp))
      jx   = min(xj,nqrp-one)
      qrpf = tbqrp(jx)  + (xj-jx) * (tbqrp(jx+1)-tbqrp(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
      subroutine qrabf(qrp,qraf,qrbf)
      use machine , only : kind_phys
      use module_ras , only : nqrp,c1xqrp,c2xqrp,tbqrp,tbqra,tbqrb
      implicit none
!
      real(kind=kind_phys) qrp, qraf, qrbf, xj, real_nqrp, one
      parameter (one=1.0)
      integer jx
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      real_nqrp=real(nqrp)
      xj   = min(max(c1xqrp+c2xqrp*qrp,one),real_nqrp)
      jx   = min(xj,nqrp-one)
      xj   = xj - jx
      qraf = tbqra(jx)  + xj * (tbqra(jx+1)-tbqra(jx))
      qrbf = tbqrb(jx)  + xj * (tbqrb(jx+1)-tbqrb(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
      subroutine setvtp
      use machine , only : kind_phys
      use module_ras , only : nvtp,c1xvtp,c2xvtp,tbvtp
      implicit none

      real(kind=kind_phys) vtpexp,xinc,x,xmax,xmin
      integer jx
      parameter(vtpexp=-0.3636)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      xmin   = 0.05
      xmax   = 1.5
      xinc   = (xmax-xmin)/(nvtp-1)
      c2xvtp = 1.0/xinc
      c1xvtp = 1.0 - xmin*c2xvtp
      do jx=1,nvtp
        x         = xmin + (jx-1)*xinc
        tbvtp(jx) =        x ** vtpexp
      enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
      function vtpf(ror)
!
      use machine , only : kind_phys
      use module_ras , only : nvtp,c1xvtp,c2xvtp,tbvtp
      implicit none
      real(kind=kind_phys) ror, vtpf, xj, real_nvtp, one
      parameter (one=1.0)
      integer jx
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      real_nvtp = real(nvtp)
      xj   = min(max(c1xvtp+c2xvtp*ror,one),real_nvtp)
      jx   = min(xj,nvtp-one)
      vtpf = tbvtp(jx)  + (xj-jx) * (tbvtp(jx+1)-tbvtp(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
      function clf(prate)
!
      use machine , only : kind_phys
      implicit none
      real(kind=kind_phys) prate, clf
!
      real (kind=kind_phys), parameter :: ccf1=0.30, ccf2=0.09          &
     &,                                   ccf3=0.04, ccf4=0.01          &
     &,                                   pr1=1.0,   pr2=5.0            &
     &,                                   pr3=20.0
!
      if (prate .lt. pr1) then
        clf = ccf1
      elseif (prate .lt. pr2) then
        clf = ccf2
      elseif (prate .lt. pr3) then
        clf = ccf3
      else
        clf = ccf4
      endif
!
      return
      end
