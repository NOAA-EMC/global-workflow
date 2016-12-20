      subroutine sascnv(im,ix,km,jcap,delt,delp,prslp,psp,phil,ql,
!     subroutine sascnv(im,ix,km,jcap,delt,del,prsl,phil,ql,
     &       q1,t1,u1,v1,cldwrk,rn,kbot,ktop,kuo,slimsk,
     &       dot,xkt2,ncloud,ud_mf,dd_mf,dt_mf,cnvw,cnvc)
! hchuang code change [r1l]
!    &       dot,xkt2,ncloud)
!
! 10/14/2008 ho-chun huang the cloudmass flux fields was added by jongil
!
!  for cloud water version
!     parameter(ncloud=0)
!     subroutine sascnv(km,jcap,delt,del,sl,slk,ps,ql,
!    &       q1,t1,u1,v1,cldwrk,rn,kbot,ktop,kuo,slimsk,
!    &       dot,xkt2,ncloud)
!
      use machine , only : kind_phys
      use funcphys , only : fpvs
      use physcons, grav => con_g, cp => con_cp, hvap => con_hvap
     &,             rv => con_rv, fv => con_fvirt, t0c => con_t0c
     &,             cvap => con_cvap, cliq => con_cliq
     &,             eps => con_eps, epsm1 => con_epsm1
      implicit none
!
!
      integer            im, ix,  km, jcap, ncloud,
     &                   kbot(im), ktop(im), kuo(im)
      real(kind=kind_phys) delt
      real(kind=kind_phys) psp(im),    delp(ix,km), prslp(ix,km)
      real(kind=kind_phys) ps(im),     del(ix,km),  prsl(ix,km),
!     real(kind=kind_phys)             del(ix,km),  prsl(ix,km),
     &                     ql(ix,km,2),q1(ix,km),   t1(ix,km),
     &                     u1(ix,km),  v1(ix,km),
     &                     cldwrk(im), rn(im),      slimsk(im),
     &                     dot(ix,km), xkt2(im),    phil(ix,km)
     &,                    cnvw(ix,km),cnvc(ix,km)
! hchuang code change [+1l] mass flux output
     &,                    ud_mf(im,km),dd_mf(im,km),dt_mf(im,km)
!
      integer              i, indx, jmn, k, knumb, latd, lond, km1
!
      real(kind=kind_phys) adw,     alpha,   alphal,  alphas,
     &                     aup,     beta,    betal,   betas,
     &                     c0,      dellat,  delta,
     &                     desdt,   deta,    detad,   dg,
     &                     dh,      dhh,     dp,
     &                     dq,      dqsdp,   dqsdt,   dt,
     &                     dt2,     dtmax,   dtmin,   dv1,
     &                     dv1q,    dv2,     dv2q,    dv1u,
     &                     dv1v,    dv2u,    dv2v,    dv3u,
     &                     dv3v,    dv3,     dv3q,    dvq1,
     &                     dz,      dz1,     e1,      edtmax,
     &                     edtmaxl, edtmaxs, el2orc,  elocp,
     &                     es,      etah,
     &                     evef,    evfact,  evfactl, fact1,
     &                     fact2,   factor,  fjcap,   fkm,
     &                     fuv,     g,       gamma,   onemf,
     &                     onemfu,  pdetrn,  pdpdwn,  pprime,
     &                     qc,      qlk,     qrch,    qs,
     &                     rain,    rfact,   shear,   tem1,
     &                     tem2,    val,     val1,
     &                     val2,    w1,      w1l,     w1s,
     &                     w2,      w2l,     w2s,     w3,
     &                     w3l,     w3s,     w4,      w4l,
     &                     w4s,     xdby,    xpw,     xpwd,
     &                     xqc,     xqrch,   xlambu,  mbdt,
     &                     tem
!
!
      integer              jmin(im), kb(im), kbcon(im), kbdtr(im),
     &                     kt2(im),  ktcon(im), lmin(im),
     &                     kbm(im),  kbmax(im), kmax(im)
!
      real(kind=kind_phys) aa1(im),     acrt(im),   acrtfct(im),
     &                     delhbar(im), delq(im),   delq2(im),
     &                     delqbar(im), delqev(im), deltbar(im),
     &                     deltv(im),   dtconv(im), edt(im),
     &                     edto(im),    edtx(im),   fld(im),
     &                     hcdo(im),    hkbo(im),   hmax(im),
     &                     hmin(im),    hsbar(im),  ucdo(im),
     &                     ukbo(im),    vcdo(im),   vkbo(im),
     &                     pbcdif(im),  pdot(im),   po(im,km),
     &                                  pwavo(im),  pwevo(im),
!    &                     psfc(im),    pwavo(im),  pwevo(im),
     &                     qcdo(im),    qcond(im),  qevap(im),
     &                     qkbo(im),    rntot(im),  vshear(im),
     &                     xaa0(im),    xhcd(im),   xhkb(im),
     &                     xk(im),      xlamb(im),  xlamd(im),
     &                     xmb(im),     xmbmax(im), xpwav(im),
     &                     xpwev(im),   xqcd(im),   xqkb(im)
cc
c  physical parameters
      parameter(g=grav)
      parameter(elocp=hvap/cp,
     &          el2orc=hvap*hvap/(rv*cp))
      parameter(c0=.002,delta=fv)
      parameter(fact1=(cvap-cliq)/rv,fact2=hvap/rv-fact1*t0c)
!     real      terr
!     parameter(terr=0.)
c  local variables and arrays
      real(kind=kind_phys) pfld(im,km),    to(im,km),     qo(im,km),
     &                     uo(im,km),      vo(im,km),     qeso(im,km)
c  cloud water
      real(kind=kind_phys) qlko_ktcon(im), dellal(im),    tvo(im,km),
     &                     dbyo(im,km),    zo(im,km),     sumz(im,km),
     &                     sumh(im,km),    heo(im,km),    heso(im,km),
     &                     qrcd(im,km),    dellah(im,km), dellaq(im,km),
     &                     dellau(im,km),  dellav(im,km), hcko(im,km),
     &                     ucko(im,km),    vcko(im,km),   qcko(im,km),
     &                     eta(im,km),     etau(im,km),   etad(im,km),
     &                     qrcdo(im,km),   pwo(im,km),    pwdo(im,km),
     &                     rhbar(im),      tx1(im),       cnvwt(im,km)
!
      logical totflg, cnvflg(im), dwnflg(im), dwnflg2(im), flg(im)
!
      real(kind=kind_phys) pcrit(15), acritt(15), acrit(15)
cmy      save pcrit, acritt
      data pcrit/850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,
     &           350.,300.,250.,200.,150./
      data acritt/.0633,.0445,.0553,.0664,.075,.1082,.1521,.2216,
     &           .3151,.3677,.41,.5255,.7663,1.1686,1.6851/
c  gdas derived acrit
c     data acritt/.203,.515,.521,.566,.625,.665,.659,.688,
c    &            .743,.813,.886,.947,1.138,1.377,1.896/
cc
      real(kind=kind_phys) tf, tcr, tcrf
      parameter (tf=233.16, tcr=263.16, tcrf=1.0/(tcr-tf)) ! from lord(1978)
!
!     parameter (tf=258.16, tcr=273.16, tcrf=1.0/(tcr-tf))
!
      real(kind=kind_phys), parameter :: cons_0=0.0
c
c--------------------------------------------------------------------
!
!************************************************************************
!     convert input pa terms to cb terms  -- moorthi
      ps   = psp   * 0.001
      prsl = prslp * 0.001
      del  = delp  * 0.001
!************************************************************************
!
      km1 = km - 1
c  initialize arrays
c
      do i=1,im
        rn(i)=0.
        kbot(i)=km+1
        ktop(i)=0
!       kuo(i)=0
        cnvflg(i) = .true.
        dtconv(i) = 3600.
        cldwrk(i) = 0.
        pdot(i) = 0.
        kt2(i) = 0
        qlko_ktcon(i) = 0.
        dellal(i) = 0.
      enddo
      do k = 1, km
        do i = 1, im
          cnvw(i,k) = 0.
          cnvc(i,k) = 0.
        enddo
      enddo
! hchuang code change [+7l]
      do k = 1, km
        do i=1,im
          ud_mf(i,k) = 0.
          dd_mf(i,k) = 0.
          dt_mf(i,k) = 0.
        enddo
      enddo
!!
      do k = 1, 15
        acrit(k) = acritt(k) * (975. - pcrit(k))
      enddo
      dt2 = delt
      val   =         1200.
      dtmin = max(dt2, val )
      val   =         3600.
      dtmax = max(dt2, val )
c  model tunable parameters are all here
      mbdt    = 10.
      edtmaxl = .3
      edtmaxs = .3
      alphal  = .5
      alphas  = .5
      betal   = .15
      betas   = .15
      betal   = .05
      betas   = .05
c     evef    = 0.07
      evfact  = 0.3
      evfactl = 0.3
      pdpdwn  = 0.
      pdetrn  = 200.
      xlambu  = 1.e-4
      fjcap   = (float(jcap) / 126.) ** 2
      val     =           1.
      fjcap   = max(fjcap,val)
      fkm     = (float(km) / 28.) ** 2
      fkm     = max(fkm,val)
      w1l     = -8.e-3
      w2l     = -4.e-2
      w3l     = -5.e-3
      w4l     = -5.e-4
      w1s     = -2.e-4
      w2s     = -2.e-3
      w3s     = -1.e-3
      w4s     = -2.e-5
ccccc if(im.eq.384) then
        latd  = 92
        lond  = 189
ccccc elseif(im.eq.768) then
ccccc   latd = 80
ccccc else
ccccc   latd = 0
ccccc endif
c
c  define top layer for search of the downdraft originating layer
c  and the maximum thetae for updraft
c
      do i=1,im
        kbmax(i) = km
        kbm(i)   = km
        kmax(i)  = km
        tx1(i)   = 1.0 / ps(i)
      enddo
!
      do k = 1, km
        do i=1,im
          if (prsl(i,k)*tx1(i) .gt. 0.45) kbmax(i) = k + 1
          if (prsl(i,k)*tx1(i) .gt. 0.70) kbm(i)   = k + 1
          if (prsl(i,k)*tx1(i) .gt. 0.04) kmax(i)  = k + 1
        enddo
      enddo
      do i=1,im
        kmax(i)  = min(km,kmax(i))
        kbmax(i) = min(kbmax(i),kmax(i))
        kbm(i)   = min(kbm(i),kmax(i))
      enddo
c
c   convert surface pressure to mb from cb
c
!!
      do k = 1, km
        do i=1,im
          if (k .le. kmax(i)) then
            pfld(i,k) = prsl(i,k) * 10.0
            pwo(i,k)  = 0.
            pwdo(i,k) = 0.
            to(i,k)   = t1(i,k)
            qo(i,k)   = q1(i,k)
            uo(i,k)   = u1(i,k)
            vo(i,k)   = v1(i,k)
            dbyo(i,k) = 0.
            sumz(i,k) = 0.
            sumh(i,k) = 0.
            cnvwt(i,k)= 0.
          endif
        enddo
      enddo
c
c  column variables
c  p is pressure of the layer (mb)
c  t is temperature at t-dt (k)..tn
c  q is mixing ratio at t-dt (kg/kg)..qn
c  to is temperature at t+dt (k)... this is after advection and turbulan
c  qo is mixing ratio at t+dt (kg/kg)..q1
c
      do k = 1, km
        do i=1,im
          if (k .le. kmax(i)) then
!jfe        qeso(i,k) = 10. * fpvs(t1(i,k))
!
            qeso(i,k) = 0.01 * fpvs(t1(i,k))      ! fpvs is in pa
!
            qeso(i,k) = eps * qeso(i,k) / (pfld(i,k) + epsm1*qeso(i,k))
            val1      =             1.e-8
            qeso(i,k) = max(qeso(i,k), val1)
            val2      =           1.e-10
            qo(i,k)   = max(qo(i,k), val2 )
c           qo(i,k)   = min(qo(i,k),qeso(i,k))
            tvo(i,k)  = to(i,k) + delta * to(i,k) * qo(i,k)
          endif
        enddo
      enddo
c
c  hydrostatic height assume zero terr
c
      do k = 1, km
        do i=1,im
          zo(i,k) = phil(i,k) / g
        enddo
      enddo
c  compute moist static energy
      do k = 1, km
        do i=1,im
          if (k .le. kmax(i)) then
!           tem       = g * zo(i,k) + cp * to(i,k)
            tem       = phil(i,k) + cp * to(i,k)
            heo(i,k)  = tem  + hvap * qo(i,k)
            heso(i,k) = tem  + hvap * qeso(i,k)
c           heo(i,k)  = min(heo(i,k),heso(i,k))
          endif
        enddo
      enddo
c
c  determine level with largest moist static energy
c  this is the level where updraft starts
c
      do i=1,im
        hmax(i) = heo(i,1)
        kb(i) = 1
      enddo
!!
      do k = 2, km
        do i=1,im
          if (k .le. kbm(i)) then
            if(heo(i,k).gt.hmax(i).and.cnvflg(i)) then
              kb(i)   = k
              hmax(i) = heo(i,k)
            endif
          endif
        enddo
      enddo
c     do k = 1, kmax - 1
c         tol(k) = .5 * (to(i,k) + to(i,k+1))
c         qol(k) = .5 * (qo(i,k) + qo(i,k+1))
c         qesol(i,k) = .5 * (qeso(i,k) + qeso(i,k+1))
c         heol(i,k) = .5 * (heo(i,k) + heo(i,k+1))
c         hesol(i,k) = .5 * (heso(i,k) + heso(i,k+1))
c     enddo
      do k = 1, km1
        do i=1,im
          if (k .le. kmax(i)-1) then
            dz      = .5 * (zo(i,k+1) - zo(i,k))
            dp      = .5 * (pfld(i,k+1) - pfld(i,k))
!jfe        es      = 10. * fpvs(to(i,k+1))
!
            es      = 0.01 * fpvs(to(i,k+1))      ! fpvs is in pa
!
            pprime  = pfld(i,k+1) + epsm1 * es
            qs      = eps * es / pprime
            dqsdp   = - qs / pprime
            desdt   = es * (fact1 / to(i,k+1) + fact2 / (to(i,k+1)**2))
            dqsdt   = qs * pfld(i,k+1) * desdt / (es * pprime)
            gamma   = el2orc * qeso(i,k+1) / (to(i,k+1)**2)
            dt      = (g * dz + hvap * dqsdp * dp) / (cp * (1. + gamma))
            dq      = dqsdt * dt + dqsdp * dp
            to(i,k) = to(i,k+1) + dt
            qo(i,k) = qo(i,k+1) + dq
            po(i,k) = .5 * (pfld(i,k) + pfld(i,k+1))
          endif
        enddo
      enddo
!
      do k = 1, km1
        do i=1,im
          if (k .le. kmax(i)-1) then
!jfe        qeso(i,k) = 10. * fpvs(to(i,k))
!
            qeso(i,k) = 0.01 * fpvs(to(i,k))      ! fpvs is in pa
!
            qeso(i,k) = eps * qeso(i,k) / (po(i,k) + epsm1*qeso(i,k))
            val1      =             1.e-8
            qeso(i,k) = max(qeso(i,k), val1)
            val2      =           1.e-10
            qo(i,k)   = max(qo(i,k), val2 )
c           qo(i,k)   = min(qo(i,k),qeso(i,k))
            heo(i,k)  = .5 * g * (zo(i,k) + zo(i,k+1)) +
     &                  cp * to(i,k) + hvap * qo(i,k)
            heso(i,k) = .5 * g * (zo(i,k) + zo(i,k+1)) +
     &                  cp * to(i,k) + hvap * qeso(i,k)
            uo(i,k)   = .5 * (uo(i,k) + uo(i,k+1))
            vo(i,k)   = .5 * (vo(i,k) + vo(i,k+1))
          endif
        enddo
      enddo
c     k = kmax
c       heo(i,k) = heo(i,k)
c       hesol(k) = heso(i,k)
c      if(lat.eq.latd.and.lon.eq.lond.and.cnvflg(i)) then
c        print *, '   heo ='
c        print 6001, (heo(i,k),k=1,kmax)
c        print *, '   heso ='
c        print 6001, (heso(i,k),k=1,kmax)
c        print *, '   to ='
c        print 6002, (to(i,k)-273.16,k=1,kmax)
c        print *, '   qo ='
c        print 6003, (qo(i,k),k=1,kmax)
c        print *, '   qso ='
c        print 6003, (qeso(i,k),k=1,kmax)
c      endif
c
c  look for convective cloud base as the level of free convection
c
      do i=1,im
        if(cnvflg(i)) then
          indx    = kb(i)
          hkbo(i) = heo(i,indx)
          qkbo(i) = qo(i,indx)
          ukbo(i) = uo(i,indx)
          vkbo(i) = vo(i,indx)
        endif
        flg(i)    = cnvflg(i)
        kbcon(i)  = kmax(i)
      enddo
!!
      do k = 1, km
        do i=1,im
          if (k .le. kbmax(i)) then
            if(flg(i).and.k.gt.kb(i)) then
              hsbar(i)   = heso(i,k)
              if(hkbo(i).gt.hsbar(i)) then
                flg(i)   = .false.
                kbcon(i) = k
              endif
            endif
          endif
        enddo
      enddo
      do i=1,im
        if(cnvflg(i)) then
          pbcdif(i) = -pfld(i,kbcon(i)) + pfld(i,kb(i))
!         pdot(i)   = 10.* dot(i,kbcon(i))
          pdot(i)   = 0.01 * dot(i,kbcon(i))  ! now dot is in pa/s
          if(pbcdif(i).gt.150.)    cnvflg(i) = .false.
          if(kbcon(i).eq.kmax(i))  cnvflg(i) = .false.
        endif
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
c  found lfc, can define rest of variables
 6001 format(2x,-2p10f12.2)
 6002 format(2x,10f12.2)
 6003 format(2x,3p10f12.2)
c
c  determine entrainment rate between kb and kbcon
c
      do i = 1, im
        alpha = alphas
        if(slimsk(i).eq.1.) alpha = alphal
        if(cnvflg(i)) then
          if(kb(i).eq.1) then
            dz = .5 * (zo(i,kbcon(i)) + zo(i,kbcon(i)-1)) - zo(i,1)
          else
            dz = .5 * (zo(i,kbcon(i)) + zo(i,kbcon(i)-1))
     &         - .5 * (zo(i,kb(i)) + zo(i,kb(i)-1))
          endif
          if(kbcon(i).ne.kb(i)) then
            xlamb(i) = - log(alpha) / dz
          else
            xlamb(i) = 0.
          endif
        endif
      enddo
c  determine updraft mass flux
      do k = 1, km
        do i = 1, im
          if (k .le. kmax(i) .and. cnvflg(i)) then
            eta(i,k)  = 1.
            etau(i,k) = 1.
          endif
        enddo
      enddo
      do k = km1, 2, -1
        do i = 1, im
          if (k .le. kbmax(i)) then
            if(cnvflg(i).and.k.lt.kbcon(i).and.k.ge.kb(i)) then
              dz        = .5 * (zo(i,k+1) - zo(i,k-1))
              eta(i,k)  = eta(i,k+1) * exp(-xlamb(i) * dz)
              etau(i,k) = eta(i,k)
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i).and.kb(i).eq.1.and.kbcon(i).gt.1) then
          dz = .5 * (zo(i,2) - zo(i,1))
          eta(i,1) = eta(i,2) * exp(-xlamb(i) * dz)
          etau(i,1) = eta(i,1)
        endif
      enddo
c
c  work up updraft cloud properties
c
      do i = 1, im
        if(cnvflg(i)) then
          indx         = kb(i)
          hcko(i,indx) = hkbo(i)
          qcko(i,indx) = qkbo(i)
          ucko(i,indx) = ukbo(i)
          vcko(i,indx) = vkbo(i)
          pwavo(i)     = 0.
        endif
      enddo
c
c  cloud property below cloud base is modified by the entrainment proces
c
      do k = 2, km1
        do i = 1, im
          if (k .le. kmax(i)-1) then
            if(cnvflg(i).and.k.gt.kb(i).and.k.le.kbcon(i)) then
              factor = eta(i,k-1) / eta(i,k)
              onemf = 1. - factor
              hcko(i,k) = factor * hcko(i,k-1) + onemf *
     &                    .5 * (heo(i,k) + heo(i,k+1))
              ucko(i,k) = factor * ucko(i,k-1) + onemf *
     &                    .5 * (uo(i,k) + uo(i,k+1))
              vcko(i,k) = factor * vcko(i,k-1) + onemf *
     &                    .5 * (vo(i,k) + vo(i,k+1))
              dbyo(i,k) = hcko(i,k) - heso(i,k)
            endif
            if(cnvflg(i).and.k.gt.kbcon(i)) then
              hcko(i,k) = hcko(i,k-1)
              ucko(i,k) = ucko(i,k-1)
              vcko(i,k) = vcko(i,k-1)
              dbyo(i,k) = hcko(i,k) - heso(i,k)
            endif
          endif
        enddo
      enddo
c  determine cloud top
      do i = 1, im
        flg(i) = cnvflg(i)
        ktcon(i) = 1
      enddo
c     do k = 2, kmax
c       kk = kmax - k + 1
c         if(dbyo(i,kk).ge.0..and.flg(i).and.kk.gt.kbcon(i)) then
c           ktcon(i) = kk + 1
c           flg(i) = .false.
c         endif
c     enddo
      do k = 2, km
        do i = 1, im
          if (k .le. kmax(i)) then
            if(dbyo(i,k).lt.0..and.flg(i).and.k.gt.kbcon(i)) then
              ktcon(i) = k
              flg(i) = .false.
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i).and.(pfld(i,kbcon(i)) - pfld(i,ktcon(i))).lt.150.)
     &  cnvflg(i) = .false.
      enddo
      totflg = .true.
      do i = 1, im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
c
c  search for downdraft originating level above theta-e minimum
c
      do i = 1, im
        hmin(i) = heo(i,kbcon(i))
        lmin(i) = kbmax(i)
        jmin(i) = kbmax(i)
      enddo
      do i = 1, im
        do k = kbcon(i), kbmax(i)
          if(heo(i,k).lt.hmin(i).and.cnvflg(i)) then
            lmin(i) = k + 1
            hmin(i) = heo(i,k)
          endif
        enddo
      enddo
c
c  make sure that jmin(i) is within the cloud
c
      do i = 1, im
        if(cnvflg(i)) then
          jmin(i) = min(lmin(i),ktcon(i)-1)
          xmbmax(i) = .1
          jmin(i) = max(jmin(i),kbcon(i)+1)
        endif
      enddo
c
c  entraining cloud
c
      do k = 2, km1
        do i = 1, im
          if (k .le. kmax(i)-1) then
            if(cnvflg(i).and.k.gt.jmin(i).and.k.le.ktcon(i)) then
              sumz(i,k) = sumz(i,k-1) + .5 * (zo(i,k+1) - zo(i,k-1))
              sumh(i,k) = sumh(i,k-1) + .5 * (zo(i,k+1) - zo(i,k-1))
     &                  * heo(i,k)
            endif
          endif
        enddo
      enddo
!!
      do i = 1, im
        if(cnvflg(i)) then
c         call random_number(xkt2)
c         call srand(fhour)
c         xkt2(i) = rand()
          kt2(i) = nint(xkt2(i)*float(ktcon(i)-jmin(i))-.5)+jmin(i)+1
!         kt2(i) = nint(sqrt(xkt2(i))*float(ktcon(i)-jmin(i))-.5) + jmin(i) + 1
c         kt2(i) = nint(ranf() *float(ktcon(i)-jmin(i))-.5) + jmin(i) + 1
          tem1 = (hcko(i,jmin(i)) - heso(i,kt2(i)))
          tem2 = (sumz(i,kt2(i)) * heso(i,kt2(i)) - sumh(i,kt2(i)))
          if (abs(tem2) .gt. 0.000001) then
            xlamb(i) = tem1 / tem2
          else
            cnvflg(i) = .false.
          endif
!         xlamb(i) = (hcko(i,jmin(i)) - heso(i,kt2(i)))
!    &          / (sumz(i,kt2(i)) * heso(i,kt2(i)) - sumh(i,kt2(i)))
          xlamb(i) = max(xlamb(i),cons_0)
          xlamb(i) = min(xlamb(i),2.3/sumz(i,kt2(i)))
        endif
      enddo
!!
      do i = 1, im
       dwnflg(i)  = cnvflg(i)
       dwnflg2(i) = cnvflg(i)
       if(cnvflg(i)) then
        if(kt2(i).ge.ktcon(i)) dwnflg(i) = .false.
      if(xlamb(i).le.1.e-30.or.hcko(i,jmin(i))-heso(i,kt2(i)).le.1.e-30)
     &  dwnflg(i) = .false.
        do k = jmin(i), kt2(i)
          if(dwnflg(i).and.heo(i,k).gt.heso(i,kt2(i))) dwnflg(i)=.false.
        enddo
c       if(cnvflg(i).and.(pfld(kbcon(i))-pfld(ktcon(i))).gt.pdetrn)
c    &     dwnflg(i)=.false.
        if(cnvflg(i).and.(pfld(i,kbcon(i))-pfld(i,ktcon(i))).lt.pdpdwn)
     &     dwnflg2(i)=.false.
       endif
      enddo
!!
      do k = 2, km1
        do i = 1, im
          if (k .le. kmax(i)-1) then
            if(dwnflg(i).and.k.gt.jmin(i).and.k.le.kt2(i)) then
              dz        = .5 * (zo(i,k+1) - zo(i,k-1))
c             eta(i,k)  = eta(i,k-1) * exp( xlamb(i) * dz)
c  to simplify matter, we will take the linear approach here
c
              eta(i,k)  = eta(i,k-1) * (1. + xlamb(i) * dz)
              etau(i,k) = etau(i,k-1) * (1. + (xlamb(i)+xlambu) * dz)
            endif
          endif
        enddo
      enddo
!!
      do k = 2, km1
        do i = 1, im
          if (k .le. kmax(i)-1) then
c           if(.not.dwnflg(i).and.k.gt.jmin(i).and.k.le.kt2(i)) then
            if(.not.dwnflg(i).and.k.gt.jmin(i).and.k.le.ktcon(i)) then
              dz        = .5 * (zo(i,k+1) - zo(i,k-1))
              etau(i,k) = etau(i,k-1) * (1. + xlambu * dz)
            endif
          endif
        enddo
      enddo
c      if(lat.eq.latd.and.lon.eq.lond.and.cnvflg(i)) then
c        print *, ' lmin(i), kt2(i)=', lmin(i), kt2(i)
c        print *, ' kbot, ktop, jmin(i) =', kbcon(i), ktcon(i), jmin(i)
c      endif
c     if(lat.eq.latd.and.lon.eq.lond) then
c       print *, ' xlamb =', xlamb
c       print *, ' eta =', (eta(k),k=1,kt2(i))
c       print *, ' etau =', (etau(i,k),k=1,kt2(i))
c       print *, ' hcko =', (hcko(i,k),k=1,kt2(i))
c       print *, ' sumz =', (sumz(i,k),k=1,kt2(i))
c       print *, ' sumh =', (sumh(i,k),k=1,kt2(i))
c     endif
      do i = 1, im
        if(dwnflg(i)) then
          ktcon(i) = kt2(i)
        endif
      enddo
c
c  cloud property above cloud base is modified by the detrainment process
c
      do k = 2, km1
        do i = 1, im
          if (k .le. kmax(i)-1) then
cjfe
            if(cnvflg(i).and.k.gt.kbcon(i).and.k.le.ktcon(i)) then
cjfe      if(k.gt.kbcon(i).and.k.le.ktcon(i)) then
              factor    = eta(i,k-1) / eta(i,k)
              onemf     = 1. - factor
              fuv       = etau(i,k-1) / etau(i,k)
              onemfu    = 1. - fuv
              hcko(i,k) = factor * hcko(i,k-1) + onemf *
     &                    .5 * (heo(i,k) + heo(i,k+1))
              ucko(i,k) = fuv * ucko(i,k-1) + onemfu *
     &                    .5 * (uo(i,k) + uo(i,k+1))
              vcko(i,k) = fuv * vcko(i,k-1) + onemfu *
     &                    .5 * (vo(i,k) + vo(i,k+1))
              dbyo(i,k) = hcko(i,k) - heso(i,k)
            endif
          endif
        enddo
      enddo
c      if(lat.eq.latd.and.lon.eq.lond.and.cnvflg(i)) then
c        print *, ' ucko=', (ucko(i,k),k=kbcon(i)+1,ktcon(i))
c        print *, ' uenv=', (.5*(uo(i,k)+uo(i,k-1)),k=kbcon(i)+1,ktcon(i))
c      endif
      do i = 1, im
        if(cnvflg(i).and.dwnflg2(i).and.jmin(i).le.kbcon(i))
     &     then
          cnvflg(i) = .false.
          dwnflg(i) = .false.
          dwnflg2(i) = .false.
        endif
      enddo
!!
      totflg = .true.
      do i = 1, im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
c
c  compute cloud moisture property and precipitation
c
      do i = 1, im
          aa1(i) = 0.
          rhbar(i) = 0.
      enddo
      do k = 1, km
        do i = 1, im
          if (k .le. kmax(i)) then
            if(cnvflg(i).and.k.gt.kb(i).and.k.lt.ktcon(i)) then
              dz = .5 * (zo(i,k+1) - zo(i,k-1))
              dz1 = (zo(i,k) - zo(i,k-1))
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              qrch = qeso(i,k)
     &             + gamma * dbyo(i,k) / (hvap * (1. + gamma))
              factor = eta(i,k-1) / eta(i,k)
              onemf = 1. - factor
              qcko(i,k) = factor * qcko(i,k-1) + onemf *
     &                    .5 * (qo(i,k) + qo(i,k+1))
              dq = eta(i,k) * qcko(i,k) - eta(i,k) * qrch
              rhbar(i) = rhbar(i) + qo(i,k) / qeso(i,k)
c
c  below lfc check if there is excess moisture to release latent heat
c
              if(dq.gt.0.) then
                etah = .5 * (eta(i,k) + eta(i,k-1))
                dp =1000. * del(i,k)
                qlk = dq / (eta(i,k) + etah * c0 * dz)
                aa1(i) = aa1(i) - dz1 * g * qlk
                qc = qlk + qrch
                pwo(i,k) = etah * c0 * dz * qlk
                qcko(i,k) = qc
                pwavo(i) = pwavo(i) + pwo(i,k)
                cnvwt(i,k) = etah * qlk * g / dp
              endif
            endif
          endif
        enddo
      enddo
      do i = 1, im
        rhbar(i) = rhbar(i) / float(ktcon(i) - kb(i) - 1)
      enddo
c
c  this section is ready for cloud water
c
      if(ncloud.gt.0) then
c
c  compute liquid and vapor separation at cloud top
c
      do i = 1, im
        k = ktcon(i)
        if(cnvflg(i)) then
          gamma = el2orc * qeso(i,k) / (to(i,k)**2)
          qrch = qeso(i,k)
     &         + gamma * dbyo(i,k) / (hvap * (1. + gamma))
          dq = qcko(i,k-1) - qrch
c
c  check if there is excess moisture to release latent heat
c
          if(dq.gt.0.) then
            qlko_ktcon(i) = dq
            qcko(i,k-1) = qrch
          endif
        endif
      enddo
      endif
c
c  calculate cloud work function at t+dt
c
      do k = 1, km
        do i = 1, im
          if (k .le. kmax(i)) then
            if(cnvflg(i).and.k.gt.kbcon(i).and.k.le.ktcon(i)) then
              dz1 = zo(i,k) - zo(i,k-1)
              gamma = el2orc * qeso(i,k-1) / (to(i,k-1)**2)
              rfact =  1. + delta * cp * gamma
     &                 * to(i,k-1) / hvap
              aa1(i) = aa1(i) +
     &                 dz1 * (g / (cp * to(i,k-1)))
     &                 * dbyo(i,k-1) / (1. + gamma)
     &                 * rfact
              val = 0.
              aa1(i)=aa1(i)+
     &                 dz1 * g * delta *
     &                 max(val,(qeso(i,k-1) - qo(i,k-1)))
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i).and.aa1(i).le.0.) dwnflg(i)  = .false.
        if(cnvflg(i).and.aa1(i).le.0.) dwnflg2(i) = .false.
        if(cnvflg(i).and.aa1(i).le.0.) cnvflg(i)  = .false.
      enddo
!!
      totflg = .true.
      do i = 1, im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
ccccc if(lat.eq.latd.and.lon.eq.lond.and.cnvflg(i)) then
ccccc   print *, ' aa1(i) before dwndrft =', aa1(i)
ccccc endif
c
c------- downdraft calculations
c
c
c--- determine downdraft strength in terms of windshear
c
      do i = 1, im
        if(cnvflg(i)) then
          vshear(i) = 0.
        endif
      enddo
      do k = 1, km
        do i = 1, im
          if (k .le. kmax(i)) then
            if(k.ge.kb(i).and.k.le.ktcon(i).and.cnvflg(i)) then
              shear     = sqrt((uo(i,k+1)-uo(i,k)) ** 2
     &                       + (vo(i,k+1)-vo(i,k)) ** 2)
              vshear(i) = vshear(i) + shear
            endif
          endif
        enddo
      enddo
      do i = 1, im
        edt(i) = 0.
        if(cnvflg(i)) then
          knumb = ktcon(i) - kb(i) + 1
          knumb = max(knumb,1)
          vshear(i) = 1.e3 * vshear(i) / (zo(i,ktcon(i))-zo(i,kb(i)))
          e1=1.591-.639*vshear(i)
     &       +.0953*(vshear(i)**2)-.00496*(vshear(i)**3)
          edt(i)=1.-e1
          val =         .9
          edt(i) = min(edt(i),val)
          val =         .0
          edt(i) = max(edt(i),val)
          edto(i)=edt(i)
          edtx(i)=edt(i)
        endif
      enddo
c  determine detrainment rate between 1 and kbdtr
      do i = 1, im
        kbdtr(i) = kbcon(i)
        beta = betas
        if(slimsk(i).eq.1.) beta = betal
        if(cnvflg(i)) then
          kbdtr(i) = kbcon(i)
          kbdtr(i) = max(kbdtr(i),1)
          xlamd(i) = 0.
          if(kbdtr(i).gt.1) then
            dz = .5 * zo(i,kbdtr(i)) + .5 * zo(i,kbdtr(i)-1)
     &         - zo(i,1)
            xlamd(i) =  log(beta) / dz
          endif
        endif
      enddo
c  determine downdraft mass flux
      do k = 1, km
        do i = 1, im
          if(k .le. kmax(i)) then
            if(cnvflg(i)) then
              etad(i,k) = 1.
            endif
            qrcdo(i,k) = 0.
          endif
        enddo
      enddo
      do k = km1, 2, -1
        do i = 1, im
          if (k .le. kbmax(i)) then
            if(cnvflg(i).and.k.lt.kbdtr(i)) then
              dz        = .5 * (zo(i,k+1) - zo(i,k-1))
              etad(i,k) = etad(i,k+1) * exp(xlamd(i) * dz)
            endif
          endif
        enddo
      enddo
      k = 1
      do i = 1, im
        if(cnvflg(i).and.kbdtr(i).gt.1) then
          dz = .5 * (zo(i,2) - zo(i,1))
          etad(i,k) = etad(i,k+1) * exp(xlamd(i) * dz)
        endif
      enddo
c
c--- downdraft moisture properties
c
      do i = 1, im
        pwevo(i) = 0.
        flg(i) = cnvflg(i)
      enddo
      do i = 1, im
        if(cnvflg(i)) then
          jmn = jmin(i)
          hcdo(i) = heo(i,jmn)
          qcdo(i) = qo(i,jmn)
          qrcdo(i,jmn) = qeso(i,jmn)
          ucdo(i) = uo(i,jmn)
          vcdo(i) = vo(i,jmn)
        endif
      enddo
      do k = km1, 1, -1
        do i = 1, im
          if (k .le. kmax(i)-1) then
            if(cnvflg(i).and.k.lt.jmin(i)) then
              dq = qeso(i,k)
              dt = to(i,k)
              gamma      = el2orc * dq / dt**2
              dh         = hcdo(i) - heso(i,k)
              qrcdo(i,k) = dq+(1./hvap)*(gamma/(1.+gamma))*dh
              detad      = etad(i,k+1) - etad(i,k)
              pwdo(i,k)  = etad(i,k+1) * qcdo(i) -
     &                     etad(i,k) * qrcdo(i,k)
              pwdo(i,k)  = pwdo(i,k) - detad *
     &                    .5 * (qrcdo(i,k) + qrcdo(i,k+1))
              qcdo(i)    = qrcdo(i,k)
              pwevo(i)   = pwevo(i) + pwdo(i,k)
            endif
          endif
        enddo
      enddo
c     if(lat.eq.latd.and.lon.eq.lond.and.dwnflg(i)) then
c       print *, ' pwavo(i), pwevo(i) =', pwavo(i), pwevo(i)
c     endif
c
c--- final downdraft strength dependent on precip
c--- efficiency (edt), normalized condensate (pwav), and
c--- evaporate (pwev)
c
      do i = 1, im
        edtmax = edtmaxl
        if(slimsk(i).eq.0.) edtmax = edtmaxs
        if(dwnflg2(i)) then
          if(pwevo(i).lt.0.) then
            edto(i) = -edto(i) * pwavo(i) / pwevo(i)
            edto(i) = min(edto(i),edtmax)
          else
            edto(i) = 0.
          endif
        else
          edto(i) = 0.
        endif
      enddo
c
c
c--- downdraft cloudwork functions
c
c
      do k = km1, 1, -1
        do i = 1, im
          if (k .le. kmax(i)-1) then
            if(dwnflg2(i).and.k.lt.jmin(i)) then
              gamma = el2orc * qeso(i,k+1) / to(i,k+1)**2
              dhh=hcdo(i)
              dt=to(i,k+1)
              dg=gamma
              dh=heso(i,k+1)
              dz=-1.*(zo(i,k+1)-zo(i,k))
              aa1(i)=aa1(i)+edto(i)*dz*(g/(cp*dt))*((dhh-dh)/(1.+dg))
     &               *(1.+delta*cp*dg*dt/hvap)
              val=0.
              aa1(i)=aa1(i)+edto(i)*
     &        dz*g*delta*max(val,(qeso(i,k+1)-qo(i,k+1)))
            endif
          endif
        enddo
      enddo
ccccc if(lat.eq.latd.and.lon.eq.lond.and.dwnflg2(i)) then
ccccc   print *, '  aa1(i) after dwndrft =', aa1(i)
ccccc endif
      do i = 1, im
        if(aa1(i).le.0.) cnvflg(i)  = .false.
        if(aa1(i).le.0.) dwnflg(i)  = .false.
        if(aa1(i).le.0.) dwnflg2(i) = .false.
      enddo
!!
      totflg = .true.
      do i = 1, im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
c
c
c--- what would the change be, that a cloud with unit mass
c--- will do to the environment?
c
      do k = 1, km
        do i = 1, im
          if(k .le. kmax(i) .and. cnvflg(i)) then
            dellah(i,k) = 0.
            dellaq(i,k) = 0.
            dellau(i,k) = 0.
            dellav(i,k) = 0.
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i)) then
          dp = 1000. * del(i,1)
          dellah(i,1) = edto(i) * etad(i,1) * (hcdo(i)
     &                - heo(i,1)) * g / dp
          dellaq(i,1) = edto(i) * etad(i,1) * (qcdo(i)
     &                - qo(i,1)) * g / dp
          dellau(i,1) = edto(i) * etad(i,1) * (ucdo(i)
     &                - uo(i,1)) * g / dp
          dellav(i,1) = edto(i) * etad(i,1) * (vcdo(i)
     &                - vo(i,1)) * g / dp
        endif
      enddo
c
c--- changed due to subsidence and entrainment
c
      do k = 2, km1
        do i = 1, im
          if (k .le. kmax(i)-1) then
            if(cnvflg(i).and.k.lt.ktcon(i)) then
              aup = 1.
              if(k.le.kb(i)) aup = 0.
              adw = 1.
              if(k.gt.jmin(i)) adw = 0.
              dv1= heo(i,k)
              dv2 = .5 * (heo(i,k) + heo(i,k+1))
              dv3= heo(i,k-1)
              dv1q= qo(i,k)
              dv2q = .5 * (qo(i,k) + qo(i,k+1))
              dv3q= qo(i,k-1)
              dv1u= uo(i,k)
              dv2u = .5 * (uo(i,k) + uo(i,k+1))
              dv3u= uo(i,k-1)
              dv1v= vo(i,k)
              dv2v = .5 * (vo(i,k) + vo(i,k+1))
              dv3v= vo(i,k-1)
              dp = 1000. * del(i,k)
              dz = .5 * (zo(i,k+1) - zo(i,k-1))
              deta = eta(i,k) - eta(i,k-1)
              detad = etad(i,k) - etad(i,k-1)
              dellah(i,k) = dellah(i,k) +
     &            ((aup * eta(i,k) - adw * edto(i) * etad(i,k)) * dv1
     &        - (aup * eta(i,k-1) - adw * edto(i) * etad(i,k-1))* dv3
     &                    - aup * deta * dv2
     &                    + adw * edto(i) * detad * hcdo(i)) * g / dp
              dellaq(i,k) = dellaq(i,k) +
     &            ((aup * eta(i,k) - adw * edto(i) * etad(i,k)) * dv1q
     &        - (aup * eta(i,k-1) - adw * edto(i) * etad(i,k-1))* dv3q
     &                    - aup * deta * dv2q
     &       +adw*edto(i)*detad*.5*(qrcdo(i,k)+qrcdo(i,k-1))) * g / dp
              dellau(i,k) = dellau(i,k) +
     &            ((aup * eta(i,k) - adw * edto(i) * etad(i,k)) * dv1u
     &        - (aup * eta(i,k-1) - adw * edto(i) * etad(i,k-1))* dv3u
     &                     - aup * deta * dv2u
     &                    + adw * edto(i) * detad * ucdo(i)
     &                    ) * g / dp
              dellav(i,k) = dellav(i,k) +
     &            ((aup * eta(i,k) - adw * edto(i) * etad(i,k)) * dv1v
     &        - (aup * eta(i,k-1) - adw * edto(i) * etad(i,k-1))* dv3v
     &                     - aup * deta * dv2v
     &                    + adw * edto(i) * detad * vcdo(i)
     &                    ) * g / dp
            endif
          endif
        enddo
      enddo
c
c------- cloud top
c
      do i = 1, im
        if(cnvflg(i)) then
          indx = ktcon(i)
          dp = 1000. * del(i,indx)
          dv1 = heo(i,indx-1)
          dellah(i,indx) = eta(i,indx-1) *
     &                     (hcko(i,indx-1) - dv1) * g / dp
          dvq1 = qo(i,indx-1)
          dellaq(i,indx) = eta(i,indx-1) *
     &                     (qcko(i,indx-1) - dvq1) * g / dp
          dv1u = uo(i,indx-1)
          dellau(i,indx) = eta(i,indx-1) *
     &                     (ucko(i,indx-1) - dv1u) * g / dp
          dv1v = vo(i,indx-1)
          dellav(i,indx) = eta(i,indx-1) *
     &                     (vcko(i,indx-1) - dv1v) * g / dp
c
c  cloud water
c
          dellal(i) = eta(i,indx-1) * qlko_ktcon(i) * g / dp
        endif
      enddo
c
c------- final changed variable per unit mass flux
c
      do k = 1, km
        do i = 1, im
          if (k .le. kmax(i)) then
            if(cnvflg(i).and.k.gt.ktcon(i)) then
              qo(i,k) = q1(i,k)
              to(i,k) = t1(i,k)
              uo(i,k) = u1(i,k)
              vo(i,k) = v1(i,k)
            endif
            if(cnvflg(i).and.k.le.ktcon(i)) then
              qo(i,k) = dellaq(i,k) * mbdt + q1(i,k)
              dellat = (dellah(i,k) - hvap * dellaq(i,k)) / cp
              to(i,k) = dellat * mbdt + t1(i,k)
              val   =           1.e-10
              qo(i,k) = max(qo(i,k), val  )
            endif
          endif
        enddo
      enddo
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c--- the above changed environment is now used to calulate the
c--- effect the arbitrary cloud (with unit mass flux)
c--- would have on the stability,
c--- which then is used to calculate the real mass flux,
c--- necessary to keep this change in balance with the large-scale
c--- destabilization.
c
c--- environmental conditions again, first heights
c
      do k = 1, km
        do i = 1, im
          if(k .le. kmax(i) .and. cnvflg(i)) then
!jfe        qeso(i,k) = 10. * fpvs(to(i,k))
!
            qeso(i,k) = 0.01 * fpvs(to(i,k))      ! fpvs is in pa
!
            qeso(i,k) = eps * qeso(i,k) / (pfld(i,k)+epsm1*qeso(i,k))
            val       =             1.e-8
            qeso(i,k) = max(qeso(i,k), val )
            tvo(i,k)  = to(i,k) + delta * to(i,k) * qo(i,k)
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i)) then
          xaa0(i) = 0.
          xpwav(i) = 0.
        endif
      enddo
c
c  hydrostatic height assume zero terr
c
!     do i = 1, im
!       if(cnvflg(i)) then
!         dlnsig =  log(prsl(i,1)/ps(i))
!         zo(i,1) = terr - dlnsig * rd / g * tvo(i,1)
!       endif
!     enddo
!     do k = 2, km
!       do i = 1, im
!         if(k .le. kmax(i) .and. cnvflg(i)) then
!           dlnsig =  log(prsl(i,k) / prsl(i,k-1))
!           zo(i,k) = zo(i,k-1) - dlnsig * rd / g
!    &             * .5 * (tvo(i,k) + tvo(i,k-1))
!         endif
!       enddo
!     enddo
c
c--- moist static energy
c
      do k = 1, km1
        do i = 1, im
          if(k .le. kmax(i)-1 .and. cnvflg(i)) then
            dz = .5 * (zo(i,k+1) - zo(i,k))
            dp = .5 * (pfld(i,k+1) - pfld(i,k))
cjfe        es = 10. * fpvs(to(i,k+1))
!
            es = 0.01 * fpvs(to(i,k+1))      ! fpvs is in pa
!
            pprime = pfld(i,k+1) + epsm1 * es
            qs = eps * es / pprime
            dqsdp = - qs / pprime
            desdt = es * (fact1 / to(i,k+1) + fact2 / (to(i,k+1)**2))
            dqsdt = qs * pfld(i,k+1) * desdt / (es * pprime)
            gamma = el2orc * qeso(i,k+1) / (to(i,k+1)**2)
            dt = (g * dz + hvap * dqsdp * dp) / (cp * (1. + gamma))
            dq = dqsdt * dt + dqsdp * dp
            to(i,k) = to(i,k+1) + dt
            qo(i,k) = qo(i,k+1) + dq
            po(i,k) = .5 * (pfld(i,k) + pfld(i,k+1))
          endif
        enddo
      enddo
      do k = 1, km1
        do i = 1, im
          if(k .le. kmax(i)-1 .and. cnvflg(i)) then
cjfe        qeso(i,k) = 10. * fpvs(to(i,k))
!
            qeso(i,k) = 0.01 * fpvs(to(i,k))      ! fpvs is in pa
!
            qeso(i,k) = eps * qeso(i,k) / (po(i,k) + epsm1 * qeso(i,k))
            val1      =             1.e-8
            qeso(i,k) = max(qeso(i,k), val1)
            val2      =           1.e-10
            qo(i,k)   = max(qo(i,k), val2 )
c           qo(i,k)   = min(qo(i,k),qeso(i,k))
            heo(i,k)   = .5 * g * (zo(i,k) + zo(i,k+1)) +
     &                    cp * to(i,k) + hvap * qo(i,k)
            heso(i,k) = .5 * g * (zo(i,k) + zo(i,k+1)) +
     &                  cp * to(i,k) + hvap * qeso(i,k)
          endif
        enddo
      enddo
      do i = 1, im
        k = kmax(i)
        if(cnvflg(i)) then
          heo(i,k) = g * zo(i,k) + cp * to(i,k) + hvap * qo(i,k)
          heso(i,k) = g * zo(i,k) + cp * to(i,k) + hvap * qeso(i,k)
c         heo(i,k) = min(heo(i,k),heso(i,k))
        endif
      enddo
      do i = 1, im
        if(cnvflg(i)) then
          indx = kb(i)
          xhkb(i) = heo(i,indx)
          xqkb(i) = qo(i,indx)
          hcko(i,indx) = xhkb(i)
          qcko(i,indx) = xqkb(i)
        endif
      enddo
c
c
c**************************** static control
c
c
c------- moisture and cloud work functions
c
      do k = 2, km1
        do i = 1, im
          if (k .le. kmax(i)-1) then
c           if(cnvflg(i).and.k.gt.kb(i).and.k.le.kbcon(i)) then
            if(cnvflg(i).and.k.gt.kb(i).and.k.le.ktcon(i)) then
              factor = eta(i,k-1) / eta(i,k)
              onemf = 1. - factor
              hcko(i,k) = factor * hcko(i,k-1) + onemf *
     &                    .5 * (heo(i,k) + heo(i,k+1))
            endif
c           if(cnvflg(i).and.k.gt.kbcon(i)) then
c             heo(i,k) = heo(i,k-1)
c           endif
          endif
        enddo
      enddo
      do k = 2, km1
        do i = 1, im
          if (k .le. kmax(i)-1) then
            if(cnvflg(i).and.k.gt.kb(i).and.k.lt.ktcon(i)) then
              dz = .5 * (zo(i,k+1) - zo(i,k-1))
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              xdby = hcko(i,k) - heso(i,k)
              val  =          0.
              xdby = max(xdby,val)
              xqrch = qeso(i,k)
     &              + gamma * xdby / (hvap * (1. + gamma))
              factor = eta(i,k-1) / eta(i,k)
              onemf = 1. - factor
              qcko(i,k) = factor * qcko(i,k-1) + onemf *
     &                    .5 * (qo(i,k) + qo(i,k+1))
              dq = eta(i,k) * qcko(i,k) - eta(i,k) * xqrch
              if(dq.gt.0.) then
                etah = .5 * (eta(i,k) + eta(i,k-1))
                qlk = dq / (eta(i,k) + etah * c0 * dz)
                xaa0(i) = xaa0(i) - (zo(i,k) - zo(i,k-1)) * g * qlk
                xqc = qlk + xqrch
                xpw = etah * c0 * dz * qlk
                qcko(i,k) = xqc
                xpwav(i) = xpwav(i) + xpw
              endif
            endif
c           if(cnvflg(i).and.k.gt.kbcon(i).and.k.lt.ktcon(i)) then
            if(cnvflg(i).and.k.gt.kbcon(i).and.k.le.ktcon(i)) then
              dz1 = zo(i,k) - zo(i,k-1)
              gamma = el2orc * qeso(i,k-1) / (to(i,k-1)**2)
              rfact =  1. + delta * cp * gamma
     &                 * to(i,k-1) / hvap
              xdby = hcko(i,k-1) - heso(i,k-1)
              xaa0(i) = xaa0(i)
     &                + dz1 * (g / (cp * to(i,k-1)))
     &                * xdby / (1. + gamma)
     &                * rfact
              val=0.
              xaa0(i)=xaa0(i)+
     &                 dz1 * g * delta *
     &                 max(val,(qeso(i,k-1) - qo(i,k-1)))
            endif
          endif
        enddo
      enddo
ccccc if(lat.eq.latd.and.lon.eq.lond.and.cnvflg(i)) then
ccccc   print *, ' xaa before dwndrft =', xaa0(i)
ccccc endif
c
c------- downdraft calculations
c
c
c--- downdraft moisture properties
c
      do i = 1, im
        xpwev(i) = 0.
      enddo
      do i = 1, im
        if(dwnflg2(i)) then
          jmn = jmin(i)
          xhcd(i) = heo(i,jmn)
          xqcd(i) = qo(i,jmn)
          qrcd(i,jmn) = qeso(i,jmn)
        endif
      enddo
      do k = km1, 1, -1
        do i = 1, im
          if (k .le. kmax(i)-1) then
            if(dwnflg2(i).and.k.lt.jmin(i)) then
              dq = qeso(i,k)
              dt = to(i,k)
              gamma    = el2orc * dq / dt**2
              dh       = xhcd(i) - heso(i,k)
              qrcd(i,k)=dq+(1./hvap)*(gamma/(1.+gamma))*dh
              detad    = etad(i,k+1) - etad(i,k)
              xpwd     = etad(i,k+1) * qrcd(i,k+1) -
     &                   etad(i,k) * qrcd(i,k)
              xpwd     = xpwd - detad *
     &                 .5 * (qrcd(i,k) + qrcd(i,k+1))
              xpwev(i) = xpwev(i) + xpwd
            endif
          endif
        enddo
      enddo
c
      do i = 1, im
        edtmax = edtmaxl
        if(slimsk(i).eq.0.) edtmax = edtmaxs
        if(dwnflg2(i)) then
          if(xpwev(i).ge.0.) then
            edtx(i) = 0.
          else
            edtx(i) = -edtx(i) * xpwav(i) / xpwev(i)
            edtx(i) = min(edtx(i),edtmax)
          endif
        else
          edtx(i) = 0.
        endif
      enddo
c
c
c
c--- downdraft cloudwork functions
c
c
      do k = km1, 1, -1
        do i = 1, im
          if (k .le. kmax(i)-1) then
            if(dwnflg2(i).and.k.lt.jmin(i)) then
              gamma = el2orc * qeso(i,k+1) / to(i,k+1)**2
              dhh=xhcd(i)
              dt= to(i,k+1)
              dg= gamma
              dh= heso(i,k+1)
              dz=-1.*(zo(i,k+1)-zo(i,k))
              xaa0(i)=xaa0(i)+edtx(i)*dz*(g/(cp*dt))*((dhh-dh)/(1.+dg))
     &                *(1.+delta*cp*dg*dt/hvap)
              val=0.
              xaa0(i)=xaa0(i)+edtx(i)*
     &        dz*g*delta*max(val,(qeso(i,k+1)-qo(i,k+1)))
            endif
          endif
        enddo
      enddo
ccccc if(lat.eq.latd.and.lon.eq.lond.and.dwnflg2(i)) then
ccccc   print *, '  xaa after dwndrft =', xaa0(i)
ccccc endif
c
c  calculate critical cloud work function
c
      do i = 1, im
        acrt(i) = 0.
        if(cnvflg(i)) then
c       if(cnvflg(i).and.slimsk(i).ne.1.) then
          if(pfld(i,ktcon(i)).lt.pcrit(15))then
            acrt(i)=acrit(15)*(975.-pfld(i,ktcon(i)))
     &              /(975.-pcrit(15))
          else if(pfld(i,ktcon(i)).gt.pcrit(1))then
            acrt(i)=acrit(1)
          else
            k =  int((850. - pfld(i,ktcon(i)))/50.) + 2
            k = min(k,15)
            k = max(k,2)
            acrt(i)=acrit(k)+(acrit(k-1)-acrit(k))*
     *           (pfld(i,ktcon(i))-pcrit(k))/(pcrit(k-1)-pcrit(k))
           endif
c        else
c          acrt(i) = .5 * (pfld(i,kbcon(i)) - pfld(i,ktcon(i)))
         endif
      enddo
      do i = 1, im
        acrtfct(i) = 1.
        if(cnvflg(i)) then
          if(slimsk(i).eq.1.) then
            w1 = w1l
            w2 = w2l
            w3 = w3l
            w4 = w4l
          else
            w1 = w1s
            w2 = w2s
            w3 = w3s
            w4 = w4s
          endif
c       if(cnvflg(i).and.slimsk(i).eq.1.) then
c         acrtfct(i) = pdot(i) / w3
c
c  modify critical cloud workfunction by cloud base vertical velocity
c
          if(pdot(i).le.w4) then
            acrtfct(i) = (pdot(i) - w4) / (w3 - w4)
          elseif(pdot(i).ge.-w4) then
            acrtfct(i) = - (pdot(i) + w4) / (w4 - w3)
          else
            acrtfct(i) = 0.
          endif
          val1    =             -1.
          acrtfct(i) = max(acrtfct(i),val1)
          val2    =             1.
          acrtfct(i) = min(acrtfct(i),val2)
          acrtfct(i) = 1. - acrtfct(i)
c
c  modify acrtfct(i) by colume mean rh if rhbar(i) is greater than 80 percent
c
c         if(rhbar(i).ge..8) then
c           acrtfct(i) = acrtfct(i) * (.9 - min(rhbar(i),.9)) * 10.
c         endif
c
c  modify adjustment time scale by cloud base vertical velocity
c
          dtconv(i) = dt2 + max((1800. - dt2),cons_0) *
     &                (pdot(i) - w2) / (w1 - w2)
c         dtconv(i) = max(dtconv(i), dt2)
c         dtconv(i) = 1800. * (pdot(i) - w2) / (w1 - w2)
          dtconv(i) = max(dtconv(i),dtmin)
          dtconv(i) = min(dtconv(i),dtmax)
 
        endif
      enddo
c
c--- large scale forcing
c
      do i= 1, im
        flg(i) = cnvflg(i)
        if(cnvflg(i)) then
c         f = aa1(i) / dtconv(i)
          fld(i) = (aa1(i) - acrt(i) * acrtfct(i)) / dtconv(i)
          if(fld(i).le.0.) flg(i) = .false.
        endif
        cnvflg(i) = flg(i)
        if(cnvflg(i)) then
c         xaa0(i) = max(xaa0(i),0.)
          xk(i) = (xaa0(i) - aa1(i)) / mbdt
          if(xk(i).ge.0.) flg(i) = .false.
        endif
c
c--- kernel, cloud base mass flux
c
        cnvflg(i) = flg(i)
        if(cnvflg(i)) then
          xmb(i) = -fld(i) / xk(i)
          xmb(i) = min(xmb(i),xmbmax(i))
        endif
      enddo
c      if(lat.eq.latd.and.lon.eq.lond.and.cnvflg(i)) then
c        print *, ' rhbar(i), acrtfct(i) =', rhbar(i), acrtfct(i)
c        print *, '  a1, xa =', aa1(i), xaa0(i)
c        print *, ' xmb(i), acrt =', xmb(i), acrt
c      endif
      totflg = .true.
      do i = 1, im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
c
c  restore t0 and qo to t1 and q1 in case convection stops
c
      do k = 1, km
        do i = 1, im
          if (k .le. kmax(i)) then
            to(i,k) = t1(i,k)
            qo(i,k) = q1(i,k)
!jfe        qeso(i,k) = 10. * fpvs(t1(i,k))
!
            qeso(i,k) = 0.01 * fpvs(t1(i,k))      ! fpvs is in pa
!
            qeso(i,k) = eps * qeso(i,k) / (pfld(i,k) + epsm1*qeso(i,k))
            val     =             1.e-8
            qeso(i,k) = max(qeso(i,k), val )
          endif
        enddo
      enddo
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c--- feedback: simply the changes from the cloud with unit mass flux
c---           multiplied by  the mass flux necessary to keep the
c---           equilibrium with the larger-scale.
c
      do i = 1, im
        delhbar(i) = 0.
        delqbar(i) = 0.
        deltbar(i) = 0.
        qcond(i) = 0.
      enddo
      do k = 1, km
        do i = 1, im
          if (k .le. kmax(i)) then
            if(cnvflg(i).and.k.le.ktcon(i)) then
              aup = 1.
              if(k.le.kb(i)) aup = 0.
              adw = 1.
              if(k.gt.jmin(i)) adw = 0.
              dellat = (dellah(i,k) - hvap * dellaq(i,k)) / cp
              t1(i,k) = t1(i,k) + dellat * xmb(i) * dt2
              q1(i,k) = q1(i,k) + dellaq(i,k) * xmb(i) * dt2
              u1(i,k) = u1(i,k) + dellau(i,k) * xmb(i) * dt2
              v1(i,k) = v1(i,k) + dellav(i,k) * xmb(i) * dt2
              dp = 1000. * del(i,k)
              delhbar(i) = delhbar(i) + dellah(i,k)*xmb(i)*dp/g
              delqbar(i) = delqbar(i) + dellaq(i,k)*xmb(i)*dp/g
              deltbar(i) = deltbar(i) + dellat*xmb(i)*dp/g
            endif
          endif
        enddo
      enddo
      do k = 1, km
        do i = 1, im
          if (k .le. kmax(i)) then
            if(cnvflg(i).and.k.le.ktcon(i)) then
!jfe          qeso(i,k) = 10. * fpvs(t1(i,k))
!
              qeso(i,k) = 0.01 * fpvs(t1(i,k))      ! fpvs is in pa
!
              qeso(i,k) = eps * qeso(i,k)/(pfld(i,k) + epsm1*qeso(i,k))
              val     =             1.e-8
              qeso(i,k) = max(qeso(i,k), val )
c
c  cloud water
c
              if(ncloud.gt.0.and.cnvflg(i).and.k.eq.ktcon(i)) then
                tem  = dellal(i) * xmb(i) * dt2
                tem1 = max(0.0, min(1.0, (tcr-t1(i,k))*tcrf))
                if (ql(i,k,2) .gt. -999.0) then
                  ql(i,k,1) = ql(i,k,1) + tem * tem1            ! ice
                  ql(i,k,2) = ql(i,k,2) + tem *(1.0-tem1)       ! water
                else
                  ql(i,k,1) = ql(i,k,1) + tem
                endif
                dp = 1000. * del(i,k)
                dellal(i) = dellal(i) * xmb(i) * dp / g
              endif
!
!             if(ncloud.gt.0.and.cnvflg(i).and.k.eq.ktcon(i)) then
!               ql(i,k) = ql(i,k) + dellal(i) * xmb(i) * dt2
!               dp = 1000. * del(i,k)
!               dellal(i) = dellal(i) * xmb(i) * dp / g
!             endif
!
            endif
          endif
        enddo
      enddo
c     if(lat.eq.latd.and.lon.eq.lond.and.cnvflg(i) ) then
c       print *, ' delhbar, delqbar, deltbar ='
c       print *, delhbar, hvap*delqbar, cp*deltbar
c       print *, '   dellbar ='
c       print 6003,  hvap*dellbar
c       print *, '   dellaq ='
c       print 6003, (hvap*dellaq(i,k)*xmb(i),k=1,kmax)
c       print *, '   dellat ='
c       print 6003, (dellah(i,k)*xmb(i)-hvap*dellaq(i,k)*xmb(i),
c    &               k=1,kmax)
c     endif
      do i = 1, im
        rntot(i) = 0.
        delqev(i) = 0.
        delq2(i) = 0.
        flg(i) = cnvflg(i)
      enddo
      do k = km, 1, -1
        do i = 1, im
          if (k .le. kmax(i)) then
            if(cnvflg(i).and.k.le.ktcon(i)) then
              aup = 1.
              if(k.le.kb(i)) aup = 0.
              adw = 1.
              if(k.gt.jmin(i)) adw = 0.
              rain =  aup * pwo(i,k) + adw * edto(i) * pwdo(i,k)
              rntot(i) = rntot(i) + rain * xmb(i) * .001 * dt2
            endif
          endif
        enddo
      enddo
      do k = km, 1, -1
        do i = 1, im
          if (k .le. kmax(i)) then
            deltv(i) = 0.
            delq(i) = 0.
            qevap(i) = 0.
            if(cnvflg(i).and.k.le.ktcon(i)) then
              aup = 1.
              if(k.le.kb(i)) aup = 0.
              adw = 1.
              if(k.gt.jmin(i)) adw = 0.
              rain =  aup * pwo(i,k) + adw * edto(i) * pwdo(i,k)
              rn(i) = rn(i) + rain * xmb(i) * .001 * dt2
            endif
            if(flg(i).and.k.le.ktcon(i)) then
              evef = edt(i) * evfact
              if(slimsk(i).eq.1.) evef=edt(i) * evfactl
!             if(slimsk(i).eq.1.) evef=.07
c             if(slimsk(i).ne.1.) evef = 0.
              qcond(i) = evef * (q1(i,k) - qeso(i,k))
     &                 / (1. + el2orc * qeso(i,k) / t1(i,k)**2)
              dp = 1000. * del(i,k)
              if(rn(i).gt.0..and.qcond(i).lt.0.) then
                qevap(i) = -qcond(i) * (1.-exp(-.32*sqrt(dt2*rn(i))))
                qevap(i) = min(qevap(i), rn(i)*1000.*g/dp)
                delq2(i) = delqev(i) + .001 * qevap(i) * dp / g
              endif
              if(rn(i).gt.0..and.qcond(i).lt.0..and.
     &           delq2(i).gt.rntot(i)) then
                qevap(i) = 1000.* g * (rntot(i) - delqev(i)) / dp
                flg(i) = .false.
              endif
              if(rn(i).gt.0..and.qevap(i).gt.0.) then
                q1(i,k) = q1(i,k) + qevap(i)
                t1(i,k) = t1(i,k) - elocp * qevap(i)
                rn(i) = rn(i) - .001 * qevap(i) * dp / g
                deltv(i) = - elocp*qevap(i)/dt2
                delq(i) =  + qevap(i)/dt2
                delqev(i) = delqev(i) + .001*dp*qevap(i)/g
              endif
              dellaq(i,k) = dellaq(i,k) + delq(i) / xmb(i)
              delqbar(i) = delqbar(i) + delq(i)*dp/g
              deltbar(i) = deltbar(i) + deltv(i)*dp/g
            endif
          endif
        enddo
      enddo
c      if(lat.eq.latd.and.lon.eq.lond.and.cnvflg(i) ) then
c        print *, '   dellah ='
c        print 6003, (dellah(k)*xmb(i),k=1,kmax)
c        print *, '   dellaq ='
c        print 6003, (hvap*dellaq(i,k)*xmb(i),k=1,kmax)
c        print *, ' delhbar, delqbar, deltbar ='
c        print *, delhbar, hvap*delqbar, cp*deltbar
c        print *, ' precip =', hvap*rn(i)*1000./dt2
ccccc   print *, '   dellbar ='
ccccc   print *,  hvap*dellbar
c      endif
c
c  precipitation rate converted to actual precip
c  in unit of m instead of kg
c
      do i = 1, im
        if(cnvflg(i)) then
c
c  in the event of upper level rain evaporation and lower level downdraf
c    moistening, rn can become negative, in this case, we back out of th
c    heating and the moistening
c
          if(rn(i).lt.0..and..not.flg(i)) rn(i) = 0.
          if(rn(i).le.0.) then
            rn(i) = 0.
          else
            ktop(i) = ktcon(i)
            kbot(i) = kbcon(i)
            kuo(i) = 1
            cldwrk(i) = aa1(i)
          endif
        endif
      enddo
c
c  convective cloud water
c
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. rn(i).gt.0.) then
            if (k.ge.kbcon(i).and.k.lt.ktcon(i)) then
              cnvw(i,k) = cnvwt(i,k) * xmb(i) * dt2
            endif
          endif
        enddo
      enddo
c
c  convective cloud cover
c
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. rn(i).gt.0.) then
            if (k.ge.kbcon(i).and.k.lt.ktcon(i)) then
              cnvc(i,k) = 0.04 * log(1. + 576. * eta(i,k) * xmb(i)) ! in uetm da run
!              cnvc(i,k) = 0.01 * log(1. + 500. * eta(i,k) * xmb(i))
              cnvc(i,k) = min(cnvc(i,k), 0.6)
              cnvc(i,k) = max(cnvc(i,k), 0.0)
            endif
          endif
        enddo
      enddo

      do k = 1, km
        do i = 1, im
          if (k .le. kmax(i)) then
            if(cnvflg(i).and.rn(i).le.0.) then
              t1(i,k) = to(i,k)
              q1(i,k) = qo(i,k)
            endif
          endif
        enddo
      enddo
! hchuang code change [+24l]
      do k = 1, km
        do i = 1, im
          if(cnvflg(i).and.rn(i).gt.0.) then
            if(k.ge.kb(i) .and. k.lt.ktop(i)) then
              ud_mf(i,k) = eta(i,k) * xmb(i) * dt2
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i).and.rn(i).gt.0.) then
           k = ktop(i)-1
           dt_mf(i,k) = ud_mf(i,k)
        endif
      enddo
      do k = 1, km
        do i = 1, im
          if(cnvflg(i).and.rn(i).gt.0.) then
            if(k.ge.1 .and. k.le.jmin(i)) then
              dd_mf(i,k) = edto(i) * etad(i,k) * xmb(i) * dt2
            endif
          endif
        enddo
      enddo
!!
      return
      end
