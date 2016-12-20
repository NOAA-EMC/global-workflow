cfpp$ noconcur r
      subroutine moninq1(ix,im,km,ntrac,ntcw,dv,du,tau,rtg,
     &     u1,v1,t1,q1,swh,hlw,xmu,
     &     psk,rbsoil,fm,fh,tsea,qss,heat,evap,stress,spd1,kpbl,
     &     prsi,del,prsl,prslk,phii,phil,deltim,
     &     dusfc,dvsfc,dtsfc,dqsfc,hpbl,hgamt,hgamq,dkt,kinver)
!
      use machine  , only : kind_phys
      use funcphys , only : fpvs
      use physcons, grav => con_g, rd => con_rd, cp => con_cp
     &,             hvap => con_hvap, rog => con_rog, fv => con_fvirt
     &,             eps => con_eps, epsm1 => con_epsm1
      implicit none
!
!     arguments
!
      integer ix, im, km, ntrac, ntcw, kpbl(im), kpblx(im)
      integer kinver(im)
!
      real(kind=kind_phys) deltim
      real(kind=kind_phys) dv(im,km),     du(im,km),
     &                     tau(im,km),    rtg(im,km,ntrac),
     &                     u1(ix,km),     v1(ix,km),
     &                     t1(ix,km),     q1(ix,km,ntrac),
     &                     swh(ix,km),    hlw(ix,km),
     &                     xmu(im), 
     &                     psk(im),       rbsoil(im),
!    &                     cd(im),        ch(im),
     &                     fm(im),        fh(im),
     &                     tsea(im),      qss(im),
     &                                    spd1(im),
!    &                     dphi(im),      spd1(im),
     &                     prsi(ix,km+1), del(ix,km),
     &                     prsl(ix,km),   prslk(ix,km),
     &                     phii(ix,km+1), phil(ix,km),
     &                     dusfc(im),
     &                     dvsfc(im),     dtsfc(im),
     &                     dqsfc(im),     hpbl(im),      hpblx(im),
     &                     hgamt(im),     hgamq(im),
     &                     hgamu(im),     hgamv(im),     hgams(im)
!
!    locals
!
      integer i,is,k,kk,km1,kmpbl
!     integer iprt,latd,lond
      integer lcld(im),icld(im),kcld(im),krad(im)
      integer kemx(im)
!
!     real(kind=kind_phys) betaq(im), betat(im),   betaw(im),
      real(kind=kind_phys) evap(im),  heat(im),    phih(im),
     &                     phim(im),  rbdn(im),    rbup(im),
     &                     stress(im),beta(im),
     &                     ustar(im), wscale(im),  thermal(im),
     &                     ust3(im),  wstar3(im),  
     &                     sumz(im),  sumt(im)
     &,                    entflx(im),sflux(im)
!
      real(kind=kind_phys) thlx(im,km), thlvx(im,km), tlx(im,km),
     &                     thvx(im,km), qlx(im,km),
     &                     qtx(im,km),  bf(im,km-1),
     &                     govrth(im),  hrad(im),     radx(im,km-1),
     &                     hradm(im),   radmin(im),   vrad(im),
     &                     zd(im),      thlvx1(im)
!
      real(kind=kind_phys) rdzt(im,km-1),dktx(im,km-1),dkux(im,km-1),
     &                     zi(im,km+1),  zl(im,km),  xkzo(im,km),
     &                     dku(im,km-1), dkt(im,km-1),
     &                     dkuy(im,km-1),dkty(im,km-1),
     &                     cku(im,km-1), ckt(im,km-1),
     &                     al(im,km-1),  ad(im,km),
     &                     au(im,km-1),  a1(im,km), 
     &                     a2(im,km*ntrac), theta(im,km)
!
      real(kind=kind_phys) hol(im), prinv(im), hpbl01(im)
!
      logical  pblflg(im), sfcflg(im), scuflg(im), flg(im)
!
      real(kind=kind_phys) aphi16,  aphi5,  bvf2,   wfac,
     &                     cfac,    conq,   cont,   conw,
     &                     dk,     dkmax,  dkmin,
     &                     dsdz2,  dsdzq,  dsdzt,
     &                     dsdzu,   dsdzv,  sfac,
     &                     dsig,    dt,     dtodsd,
     &                     dtodsu,  dw2,    dw2min, g,
     &                     gamcrq,  gamcrt, gocp,   gravi,
     &                     hol1,    pfac,   prmax,  prmin,
     &                     prnum,   qmin,   tdzmin, qtend, rbcr,
     &                     rbint,   rdt,    rdz,    qlmin, 
!    &                     rbint,   rdt,    rdz,    rdzt1,
     &                     ri,      rimin,  rl2,    rlam,  rlamun,
     &                     sfcfrac,
     &                     shr2,    spdk2,  sri,
     &                     tem,     ti,     ttend,  
     &                     utend,  vk, 
     &                     vtend,   zfac,   vpert,  cpert,
     &                     entfac,  rentfac,radfac,
     &                     zfmin,   zk,     tem1,   tem2,  xkzm,
     &                     ptem,    ptem1,  ptem2
!
      real(kind=kind_phys) zstblmax,h1,     
     &                     qlcr,    cldtime,alpri,  chiri,
     &                     u01,     v01,    delu,   delv
cc
      parameter(gravi=1.0/grav)
      parameter(g=grav)
      parameter(gocp=g/cp)
      parameter(cont=cp/g,conq=hvap/g,conw=1.0/g)
      parameter(alpri=hvap/rd,chiri=eps*hvap*hvap/cp/rd)
      parameter(rlam=30.0,vk=0.4)
      parameter(prmin=0.25,prmax=4.)
      parameter(dw2min=0.0001,dkmin=0.0,dkmax=1000.,rimin=-100.)
      parameter(rbcr=0.25,wfac=7.0,cfac=6.5,pfac=2.0,sfcfrac=0.1)
!     parameter(qmin=1.e-8,xkzm=1.0,zfmin=1.e-8,aphi5=5.,aphi16=16.)
      parameter(qmin=1.e-8,xkzm=0.25,zfmin=1.e-8,aphi5=5.,aphi16=16.)
      parameter(tdzmin=1.e-3,qlmin=1.e-12,cpert=0.25,sfac=5.4)
      parameter(h1=0.33333333)
      parameter(cldtime=500.)
!     parameter(gamcrt=3.,gamcrq=2.e-3,rlamun=150.0)
      parameter(gamcrt=3.,gamcrq=0.,rlamun=150.0)
      parameter(entfac=0.2,rentfac=0.2,radfac=0.85)
!
!     parameter (zstblmax = 2500., qlcr=3.0e-5)
!     parameter (zstblmax = 2500., qlcr=3.5e-5)
!     parameter (zstblmax = 2500., qlcr=4.5e-5)
      parameter (zstblmax = 2500., qlcr=5.0e-5)
c
c-----------------------------------------------------------------------
c
 601  format(1x,' moninp lat lon step hour ',3i6,f6.1)
 602      format(1x,'    k','        z','        t','       th',
     1     '      tvh','        q','        u','        v',
     2     '       sp')
 603      format(1x,i5,8f9.1)
 604      format(1x,'  sfc',9x,f9.1,18x,f9.1)
 605      format(1x,'    k      zl    spd2   thekv   the1v'
     1         ,' thermal    rbup')
 606      format(1x,i5,6f8.2)
 607      format(1x,' kpbl    hpbl      fm      fh   hgamt',
     1         '   hgamq      ws   ustar      cd      ch')
 608      format(1x,i5,9f8.2)
 609      format(1x,' k pr dkt dku ',i5,3f8.2)
 610      format(1x,' k pr dkt dku ',i5,3f8.2,' l2 ri t2',
     1         ' sr2  ',2f8.2,2e10.2)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     compute preliminary variables
c
      if (ix .lt. im) stop
!
!     iprt = 0
!     if(iprt.eq.1) then
ccc   latd = 0
!     lond = 0
!     else
ccc   latd = 0
!     lond = 0
!     endif
c
      dt    = 2. * deltim
      rdt   = 1. / dt
      km1   = km - 1
      kmpbl = km / 2
!
      do k=1,km
        do i=1,im
          zi(i,k) = phii(i,k) * gravi
          zl(i,k) = phil(i,k) * gravi
        enddo
      enddo
      do i=1,im
         zi(i,km+1) = phii(i,km+1) * gravi
      enddo
c
      do k = 1,km1
        do i=1,im
          rdzt(i,k) = 1.0 / (zl(i,k+1) - zl(i,k))
        enddo
      enddo
c
      do k = 1,km1
        do i=1,im
          if (k .lt. kinver(i)) then
            tem1      = 1.0 - prsi(i,k+1) / prsi(i,1)
            tem1      = tem1 * tem1 * 10.0
            xkzo(i,k) = xkzm * min(1.0, exp(-tem1))
          else
            xkzo(i,k) = 0.0
          endif
        enddo
      enddo
c
      do i = 1,im
         dusfc(i) = 0.
         dvsfc(i) = 0.
         dtsfc(i) = 0.
         dqsfc(i) = 0.
         hgamt(i) = 0.
         hgamq(i) = 0.
         hgamu(i) = 0.
         hgamv(i) = 0.
         hgams(i) = 0.
         wscale(i)= 0.
         entflx(i)= 0.
         kpbl(i)  = 1
         hpbl(i)  = zi(i,1)
         pblflg(i)= .true.
         sfcflg(i)= .true.
         if(rbsoil(i).gt.0.0) sfcflg(i) = .false.
         sumz(i)  = 0.
         sumt(i)  = 0.
         scuflg(i)= .true.
         if(scuflg(i)) then
           radmin(i)= 0.
           hrad(i)  = zi(i,1)
           icld(i)  = 0
           lcld(i)  = km1
           kcld(i)  = km1
           krad(i)  = km1
           zd(i)    = 0.
        endif
      enddo
!
      do k = 1,km
        do i = 1,im
          theta(i,k) = t1(i,k) * psk(i) / prslk(i,k)
          qlx(i,k)   = max(q1(i,k,ntcw),qlmin)
          qtx(i,k)   = max(q1(i,k,1),qmin)+qlx(i,k)
          ptem       = qlx(i,k)
          ptem1      = hvap*max(q1(i,k,1),qmin)/(cp*t1(i,k))
          thvx(i,k)  = theta(i,k)*(1.+fv*max(q1(i,k,1),qmin)-ptem)
          tlx(i,k)   = t1(i,k)-(hvap/cp)*ptem
          thlx(i,k)  = theta(i,k)-(hvap/cp)*ptem
          thlvx(i,k) = thlx(i,k)*(1.+fv*qtx(i,k))
        enddo
      enddo
      do k = 1,km1
        do i = 1,im
          dku(i,k)  = 0.
          dkt(i,k)  = 0.
          dktx(i,k) = 0.
          dkux(i,k) = 0.
          dkty(i,k) = 0.
          dkuy(i,k) = 0.
          cku(i,k)  = 0.
          ckt(i,k)  = 0.
          tem       = zi(i,k+1)-zi(i,k)
          radx(i,k) = tem*(swh(i,k)*xmu(i)+hlw(i,k))
        enddo
      enddo
c
      do i=1,im
         flg(i)  = scuflg(i)
      enddo
      do k = 1, km1
        do i=1,im
          if(flg(i).and.zl(i,k).ge.zstblmax) then
             lcld(i)=k
             flg(i)=.false.
          endif
      enddo
      enddo
c
c  compute buoyancy flux
c
      do k = 1, km1
      do i = 1, im
         bf(i,k) = (thvx(i,k+1)-thvx(i,k))*rdzt(i,k)
      enddo
      enddo
c
      do i = 1,im
        govrth(i) = g/theta(i,1)
      enddo
c
      do i=1,im
         beta(i)  = dt / (zi(i,2)-zi(i,1))
      enddo
c
      do i=1,im
         ustar(i) = sqrt(stress(i))
      enddo
c
c  compute the first guess pbl height
c
      do i = 1,im
         sflux(i)  = heat(i) + evap(i)*fv*theta(i,1)
         if(.not.sfcflg(i).or.sflux(i).le.0.0) pblflg(i)=.false.
      enddo
c
      do i=1,im
         flg(i) = .true.
         if(pblflg(i)) then
            flg(i)  = .false.
            sumz(i) = zl(i,1)
            if(scuflg(i)) then
              rbup(i) = thlvx(i,1)
            else
              rbup(i) = thvx(i,1)
            endif
            sumt(i) = rbup(i)*zl(i,1)
         endif
      enddo
      do k = 2, kmpbl
      do i = 1, im
        if(.not.flg(i)) then
          rbdn(i) = rbup(i)
          tem     = zl(i,k)-zl(i,k-1)
          sumz(i) = sumz(i)+tem
          if(scuflg(i)) then
            tem1    = 0.5*(thlvx(i,k)+thlvx(i,k-1))
            rbup(i) = thlvx(i,k)
          else
            tem1    = 0.5*(thvx(i,k)+thvx(i,k-1))
            rbup(i) = thvx(i,k)
          endif
          sumt(i) = sumt(i)+tem1*tem
          thermal(i)= sumt(i)/sumz(i)+cpert
          kpbl(i) = k
          flg(i)  = rbup(i).gt.thermal(i)
        endif
      enddo
      enddo
      do i = 1,im
        if(pblflg(i)) then
          k = kpbl(i)
          if(rbdn(i).ge.thermal(i)) then
             rbint = 0.
          elseif(rbup(i).le.thermal(i)) then
             rbint = 1.
          else
             rbint = (thermal(i)-rbdn(i))/(rbup(i)-rbdn(i))
          endif
          hpbl(i) = zl(i,k-1) + rbint*(zl(i,k)-zl(i,k-1))
          kpbl(i) = kpbl(i) - 1
        endif
      enddo
c
      do i = 1, im
        if(pblflg(i)) then
           hpbl01(i) = sfcfrac*hpbl(i)
           if(scuflg(i)) then
             thermal(i) = thlvx(i,1)
           else
             thermal(i) = thvx(i,1)
           endif
        endif
      enddo
c
c     compute similarity parameters 
c
      do i=1,im
         if(pblflg(i)) then
           hol(i) = max(rbsoil(i)*fm(i)*fm(i)/fh(i),rimin)
           hol(i) = min(hol(i),-zfmin)
c
           hol1 = hol(i)*hpbl(i)/zl(i,1)*sfcfrac
!          phim(i) = (1.-aphi16*hol1)**(-1./4.)
!          phih(i) = (1.-aphi16*hol1)**(-1./2.)
           tem     = 1.0 / (1. - aphi16*hol1)
           phih(i) = sqrt(tem)
           phim(i) = sqrt(phih(i))
           ptem = max(heat(i)+fv*theta(i,1)*evap(i),0.)
           wstar3(i) = govrth(i)*ptem*hpbl(i)
           ust3(i)   = ustar(i)**3.
           wscale(i) = (ust3(i)+wfac*vk*wstar3(i)*sfcfrac)**h1
!          wscale(i) = ustar(i)/phim(i)
!          wscale(i) = min(wscale(i),ustar(i)*aphi16)
           wscale(i) = max(wscale(i),ustar(i)/aphi5)
         endif
      enddo
c
c compute counter-gradient mixing term for heat and moisture
c
      do i = 1,im
         if(pblflg(i)) then
           hgamt(i)  = min(cfac*heat(i)/wscale(i),gamcrt)
           hgamq(i)  = min(cfac*evap(i)/wscale(i),gamcrq)
           vpert     = hgamt(i) + hgamq(i)*fv*theta(i,1)
           vpert     = min(vpert,gamcrt)
           thermal(i)= thermal(i)+max(vpert,0.)
           hgamt(i)  = max(hgamt(i),0.0)
           hgamq(i)  = max(hgamq(i),0.0)
         endif
      enddo
c
c compute large-scale mixing term for momentum
c
      do i = 1,im
        flg(i) = pblflg(i)
        kemx(i)= 1
      enddo
      do k = 1, kmpbl
      do i = 1, im
        if(flg(i).and.zl(i,k).gt.hpbl01(i)) then
          kemx(i) = k
          flg(i)  = .false.
        endif
      enddo
      enddo
      do i = 1, im
        if(pblflg(i)) then
          kk = kpbl(i)
          if(kemx(i).le.1) then
            ptem  = u1(i,1)/zl(i,1)
            ptem1 = v1(i,1)/zl(i,1)
            u01   = ptem*hpbl01(i)
            v01   = ptem1*hpbl01(i)
          else
            tem   = zl(i,kemx(i))-zl(i,kemx(i)-1)
            ptem  = (u1(i,kemx(i))-u1(i,kemx(i)-1))/tem
            ptem1 = (v1(i,kemx(i))-v1(i,kemx(i)-1))/tem
            tem1  = hpbl01(i)-zl(i,kemx(i)-1)
            u01   = u1(i,kemx(i)-1)+ptem*tem1
            v01   = v1(i,kemx(i)-1)+ptem1*tem1
          endif
          delu  = u1(i,kk)-u01
          delv  = v1(i,kk)-v01
          tem2  = sqrt(delu**2+delv**2)
          tem2  = max(tem2,0.1)
          ptem2 = -sfac*ustar(i)*ustar(i)*wstar3(i)
     1                /(wscale(i)**4.)
          hgamu(i) = ptem2*delu/tem2 
          hgamv(i) = ptem2*delv/tem2
          tem  = sqrt(u1(i,kk)**2+v1(i,kk)**2)
          tem1 = sqrt(u01**2+v01**2)
          ptem = tem - tem1
          if(ptem.gt.0.) then
            hgams(i) = -sfac*vk*sfcfrac*wstar3(i)/(wscale(i)**3.)
          else
            hgams(i) = sfac*vk*sfcfrac*wstar3(i)/(wscale(i)**3.)
          endif
        endif
      enddo
c
c  enhance the pbl height by considering the thermal excess
c
      do i=1,im
         flg(i)  = .true.
         if(pblflg(i)) then
           flg(i)  = .false.
           if(scuflg(i)) then
             rbup(i) = thlvx(i,1)
           else
             rbup(i) = thvx(i,1)
           endif
         endif
      enddo
      do k = 2, kmpbl
      do i = 1, im
        if(.not.flg(i)) then
          rbdn(i) = rbup(i)
          if(scuflg(i)) then
            rbup(i) = thlvx(i,k)
          else
            rbup(i) = thvx(i,k)
          endif
          kpblx(i) = k
          flg(i)  = rbup(i).gt.thermal(i)
        endif
      enddo
      enddo
      do i = 1,im
        if(pblflg(i)) then
           k = kpblx(i)
           if(rbdn(i).ge.thermal(i)) then
              rbint = 0.
           elseif(rbup(i).le.thermal(i)) then
              rbint = 1.
           else
              rbint = (thermal(i)-rbdn(i))/(rbup(i)-rbdn(i))
           endif
           hpblx(i) = zl(i,k-1) + rbint*(zl(i,k)-zl(i,k-1))
           kpblx(i) = kpblx(i) - 1
           if(hpblx(i).gt.hpbl(i)) then
              hpbl(i) = hpblx(i)
              kpbl(i) = kpblx(i)
              if(kpbl(i).le.1) pblflg(i)=.false.
           endif
        endif
      enddo
c
c  look for stratocumulus-topped pbl
c
      do i = 1, im
        flg(i)=scuflg(i)
      enddo
      do k = kmpbl,1,-1
      do i = 1, im
        if(flg(i).and.k.le.lcld(i)) then
          if(qlx(i,k).ge.qlcr) then
             kcld(i)=k
             flg(i)=.false.
          endif
        endif
      enddo
      enddo
      do i = 1, im
        if(scuflg(i).and.kcld(i).eq.km1) scuflg(i)=.false.
      enddo
c
      do i = 1, im
        flg(i)=scuflg(i)
      enddo
      do k = kmpbl,1,-1
      do i = 1, im
        if(flg(i).and.k.le.kcld(i)) then
          if(qlx(i,k).ge.qlcr) then
            if(radx(i,k).lt.radmin(i)) then
              radmin(i)=radx(i,k)
              krad(i)=k
            endif
          else
            flg(i)=.false.
          endif
        endif
      enddo
      enddo
      do i = 1, im
        if(scuflg(i).and.krad(i).eq.km1) scuflg(i)=.false.
        if(scuflg(i).and.krad(i).le.1) scuflg(i)=.false.
        if(scuflg(i).and.radmin(i).ge.0.) scuflg(i)=.false.
      enddo
c
      do i = 1, im
        flg(i)=scuflg(i)
      enddo
      do k = kmpbl,2,-1
      do i = 1, im
        if(flg(i).and.k.le.krad(i)) then
          if(qlx(i,k).ge.qlcr) then
            icld(i)=icld(i)+1
          else
            flg(i)=.false.
          endif
        endif
      enddo
      enddo
      do i = 1, im
        if(scuflg(i).and.icld(i).lt.2) scuflg(i)=.false.
      enddo
c
      do i = 1, im
        if(scuflg(i)) then
           hrad(i) = zi(i,krad(i)+1)
           hradm(i)= zl(i,krad(i))
        endif
      enddo
c
      do i = 1, im
        if(scuflg(i).and.hrad(i).lt.200.) scuflg(i)=.false.
      enddo
c
      do i = 1, im
        if(scuflg(i)) then
          k    = krad(i)
          tem  = zi(i,k+1)-zi(i,k)
          tem1 = cldtime*radmin(i)/tem
          thlvx1(i) = thlvx(i,k)+tem1
          if(thlvx1(i).gt.thlvx(i,k-1)) scuflg(i)=.false.
        endif
      enddo
c 
      do i = 1, im
         flg(i)=scuflg(i)
      enddo
      do k = kmpbl,1,-1
      do i = 1, im
        if(flg(i).and.k.le.krad(i))then
          if(thlvx1(i).le.thlvx(i,k))then
             tem=zi(i,k+1)-zi(i,k)
             zd(i)=zd(i)+tem
          else
             flg(i)=.false.
          endif
        endif
      enddo
      enddo
      do i = 1, im
        if(scuflg(i))then
          zd(i) = min(zd(i),hrad(i))
          tem   = govrth(i)*zd(i)*(-radmin(i))
          vrad(i)= tem**h1
        endif
      enddo
c
      do i = 1, im
        if(scuflg(i).and.pblflg(i)) then
          if(hpbl(i).ge.hradm(i)) then
             hpbl(i)=hrad(i)
             kpbl(i)=krad(i)
          endif
        endif
      enddo
c
c     compute inverse prandtl number
c
      do i = 1, im
        if(pblflg(i)) then
          tem = phih(i)/phim(i)+cfac*vk*sfcfrac
          prinv(i) = (1.0-hgams(i))/tem
          prinv(i) = min(prinv(i),prmax)
          prinv(i) = max(prinv(i),prmin)
        endif
      enddo
c
c     compute entrainment flux at pbl top
c
      do i = 1, im
        if(pblflg(i)) then
           ptem=(theta(i,1)/g)*ust3(i)
           entflx(i)=entfac*(sflux(i)+5.*ptem/hpbl(i))
        endif
      enddo
c
c     compute diffusion coefficients below pbl
c
      do k = 1, kmpbl
      do i=1,im
         if(pblflg(i).and.k.lt.kpbl(i)) then
!           zfac = max((1.-(zi(i,k+1)-zl(i,1))/
!    1             (hpbl(i)-zl(i,1))), zfmin)
            zfac = max((1.-zi(i,k+1)/hpbl(i)), zfmin)
            dku(i,k) = wscale(i)*vk*zi(i,k+1)
     1                 *zfac**pfac
!           dku(i,k) = xkzo(i,k)+wscale(i)*vk*zi(i,k+1)
!    1                 *zfac**pfac
            dkt(i,k) = dku(i,k)*prinv(i)
            dku(i,k) = min(dku(i,k),dkmax)
            dku(i,k) = max(dku(i,k),xkzo(i,k))
            dkt(i,k) = min(dkt(i,k),dkmax)
            dkt(i,k) = max(dkt(i,k),xkzo(i,k))
            dktx(i,k)= dkt(i,k)
            dkux(i,k)= dku(i,k)
         endif
      enddo
      enddo
c
      do i = 1, im
        if(pblflg(i)) then
           k = kpbl(i)
           if(bf(i,k).gt.0.) then
             ptem = max(bf(i,k),tdzmin)
             dkt(i,k) = entflx(i)/ptem
             dkt(i,k) = min(dkt(i,k),dkmax)
             dku(i,k) = dkt(i,k)
           endif
        endif
      enddo
c
c compute diffusion coefficients based on local scheme,
c which are applied to nighttime stable boundary layer and
c free atmosphere over pbl in the daytime unstable condition 
c
      do k = 1, km1
         do i=1,im
!!          if(k.ge.kpbl(i)) then
               rdz  = rdzt(i,k)
               dw2  = ((u1(i,k)-u1(i,k+1))**2
     &              + (v1(i,k)-v1(i,k+1))**2)
               shr2 = max(dw2,dw2min)*rdz*rdz
!              if(qlx(i,k).ge.qlcr.and.qlx(i,k+1).ge.qlcr) then
               if(qlx(i,k).ge.qlcr.or.qlx(i,k+1).ge.qlcr) then
                 ti   = 2./(t1(i,k)+t1(i,k+1))
                 tem  = .5*(max(q1(i,k,1),qmin)+max(q1(i,k+1,1),qmin))
                 tem1 = alpri*tem*ti
                 tem2 = chiri*tem*ti*ti
                 ptem = (tem2-tem1)/(1.+tem2)
                 ptem1= bf(i,k)/thvx(i,k+1)-g*ptem*ti/cp
                 bvf2 = (1.+tem1)*g*ptem1
               else
                 bvf2 = g*bf(i,k)/thvx(i,k+1)
               endif 
               ri   = max(bvf2/shr2,rimin)
               zk   = vk*zi(i,k+1)
               if(ri.lt.0.) then ! unstable regime
                  rl2      = zk*rlamun/(rlamun+zk)
                  dk       = rl2*rl2*sqrt(shr2)
                  sri      = sqrt(-ri)
!                 dkuy(i,k) = xkzo(i,k) + dk*(1+8.*(-ri)/(1+1.746*sri))
!                 dkty(i,k) = xkzo(i,k) + dk*(1+8.*(-ri)/(1+1.286*sri))
                  dkuy(i,k) = dk*(1+8.*(-ri)/(1+1.746*sri))
                  dkty(i,k) = dk*(1+8.*(-ri)/(1+1.286*sri))
               else             ! stable regime
                  rl2      = zk*rlam/(rlam+zk)
!!                tem      = rlam * sqrt(0.01*prsi(i,k))
!!                rl2      = zk*tem/(tem+zk)
                  dk       = rl2*rl2*sqrt(shr2)
!                 dkty(i,k)= xkzo(i,k) + dk/(1+5.*ri)**2
                  dkty(i,k)= dk/(1.+5.*ri)**2
                  prnum = 1.0 + 2.1*ri
                  prnum = min(prnum,prmax)
!                 dkuy(i,k)= (dkty(i,k)-xkzo(i,k))*prnum + xkzo(i,k)
                  dkuy(i,k)= dkty(i,k)*prnum
               endif
c
               dkuy(i,k) = min(dkuy(i,k),dkmax)
               dkuy(i,k) = max(dkuy(i,k),xkzo(i,k))
               dkty(i,k) = min(dkty(i,k),dkmax)
               dkty(i,k) = max(dkty(i,k),xkzo(i,k))
c
!!          endif
c
         enddo
      enddo
c
c  compute diffusion coefficients for cloud-top driven diffusion
c
      do i = 1, im
        if(scuflg(i)) then
           k = krad(i)
           if(bf(i,k).gt.0.) then
             tem1  = max(bf(i,k),tdzmin)
             ckt(i,k) = -rentfac*radmin(i)/tem1
             cku(i,k) = ckt(i,k)
           endif
        endif
      enddo
c
      do k = 1, kmpbl
         do i=1,im
            if(scuflg(i).and.k.lt.krad(i)) then
               tem1=hrad(i)-zd(i)
               tem2=zi(i,k+1)-tem1
               if(tem2.gt.0.) then
                  ptem= tem2/zd(i)
                  if(ptem.ge.1.) ptem= 1.
                  ptem= tem2*ptem*sqrt(1.-ptem)
                  ckt(i,k) = radfac*vk*vrad(i)*ptem
                  cku(i,k) = 0.75*ckt(i,k)
                  ckt(i,k) = max(ckt(i,k),dkmin)
                  ckt(i,k) = min(ckt(i,k),dkmax)
                  cku(i,k) = max(cku(i,k),dkmin)
                  cku(i,k) = min(cku(i,k),dkmax)
               endif
            endif
         enddo
      enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      do k = 1, kmpbl
        do i=1,im
          if(scuflg(i)) then
             dkt(i,k) = dkt(i,k)+ckt(i,k)
             dku(i,k) = dku(i,k)+cku(i,k)
             dkt(i,k) = min(dkt(i,k),dkmax)
             dku(i,k) = min(dku(i,k),dkmax)
          endif
        enddo
      enddo
c
      do k = 1, km1
        do i=1,im
           dkt(i,k) = max(dkt(i,k),dkty(i,k))
           dku(i,k) = max(dku(i,k),dkuy(i,k))
        enddo
      enddo
c
c     compute tridiagonal matrix elements for heat and moisture
c
      do i=1,im
         ad(i,1) = 1.
         a1(i,1) = t1(i,1)   + beta(i) * heat(i)
         a2(i,1) = q1(i,1,1) + beta(i) * evap(i)
      enddo
      if(ntrac.ge.2) then
        do k = 2, ntrac
          is = (k-1) * km
          do i = 1, im
            a2(i,1+is) = q1(i,1,k)
          enddo
        enddo
      endif
c
      do k = 1,km1
        do i = 1,im
          dtodsd = dt/del(i,k)
          dtodsu = dt/del(i,k+1)
          dsig   = prsl(i,k)-prsl(i,k+1)
!         rdz    = rdzt(i,k)*2./(t1(i,k)+t1(i,k+1))
          rdz    = rdzt(i,k)
          tem1   = dsig * dkt(i,k) * rdz
          if(pblflg(i).and.k.lt.kpbl(i)) then
!            dsdzt = dsig*dkt(i,k)*rdz*(gocp-hgamt(i)/hpbl(i))
!            dsdzq = dsig*dkt(i,k)*rdz*(-hgamq(i)/hpbl(i))
             ptem1 = dsig * dktx(i,k) * rdz
             tem   = 1.0 / hpbl(i)
             dsdzt = tem1 * gocp - ptem1*hgamt(i)*tem
             dsdzq = ptem1 * (-hgamq(i)*tem)
             a2(i,k)   = a2(i,k)+dtodsd*dsdzq
             a2(i,k+1) = q1(i,k+1,1)-dtodsu*dsdzq
          else
!            dsdzt = dsig*dkt(i,k)*rdz*(gocp)
             dsdzt = tem1 * gocp
             a2(i,k+1) = q1(i,k+1,1)
          endif
!         dsdz2 = dsig*dkt(i,k)*rdz*rdz
          dsdz2     = tem1 * rdz
          au(i,k)   = -dtodsd*dsdz2
          al(i,k)   = -dtodsu*dsdz2
          ad(i,k)   = ad(i,k)-au(i,k)
          ad(i,k+1) = 1.-al(i,k)
          a1(i,k)   = a1(i,k)+dtodsd*dsdzt
          a1(i,k+1) = t1(i,k+1)-dtodsu*dsdzt
        enddo
      enddo
      if(ntrac.ge.2) then
        do kk = 2, ntrac
          is = (kk-1) * km
          do k = 1, km1
            do i = 1, im
              a2(i,k+1+is) = q1(i,k+1,kk)
            enddo
          enddo
        enddo
      endif
c
c     solve tridiagonal problem for heat and moisture
c
      call tridin(im,km,ntrac,al,ad,au,a1,a2,au,a1,a2)
c
c     recover tendencies of heat and moisture
c
      do  k = 1,km
         do i = 1,im
            ttend      = (a1(i,k)-t1(i,k))*rdt
            qtend      = (a2(i,k)-q1(i,k,1))*rdt
            tau(i,k)   = tau(i,k)+ttend
            rtg(i,k,1) = rtg(i,k,1)+qtend
            dtsfc(i)   = dtsfc(i)+cont*del(i,k)*ttend
            dqsfc(i)   = dqsfc(i)+conq*del(i,k)*qtend
         enddo
      enddo
      if(ntrac.ge.2) then
        do kk = 2, ntrac
          is = (kk-1) * km
          do k = 1, km 
            do i = 1, im
              qtend = (a2(i,k+is)-q1(i,k,kk))*rdt
              rtg(i,k,kk) = rtg(i,k,kk)+qtend
            enddo
          enddo
        enddo
      endif
c
c     compute tridiagonal matrix elements for momentum
c
      do i=1,im
         ad(i,1) = 1.0 + beta(i) * stress(i) / spd1(i)
         a1(i,1) = u1(i,1)
         a2(i,1) = v1(i,1)
      enddo
c
      do k = 1,km1
        do i=1,im
          dtodsd = dt/del(i,k)
          dtodsu = dt/del(i,k+1)
          dsig   = prsl(i,k)-prsl(i,k+1)
          rdz    = rdzt(i,k)
          tem1   = dsig*dku(i,k)*rdz
          if(pblflg(i).and.k.lt.kpbl(i))then
            ptem1 = dsig*dkux(i,k)*rdz
            dsdzu = ptem1*(-hgamu(i)/hpbl(i))
            dsdzv = ptem1*(-hgamv(i)/hpbl(i))
            a1(i,k)   = a1(i,k)+dtodsd*dsdzu
            a1(i,k+1) = u1(i,k+1)-dtodsu*dsdzu
            a2(i,k)   = a2(i,k)+dtodsd*dsdzv
            a2(i,k+1) = v1(i,k+1)-dtodsu*dsdzv
          else
            a1(i,k+1) = u1(i,k+1)
            a2(i,k+1) = v1(i,k+1)
          endif
!         dsdz2     = dsig*dku(i,k)*rdz*rdz
          dsdz2     = tem1*rdz
          au(i,k)   = -dtodsd*dsdz2
          al(i,k)   = -dtodsu*dsdz2
          ad(i,k)   = ad(i,k)-au(i,k)
          ad(i,k+1) = 1.-al(i,k)
        enddo
      enddo
c
c     solve tridiagonal problem for momentum
c
      call tridi2(im,km,al,ad,au,a1,a2,au,a1,a2)
c
c     recover tendencies of momentum
c
      do k = 1,km
         do i = 1,im
            utend    = (a1(i,k)-u1(i,k))*rdt
            vtend    = (a2(i,k)-v1(i,k))*rdt
            du(i,k)  = du(i,k)  + utend
            dv(i,k)  = dv(i,k)  + vtend
            dusfc(i) = dusfc(i) + conw*del(i,k)*utend
            dvsfc(i) = dvsfc(i) + conw*del(i,k)*vtend
         enddo
      enddo
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c  estimate the pbl height for diagnostic purpose
c
      do i = 1, im
         if(scuflg(i)) then
            thermal(i) = thlvx(i,1)
         else
            thermal(i) = thvx(i,1)
         endif
         flg(i) = .false.
         rbup(i)= rbsoil(i)
      enddo
      do k = 2, kmpbl
      do i = 1, im
        if(.not.flg(i)) then
          rbdn(i)   = rbup(i)
          spdk2     = max((u1(i,k)**2+v1(i,k)**2),1.)
          if(scuflg(i)) then
             rbup(i)   = (thlvx(i,k)-thermal(i))*
     &                   (g*zl(i,k)/thlvx(i,1))/spdk2
          else
             rbup(i)   = (thvx(i,k)-thermal(i))*
     &                   (g*zl(i,k)/thvx(i,1))/spdk2
          endif
          kpbl(i)= k
          flg(i) = rbup(i).gt.rbcr
        endif
      enddo
      enddo
c
      do i = 1,im
         k = kpbl(i)
         if(rbdn(i).ge.rbcr) then
            rbint = 0.
         elseif(rbup(i).le.rbcr) then
            rbint = 1.
         else
            rbint = (rbcr-rbdn(i))/(rbup(i)-rbdn(i))
         endif
         hpbl(i) = zl(i,k-1) + rbint*(zl(i,k)-zl(i,k-1))
         if(hpbl(i).lt.zi(i,kpbl(i))) kpbl(i) = kpbl(i) - 1
      enddo
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      return
      end
