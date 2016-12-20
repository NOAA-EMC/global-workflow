!!!!!  ==========================================================  !!!!!
! subroutine 'moninedmf' computes subgrid vertical mixing by turbulence
! 
! for the convective boundary layer, the scheme adopts eddy-diffusion
!  mass-flux (edmf) parameterization (siebesma et al., 2007) to take into 
!  account nonlocal transport by large eddies. to reduce the tropical wind rmse, 
!  a hybrid scheme is used, in which the edmf scheme is used only for strongly 
!  unstable pbl while the current operational vertical diffusion scheme is called 
!  for the weakly unstable pbl.  
!
      subroutine moninedmf(ix,im,km,ntrac,ntcw,dv,du,tau,rtg,
     &   u1,v1,t1,q1,swh,hlw,xmu,
     &   psk,rbsoil,zorl,u10m,v10m,fm,fh,
     &   tsea,qss,heat,evap,stress,spd1,kpbl,
     &   prsi,del,prsl,prslk,phii,phil,delt,dspheat,
     &   dusfc,dvsfc,dtsfc,dqsfc,hpbl,hgamt,hgamq,dkt,
     &   kinver,xkzm_m,xkzm_h,xkzm_s,lprnt,ipr)
!
      use machine  , only : kind_phys
      use funcphys , only : fpvs
      use physcons, grav => con_g, rd => con_rd, cp => con_cp
     &,             hvap => con_hvap, fv => con_fvirt
      implicit none
!
!     arguments
!
      logical lprnt
      integer ipr
      integer ix, im, km, ntrac, ntcw, kpbl(im), kinver(im)
!
      real(kind=kind_phys) delt, xkzm_m, xkzm_h, xkzm_s
      real(kind=kind_phys) dv(im,km),     du(im,km),
     &                     tau(im,km),    rtg(im,km,ntrac),
     &                     u1(ix,km),     v1(ix,km),
     &                     t1(ix,km),     q1(ix,km,ntrac),
     &                     swh(ix,km),    hlw(ix,km),
     &                     xmu(im),       psk(im),
     &                     rbsoil(im),    zorl(im),
     &                     u10m(im),      v10m(im),
     &                     fm(im),        fh(im),
     &                     tsea(im),      qss(im),
     &                                    spd1(im),
     &                     prsi(ix,km+1), del(ix,km),
     &                     prsl(ix,km),   prslk(ix,km),
     &                     phii(ix,km+1), phil(ix,km),
     &                     dusfc(im),     dvsfc(im),
     &                     dtsfc(im),     dqsfc(im),
     &                     hpbl(im),      hpblx(im),
     &                     hgamt(im),     hgamq(im)
!
      logical dspheat
!          flag for tke dissipative heating
!
!    locals
!
      integer i,iprt,is,iun,k,kk,km1,kmpbl,latd,lond
      integer lcld(im),icld(im),kcld(im),krad(im)
      integer kx1(im), kpblx(im)
!
!     real(kind=kind_phys) betaq(im), betat(im),   betaw(im),
      real(kind=kind_phys) evap(im),  heat(im),    phih(im),
     &                     phim(im),  rbdn(im),    rbup(im),
     &                     stress(im),beta(im),    sflux(im),
     &                     z0(im),    crb(im),     wstar(im),
     &                     zol(im),   ustmin(im),  ustar(im),
     &                     thermal(im),wscale(im), wscaleu(im)
!
      real(kind=kind_phys) theta(im,km),thvx(im,km),  thlvx(im,km),
     &                     qlx(im,km),  thetae(im,km),
     &                     qtx(im,km),  bf(im,km-1),  diss(im,km),
     &                     radx(im,km-1),
     &                     govrth(im),  hrad(im),
!    &                     hradm(im),   radmin(im),   vrad(im),
     &                     radmin(im),  vrad(im),
     &                     zd(im),      zdd(im),      thlvx1(im)
!
      real(kind=kind_phys) rdzt(im,km-1),dktx(im,km-1),
     &                     zi(im,km+1),  zl(im,km),    xkzo(im,km-1),
     &                     dku(im,km-1), dkt(im,km-1), xkzmo(im,km-1),
     &                     cku(im,km-1), ckt(im,km-1),
     &                     ti(im,km-1),  shr2(im,km-1),
     &                     al(im,km-1),  ad(im,km),
     &                     au(im,km-1),  a1(im,km), 
     &                     a2(im,km*ntrac)
!
      real(kind=kind_phys) tcko(im,km),  qcko(im,km,ntrac),
     &                     ucko(im,km),  vcko(im,km),  xmf(im,km)
!
      real(kind=kind_phys) prinv(im), rent(im)
!
      logical  pblflg(im), sfcflg(im), scuflg(im), flg(im)
      logical  ublflg(im), pcnvflg(im)
!
!  pcnvflg: true for convective(strongly unstable) pbl
!  ublflg: true for unstable but not convective(strongly unstable) pbl
!
      real(kind=kind_phys) aphi16,  aphi5,  bvf2,   wfac,
     &                     cfac,    conq,   cont,   conw,
     &                     dk,      dkmax,  dkmin,
     &                     dq1,     dsdz2,  dsdzq,  dsdzt,
     &                     dsdzu,   dsdzv,
     &                     dsig,    dt2,    dthe1,  dtodsd,
     &                     dtodsu,  dw2,    dw2min, g,
     &                     gamcrq,  gamcrt, gocp,
     &                     gravi,   f0,
     &                     prnum,   prmax,  prmin,  pfac,  crbcon,
     &                     qmin,    tdzmin, qtend,  crbmin,crbmax,
     &                     rbint,   rdt,    rdz,    qlmin, 
     &                     ri,      rimin,  rl2,    rlam,  rlamun,
     &                     rone,    rzero,  sfcfrac,
     &                     spdk2,   sri,    zol1,   zolcr, zolcru,
     &                     robn,    ttend, 
     &                     utend,   vk,     vk2,
     &                     ust3,    wst3,
     &                     vtend,   zfac,   vpert,  cteit,
     &                     rentf1,  rentf2, radfac,
     &                     zfmin,   zk,     tem,    tem1,  tem2,
     &                     xkzm,    xkzmu,  xkzminv,
     &                     ptem,    ptem1,  ptem2, tx1(im), tx2(im)
!
      real(kind=kind_phys) zstblmax,h1,     h2,     qlcr,  actei,
     &                     cldtime
cc
      parameter(gravi=1.0/grav)
      parameter(g=grav)
      parameter(gocp=g/cp)
      parameter(cont=cp/g,conq=hvap/g,conw=1.0/g)               ! for del in pa
!     parameter(cont=1000.*cp/g,conq=1000.*hvap/g,conw=1000./g) ! for del in kpa
      parameter(rlam=30.0,vk=0.4,vk2=vk*vk)
      parameter(prmin=0.25,prmax=4.,zolcr=0.2,zolcru=-0.5)
      parameter(dw2min=0.0001,dkmin=0.0,dkmax=1000.,rimin=-100.)
      parameter(crbcon=0.25,crbmin=0.15,crbmax=0.35)
      parameter(wfac=7.0,cfac=6.5,pfac=2.0,sfcfrac=0.1)
!     parameter(qmin=1.e-8,xkzm=1.0,zfmin=1.e-8,aphi5=5.,aphi16=16.)
      parameter(qmin=1.e-8,         zfmin=1.e-8,aphi5=5.,aphi16=16.)
      parameter(tdzmin=1.e-3,qlmin=1.e-12,f0=1.e-4)
      parameter(h1=0.33333333,h2=0.66666667)
      parameter(cldtime=500.,xkzminv=0.3)
!     parameter(cldtime=500.,xkzmu=3.0,xkzminv=0.3)
!     parameter(gamcrt=3.,gamcrq=2.e-3,rlamun=150.0)
      parameter(gamcrt=3.,gamcrq=0.,rlamun=150.0)
      parameter(rentf1=0.2,rentf2=1.0,radfac=0.85)
      parameter(iun=84)
!
!     parameter (zstblmax = 2500., qlcr=1.0e-5)
!     parameter (zstblmax = 2500., qlcr=3.0e-5)
!     parameter (zstblmax = 2500., qlcr=3.5e-5)
!     parameter (zstblmax = 2500., qlcr=1.0e-4)
      parameter (zstblmax = 2500., qlcr=3.5e-5)
!     parameter (actei = 0.23)
      parameter (actei = 0.7)
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
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     compute preliminary variables
!
      if (ix .lt. im) stop
!
!     iprt = 0
!     if(iprt.eq.1) then
!cc   latd = 0
!     lond = 0
!     else
!cc   latd = 0
!     lond = 0
!     endif
!
      dt2   = delt
      rdt   = 1. / dt2
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
!
      do k = 1,km1
        do i=1,im
          rdzt(i,k) = 1.0 / (zl(i,k+1) - zl(i,k))
        enddo
      enddo
!
      do i=1,im
        kx1(i) = 1
        tx1(i) = 1.0 / prsi(i,1)
        tx2(i) = tx1(i)
      enddo
      do k = 1,km1
        do i=1,im
          xkzo(i,k)  = 0.0
          xkzmo(i,k) = 0.0
          if (k < kinver(i)) then
!                                  vertical background diffusivity
            ptem      = prsi(i,k+1) * tx1(i)
            tem1      = 1.0 - ptem
            tem1      = tem1 * tem1 * 10.0
            xkzo(i,k) = xkzm_h * min(1.0, exp(-tem1))

!                                  vertical background diffusivity for momentum
            if (ptem >= xkzm_s) then
              xkzmo(i,k) = xkzm_m
              kx1(i)     = k + 1
            else
              if (k == kx1(i) .and. k > 1) tx2(i) = 1.0 / prsi(i,k)
              tem1 = 1.0 - prsi(i,k+1) * tx2(i)
              tem1 = tem1 * tem1 * 5.0
              xkzmo(i,k) = xkzm_m * min(1.0, exp(-tem1))
            endif
          endif
        enddo
      enddo
!     if (lprnt) then
!       print *,' xkzo=',(xkzo(ipr,k),k=1,km1)
!       print *,' xkzmo=',(xkzmo(ipr,k),k=1,km1)
!     endif
!
!  diffusivity in the inversion layer is set to be xkzminv (m^2/s)
!
      do k = 1,kmpbl
        do i=1,im
!         if(zi(i,k+1) > 200..and.zi(i,k+1) < zstblmax) then
          if(zi(i,k+1) > 250.) then
            tem1 = (t1(i,k+1)-t1(i,k)) * rdzt(i,k)
            if(tem1 > 1.e-5) then
               xkzo(i,k)  = min(xkzo(i,k),xkzminv)
            endif
          endif
        enddo
      enddo
!
      do i = 1,im
         z0(i)    = 0.01 * zorl(i)
         dusfc(i) = 0.
         dvsfc(i) = 0.
         dtsfc(i) = 0.
         dqsfc(i) = 0.
         wscale(i)= 0.
         wscaleu(i)= 0.
         kpbl(i)  = 1
         hpbl(i)  = zi(i,1)
         hpblx(i) = zi(i,1)
         pblflg(i)= .true.
         sfcflg(i)= .true.
         if(rbsoil(i) > 0.) sfcflg(i) = .false.
         ublflg(i)= .false.
         pcnvflg(i)= .false.
         scuflg(i)= .true.
         if(scuflg(i)) then
           radmin(i)= 0.
           rent(i)  = rentf1
           hrad(i)  = zi(i,1)
!          hradm(i) = zi(i,1)
           krad(i)  = 1
           icld(i)  = 0
           lcld(i)  = km1
           kcld(i)  = km1
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
          thetae(i,k)= theta(i,k)*(1.+ptem1)
          thvx(i,k)  = theta(i,k)*(1.+fv*max(q1(i,k,1),qmin)-ptem)
          ptem2      = theta(i,k)-(hvap/cp)*ptem
          thlvx(i,k) = ptem2*(1.+fv*qtx(i,k))
        enddo
      enddo
      do k = 1,km1
        do i = 1,im
          dku(i,k)  = 0.
          dkt(i,k)  = 0.
          dktx(i,k) = 0.
          cku(i,k)  = 0.
          ckt(i,k)  = 0.
          tem       = zi(i,k+1)-zi(i,k)
          radx(i,k) = tem*(swh(i,k)*xmu(i)+hlw(i,k))
        enddo
      enddo
!
      do i=1,im
         flg(i)  = scuflg(i)
      enddo
      do k = 1, km1
        do i=1,im
          if(flg(i).and.zl(i,k) >= zstblmax) then
             lcld(i)=k
             flg(i)=.false.
          endif
      enddo
      enddo
!
!  compute virtual potential temp gradient (bf) and winshear square
!
      do k = 1, km1
      do i = 1, im
         rdz  = rdzt(i,k)
         bf(i,k) = (thvx(i,k+1)-thvx(i,k))*rdz
         ti(i,k) = 2./(t1(i,k)+t1(i,k+1))
         dw2  = (u1(i,k)-u1(i,k+1))**2
     &        + (v1(i,k)-v1(i,k+1))**2
         shr2(i,k) = max(dw2,dw2min)*rdz*rdz
      enddo
      enddo
!
      do i = 1,im
        govrth(i) = g/theta(i,1)
      enddo
!
      do i=1,im
         beta(i)  = dt2 / (zi(i,2)-zi(i,1))
      enddo
!
      do i=1,im
         ustar(i) = sqrt(stress(i))
      enddo
!
      do i = 1,im
         sflux(i)  = heat(i) + evap(i)*fv*theta(i,1)
         if(.not.sfcflg(i) .or. sflux(i) <= 0.) pblflg(i)=.false.
      enddo
!
!  compute the pbl height
!
      do i=1,im
         flg(i) = .false.
         rbup(i) = rbsoil(i)
!
         if(pblflg(i)) then
           thermal(i) = thvx(i,1)
           crb(i) = crbcon
         else
           thermal(i) = tsea(i)*(1.+fv*max(q1(i,1,1),qmin))
           tem = sqrt(u10m(i)**2+v10m(i)**2)
           tem = max(tem, 1.)
           robn = tem / (f0 * z0(i))
           tem1 = 1.e-7 * robn
           crb(i) = 0.16 * (tem1 ** (-0.18))
           crb(i) = max(min(crb(i), crbmax), crbmin)
         endif
      enddo
      do k = 1, kmpbl
      do i = 1, im
        if(.not.flg(i)) then
          rbdn(i) = rbup(i)
          spdk2   = max((u1(i,k)**2+v1(i,k)**2),1.)
          rbup(i) = (thvx(i,k)-thermal(i))*
     &              (g*zl(i,k)/thvx(i,1))/spdk2
          kpbl(i) = k
          flg(i)  = rbup(i) > crb(i)
        endif
      enddo
      enddo
      do i = 1,im
        if(kpbl(i) > 1) then
          k = kpbl(i)
          if(rbdn(i) >= crb(i)) then
            rbint = 0.
          elseif(rbup(i) <= crb(i)) then
            rbint = 1.
          else
            rbint = (crb(i)-rbdn(i))/(rbup(i)-rbdn(i))
          endif
          hpbl(i) = zl(i,k-1) + rbint*(zl(i,k)-zl(i,k-1))
          if(hpbl(i) < zi(i,kpbl(i))) kpbl(i) = kpbl(i) - 1
        else
          hpbl(i) = zl(i,1)
          kpbl(i) = 1
        endif
        kpblx(i) = kpbl(i)
        hpblx(i) = hpbl(i)
      enddo
!
!  compute similarity parameters 
!
      do i=1,im
         zol(i) = max(rbsoil(i)*fm(i)*fm(i)/fh(i),rimin)
         if(sfcflg(i)) then
           zol(i) = min(zol(i),-zfmin)
         else
           zol(i) = max(zol(i),zfmin)
         endif
         zol1 = zol(i)*sfcfrac*hpbl(i)/zl(i,1)
         if(sfcflg(i)) then
!          phim(i) = (1.-aphi16*zol1)**(-1./4.)
!          phih(i) = (1.-aphi16*zol1)**(-1./2.)
           tem     = 1.0 / (1. - aphi16*zol1)
           phih(i) = sqrt(tem)
           phim(i) = sqrt(phih(i))
         else
           phim(i) = 1. + aphi5*zol1
           phih(i) = phim(i)
         endif
         wscale(i) = ustar(i)/phim(i)
         ustmin(i) = ustar(i)/aphi5
         wscale(i) = max(wscale(i),ustmin(i))
      enddo
      do i=1,im
        if(pblflg(i)) then
          if(zol(i) < zolcru .and. kpbl(i) > 1) then
            pcnvflg(i) = .true.
          else
            ublflg(i) = .true.
          endif
          wst3 = govrth(i)*sflux(i)*hpbl(i)
          wstar(i)= wst3**h1
          ust3 = ustar(i)**3.
          wscaleu(i) = (ust3+wfac*vk*wst3*sfcfrac)**h1
          wscaleu(i) = max(wscaleu(i),ustmin(i))
        endif
      enddo
!
! compute counter-gradient mixing term for heat and moisture
!
      do i = 1,im
         if(ublflg(i)) then
           hgamt(i)  = min(cfac*heat(i)/wscaleu(i),gamcrt)
           hgamq(i)  = min(cfac*evap(i)/wscaleu(i),gamcrq)
           vpert     = hgamt(i) + hgamq(i)*fv*theta(i,1)
           vpert     = min(vpert,gamcrt)
           thermal(i)= thermal(i)+max(vpert,0.)
           hgamt(i)  = max(hgamt(i),0.0)
           hgamq(i)  = max(hgamq(i),0.0)
         endif
      enddo
!
!  enhance the pbl height by considering the thermal excess
!
      do i=1,im
         flg(i)  = .true.
         if(ublflg(i)) then
           flg(i)  = .false.
           rbup(i) = rbsoil(i)
         endif
      enddo
      do k = 2, kmpbl
      do i = 1, im
        if(.not.flg(i)) then
          rbdn(i) = rbup(i)
          spdk2   = max((u1(i,k)**2+v1(i,k)**2),1.)
          rbup(i) = (thvx(i,k)-thermal(i))*
     &              (g*zl(i,k)/thvx(i,1))/spdk2
          kpbl(i) = k
          flg(i)  = rbup(i) > crb(i)
        endif
      enddo
      enddo
      do i = 1,im
        if(ublflg(i)) then
           k = kpbl(i)
           if(rbdn(i) >= crb(i)) then
              rbint = 0.
           elseif(rbup(i) <= crb(i)) then
              rbint = 1.
           else
              rbint = (crb(i)-rbdn(i))/(rbup(i)-rbdn(i))
           endif
           hpbl(i) = zl(i,k-1) + rbint*(zl(i,k)-zl(i,k-1))
           if(hpbl(i) < zi(i,kpbl(i))) kpbl(i) = kpbl(i) - 1
           if(kpbl(i) <= 1) then
              ublflg(i) = .false.
              pblflg(i) = .false.
           endif
        endif
      enddo
!
!  look for stratocumulus
!
      do i = 1, im
        flg(i)=scuflg(i)
      enddo
      do k = kmpbl,1,-1
      do i = 1, im
        if(flg(i) .and. k <= lcld(i)) then
          if(qlx(i,k).ge.qlcr) then
             kcld(i)=k
             flg(i)=.false.
          endif
        endif
      enddo
      enddo
      do i = 1, im
        if(scuflg(i) .and. kcld(i)==km1) scuflg(i)=.false.
      enddo
!
      do i = 1, im
        flg(i)=scuflg(i)
      enddo
      do k = kmpbl,1,-1
      do i = 1, im
        if(flg(i) .and. k <= kcld(i)) then
          if(qlx(i,k) >= qlcr) then
            if(radx(i,k) < radmin(i)) then
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
        if(scuflg(i) .and. krad(i) <= 1) scuflg(i)=.false.
        if(scuflg(i) .and. radmin(i)>=0.) scuflg(i)=.false.
      enddo
!
      do i = 1, im
        flg(i)=scuflg(i)
      enddo
      do k = kmpbl,2,-1
      do i = 1, im
        if(flg(i) .and. k <= krad(i)) then
          if(qlx(i,k) >= qlcr) then
            icld(i)=icld(i)+1
          else
            flg(i)=.false.
          endif
        endif
      enddo
      enddo
      do i = 1, im
        if(scuflg(i) .and. icld(i) < 1) scuflg(i)=.false.
      enddo
!
      do i = 1, im
        if(scuflg(i)) then
           hrad(i) = zi(i,krad(i)+1)
!          hradm(i)= zl(i,krad(i))
        endif
      enddo
!
      do i = 1, im
        if(scuflg(i) .and. hrad(i)<zi(i,2)) scuflg(i)=.false.
      enddo
!
      do i = 1, im
        if(scuflg(i)) then
          k    = krad(i)
          tem  = zi(i,k+1)-zi(i,k)
          tem1 = cldtime*radmin(i)/tem
          thlvx1(i) = thlvx(i,k)+tem1
!         if(thlvx1(i) > thlvx(i,k-1)) scuflg(i)=.false.
        endif
      enddo
! 
      do i = 1, im
         flg(i)=scuflg(i)
      enddo
      do k = kmpbl,1,-1
      do i = 1, im
        if(flg(i) .and. k <= krad(i))then
          if(thlvx1(i) <= thlvx(i,k))then
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
          kk = max(1, krad(i)+1-icld(i))
          zdd(i) = hrad(i)-zi(i,kk)
        endif
      enddo
      do i = 1, im
        if(scuflg(i))then
          zd(i) = max(zd(i),zdd(i))
          zd(i) = min(zd(i),hrad(i))
          tem   = govrth(i)*zd(i)*(-radmin(i))
          vrad(i)= tem**h1
        endif
      enddo
!
!     compute inverse prandtl number
!
      do i = 1, im
        if(ublflg(i)) then
          tem = phih(i)/phim(i)+cfac*vk*sfcfrac
        else
          tem = phih(i)/phim(i)
        endif
        prinv(i) =  1.0 / tem
        prinv(i) = min(prinv(i),prmax)
        prinv(i) = max(prinv(i),prmin)
      enddo
      do i = 1, im
        if(zol(i) > zolcr) then
          kpbl(i) = 1
        endif
      enddo
!
!     compute diffusion coefficients below pbl
!
      do k = 1, kmpbl
      do i=1,im
         if(k < kpbl(i)) then
!           zfac = max((1.-(zi(i,k+1)-zl(i,1))/
!    1             (hpbl(i)-zl(i,1))), zfmin)
            zfac = max((1.-zi(i,k+1)/hpbl(i)), zfmin)
            tem = zi(i,k+1) * (zfac**pfac)
            if(pblflg(i)) then
              tem1 = vk * wscaleu(i) * tem
!             dku(i,k) = xkzmo(i,k) + tem1
!             dkt(i,k) = xkzo(i,k)  + tem1 * prinv(i)
              dku(i,k) = tem1
              dkt(i,k) = tem1 * prinv(i)
            else
              tem1 = vk * wscale(i) * tem
!             dku(i,k) = xkzmo(i,k) + tem1
!             dkt(i,k) = xkzo(i,k)  + tem1 * prinv(i)
              dku(i,k) = tem1
              dkt(i,k) = tem1 * prinv(i)
            endif
            dku(i,k) = min(dku(i,k),dkmax)
            dku(i,k) = max(dku(i,k),xkzmo(i,k))
            dkt(i,k) = min(dkt(i,k),dkmax)
            dkt(i,k) = max(dkt(i,k),xkzo(i,k))
            dktx(i,k)= dkt(i,k)
         endif
      enddo
      enddo
!
! compute diffusion coefficients based on local scheme above pbl
!
      do k = 1, km1
         do i=1,im
            if(k >= kpbl(i)) then
               bvf2 = g*bf(i,k)*ti(i,k)
               ri   = max(bvf2/shr2(i,k),rimin)
               zk   = vk*zi(i,k+1)
               if(ri < 0.) then ! unstable regime
                  rl2      = zk*rlamun/(rlamun+zk)
                  dk       = rl2*rl2*sqrt(shr2(i,k))
                  sri      = sqrt(-ri)
!                 dku(i,k) = xkzmo(i,k) + dk*(1+8.*(-ri)/(1+1.746*sri))
!                 dkt(i,k) = xkzo(i,k)  + dk*(1+8.*(-ri)/(1+1.286*sri))
                  dku(i,k) = dk*(1+8.*(-ri)/(1+1.746*sri))
                  dkt(i,k) = dk*(1+8.*(-ri)/(1+1.286*sri))
               else             ! stable regime
                  rl2      = zk*rlam/(rlam+zk)
!!                tem      = rlam * sqrt(0.01*prsi(i,k))
!!                rl2      = zk*tem/(tem+zk)
                  dk       = rl2*rl2*sqrt(shr2(i,k))
                  tem1     = dk/(1+5.*ri)**2
!
                  if(k >= kpblx(i)) then
                    prnum = 1.0 + 2.1*ri
                    prnum = min(prnum,prmax)
                  else
                    prnum = 1.0
                  endif
!                 dku(i,k) = xkzmo(i,k) + tem1 * prnum
!                 dkt(i,k) = xkzo(i,k)  + tem1
                  dku(i,k) = tem1 * prnum
                  dkt(i,k) = tem1
               endif
!
               dku(i,k) = min(dku(i,k),dkmax)
               dku(i,k) = max(dku(i,k),xkzmo(i,k))
               dkt(i,k) = min(dkt(i,k),dkmax)
               dkt(i,k) = max(dkt(i,k),xkzo(i,k))
!
            endif
!
         enddo
      enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  compute components for mass flux mixing by large thermals
!
      do k = 1, km
        do i = 1, im
          if(pcnvflg(i)) then
            tcko(i,k) = t1(i,k)
            ucko(i,k) = u1(i,k)
            vcko(i,k) = v1(i,k)
            xmf(i,k) = 0.
          endif
        enddo
      enddo
      do kk = 1, ntrac
      do k = 1, km
        do i = 1, im
          if(pcnvflg(i)) then
            qcko(i,k,kk) = q1(i,k,kk)
          endif
        enddo
      enddo
      enddo
!
      call mfpbl(im,ix,km,ntrac,dt2,pcnvflg,
     &       zl,zi,thvx,q1,t1,u1,v1,hpbl,kpbl,
     &       sflux,ustar,wstar,xmf,tcko,qcko,ucko,vcko)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  compute diffusion coefficients for cloud-top driven diffusion
!  if the condition for cloud-top instability is met,
!    increase entrainment flux at cloud top
!
      do i = 1, im
        if(scuflg(i)) then
           k = krad(i)
           tem = thetae(i,k) - thetae(i,k+1)
           tem1 = qtx(i,k) - qtx(i,k+1)
           if (tem > 0. .and. tem1 > 0.) then
             cteit= cp*tem/(hvap*tem1)
             if(cteit > actei) rent(i) = rentf2
           endif
        endif
      enddo
      do i = 1, im
        if(scuflg(i)) then
           k = krad(i)
           tem1  = max(bf(i,k),tdzmin)
           ckt(i,k) = -rent(i)*radmin(i)/tem1
           cku(i,k) = ckt(i,k)
        endif
      enddo
!
      do k = 1, kmpbl
         do i=1,im
            if(scuflg(i) .and. k < krad(i)) then
               tem1=hrad(i)-zd(i)
               tem2=zi(i,k+1)-tem1
               if(tem2 > 0.) then
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
!
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
!
!     compute tridiagonal matrix elements for heat and moisture
!
      do i=1,im
         ad(i,1) = 1.
         a1(i,1) = t1(i,1)   + beta(i) * heat(i)
         a2(i,1) = q1(i,1,1) + beta(i) * evap(i)
      enddo

      if(ntrac >= 2) then
        do k = 2, ntrac
          is = (k-1) * km
          do i = 1, im
            a2(i,1+is) = q1(i,1,k)
          enddo
        enddo
      endif
!
      do k = 1,km1
        do i = 1,im
          dtodsd = dt2/del(i,k)
          dtodsu = dt2/del(i,k+1)
          dsig   = prsl(i,k)-prsl(i,k+1)
          rdz    = rdzt(i,k)
          tem1   = dsig * dkt(i,k) * rdz
          dsdz2     = tem1 * rdz
          au(i,k)   = -dtodsd*dsdz2
          al(i,k)   = -dtodsu*dsdz2
!
          if(pcnvflg(i) .and. k < kpbl(i)) then
             tem2      = dsig * rdz
             ptem      = 0.5 * tem2 * xmf(i,k)
             ptem1     = dtodsd * ptem
             ptem2     = dtodsu * ptem
             ad(i,k)   = ad(i,k)-au(i,k)-ptem1
             ad(i,k+1) = 1.-al(i,k)+ptem2
             au(i,k)   = au(i,k)-ptem1
             al(i,k)   = al(i,k)+ptem2
             ptem      = tcko(i,k) + tcko(i,k+1)
             dsdzt     = tem1 * gocp
             a1(i,k)   = a1(i,k)+dtodsd*dsdzt-ptem1*ptem
             a1(i,k+1) = t1(i,k+1)-dtodsu*dsdzt+ptem2*ptem
             ptem      = qcko(i,k,1) + qcko(i,k+1,1)
             a2(i,k)   = a2(i,k) - ptem1 * ptem
             a2(i,k+1) = q1(i,k+1,1) + ptem2 * ptem
          elseif(ublflg(i) .and. k < kpbl(i)) then
             ptem1 = dsig * dktx(i,k) * rdz
             tem   = 1.0 / hpbl(i)
             dsdzt = tem1 * gocp - ptem1 * hgamt(i) * tem
             dsdzq = - ptem1 * hgamq(i) * tem
             ad(i,k)   = ad(i,k)-au(i,k)
             ad(i,k+1) = 1.-al(i,k)
             a1(i,k)   = a1(i,k)+dtodsd*dsdzt
             a1(i,k+1) = t1(i,k+1)-dtodsu*dsdzt
             a2(i,k)   = a2(i,k)+dtodsd*dsdzq
             a2(i,k+1) = q1(i,k+1,1)-dtodsu*dsdzq
          else
             ad(i,k)   = ad(i,k)-au(i,k)
             ad(i,k+1) = 1.-al(i,k)
             dsdzt     = tem1 * gocp
             a1(i,k)   = a1(i,k)+dtodsd*dsdzt
             a1(i,k+1) = t1(i,k+1)-dtodsu*dsdzt
             a2(i,k+1) = q1(i,k+1,1)
          endif
!
        enddo
      enddo
!
      if(ntrac >= 2) then
        do kk = 2, ntrac
          is = (kk-1) * km
          do k = 1, km1
            do i = 1, im
              if(pcnvflg(i) .and. k < kpbl(i)) then
                dtodsd = dt2/del(i,k)
                dtodsu = dt2/del(i,k+1)
                dsig  = prsl(i,k)-prsl(i,k+1)
                tem   = dsig * rdzt(i,k)
                ptem  = 0.5 * tem * xmf(i,k)
                ptem1 = dtodsd * ptem
                ptem2 = dtodsu * ptem
                tem1  = qcko(i,k,kk) + qcko(i,k+1,kk)
                a2(i,k+is) = a2(i,k+is) - ptem1*tem1
                a2(i,k+1+is)= q1(i,k+1,kk) + ptem2*tem1
              else
                a2(i,k+1+is) = q1(i,k+1,kk)
              endif
            enddo
          enddo
        enddo
      endif
!
!     solve tridiagonal problem for heat and moisture
!
      call tridin(im,km,ntrac,al,ad,au,a1,a2,au,a1,a2)

!
!     recover tendencies of heat and moisture
!
      do  k = 1,km
         do i = 1,im
            ttend      = (a1(i,k)-t1(i,k)) * rdt
            qtend      = (a2(i,k)-q1(i,k,1))*rdt
            tau(i,k)   = tau(i,k)+ttend
            rtg(i,k,1) = rtg(i,k,1)+qtend
            dtsfc(i)   = dtsfc(i)+cont*del(i,k)*ttend
            dqsfc(i)   = dqsfc(i)+conq*del(i,k)*qtend
         enddo
      enddo
      if(ntrac >= 2) then
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
!
!   compute tke dissipation rate
!
      if(dspheat) then
!
      do k = 1,km1
        do i = 1,im
          diss(i,k) = dku(i,k)*shr2(i,k)-g*ti(i,k)*dkt(i,k)*bf(i,k)
!         diss(i,k) = dku(i,k)*shr2(i,k)
        enddo
      enddo
!
!     add dissipative heating at the first model layer
!
      do i = 1,im
         tem   = govrth(i)*sflux(i)
         tem1  = tem + stress(i)*spd1(i)/zl(i,1)
         tem2  = 0.5 * (tem1+diss(i,1)) 
         tem2  = max(tem2, 0.)
         ttend = tem2 / cp
         tau(i,1) = tau(i,1)+0.5*ttend
      enddo
!
!     add dissipative heating above the first model layer
!
      do k = 2,km1
        do i = 1,im
          tem = 0.5 * (diss(i,k-1)+diss(i,k))
          tem  = max(tem, 0.)
          ttend = tem / cp
          tau(i,k) = tau(i,k) + 0.5*ttend
        enddo
      enddo
!
      endif
!
!     compute tridiagonal matrix elements for momentum
!
      do i=1,im
         ad(i,1) = 1.0 + beta(i) * stress(i) / spd1(i)
         a1(i,1) = u1(i,1)
         a2(i,1) = v1(i,1)
      enddo
!
      do k = 1,km1
        do i=1,im
          dtodsd  = dt2/del(i,k)
          dtodsu  = dt2/del(i,k+1)
          dsig    = prsl(i,k)-prsl(i,k+1)
          rdz     = rdzt(i,k)
          tem1    = dsig*dku(i,k)*rdz
          dsdz2   = tem1 * rdz
          au(i,k) = -dtodsd*dsdz2
          al(i,k) = -dtodsu*dsdz2
!
          if(pcnvflg(i) .and. k < kpbl(i)) then
             tem2      = dsig * rdz
             ptem      = 0.5 * tem2 * xmf(i,k)
             ptem1     = dtodsd * ptem
             ptem2     = dtodsu * ptem
             ad(i,k)   = ad(i,k)-au(i,k)-ptem1
             ad(i,k+1) = 1.-al(i,k)+ptem2
             au(i,k)   = au(i,k)-ptem1
             al(i,k)   = al(i,k)+ptem2
             ptem      = ucko(i,k) + ucko(i,k+1)
             a1(i,k)   = a1(i,k) - ptem1 * ptem
             a1(i,k+1) = u1(i,k+1) + ptem2 * ptem
             ptem      = vcko(i,k) + vcko(i,k+1)
             a2(i,k)   = a2(i,k) - ptem1 * ptem
             a2(i,k+1) = v1(i,k+1) + ptem2 * ptem
          else
             ad(i,k)   = ad(i,k)-au(i,k)
             ad(i,k+1) = 1.-al(i,k)
             a1(i,k+1) = u1(i,k+1)
             a2(i,k+1) = v1(i,k+1)
          endif
!
        enddo
      enddo
!
!     solve tridiagonal problem for momentum
!
      call tridi2(im,km,al,ad,au,a1,a2,au,a1,a2)
!
!     recover tendencies of momentum
!
      do k = 1,km
         do i = 1,im
            utend = (a1(i,k)-u1(i,k))*rdt
            vtend = (a2(i,k)-v1(i,k))*rdt
            du(i,k)  = du(i,k)  + utend
            dv(i,k)  = dv(i,k)  + vtend
            dusfc(i) = dusfc(i) + conw*del(i,k)*utend
            dvsfc(i) = dvsfc(i) + conw*del(i,k)*vtend
!
!  for dissipative heating for ecmwf model
!
!           tem1 = 0.5*(a1(i,k)+u1(i,k))
!           tem2 = 0.5*(a2(i,k)+v1(i,k))
!           diss(i,k) = -(tem1*utend+tem2*vtend)
!           diss(i,k) = max(diss(i,k),0.)
!           ttend = diss(i,k) / cp
!           tau(i,k) = tau(i,k) + ttend
!
         enddo
      enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      do i = 1, im
         hpbl(i) = hpblx(i)
         kpbl(i) = kpblx(i)
      enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      return
      end
c-----------------------------------------------------------------------
      subroutine tridi2(l,n,cl,cm,cu,r1,r2,au,a1,a2)
cc
      use machine     , only : kind_phys
      implicit none
      integer             k,n,l,i
      real(kind=kind_phys) fk
cc
      real(kind=kind_phys) cl(l,2:n),cm(l,n),cu(l,n-1),r1(l,n),r2(l,n),
     &          au(l,n-1),a1(l,n),a2(l,n)
c-----------------------------------------------------------------------
      do i=1,l
        fk      = 1./cm(i,1)
        au(i,1) = fk*cu(i,1)
        a1(i,1) = fk*r1(i,1)
        a2(i,1) = fk*r2(i,1)
      enddo
      do k=2,n-1
        do i=1,l
          fk      = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
          au(i,k) = fk*cu(i,k)
          a1(i,k) = fk*(r1(i,k)-cl(i,k)*a1(i,k-1))
          a2(i,k) = fk*(r2(i,k)-cl(i,k)*a2(i,k-1))
        enddo
      enddo
      do i=1,l
        fk      = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
        a1(i,n) = fk*(r1(i,n)-cl(i,n)*a1(i,n-1))
        a2(i,n) = fk*(r2(i,n)-cl(i,n)*a2(i,n-1))
      enddo
      do k=n-1,1,-1
        do i=1,l
          a1(i,k) = a1(i,k)-au(i,k)*a1(i,k+1)
          a2(i,k) = a2(i,k)-au(i,k)*a2(i,k+1)
        enddo
      enddo
c-----------------------------------------------------------------------
      return
      end
c-----------------------------------------------------------------------
      subroutine tridin(l,n,nt,cl,cm,cu,r1,r2,au,a1,a2)
cc
      use machine     , only : kind_phys
      implicit none
      integer             is,k,kk,n,nt,l,i
      real(kind=kind_phys) fk(l)
cc
      real(kind=kind_phys) cl(l,2:n), cm(l,n), cu(l,n-1),
     &                     r1(l,n),   r2(l,n*nt),
     &                     au(l,n-1), a1(l,n), a2(l,n*nt),
     &                     fkk(l,2:n-1)
c-----------------------------------------------------------------------
      do i=1,l
        fk(i)   = 1./cm(i,1)
        au(i,1) = fk(i)*cu(i,1)
        a1(i,1) = fk(i)*r1(i,1)
      enddo
      do k = 1, nt
        is = (k-1) * n
        do i = 1, l
          a2(i,1+is) = fk(i) * r2(i,1+is)
        enddo
      enddo
      do k=2,n-1
        do i=1,l
          fkk(i,k) = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
          au(i,k)  = fkk(i,k)*cu(i,k)
          a1(i,k)  = fkk(i,k)*(r1(i,k)-cl(i,k)*a1(i,k-1))
        enddo
      enddo
      do kk = 1, nt
        is = (kk-1) * n
        do k=2,n-1
          do i=1,l
            a2(i,k+is) = fkk(i,k)*(r2(i,k+is)-cl(i,k)*a2(i,k+is-1))
          enddo
        enddo
      enddo
      do i=1,l
        fk(i)   = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
        a1(i,n) = fk(i)*(r1(i,n)-cl(i,n)*a1(i,n-1))
      enddo
      do k = 1, nt
        is = (k-1) * n
        do i = 1, l
          a2(i,n+is) = fk(i)*(r2(i,n+is)-cl(i,n)*a2(i,n+is-1))
        enddo
      enddo
      do k=n-1,1,-1
        do i=1,l
          a1(i,k) = a1(i,k) - au(i,k)*a1(i,k+1)
        enddo
      enddo
      do kk = 1, nt
        is = (kk-1) * n
        do k=n-1,1,-1
          do i=1,l
            a2(i,k+is) = a2(i,k+is) - au(i,k)*a2(i,k+is+1)
          enddo
        enddo
      enddo
c-----------------------------------------------------------------------
      return
      end
