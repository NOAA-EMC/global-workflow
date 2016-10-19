!!!!!  ==========================================================  !!!!!
! subroutine 'moninshoc' computes pbl height and applies vertical diffusion
! using the coefficient provided by the SHOC scheme (from previous step)
! 1015-05-04 - Shrinivas Moorthi - original version based on monin
! 
      subroutine moninshoc(ix,im,km,ntrac,ntcw,dv,du,tau,rtg,
     &                     u1,v1,t1,q1,tkh,prnum,ntke,
     &                     psk,rbsoil,zorl,u10m,v10m,fm,fh,
     &                     tsea,heat,evap,stress,spd1,kpbl,
     &                     prsi,del,prsl,prslk,phii,phil,delt,
     &                     dusfc,dvsfc,dtsfc,dqsfc,dkt,hpbl,
     &                     kinver,xkzm_m,xkzm_h,xkzm_s,lprnt,ipr,me)
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
      integer ipr, me , ix, im, km, ntrac, ntcw,  ntke
      integer, dimension(im) ::  kinver, kpbl
!
      real(kind=kind_phys) delt, xkzm_m, xkzm_h, xkzm_s
      real(kind=kind_phys), dimension(im,km) :: du, dv, tau, prnum
!
      real(kind=kind_phys), dimension(im,km,ntrac) :: rtg

      real(kind=kind_phys), dimension(ix,km)   :: u1, v1, t1, tkh 
     &,                                           prsl, del, phil, prslk
      real(kind=kind_phys), dimension(ix,km+1) :: prsi, phii
      real(kind=kind_phys), dimension(ix,km,ntrac) :: q1
      real(kind=kind_phys), dimension(im)          :: psk, rbsoil, zorl
     &,                                               spd1, u10m, v10m
     &,                                               fm, fh, tsea, hpbl
     &,                                               dusfc, dvsfc
     &,                                               dtsfc, dqsfc
!
!    locals
!
      integer i,iprt,is,k,kk,km1,kmpbl
      integer kx1(im)
!
      logical  pblflg(im), sfcflg(im), flg(im)

      real(kind=kind_phys), dimension(im) ::  evap, heat, phih, phim
     &,                     rbdn, rbup, sflux, z0, crb, zol, thermal
     &,                     stress, beta, tx1, tx2
!
      real(kind=kind_phys), dimension(im,km)  :: theta, thvx, zl, a1, ad
      real(kind=kind_phys), dimension(im,km-1):: xkzo, xkzmo, al, au
     &,                                          dku, dkt, rdzt
!    &,                                          dku, dkt, rdzt, prnum
!
      real(kind=kind_phys) zi(im,km+1), a2(im,km*(ntrac+1))
!
      real(kind=kind_phys) dsdz2,  dsdzq,  dsdzt, dsig, dt2, rdt
     &,                    dtodsd, dtodsu, rdz,   tem,  tem1, ptem
     &,                    ttend,  utend, vtend,  qtend
     &,                    spdk2,  rbint, ri,     zol1, robn, bvf2
!
      real(kind=kind_phys), parameter :: gravi=1.0/grav, zolcr=0.2,
     &                      zolcru=-0.5, rimin=-100.,    sfcfrac=0.1,
     &                      crbcon=0.25, crbmin=0.15,    crbmax=0.35,
     &                      qmin=1.e-8,  zfmin=1.e-8,    qlmin=1.e-12,
     &                      aphi5=5.,    aphi16=16.,     f0=1.e-4
     &,                     cont=cp/grav, conq=hvap/grav,conw=1.0/grav
     &,                     dkmin=0.0,    dkmax=1000.,   xkzminv=0.3
     &,                     gocp=grav/cp, prmin=0.25,    prmax=4.0
     &,                     vk=0.4, cfac=6.5
!
!-----------------------------------------------------------------------
!
!     compute preliminary variables
!
      if (ix < im) stop
!
      if (lprnt) write(0,*)' in moninshoc tsea=',tsea(ipr)
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
          rdzt(i,k)  = 1.0 / (zl(i,k+1) - zl(i,k))
          prnum(i,k) = 1.0
        enddo
      enddo
!               Setup backgrond diffision
      do i=1,im
        prnum(i,km) = 1.0
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
          if(zi(i,k+1) > 250.) then
            tem1 = (t1(i,k+1)-t1(i,k)) * rdzt(i,k)
            if(tem1 > 1.e-5) then
               xkzo(i,k)  = min(xkzo(i,k),xkzminv)
            endif
          endif
        enddo
      enddo
!
!
      do i = 1,im
         z0(i)    = 0.01 * zorl(i)
         kpbl(i)  = 1
         hpbl(i)  = zi(i,1)
         pblflg(i)= .true.
         sfcflg(i)= .true.
         if(rbsoil(i) > 0.) sfcflg(i) = .false.
         dusfc(i) = 0.
         dvsfc(i) = 0.
         dtsfc(i) = 0.
         dqsfc(i) = 0.
      enddo
!
      do k = 1,km
        do i = 1,im
          tem = max(q1(i,k,ntcw),qlmin)
          theta(i,k) = t1(i,k) * psk(i) / prslk(i,k)
          thvx(i,k)  = theta(i,k)*(1.+fv*max(q1(i,k,1),qmin)-tem)
        enddo
      enddo
!
      do i = 1,im
         sflux(i)  = heat(i) + evap(i)*fv*theta(i,1)
         if(.not.sfcflg(i) .or. sflux(i) <= 0.) pblflg(i)=.false.
         beta(i)  = dt2 / (zi(i,2)-zi(i,1))
      enddo
!
!  compute the pbl height
!
!     write(0,*)' IN moninbl u10=',u10m(1:5),' v10=',v10m(1:5)
      do i=1,im
         flg(i) = .false.
         rbup(i) = rbsoil(i)
!
         if(pblflg(i)) then
           thermal(i) = thvx(i,1)
           crb(i) = crbcon
         else
           thermal(i) = tsea(i)*(1.+fv*max(q1(i,1,1),qmin))
           tem = max(1.0, sqrt(u10m(i)*u10m(i) + v10m(i)*v10m(i)))
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
            spdk2   = max((u1(i,k)*u1(i,k)+v1(i,k)*v1(i,k)),1.)
            rbup(i) = (thvx(i,k)-thermal(i))*phil(i,k)
     &              / (thvx(i,1)*spdk2)
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
      enddo
!
!  enhance the pbl height by considering the thermal excess
!
      do i=1,im
         flg(i)  = .true.
         if (pblflg(i)) then
           flg(i) = .false.
           rbup(i) = rbsoil(i)
         endif
      enddo
      do k = 2, kmpbl
        do i = 1, im
          if(.not.flg(i)) then
            rbdn(i) = rbup(i)
            spdk2   = max((u1(i,k)*u1(i,k)+v1(i,k)*v1(i,k)),1.)
            rbup(i) = (thvx(i,k)-thermal(i)) * phil(i,k)
     &              / (thvx(i,1)*spdk2)
            kpbl(i) = k
            flg(i)  = rbup(i) > crb(i)
          endif
        enddo
      enddo
      do i = 1,im
        if (pblflg(i)) then
          k = kpbl(i)
          if(rbdn(i) >= crb(i)) then
            rbint = 0.
          elseif(rbup(i) <= crb(i)) then
            rbint = 1.
          else
            rbint = (crb(i)-rbdn(i))/(rbup(i)-rbdn(i))
          endif
          if (k > 1) then
            hpbl(i) = zl(i,k-1) + rbint*(zl(i,k)-zl(i,k-1))
            if(hpbl(i) < zi(i,kpbl(i))) kpbl(i) = kpbl(i) - 1
            if(kpbl(i) <= 1) then
              pblflg(i) = .false.
            endif
          else
            pblflg(i) = .false.
          endif
        endif
        if (pblflg(i)) then
          tem = phih(i)/phim(i)+cfac*vk*sfcfrac
        else
          tem = phih(i)/phim(i)
        endif
        prnum(i,1) = min(prmin,max(prmax,tem))
      enddo
!
      do i = 1, im
        if(zol(i) > zolcr) then
          kpbl(i) = 1
        endif
      enddo
!
!  compute Prandtl number above boundary layer
!
      do k = 1, km1
        do i=1,im
          if(k >= kpbl(i)) then
            rdz  = rdzt(i,k)
            tem  = u1(i,k)-u1(i,k+1)
            tem1 = v1(i,k)-v1(i,k+1)
            tem  = (tem*tem + tem1*tem1) * rdz * rdz
            bvf2 = (0.5*grav)*(thvx(i,k+1)-thvx(i,k))*rdz
     &           / (t1(i,k)+t1(i,k+1))
            ri   = max(bvf2/tem,rimin)
            if(ri < 0.) then ! unstable regime
              prnum(i,k) = 1.0
            else
              prnum(i,k) = min(1.0 + 2.1*ri, prmax)
            endif
          elseif (k > 1) then
            prnum(i,k) = prnum(i,1)
          endif
!
!         prnum(i,k) = 1.0
          prnum(i,k) = max(prmin, min(prmax, prnum(i,k)))
          tem      = tkh(i,k+1) * prnum(i,k)
          dku(i,k) = max(min(tem+xkzmo(i,k),       dkmax), xkzmo(i,k))
          dkt(i,k) = max(min(tkh(i,k+1)+xkzo(i,k), dkmax), xkzo(i,k))
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

      if(ntrac > 2) then
        do k = 2, ntrac-1
          is = (k-1) * km
          do i = 1, im
            a2(i,1+is) = q1(i,1,k)
          enddo
        enddo
      endif
!
      do k = 1,km1
        do i = 1,im
          dtodsd    = dt2/del(i,k)
          dtodsu    = dt2/del(i,k+1)
          dsig      = prsl(i,k)-prsl(i,k+1)
          rdz       = rdzt(i,k)
          tem1      = dsig * dkt(i,k) * rdz
          dsdz2     = tem1 * rdz
          au(i,k)   = -dtodsd*dsdz2
          al(i,k)   = -dtodsu*dsdz2
!
          ad(i,k)   = ad(i,k)-au(i,k)
          ad(i,k+1) = 1.-al(i,k)
          dsdzt     = tem1 * gocp
          a1(i,k)   = a1(i,k)+dtodsd*dsdzt
          a1(i,k+1) = t1(i,k+1)-dtodsu*dsdzt
          a2(i,k+1) = q1(i,k+1,1)
!
        enddo
      enddo
!
      if(ntrac > 2) then
        do kk = 2, ntrac-1
          is = (kk-1) * km
          do k = 1, km1
            do i = 1, im
              a2(i,k+1+is) = q1(i,k+1,kk)
            enddo
          enddo
        enddo
      endif
!
!     solve tridiagonal problem for heat and moisture
!
      call tridin(im,km,ntrac-1,al,ad,au,a1,a2,au,a1,a2)

!
!     recover tendencies of heat and moisture
!
      do  k = 1,km
         do i = 1,im
            ttend      = (a1(i,k)-t1(i,k))   * rdt
            qtend      = (a2(i,k)-q1(i,k,1)) * rdt
            tau(i,k)   = tau(i,k)   + ttend
            rtg(i,k,1) = rtg(i,k,1) + qtend
            dtsfc(i)   = dtsfc(i)   + cont*del(i,k)*ttend
            dqsfc(i)   = dqsfc(i)   + conq*del(i,k)*qtend
         enddo
      enddo
      if(ntrac > 2) then
        do kk = 2, ntrac-1
          is = (kk-1) * km
          do k = 1, km 
            do i = 1, im
              qtend = (a2(i,k+is)-q1(i,k,kk))*rdt
              rtg(i,k,kk) = rtg(i,k,kk) + qtend
            enddo
          enddo
        enddo
      endif
!
!     compute tridiagonal matrix elements for momentum
!
      do i=1,im
         ad(i,1) = 1.0 + beta(i) * stress(i) / spd1(i)
         a1(i,1) = u1(i,1)
         a2(i,1) = v1(i,1)
      enddo
      if (ntke > 0) then
        do i = 1, im
          a2(i,1+km) = q1(i,1,ntke)
        enddo
      endif
!
      do k = 1,km1
        do i=1,im
          dtodsd    = dt2/del(i,k)
          dtodsu    = dt2/del(i,k+1)
          dsig      = prsl(i,k)-prsl(i,k+1)
          rdz       = rdzt(i,k)
          tem1      = dsig*dku(i,k)*rdz
          dsdz2     = tem1 * rdz
          au(i,k)   = -dtodsd*dsdz2
          al(i,k)   = -dtodsu*dsdz2
!
          ad(i,k)   = ad(i,k) - au(i,k)
          ad(i,k+1) = 1.0 - al(i,k)
          a1(i,k+1) = u1(i,k+1)
          a2(i,k+1) = v1(i,k+1)
!
        enddo
      enddo
      if (ntke > 0) then    ! solve tridiagonal problem for momentum and tke
        do k = 1, km1
          do i = 1, im
            a2(i,k+1+km) = q1(i,k+1,ntke)
          enddo
        enddo
        call tridin(im,km,3,al,ad,au,a1,a2,au,a1,a2)
!
        do k = 1, km !     recover tendencies of tke
          do i = 1, im
            qtend = (a2(i,k+km)-q1(i,k,ntke))*rdt
            rtg(i,k,ntke) = rtg(i,k,ntke) + qtend
          enddo
        enddo
      else                 ! solve tridiagonal problem for momentum
        call tridi2(im,km,al,ad,au,a1,a2,au,a1,a2)
      endif
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
        enddo
      enddo
!
      return
      end
