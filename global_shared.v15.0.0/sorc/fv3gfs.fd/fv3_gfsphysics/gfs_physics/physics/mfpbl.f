!!!!!  ==========================================================  !!!!!
! subroutine 'mfpbl' computes mass-flux components, called by 
!  subroutine 'moninedmf'.
!
      subroutine mfpbl(im,ix,km,ntrac,delt,cnvflg,
     &   zl,zm,thvx,q1,t1,u1,v1,hpbl,kpbl,
     &   sflx,ustar,wstar,xmf,tcko,qcko,ucko,vcko)
!
      use machine , only : kind_phys
      use physcons, grav => con_g, cp => con_cp
!
      implicit none
!
      integer              im, ix, km, ntrac
!    &,                    me
      integer              kpbl(im)
      logical              cnvflg(im)
      real(kind=kind_phys) delt
      real(kind=kind_phys) q1(ix,km,ntrac), t1(ix,km),
     &                     u1(ix,km),  v1(ix,km),
     &                     thvx(im,km),
     &                     zl(im,km),  zm(im,km+1),
     &                     hpbl(im),   sflx(im),    ustar(im),
     &                     wstar(im),  xmf(im,km),
     &                     tcko(im,km),qcko(im,km,ntrac),
     &                     ucko(im,km),vcko(im,km)
!
c  local variables and arrays
!
      integer   i, j, k, n, kmpbl
!
      real(kind=kind_phys) dt2,     dz,      ce0,
     &                     h1,      factor,  gocp,
     &                     g,       c1,      d1,
     &                     b1,      f1,      bb1,     bb2,
     &                     alp,     a1,      qmin,    zfmin,
     &                     xmmx,    rbint,   tau,
!    &                     rbint,   tau,
     &                     tem,     tem1,    tem2,
     &                     ptem,    ptem1,   ptem2,
     &                     pgcon
!
      real(kind=kind_phys) sigw1(im),   usws3(im),  xlamax(im),
     &                     rbdn(im),    rbup(im),   delz(im)
!
      real(kind=kind_phys) wu2(im,km),     xlamue(im,km),
     &                     thvu(im,km),    zi(im,km),
     &                     buo(im,km)
!
      logical totflg, flg(im)
!
c  physical parameters
      parameter(g=grav)
      parameter(gocp=g/cp)
!     parameter(ce0=0.37,qmin=1.e-8,alp=1.0,pgcon=0.55)
      parameter(ce0=0.38,qmin=1.e-8,alp=1.0,pgcon=0.55)
      parameter(a1=0.08,b1=0.5,f1=0.15,c1=0.3,d1=2.58,tau=500.)
      parameter(zfmin=1.e-8,h1=0.33333333)
!
c-----------------------------------------------------------------------
!
!************************************************************************
!
      kmpbl = km/2 + 1
      dt2 = delt
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
      do k = 1, km
        do i=1,im
          if (cnvflg(i)) then
            zi(i,k) = zm(i,k+1)
          endif
        enddo
      enddo
!
      do i=1,im
        if(cnvflg(i)) then 
          k = kpbl(i) / 2
          k = max(k, 1) 
          delz(i) = zl(i,k+1) - zl(i,k)
          xlamax(i) = ce0 / delz(i)
        endif
      enddo
      do k = 1, kmpbl
        do i=1,im
          if(cnvflg(i)) then
            if(k < kpbl(i)) then
              ptem = 1./(zi(i,k)+delz(i))
              tem = max((hpbl(i)-zi(i,k)+delz(i)) ,delz(i))
              ptem1 = 1./tem
              xlamue(i,k) = ce0 * (ptem+ptem1)
            else
              xlamue(i,k) = xlamax(i)
            endif
          endif
        enddo
      enddo
c
c  compute thermal excess
c
      do i=1,im
        if(cnvflg(i)) then
          tem = zl(i,1)/hpbl(i)
          usws3(i) = (ustar(i)/wstar(i))**3.
          tem1 = usws3(i) + 0.6*tem
          tem2 = max((1.-tem), zfmin)
          ptem = (tem1**h1) * sqrt(tem2)
          sigw1(i) = 1.3 * ptem * wstar(i)
          ptem1 = alp * sflx(i) / sigw1(i)
          thvu(i,1) = thvx(i,1) + ptem1
          buo(i,1) = g * (thvu(i,1)/thvx(i,1)-1.)
        endif
      enddo
c
c  compute potential temperature and buoyancy for updraft air parcel
c
      do k = 2, kmpbl
        do i=1,im
          if(cnvflg(i)) then
            dz = zl(i,k) - zl(i,k-1)
            tem = xlamue(i,k-1) * dz
            ptem = 2. + tem
            ptem1 = (2. - tem) / ptem
            tem1 = tem  * (thvx(i,k)+thvx(i,k-1)) / ptem
            thvu(i,k) = ptem1 * thvu(i,k-1) + tem1
            buo(i,k) = g * (thvu(i,k)/thvx(i,k)-1.)
          endif
        enddo
      enddo
c
c  compute updraft velocity square(wu2)
c
!     tem = 1.-2.*f1
!     bb1 = 2. * b1 / tem
!     bb2 = 2. / tem
!  from soares et al. (2004,qjrms)
!     bb1 = 2.
!     bb2 = 4.
!
!  from bretherton et al. (2004, mwr)
!     bb1 = 4.
!     bb2 = 2.
!
!  from our tuning
      bb1 = 1.8
      bb2 = 3.5 
!
      do i = 1, im
        if(cnvflg(i)) then
!
!         tem = zi(i,1)/hpbl(i)
!         tem1 = usws3(i) + 0.6*tem
!         tem2 = max((1.-tem), zfmin)
!         ptem = (tem1**h1) * sqrt(tem2)
!         ptem1 = 1.3 * ptem * wstar(i)
!         wu2(i,1) = d1*d1*ptem1*ptem1
!
          dz   = zi(i,1)
          tem  = 0.5*bb1*xlamue(i,1)*dz
          tem1 = bb2 * buo(i,1) * dz
          ptem1 = 1. + tem
          wu2(i,1) = tem1 / ptem1
!
        endif
      enddo
      do k = 2, kmpbl
        do i = 1, im
          if(cnvflg(i)) then
            dz    = zi(i,k) - zi(i,k-1)
            tem  = 0.25*bb1*(xlamue(i,k)+xlamue(i,k-1))*dz
            tem1 = bb2 * buo(i,k) * dz
            ptem = (1. - tem) * wu2(i,k-1)
            ptem1 = 1. + tem
            wu2(i,k) = (ptem + tem1) / ptem1
          endif
        enddo
      enddo
c
c  update pbl height as the height where updraft velocity vanishes
c
      do i=1,im
         flg(i)  = .true.
         if(cnvflg(i)) then
           flg(i)  = .false.
           rbup(i) = wu2(i,1)
         endif
      enddo
      do k = 2, kmpbl
      do i = 1, im
        if(.not.flg(i)) then
          rbdn(i) = rbup(i)
          rbup(i) = wu2(i,k)
          kpbl(i) = k
          flg(i)  = rbup(i).le.0.
        endif
      enddo
      enddo
      do i = 1,im
        if(cnvflg(i)) then
           k = kpbl(i)
           if(rbdn(i) <= 0.) then
              rbint = 0.
           elseif(rbup(i) >= 0.) then
              rbint = 1.
           else
              rbint = rbdn(i)/(rbdn(i)-rbup(i))
           endif
           hpbl(i) = zi(i,k-1) + rbint*(zi(i,k)-zi(i,k-1))
        endif
      enddo
c
      do i=1,im
        if(cnvflg(i)) then
          k = kpbl(i) / 2
          k = max(k, 1)
          delz(i) = zl(i,k+1) - zl(i,k)
          xlamax(i) = ce0 / delz(i)
        endif
      enddo
c
c  update entrainment rate
c
!     do k = 1, kmpbl
!       do i=1,im
!         if(cnvflg(i)) then
!           if(k < kpbl(i)) then
!             tem = tau * sqrt(wu2(i,k))
!             tem1 = 1. / tem
!             ptem = ce0 / zi(i,k)
!             xlamue(i,k) = max(tem1, ptem)
!           else
!             xlamue(i,k) = xlamax(i)
!           endif
!         endif
!       enddo
!     enddo
!
      do k = 1, kmpbl
        do i=1,im
          if(cnvflg(i)) then
            if(k < kpbl(i)) then
              ptem = 1./(zi(i,k)+delz(i))
              tem = max((hpbl(i)-zi(i,k)+delz(i)) ,delz(i))
              ptem1 = 1./tem
              xlamue(i,k) = ce0 * (ptem+ptem1)
            else
              xlamue(i,k) = xlamax(i)
            endif
          endif
        enddo
      enddo
c
c  updraft mass flux as a function of sigmaw
c   (0.3*sigmaw[square root of vertical turbulence variance])
c
!     do k = 1, kmpbl
!       do i=1,im
!         if(cnvflg(i) .and. k < kpbl(i)) then
!           tem = zi(i,k)/hpbl(i)
!           tem1 = usws3(i) + 0.6*tem
!           tem2 = max((1.-tem), zfmin)
!           ptem = (tem1**h1) * sqrt(tem2)
!           ptem1 = 1.3 * ptem * wstar(i)
!           xmf(i,k) = c1 * ptem1
!         endif
!       enddo
!     enddo
c
c  updraft mass flux as a function of updraft velocity profile
c
      do k = 1, kmpbl
        do i = 1, im
          if (cnvflg(i) .and. k < kpbl(i)) then
             xmf(i,k) = a1 * sqrt(wu2(i,k))
             dz   = zl(i,k+1) - zl(i,k)
             xmmx = dz / dt2
             xmf(i,k) = min(xmf(i,k),xmmx)
          endif
        enddo
      enddo
c
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c  compute updraft property
c
      do k = 2, kmpbl
        do i = 1, im
          if (cnvflg(i) .and. k <= kpbl(i)) then
             dz   = zl(i,k) - zl(i,k-1)
             tem  = 0.5 * xlamue(i,k-1) * dz
             factor = 1. + tem
             ptem = tem + pgcon
             ptem1= tem - pgcon
!
             tcko(i,k) = ((1.-tem)*tcko(i,k-1)+tem*
     &                    (t1(i,k)+t1(i,k-1))-gocp*dz)/factor
             ucko(i,k) = ((1.-tem)*ucko(i,k-1)+ptem*u1(i,k)
     &                    +ptem1*u1(i,k-1))/factor
             vcko(i,k) = ((1.-tem)*vcko(i,k-1)+ptem*v1(i,k)
     &                    +ptem1*v1(i,k-1))/factor
          endif
        enddo
      enddo
      do n = 1, ntrac
      do k = 2, kmpbl
        do i = 1, im
          if (cnvflg(i) .and. k <= kpbl(i)) then
             dz   = zl(i,k) - zl(i,k-1)
             tem  = 0.5 * xlamue(i,k-1) * dz
             factor = 1. + tem
 
             qcko(i,k,n) = ((1.-tem)*qcko(i,k-1,n)+tem*
     &                    (q1(i,k,n)+q1(i,k-1,n)))/factor
          endif
        enddo
      enddo
      enddo
!
      return
      end
