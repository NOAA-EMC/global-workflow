      subroutine cnvc90(clstp,im,ix,rn,kbot,ktop,km,prsi,
     1                   acv,acvb,acvt,cv,cvb,cvt)
!
      use machine, only :kind_phys
      implicit none
      integer              i,ibot,im,itop,km,lc,lz,n,ncc,ix
      real(kind=kind_phys) ah,cc1,cc2,clstp,cvb0,p1,p2
      integer              kbot(im),ktop(im)
      real(kind=kind_phys) rn(im),  acv(im), acvb(im), acvt(im),
     &                     cv(im),  cvb(im), cvt(im)
      real(kind=kind_phys) prsi(ix,km+1)
      integer              nmd(im)
      real(kind=kind_phys) pmd(im)
!
      real (kind=kind_phys), parameter :: cons_100=100.0
      real(kind=kind_phys) r_kbot_i, r_ktop_i
!
      parameter(ncc=9)
      real(kind=kind_phys) cc(ncc),p(ncc)
      data cc/0.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8/
      data p/.14,.31,.70,1.6,3.4,7.7,17.,38.,85./
      data cvb0/100./
!
      lz = 0
      lc = 0
      if(clstp >= 1000.) lz=1
      if(clstp >= 1100..or.(clstp < 1000..and.clstp >= 100.)) lc = 1
      ah = mod(clstp,cons_100)
      if(lz /= 0) then
        do i=1,im
          acv(i)  = 0.
          acvb(i) = cvb0
          acvt(i) = 0.
        enddo
      endif
      if(lc /= 0) then
        do i=1,im
          if(rn(i) > 0.) then
            acv(i)   = acv(i)+rn(i)
            r_kbot_i = kbot(i)
            acvb(i)  = min(acvb(i),r_kbot_i)
            r_ktop_i = ktop(i)
            acvt(i)  = max(acvt(i),r_ktop_i)
          endif
        enddo
      endif
      if(ah > 0.01.and.ah < 99.99) then
        do i=1,im
          if(acv(i) > 0.) then
!           cvb(i) = acvb(i)
!           cvt(i) = acvt(i)
c....   convert cvt and cvb to pressures
            itop   = nint(acvt(i))
            cvt(i) = prsi(i,itop+1) * 0.001     ! from pa to kpa
            ibot   = nint(acvb(i))
            cvb(i) = prsi(i,ibot)   * 0.001     ! from pa to kpa
          else
!           cvb(i) = cvb0
            cvb(i) = 0.
            cvt(i) = 0.
          endif
          pmd(i)   = acv(i)*(24.e+3/ah)
          nmd(i)   = 0
        enddo
        do n=1,ncc
          do i=1,im
            if(pmd(i) > p(n)) nmd(i) = n
          enddo
        enddo
        do i=1,im
          if(nmd(i) == 0) then
            cv(i)  = 0.
!           cvb(i) = cvb0
            cvb(i) = 0.
            cvt(i) = 0.
          elseif(nmd(i) == ncc) then
            cv(i)  = cc(ncc)
          else
            cc1    = cc(nmd(i))
            cc2    = cc(nmd(i)+1)
            p1     = p(nmd(i))
            p2     = p(nmd(i)+1)
            cv(i)  = cc1 + (cc2-cc1)*(pmd(i)-p1)/(p2-p1)
          endif
        enddo
      endif
      return
      end

