      subroutine glats_stochy(lgghaf,colrad,wgt,wgtcs,rcs2,iprint)
!
! Jan 2013   Henry Juang  increase precision by kind_qdt_prec=16
!                         to help wgt (Gaussian weighting)
      use machine
      implicit none
      integer                  iter,k,k1,l2,lgghaf,iprint
!
! increase precision for more significant digit to help wgt
      real(kind=kind_qdt_prec) drad,dradz,p1,p2,phi,pi,rad,rc
!     real(kind=kind_qdt_prec) drad,dradz,eps,p1,p2,phi,pi,rad,rc
      real(kind=kind_qdt_prec) rl2,scale,si,sn,w,x
!
      real(kind=kind_dbl_prec), dimension(lgghaf) ::  colrad, wgt,
     &                                                wgtcs,  rcs2
!
      real(kind=kind_dbl_prec), parameter :: cons0 = 0.d0, cons1 = 1.d0,
     &                                       cons2 = 2.d0, cons4 = 4.d0,
     &                                       cons180 = 180.d0,
     &                                       cons360 = 360.d0,
     &                                       cons0p25 = 0.25d0
      real(kind=kind_qdt_prec), parameter :: eps = 1.d-20
!
! for better accuracy to select smaller number
!     eps = 1.d-12
!     eps = 1.d-20
!
      if(iprint == 1) print 101
 101  format ('   i   colat   colrad     wgt', 12x, 'wgtcs',
     & 10x, 'iter  res')
      si    = cons1
      l2    = 2*lgghaf
      rl2   = l2
      scale = cons2/(rl2*rl2)
      k1    = l2-1
      pi    = atan(si)*cons4
!     dradz = pi / cons360 / 10.0
!  for better accuracy to start iteration
      dradz = pi / float(lgghaf) / 200.0
      rad   = cons0
      do k=1,lgghaf
        iter = 0
        drad = dradz
1       call poly(l2,rad,p2)
2       p1 = p2
        iter = iter + 1
        rad = rad + drad
        call poly(l2,rad,p2)
        if(sign(si,p1) == sign(si,p2)) go to 2
        if(drad < eps)go to 3
        rad  = rad-drad
        drad = drad * cons0p25
        go to 1
3       continue
        colrad(k) = rad
        phi = rad * cons180 / pi
        call poly(k1,rad,p1)
        x        = cos(rad)
        w        =  scale * (cons1 - x*x)/ (p1*p1)
        wgt(k)   = w
        sn       = sin(rad)
        w        = w/(sn*sn)
        wgtcs(k) = w
        rc       = cons1/(sn*sn)
        rcs2(k)  = rc
        call poly(l2,rad,p1)
        if(iprint == 1)
     &       print 102,k,phi,colrad(k),wgt(k),wgtcs(k),iter,p1
 102    format(1x,i3,2x,f6.2,2x,f10.7,2x,e14.7,2x,e14.7,2x,i4,2x,e14.7)
      enddo
      if(iprint == 1) print 100,lgghaf
100   format(1h ,'shalom from 0.0e0 glats for ',i3)
!
      return
      end

      subroutine poly(n,rad,p)
      use machine
!
      implicit none
!
      integer                  i,n
!
! increase precision for more significant digit to help wgt
      real(kind=kind_qdt_prec) floati,g,p,rad,x,y1,y2,y3
!
      real(kind=kind_dbl_prec), parameter ::  cons1 = 1.d0
!
      x  = cos(rad)
      y1 = cons1
      y2 = x
      do i=2,n
        g = x*y2
        floati = i
        y3 = g - y1 + g - (g-y1)/floati
        y1 = y2
        y2 = y3
      enddo
      p = y3
      return
      end

