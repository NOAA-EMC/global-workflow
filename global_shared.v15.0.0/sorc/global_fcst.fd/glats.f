      subroutine glats(lgghaf,colrad,wgt,wgtcs,rcs2,iprint)
cc
!! nov 2012   henry juang  increase precision by selected_real_kind
!!                         to help wgt (gaussian weighting)
      use resol_def
      implicit none

      integer                  iter,k,k1,l2,lgghaf,iprint
cc
      real(kind=kind_dbl_prec) colrad(lgghaf)
      real(kind=kind_dbl_prec)    wgt(lgghaf)
      real(kind=kind_dbl_prec)  wgtcs(lgghaf)
      real(kind=kind_dbl_prec)   rcs2(lgghaf)
cc
! incread precision for more significant digit to help wgt by henry juang
      real(kind=kind_qdt_prec) drad,dradz,eps,p1,p2,phi,pi,rad,rc
      real(kind=kind_qdt_prec) rl2,scale,si,sn,w,x
cc
      real(kind=kind_dbl_prec) cons0,cons0p25,cons1             !constant
      real(kind=kind_dbl_prec) cons2,cons4,cons180,cons360      !constant
cc
      cons0    =   0.d0       !constant
      cons0p25 =   0.25d0     !constant
      cons1    =   1.d0       !constant
      cons2    =   2.d0       !constant
      cons4    =   4.d0       !constant
      cons180  = 180.d0       !constant
      cons360  = 360.d0       !constant
cc
! hmhj for better accuracy with x-number poly routine
!     eps=10.*epsilon(x)
!     eps=1.d-25              !constant
      eps=1.d-20              !constant
cc
cjfe  if(iprint.eq.1) print 101
 101  format ('   i   colat   colrad     wgt', 12x, 'wgtcs',
     1 10x, 'iter  res')
      si = cons1                  !constant
      l2=2*lgghaf
      rl2=l2
      scale = cons2/(rl2*rl2)     !constant
      k1=l2-1
      pi = atan(si)*cons4         !constant
!hmhj dradz = pi / cons360 / 100.0       !constant
!hmhj for better accuracy to start based on resolution
      dradz = pi / float(lgghaf) / 200.0       !constant
      rad = cons0                 !constant
      do 1000 k=1,lgghaf
      iter=0
      drad=dradz
1     call poly(l2,rad,p2)
2     p1 =p2
      iter=iter+1
      rad=rad+drad
      call poly(l2,rad,p2)
      if(sign(si,p1).eq.sign(si,p2)) go to 2
      if(drad.lt.eps)go to 3
      rad=rad-drad
      drad = drad * cons0p25                 !constant
      go to 1
3     continue
      colrad(k)=rad
      phi = rad * cons180 / pi               !constant
      call poly(k1,rad,p1)
      x = cos(rad)
      w = scale * (cons1 - x*x)/ (p1*p1)     !constant
      wgt(k) = w
      sn = sin(rad)
      w=w/(sn*sn)
      wgtcs(k) = w
      rc=cons1/(sn*sn)                       !constant
      rcs2(k) = rc
      call poly(l2,rad,p1)
cjfe  if(iprint.eq.1)
c$$$       print 102,k,phi,colrad(k),wgt(k),wgtcs(k),iter,p1
c$$$ 102  format(1x,i3,2x,f6.2,2x,f10.7,2x,e13.7,2x,e13.7,2x,i4,2x,e13.7)
1000  continue
cjfe  if(iprint.eq.1) print 100,lgghaf
100   format(1h ,'shalom from 0.0e0 glats for ',i3)
cc
      return
      end

      subroutine poly(n,rad,p)
cc
      use machine
      implicit none
cc
      integer                  i,n
cc
! incread precision for more significant digit to help wgt by henry juang
      real(kind=kind_qdt_prec) g,p,rad,x,y1,y2,y3
      real(kind=kind_dbl_prec) cons1
cc
      cons1 = 1.d0     !constant
cc
      x = cos(rad)
      y1 = cons1       !constant
      y2=x
      do i=2,n
        g=x*y2
        y3=g-y1+g-(g-y1)/float(i)
        y1=y2
        y2=y3
      end do

      p = y3
      return
      end
