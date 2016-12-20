      subroutine get_cd_hyb(dt)
      use machine , only : kind_phys
      use resol_def
      use coordinate_def                                                ! hmhj
      implicit none
      integer              i,j,k,n,nn
      real(kind=kind_evod) dt,rnn1
      real(kind=kind_evod) ym(levs,levs)
      real(kind=kind_evod) rim(levs,levs)
!sela real(kind=kind_evod) dm205_hyb(jcap1,levs,levs)
      real(kind=kind_evod) ddd(jcap1),ppp(jcap1),rrr(jcap1)
      real(kind=kind_evod) cons0,cons1     !constant
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
 
      call am_bm_hyb
 
      do 250 k=1,levs
      do 200 j=1,levs
      rim(j,k)=cons0     !constant
200   continue
250   continue
 
      do 1 k=1,levs
      rim(k,k) = cons1     !constant
1     continue
 
c***********************************************************************
c
c       initialisation of d_hyb_m.
c
c***********************************************************************
c
c     computations which do not depend on n
c     *************************************
c
!-------------------------------------------------------
      do 10 i=1,levs
 
      do  7 j=1,levs
       ym(i,j) = tor_hyb(i)*svhyb(j)
7     continue
 
      do 9 k=1,levs
      do 8 j=1,levs
      ym(i,j) = ym(i,j) + amhyb(i,k)*bmhyb(k,j)
8     continue
9     continue
 
10    continue
!-------------------------------------------------------
c
c     computations which on n
c     ***********************
!..................................................................
      do 2000 nn=1,jcap1
 
       n = nn-1
       rnn1 =       n*(n+1)
 
       do 14 i=1,levs
       do 13 j=1,levs
        dm205_hyb(nn,i,j) = rim(i,j) + rnn1*dt*dt*ym(i,j)
13     continue
14     continue
 
2000  continue
!..................................................................
      call matinv(dm205_hyb,jcap1,levs,ddd,ppp,rrr)
      do 23 nn=1,jcap1
      do 22 i=1,levs
      do 21 j=1,levs
      d_hyb_m(i,j,nn)=dm205_hyb(nn,i,j)
21    continue
22    continue
23    continue
!     print 100,dt
100   format(1h ,'completed hyb sicdif preparation getcd_hyb dt=',f7.1)
!     return
      end
      subroutine matinv(a,m,n,d,p,r)
      use machine
      implicit none
      integer              i,j,k,l,m,n
      real(kind=kind_evod) a(m,n,n),d(m),p(m),r(m)
      real(kind=kind_evod) cons0,cons1     !constant
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
      do 200 l=1,m
      d(l)=cons1     !constant
  200 continue
      do 100 k=1,n
      do 250 l=1,m
      p(l)=a(l,k,k)
  250 continue
      do 300 l=1,m
      r(l)=-cons1/p(l)     !constant
  300 continue
      do 350 l=1,m
      a(l,k,k)=cons0       !constant
  350 continue
      do  20 i=1,n
      do 400 l=1,m
      a(l,i,k)=a(l,i,k)*r(l)
  400 continue
   20 continue
      do 60 i=1,n
      if(i.eq.k) go to 60
      do  40 j=1,n
      do 450 l=1,m
      a(l,i,j)=a(l,i,k)*a(l,k,j)+a(l,i,j)
  450 continue
   40 continue
   60 continue
      do 600 l=1,m
      r(l)=-r(l)
  600 continue
      do  80 j=1,n
      do 650 l=1,m
      a(l,k,j)=a(l,k,j)*r(l)
  650 continue
   80 continue
      do 700 l=1,m
      d(l)=d(l)*p(l)
  700 continue
      do 750 l=1,m
      a(l,k,k)=r(l)
  750 continue
  100 continue
      return
      end
