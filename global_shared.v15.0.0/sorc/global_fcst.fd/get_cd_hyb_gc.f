      subroutine get_cd_hyb_gc(dti)
      use machine , only : kind_phys
      use resol_def
      use coordinate_def
      use namelist_def
      implicit none
      integer              i,j,k,n,nn
      real(kind=kind_evod) dti,dt,rnn1
      real(kind=kind_evod) ym(levs,levs)
      real(kind=kind_evod) rim(levs,levs)
!     real(kind=kind_evod) dm205_hyb(jcap1,levs,levs)
      real(kind=kind_evod) ddd(jcap1),ppp(jcap1),rrr(jcap1)
      real(kind=kind_evod) cons0,cons1     !constant

!     print *,' enter get_cd_hyb_gc_h '

      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant

! hmhj forward-weighted semi-implicit if eps_si >0
!      center-averaged semi-implicit if eps_si=0
      if( vertcoord_id.eq.3. ) then
        eps_si=0.20
      else
        eps_si=0.00
      endif
      if( ndsl ) eps_si=0.50
      dt=(cons1+eps_si)*dti
 
      call am_bm_hyb_gc
 
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
!hmhj print 100,dt
100   format(1h ,'completed hyb sicdif preparation getcd_hyb dt=',f7.1)

!     print *,' end of get_cd_hyb_gc_h '

      return
      end
