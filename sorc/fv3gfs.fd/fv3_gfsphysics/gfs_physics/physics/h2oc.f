c***********************************************************************
c***********************************************************************
c File h2ocup.f created for data storage, initialization, and 
c calculation of H2O IR cooling rates in the rotational and 6.3-mum
c vibrational bands after Xun Zhu (1999) reusing some of his code and 
c data
c September, 2007: made by Rashid Akmaev for a pressure grid going 
c       upward
! Spr 06 2012    Henry Juang, initial implement for nems
! Oct    2012    Jun Wang,    change reading files by 1 pe reading and
!                             broardcasting to all pes
! Dec    2012    Jun Wang,    move init out of column physics
!
c Contains
c      module h2ocm
c      subroutine h2ocin(p0,lx)	! hmhj modified
c      subroutine h2occ(t,p0,wvmmr,qr,qv,lx)
c***********************************************************************
c***********************************************************************

      module h2ocm

c Module to keep data for calculation of H2O IR cooling after Zhu (1994)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c September 30, 2003
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Parameters
c General
      real,parameter:: daysec=86400.,r_daysec=1./daysec

c Integers precalculated in h2ocin:
c -number of model starting layer (counted from the top)
c -number of model layers in a 1 scale height above the top
C       parameterization layer
      integer        lh2oc,ltop1

c Parameterization arrays precalculated for model grid from band data
c in h2ocin:
c -optical band parameters
c -reference H2O MMR
c -interpolation coefficients
      real     ,allocatable,dimension(:):: gh2ort,gh2ovb,dg1rt,dg2rt,
     $     dg1vb,dg2vb,gdp,xx,wvmmrc,coeff

      end module h2ocm

c***********************************************************************
c***********************************************************************

      subroutine h2ocin(p0,lx,me,mpi_ior,mpi_comm)
!hmhj subroutine h2ocin(p0,lx,dir)

c Subroutine to initialize calculations of H2O IR cooling rates done 
c by h2occ after Zhu (1994)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Nov 12, 2008: corrected errors in calculation of gdp
c Sep 24, 2007: made from h2ocin for upward model grid
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      use h2ocm
      implicit none
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Arguments
c INPUT
c -mid-layer model pressure (Pa) grid levels going up
      integer,intent(in):: lx,me,mpi_ior,mpi_comm
      real,intent(in),dimension(lx):: p0

c -directory where input files are located
!hmhj character(len=*),intent(in):: dir

c - OUTPUT: placed in the module
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Internal parameters
      real,parameter:: delr0=.5,refpre=1./3e3
      real,dimension(lx)::tref
      integer,parameter:: lmr=3,lmt=1

c Work space
      integer:: l,lu
      real:: workx
      real,dimension(lx,lmr,lmt):: gamyrt,gamyvb
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Allocate 10 module arrays
      allocate(gh2ort(lx),gh2ovb(lx),dg1rt(lx),dg2rt(lx),
     $     dg1vb(lx),dg2vb(lx),gdp(lx),xx(lx),wvmmrc(lx),coeff(lx))

c Initialize module parameters (1 integer and 9 real arrays). These 
c internal arrays go down for convenient calculation in the 
c cooling-to-space approximation. lh2oc is the starting model level
c (counting down from top) corresponding to approximately 100 km. Above
c that level the cooling rates are extrapolated to 0 within one scale
c height (within ltop1 model layers). If the model top does not reach
c 100 km, lh2oc=1, ltop1=0; if the model bottom is above 100 km, 
c lh2oc > lx and cooling rates are set to 0 in the model domain.
      lh2oc=lx+1
      ltop1=0
      gh2ort(:)=0.
      gh2ovb(:)=0.
      dg1rt(:)=0.
      dg2rt(:)=0.
      dg1vb(:)=0.
      dg2vb(:)=0.
      wvmmrc(:)=0.
      gdp(:)=0.
      xx(:)=0.
      coeff(:)=0.

c Precalculate parameters for matrix interpolation

c Prepare reference atmosphere on model grid and other grid params. In
c the call to wvrefm, the model grid is inversed (goes down from the 
c top) for compatibility with Xun's original code and to simplify 
c calculations in the cooling-to-space approximation
      tref(:)=0.

      call wvrefm(p0(lx:1:-1),wvmmrc,tref,coeff,lx,lh2oc,ltop1)
c     print*,'www1',lx,lh2oc,ltop1

      if(lh2oc > lx) return

      gdp(lh2oc)=(1.+refpre*p0(lx+1-lh2oc))*(p0(lx-lh2oc)-
     $     p0(lx+1-lh2oc))
      gdp(lx)=(1.+refpre*p0(1))*(p0(1)-p0(2))
      do l=(lh2oc+1),(lx-1)
         lu=lx+1-l
         gdp(l)=.5*(1.+refpre*p0(lu))*(p0(lu-1)-p0(lu+1))
      enddo

      workx=0.
      do l=lh2oc,lx
         workx=workx+delr0*wvmmrc(l)*gdp(l)
         xx(l)=1./workx
      enddo

      l=lx-lh2oc+1

      call g1rtxz(l,tref(lh2oc:),p0(l:1:-1),wvmmrc(lh2oc:),
     $     lmr,lmt,gamyrt(lh2oc:,:,:),me,mpi_ior,mpi_comm)
!hmhj$     lmr,lmt,gamyrt(lh2oc:,:,:),dir)
      call g1vbxz(l,tref(lh2oc:),p0(l:1:-1),wvmmrc(lh2oc:),
     $     lmr,lmt,gamyvb(lh2oc:,:,:),me,mpi_ior,mpi_comm)
!hmhj$     lmr,lmt,gamyvb(lh2oc:,:,:),dir)
      call gtoaxz(l,lmr,gamyrt(lh2oc:,:,:),gamyvb(lh2oc:,:,:),
     $     dg1rt(lh2oc:),dg2rt(lh2oc:),dg1vb(lh2oc:),dg2vb(lh2oc:))

      do l=lh2oc,lx
         gh2ort(l)=gamyrt(l,2,1)
         gh2ovb(l)=gamyvb(l,2,1)
      enddo

      end subroutine h2ocin

c***********************************************************************
c***********************************************************************

      subroutine h2occ(t,p0,wvmmr,qr,qv,lx)
c Subroutine to calculate H2O IR cooling rates after Zhu (1994).  Made
c using his code, substantially rewritten.
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Sep 24, 2007: Made from h2oc_calc for upward pressure grid
c October 1, 2003
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      use h2ocm
      implicit none
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Arguments
c - IN: temperature (K), pressure (Pa), and H2O MMR (relative units)
C       on the same model grid going up as in h2ocin
      integer:: lx
      real,intent(in),dimension(lx):: t,p0,wvmmr

c - OUT: heating rates (K/s) in the rotational and vibrational bands, 
c respectively
      real,dimension(lx),intent(out):: qr,qv
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Internal parameters
c - inverse of reference H2O MMR
      real,parameter:: rrmmr=1./(3.e-6)

c Work space (most of it kept for historic compatibility reasons)
      integer:: l,lu,lw
      real:: phiv,thr,thv,gr2,gv2,wk1,wk2,yy
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      qr(:)=0.
      qv(:)=0.

      if(lh2oc > lx) return

c Calculate cooling rates up to layer lh2oc. Index l goes down, lu goes
c up
      yy=0.
      lw=lx+1
      do l=lh2oc,lx
         lu=lw-l
         yy=yy+(wvmmr(lu)-wvmmrc(l))*gdp(l)
         wk1=yy*xx(l)
         wk2=wvmmr(lu)*rrmmr*r_daysec
c
c 2*(4.3E-20/20.5)*air density in m^-3 (Coefficient 2 is due to
c     rearranged expression for calculation of qv)
c
         phiv=420.*p0(lu)/(1.380658*t(lu))
         thr=81.6/(exp(568.01/t(lu))-1.)         ! S*B in K/day
         thv=2730.0/(exp(2300.8/t(lu))-1.)       ! S*B in K/day
         gr2=gh2ort(l)+wk1*(dg1rt(l)+dg2rt(l)*wk1)
         gv2=gh2ovb(l)+wk1*(dg1vb(l)+dg2vb(l)*wk1)
         qr(lu)=-wk2*thr*gr2
         qv(lu)=-wk2*thv*gv2*phiv/(phiv+gv2)
      enddo

c Extrapolate cooling rates linearly to zero a scale height above lh2oc
      if(ltop1 > 0) then
         lw=lx+1-lh2oc
         do l=1,ltop1
            lu=lw+l
            qr(lu)=qr(lw)*(1.-coeff(l))
            qv(lu)=qv(lw)*(1.-coeff(l))
         enddo
      endif

      end subroutine h2occ

c***********************************************************************
c***********************************************************************
c End of file h2ocup.f
c***********************************************************************
c***********************************************************************

c***********************************************************************
c File h2olib contains 10 subroutines/functions used by h2oc.f
c Nov 13, 2008: updated
c     subroutine wvrefm
c Sept, 2007: Assembled by Rashid Akmaev
c***********************************************************************

      FUNCTION BLAC(V,T)
C  Planck black-body function J/m/s at the wavenumber v and
C  temperature T.  B = [2hv**3*c**2]/[exp(hcv/kT)-1] with
C  h=6.6262E-34 Js, c=2.998E8 m/s, k=1.381E-23 J/K, v~6.75E4 m^-1.
C  f1=2hc*c=1.19109E-16 Jm*m/s, f2=hc/k=0.0143847 mK.
      BLAC=1.19109E-16*V**3/(EXP(0.0143847*V/T)-1.0)

      END function blac

c***********************************************************************

      FUNCTION ENZ2(Z)
CC
C  Calcualte exponential integral from polynomial and rational 
C  approximation E2(z)=[exp(-z)-zE1(z)]=exp(-z)[1-exp(z)zE1(z)]
CC
      IF(Z.LE.1.0) THEN
        IF(Z.LE.1.0E-35) THEN
           ENZ2=1.0
           RETURN
        ENDIF
      ENZ=-ALOG(Z)-0.57721566+Z*(0.99999193-Z*(0.24991055
     1 -Z*(0.05519968-Z*(0.00976004E0-Z*0.00107857))))
      ENZ2=EXP(-Z)-Z*ENZ
      RETURN
      ENDIF
      IF(Z.GE.80.0) THEN
      ENZ2=0.0
      RETURN
      ENDIF
      ENZ=(0.2677737343+Z*(8.6347608925+Z*(18.0590169730+Z*
     &  (8.5733287401+Z))))/(3.9584969228+Z*(21.0996530827+Z*
     &  (25.6329561486+Z*(9.5733223454+Z))))
      ENZ2=EXP(-Z)*(1.0-ENZ)

      END function enz2

c***********************************************************************

      subroutine g1rtxz(kus,tus0,pus,rus0,lmr,lmt,gamav,                 &
     &  me,mpi_ior,mpi_comm)
!hmhj subroutine g1rtxz(kus,tus0,pus,rus0,lmr,lmt,gamav,dirin)

c Sept, 2007: made by Rashid Akmaev from Xun Zhu's code for H2O cooling
!
      include 'mpif.h'

      integer,intent(in):: kus,lmr,lmt,me,mpi_ior,mpi_comm
      real,intent(in):: pus(kus),TUS0(KUS),RUS0(KUS)
!hmhj character(len=*),intent(in) :: dirin

      real,intent(out):: gamav(kus,lmr,lmt)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Internal parameters and work space
      PARAMETER (NQUS=17,KFS=20,KM=40,MM4=7,NQ=17)

      real,dimension(km) :: GG,WW
      real :: XK2(KFS,NQ,KM,MM4)
      real :: PRE(KFS),TEM4(MM4),T77(MM4)
      real :: XKUS(KUS,NQ,KM,MM4)
      integer info

      dimension TUS(KUS),RUS(KUS)

      dimension VNQ(NQ),VNQS(NQUS),STRB(KUS,NQUS),GAMSP(KUS,NQUS)
     & ,QB1(KUS,NQUS),QB2(KUS,NQUS),QBT(KUS,NQUS)
     & ,QAL1(KUS,LMR,LMT),QAL2(KUS,LMR,LMT),QALT(KUS,LMR,LMT)

      DIMENSION WK1(KUS),WK2(KUS),WK3(KUS),WK4(KUS)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      DDV=25.0E2             ! unit of bandwidth: m^-1
      V00=50.0E2             ! [nu]0 for H2O in m^-1

      DO 2 N1=1,NQ
   2  VNQ(N1)=V00+DDV*(FLOAT(N1)-0.5)
!
! open first pe to read the file
      if(me==0) then

        OPEN(11,FILE='global_idea_ggww_in4.par',STATUS='OLD')
        DO 5 K2=1,KM
   5      READ(11,*) GG(K2),WW(K2)
        CLOSE(UNIT=11)

        OPEN(71,FILE='global_idea_h2ort_kg7t.par',STATUS='OLD')
        DO 7 ICON=1,MM4
        DO 7 N1=1,NQ
        DO 7 K1=1,KFS       !  read in the k-coefficient in m*m/kg
          READ(71,*) (XK2(K1,N1,K2,ICON),K2=1,KM)
   7    CONTINUE
        CLOSE(UNIT=71)

      endif
!
!      print *,'bf mpi_bcast, km=',km,'MPI_IOR=',MPI_IOR,mpi_comm,
!     &   size(gg),mpi_real4
      call mpi_bcast(GG,KM,MPI_REAL8,0,mpi_comm,info)
      call mpi_bcast(WW,KM,MPI_REAL8,0,mpi_comm,info)
      call mpi_bcast(XK2,MM4*NQ*KFS*KM,MPI_REAL8,0,mpi_comm,info)

      DO 9 M=1,MM4
        TEM4(M)=150.0+FLOAT(M-1)*25.0     !  7 reference temperatures
   9    T77(M)=TEM4(M)

      KFSM1=KFS-1
      PRE(1)=1.0E-4
      FACT2=10.0E0**0.25E0
      PRE(2)=10.0E0**0.5E0
      PRE(3)=10.0E0
      DO 10 K=4,KFSM1
        PRE(K)=PRE(K-1)*FACT2
 10   CONTINUE

      PRE(KFS)=1.1E5             ! 20 reference pressures

      DELTR=0.5
    
      DO 500 LLR=1,LMR
      DO 500 LLT=1,LMT
      
        DO 33 K=1,KUS
         TUS(K)=TUS0(K)
         RUS(K)=RUS0(K)*(1.0+DELTR*FLOAT(LLR-2))
  33    CONTINUE
      
        CALL INTERK(kus,kfs,km,mm4,nq,pre,xk2,pus,xkus)

        DO 80 N=1,NQUS
          VNQS(N)=V00+DDV*(FLOAT(N)-0.5)
          CALL QSINGL(WK1,WK2,RUS,PUS,TUS,WK3,WK4,KUS,
     &      WW,GG,KM,N,VNQS(N),DDV,1,mm4,nq,t77,xkus)
          DO 60 K=1,KUS
            QB1(K,N)=WK1(K)
            QB2(K,N)=WK2(K)
            QBT(K,N)=QB1(K,N)+QB2(K,N)
            STRB(K,N)=WK3(K)
            GAMSP(K,N)=WK4(K)
  60      CONTINUE
  80    CONTINUE

        DO 100 K=1,KUS
          QAL1(K,LLR,LLT)=0.0
          QAL2(K,LLR,LLT)=0.0
          QALT(K,LLR,LLT)=0.0
          GAMAV(K,LLR,LLT)=0.0
          SUMX1=0.0
!
          DO 90 N=1,NQUS
            QAL1(K,LLR,LLT)=QAL1(K,LLR,LLT)+QB1(K,N)
            QAL2(K,LLR,LLT)=QAL2(K,LLR,LLT)+QB2(K,N)
            QALT(K,LLR,LLT)=QALT(K,LLR,LLT)+QBT(K,N)
            FACX1=STRB(K,N)*BLAC(VNQS(N),TUS(K))
            GAMAV(K,LLR,LLT)=GAMAV(K,LLR,LLT)+FACX1*GAMSP(K,N)
            SUMX1=SUMX1+FACX1
  90      CONTINUE
          GAMAV(K,LLR,LLT)=GAMAV(K,LLR,LLT)/SUMX1
 100    CONTINUE
  
 500  CONTINUE

      END subroutine g1rtxz

c***********************************************************************

      subroutine g1vbxz(kus,tus0,pus,rus0,lmr,lmt,gamav,                 &
     &   me,mpi_ior,mpi_comm)

c Sept, 2007: made by Rashid Akmaev from Xun Zhu's code for H2O cooling
!
       include 'mpif.h'

       integer,intent(in):: kus,lmr,lmt
       real,intent(in):: pus(kus),TUS0(KUS),RUS0(KUS)

       real,intent(out):: gamav(kus,lmr,lmt)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Internal parameters and work space
       PARAMETER (NQUS=14,KFS=20,KM=30,MM4=7,NQ=14)

      dimension PRE(KFS),TEM4(MM4),GG(KM),WW(KM),XK2(KFS,NQ,KM,MM4)
      dimension T77(MM4),XKUS(KUS,NQ,KM,MM4)

      dimension TUS(KUS),RUS(KUS)

      dimension VNQ(NQ),VNQS(NQUS),STRB(KUS,NQUS),GAMSP(KUS,NQUS)
     & ,QB1(KUS,NQUS),QB2(KUS,NQUS),QBT(KUS,NQUS)
     & ,QAL1(KUS,LMR,LMT),QAL2(KUS,LMR,LMT),QALT(KUS,LMR,LMT)

      DIMENSION WK1(KUS),WK2(KUS),WK3(KUS),WK4(KUS)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      DDV=50.0E2             ! unit of bandwidth: m^-1
      V00=1300.0E2             ! [nu]0 for H2O in m^-1

      DO 2 N1=1,NQ
   2  VNQ(N1)=V00+DDV*(FLOAT(N1)-0.5)
!
! only pe 0 read the file
      if(me==0) then

        OPEN(11,FILE='global_idea_ggww_in1.par',STATUS='OLD')
        DO 5 K2=1,KM
   5      READ(11,*) GG(K2),WW(K2)
        CLOSE(UNIT=11)

        OPEN(71,FILE='global_idea_h2ovb_kg7t.par',STATUS='OLD')
        DO 7 ICON=1,MM4
        DO 7 N1=1,NQ
        DO 7 K1=1,KFS         !  read in the k-coefficient in m*m/kg
          READ(71,*) (XK2(K1,N1,K2,ICON),K2=1,KM)
   7    CONTINUE
        CLOSE(UNIT=71)

      endif
!
      call mpi_bcast(GG,KM,MPI_IOR,0,mpi_comm,info)
      call mpi_bcast(WW,KM,MPI_IOR,0,mpi_comm,info)
      call mpi_bcast(XK2,MM4*NQ*KFS*KM,MPI_IOR,0,mpi_comm,info)


      DO 9 M=1,MM4
        TEM4(M)=150.0+FLOAT(M-1)*25.0     !  7 reference temperatures
   9    T77(M)=TEM4(M)

      KFSM1=KFS-1
      PRE(1)=1.0E-4
      FACT2=10.0E0**0.25E0
      PRE(2)=10.0E0**0.5E0
      PRE(3)=10.0E0
      DO 10 K=4,KFSM1
        PRE(K)=PRE(K-1)*FACT2
 10   CONTINUE

      PRE(KFS)=1.1E5             ! 20 reference pressures

      DELTR=0.5
      
      DO 500 LLR=1,LMR
      DO 500 LLT=1,LMT
      
        DO 33 K=1,KUS
          TUS(K)=TUS0(K)
          RUS(K)=RUS0(K)*(1.0+DELTR*FLOAT(LLR-2))
  33    CONTINUE
      
        CALL INTERK(kus,kfs,km,mm4,nq,pre,xk2,pus,xkus)

        DO 80 N=1,NQUS
          VNQS(N)=V00+DDV*(FLOAT(N)-0.5)
          CALL QSINGL(WK1,WK2,RUS,PUS,TUS,WK3,WK4,KUS,
     &      WW,GG,KM,N,VNQS(N),DDV,1,mm4,nq,t77,xkus)
          DO 60 K=1,KUS
            QB1(K,N)=WK1(K)
            QB2(K,N)=WK2(K)
            QBT(K,N)=QB1(K,N)+QB2(K,N)
            STRB(K,N)=WK3(K)
            GAMSP(K,N)=WK4(K)
  60      CONTINUE
  80    CONTINUE

        DO 100 K=1,KUS
          QAL1(K,LLR,LLT)=0.0
          QAL2(K,LLR,LLT)=0.0
          QALT(K,LLR,LLT)=0.0
          GAMAV(K,LLR,LLT)=0.0
          SUMX1=0.0
!
          DO 90 N=1,NQUS
            QAL1(K,LLR,LLT)=QAL1(K,LLR,LLT)+QB1(K,N)
            QAL2(K,LLR,LLT)=QAL2(K,LLR,LLT)+QB2(K,N)
            QALT(K,LLR,LLT)=QALT(K,LLR,LLT)+QBT(K,N)
            FACX1=STRB(K,N)*BLAC(VNQS(N),TUS(K))
            GAMAV(K,LLR,LLT)=GAMAV(K,LLR,LLT)+FACX1*GAMSP(K,N)
            SUMX1=SUMX1+FACX1
  90      CONTINUE
          GAMAV(K,LLR,LLT)=GAMAV(K,LLR,LLT)/SUMX1
 100    CONTINUE
  
 500  CONTINUE

      END subroutine g1vbxz

c***********************************************************************

      subroutine gtoaxz(kus,lmr,gamrt,gamvb,c1rt,c2rt,c1vb,c2vb)

c Sept, 2007: made by Rashid Akmaev from Xun Zhu's code for H2O cooling

      implicit none
      integer,intent(in):: kus,lmr
      real,intent(in):: gamrt(kus,lmr),gamvb(kus,lmr)

      real,intent(out)::  c1rt(kus),c2rt(kus),c1vb(kus),c2vb(kus)

      integer:: k
      
      DO K=1,KUS
      C1RT(K)=(GAMRT(K,3)-GAMRT(K,1))/2.0
      C1VB(K)=(GAMVB(K,3)-GAMVB(K,1))/2.0
      C2RT(K)=(GAMRT(K,3)+GAMRT(K,1)-2.0*GAMRT(K,2))/2.0
      C2VB(K)=(GAMVB(K,3)+GAMVB(K,1)-2.0*GAMVB(K,2))/2.0
      enddo

      end subroutine gtoaxz

c***********************************************************************

      SUBROUTINE INTERK(kus,kfs,km,mm4,nq,pre,xk2,pus,xkus)

      implicit none
      integer,intent(in):: kus,kfs,km,mm4,nq
      real,intent(in):: pre(kfs),xk2(kfs,nq,km,mm4),pus(kus)
      real,intent(out):: xkus(kus,nq,km,mm4)
      
      integer:: i,k,kref,k2,m,n1
      real:: dy,yk,xx,WR1(2),WR2(2)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      DO 60 K2=1,KUS

      DO 10 K=1,KFS
      KREF=K
      IF(PUS(K2).LT.PRE(K)) GO TO 12
  10  CONTINUE
  12  CONTINUE

      IF(KREF.EQ.1) KREF=2
      WR1(1)=ALOG(PRE(KREF-1))
      WR1(2)=ALOG(PRE(KREF))

      DO 50 N1=1,NQ
      DO 50 M=1,MM4
      DO 50 I=1,KM
      WR2(1)=ALOG(XK2(KREF-1,N1,I,M))
      WR2(2)=ALOG(XK2(KREF,N1,I,M))
      XX=ALOG(PUS(K2))
      CALL POLINT(WR1,WR2,2,XX,YK,DY)
      XKUS(K2,N1,I,M)=EXP(YK)
  50  CONTINUE

  60  CONTINUE

      END subroutine interk

c***********************************************************************
      
      SUBROUTINE POLINT(XA,YA,N,X,Y,DY)
C  Polynomial interpolation from "Numerical Recipes"
      PARAMETER (NMAX=10) 
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)
      NS=1
      DIF=ABS(X-XA(1))
      DO 11 I=1,N 
        DIFT=ABS(X-XA(I))
        IF (DIFT.LT.DIF) THEN
          NS=I
          DIF=DIFT
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          HO=XA(I)-X
          HP=XA(I+M)-X
          W=C(I+1)-D(I)
          DEN=HO-HP
!!!compiler warning          IF(DEN.EQ.0.) PAUSE
          IF(DEN.EQ.0.) STOP 'DEN.EQ.0. in POLINT'
          DEN=W/DEN
          D(I)=HP*DEN
          C(I)=HO*DEN
12      CONTINUE
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE

      END subroutine polint

c***********************************************************************

      SUBROUTINE QSINGL(Q1,Q2,RX,PRE,TEM,STR,GAMS,KM,
     &   WI,GI,IM,N,VM,DVM,ITOP,mm4,nq,t77,xkus)

C Cooling rate by a single line
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      dimension t77(mm4),xkus(km,nq,im,mm4)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CCCCC
C    To calculate the cooling rate by a single line located at VM in 1/m
C    with width DVM. PRE = pressure in pascal, TEM = temperature in Kelvin, 
C    SI = k-coefficient at GI in m*m/kg, RX = mass mixing ratio
C    Q1 & Q2 = cool-to-space & heat exchange cooling rate in K/day,
C    ITOP=0 ==> PRE(KM)->0;   ITOP=1 ==> PRE(1)->0.
CCCCC
      PARAMETER (KMAX=150)
      DIMENSION Q1(KM),Q2(KM),RX(KM),PRE(KM),TEM(KM),GAMS(KM)
     & ,STR(KM),WI(IM),GI(IM),GAM1(KMAX,KMAX),BJX(KMAX),H00(KMAX)
      KMM=KM-1
      PI=3.14159265
      GRAV1=9.8             ! gravitational constant in m s**(-2)
      B250=BLAC(VM,250.0)
      DO 20 K=1,KM
      STR(K)=0.0
      DO 15 I=1,IM
      STR(K)=STR(K)+WI(I)*ZXKTP(TEM(K),K,I,N,km,im,mm4,nq,t77,xkus)
  15  CONTINUE
      BJX(K)=BLAC(VM,TEM(K))/B250
      H00(K)=2.0*PI*86400.0*RX(K)*(STR(K)*DVM)*B250/1004.0
  20  CONTINUE
      DO 150 I=1,KM
      DO 150 J=1,KM
      GAM1(I,J)=0.0
      if(j.ne.1) go to 150
      IJ1=MIN0(I,J)
      IJ2=MAX0(I,J)-1
      DO 120 L=1,IM
      IF(I.EQ.J) GO TO 50
      DELU=0.0
      DO 40 M=IJ1,IJ2
      FAC1=(ZXKTP(TEM(M),M,L,N,km,im,mm4,nq,t77,xkus)+
     $        ZXKTP(TEM(M+1),M+1,L,N,km,im,mm4,nq,t77,xkus))/2.0
      FAC2=(RX(M)+RX(M+1))/2.0
      DP1=ABS(PRE(M)-PRE(M+1))
      DELU=DELU+FAC1*FAC2*DP1
  40  CONTINUE
      DELU=DELU/GRAV1
      GO TO 100
  50  CONTINUE
      IF(I.EQ.1) DP1=ABS(PRE(1)-PRE(2))
      IF(I.EQ.KM) DP1=ABS(PRE(KM)-PRE(KM-1))
      IF(I.NE.1.AND.I.NE.KM) DP1=ABS(PRE(I+1)-PRE(I-1))/2.0
      DP1=DP1*0.5
      DELU=ZXKTP(TEM(I),I,L,N,km,im,mm4,nq,t77,xkus)*RX(I)*DP1/GRAV1
 100  CONTINUE
      GAM1(I,J)=GAM1(I,J)+
     $     WI(L)*ZXKTP(TEM(I),I,L,N,km,im,mm4,nq,t77,xkus)*ENZ2(DELU)
 120  CONTINUE
      GAM1(I,J)=GAM1(I,J)/STR(I)
 150  CONTINUE
 
      DO 200 K=1,KM
      IF(ITOP.EQ.0) GAMS(K)=GAM1(K,KM)
      IF(ITOP.EQ.1) GAMS(K)=GAM1(K,1)
      Q1(K)=-H00(K)*BJX(K)*GAMS(K)
 200  CONTINUE
      DO 400 K=1,KM
      Q2(K)=0.0
      DO 350 L=2,KM
      FAC1=(BJX(L)+BJX(L-1)-2.0*BJX(K))/2.0
      FAC2=FAC1*ABS(GAM1(K,L)-GAM1(K,L-1))
      Q2(K)=Q2(K)+FAC2
 350  CONTINUE
      Q2(K)=Q2(K)*H00(K)
 400  CONTINUE

      END

c***********************************************************************

      subroutine wvrefm(pmy,wvmy,tmy,coeff,lmy,lh2o,llin)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Nov 12, 2008: Calculation of lh2o corrected to avoid lh2o=0
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
      integer,intent(in):: lmy
      integer,intent(out):: lh2o,llin
      real,dimension(lmy),intent(in):: pmy
      real,dimension(lmy),intent(out):: coeff,wvmy,tmy
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer:: l
      real,dimension(lmy):: xmy
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Log-pressure above which cooling rates are not calculated but 
c extrapolated to 0 over a scale height (15 roughly corresponds to 
c 100 km)
      real,parameter:: xtop=15.

c Empirical model of H2O MMR and T at 0-115 km based on Xun's models
      integer,parameter:: kz0=116
      real,parameter,dimension(kz0):: x0=(/
     $ -1.3074159E-02,  1.0684384E-01,  2.2952638E-01,  3.5510332E-01,
     $  4.8371642E-01,  6.1551599E-01,  7.5066825E-01,  8.8935186E-01,
     $  1.0317549E+00,  1.1780874E+00,  1.3285728E+00,  1.4833737E+00,
     $  1.6404333E+00,  1.7975341E+00,  1.9545796E+00,  2.1115846E+00,
     $  2.2685412E+00,  2.4254411E+00,  2.5822937E+00,  2.7390828E+00,
     $  2.8957331E+00,  3.0520064E+00,  3.2076342E+00,  3.3625239E+00,
     $  3.5166655E+00,  3.6700616E+00,  3.8227218E+00,  3.9746487E+00,
     $  4.1258476E+00,  4.2763270E+00,  4.4260956E+00,  4.5751261E+00,
     $  4.7233255E+00,  4.8703174E+00,  5.0157147E+00,  5.1593975E+00,
     $  5.3013821E+00,  5.4417006E+00,  5.5803912E+00,  5.7174910E+00,
     $  5.8530322E+00,  5.9870543E+00,  6.1195843E+00,  6.2506551E+00,
     $  6.3803027E+00,  6.5085564E+00,  6.6354559E+00,  6.7611041E+00,
     $  6.8858666E+00,  7.0102588E+00,  7.1345618E+00,  7.2589112E+00,
     $  7.3836434E+00,  7.5092845E+00,  7.6361235E+00,  7.7642453E+00,
     $  7.8936802E+00,  8.0244623E+00,  8.1566209E+00,  8.2901811E+00,
     $  8.4251793E+00,  8.5616468E+00,  8.6996208E+00,  8.8391217E+00,
     $  8.9801974E+00,  9.1228883E+00,  9.2672198E+00,  9.4132390E+00,
     $  9.5609850E+00,  9.7105014E+00,  9.8618275E+00,  1.0014991E+01,
     $  1.0169933E+01,  1.0326467E+01,  1.0484460E+01,  1.0643902E+01,
     $  1.0804806E+01,  1.0967206E+01,  1.1131125E+01,  1.1296597E+01,
     $  1.1463650E+01,  1.1632315E+01,  1.1802627E+01,  1.1974618E+01,
     $  1.2148311E+01,  1.2323674E+01,  1.2500403E+01,  1.2677767E+01,
     $  1.2855241E+01,  1.3032687E+01,  1.3210080E+01,  1.3387417E+01,
     $  1.3564666E+01,  1.3741684E+01,  1.3918284E+01,  1.4094282E+01,
     $  1.4269498E+01,  1.4443745E+01,  1.4616840E+01,  1.4788594E+01,
     $  1.4958815E+01,  1.5127306E+01,  1.5293868E+01,  1.5458278E+01,
     $  1.5620313E+01,  1.5779732E+01,  1.5936241E+01,  1.6089528E+01,
     $  1.6239184E+01,  1.6384648E+01,  1.6525018E+01,  1.6659004E+01,
     $  1.6786726E+01,  1.6908743E+01,  1.7025538E+01,  1.7137535E+01/)
      real,parameter,dimension(kz0):: mmr0=(/
     $  6.3427625E-03,  4.6896568E-03,  3.2016360E-03,  2.2034693E-03,
     $  1.4804843E-03,  9.2871409E-04,  5.4321145E-04,  2.9824653E-04,
     $  1.5777393E-04,  8.5725775E-05,  5.6769328E-05,  3.0951885E-05,
     $  1.6396563E-05,  8.6909902E-06,  5.5927135E-06,  3.2080393E-06,
     $  2.1766575E-06,  1.9823482E-06,  2.2125981E-06,  2.2340431E-06,
     $  2.2576351E-06,  2.2822872E-06,  2.3105531E-06,  2.3412808E-06,
     $  2.3789415E-06,  2.4225076E-06,  2.4790281E-06,  2.5442728E-06,
     $  2.6249287E-06,  2.7176809E-06,  2.8232316E-06,  2.9402314E-06,
     $  3.0660555E-06,  3.1990915E-06,  3.3302645E-06,  3.4635988E-06,
     $  3.5798847E-06,  3.6939696E-06,  3.7870371E-06,  3.8757534E-06,
     $  3.9469313E-06,  4.0144905E-06,  4.0577301E-06,  4.0978281E-06,
     $  4.1199851E-06,  4.1398489E-06,  4.1510213E-06,  4.1606770E-06,
     $  4.1631141E-06,  4.1645934E-06,  4.1611823E-06,  4.1571837E-06,
     $  4.1505526E-06,  4.1435602E-06,  4.1325911E-06,  4.1214015E-06,
     $  4.1073816E-06,  4.0932284E-06,  4.0767109E-06,  4.0601122E-06,
     $  4.0385647E-06,  4.0169676E-06,  3.9888507E-06,  3.9607035E-06,
     $  3.9267745E-06,  3.8928269E-06,  3.8521707E-06,  3.8115032E-06,
     $  3.7608379E-06,  3.7101658E-06,  3.6459496E-06,  3.5817291E-06,
     $  3.4989563E-06,  3.4161811E-06,  3.3081287E-06,  3.2000750E-06,
     $  3.0569701E-06,  2.9138647E-06,  2.7265409E-06,  2.5392176E-06,
     $  2.2999015E-06,  2.0605864E-06,  1.7760898E-06,  1.4915889E-06,
     $  1.2209422E-06,  9.5026912E-07,  7.5322985E-07,  5.5612383E-07,
     $  4.2992454E-07,  3.0360226E-07,  2.3203341E-07,  1.6029976E-07,
     $  1.2252130E-07,  8.4579052E-08,  6.5370065E-08,  4.6036091E-08,
     $  3.6250825E-08,  2.6386714E-08,  2.1301281E-08,  1.6171887E-08,
     $  1.3413245E-08,  1.0631554E-08,  9.0590299E-09,  7.4748183E-09,
     $  6.5369535E-09,  5.5932822E-09,  5.0231776E-09,  4.4502798E-09,
     $  4.0987424E-09,  3.7458969E-09,  3.5280209E-09,  3.3095468E-09,
     $  3.1712338E-09,  3.0326432E-09,  2.9426375E-09,  2.8524963E-09/)
      real,parameter,dimension(kz0):: tem0=(/
     $  2.8815000E+02,  2.8165000E+02,  2.7515000E+02,  2.6865000E+02,
     $  2.6215000E+02,  2.5565000E+02,  2.4915000E+02,  2.4265000E+02,
     $  2.3615000E+02,  2.2965000E+02,  2.2315000E+02,  2.1733000E+02,
     $  2.1665000E+02,  2.1665000E+02,  2.1665000E+02,  2.1665000E+02,
     $  2.1665000E+02,  2.1665000E+02,  2.1666000E+02,  2.1670000E+02,
     $  2.1695000E+02,  2.1763000E+02,  2.1856000E+02,  2.1955000E+02,
     $  2.2055000E+02,  2.2155000E+02,  2.2255000E+02,  2.2355000E+02,
     $  2.2455000E+02,  2.2555000E+02,  2.2656000E+02,  2.2764000E+02,
     $  2.2907000E+02,  2.3126000E+02,  2.3389000E+02,  2.3663000E+02,
     $  2.3938000E+02,  2.4213000E+02,  2.4488000E+02,  2.4763000E+02,
     $  2.5038000E+02,  2.5313000E+02,  2.5588000E+02,  2.5863000E+02,
     $  2.6137000E+02,  2.6411000E+02,  2.6679000E+02,  2.6911000E+02,
     $  2.7036000E+02,  2.7066000E+02,  2.7064000E+02,  2.7021000E+02,
     $  2.6871000E+02,  2.6627000E+02,  2.6357000E+02,  2.6082000E+02,
     $  2.5808000E+02,  2.5532000E+02,  2.5257000E+02,  2.4982000E+02,
     $  2.4707000E+02,  2.4432000E+02,  2.4157000E+02,  2.3882000E+02,
     $  2.3607000E+02,  2.3332000E+02,  2.3057000E+02,  2.2782000E+02,
     $  2.2507000E+02,  2.2233000E+02,  2.1959000E+02,  2.1691000E+02,
     $  2.1448000E+02,  2.1237000E+02,  2.1037000E+02,  2.0840000E+02,
     $  2.0643000E+02,  2.0446000E+02,  2.0249000E+02,  2.0052000E+02,
     $  1.9855000E+02,  1.9658000E+02,  1.9461000E+02,  1.9264000E+02,
     $  1.9070000E+02,  1.8892000E+02,  1.8775000E+02,  1.8739000E+02,
     $  1.8733000E+02,  1.8732000E+02,  1.8732000E+02,  1.8732000E+02,
     $  1.8742000E+02,  1.8771000E+02,  1.8819000E+02,  1.8887000E+02,
     $  1.8976000E+02,  1.9086000E+02,  1.9218000E+02,  1.9373000E+02,
     $  1.9553000E+02,  1.9761000E+02,  1.9998000E+02,  2.0268000E+02,
     $  2.0576000E+02,  2.0929000E+02,  2.1335000E+02,  2.1808000E+02,
     $  2.2374000E+02,  2.3078000E+02,  2.4045000E+02,  2.5245000E+02,
     $  2.6445000E+02,  2.7645000E+02,  2.8845000E+02,  3.0045000E+02/)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c Note, in the call from uh2oci the model grid is inversed (going down)
      do l=1,lmy
         xmy(l)=alog(1.e5/pmy(l))
      enddo
c     print*,'www1',pmy

c Determine the first model layer from the top below or about 100 km
      lh2o=lmy+1
      do l=1,lmy
         if(xmy(l) <= xtop) then
            lh2o=l
            exit
         endif
      enddo

      if(lh2o > lmy) then
         write(6,*) '***Warning: Model bottom above H2O cooling region'
         write(6,'(2f8.2)') xmy(lmy),xtop
         write(6,*) '***Cooling rates will be set to 0'
         return
      endif

      wvmy(:)=0.
      tmy(:)=0.
      call splin1(x0(kz0:1:-1),mmr0(kz0:1:-1),
     $     xmy(lh2o),wvmy(lh2o),kz0,lmy+1-lh2o)
      call splin1(x0(kz0:1:-1),tem0(kz0:1:-1),
     $     xmy(lh2o),tmy(lh2o),kz0,lmy+1-lh2o)

c For linear extrapolation of cooling rates upward of xmy(lh2o), count
c how many model layers are between xmy(lh2o) and xmu(lh2o)+1 and 
c calculate extrapolation coefficients
      if(lh2o > 1) then
         llin=0
         do l=1,lh2o-1
            if(xmy(l) < xmy(lh2o)+1.) then
               llin=llin+1
            endif
         enddo
      endif
      coeff(:)=0.
      if(llin > 0) then
         do l=1,llin
            coeff(l)=xmy(lh2o-l)-xmy(lh2o)
         enddo
      endif

      END subroutine wvrefm

c***********************************************************************

      real FUNCTION ZXKTP(T,K,I,N,kus,km,mm4,nq,t77,xkus)
      
      implicit none
      integer,intent(in):: i,k,n,kus,km,mm4,nq
      real,intent(in):: t,t77(mm4),xkus(kus,nq,km,mm4)

      integer:: jj,jj1,jj3,jja,jjb
      real:: y,dy,xt,WR1(3),WR2(3)

      XT=T
      IF(T.LE.T77(1)) XT=T77(1)
      IF(T.GT.T77(MM4)) XT=T77(MM4)
      JJ1=INT((XT-T77(1))/(T77(2)-T77(1)))+1
      IF(JJ1.LE.1) JJ1=2
      IF(JJ1.GE.MM4) JJ1=MM4-1
      JJA=JJ1-1
      JJB=JJ1+1

      DO 10 JJ=JJA,JJB
      JJ3=JJ-JJA+1
      WR1(JJ3)=T77(JJ)
  10  WR2(JJ3)=ALOG(XKUS(K,N,I,JJ))

      CALL POLINT(WR1,WR2,3,XT,Y,DY)
      ZXKTP=EXP(Y)

      END function zxktp

c***********************************************************************
