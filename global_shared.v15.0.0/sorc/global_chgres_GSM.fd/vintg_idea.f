      SUBROUTINE VINTG_IDEA(IMO,LATCH,KM2,NT,P2,RLAT,JMO,J1,J2,IDAY,
     &U2,V2,T2,Q2)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C ABSTRACT: MAKE UPPER-AIR FIELDS MORE REAL
C   WIND, TEMPERATURE, HUMIDITY AND OTHER TRACER
C
C USAGE:    CALL VINTG_IDEA(IMO,LATCH,KM2,NT,P2,RLAT,JMO,J1,J2,IDAY,
C    &U2,V2,T2,Q2)
C   INPUT ARGUMENT LIST:
C     IMO          INTEGER NUMBER OF LOGITUDE
C     LATCH        INTEGER MAX NUMBER OF LAT TO PROCCESS
C     KM2          INTEGER NUMBER OF OUTPUT LEVELS
C     NT           INTEGER NUMBER OF TRACERS INPUT
C     JMO          INTEGER NUMDER OF LATITUDE
C     J1           INTEGER FIRST LATITUDE INDEX,(NORTH TO SOUTH)
C     J2           INTEGER LAST LATITUDE INDEX,
C     IDAY         INTEGER (4) HOUR MONTH DAY YEAR
C     RLAT         REAL (JMO) LATITUDE IN DEGREE
C     P2           REAL (IMO,2*LATCH,KM2) PRESSURES
C                  ORDERED FROM BOTTOM TO TOP OF ATMOSPHERE
C   OUTPUT AND INPUT  ARGUMENT LIST:
C     U2           REAL (IMO,2*LATCH,KM2)  ZONAL WIND
C     V2           REAL (IMO,2*LATCH,KM2)  MERIDIONAL WIND
C     T2           REAL (IMO,2*LATCH,KM2)  TEMPERATURE (K)
C     Q2           REAL (IMO,2*LATCH,KM2,NT+2)  TRACERS (HUMIDITY FIRST)
C
C SUBPROGRAMS CALLED:
C   GETTEMP        Calculate temperature
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      IMPLICIT NONE
      INTEGER, INTENT(IN)    :: imo,latch,km2,nt,jmo,j1,j2,iday(4)
      REAL   , INTENT(IN)    :: rlat(jmo),p2(imo,2*latch,km2)
      REAL   , INTENT(INOUT) :: u2(imo,2*latch,km2),
     &                          v2(imo,2*latch,km2),
     &                          t2(imo,2*latch,km2),
     &                          q2(imo,2*latch,km2,nt)
      REAL, parameter:: top=64.25
      REAL, parameter:: amo=15.9994  ! molecular wght of O (g/mol)
      REAL, parameter:: amo2=31.999  ! molecular wght of O2 (g/mol)
      REAL, parameter:: amn2=28.013  ! molecular wght of N2 (g/mol)
      REAL  temps(km2),tempn(km2),zmprn(km2),zmprs(km2),wfun(10)
      REAL  n_os(km2),n_on(km2),n_o2s(km2),n_o2n(km2)
      REAL  n_n2s(km2),n_n2n(km2)
      REAL  sumn,sums,rlats,coe,hold
      INTEGER i, j, k, kref, jjn, jjs, ciday, ik,idat(8),jdow,jday
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C get weight function at joints
      do i=1,10
      wfun(i)=(i-1)/9.
      enddo
C get calendar day
c     call getcday(iday,ciday)
c     ciday=iday(3)+30*(iday(2)-1)
      idat(1)=iday(4)
      idat(2)=iday(2)
      idat(3)=iday(3)
      idat(5)=iday(1)
      call w3doxdat(idat,jdow,ciday,jday)
c     print*,idat
      print*,iday
      print*,'ciday',ciday
C
      do i=1,imo
      do j=1,16
      do k=1,km2
      if(p2(i,j,k).le.0.) print*,i,j,k
      enddo
      enddo
      enddo
C  For EACH LATITUDE couple
      do j=j1,j2 
C second index of data array for latitude couple
      jjn=2*(j-j1)+1
      jjs=2*(j-j1)+2
C Get zonal meam pressure
        do k=1,km2
        sumn=0.
        sums=0.
          do i=1,imo
          sumn=sumn+p2(i,jjn,k)
          sums=sums+p2(i,jjs,k)
          enddo
        zmprn(k)=sumn/float(imo)*.01
        zmprs(k)=sums/float(imo)*.01
        enddo
C GET TEMP PROFILE
        call gettemp(ciday,1,rlat(j),1,zmprn,km2,tempn,n_on,n_o2n,
     &   n_n2n)
        rlats=-1.*rlat(j)
        call gettemp(ciday,1,rlats,1,zmprs,km2,temps,n_os,n_o2s,
     &   n_n2s)
C JIONT WITH EACH LONGITUDE north
        do i=1,imo
           do k=1,km2
             hold = 1./(n_on(k)*amo+amo2*n_o2n(k)+amn2*n_n2n(k))
!            q2(i,jjn,k,nt+1)=(amo*n_on(k))*hold
!            q2(i,jjn,k,nt+2)=(amo2*n_o2n(k))*hold
             q2(i,jjn,k,nt-1) = (amo*n_on(k))   * hold
             q2(i,jjn,k,nt)   = (amo2*n_o2n(k)) * hold
           enddo
C find joint location (orig data top)
           do k=1,km2
             if(p2(i,jjn,k).le.top) then
               kref=k
               go to 10
             endif
           enddo
   10 continue
c temperature joint
           do k=kref,km2
             t2(i,jjn,k) = tempn(k)
           enddo
           do k=kref-10,kref-1
             t2(i,jjn,k) =    wfun(k-kref+11)  * tempn(k)+
     &                    (1.-wfun(k-kref+11)) * t2(i,jjn,k)
           enddo
c others : u v q
           do k=kref,km2
             coe = p2(i,jjn,k)/p2(i,jjn,kref)
c            coe = log(p2(i,jjn,kref))/log(p2(i,jjn,k))
             u2(i,jjn,k) = coe*u2(i,jjn,kref)
             v2(i,jjn,k) = coe*v2(i,jjn,kref)
           enddo
        enddo
C JIONT WITH EACH LONGITUDE south
        do i=1,imo
           do k=1,km2
             hold = 1./(n_os(k)*amo+amo2*n_o2s(k)+amn2*n_n2s(k))
             q2(i,jjs,k,nt-1) = (amo*n_os(k))   * hold
             q2(i,jjs,k,nt)   = (amo2*n_o2s(k)) * hold
           enddo
C find joint location (orig data top)
           do k=1,km2
             if(p2(i,jjs,k).le.top) then
               kref=k
               go to 11
             endif
           enddo
   11      continue
c temperature joint
           do k=kref,km2
             t2(i,jjs,k) = temps(k)
           enddo
           do k=kref-10,kref-1
             t2(i,jjs,k) = wfun(k-kref+11)  * temps(k)+
     &                (1.- wfun(k-kref+11)) * t2(i,jjs,k)
           enddo
c others : u v q ...........
           do k=kref,km2
             coe = p2(i,jjs,k)/p2(i,jjs,kref)
c            coe = log(p2(i,jjs,kref))/log(p2(i,jjs,k))
             u2(i,jjs,k) = coe*u2(i,jjs,kref)
             v2(i,jjs,k) = coe*v2(i,jjs,kref)
           enddo
        enddo !logitude
      enddo
c     print*,'www1'
c     print'(12f6.1)',(q2(1,i,km2,4),i=1,2*latch)
      end subroutine vintg_idea
!-----------------------------------------------------------------------
      subroutine gettemp(iday,nday,xlat,nlat,pr,np,temp,n_o,n_o2,
     & n_n2)
!  calculate temperature at each grid point useing nrlmsise00_sub
      implicit none
      integer, intent(in) :: nday                 !number of days 
      integer, intent(in) :: nlat                 !number of latitudes
      integer, intent(in) :: np                   !number of pressure levels
      real,    intent(in) :: pr(np)               ! pressure in mb
      real,    intent(in) :: xlat(nlat)           !latitude in degree
      integer, intent(in) :: iday(nday)           !calender day
      real,   intent(out) :: temp(np,nlat,nday)   ! temperature
      real,   intent(out) :: n_o(np,nlat,nday)   ! number density of o (/cm3)
      real,   intent(out) :: n_o2(np,nlat,nday)   ! number density of o2 (/cm3)
      real,   intent(out) :: n_n2(np,nlat,nday)   ! number density of N2 (/cm3)
      real                :: alt(np,nlat,nday)    ! altitude in km
      real                :: D(9),T(2),SW(25),AP(7),ut,xlong,xlst,f107,
     &                       f107a
      integer             :: k,il,ip
! set magnetic index average value
      DATA AP/7*9./
! set swich 7,8,10,14  zero to avoid diurnal changes in output temperature.
! swich #7 is for diurnal,#8 is for semidiurnal,# 10 is for all UT/longitude 
! effect,#14 is for terdiurnal 
      data sw/1.,1.,1.,1.,1.,1.,0.,0.,1.,0.,1.,1.,1.,0.,1.,1.,1.,1.,1.,
     &1.,1.,1.,1.,1.,1./
! set 10.7cm flux be average value
      f107=150.
      f107a=150.
! turn on swich
      CALL TSELEC(SW)
! set longitude, UT, local time , It should not make difference to output
      ut=0.
      xlong=0.
      xlst=ut/3600.+xlong/15.
! calculate temperature for each lat,pres level,day
      do k=1,nday
      do il=1,nlat
      do ip=1,np
      CALL GHP7(IDAY(k),UT,ALT(ip,il,k),XLAT(il),XLONG,XLST,F107A,F107,
     &AP,D,T,pr(ip))
      temp(ip,il,k)=t(2)
      n_o(ip,il,k)=D(2)
      n_o2(ip,il,k)=D(4)
      n_n2(ip,il,k)=D(3)
      enddo
      enddo
      enddo
      end subroutine gettemp
      subroutine getcday(iday,cday)
c get calendar day
      implicit none
      integer, intent(in) :: iday(4) !GFS time h,m,d,y
      integer, intent(out):: cday !calender day
      integer day(11),i
      data day/31,28,31,30,31,30,31,31,30,31,30/
      cday=iday(3)
      if(iday(2).gt.1) then
        do i=1,iday(2)-1
          cday=cday+day(i)
        enddo
      endif
      if(mod(iday(4),4).eq.0.and.iday(2).ge.3) cday=cday+1
      return
      end
