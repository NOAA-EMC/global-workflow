!
!
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  get_sfm_1d_gnl                              
!
!   PRGMMR:                  ORG:                DATE: 
!
! ABSTRACT: 
!  This subroutine calculate liquid water content for convection cloud
!    This subroutine is from ARPS cloud analysis package
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!
!   output argument list:
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

!##################################################################
!##################################################################
!######                                                      ######
!######               SUBROUTINE GET_SFM_1D                  ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE get_sfm_1d_gnl (nz,zcb,zctop,zs_1d,p_mb_1d,t_1d,ql,qi,cldt,      &
                       l_prt)
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!c-----------------------------------------------------------------
!c
!c    This is the streamlined version of the Smith-Feddes
!c    and Temperature Adjusted LWC calculation methodologies
!c    produced at Purdue University under sponsorship
!c    by the FAA Technical Center.
!c
!c    Currently, this subroutine will only use the Smith-
!c    Feddes and will only do so as if there are solely
!c    stratiform clouds present, however, it is very easy
!c    to switch so that only the Temperature Adjusted
!c    method is used.
!c
!c    Dilution by glaciation is also included, it is a
!c    linear function of in cloud temperature going from
!c    all liquid water at -10 C to all ice at -30 C
!c    as such the amount of ice is also calculated
!
!-----------------------------------------------------------------------
!
!  AUTHOR:  Jian Zhang
!  05/96    Based on the LAPS cloud analysis code of 07/1995
!
!  MODIFICATION HISTORY:
!
!  05/16/96 (Jian Zhang)
!           Modified for ADAS format. Added full documentation.
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE
!
!
!-----------------------------------------------------------------------
!
!  INPUT:
  INTEGER :: nz             ! number of model vertical levels
  REAL :: zs_1d(nz)         ! physical height (m) at each scalar level
  REAL :: p_mb_1d(nz)       ! pressure (mb) at each level
  REAL :: t_1d(nz)          ! temperature (K) at each level

  REAL :: zcb               ! cloud base height (m)
  REAL :: zctop             ! cloud top height (m)
!
!  OUTPUT:
  REAL :: ql(nz)            ! liquid water content (g/kg)
  REAL :: qi(nz)            ! ice water content (g/kg)
  REAL :: cldt(nz)
!
!  LOCAL:
  REAL :: calw(200)
  REAL :: cali(200)
  REAL :: catk(200)
  REAL :: entr(200)
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  REAL :: dz,rv,rair,grav,cp,rlvo,rlso,dlvdt,eso
  REAL :: c,a1,b1,c1,a2,b2,c2
  REAL :: delz,delt,cldbtm,cldbp,cldtpt,tbar
  REAL :: arg,fraclw,tlwc
  REAL :: temp,press,zbase,alw,zht,ht,y
  REAL :: rl,es,qvs1,p,des,dtz,es2,qvs2
  INTEGER :: i,j,k,nlevel,nlm1,ip,kctop,kctop1,kcb,kcb1
  REAL :: dtdz,dttdz,zcloud,entc,tmpk
  LOGICAL :: l_prt
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!
!-----------------------------------------------------------------------
!
!  Initialize 1d liquid water and ice arrays (for 100m layers)
!
!-----------------------------------------------------------------------
!
  DO i=1,200
    calw(i)=0.0
    cali(i)=0.0
  END DO
!
!-----------------------------------------------------------------------
!
!  Preset some constants and coefficients.
!
!-----------------------------------------------------------------------
!
  dz=100.0                ! m
  rv=461.5                ! J/deg/kg
  rair=287.04             ! J/deg/kg
  grav=9.81               ! m/s2
  cp=1004.                ! J/deg/kg
  rlvo=2.5003E+6          ! J/kg
  rlso=2.8339E+6          ! J/kg
  dlvdt=-2.3693E+3        ! J/kg/K
  eso=610.78              ! pa
  c=0.01
  a1=8.4897
  b1=-13.2191
  c1=4.7295
  a2=10.357
  b2=-28.2416
  c2=8.8846
!
!-----------------------------------------------------------------------
!
!  Calculate indices of cloud top and base
!
!-----------------------------------------------------------------------
!
  DO k=1,nz-1
    IF(zs_1d(k) < zcb .AND. zs_1d(k+1) > zcb) THEN
      kcb=k
      kcb1=kcb+1
    END IF
    IF(zs_1d(k) < zctop .AND. zs_1d(k+1) > zctop) THEN
      kctop=k
      kctop1=kctop+1
    END IF
  END DO
!
!-----------------------------------------------------------------------
!
!  Obtain cloud base and top conditions
!
!-----------------------------------------------------------------------
!
  delz   = zs_1d(kcb+1)-zs_1d(kcb)
  delt   = t_1d(kcb+1)-t_1d(kcb)
  cldbtm = delt*(zcb-zs_1d(kcb))/delz+t_1d(kcb)
  tbar   = (cldbtm+t_1d(kcb))/2.
  arg    = -grav*(zcb-zs_1d(kcb))/rair/tbar
  cldbp  = p_mb_1d(kcb)*EXP(arg)
  delz   = zs_1d(kctop+1)-zs_1d(kctop)
  delt   = t_1d(kctop+1)-t_1d(kctop)
  cldtpt = delt*(zctop-zs_1d(kctop))/delz+t_1d(kctop)
!
!-----------------------------------------------------------------------
!
!  Calculate cloud lwc profile for cloud base/top pair
!
!-----------------------------------------------------------------------
!
  temp   = cldbtm
  press  = cldbp*100.0
  zbase  = zcb
  nlevel = ((zctop-zcb)/100.0)+1
  IF(nlevel <= 0) nlevel=1
  alw    = 0.0
  calw(1)= 0.0
  cali(1)= 0.0
  catk(1)= temp
  entr(1)= 1.0
  nlm1   = nlevel-1
  IF(nlm1 < 1) nlm1=1
  zht    = zbase

  DO j=1,nlm1
    rl   = rlvo+(273.15-temp)*dlvdt
    arg  = rl*(temp-273.15)/273.15/temp/rv
    es   = eso*EXP(arg)
    qvs1 = 0.622*es/(press-es)
!        rho1 = press/(rair*temp)
    arg  = -grav*dz/rair/temp
    p    = press*EXP(arg)

    IF(l_prt) THEN
      WRITE(6,605) j,zht,temp,press,1000.0*qvs1,es,rl
      605       FORMAT('get_sfm_1d_gnl:',1X,i2,' ht=',f8.0,' T=',f6.1,' P=',f9.1,' qvs=', &
                       f7.3,' es=',f6.1,' Lv=',e10.3)
    END IF
!
!-----------------------------------------------------------------------
!
!  Calculate saturated adiabatic lapse rate
!
!-----------------------------------------------------------------------
!
    des   = es*rl/temp/temp/rv
    dtz   = -grav*((1.0+0.621*es*rl/(press*rair*temp))/                 &
                 (cp+0.621*rl*des/press))
    zht   = zht+dz
    press = p
    temp  = temp+dtz*dz
    rl    = rlvo+(273.15-temp)*dlvdt
    arg   = rl*(temp-273.15)/273.15/temp/rv
    es2   = eso*EXP(arg)
    qvs2  = 0.622*es2/(press-es2)

    alw   = alw+(qvs1-qvs2)                   ! kg/kg
    calw(j+1) = alw

    IF (l_prt) THEN
      WRITE(6,9015) j,1000.0*calw(j+1),zht
      9015      FORMAT('get_sfm_1d_gnl',1X,'j=',i3,'  adiab.lwc =',f7.3,'  alt =',f8.0)
    END IF
!
!-----------------------------------------------------------------------
!
!  Reduction of lwc by entrainment
!
!-----------------------------------------------------------------------
!
    ht = (zht-zbase)*.001
!
!c   ------------------------------------------------------------------
!c
!c                          skatskii's curve(convective)
!c
!c   ------------------------------------------------------------------
!c      if(ht.lt.0.3) then
!c        y    = -1.667*(ht-0.6)
!c      elseif(ht.lt.1.0) then
!c        arg1 = b1*b1-4.0*a1*(c1-ht)
!c        y    = (-b1-sqrt(arg1))/(2.0*a1)
!c      elseif(ht.lt.2.9) then
!c        arg2 = b2*b2-4.0*a2*(c2-ht)
!c        y    = (-b2-sqrt(arg2))/(2.0*a2)
!c      else
!c        y    = 0.26
!c      endif
!c
!c   ------------------------------------------------------------------
!c
!c                         warner's curve(stratiform)
!c
!c   ------------------------------------------------------------------
    IF(ht < 0.032) THEN
      y = -11.0*ht+1.0           ! y(ht=0.032) = 0.648
    ELSE IF(ht <= 0.177) THEN
      y = -1.4*ht+0.6915         ! y(ht=0.177) = 0.4437
    ELSE IF(ht <= 0.726) THEN
      y = -0.356*ht+0.505        ! y(ht=0.726) = 0.2445
    ELSE IF(ht <= 1.5) THEN
      y = -0.0608*ht+0.2912      ! y(ht=1.5) = 0.2
    ELSE
      y = 0.20
    END IF
!
!-----------------------------------------------------------------------
!
!  Calculate reduced lwc by entrainment and dilution
!
!  Note at -5 C and warmer, all liquid.   ! changed from -10 KB
!       at -25 C and colder, all ice      ! changed from -30 KB
!       Linear ramp between.
!
!-----------------------------------------------------------------------
!
    IF(temp < 268.15) THEN
      IF(temp > 248.15) THEN
        fraclw=0.05*(temp-248.15)
      ELSE
        fraclw=0.0
      END IF
    ELSE
      fraclw=1.0
    END IF

    tlwc=1000.*y*calw(j+1)                ! g/kg
    calw(j+1)=tlwc*fraclw
    cali(j+1)=tlwc*(1.-fraclw)
    catk(j+1)=temp
    entr(j+1)=y

  END DO
!
!-----------------------------------------------------------------------
!
!  Alternative calculation procedure using the observed or
!  inferred in cloud temperature profile
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!  Obtain profile of LWCs at the given grid point
!
!-----------------------------------------------------------------------
!

  DO ip=2,nz-1
    IF(zs_1d(ip) <= zcb .OR. zs_1d(ip) > zctop) THEN
      ql(ip)=0.0
      qi(ip)=0.0
      cldt(ip)=t_1d(ip)
    ELSE
      DO j=2,nlevel
        zcloud = zcb+(j-1)*dz
        IF(zcloud >= zs_1d(ip)) THEN
          ql(ip) = (zs_1d(ip)-zcloud+100.)*(calw(j)-calw(j-1))*0.01     &
                       +calw(j-1)
          qi(ip) = (zs_1d(ip)-zcloud+100.)*(cali(j)-cali(j-1))*0.01     &
                       +cali(j-1)
          tmpk = (zs_1d(ip)-zcloud+100.)*(catk(j)-catk(j-1))*0.01     &
                       +catk(j-1)
          entc = (zs_1d(ip)-zcloud+100.)*(entr(j)-entr(j-1))*0.01     &
                       +entr(j-1)
          cldt(ip) = (1.-entc)*t_1d(ip) + entc*tmpk

          EXIT
        END IF
      END DO
    END IF
  END DO
!
!-----------------------------------------------------------------------
!
!  Write out file of lwc comparisons
!
!-----------------------------------------------------------------------
!
  RETURN
END SUBROUTINE get_sfm_1d_gnl
