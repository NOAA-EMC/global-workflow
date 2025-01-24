!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ARPS_cldLib 
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 
!
! ABSTRACT: 
!  This file include a collection of subroutines that are related to 
!              cloud analysis from ARPS cloud analysis 
!
! PROGRAM HISTORY LOG:
!    2009-01-02  Hu  Add NCO document block
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
!
!
!##################################################################
!##################################################################
!######                                                      ######
!######               SUBROUTINE GET_STABILITY               ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE get_stability (nz,t_1d,zs_1d,p_mb_1d,kbtm,ktop               &
           ,dte_dz_1d)
!
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!  This routine returns stability at a given level given
!  1D temperature and pressure array inputs
!
!-----------------------------------------------------------------------
!
!  AUTHOR:  Jian Zhang
!  05/96    Based on LAPS cloud analysis code of 07/95
!
!  MODIFICATION HISTORY:
!
!  05/11/96  (J. Zhang)
!            Modified for ADAS format. Added full documentation.
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_single,i_kind,r_kind
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  integer(i_kind),INTENT(IN) :: nz         ! number of vertical model levels
  REAL(r_single) ,INTENT(IN) :: t_1d(nz)   ! temperature (degree Kelvin) profile
  REAL(r_single) ,INTENT(IN) :: zs_1d(nz)  ! heights (m MSL) of each level
  REAL(r_single) ,INTENT(IN) :: p_mb_1d(nz)! pressure (mb) at each level
  INTEGER(i_kind),INTENT(IN) :: kbtm,ktop  ! indices of the bottom and top cloud layer
!
!  OUTPUT:
  REAL(r_single) ,INTENT(out):: dte_dz_1d(nz) ! stability array
!
!  LOCAL:
  REAL(r_single) :: thetae_1d(nz) ! (equivalent) potential temperature.
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  INTEGER(i_kind) :: k,km1,kp1,klow,khigh
  REAL(r_single) :: os_fast
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
!  Calculate Stability
!
!-----------------------------------------------------------------------
!
  klow  = MAX(kbtm-1,1)
  khigh = MIN(ktop+1,nz-1)

  DO k = klow,khigh
    thetae_1d(k)  = os_fast(t_1d(k), p_mb_1d(k))
  END DO ! k
  
  dte_dz_1d=0._r_kind

  DO k = kbtm,ktop
    km1  = MAX(k-1,1)
    kp1  = MIN(k+1,nz-1)

    IF( (zs_1d(kp1) - zs_1d(km1)) <= 0._r_kind) THEN
      write(6,*) 'GNRLCLD_mpi, get_stability: Error in get_stability '
      write(6,*) 'GNRLCLD_mpi, get_stability: k,kp1,km1 = ',k,kp1,km1
      write(6,*) 'GNRLCLD_mpi, get_stability: zs_1d(kp1),zs_1d(km1)= ',zs_1d(kp1),zs_1d(km1),        &
                 (zs_1d(kp1) - zs_1d(km1))
      call STOP2(114)
    ELSE 
      dte_dz_1d(k) = (thetae_1d(kp1) - thetae_1d(km1))                  &
                           / (zs_1d(kp1) - zs_1d(km1))
    END IF
  END DO ! k

  RETURN
END SUBROUTINE get_stability


!
!##################################################################
!##################################################################
!######                                                      ######
!######            FUNCTION OS_FAST                          ######
!######                                                      ######
!##################################################################
!##################################################################
!

  FUNCTION os_fast(tk,p)
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  THIS FUNCTION RETURNS THE EQUIVALENT POTENTIAL TEMPERATURE OS
!  (K) FOR A PARCEL OF AIR SATURATED AT TEMPERATURE T (K)
!  AND PRESSURE P (MILLIBARS).
!
!
!-----------------------------------------------------------------------
!
!  AUTHOR: (BAKER,SCHLATTER)
!  05/17/1982
!
!
!  MODIFICATION HISTORY:
!  05/11/96 (Jian Zhang)
!  Modified for ADAS grid. Add document stuff.
!
!-----------------------------------------------------------------------
!
!  Variables declaration
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_single,i_kind,r_kind
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  REAL(r_single) ,INTENT(IN) :: tk     ! temperature in kelvin
  REAL(r_single) ,INTENT(IN) :: p      ! pressure in mb
!
!  OUTPUT:
  REAL(r_single) :: os_fast  ! equivalent potential temperature
!
!  LOCAL:
  REAL(r_kind) :: b  ! empirical const. approx.= latent heat of
                     ! vaporiz'n for water devided by the specific
                     ! heat at const. pressure for dry air.
  DATA b/2.6518986_r_kind/

  REAL(r_kind) :: tc,x,w
  REAL(r_kind) :: eslo
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  tc = tk - 273.15_r_kind
!
!-----------------------------------------------------------------------
!
!  From W routine
!
!-----------------------------------------------------------------------
!
  x= eslo(tc)
  w= 622._r_kind*x/(p-x)

  os_fast= tk*((1000._r_kind/p)**.286_r_kind)*(EXP(b*w/tk))

  RETURN
  END FUNCTION os_fast



!
!
!##################################################################
!##################################################################
!######                                                      ######
!######               SUBROUTINE GET_CLOUDTYPE               ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE get_cloudtype(temp_k,dte_dz,cbase_m,ctop_m                   &
           ,itype,c2_type)
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!  This routine returns cloud type at a given point given
!  temperature and stability inputs
!
!-----------------------------------------------------------------------
!
!  AUTHOR:  Jian Zhang
!  05/96    Based on the LAPS cloud analysis code of 05/1995
!
!  MODIFICATION HISTORY:
!
!  05/11/96  (J. Zhang)
!            Modified for ADAS format. Added full documentation.
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_single,i_kind,r_kind
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  REAL(r_single),INTENT(IN)  :: temp_k       ! temperature
  REAL(r_single),INTENT(IN)  :: dte_dz       ! stability factor
  REAL(r_single),INTENT(IN)  :: cbase_m      ! height at cloud base level
  REAL(r_single),INTENT(IN)  :: ctop_m       ! height at cloud top level
!
!  OUTPUT:
  INTEGER(i_kind),INTENT(out):: itype        ! cloud type index
  CHARACTER (LEN=2) :: c2_type
!
!  LOCAL:
  CHARACTER (LEN=2) :: c2_cldtyps(10)

  DATA c2_cldtyps /'St','Sc','Cu','Ns','Ac'                             &
                  ,'As','Cs','Ci','Cc','Cb'/
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  REAL(r_kind) :: depth_m,temp_c
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  temp_c = temp_k - 273.15_r_kind
  depth_m = ctop_m - cbase_m
!
!-----------------------------------------------------------------------
!
!  Go from Stability to Cloud Type
!
!-----------------------------------------------------------------------
!
  IF ( temp_c >= -10._r_kind) THEN
    IF (dte_dz >= +.001_r_kind) THEN
      itype = 1      ! St
    ELSE IF (dte_dz < +.001_r_kind .AND. dte_dz >= -.001_r_kind)  THEN
      itype = 2      ! Sc
    ELSE IF (dte_dz < -.001_r_kind .AND. dte_dz >= -.005_r_kind)  THEN
      itype = 3      ! Cu
    ELSE ! dte_dz .lt. -.005
      IF(depth_m > 5000) THEN
        itype = 10   ! Cb
      ELSE  ! depth < 5km
        itype = 3    ! Cu
      END IF
    END IF

  ELSE IF (temp_c < -10._r_kind .AND. temp_c >= -20._r_kind) THEN

    IF (dte_dz < 0._r_kind) THEN
      IF(depth_m > 5000) THEN
        itype = 10   ! Cb
      ELSE
        itype = 5    ! Ac
      END IF
    ELSE
      itype = 6      ! As
    END IF

  ELSE               ! temp_c.lt.-20.

    IF (dte_dz >= +.0005_r_kind) THEN
      itype = 7      ! Cs
    ELSE IF (dte_dz < +.0005_r_kind .AND. dte_dz >= -.0005_r_kind) THEN
      itype = 8      ! Ci
    ELSE             ! dte_dz .lt. -.0005
      itype = 9      ! Cc
    END IF

    IF(depth_m > 5000 .AND. dte_dz < -.0000_r_kind) THEN
      itype = 10     ! Cb
    END IF

  END IF

  c2_type = c2_cldtyps(itype)

  RETURN
END SUBROUTINE get_cloudtype

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

SUBROUTINE get_sfm_1d (nz,zcb,zctop,zs_1d,p_mb_1d,t_1d,ql,qi,cldt,      &
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
  use kinds, only: r_single,i_kind
  IMPLICIT NONE
!
!
!-----------------------------------------------------------------------
!
!  INPUT:
  INTEGER(i_kind),intent(in) :: nz        ! number of model vertical levels
  REAL(r_single),intent(in)  :: zs_1d(nz) ! physical height (m) at each scalar level
  REAL(r_single),intent(in)  :: p_mb_1d(nz)! pressure (mb) at each level
  REAL(r_single),intent(in)  :: t_1d(nz)   ! temperature (K) at each level

  REAL(r_single),intent(in)  :: zcb        ! cloud base height (m)
  REAL(r_single),intent(in)  :: zctop      ! cloud top height (m)
!
!  OUTPUT:
  REAL(r_single),intent(out) :: ql(nz)     ! liquid water content (g/kg)
  REAL(r_single),intent(out) :: qi(nz)     ! ice water content (g/kg)
  REAL(r_single),intent(out) :: cldt(nz)
!
!  LOCAL:
  REAL(r_single) :: calw(200)
  REAL(r_single) :: cali(200)
  REAL(r_single) :: catk(200)
  REAL(r_single) :: entr(200)
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  REAL(r_single) :: dz,rv,rair,grav,cp,rlvo,rlso,dlvdt,eso
  REAL(r_single) :: c,a1,b1,c1,a2,b2,c2
  REAL(r_single) :: delz,delt,cldbtm,cldbp,cldtpt,tbar
  REAL(r_single) :: arg,fraclw,tlwc
  REAL(r_single) :: temp,press,zbase,alw,zht,ht,y
  REAL(r_single) :: rl,es,qvs1,p,des,dtz,es2,qvs2
  INTEGER(i_kind):: i,j,k,nlevel,nlm1,ip,kctop,kctop1,kcb,kcb1
  REAL(r_single) :: dtdz,dttdz,zcloud,entc,tmpk
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
    calw(i)=0.0_r_single
    cali(i)=0.0_r_single
  END DO
!      if(i_prt.le.20) then
!        i_prt=i_prt+1
!        l_prt=.true.
!      else
!        l_prt=.false.
!      endif
!
!-----------------------------------------------------------------------
!
!  Preset some constants and coefficients.
!
!-----------------------------------------------------------------------
!
  dz=100.0_r_single                ! m
  rv=461.5_r_single                ! J/deg/kg
  rair=287.04_r_single             ! J/deg/kg
  grav=9.81_r_single               ! m/s2
  cp=1004._r_single                ! J/deg/kg
  rlvo=2.5003E+6_r_single          ! J/kg
  rlso=2.8339E+6_r_single          ! J/kg
  dlvdt=-2.3693E+3_r_single        ! J/kg/K
  eso=610.78_r_single              ! pa
  c=0.01_r_single
  a1=8.4897_r_single
  b1=-13.2191_r_single
  c1=4.7295_r_single
  a2=10.357_r_single
  b2=-28.2416_r_single
  c2=8.8846_r_single
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
  tbar   = (cldbtm+t_1d(kcb))/2._r_single
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
  press  = cldbp*100.0_r_single
  zbase  = zcb
  nlevel = ((zctop-zcb)/100.0_r_single)+1
  IF(nlevel <= 0) nlevel=1
  alw    = 0.0_r_single
  calw(1)= 0.0_r_single
  cali(1)= 0.0_r_single
  catk(1)= temp
  entr(1)= 1.0_r_single
  nlm1   = nlevel-1
  IF(nlm1 < 1) nlm1=1
  zht    = zbase

  DO j=1,nlm1
    rl   = rlvo+(273.15_r_single-temp)*dlvdt
    arg  = rl*(temp-273.15_r_single)/273.15_r_single/temp/rv
    es   = eso*EXP(arg)
    qvs1 = 0.622_r_single*es/(press-es)
!        rho1 = press/(rair*temp)
    arg  = -grav*dz/rair/temp
    p    = press*EXP(arg)
!
!-----------------------------------------------------------------------
!
!  Calculate saturated adiabatic lapse rate
!
!-----------------------------------------------------------------------
!
    des   = es*rl/temp/temp/rv
    dtz   = -grav*((1.0_r_single+0.621_r_single*es*rl/(press*rair*temp))/ &
                 (cp+0.621_r_single*rl*des/press))
    zht   = zht+dz
    press = p
    temp  = temp+dtz*dz
    rl    = rlvo+(273.15_r_single-temp)*dlvdt
    arg   = rl*(temp-273.15_r_single)/273.15_r_single/temp/rv
    es2   = eso*EXP(arg)
    qvs2  = 0.622_r_single*es2/(press-es2)

    alw   = alw+(qvs1-qvs2)                   ! kg/kg
    calw(j+1) = alw
!
!-----------------------------------------------------------------------
!
!  Reduction of lwc by entrainment
!
!-----------------------------------------------------------------------
!
    ht = (zht-zbase)*.001_r_single
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
    IF(ht < 0.032_r_single) THEN
      y = -11.0_r_single*ht+1.0_r_single           ! y(ht=0.032) = 0.648
    ELSE IF(ht <= 0.177_r_single) THEN
      y = -1.4_r_single*ht+0.6915_r_single         ! y(ht=0.177) = 0.4437
    ELSE IF(ht <= 0.726_r_single) THEN
      y = -0.356_r_single*ht+0.505_r_single        ! y(ht=0.726) = 0.2445
    ELSE IF(ht <= 1.5_r_single) THEN
      y = -0.0608_r_single*ht+0.2912_r_single      ! y(ht=1.5) = 0.2
    ELSE
      y = 0.20_r_single
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
    IF(temp < 268.15_r_single) THEN
      IF(temp > 248.15_r_single) THEN
        fraclw=0.05*(temp-248.15_r_single)
      ELSE
        fraclw=0.0_r_single
      END IF
    ELSE
      fraclw=1.0_r_single
    END IF

    tlwc=1000._r_single*y*calw(j+1)                ! g/kg
    calw(j+1)=tlwc*fraclw
    cali(j+1)=tlwc*(1._r_single-fraclw)
    catk(j+1)=temp
    entr(j+1)=y

  END DO
!
!-----------------------------------------------------------------------
!
!  Obtain profile of LWCs at the given grid point
!
!-----------------------------------------------------------------------
!
  DO ip=2,nz-1
    IF(zs_1d(ip) <= zcb .OR. zs_1d(ip) > zctop) THEN
      ql(ip)=0.0_r_single
      qi(ip)=0.0_r_single
      cldt(ip)=t_1d(ip)
    ELSE
      DO j=2,nlevel
        zcloud = zcb+(j-1)*dz
        IF(zcloud >= zs_1d(ip)) THEN
          ql(ip) = (zs_1d(ip)-zcloud+100._r_single)*     &
                   (calw(j)-calw(j-1))*0.01_r_single+calw(j-1)     
          qi(ip) = (zs_1d(ip)-zcloud+100._r_single)*     &
                   (cali(j)-cali(j-1))*0.01_r_single+cali(j-1)
          tmpk = (zs_1d(ip)-zcloud+100._r_single)*       &
                 (catk(j)-catk(j-1))*0.01_r_single       &
                       +catk(j-1)
          entc = (zs_1d(ip)-zcloud+100._r_single)*       &
                 (entr(j)-entr(j-1))*0.01_r_single       &
                       +entr(j-1)
          cldt(ip) = (1.-entc)*t_1d(ip) + entc*tmpk

          EXIT
        END IF
      END DO
    END IF
  END DO
!
  RETURN
END SUBROUTINE get_sfm_1d


!
!
!##################################################################
!##################################################################
!######                                                      ######
!######              SUBROUTINE PCP_TYPE_3D                  ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE pcp_type_3d (nx,ny,nz,temp_3d,rh_3d,p_pa_3d                  &
           ,radar_3d,l_mask,cldpcp_type_3d,istatus)

!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!  This routine returns 3D cloud and precipitation type field.
!
!-----------------------------------------------------------------------
!
!  AUTHOR:  Jian Zhang
!  05/1996  Based on the LAPS cloud analysis code developed by
!           Steve Albers.
!
!  This program modifies the most significant 4 bits of the integer
!  array by inserting multiples of 16.
!
!  MODIFICATION HISTORY:
!
!  05/16/96 (J. Zhang)
!           Modified for ADAS format. Added full documentation.
!  01/20/98 (J. Zhang)
!           Fixed a bug that no precip. type was assigned for a
!           grid point at the top of the radar echo with Tw
!           falling in the range of 0 to 1.3 degree C.
!  01/21/98 (J. Zhang)
!           Fixed a bug that does the freezing/refreezing test
!           on ice precipitates.
!  02/17/98 (J. Zhang)
!           Change the hail diagnose procedure.
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_single,i_kind, r_kind
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  INTEGER(i_kind), intent(in)  :: nx,ny,nz                     ! Model grid size
  REAL(r_single),  intent(in)  :: temp_3d(nx,ny,nz)            ! temperature (K)
  REAL(r_single),  intent(in)  :: rh_3d(nx,ny,nz)              ! relative humudity
  REAL(r_single),  intent(in)  :: p_pa_3d(nx,ny,nz)            ! pressure (Pascal)
  REAL(r_kind),    intent(in)  :: radar_3d(nx,ny,nz)           ! radar refl. (dBZ)
!
!  OUTPUT:
  INTEGER(i_kind), intent(out) :: istatus
  INTEGER(i_kind), intent(out) :: cldpcp_type_3d(nx,ny,nz)! cld/precip type
  LOGICAL :: l_mask(nx,ny)             ! "Potential" Precip Type
!
!  LOCAL functions:
  REAL(r_kind) :: wb_melting_thres             ! define melting temp. thresh.
  REAL(r_kind) :: tw                           ! for wet-bulb temp calcl'n
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  INTEGER(i_kind) :: itype                   ! cld/precip type index
  INTEGER(i_kind) :: i,j,k,k_upper
  REAL(r_kind) :: t_c,td_c,t_wb_c,temp_lower_c,temp_upper_c,tbar_c              &
       ,p_mb,thickns,frac_below_zero
  INTEGER(i_kind) :: iprecip_type,iprecip_type_last,iflag_melt                  &
          ,iflag_refreez
  REAL(r_kind) :: zero_c,rlayer_refreez_max,rlayer_refreez
  INTEGER(i_kind) :: n_zr,n_sl,n_last
  REAL(r_kind) :: tmelt_c,x
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!-----------------------------------------------------------------------
!
  return
  istatus=0
  wb_melting_thres = 1.3  ! Units are C
!
!-----------------------------------------------------------------------
!
!  Stuff precip type into cloud type array
!  0 - No Precip
!  1 - Rain
!  2 - Snow
!  3 - Freezing Rain
!  4 - Sleet
!  5 - Hail
!
!-----------------------------------------------------------------------
!
  zero_c = 273.15_r_kind
  rlayer_refreez_max = 0.0_r_kind

  n_zr = 0
  n_sl = 0
  n_last = 0

  DO j = 1,ny-1
    DO i = 1,nx-1

      iflag_melt = 0
      iflag_refreez = 0
      rlayer_refreez = 0.0_r_kind

      iprecip_type_last = 0

      DO k = nz-1,1,-1

        IF(radar_3d(i,j,k) >= 0._r_kind.OR. l_mask(i,j)) THEN
!
!-----------------------------------------------------------------------
!
!  Set refreezing flag
!
!-----------------------------------------------------------------------
!
          t_c  = temp_3d(i,j,k) - zero_c
!   compute dew point depression.
!          td_c = dwpt(t_c,rh_3d(i,j,k))
          x = 1._r_kind-0.01_r_kind*rh_3d(i,j,k)
          td_c =t_c-(14.55_r_kind+0.114_r_kind*t_c)*x+      &
                ((2.5_r_kind+0.007_r_kind*t_c)*x)**3+      &
                (15.9_r_kind+0.117_r_kind*t_c)*x**14      

          p_mb = 0.01_r_kind*p_pa_3d(i,j,k)

          tmelt_c = wb_melting_thres
          t_wb_c = tw(t_c,td_c,p_mb)

          IF(t_wb_c < 0._r_kind) THEN
            IF(iflag_melt == 1) THEN
!
!-----------------------------------------------------------------------
!
!  Integrate below freezing temperature times column thickness
!   - ONLY for portion of layer below freezing
!
!-----------------------------------------------------------------------
!
              temp_lower_c = t_wb_c
              k_upper = MIN(k+1,nz-1)
!
!-----------------------------------------------------------------------
!
!  For simplicity and efficiency, the assumption is here made that
!  the wet bulb depression is constant throughout the level.
!
!-----------------------------------------------------------------------
!
              temp_upper_c = t_wb_c + ( temp_3d(i,j,k_upper)            &
                                          - temp_3d(i,j,k))
              IF(temp_upper_c <= 0._r_kind) THEN
                frac_below_zero = 1.0_r_kind
                tbar_c = 0.5_r_kind * (temp_lower_c + temp_upper_c)

              ELSE ! Layer straddles the freezing level
                frac_below_zero = temp_lower_c                          &
                                        / (temp_lower_c - temp_upper_c)
                tbar_c = 0.5_r_kind * temp_lower_c

              END IF

              thickns = p_pa_3d(i,j,k_upper) - p_pa_3d(i,j,k)
              rlayer_refreez = rlayer_refreez                           &
                   + ABS(tbar_c * thickns * frac_below_zero)

              IF(rlayer_refreez >= 25000._r_kind) THEN
                iflag_refreez = 1
              END IF

              rlayer_refreez_max =                                      &
                                MAX(rlayer_refreez_max,rlayer_refreez)

            END IF ! iflag_melt = 1

          ELSE ! Temp > 0C
            iflag_refreez = 0
            rlayer_refreez = 0.0

          END IF ! T < 0.0c, Temp is below freezing
!
!-----------------------------------------------------------------------
!
!  Set melting flag
!
!-----------------------------------------------------------------------
!
          IF(t_wb_c >= tmelt_c) THEN
            iflag_melt = 1
          END IF

          IF(t_wb_c >= tmelt_c) THEN  ! Melted to Rain
            iprecip_type = 1

          ELSE ! Check if below zero_c (Refrozen Precip or Snow)
            IF(t_wb_c < 0.0_r_kind) THEN
              IF(iflag_melt == 1) THEN
                IF(iprecip_type_last == 1 .OR.iprecip_type_last == 3) THEN
                                   ! test if rain or zr freeze
                  IF(iflag_refreez == 0) THEN ! Freezing Rain
                    n_zr = n_zr + 1
                    IF(n_zr < 30) THEN
!                      WRITE(6,5)i,j,k,t_wb_c,temp_3d(i,j,k)             &
!                          ,rh_3d(i,j,k)
                      5                     FORMAT('zr',3I3,2F8.2,f8.1)
                    END IF
                    iprecip_type = 3

                  ELSE  ! (iflag_refreez = 1)  ! Sleet
                    n_sl = n_sl + 1
                    iprecip_type = 4
                  END IF ! iflag_refreez .eq. 0
                ELSE
                  iprecip_type = iprecip_type_last  ! Unchanged
                  n_last = n_last + 1
                  IF(n_last < 5) THEN
!                    WRITE(6,*)'Unchanged Precip',i,j,k,t_wb_c
                  END IF
                END IF      ! liquid precip. at upper level?

              ELSE    ! iflag_melt =0        ! Snow
                iprecip_type = 2

              END IF   ! iflag_melt = 1?
            ELSE ! t_wb_c >= 0c, and t_wb_c < tmelt_c

              IF (iprecip_type_last == 0) THEN        !   1/20/98
                iprecip_type = 1    ! rain:at echo top and 0<Tw<1.3C
                iflag_melt = 1
              ELSE
                iprecip_type = iprecip_type_last
                n_last = n_last + 1
                IF(n_last < 5) THEN
                  WRITE(6,*)'Unchanged Precip',i,j,k,t_wb_c
                END IF
              END IF

            END IF ! t_wb_c < 0c
          END IF  ! t_wb_c >= tmelt_c

        ELSE ! radar_3d < 0dBZ;  No Radar Echo
          iprecip_type = 0
          iflag_melt = 0
          iflag_refreez = 0
          rlayer_refreez = 0.0_r_kind

        END IF ! radar_3d(i,j,k).ge.0. .or. l_mask(i,j);  Radar Echo?
!
!-----------------------------------------------------------------------
!
!  Insert most sig 4 bits into array
!
!-----------------------------------------------------------------------
!
        itype = cldpcp_type_3d(i,j,k)
        itype = itype - (itype/16)*16     ! Initialize the 4 bits
        itype = itype + iprecip_type * 16 ! Add in the new value
        cldpcp_type_3d(i,j,k) = itype

        iprecip_type_last = iprecip_type

      END DO ! k
    END DO ! j
  END DO ! i

  DO j = 1,ny-1
    DO i = 1,nx-1
      DO k = 1,nz-1
        IF(radar_3d(i,j,k) >= 50._r_kind) THEN
          iprecip_type = 5
          itype = cldpcp_type_3d(i,j,k)
          itype = itype - (itype/16)*16     ! Initialize the 4 bits
          itype = itype + iprecip_type * 16 ! Add in the new value
          cldpcp_type_3d(i,j,k) = itype
        END IF
      END DO ! k
    END DO ! j
  END DO ! i

  istatus=1

  RETURN
END SUBROUTINE pcp_type_3d

!
!
!##################################################################
!##################################################################
!######                                                      ######
!######               SUBROUTINE GET_SLWC1D                  ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE get_slwc1d (nk,cbase_m,ctop_m,kbase,ktop                     &
           ,zs_1d,t_1d,p_pa_1d,iflag_slwc,slwc_1d)

!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!  This routine calls a subroutine "lwc_rep" which calculates
!  the adiabatic liquid water content.
!
!-----------------------------------------------------------------------
!
!  AUTHOR:  Jian Zhang
!  05/96    Based on the LAPS cloud analysis code of 07/1995
!
!  MODIFICATION HISTORY:
!
!  05/13/96  (Jian Zhang)
!            Modified for ADAS format. Added full documentation.
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_single,i_kind,r_kind
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  INTEGER(i_kind),intent(in) :: iflag_slwc     ! indicator for LWC scheme option
  INTEGER(i_kind),intent(in) :: nk             ! number of model vertical levels
  REAL(r_single),intent(in)  :: t_1d(nk)       ! temperature (k) in one model column
  REAL(r_single),intent(in)  :: zs_1d(nk)      ! heights (m) at grd pts in one model column
  REAL(r_single),intent(in)  :: p_pa_1d(nk)    ! pressure (pa) in one model column
  REAL(r_single),intent(in)  :: cbase_m,ctop_m ! heights (m) of cloud base and top levels
  INTEGER(i_kind),intent(in) :: kbase,ktop     ! vertical index of cloud base and top levels
!
!  OUTPUT:
  REAL(r_single),intent(out) :: slwc_1d(nk)     ! estimated adiabatic liquid water
!
!  LOCAL:
  INTEGER(i_kind) :: i_status1,i_status2       ! flag for subroutine calling
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  INTEGER(i_kind):: k
  REAL(r_single) :: p_low,p_high,cbase_pa,cbase_k,ctop_k,frac_k        &
                    ,grid_top_pa,grid_top_k
  REAL(r_single) :: fraction,thickness,dlog_space
  REAL(r_single) :: adiabatic_lwc,adjusted_lwc,adjusted_slwc
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
!  Initialize
!
!-----------------------------------------------------------------------
!
  DO k = 1,nk
    slwc_1d(k) = 0.0_r_single
  END DO

  IF(ctop_m > cbase_m) THEN
!
!-----------------------------------------------------------------------
!
!  Determine Lowest and Highest Grid Points within the cloud
!
!-----------------------------------------------------------------------
!
    IF(ktop >= kbase .AND. kbase >= 2) THEN
!
!-----------------------------------------------------------------------
!
!  Get cloud base pressure and temperature
!
!-----------------------------------------------------------------------
!
      cbase_pa = -999._r_single         ! Default value is off the grid
      DO k = 1,nk-2
        IF(zs_1d(k+1) > cbase_m .AND. zs_1d(k) <= cbase_m) THEN
          thickness = zs_1d(k+1) - zs_1d(k)
          fraction = (cbase_m - zs_1d(k))/thickness
          p_low = p_pa_1d(k)
          p_high = p_pa_1d(k+1)
          dlog_space = LOG(p_high/p_low)
          cbase_pa = p_low * EXP(dlog_space*fraction)
        END IF
      END DO ! k

      frac_k=(cbase_m-zs_1d(kbase-1))/(zs_1d(kbase)-zs_1d(kbase-1))
      IF(frac_k /= fraction)                                            &
          PRINT*,' **GET_SLWC1D**  frac=',fraction,' frac_k=',frac_k

      cbase_k = t_1d(kbase-1)*(1.0_r_single-frac_k) + t_1d(kbase)*frac_k
!
!-----------------------------------------------------------------------
!
!  Get cloud top temperature
!
!-----------------------------------------------------------------------
!
      frac_k = (ctop_m-zs_1d(ktop-1)) / (zs_1d(ktop)-zs_1d(ktop-1))
      ctop_k = t_1d(ktop-1)*(1.0_r_single - frac_k) + t_1d(ktop) * frac_k
!
!-----------------------------------------------------------------------
!
!  Calculate SLWC at each vertical grid point. For each level
!  we use an assumed cloud extending from the actual cloud base
!  to the height of the grid point in question.
!
!-----------------------------------------------------------------------
!
      DO k=kbase,ktop
        grid_top_pa = p_pa_1d(k)
        grid_top_k = t_1d(k)

        CALL slwc_revb(cbase_pa,cbase_k                                 &
                  ,grid_top_pa,grid_top_k,ctop_k                        &
                  ,adiabatic_lwc,adjusted_lwc,adjusted_slwc             &
                  ,i_status1,i_status2)
!
        IF(i_status2 == 1) THEN
          IF(iflag_slwc == 1) THEN
            slwc_1d(k) = adiabatic_lwc
          ELSE IF(iflag_slwc == 2) THEN
            slwc_1d(k) = adjusted_lwc
          ELSE IF(iflag_slwc == 3) THEN
            slwc_1d(k) = adjusted_slwc
          END IF
        ELSE
          WRITE(6,*)' Error Detected in SLWC'
        END IF
      END DO ! k
    END IF ! ktop > kbase & kbase > 2,  thick enough cloud exists
  END IF ! ctop_m > cbase_m,  cloud exists

  RETURN
END SUBROUTINE get_slwc1d

SUBROUTINE slwc_revb(cb_pa,cb_k,gt_pa,gt_k,ct_k,                        &
           adiabatic_lwc,adjusted_lwc,adjusted_slwc,                    &
           i_status1,i_status2)
!
!.......................HISTORY.............................
!
!     WRITTEN: CA. 1982 BY W. A. COOPER IN HP FORTRAN 4
!
!....... CALCULATES TEMPERATURE T AND LIQUID WATER CONTENT FROM
!..      CLOUD BASE PRESSURE P0 AND TEMPERATURE T0, FOR ADIABATIC
!..      ASCENT TO THE PRESSURE P.
!..     ->  INPUT:  CLOUD BASE PRESSURE P0 AND TEMPERATURE T0
!..                 PRESSURE AT OBSERVATION LEVEL P
!..     ->  OUTPUT: "ADIABATIC" TEMPERATURE T AND LIQUID WATER CONTENT
!
!     MODIFIED: November 1989 by Paul Lawson for LAPS/WISP.  Routine
!               now calculates adiabatic liquid water content
!               (ADIABATIC_LWC) using cloud base pressure and grid-top
!               temperature and pressure.  Also calculated are ADJUSTED_LWC,
!               which adjusts ADIABATIC_LWC using an empirical cloud
!               water depletion algorithm, and ADJUSTED_SLWC, which is
!               ADIABATIC_LWC in regions where T < 0 C adjusted
!               using an empirical algorithm by Marcia Politovich.
!
!               Subroutine is now hardwired for stratiform cloud only.
!               Can be modified to include Cu with input from LAPS main.
!
!               revb: ca 12/89 Calculate adiabatic lwc by going from cloud
!                     base to LAPS grid level instead to cloud top, thus
!                     helping to better calculate in layer clouds.
!                     Add TG (grid temperature) to calcualtion.
!
!               revc: 2/27/90 Correct error in code.  Zero-out slwc when grid
!                     temperature (GT) > 0.
!
!               J.Z.: 4/7/97 Correct error in code
!                     Grid temperature should be TG, not GT.
!
!
!     OUTPUTS:  ADIABATIC_LWC
!               ADJUSTED_LWC
!               ADJUSTED_SLWC
!               I_STATUS1 - 1 when -20 < cld_top_temp < 0 for Stratus
!                           0 Otherwise
!               I_STATUS2 - 1 when valid input data provided from main
!
  use kinds, only: r_single,i_kind,r_kind
  IMPLICIT NONE

  real(r_single), intent(in) :: cb_pa,cb_k,gt_pa,gt_k,ct_k
  real(r_single), intent(out) :: adiabatic_lwc,adjusted_lwc,adjusted_slwc
  INTEGER(i_kind),intent(out) :: i_status1,i_status2

  real(r_kind) :: eps,cpd,cw,rd,alhv
  DATA eps/0.622_r_kind/,cpd/1.0042E3_r_kind/,cw/4.218E3_r_kind/,rd/287.05_r_kind/,alhv/2.501E6_r_kind/
  INTEGER(i_kind) :: cty,i
  real(r_kind) :: p0,p,t0,tg,ctt,tk,e,r,cpt,t1,thetaq,rv,t,tw
  real(r_kind) :: vapor
!
!
  i_status1=1
  i_status2=1
!   2 Print *,'ENTER: P-BASE(mb), T-BASE(C), P-TOP, T-TOP, CLD TYPE'
!  READ(5,*) P0, T0, P, CTT, CTY
!  If(CTY.ne.0.and.CTY.ne.1) Go to 2
!
!  Hardwire cloud type (CTY) for stratus for now
!
  cty=0
!
!.....Convert Pa to mb and Kelvin to Celcius
!
  p0 = cb_pa/100._r_kind
  p  = gt_pa/100._r_kind
  t0 = cb_k - 273.15_r_kind
  tg = gt_k - 273.15_r_kind
  ctt= ct_k - 273.15_r_kind
!  Print *, 'CTT in Sub = ', CTT
!
!  Check for valid input data...
!
  IF(p0 > 1013._r_kind.OR.p0 < 50._r_kind) THEN
    i_status2=0
    RETURN
  ELSE
  END IF
!
!
  IF(t0 > 50._r_kind.OR.t0 < -70._r_kind) THEN
    i_status2=0
    RETURN
  ELSE
  END IF
!
!
  IF(p > 1013._r_kind.OR.p < 50._r_kind) THEN
    i_status2=0
    RETURN
  ELSE
  END IF
!
!  Set I_STATUS1 = F if 0 < cld top < -20 C (for stratus).
!
  IF(tg >= 0._r_kind.OR.ctt < -20._r_kind) i_status1=0
!
  tk=t0+273.15_r_kind
  e=vapor(t0)
  r=eps*e/(p0-e)
  cpt=cpd+r*cw
  thetaq=tk*(1000._r_kind/(p0-e))**(rd/cpt)*EXP(alhv*r/(cpt*tk))
! 1ST APPROX
  t1=tk
  e=vapor(t1-273.15_r_kind)
  rv=eps*e/(p-e)
  t1=thetaq/((1000._r_kind/(p-e))**(rd/cpt)*EXP(alhv*rv/(cpt*t1)))
! SUCCESSIVE APPROXIMATIONS
  DO i=1,10
    e=vapor(t1-273.15_r_kind)
    rv=eps*e/(p-e)
    t1=(thetaq/((1000._r_kind/(p-e))**(rd/cpt)*EXP(alhv*rv/(cpt*t1)))          &
        +t1)/2._r_kind
    t=t1-273.15_r_kind
!  Print *, P0,T0,P,T,E,RV,THETAQ
  END DO
! GET LWC
  e=vapor(t)
  rv=eps*e/(p-e)
  tw=r-rv
  adiabatic_lwc=tw*p*28.9644_r_kind/(8.314E7_r_kind*t1)*1.e9_r_kind
  IF(adiabatic_lwc < 0._r_kind) adiabatic_lwc=0._r_kind
!  Print *, 'Adiabtic LWC = ', ADIABATIC_LWC
  IF(tg >= 0._r_kind) THEN
!
    adjusted_slwc=0._r_kind                                          ! Added 2/27/90
!

    IF(cty == 0._r_kind) THEN
      IF(ctt < -20._r_kind) THEN
        adjusted_lwc=0._r_kind
      ELSE IF(ctt < -15._r_kind.AND.ctt >= -20._r_kind) THEN
        adjusted_lwc=adiabatic_lwc/8._r_kind
      ELSE IF(ctt < -10._r_kind.AND.ctt >= -15._r_kind) THEN
        adjusted_lwc=adiabatic_lwc/4._r_kind
      ELSE
        adjusted_lwc=adiabatic_lwc/2._r_kind
      END IF
    ELSE
      IF(ctt < -25._r_kind) THEN
       adjusted_lwc=0._r_kind
      ELSE IF(ctt < -15._r_kind.AND.ctt >= -25._r_kind) THEN
        adjusted_lwc=adiabatic_lwc/8._r_kind
      ELSE IF(ctt < -10._r_kind.AND.ctt >= -15._r_kind) THEN
        adjusted_lwc=adiabatic_lwc/4._r_kind
      ELSE
        adjusted_lwc=adiabatic_lwc/2._r_kind
      END IF
    END IF
  ELSE
    IF(cty == 0._r_kind) THEN
      IF(ctt < -20._r_kind) THEN
        adjusted_lwc=0._r_kind
        adjusted_slwc=0._r_kind
      ELSE IF(ctt < -15._r_kind.AND.ctt >= -20._r_kind) THEN
        adjusted_lwc=adiabatic_lwc/8._r_kind
        adjusted_slwc=adiabatic_lwc/8._r_kind
      ELSE IF(ctt < -10._r_kind.AND.ctt >= -15._r_kind) THEN
        adjusted_lwc=adiabatic_lwc/4._r_kind
        adjusted_slwc=adiabatic_lwc/4._r_kind
      ELSE
        adjusted_lwc=adiabatic_lwc/2._r_kind
        adjusted_slwc=adiabatic_lwc/2._r_kind
      END IF
    ELSE
      IF(ctt < -25._r_kind) THEN
        adjusted_lwc=0._r_kind
        adjusted_slwc=0._r_kind
      ELSE IF(ctt < -15._r_kind.AND.ctt >= -25._r_kind) THEN
        adjusted_lwc=adiabatic_lwc/8._r_kind
        adjusted_slwc=adiabatic_lwc/8._r_kind
      ELSE IF(ctt < -10._r_kind.AND.ctt >= -15._r_kind) THEN
        adjusted_lwc=adiabatic_lwc/4._r_kind
        adjusted_slwc=adiabatic_lwc/4._r_kind
      ELSE
        adjusted_lwc=adiabatic_lwc/2._r_kind
        adjusted_slwc=adiabatic_lwc/2._r_kind
      END IF
    END IF
  END IF
!  Print *,'Adjusted LWC = ', ADJUSTED_LWC
!  Print *,'Adjusted SLWC = ', ADJUSTED_SLWC
END SUBROUTINE slwc_revb

!  FUNCTION TO CALCULATE VAPOR PRESSURE:
!

  FUNCTION vapor(tfp)
! INPUT IS IN DEGREES C.  IF GT 0, ASSUMED TO BE DEW POINT.  IF
! LESS THAN 0, ASSUMED TO BE FROST POINT.
! ROUTINE CODES GOFF-GRATCH FORMULA
  use kinds, only: i_kind,r_kind
  IMPLICIT NONE

  real(r_kind), intent(in) :: tfp
  real(r_kind) :: vapor

!
  real(r_kind) :: tvap, e

  tvap=273.16_r_kind+tfp
  IF(tfp > 0.) GO TO 1
! THIS IS ICE SATURATION VAPOR PRESSURE
  IF(tvap <= 0) tvap=1E-20_r_kind
  e=-9.09718_r_kind*(273.16_r_kind/tvap-1._r_kind)- &
         3.56654_r_kind*LOG10(273.16_r_kind/tvap)   &
      +0.876793_r_kind*(1.-tvap/273.16_r_kind)
  vapor=6.1071_r_kind*10._r_kind**e
  RETURN
  1    CONTINUE
! THIS IS WATER SATURATION VAPOR PRESSURE
  IF(tvap <= 0) tvap=1E-20_r_kind
  e=-7.90298_r_kind*(373.16_r_kind/tvap-1._r_kind)+ &
     5.02808_r_kind*LOG10(373.16_r_kind/tvap)       &
      -1.3816E-7_r_kind*(10._r_kind**(11.344_r_kind*&
      (1._r_kind-tvap/373.16_r_kind))-1._r_kind)    &
      +8.1328E-3_r_kind*(10._r_kind**(3.49149_r_kind&
      *(1-373.16_r_kind/tvap))-1)
  vapor=1013.246_r_kind*10._r_kind**e
  RETURN
  END FUNCTION vapor
