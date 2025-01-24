!
!$$$  subprogram documentation block
!                .      .    .                                       .
! ABSTRACT: 
!  This file collects subroutines related to cloud analysis in ADAS (CAPS)
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
!
!##################################################################
!##################################################################
!######                                                      ######
!######            FUNCTION RH_TO_CLDCV                      ######
!######                                                      ######
!##################################################################
!##################################################################
!

  FUNCTION rh_to_cldcv(rh,hgt)
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Obtain first guess cloud cover field from relative humidity.
!
!
!  AUTHOR:   Jian Zhang
!  07/95
!
!  MODIFICATION HISTORY
!
!  04/08/97  J. Zhang
!            Added the empirical relationship between RH and
!            cloud cover used by Koch et al. (1997).
!  Reference:
!  Reference:
!  Koch, S.E., A. Aksakal, and J.T. McQueen, 1997:
!      The influence of mesoscale humidity and evapotranspiration
!      fields on a model forecast of a cold-frontal squall line.
!      Mon. Wea. Rev.,  Vol.125,  384-409
!  09/10/97  J. Zhang
!            Modified the empirical relationship between cloud
!            fraction and relative humidity from quadratic
!            to one-fourth-power.
!
!
!-----------------------------------------------------------------------
!
!  INPUT:
!    rh               ! relative humidity
!    hgt              ! height (AGL)
!
!  OUTPUT:
!    rh_to_cld_cv     ! cloud fractional cover value
!
!  LOCAL:
!    rh0              ! the critical RH value that seperate clear
                      ! air condition and cloudy condition
!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_single,i_kind,r_kind

  IMPLICIT NONE

  INTEGER(i_kind) :: rh2cform
  PARAMETER (rh2cform=2)

  REAL(r_kind), intent(in) :: rh,hgt
  REAL(r_kind) :: rh_to_cldcv
  REAL(r_kind) :: rh0

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!
  IF(rh2cform == 1) THEN
!
!-----------------------------------------------------------------------
!
!   A quadratic relationship between relative humidity and cloud
!   fractional cover.
!
!-----------------------------------------------------------------------
!
    IF (hgt < 600.0_r_kind) THEN
      rh0 = 0.9_r_kind
    ELSE IF (hgt < 1500.0_r_kind) THEN
      rh0 = 0.8_r_kind
    ELSE IF (hgt < 2500.0_r_kind) THEN
      rh0 = 0.6_r_kind
    ELSE
      rh0 = 0.5_r_kind
    END IF

    IF (rh < rh0) THEN
      rh_to_cldcv = 0.0_r_kind
    ELSE
      rh_to_cldcv = (rh - rh0)/(1.0_r_kind - rh0)
      rh_to_cldcv = rh_to_cldcv*rh_to_cldcv
    END IF

  ELSE IF(rh2cform == 2) THEN
!
!-----------------------------------------------------------------------
!
!   A quadratic relationship between relative humidity and cloud
!   fractional cover with fixed rh0=0.75
!
!-----------------------------------------------------------------------
!
!
    IF (rh < 0.75_r_kind) THEN
      rh_to_cldcv = 0.0_r_kind
    ELSE
      rh_to_cldcv = 16._r_kind*(rh - 0.75_r_kind)*(rh - 0.75_r_kind)
    END IF

  ELSE
!
!-----------------------------------------------------------------------!
!   A modified version of the sqrt relationship between
!   relative humidity and cloud fractional cover used in Eta model.
!
!-----------------------------------------------------------------------
!
    IF (hgt < 600._r_kind) THEN
      rh0 = 0.8_r_kind
    ELSE
      rh0 = 0.75_r_kind
    END IF

    IF (rh < rh0) THEN
      rh_to_cldcv = 0.0_r_kind
    ELSE
      rh_to_cldcv = 1.0_r_kind - SQRT((1.0_r_kind - rh)/(1.0_r_kind - rh0))
    END IF

  END IF

  RETURN
  END FUNCTION rh_to_cldcv
!
!
!##################################################################
!##################################################################
!######                                                      ######
!######                   FUNCTION F_ES                      ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

FUNCTION f_es( p, t )
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Calculate the saturation specific humidity using enhanced Teten's
!  formula.
!
!-----------------------------------------------------------------------
!
!  AUTHOR: Yuhe Liu
!  01/08/1998
!
!  MODIFICATION HISTORY:
!
!-----------------------------------------------------------------------
!
!  INPUT :
!
!    p        Pressure (Pascal)
!    t        Temperature (K)
!
!  OUTPUT:
!
!    f_es     Saturation water vapor pressure (Pa)
!
!-----------------------------------------------------------------------
!

!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE

  REAL :: p         ! Pressure (Pascal)
  REAL :: t         ! Temperature (K)
  REAL :: f_es      ! Saturation water vapor pressure (Pa)
!
!-----------------------------------------------------------------------
!
!  Function f_es and inline directive for Cray PVP
!
!-----------------------------------------------------------------------
!
  REAL :: f_esl, f_esi

!fpp$ expand (f_esl)
!fpp$ expand (f_esi)
!!dir$ inline always f_esl, f_esi
!*$*  inline routine (f_esl, f_esi)

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  IF ( t >= 273.15 ) THEN      ! for water
    f_es = f_esl( p,t )
  ELSE                            ! for ice
    f_es = f_esi( p,t )
  END IF

  RETURN
END FUNCTION f_es

!
!-----------------------------------------------------------------------
!
!  Calculate the saturation water vapor over liquid water using
!  enhanced Teten's formula.
!
!-----------------------------------------------------------------------
!

FUNCTION f_esl( p, t )

  IMPLICIT NONE

!  constant
  REAL :: satfwa, satfwb
  PARAMETER ( satfwa = 1.0007 )
  PARAMETER ( satfwb = 3.46E-8 )  ! for p in Pa

  REAL :: satewa, satewb, satewc
  PARAMETER ( satewa = 611.21 )   ! es in Pa
  PARAMETER ( satewb = 17.502 )
  PARAMETER ( satewc = 32.18 )

  REAL :: p         ! Pressure (Pascal)
  REAL :: t         ! Temperature (K)
  REAL :: f_esl     ! Saturation water vapor pressure over liquid water

  REAL :: f

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  f = satfwa + satfwb * p
  f_esl = f * satewa * EXP( satewb*(t-273.15)/(t-satewc) )

  RETURN
END FUNCTION f_esl
!
!-----------------------------------------------------------------------
!
!  Calculate the saturation water vapor over ice using enhanced
!  Teten's formula.
!
!-----------------------------------------------------------------------
!

FUNCTION f_esi( p, t )

  IMPLICIT NONE

!
  REAL :: satfia, satfib
  PARAMETER ( satfia = 1.0003 )
  PARAMETER ( satfib = 4.18E-8 )  ! for p in Pa

  REAL :: sateia, sateib, sateic
  PARAMETER ( sateia = 611.15 )   ! es in Pa
  PARAMETER ( sateib = 22.452 )
  PARAMETER ( sateic = 0.6 )

  REAL :: p         ! Pressure (Pascal)
  REAL :: t         ! Temperature (K)
  REAL :: f_esi     ! Saturation water vapor pressure over ice (Pa)

  REAL :: f

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  f = satfia + satfib * p
  f_esi = f * sateia * EXP( sateib*(t-273.15)/(t-sateic) )

  RETURN
END FUNCTION f_esi
!
!
!##################################################################
!##################################################################
!######                                                      ######
!######                   FUNCTION F_QVSAT                   ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

FUNCTION f_qvsat( p, t )
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Calculate the saturation specific humidity using enhanced Teten's
!  formula.
!
!-----------------------------------------------------------------------
!
!  AUTHOR: Yuhe Liu
!  01/08/1998
!
!  MODIFICATION HISTORY:
!
!-----------------------------------------------------------------------
!
!  INPUT :
!
!    p        Pressure (Pascal)
!    t        Temperature (K)
!
!  OUTPUT:
!
!    f_qvsat  Saturation water vapor specific humidity (kg/kg).
!
!-----------------------------------------------------------------------
!

!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE

  REAL :: p         ! Pressure (Pascal)
  REAL :: t         ! Temperature (K)
  REAL :: f_qvsat   ! Saturation water vapor specific humidity (kg/kg)
!
!-----------------------------------------------------------------------
!
!  Include files:
!
!-----------------------------------------------------------------------
!
!

  REAL :: rd        ! Gas constant for dry air  (m**2/(s**2*K))
  PARAMETER( rd     = 287.0 )

  REAL :: rv        ! Gas constant for water vapor  (m**2/(s**2*K)).
  PARAMETER( rv     = 461.0 )

  REAL :: rddrv
  PARAMETER( rddrv  = rd/rv )

!
!-----------------------------------------------------------------------
!
!  Function f_es and inline directive for Cray PVP
!
!-----------------------------------------------------------------------
!
  REAL :: f_es
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  f_qvsat = rddrv * f_es(p,t) / (p-(1.0-rddrv)*f_es(p,t))

  RETURN
END FUNCTION f_qvsat

SUBROUTINE getdays(nday,iyear,imonth,iday)

  use kinds, only: i_kind
  implicit none
!
  INTEGER(i_kind), intent(in)  :: iyear,imonth,iday
  INTEGER(i_kind), intent(out) :: nday
!

  nday=0
  if(imonth==1) then
    nday=iday
  elseif(imonth==2) then
    nday=31+iday
  elseif(imonth==3) then
    nday=59+iday
  elseif(imonth==4) then
    nday=90+iday
  elseif(imonth==5) then
    nday=120+iday
  elseif(imonth==6) then
    nday=151+iday
  elseif(imonth==7) then
    nday=181+iday
  elseif(imonth==8) then
    nday=212+iday
  elseif(imonth==9) then
    nday=243+iday
  elseif(imonth==10) then
    nday=273+iday
  elseif(imonth==11) then
    nday=304+iday
  elseif(imonth==12) then
    nday=334+iday
  endif
  if(mod(iyear,4) == 0 .and. imonth > 2 ) nday=nday+1

END SUBROUTINE getdays
