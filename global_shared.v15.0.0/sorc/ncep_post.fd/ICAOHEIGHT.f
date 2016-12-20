      SUBROUTINE ICAOHEIGHT(MAXWP,     & !input
                            MAXWICAOZ)   ! output

! SUBPROGRAM:    ICAOHEIGHT      
!   PRGRMMR: CHUANG         ORG: W/NP2      DATE: 09-05-08
!     
! ABSTRACT:
!     THIS ROUTINE CONVERT PRESSURE FIELDS TO ICAO HEIGHT
!

! Description:
!   Convert pressure (Pa) to height (m) using ICAO standard atmosphere
!   adpt code from uk
! Method:
!
!
! Code Description:
!   Language:           Fortran 90
!   Software Standards: UMDP3 v6

use ctlblk_mod, only: jsta, jend, spval, im
use physcons, only: con_g, con_rd
IMPLICIT None

! Subroutine Arguments:
!REAL, INTENT(IN) :: SPVAL
REAL, INTENT(IN) :: MAXWP(IM,jsta:jend)       !P field for conversion

REAL, INTENT(INOUT) :: MAXWICAOZ(IM,jsta:jend)   !ICAO height in m
!INTEGER, INTENT(INOUT) :: ErrorStatus

! Local Constants:
REAL, PARAMETER :: G_over_R = con_G / con_Rd
REAL, PARAMETER :: Lapse_RateL = 6.5E-03  ! For levels below 11,000 gpm
REAL, PARAMETER :: Lapse_RateU = -1.0E-03 ! For levels above 11,000 gpm
REAL, PARAMETER :: Press_Bot = 101325.    ! ICAO std: surface pressure
REAL, PARAMETER :: Press_Mid = 22632.     !      pressure @ 11,000 gpm
REAL, PARAMETER :: Press_Top = 5474.87    !      pressure @ 20,000 gpm
REAL, PARAMETER :: Temp_Bot = 288.15      ! Surface temperature
REAL, PARAMETER :: Temp_Top = 216.65      ! Temperature of isotherm
REAL, PARAMETER :: Gpm1 = 11000.0  ! Ht limit (gpm) for std lower
                                   ! lapse rate
REAL, PARAMETER :: Gpm2 = 20000.0  ! Ht (gpm) of top of isothermal layer
REAL, PARAMETER :: ZP1 = Lapse_RateL/G_over_R ! Exponents used for
REAL, PARAMETER :: ZP2 = Lapse_RateU/G_over_R ! calculation

! Local Variables:
INTEGER :: i,j      ! loop counters
REAL :: Pressure    ! Local pressure

! End of header --------------------------------------------------------

!IF ( ErrorStatus /= StatusOK ) THEN
  ! Previous error - do not proceed
!  GO TO 9999
!END IF

!IF ( ASSOCIATED( IHField % RData ) ) THEN
!  DEALLOCATE( IHField % RData )
!END IF



DO J=JSTA,JEND
  DO I=1,IM
    pressure = MAXWP(i,j)
    IF ( (pressure <= 1000.) .AND. (pressure >= 0.) ) THEN
      pressure = 1000.
      print*,'lower ICAO pressure to 10 mb'
    END IF
    IF ( pressure > Press_Bot .and. pressure<spval) THEN
      pressure = Press_Bot
    END IF

    IF (pressure == spval) THEN
      MAXWICAOZ(i,j) = spval
    ELSE IF (pressure > Press_Mid) THEN ! Hts up to 11,000 GPM
      pressure = pressure/Press_Bot
      pressure = 1.0 - pressure**ZP1
      MAXWICAOZ(i,j) = pressure*Temp_Bot/Lapse_RateL

    ELSE IF (pressure > Press_Top) THEN ! Hts between 11,000
                                        !     and     20,000 GPM
      pressure = pressure/Press_Mid
      pressure = -ALOG(pressure)
      MAXWICAOZ(i,j) = Gpm1 + pressure*Temp_Top/G_over_R

    ELSE                                ! Hts above 20,000 GPM
      pressure = pressure/Press_Top
      pressure = 1.0 - pressure**ZP2
      MAXWICAOZ(i,j) = Gpm2 + pressure*Temp_Top/Lapse_RateU

    END IF

  ENDDO
ENDDO


!9999 CONTINUE

END SUBROUTINE ICAOHeight


