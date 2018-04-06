      SUBROUTINE MSFPS(LAT,TRUELAT1,MSF)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    MSFPS Computes the map scale factor for a Polar
!                      Stereographic grid at a give latitude.
!
! ABSTRACT:
!     Computes the map scale factor for a Polar Stereographic
!     grid at a give latitude.
!
! PROGRAM HISTORY LOG:
!   06-11-01 SWIPED FROM WRF SI PACKAGE BY ROZUMALSKI
!
!   INPUT ARGUMENT LIST:
!     LAT     - LATITUDE AT WHICH MAP FACTOR IS VALID
!     TRUELAT1 - TRUELAT 1
!
!   OUTPUT ARGUMENT LIST:
!     MSF - MAP SCALE FACTOR
!
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!$$$

! Computes the map scale factor for a Polar Stereographic grid at a given
! latitude.

      IMPLICIT NONE

! Define some private constants
!
      REAL, PARAMETER   :: pi = 3.1415927
      REAL, PARAMETER   :: rad_per_deg = pi / 180.

      REAL, INTENT(IN)           :: lat  ! latitude where msf is requested
      REAL, INTENT(IN)           :: truelat1
      REAL, INTENT(OUT)          :: msf

      REAL                       :: psi1, psix, pole

      IF (truelat1 .GE. 0.) THEN
        psi1 = (90. - truelat1) * rad_per_deg
        pole =90.
      ELSE
        psi1 = (90. + truelat1) * rad_per_deg
        pole = -90.
      ENDIF

      psix = (pole - lat)*rad_per_deg
      msf = ((1.+COS(psi1))/(1.0 + COS(psix)))
      RETURN

      END SUBROUTINE MSFPS

