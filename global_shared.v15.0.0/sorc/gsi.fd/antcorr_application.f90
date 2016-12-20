!
! AntCorr_Application
!
! Module containing routines to apply/remove antenna corrections to/from 
! supported microwave sensor observations.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Aug-2008
!                       paul.vandelst@noaa.gov
!
!   2011-04-25   A.Collard   Modified to be consistent with CRTM 2.1
! 

MODULE AntCorr_Application

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: Display_Message, FAILURE
  USE ACCoeff_Define   , ONLY: ACCoeff_type
  ! Disable all implicit typing
  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------
  ! Everything private by default
  PRIVATE
  ! Inherited procedures from definition module
  ! -------------------------------------------
  ! The AntCorr structure definition
  PUBLIC :: ACCoeff_type
  PUBLIC :: Remove_AntCorr
  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! Invalid result
  REAL(fp), PARAMETER :: INVALID = -1.0_fp
    
  ! Cosmic background temperature. Taken from
  ! Mather,J.C. et. al., 1999, "Calibrator Design for the COBE
  !    Far-Infrared Absolute Spectrophotometer (FIRAS)"
  !    Astrophysical Journal, vol 512, pp 511-520
  REAL(fp), PARAMETER :: TSPACE = 2.7253_fp
  

CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Remove_AntCorr
!
! PURPOSE:
!       Subroutine to remove an antenna correction to microwave instrument
!       brightness temperatures, Tb, to produce antenna temperatures, Ta.
!
! CALLING SEQUENCE:
!       CALL Remove_AntCorr( AC  , &  ! Input
!                            iFOV, &  ! Input
!                            T     )  ! In/Output
!
! INPUT ARGUMENTS:
!       AC:             Structure containing the antenna correction coefficients
!                       for the sensor of interest.
!                       UNITS:      N/A
!                       TYPE:       TYPE(ACCoeff_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       iFOV:           The FOV index for a scanline of the sensor of interest.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       T:              On input, this argument contains the brightness
!                       temperatures for the sensor channels.
!                       UNITS:      Kelvin
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Channels)
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       T:              On output, this argument contains the antenna
!                       temperatures for the sensor channels.
!                       If an error occurs, the return values are all -1.
!                       UNITS:      Kelvin
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Channels)
!                       ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       The temperature array argument, T, is modified.
!
! PROCEDURE:
!       For every FOV and channel, the brightness temperature, Tb, is converted
!       to antenna temperature, Ta, using,
!
!         Ta = Ae.Tb + Ap.Tb + As.Ts
!
!       where Ae == antenna efficiency for the Earth view
!             Ap == antenna efficiency for satellite platform view
!             As == antenna efficiency for cold space view
!             Ts == cosmic background temperature.
!
!       Note that the observed earth view brightness temperature is used as a
!       proxy for the platform temperature for the (Ap.Tb) component since
!       there is no measurement of the platform temperature in-flight.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Remove_AntCorr( AC  , &  ! Input
                             iFOV, &  ! Input
                             T     )  ! In/Output
    implicit none

    ! Arguments
    TYPE(ACCoeff_type), INTENT(IN)     :: AC
    INTEGER           , INTENT(IN)     :: iFOV
    REAL(fp)          , INTENT(IN OUT) :: T(:)
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Remove_AntCorr'
    ! Local variables
    INTEGER :: l
    ! Check input
    IF ( iFOV < 1 .OR. iFOV > AC%n_FOVS ) THEN
      CALL Display_Message( ROUTINE_NAME, 'Input iFOV inconsistent with AC data', FAILURE )
      T = INVALID
      RETURN
    END IF
    IF ( SIZE(T) /= AC%n_Channels ) THEN
      CALL Display_Message( ROUTINE_NAME, 'Size of T() inconsistent with AC data', FAILURE )
      T = INVALID
      RETURN
    END IF
    ! Compute the antenna temperature
    DO l = 1, AC%n_Channels
      T(l) = AC%A_earth(iFOV,l)*T(l) + AC%A_platform(iFOV,l)*T(l) + AC%A_space(iFOV,l)*TSPACE
    END DO
  END SUBROUTINE Remove_AntCorr
  
END MODULE AntCorr_Application
