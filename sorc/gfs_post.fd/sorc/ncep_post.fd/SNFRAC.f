      SUBROUTINE SNFRAC (SNEQV,IVEGx,SNCOVR)

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
       include 'mpif.h'
      
! ----------------------------------------------------------------------
! SUBROUTINE SNFRAC
! ----------------------------------------------------------------------
! CALCULATE SNOW FRACTION (0 -> 1)
! SNEQV   SNOW WATER EQUIVALENT (M)
! IVEG    VEGETATION TYPE
! SNCOVR  FRACTIONAL SNOW COVER
! SNUP    THRESHOLD SNEQV DEPTH ABOVE WHICH SNCOVR=1
! SALP    TUNING PARAMETER
! ----------------------------------------------------------------------
      integer,intent(in) :: IVEGx
      REAL,intent(in) ::  SNEQv
      REAL,intent(out) ::  SNCOVR
      REAL SALP,SNUP(20),RSNOW
      integer IVEG

      DATA SALP /4.0/
      DATA SNUP /0.080, 0.080, 0.080, 0.080, 0.080, 0.020,        &
     &            0.020, 0.060, 0.040, 0.020, 0.010, 0.020,       &
     &            0.020, 0.020, 0.013, 0.013, 0.010, 0.020,       &
     &            0.020, 0.020/
     
! ----------------------------------------------------------------------
! SNUP IS VEG-CLASS DEPENDENT SNOWDEPTH THRESHHOLD ABOVE WHICH SNOCVR=1.
! ----------------------------------------------------------------------
!jjt
        IVEG = IVEGx
        IF ( IVEG .gt. 20 .or. IVEG .lt. 1 ) then
!          print *, ' PROBLEM in SNFRAC, IVEG = ',iveg
           IVEG = 1
        END IF
        IF (SNEQV .LT. SNUP(IVEG)) THEN
          RSNOW = SNEQV/SNUP(IVEG)
          SNCOVR = 1. - (EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
        ELSE
          SNCOVR = 1.0
        ENDIF
        SNCOVR = MAX(0.,MIN(SNCOVR,1.))

      RETURN
      END
