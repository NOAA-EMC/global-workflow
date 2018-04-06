      SUBROUTINE SNFRAC_GFS(SNEQV,IVEG,SNCOVR)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!      
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
      integer,intent(in) ::  IVEG
      REAL,intent(in) ::  SNEQV
      REAL,intent(out) ::  SNCOVR
      REAL SALP,SNUP(13),RSNOW
      integer ii,jj

      DATA SALP /2.6/
      DATA SNUP /0.080, 0.080, 0.080, 0.080, 0.080, 0.080,      &
     &  	 0.040, 0.040, 0.040, 0.040, 0.025, 0.040,      &
     &  	 0.025/
     
! ----------------------------------------------------------------------
! SNUP IS VEG-CLASS DEPENDENT SNOWDEPTH THRESHHOLD ABOVE WHICH SNOCVR=1.
! ----------------------------------------------------------------------
        IF (SNEQV .LT. SNUP(IVEG)) THEN
          RSNOW = SNEQV/SNUP(IVEG)
          SNCOVR = 1. - (EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
        ELSE
          SNCOVR = 1.0
        ENDIF
        SNCOVR = MAX(0.,MIN(SNCOVR,1.))

      RETURN
      END
