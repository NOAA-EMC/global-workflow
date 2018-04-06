      SUBROUTINE CALRH(P1,T1,Q1,RH)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALRH       COMPUTES RELATIVE HUMIDITY
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22       
!     
! ABSTRACT:  
!     THIS ROUTINE COMPUTES RELATIVE HUMIDITY GIVEN PRESSURE, 
!     TEMPERATURE, SPECIFIC HUMIDITY. AN UPPER AND LOWER BOUND
!     OF 100 AND 1 PERCENT RELATIVE HUMIDITY IS ENFORCED.  WHEN
!     THESE BOUNDS ARE APPLIED THE PASSED SPECIFIC HUMIDITY 
!     ARRAY IS ADJUSTED AS NECESSARY TO PRODUCE THE SET RELATIVE
!     HUMIDITY.
!   .     
!     
! PROGRAM HISTORY LOG:
!   ??-??-??  DENNIS DEAVEN
!   92-12-22  RUSS TREADON - MODIFIED AS DESCRIBED ABOVE.
!   98-06-08  T BLACK      - CONVERSION FROM 1-D TO 2-D
!   98-08-18  MIKE BALDWIN - MODIFY TO COMPUTE RH OVER ICE AS IN MODEL
!   98-12-16  GEOFF MANIKIN - UNDO RH COMPUTATION OVER ICE
!   00-01-04  JIM TUCCILLO - MPI VERSION
!   02-06-11  MIKE BALDWIN - WRF VERSION
!     
! USAGE:    CALL CALRH(P1,T1,Q1,RH)
!   INPUT ARGUMENT LIST:
!     P1     - PRESSURE (PA)
!     T1     - TEMPERATURE (K)
!     Q1     - SPECIFIC HUMIDITY (KG/KG)
!
!   OUTPUT ARGUMENT LIST: 
!     RH     - RELATIVE HUMIDITY  (DECIMAL FORM)
!     Q1     - ADJUSTED SPECIFIC HUMIDITY (KG/KG)
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!     LIBRARY:
!       NONE
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!
     use params_mod, only: PQ0, a2, a3, a4, rhmin
     use ctlblk_mod, only: jsta, jend, spval, im
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!     
!     SET PARAMETER.
!
!     DECLARE VARIABLES.
!     
      REAL,dimension(IM,jsta:jend),intent(in)    :: P1,T1
      REAL,dimension(IM,jsta:jend),intent(inout) :: Q1
      REAL,dimension(IM,jsta:jend),intent(out)   :: RH
      REAL QC
      integer I,J
!***************************************************************
!
!     START CALRH.
!
      DO J=JSTA,JEND
        DO I=1,IM
          IF (T1(I,J) < SPVAL) THEN
            IF (ABS(P1(I,J)) > 1) THEN
              QC = PQ0/P1(I,J)*EXP(A2*(T1(I,J)-A3)/(T1(I,J)-A4))
!
              RH(I,J) = Q1(I,J)/QC
!
!   BOUNDS CHECK
!
              IF (RH(I,J) > 1.0) THEN
                RH(I,J) = 1.0
                Q1(I,J) = RH(I,J)*QC
              ENDIF
              IF (RH(I,J) < RHmin) THEN  !use smaller RH limit for stratosphere
                RH(I,J) = RHmin
                Q1(I,J) = RH(I,J)*QC
              ENDIF
!
            ENDIF
          ELSE
            RH(I,J) = SPVAL
          ENDIF
        ENDDO
      ENDDO

      RETURN
      END
