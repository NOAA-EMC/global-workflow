      SUBROUTINE SCLFLD(FLD,SCALE,IMO,JMO)
!
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    SCLFLD      SCALE ARRAY ELEMENT BY CONSTANT
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-09-13       
!     
! ABSTRACT:  
!     THIS ROUTINE MULTIPLES (SCALES) THE FIRST IMO*JMO
!     ELEMENTS OF ARRAY FLD BY THE REAL SCALAR SCALE.
!     ARRAY ELEMENTS WHICH EQUAL A SPECIAL VALUE WILL
!     NOT BE SCALED BY SCALE.  THEY WILL BE LEFT AS IS.
!     THE SPECIAL VALUE, SPVAL, IS PASSED THROUGH COMMON
!     BLOCK OPTIONS.  IT IS SET IN INCLUDE FILE OPTIONS.
!   .     
!     
! PROGRAM HISTORY LOG:
!   92-09-13  RUSS TREADON
!   00-01-04  JIM TUCCILLO
!     
! USAGE:    CALL SCLFLD(FLD,SCALE,IMO,JMO)
!   INPUT ARGUMENT LIST:
!     FLD      - ARRAY WHOSE ELEMENTS ARE TO BE SCALED.
!     SCALE    - CONSTANT BY WHICH TO SCALE ELEMENTS OF FLD.
!     IMO,JMO  - DIMENSION OF ARRAY FLD.
!
!   OUTPUT ARGUMENT LIST: 
!     FLD      - ARRAY WHOSE ELEMENTS HAVE BEEN SCALED BY SCALE.
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON - OPTIONS
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!
      use params_mod, only: small
      use ctlblk_mod, only: jsta, jend, spval
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!     
!     DECLARE VARIABLES.
!     
      integer,intent(in) :: IMO,JMO
      REAL,intent(in) ::  SCALE
      REAL,dimension(imo,jmo),intent(inout) :: FLD
      integer I,J
!     
!     
!***********************************************************************
!     START SCLFLD HERE
!     
!     MULTIPLY EACH ELEMENT OF FLD BY SCALE.
!     
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IMO
        IF(ABS(FLD(I,J)-SPVAL).GT.SMALL) FLD(I,J)=SCALE*FLD(I,J)
      ENDDO
      ENDDO
!     
!     END OF ROUTINE.
!     
      RETURN
      END
