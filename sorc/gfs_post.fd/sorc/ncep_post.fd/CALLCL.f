      SUBROUTINE CALLCL(P1D,T1D,Q1D,PLCL,ZLCL)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALLCL      COMPUTES LCL HEIGHTS AND PRESSURE
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-03-15
!     
! ABSTRACT:
!     THIS ROUTINE COMPUTES THE LIFTING CONDENSATION LEVEL 
!     PRESSURE AND HEIGHT IN EACH COLUMN AT MASS POINTS.
!     THE HEIGHT IS ABOVE GROUND LEVEL.  THE EQUATION USED
!     TO FIND THE LCL PRESSURE IS FROM BOLTAN (1980,MWR) 
!     AND IS THE SAME AS THAT USED IN SUBROUTINE CALCAPE.
!     
!     THIS ROUTINE IS A TEST VERSION.  STILL TO BE RESOLVED
!     IS THE "BEST" PARCEL TO LIFT.
!   .     
!     
! PROGRAM HISTORY LOG:
!   93-03-15  RUSS TREADON
!   98-06-16  T BLACK - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION            
!   02-04-24  MIKE BALDWIN - WRF VERSION            
!     
! USAGE:    CALL CALLCL(P1D,T1D,Q1D,PLCL,ZLCL)
!   INPUT ARGUMENT LIST:
!     P1D      - ARRAY OF PARCEL PRESSURES (PA)
!     T1D      - ARRAY OF PARCEL TEMPERATURES (K)
!     Q1D      - ARRAY OF PARCEL SPECIFIC HUMIDITIES (KG/KG)
!
!   OUTPUT ARGUMENT LIST: 
!     PLCL     - PARCEL PRESSURE AT LCL (PA)
!     ZLCL     - PARCEL AGL HEIGHT AT LCL (M)
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - LOOPS
!                  OPTIONS
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90
!     MACHINE : CRAY C-90
!$$$  
!     
!     
      use vrbls3d, only: alpint, zint
      use vrbls2d, only: fis
      use masks, only: lmh
      use params_mod, only: eps, oneps, d01, h1m12, gi, d00
      use ctlblk_mod, only: jsta, jend, spval, jsta_m, jend_m, im
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!     
      real,PARAMETER :: D35=3.5, D4805=4.805,  H2840=2840.
      real,PARAMETER :: H55=55., D2845=0.2845, D28=0.28
!
!     DECLARE VARIABLES.
!     
      REAL,dimension(IM,jsta:jend), intent(in)    :: P1D,T1D,Q1D
      REAL,dimension(IM,jsta:jend), intent(inout) :: PLCL,ZLCL
      REAL TLCL(IM,jsta:jend)
      integer I,J,L,LLMH
      real DLPLCL,ZSFC,DZ,DALP,ALPLCL,RMX,EVP,ARG,RKAPA
!     
!**********************************************************************
!     START CALLCL HERE.
!     
!     LOAD OUTPUT ARRAYS WITH SPECIAL VALUE.
!     
!$omp parallel do private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
          PLCL(I,J) = SPVAL
          TLCL(I,J) = SPVAL
          ZLCL(I,J) = SPVAL
        ENDDO
      ENDDO
!     
!     COMPUTE PRESSURE, TEMPERATURE AND AGL HEIGHT AT LCL.
!
      DO 30 J=JSTA_M,JEND_M
      DO 30 I=2,IM-1
!     DO 30 I=1,IM
      EVP       = P1D(I,J)*Q1D(I,J)/(EPS+ONEPS*Q1D(I,J))
      RMX       = EPS*EVP/(P1D(I,J)-EVP)
      RKAPA     = 1.0 / (D2845*(1.0-D28*RMX))
      ARG       = MAX(H1M12,EVP*D01)
      TLCL(I,J) = H55 + H2840 / (D35*LOG(T1D(I,J))-LOG(ARG)-D4805)
      PLCL(I,J) = P1D(I,J)*(TLCL(I,J)/T1D(I,J))**RKAPA
      ALPLCL    = LOG(PLCL(I,J))
      LLMH      = NINT(LMH(I,J))
      ZSFC      = FIS(I,J)*GI
!
      DO 20 L=LLMH,1,-1
      IF(ALPINT(I,J,L) < ALPLCL)THEN
        DLPLCL    = ALPLCL        - ALPINT(I,J,L+1)
        DALP      = ALPINT(I,J,L) - ALPINT(I,J,L+1)
        DZ        = ZINT(I,J,L)   - ZINT(I,J,L+1)
        ZLCL(I,J) = max(D00, ZINT(I,J,L+1) + DZ*DLPLCL/DALP - ZSFC)
        GOTO 30
      ENDIF
 20   CONTINUE
 30   CONTINUE
!     
!     END OF ROUTINE.
!     
      RETURN
      END
