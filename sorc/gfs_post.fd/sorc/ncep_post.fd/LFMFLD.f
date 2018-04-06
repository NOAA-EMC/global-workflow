      SUBROUTINE LFMFLD(RH3310,RH6610,RH3366,PW3310)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    LFMFLD      COMPUTES LAYER MEAN LFM FIELDS
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22
!     
! ABSTRACT:
!     THIS ROUTINE COMPUTES THREE LAYER MEAN RELATIVE HUMIDITIES
!     AND A PRECIPITABLE WATER FIELD FROM ETA LEVEL DATA.  THE
!     COMPUTED FIELDS ARE INTENDED TO MIMIC SIMILAR FIELDS COM-
!     PUTED BY THE LFM.  THE ALGORITHM USED HERE IS FAIRLY PRI-
!     MATIVE.  IN EACH COLUMN ABOVE A MASS POINT ON THE ETA GRID
!     WE SET THE FOLLOWING TARGET PRESSURES:
!         SIGMA LAYER 1.00 PRESSURE:  SURFACE PRESSURE
!         SIGMA LAYER 0.66 PRESSURE:  0.50 * SURFACE PRESSURE
!         SIGMA LAYER 0.33 PRESSURE:  0.4356 * SURFACE PRESSURE
!     GIVEN THESE PRESSURES A SURFACE UP SUMMATION IS MADE OF 
!     RELATIVE HUMIDITY AND/OR PRECIPITABLE WATER BETWEEN THESE
!     TARGET PRESSURES.  EACH TERM IN THE SUMMATION IS WEIGHTED
!     BY THE THICKNESS OF THE ETA LAYER.  THE FINAL LAYER MEAN
!     IS THIS SUM NORMALIZED BY THE TOTAL DEPTH OF THE LAYER.  
!     THERE IS, OBVIOUSLY, NO NORMALIZATION FOR PRECIPITABLE WATER.
!
!     
! PROGRAM HISTORY LOG:
!   92-12-22  RUSS TREADON
!   93-07-27  RUSS TREADON - MODIFIED SUMMATION LIMITS FROM
!                            0.66*PSFC TO 0.75*PSFC AND 0.33*PSFC 
!                            TO 0.50*PSFC, WHERE PSFC IS THE
!                            SURFACES PRESSURE.  THE REASON FOR
!                            THIS CHANGE WAS RECOGNITION THAT IN 
!                            THE LFM 0.33 AND 0.66 WERE MEASURED
!                            FROM THE SURFACE TO THE TROPOPAUSE,
!                            NOT THE TOP OF THE MODEL.
!   93-09-13  RUSS TREADON - RH CALCULATIONS WERE MADE INTERNAL
!                            TO THE ROUTINE.
!   96-03-04  MIKE BALDWIN - CHANGE PW CALC TO INCLUDE CLD WTR 
!   98-06-16  T BLACK      - CONVERSION FROM 1-D TO 2-D
!   98-08-17  MIKE BALDWIN - COMPUTE RH OVER ICE
!   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
!   00-01-04  JIM TUCCILLO - MPI VERSION
!   02-04-24  MIKE BALDWIN - WRF VERSION
!     
!     
! USAGE:    CALL LFMFLD(RH3310,RH6610,RH3366,PW3310)
!   INPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT ARGUMENT LIST: 
!     RH3310   - SIGMA LAYER 0.33-1.00 MEAN RELATIVE HUMIDITY.
!     RH6610   - SIGMA LAYER 0.66-1.00 MEAN RELATIVE HUMIDITY.
!     RH3366   - SIGMA LAYER 0.33-0.66 MEAN RELATIVE HUMIDITY.
!     PW3310   - SIGMA LAYER 0.33-1.00 PRECIPITABLE WATER.
!     
!   OUTPUT FILES:
!     NONE
!     
!   LIBRARY:
!     COMMON   - 
!                MAPOT
!                LOOPS
!                OPTIONS
!
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
!
      use vrbls3d, only: pint, alpint, zint, t, q, cwm
      use masks, only: lmh
      use params_mod, only: d00, d50, pq0, a2, a3, a4, h1, d01, gi
      use ctlblk_mod, only: jsta, jend, modelname, spval, im
      use physcons, only: con_rd, con_rv, con_eps, con_epsm1

      implicit none

      real,external::FPVSNEW
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      real,PARAMETER :: RHOWAT=1.E3
!     
!     DECLARE VARIABLES.
!     
      REAL ALPM, DZ, ES, PM, PWSUM, QM, QS, TM, DP, RH
      REAL,dimension(IM,jsta:jend),intent(inout) :: RH3310, RH6610, RH3366
      REAL,dimension(IM,jsta:jend),intent(inout) :: PW3310
      real Z3310,Z6610,Z3366,P10,P33,P66
      integer I,J,L,LLMH
!
!***********************************************************************
!     START LFMFLD HERE
!     
!
!     LOOP OVER HORIZONTAL GRID.
!     
      DO 30 J=JSTA,JEND
      DO 30 I=1,IM
!     
!        ZERO VARIABLES.
         RH3310(I,J) = D00
         PW3310(I,J) = D00
         RH6610(I,J) = D00
         RH3366(I,J) = D00
         Z3310     = D00
         Z6610     = D00
         Z3366     = D00
!     
!        SET BOUNDS FOR PRESSURES AND SURFACE L.
         P10  = PINT(I,J,NINT(LMH(I,J)))
         P66  = 0.75*P10
         P33  = 0.50*P10
         LLMH = NINT(LMH(I,J))
!     
!        ACCULMULATE RELATIVE HUMIDITIES AND PRECIPITABLE WATER.
!
         DO 10 L = LLMH,1,-1
!     
!           GET P, Z, T, AND Q AT MIDPOINT OF ETA LAYER.
            ALPM = D50*(ALPINT(I,J,L)+ALPINT(I,J,L+1))
            DZ   = ZINT(I,J,L)-ZINT(I,J,L+1)
            DP   = PINT(I,J,L+1)-PINT(I,J,L)
            PM   = EXP(ALPM)
            TM   = T(I,J,L)
            QM   = Q(I,J,L)
            QM   = AMAX1(QM,D00)
!
!            QS=PQ0/PM*EXP(A2*(TM-A3)/(TM-A4))
	    IF(MODELNAME == 'GFS')THEN
	      ES = min(FPVSNEW(TM),PM)
	      QS = CON_EPS*ES/(PM+CON_EPSM1*ES)
	    ELSE      
              QS=PQ0/PM*EXP(A2*(TM-A3)/(TM-A4))
	    END IF
            RH   = QM/QS
            IF (RH.GT.H1) THEN
               RH = H1
               QM = RH*QS
            ENDIF
            IF (RH.LT.D01) THEN
               RH = D01
               QM = RH*QS
            ENDIF
!
!           JUMP OUT OF THIS LOOP IF WE ARE ABOVE THE HIGHEST TARGET PRESSURE.
            IF (PM.LE.P33) GOTO 20
!     
!           0.66-1.00 RELATIVE HUMIDITY.
            IF ((PM.LE.P10).AND.(PM.GE.P66)) THEN
               Z6610     = Z6610 + DZ
               RH6610(I,J) = RH6610(I,J) + RH*DZ
            ENDIF
!     
!           0.33-1.00 RELATIVE HUMIDITY AND PRECIPITABLE WATER.
            IF ((PM.LE.P10).AND.(PM.GE.P33)) THEN
               Z3310      = Z3310 + DZ
               RH3310(I,J)= RH3310(I,J)+RH*DZ
               PW3310(I,J)= PW3310(I,J)+(Q(I,J,L)+CWM(I,J,L))*DP*GI
            ENDIF
!     
!           0.33-0.66 RELATIVE HUMIDITY.
            IF ((PM.LE.P66).AND.(PM.GE.P33)) THEN
               Z3366     = Z3366 + DZ
               RH3366(I,J) = RH3366(I,J) + RH*DZ
            ENDIF
!
 10      CONTINUE
 20      CONTINUE
!     
!        NORMALIZE TO GET MEAN RELATIVE HUMIDITIES.  AT
!        ONE TIME WE DIVIDED PRECIPITABLE WATER BY DENSITY
!        TO GET THE EQUIVALENT WATER DEPTH IN METERS.  NO MORE.
         IF (Z6610.GT.D00) THEN
            RH6610(I,J) = RH6610(I,J)/Z6610
         ELSE
            RH6610(I,J) = SPVAL
         ENDIF
!     
         IF (Z3310.GT.D00) THEN
            RH3310(I,J) = RH3310(I,J)/Z3310
         ELSE
            RH3310(I,J) = SPVAL
         ENDIF
!     
         IF (Z3366.GT.D00) THEN
            RH3366(I,J) = RH3366(I,J)/Z3366
         ELSE
            RH3366(I,J) = SPVAL
         ENDIF
 30   CONTINUE
!     
!     
!     END OF ROUTINE.
!     
      RETURN
      END
