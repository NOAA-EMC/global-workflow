      SUBROUTINE LFMFLD_GFS(RH4410,RH7294,RH4472,RH3310)
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
!   06-11-06 H CHUANG      - MODIFY TO OUTPUT GFS LFM FIELDS WHICH 
!                            HAVE DIFFERENT THICKNESS AS MESO AND USE DP
!                            RATHER THAN DZ 
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
      use vrbls3d, only: pint, q, t, pmid
      use masks, only: lmh
      use params_mod, only: d00
      use ctlblk_mod, only: jsta, jend, spval, im
!     
    implicit none
!
      real,PARAMETER :: RHOWAT=1.E3
      real,parameter:: con_rd      =2.8705e+2 ! gas constant air    (J/kg/K)
      real,parameter:: con_rv      =4.6150e+2 ! gas constant H2O
      real,parameter:: con_eps     =con_rd/con_rv
      real,parameter:: con_epsm1   =con_rd/con_rv-1
      real,parameter:: strh1=0.44,strh2=0.72,strh3=0.44,strh4=0.33 &
                      ,sbrh1=1.00,sbrh2=0.94,sbrh3=0.72,sbrh4=1.00
!     
!     DECLARE VARIABLES.
!     
      REAL ALPM, DZ, ES, PM, PWSUM, QM, QS
      REAL,dimension(IM,jsta:jend),intent(out) :: RH4410, RH7294, RH4472    &
                                                 ,RH3310    
!
      integer I,J,L,LLMH
      real P4410, P7294,P4472,P3310,Q4410,Q7294,Q4472,Q3310,QS4410, &
         QS7294,QS4472,QS3310,PS,P33,DP1,DP2,DP3,DP4
      real,external :: fpvsnew

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
         RH4410(I,J) = D00
         RH4472(I,J) = D00
         RH7294(I,J) = D00
         RH3310(I,J) = D00
         P4410       = D00
         P7294       = D00
         P4472       = D00
         P3310       = D00
         Q4410       = D00
         Q7294       = D00
         Q4472       = D00
         Q3310       = D00
         QS4410      = D00
         QS7294      = D00
         QS4472      = D00
         QS3310      = D00
!     
!        SET BOUNDS FOR PRESSURES AND SURFACE L.
 
         LLMH = NINT(LMH(I,J))
	 PS=PINT(I,J,LLMH+1)
	 P33  = 0.33*PS
!     
!        ACCULMULATE RELATIVE HUMIDITIES AND PRECIPITABLE WATER.
!
         DO 10 L = LLMH,1,-1
!     
!           GET P, Z, T, AND Q AT MIDPOINT OF ETA LAYER.
            
            DP1 = MAX(MIN(PINT(I,J,L+1),SBRH1*PS)      &
                     -MAX(PINT(I,J,L),STRH1*PS),0.)            
            DP2 = MAX(MIN(PINT(I,J,L+1),SBRH2*PS)      &
                     -MAX(PINT(I,J,L),STRH2*PS),0.)
            DP3 = MAX(MIN(PINT(I,J,L+1),SBRH3*PS)      &
                     -MAX(PINT(I,J,L),STRH3*PS),0.)
            DP4 = MAX(MIN(PINT(I,J,L+1),SBRH4*PS)      &
                     -MAX(PINT(I,J,L),STRH4*PS),0.)
     
            PM   = PINT(I,J,L)
            QM   = Q(I,J,L)
            QM   = MAX(QM,D00)
            ES   = min(FPVSNEW(T(I,J,L)),PMID(I,J,L))
            QS=CON_EPS*ES/(PMID(I,J,L)+CON_EPSM1*ES)
!
!
!           JUMP OUT OF THIS LOOP IF WE ARE ABOVE THE HIGHEST TARGET PRESSURE.
            IF (PM.LE.P33) GOTO 20
!     
!           0.44-1.00 RELATIVE HUMIDITY.
!            IF ((PM.LE.P10).AND.(PM.GE.P44)) THEN
               P4410     = P4410 + DP1
               Q4410     = Q4410 + QM*DP1
               QS4410    = QS4410+ QS*DP1
!            ENDIF
!     
!           0.33-1.00 RELATIVE HUMIDITY 
!            IF ((PM.LE.P10).AND.(PM.GE.P33)) THEN
               P3310      = P3310 + DP4
               Q3310     = Q3310 + QM*DP4
               QS3310    = QS3310+ QS*DP4
!            ENDIF
!     
!           0.44-0.72 RELATIVE HUMIDITY.
!            IF ((PM.LE.P66).AND.(PM.GE.P33)) THEN
               P4472     = P4472 + DP3
               Q4472     = Q4472 + QM*DP3
               QS4472    = QS4472+ QS*DP3
!            ENDIF
!           0.72-0.94 RELATIVE HUMIDITY.
!            IF ((PM.LE.P66).AND.(PM.GE.P33)) THEN
               P7294     = P7294 + DP2
               Q7294     = Q7294 + QM*DP2
               QS7294    = QS7294+ QS*DP2
!            ENDIF
!
 10      CONTINUE
 20      CONTINUE
!     
!        NORMALIZE TO GET MEAN RELATIVE HUMIDITIES.  AT
!        ONE TIME WE DIVIDED PRECIPITABLE WATER BY DENSITY
!        TO GET THE EQUIVALENT WATER DEPTH IN METERS.  NO MORE.
         IF (P4410.GT.D00) THEN
            RH4410(I,J) = Q4410/QS4410
         ELSE
            RH4410(I,J) = SPVAL
         ENDIF
!     
         IF (P3310.GT.D00) THEN
            RH3310(I,J) = Q3310/QS3310
         ELSE
            RH3310(I,J) = SPVAL
         ENDIF
!     
         IF (P4472.GT.D00) THEN
            RH4472(I,J) = Q4472/QS4472
         ELSE
            RH4472(I,J) = SPVAL
         ENDIF

         IF (P7294.GT.D00) THEN
            RH7294(I,J) = Q7294/QS7294
         ELSE
            RH7294(I,J) = SPVAL
         ENDIF
 30   CONTINUE
!     
!     END OF ROUTINE.
!     
      RETURN
      END
