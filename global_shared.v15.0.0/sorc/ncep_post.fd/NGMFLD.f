      SUBROUTINE NGMFLD(RH4710,RH4796,RH1847,RH8498,QM8510)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    NGMFLD      COMPUTES LAYER MEAN NGM FIELDS
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22
!     
! ABSTRACT:
!     THIS ROUTINE COMPUTES A HANDFUL OF NGM LAYER MEAN 
!     FIELDS.  THIS IS DONE TO PROVIDE A FULLY COMPLETE 
!     ETA NGM LOOK-ALIKE OUTPUT FILE.  THE SIGMA (LAYER)
!     FIELDS COMPUTED BY THIS ROUTINE ARE TABULATED BELOW.
!     
!           SIGMA (LAYER)         FIELD(S)
!          ---------------     --------------
!          0.47191-1.00000          RH
!          0.47171-0.96470          RH
!          0.18019-0.47191          RH
!          0.84368-0.98230          RH
!          0.85000-1.00000         MCONV
!     WHERE 
!          RH    = RELATIVE HUMIDITY
!          MCONV = MOISTURE CONVERGENCE
!
!     LAYER MEANS ARE A SUMMATION OVER ETA LAYERS MAPPING INTO
!     THE PRESSURE RANGE CORRESPONDING TO THE SIGMA RANGE ABOVE.
!     THE CALCULATION OF THESE BOUNDING PRESSURES IS DONE AT 
!     EACH HORIZONTAL GRID POINT BASED ON THE SURFACE PRESSURE.
!     EACH TERM IN THE SUMMATION IS WEIGHTED BY THE THICKNESS OF
!     THE ETA LAYER.  THE FINAL LAYER MEAN IS THIS SUM NORMALIZED
!     BY THE TOTAL DEPTH OF THE LAYER.

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
!   98-06-16  T BLACK      - CONVERSION FROM 1-D TO 2-D
!   98-08-18  MIKE BALDWIN - COMPUTE RH OVER ICE
!   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
!   00-01-04  JIM TUCCILLO - MPI VERSION
!   02-04-24  MIKE BALDWIN - WRF VERSION
!     
!     
! USAGE:    CALL NGMFLD(RH4710,RH4796,RH1847,RH8498,QM8510)
!   INPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT ARGUMENT LIST: 
!     RH4710   - SIGMA LAYER 0.47-1.00 MEAN RELATIVE HUMIDITY.
!     RH4796   - SIGMA LAYER 0.47-0.96 MEAN RELATIVE HUMIDITY.
!     RH1847   - SIGMA LAYER 0.18-0.47 MEAN RELATIVE HUMIDITY.
!     RH8498   - SIGMA LAYER 0.84-0.98 MEAN RELATIVE HUMIDITY.
!     QM8510   - SIGMA LAYER 0.85-1.00 MEAN MOISTURE CONVERGENCE.
!     
!   OUTPUT FILES:
!     NONE
!     
!   LIBRARY:
!     COMMON   - 
!                MASKS
!                OPTIONS
!                LOOPS
!                MAPOT
!                DYNAMD
!                INDX
!
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
!     
!     INCLUDE PARAMETERS
      use vrbls3d,    only: q, uh, vh, pint, alpint, zint, t
      use masks,      only: lmh
      use params_mod, only: d00, d50, h1m12, pq0, a2, a3, a4, h1, d01, small
      use ctlblk_mod, only: jsta, jend, lm, jsta_2l, jend_2u, jsta_m2, jend_m2,&
                            spval, im
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!
      real,PARAMETER :: SIG100=1.00000, SIG98=0.98230, SIG96=0.96470
      real,PARAMETER :: SIG89 =0.89671, SIG85=0.85000, SIG84=0.84368
      real,PARAMETER :: SIG78 =0.78483, SIG47=0.47191, SIG18=0.18018
!     
!     DECLARE VARIABLES.
      LOGICAL GOT8510,GOT4710,GOT4796,GOT1847,GOT8498
      REAL,dimension(IM,jsta_2l:jend_2u),intent(out) :: QM8510,RH4710,RH8498, &
                                                        RH4796,RH1847
      REAL,dimension(im,jsta_2l:jend_2u) :: Z8510,Z4710,Z8498,Z4796,Z1847
      real,dimension(im,jsta_2l:jend_2u) ::  Q1D, U1D, V1D, QCNVG
!
      integer I,J,L
      real P100,P85,P98,P96,P84,P47,P18,ALPM,DE,PM,TM,QM,     &
           QMCVG,QS,RH,DZ
!********************************************************************
!     START NGMFLD HERE.
!     
!     INITIALIZE ARRAYS.
!$omp  parallel do private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
           QM8510(I,J) = D00
           RH4710(I,J) = D00
           RH8498(I,J) = D00
           RH4796(I,J) = D00
           RH1847(I,J) = D00
           Z8510(I,J)  = D00
           Z8498(I,J)  = D00
           Z4710(I,J)  = D00
           Z4796(I,J)  = D00
           Z1847(I,J)  = D00
        ENDDO
      ENDDO
!     
!     LOOP OVER HORIZONTAL GRID.
!     
!!$omp  parallel do                                                 &
!     & private(dz,p100,p18,p47,p84,p85,                            &
!     &         p96,p98,pm,qdiv,qk,qkhn,qkhs,qkm1,qm,qm8510,        &
!     &         qmcvg,qs,qudx,qvdy,r2dx,r2dy,rh,rh1847,rh4710,      &
!     &         rh4796,rh8498,tm,tmt0,tmt15,z1847,z4710,z4796,      &
!     &         z8498,z8510,q1d,u1d,v1d,qcnvg)

     DO L=1,LM
!          COMPUTE MOISTURE CONVERGENCE
!$omp parallel do private(i,j)
       DO J=JSTA_2L,JEND_2U
         DO I=1,IM
           Q1D(I,J) = Q(I,J,L)
           U1D(I,J) = UH(I,J,L)
           V1D(I,J) = VH(I,J,L)
         ENDDO
       ENDDO
       CALL CALMCVG(Q1D,U1D,V1D,QCNVG)
!          COMPUTE MOISTURE CONVERGENCE
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
!
!        SET TARGET PRESSURES.
         
         P100  = PINT(I,J,NINT(LMH(I,J)))
         P98   = SIG98*P100
         P96   = SIG96*P100
         P85   = SIG85*P100
         P84   = SIG84*P100
         P47   = SIG47*P100
         P18   = SIG18*P100
!     
!     
!        COMPUTE LAYER MEAN FIELDS AT THE GIVEN K.
!
!          COMPUTE P, Z, T, AND Q AT THE MIDPOINT OF THE CURRENT ETA LAYER.
           ALPM = D50*(ALPINT(I,J,L)+ALPINT(I,J,L+1))
           DZ   = ZINT(I,J,L)-ZINT(I,J,L+1)
           PM   = EXP(ALPM)
           TM   = T(I,J,L)
           QM   = Q(I,J,L)
           QM   = AMAX1(QM,H1M12)
           QMCVG= QCNVG(I,J)
!
!     
!          COMPUTE RELATIVE HUMIDITY.
!
           QS=PQ0/PM*EXP(A2*(TM-A3)/(TM-A4))
!
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
!          SIGMA 0.85-1.00 MOISTURE CONVERGENCE.
           IF ((PM.LE.P100).AND.(PM.GE.P85)) THEN
              Z8510(I,J)  = Z8510(I,J) + DZ
              QM8510(I,J) = QM8510(I,J) + QMCVG*DZ
           ENDIF
!    
!          SIGMA 0.47-1.00 RELATIVE HUMIDITY.
           IF ((PM.LE.P100).AND.(PM.GE.P47)) THEN
              Z4710(I,J)  = Z4710(I,J) + DZ
              RH4710(I,J) = RH4710(I,J) + RH*DZ
           ENDIF
!
!          SIGMA 0.84-0.98 RELATIVE HUMIDITY.
           IF ((PM.LE.P98).AND.(PM.GE.P84)) THEN
              Z8498(I,J)  = Z8498(I,J) + DZ
              RH8498(I,J) = RH8498(I,J) + RH*DZ
           ENDIF
!     
!          SIGMA 0.47-0.96 RELATIVE HUMIDITY.
           IF ((PM.LE.P96).AND.(PM.GE.P47)) THEN
              Z4796(I,J)  = Z4796(I,J) + DZ
              RH4796(I,J) = RH4796(I,J) + RH*DZ
           ENDIF
!     
!          SIGMA 0.18-0.47 RELATIVE HUMIDITY.
           IF ((PM.LE.P47).AND.(PM.GE.P18)) THEN
              Z1847(I,J)  = Z1847(I,J) + DZ
              RH1847(I,J) = RH1847(I,J) + RH*DZ
           ENDIF
!
      ENDDO
      ENDDO
      ENDDO
!     
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
!        NORMALIZE TO GET LAYER MEAN VALUES.
         IF (Z8510(I,J).GT.0) THEN
            QM8510(I,J) = QM8510(I,J)/Z8510(I,J)
         ELSE
            QM8510(I,J) = SPVAL
         ENDIF
         IF (ABS(QM8510(I,J)-SPVAL).LT.SMALL)QM8510(I,J)=H1M12
!
         IF (Z4710(I,J).GT.0) THEN
            RH4710(I,J) = RH4710(I,J)/Z4710(I,J)
         ELSE
            RH4710(I,J) = SPVAL
         ENDIF
!
         IF (Z8498(I,J).GT.0) THEN
            RH8498(I,J) = RH8498(I,J)/Z8498(I,J)
         ELSE
            RH8498(I,J) = SPVAL
         ENDIF
!
         IF (Z4796(I,J).GT.0) THEN
            RH4796(I,J) = RH4796(I,J)/Z4796(I,J)
         ELSE
            RH4796(I,J) = SPVAL
         ENDIF
!
         IF (Z1847(I,J).GT.0) THEN
            RH1847(I,J) = RH1847(I,J)/Z1847(I,J)
         ELSE
            RH1847(I,J) = SPVAL
         ENDIF
      ENDDO
      ENDDO
!
!     
!     END OF ROUTINE.
!     
      RETURN
      END

