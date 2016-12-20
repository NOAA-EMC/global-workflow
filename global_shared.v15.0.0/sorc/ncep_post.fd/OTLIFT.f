      SUBROUTINE OTLIFT(SLINDX)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    OTLIFT      COMPUTES SFC TO 500MB LIFTED INDEX
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-03-10       
!     
! ABSTRACT:
!     THIS ROUTINE COMPUTES A SURFACE TO 500MB LIFTED INDEX.
!     THE LIFTED PARCEL IS FROM THE FIRST ATMOSPHERIC ETA 
!     LAYER (IE, THE ETA LAYER CLOSEST TO THE MODEL GROUND).
!     THE LIFTED INDEX IS THE DIFFERENCE BETWEEN THIS PARCEL'S
!     TEMPERATURE AT 500MB AND THE AMBIENT 500MB TEMPERATURE.
!   .     
!     
! PROGRAM HISTORY LOG:
!   ??-??-??  ??? - SUBROUTINE OTLIFT IN ETA MODEL.
!   93-03-10  RUSS TREADON - ADAPTED OTLIFT FOR USE WITH NEW POST. 
!   98-06-18  T BLACK      - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION
!   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-06-11  MIKE BALDWIN - WRF VERSION
!   11-04-12  GEOFF MANIKIN - USE VIRTUAL TEMPERATURE
!     
! USAGE:    CALL OTLIFT(SLINDX)
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST: 
!     SLINDX   - LIFTED INDEX.
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - CTLBLK
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
      use vrbls3d,    only: PMID, T, Q
      use vrbls2d,    only: T500
      use masks,      only: LMH
      use lookup_mod, only: THL, RDTH, JTB, QS0, SQS, RDQ,ITB, PTBL, PL, &
                            RDP, THE0, STHE, RDTHE, TTBL
      use ctlblk_mod, only: JSTA, JEND, IM
      use params_mod, only: D00,H10E5, CAPA, ELOCP, EPS, ONEPS
!

!
      implicit none
!
!     SET LOCAL PARAMETERS.
       real,PARAMETER :: D8202=.820231E0 , H5E4=5.E4 , P500=50000.
       real,external::FPVSNEW

!     
!     DECLARE VARIABLES.
      real,intent(out) :: SLINDX(IM,jsta:jend)
      REAL :: TVP, ESATP, QSATP
      REAL :: TTH, TP, APESP, PARTMP, THESP, TPSP
      REAL :: BQS00, SQS00, BQS10, SQS10, BQ, SQ, TQ
      REAL :: P00, P10, P01, P11, T00, T10, T01, T11
      REAL :: BTHE00, STHE00, BTHE10, STHE10, BTH, STH
      REAL :: TQQ, QQ, QBT, TTHBT, TBT, APEBT, PPQ, PP
!
      INTEGER :: I, J, LBTM, ITTBK, IQ, IT, IPTBK, ITH, IP, IQTB
      INTEGER :: ITTB, IPTB, ITHTB
!     
!***********************************************************************
!     START OTLIFT HERE
!     
!     INTIALIZE LIFTED INDEX ARRAY TO ZERO.
!$omp parallel do private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
          SLINDX(I,J) = D00
        ENDDO
      ENDDO
!--------------FIND EXNER AT LOWEST LEVEL-------------------------------
      DO J=JSTA,JEND
        DO I=1,IM
          LBTM=NINT(LMH(I,J))
          TBT = T(I,J,LBTM)
          QBT = Q(I,J,LBTM)
          APEBT = (H10E5/PMID(I,J,LBTM))**CAPA
!--------------SCALING POTENTIAL TEMPERATURE & TABLE INDEX--------------
          TTHBT = TBT*APEBT
          TTH = (TTHBT-THL)*RDTH
          TQQ = TTH-AINT(TTH)
          ITTB = INT(TTH)+1
!--------------KEEPING INDICES WITHIN THE TABLE-------------------------
          IF(ITTB .LT. 1)THEN
            ITTB = 1
            TQQ = D00
          ENDIF
            IF(ITTB .GE. JTB)THEN
            ITTB = JTB-1
            TQQ = D00
          ENDIF
!--------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY---------------
          ITTBK = ITTB
          BQS00=QS0(ITTBK)
          SQS00=SQS(ITTBK)
          BQS10=QS0(ITTBK+1)
          SQS10=SQS(ITTBK+1)
!--------------SCALING SPEC. HUMIDITY & TABLE INDEX---------------------
          BQ=(BQS10-BQS00)*TQQ+BQS00
          SQ=(SQS10-SQS00)*TQQ+SQS00
          TQ=(QBT-BQ)/SQ*RDQ
          PPQ = TQ-AINT(TQ)
          IQTB = INT(TQ)+1
!--------------KEEPING INDICES WITHIN THE TABLE-------------------------
          IF(IQTB .LT. 1)THEN
            IQTB = 1
            PPQ = D00
          ENDIF
          IF(IQTB .GE. ITB)THEN
            IQTB = ITB-1
            PPQ = D00
          ENDIF
!--------------SATURATION PRESSURE AT FOUR SURROUNDING TABLE PTS.-------
          IQ=IQTB
          IT = ITTB
          P00=PTBL(IQ,IT)
          P10=PTBL(IQ+1,IT)
          P01=PTBL(IQ,IT+1)
          P11=PTBL(IQ+1,IT+1)
!--------------SATURATION POINT VARIABLES AT THE BOTTOM-----------------
          TPSP = P00+(P10-P00)*PPQ+(P01-P00)*TQQ       &
                +(P00-P10-P01+P11)*PPQ*TQQ
          IF(TPSP .LE. D00) TPSP = H10E5
          APESP = (H10E5/TPSP)**CAPA
          THESP = TTHBT*EXP(ELOCP*QBT*APESP/TTHBT)
!--------------SCALING PRESSURE & TT TABLE INDEX------------------------
          TP=(H5E4-PL)*RDP
          QQ = TP-AINT(TP)
          IPTB = INT(TP)+1
!--------------KEEPING INDICES WITHIN THE TABLE-------------------------
          IF(IPTB .LT. 1)THEN
            IPTB = 1
            QQ = D00
          ENDIF
          IF(IPTB .GE. ITB)THEN
            IPTB = ITB-1
            QQ = D00
          ENDIF
!--------------BASE AND SCALING FACTOR FOR THE--------------------------
          IPTBK=IPTB
          BTHE00=THE0(IPTBK)
          STHE00=STHE(IPTBK)
          BTHE10=THE0(IPTBK+1)
          STHE10=STHE(IPTBK+1)
!--------------SCALING THE & TT TABLE INDEX-----------------------------
          BTH=(BTHE10-BTHE00)*QQ+BTHE00
          STH=(STHE10-STHE00)*QQ+STHE00
          TTH=(THESP-BTH)/STH*RDTHE
          PP = TTH-AINT(TTH)
          ITHTB = INT(TTH)+1
!--------------KEEPING INDICES WITHIN THE TABLE-------------------------
          IF(ITHTB .LT. 1)THEN
            ITHTB = 1
            PP = D00
          ENDIF
          IF(ITHTB .GE. JTB)THEN
            ITHTB = JTB-1
            PP = D00
          ENDIF
!--------------TEMPERATURE AT FOUR SURROUNDING TT TABLE PTS.------------
          ITH=ITHTB
          IP=IPTB
          T00=TTBL(ITH,IP)
          T10=TTBL(ITH+1,IP)
          T01=TTBL(ITH,IP+1)
          T11=TTBL(ITH+1,IP+1)
!--------------PARCEL TEMPERATURE AT 500MB----------------------------
          IF(TPSP .GE. H5E4)THEN
            PARTMP=(T00+(T10-T00)*PP+(T01-T00)*QQ    &
                +(T00-T10-T01+T11)*PP*QQ)
          ELSE
            PARTMP=TBT*APEBT*D8202
          ENDIF
!--------------LIFTED INDEX---------------------------------------------
!
! GSM  THE PARCEL TEMPERATURE AT 500 MB HAS BEEN COMPUTED, AND WE
!       FIND THE MIXING RATIO AT THAT LEVEL WHICH WILL BE THE SATURATION
!       VALUE SINCE WE'RE FOLLOWING A MOIST ADIABAT.    NOTE THAT THE
!       AMBIENT 500 MB SHOULD PROBABLY BE VIRTUALIZED, BUT THE IMPACT
!       OF MOISTURE AT THAT LEVEL IS QUITE SMALL

         ESATP=FPVSNEW(PARTMP)
         QSATP=EPS*ESATP/(P500-ESATP*ONEPS)
         TVP=PARTMP*(1+0.608*QSATP)
         SLINDX(I,J)=T500(I,J)-TVP
        ENDDO
      ENDDO
!       write(*,*) ' in otlift t500 partmp ',t500(1,1),partmp(1,1)
!       write(*,*) ' in otlift tbt ',tbt(1,1)
!
      RETURN
      END
