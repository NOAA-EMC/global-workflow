      SUBROUTINE LRGSCL(IX,IM,KM,DT,T1,Q1,PRSL,DEL,PRSLK,RAIN,CLW)
!
      USE MACHINE     , ONLY : kind_phys
      USE FUNCPHYS , ONLY : fpvs, ftdp, fthe, stma, ftlcl
      USE PHYSCONS, HVAP => con_HVAP, CP => con_CP, RV => con_RV
     &,             EPS => con_eps, EPSM1 => con_epsm1, ROCP => con_ROCP
     &,             grav => con_g
      implicit none
!
!     include 'constant.h'
!
      integer IX , IM, KM
      real(kind=kind_phys) T1(IX,KM),  Q1(IX,KM),    PRSL(IX,KM),
     &                     DEL(IX,KM), PRSLK(IX,KM), RAIN(IM),
     &                     CLW(IM,KM), DT
!
!  LOCAL VARIABLES
!
      integer              k, kmax, I
      real(kind=kind_phys) dpovg,  EI,    el2orc,
     &                     elocp,
     &                     pk,     qcond, qevap,
     &                     rnevap, SLKLCL,TDPD,
     &                     THELCL, TLCL,  val0,  val1
!
!
!  PHYSICAL PARAMETERS
      PARAMETER(ELOCP=HVAP/CP,   EL2ORC=HVAP*HVAP/(RV*CP))
!
!
      real(kind=kind_phys) TO(IM,KM),   QO(IM,KM),   QS(IM,KM),
     &                     THE(IM,KM),  DQ(IM,KM),   RAINLVL(IM,KM),
     &                     ES(IM,KM),   DQINT(IM),   PINT(IM),
     &                     DELQBAR(IM), DELTBAR(IM), THEBAR(IM),
     &                     THEINT(IM)
      integer              KMLEV(IM,KM), KE(IM), KK(IM), KS(IM)
      LOGICAL FLG(IM), TOPFLG(IM), TOTFLG
cc
cc--------------------------------------------------------------------
cc
      real(kind=kind_phys) cons_0               !constant
      real(kind=kind_phys) cons_1pdm8           !constant
cc
      cons_0          =         0.d0            !constant
      cons_1pdm8      =         1.d-8           !constant
cc
cc--------------------------------------------------------------------
cc
      KMAX = KM
      DO K = 1, KM
        do i=1,im
          IF (PRSL(I,K) .GT. 6000.0) KMAX = K + 1
        enddo
      ENDDO
C
C   SURFACE PRESSURE UNIT IS CB
C
      DO I=1,IM
!       PSK(I)     = FPKAP(PS(I))
        RAIN(I)    = 0.
        DELTBAR(I) = 0.
        DELQBAR(I) = 0.
        FLG(I)     = .FALSE.
        TOPFLG(I)  = .FALSE.
        KE(I)      = kmax + 1
        KS(I)      = 0
      ENDDO
      TOTFLG = .FALSE.
C
C  COLUMN VARIABLES
C  PRSL IS PRESSURE OF THE LAYER (Pa)
C  TO IS TEMPERATURE AT T+DT (K)... THIS IS AFTER ADVECTION AND TURBULAN
C  QO IS MIXING RATIO AT T+DT (KG/KG)..Q1
C
      DO K = 1, KMAX
       DO I=1,IM
!       PFLD(I,k) = PS(I) * SL(K)
        TO(I,k) = T1(I,k)
        QO(I,k) = Q1(I,k)
        ENDDO
      ENDDO
C
C  MODEL CONSISTENT SATURATION MIXING RATIO
C
!     es(:,:) = 0.001 * fpvs(t1(1:IM,:))        ! fpvs in Pa
      DO K = 1, KMAX
        DO I=1,IM
          es(I,k) = min(PRSL(I,k), fpvs(t1(I,k)))   ! fpvs in Pa
          QS(I,k) = EPS * ES(I,k) / (PRSL(I,k) + EPSM1*ES(I,k))
          QS(I,k) = MAX(QS(I,k),cons_1pdm8)     !constant
        ENDDO
      ENDDO
      DO K = 1, KMAX
        DO I=1,IM
          IF(QO(I,k).GT.QS(I,k)) FLG(I) = .TRUE.
        ENDDO
      ENDDO
!!
      DO I=1,IM
        IF(FLG(I)) TOTFLG = .TRUE.
      ENDDO
      IF(.NOT.TOTFLG) RETURN
!!
      DO K = 1, KMAX
        DO I = 1, IM
          DQ(I,k) = 0.
          THE(I,k) = TO(I,k)
        ENDDO
      ENDDO
C
C  COMPUTE THETA-E
C
      DO K = 1, KMAX
        DO I = 1, IM
          IF(FLG(I)) THEN
!           PK = PSK(I) * SLK(K)
            PK = PRSLK(I,K)
            THE(I,k) = FTHE(TO(I,k),PK)
            IF(THE(I,k).EQ.0.) THEN
              THE(I,k) = TO(I,k) / PK
            ENDIF
C         THE(I,k) = TO(I,k) * ((PRSL(I,k)-ES(I,k))*.01) ** (-ROCP)
C    &             * EXP(ELOCP * QS(I,k) / TO(I,k))
            DQ(I,k) = QO(I,k)- QS(I,k)
C
C  MODIFICATION OF THETA-E FOR SUPER-SATURATION
C
            THE(I,k)= THE(I,k) * (1. + HVAP*MAX(DQ(I,k),cons_0)     !constant
     &              /(CP*TO(I,k)))
          ENDIF
        ENDDO
      ENDDO
      DO K = 1, KMAX
        DO I = 1, IM
          KMLEV(I,k) = 0
          RAINLVL(I,k) = 0.
        ENDDO
      ENDDO
C
C  STARTING POINT OF ADJUSTMENT
C
      K = 1
      DO I = 1, IM
        KK(I)     = 0
        DQINT(I)  = 0.
        THEINT(I) = 0.
        THEBAR(I) = 0.
        PINT(I)   = 0.
C
C  FOR CONDITIONALLY UNSTABLE AND SUPERSATURATED LAYERS,
C    OBTAIN INTEGRATED THETA AND Q-QS
C
C  KMLEV KEEPS TRACK OF THE NUMBER OF LAYERS THAT SATISFIES
C    THE CONDITION FOR ADJUSTMENT
C
        IF(DQ(I,k).GT.0..AND.THE(I,k).GE.THE(I,K+1).AND.FLG(I)) THEN
          DQINT(I)   = DQINT(I)  + DQ(I,k) * DEL(I,K)
          THEINT(I)  = THEINT(I) + THE(I,k) * DEL(I,K)
          PINT(I)    = PINT(I)   + DEL(I,K)
          KK(I)      = KK(I)     + 1
          KMLEV(I,k) = KK(I)
        ENDIF
      ENDDO
      DO K = 2, KMAX - 1
        DO I = 1, IM
          IF(DQ(I,k).GT.0..AND.THE(I,k).GE.THE(I,K+1).AND.FLG(I)) THEN
            DQINT(I)   = DQINT(I)  + DQ(I,k) * DEL(I,K)
            THEINT(I)  = THEINT(I) + THE(I,k) * DEL(I,K)
            PINT(I)    = PINT(I)   + DEL(I,K)
            KK(I)      = KK(I)     + 1
            KMLEV(I,k) = KK(I)
          ENDIF
        ENDDO
        DO I = 1, IM
          IF(PINT(I).GT.0.)THEBAR(I) = THEINT(I) / PINT(I)
C
C  IF THE LAYER BELOW SATISFIES THE CONDITION AND THE PRESENT
C    LAYER IS COLDER THAN THE ADJSUTED THETA-E,
C    THE LAYER IS INCLUDED IF THE INTEGRATED MOISTURE EXCESS
C    CAN BE MAINTAINED
C
          IF(KMLEV(I,k).EQ.0.AND.KMLEV(I,K-1).GT.0.AND.
     &       THEBAR(I).GE.THE(I,k).AND..NOT.TOPFLG(I)) THEN
               DQINT(I) = DQINT(I) + DQ(I,k) * DEL(I,K)
          ENDIF
          IF(KMLEV(I,k).EQ.0.AND.KMLEV(I,K-1).GT.0.AND.
     &       THEBAR(I).GE.THE(I,k).AND.DQINT(I).GT.0.
     &       .AND..NOT.TOPFLG(I)) THEN
            KK(I)      = KK(I) + 1
            KMLEV(I,k) = KK(I)
            TOPFLG(I) = .TRUE.
!           PK        = PSK(I) * SLK(K)
            EI        = PRSL(I,k) * QO(I,k)
     &                                     / (EPS - EPSM1*QO(I,k))
            EI        = MIN(MAX(EI,cons_1pdm8),ES(I,k))    !constant
            TDPD      = MAX(TO(I,k)-FTDP(EI),cons_0)       !constant
            TLCL      = FTLCL(TO(I,k), TDPD)
            SLKLCL    = PRSLK(I,K) * TLCL / TO(I,k)
            THELCL    = FTHE(TLCL,SLKLCL)
            IF(THELCL.NE.0.) THEN
              THE(I,k) = THELCL
C             THE(I,k) = TO(I,k) * ((PRSL(I,k) - EI)*.01) ** (-ROCP)
C    &             * EXP(ELOCP * MAX(QO(I,k),1.E-8) / TO(I,k))
            ENDIF
            THEINT(I) = THEINT(I) + THE(I,k) * DEL(I,K)
            PINT(I)   = PINT(I) + DEL(I,K)
          ENDIF
        ENDDO
C
C  RESET THE INTEGRAL IF THE LAYER IS NOT IN THE CLOUD
C
        DO I = 1, IM
          IF(KMLEV(I,k).EQ.0.AND.KMLEV(I,K-1).GT.0) THEN
            THEBAR(I) = THEINT(I) / PINT(I)
            DQINT(I) = 0.
            THEINT(I) = 0.
            PINT(I) = 0.
            KK(I) = 0
            KS(I) = k - 1
            KE(I) = KS(I) - KMLEV(I,k-1) + 1
            FLG(I) = .false.
          ENDIF
        ENDDO
      enddo
C
C  When within A CLOUD LAYER, COMPUTE THE MOIST-ADIABATIC
C    (TO AND QO) USING THE AVERAGED THETA-E AND THE RESULTANT RAIN
C
      do k = 1, kmax
        DO I = 1, IM
          if(k.ge.KE(I).and.k.le.KS(I)) then
!           PK = PSK(I) * SLK(K)
            PK = PRSLK(I,K)
!           TO(I,k)  = FTMA(THEBAR(I),PK,QO(I,k))
            CALL STMA(THEBAR(i),PK,TO(I,k),QO(I,k))
            THE(I,k) = THEBAR(I)
            QS(I,k)  = QO(I,k)
            DPOVG    = DEL(I,K) * (1.0/grav)
            RAINLVL(I,k) = (Q1(I,k) - QO(I,k)) * dpovg
            DELTBAR(I)   = DELTBAR(I) + (TO(I,k) - T1(I,k)) * dpovg / PK
            DELQBAR(I)   = DELQBAR(I) + (QO(I,k) - Q1(I,k)) * dpovg
          ENDIF
C
C  THIS STEP TAKES CARE OF STABLE HEATING
C
          IF(KMLEV(I,k).EQ.0.AND.DQ(I,k).GT.0.) THEN
            QCOND   = (QO(I,k)-QS(I,k)) /
     &                (1.+EL2ORC*QS(I,k)/(TO(I,K)*TO(I,K)))
            QO(I,k) = QO(I,k) - QCOND
            TO(I,k) = TO(I,k) + QCOND * ELOCP
!           PK = PSK(I) * SLK(K)
            PK = PRSLK(I,K)
C           TO(I,k) = FTMA(THE(I,k),PK,QO(I,k))
            DPOVG    = DEL(I,K) * (1.0/grav)
            RAINLVL(I,k) = (Q1(I,k) - QO(I,k)) * dpovg
            DELTBAR(I)   = DELTBAR(I) + (TO(I,k) - T1(I,k)) * dpovg / PK
            DELQBAR(I)   = DELQBAR(I) + (QO(I,k) - Q1(I,k)) * dpovg
            QS(I,k) = QO(I,k)
          ENDIF
        ENDDO
      ENDDO
C
C  EVAPORATION OF FALLING RAIN
C
      DO K = KMAX, 1, -1
        DO I = 1, IM
          T1(I,k) = TO(I,k)
          Q1(I,k) = QO(I,k)
          DPOVG   = DEL(I,K) * (1.0/grav)
          RAIN(I) = RAIN(I) + RAINLVL(I,k) + CLW(I,k) * DPOVG
          DQ(I,k) = (QO(I,k) - QS(I,k)) /
     &              (1. + EL2ORC*QS(I,k)/(TO(I,K)*TO(I,K)))
          IF(RAIN(I).GT.0..AND.RAINLVL(I,k).LE.0.) THEN
            QEVAP      = -DQ(I,k)*(1.-EXP(-0.32*SQRT(DT*RAIN(I))))
            RNEVAP     = MIN(QEVAP*DPOVG,RAIN(I))
            Q1(I,k)    = Q1(I,k)+RNEVAP/DPOVG
            T1(I,k)    = T1(I,k)-RNEVAP/DPOVG*ELOCP
            RAIN(I)    = RAIN(I)-RNEVAP
            DELTBAR(I) = DELTBAR(I) - RNEVAP * ELOCP
            DELQBAR(I) = DELQBAR(I) + RNEVAP
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, IM
        RAIN(I) = MAX(RAIN(I),cons_0)     !constant
      ENDDO
!!
      RETURN
      END
