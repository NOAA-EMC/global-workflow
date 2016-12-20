C-----------------------------------------------------------------------
      SUBROUTINE MSTADBT3(IM,KM,K1,K2,PRSL,PRSLK,TENV,QENV,
     &                  KLCL,KBOT,KTOP,TCLD,QCLD)
cyt      INCLUDE DBMSTADB;
cc
      USE MACHINE , ONLY : kind_phys
      USE FUNCPHYS , ONLY : FTDP, FTHE, FTLCL, STMA
      USE PHYSCONS, EPS => con_eps, EPSM1 => con_epsm1, FV => con_FVirt
      implicit none
cc
cc
      integer              k,k1,k2,km,i,im
      real(kind=kind_phys) pv,qma,slklcl,tdpd,thelcl,tlcl
      real(kind=kind_phys) tma,tvcld,tvenv
cc
      real(kind=kind_phys) PRSL(IM,KM), PRSLK(IM,KM), TENV(IM,KM),
     &                     QENV(IM,KM), TCLD(IM,KM),  QCLD(IM,KM)
      INTEGER              KLCL(IM),    KBOT(IM),      KTOP(IM)
C  LOCAL ARRAYS
      real(kind=kind_phys) SLKMA(IM), THEMA(IM)
C-----------------------------------------------------------------------
C  DETERMINE WARMEST POTENTIAL WET-BULB TEMPERATURE BETWEEN K1 AND K2.
C  COMPUTE ITS LIFTING CONDENSATION LEVEL.
!
      DO I=1,IM
        SLKMA(I) = 0.
        THEMA(I) = 0.
      ENDDO
      DO K=K1,K2
        DO I=1,IM
          PV   = PRSL(I,K)*QENV(I,K)/(EPS-EPSM1*QENV(I,K))
          TDPD = TENV(I,K)-FTDP(PV)
          IF(TDPD.GT.0.) THEN
            TLCL   = FTLCL(TENV(I,K),TDPD)
            SLKLCL = PRSLK(I,K)*TLCL/TENV(I,K)
          ELSE
            TLCL   = TENV(I,K)
            SLKLCL = PRSLK(I,K)
          ENDIF
          THELCL=FTHE(TLCL,SLKLCL)
          IF(THELCL.GT.THEMA(I)) THEN
            SLKMA(I) = SLKLCL
            THEMA(I) = THELCL
          ENDIF
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C  SET CLOUD TEMPERATURES AND HUMIDITIES WHEREVER THE PARCEL LIFTED UP
C  THE MOIST ADIABAT IS BUOYANT WITH RESPECT TO THE ENVIRONMENT.
      DO I=1,IM
        KLCL(I)=KM+1
        KBOT(I)=KM+1
        KTOP(I)=0
      ENDDO
      DO K=1,KM
        DO I=1,IM
          TCLD(I,K)=0.
          QCLD(I,K)=0.
        ENDDO
      ENDDO
      DO K=K1,KM
        DO I=1,IM
          IF(PRSLK(I,K).LE.SLKMA(I)) THEN
            KLCL(I)=MIN(KLCL(I),K)
            CALL STMA(THEMA(I),PRSLK(I,K),TMA,QMA)
!           TMA=FTMA(THEMA(I),PRSLK(I,K),QMA)
            TVCLD=TMA*(1.+FV*QMA)
            TVENV=TENV(I,K)*(1.+FV*QENV(I,K))
            IF(TVCLD.GT.TVENV) THEN
              KBOT(I)=MIN(KBOT(I),K)
              KTOP(I)=MAX(KTOP(I),K)
              TCLD(I,K)=TMA-TENV(I,K)
              QCLD(I,K)=QMA-QENV(I,K)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
      RETURN
      END
