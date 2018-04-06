C-----------------------------------------------------------------------
      SUBROUTINE MSTADBTN(IM,KM,K1,K2,PRSL,PRSLK,TENV,QENV,ctei,
     &                  KLCL,KBOT,KTOP,TCLD,QCLD,ktopm,lprnt,ipr,ind)
!
      USE MACHINE , ONLY : kind_phys
      USE FUNCPHYS , ONLY : FTDP, FTHE, FTLCL, STMA
      USE PHYSCONS, EPS => con_eps, EPSM1 => con_epsm1, FV => con_FVirt
      implicit none
!
      logical lprnt, ctei(im)
      integer ktopm(im), ind(im), ipr
!
      integer              k,k1,k2,km,i,im
      real(kind=kind_phys) pv,qma,slklcl,tdpd,thelcl,tlcl
      real(kind=kind_phys) tma,tvcld,tvenv
!
      real(kind=kind_phys) PRSL(IM,KM), PRSLK(IM,KM), TENV(IM,KM),
     &                     QENV(IM,KM), TCLD(IM,KM),  QCLD(IM,KM)
      INTEGER              KLCL(IM),    KBOT(IM),      KTOP(IM)
!  LOCAL ARRAYS
      real(kind=kind_phys) SLKMA(IM), THEMA(IM)
      logical              find_ctop(im)
!-----------------------------------------------------------------------
!  DETERMINE WARMEST POTENTIAL WET-BULB TEMPERATURE BETWEEN K1 AND K2.
!  COMPUTE ITS LIFTING CONDENSATION LEVEL.
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
!-----------------------------------------------------------------------
!  SET CLOUD TEMPERATURES AND HUMIDITIES WHEREVER THE PARCEL LIFTED UP
!  THE MOIST ADIABAT IS BUOYANT WITH RESPECT TO THE ENVIRONMENT.
      DO I=1,IM
        KLCL(I)=KM+1
        KBOT(I)=KM+1
        KTOP(I)=0
        find_ctop(i) = .true.
      ENDDO
      DO K=1,KM
        DO I=1,IM
          TCLD(I,K)=0.
          QCLD(I,K)=0.
        ENDDO
      ENDDO
      DO K=K1,KM
        DO I=1,IM
!     if (lprnt .and. ind(i) == ipr) print *,' prslk='
!    &,prslk(i,k),' slkma=',slkma(i),' k=',k,' km=',km
          IF(PRSLK(I,K).LE.SLKMA(I) .and. k <= ktopm(I)-1) THEN
            KLCL(I) = MIN(KLCL(I),K)
            CALL STMA(THEMA(I),PRSLK(I,K),TMA,QMA)
!           TMA   = FTMA(THEMA(I),PRSLK(I,K),QMA)
            TVCLD = TMA*(1.+FV*QMA)
            TVENV = TENV(I,K)*(1.+FV*QENV(I,K))

!     if (lprnt .and. ind(i) == ipr) print *,' tvcld='
!    &,tvcld,' tvenv=',tvenv,' ktop=',ktop(i),' kbot=',kbot(i)

            IF(TVCLD > TVENV .and. find_ctop(i)) THEN
              KBOT(I)   = MIN(KBOT(I),K)
              KTOP(I)   = MAX(KTOP(I),K)
              TCLD(I,K) = TMA - TENV(I,K)
              QCLD(I,K) = QMA - QENV(I,K)
            ELSEIF (KTOP(I) > 0 .and. .not. ctei(i)) THEN
              find_ctop(i) = .false.
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
      RETURN
      END
