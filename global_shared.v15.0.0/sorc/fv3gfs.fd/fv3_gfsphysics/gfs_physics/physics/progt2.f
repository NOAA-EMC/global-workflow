      SUBROUTINE PROGT2(IM,KM,RHSCNPY,
     &                  RHSMC,AI,BI,CI,SMC,iSLIMSK,
     &                  CANOPY,PRECIP,RUNOFF,SNOWMT,
     &                  ZSOIL,SOILTYP,SIGMAF,DELT,me)
cc
      USE MACHINE     , ONLY : kind_phys
!     USE MACHINE_RAD , ONLY : kind_phys
      implicit none
      integer              km, IM, me
      real(kind=kind_phys) delt
      real(kind=kind_phys) RHSCNPY(IM),  RHSMC(IM,KM), AI(IM,KM),
     &                     BI(IM,KM),    CI(IM,KM),    SMC(IM,KM),
     &                                   CANOPY(IM),   PRECIP(IM),
     &                     RUNOFF(IM),   SNOWMT(IM),   ZSOIL(IM,KM),
     &                     SIGMAF(IM)
      INTEGER SOILTYP(IM), ISLIMSK(IM)
!
      integer              k, lond, i
      real(kind=kind_phys) CNPY(IM), PRCP(IM),   TSAT(IM),
     &                     INF(IM),  INFMAX(IM), SMSOIL(IM,KM)
!
      real(kind=kind_phys) cc,    ctfil1, ctfil2, delt2,
     &                     drip,  rffact, rhoh2o,
     &                     rzero, scanop, tdif, thsat, KSAT
!
      LOGICAL FLAG(IM)
cc
      PARAMETER (SCANOP=.5, RHOH2O=1000.)
      PARAMETER (CTFIL1=.5, CTFIL2=1.-CTFIL1)
c     PARAMETER (CTFIL1=1., CTFIL2=1.-CTFIL1)
      PARAMETER (RFFACT=.15)
C
C##DG LATD = 44
      LOND = 353
      DELT2 = DELT * 2.

      DO I=1,IM
        FLAG(I) = ISLIMSK(I) == 1
      ENDDO

C
C  PRECIPITATION RATE IS NEEDED IN UNIT OF KG M-2 S-1
C
      DO I=1,IM
       IF(FLAG(I)) THEN
        PRCP(I) = RHOH2O * (PRECIP(I)+SNOWMT(I)) / DELT
        RUNOFF(I) = 0.
        CNPY(I) = CANOPY(I)
       ENDIF
      ENDDO
C##DG  IF(LAT.EQ.LATD) THEN
C##DG    PRINT *, ' BEFORE RUNOFF CAL, RHSMC =', RHSMC(1)
C##DG  ENDIF
C
C  UPDATE CANOPY WATER CONTENT
C
      DO I=1,IM
        IF(FLAG(I)) THEN
          RHSCNPY(I) = RHSCNPY(I) + SIGMAF(I) * PRCP(I)
          CANOPY(I) = CANOPY(I) + DELT * RHSCNPY(I)
          CANOPY(I) = MAX(CANOPY(I),0.)
          PRCP(I) = PRCP(I) * (1. - SIGMAF(I))
          IF(CANOPY(I).GT.SCANOP) THEN
            DRIP = CANOPY(I) - SCANOP
            CANOPY(I) = SCANOP
            PRCP(I) = PRCP(I) + DRIP / DELT
          ENDIF
C
C  CALCULATE INFILTRATION RATE
C
          INF(I) = PRCP(I)
          TSAT(I) = THSAT(SOILTYP(I))
C         DSAT = FUNCDF(TSAT(I),SOILTYP(I))
C         KSAT = FUNCKT(TSAT(I),SOILTYP(I))
C         INFMAX(I) = -DSAT * (TSAT(I) - SMC(I,1))
C    &                / (.5 * ZSOIL(I,1))
C    &                + KSAT
          INFMAX(I) = (-ZSOIL(I,1)) *
     &                ((TSAT(I) - SMC(I,1)) / DELT - RHSMC(I,1))
     &                * RHOH2O
          INFMAX(I) = MAX(RFFACT*INFMAX(I),0.)
C         IF(SMC(I,1).GE.TSAT(I)) INFMAX(I) = KSAT
C         IF(SMC(I,1).GE.TSAT(I)) INFMAX(I) = ZSOIL(I,1) * RHSMC(I,1)
          IF(INF(I).GT.INFMAX(I)) THEN
            RUNOFF(I) = INF(I) - INFMAX(I)
            INF(I) = INFMAX(I)
          ENDIF
          INF(I) = INF(I) / RHOH2O
          RHSMC(I,1) = RHSMC(I,1) - INF(I) / ZSOIL(I,1)
        ENDIF
      ENDDO
!!
C##DG  IF(LAT.EQ.LATD) THEN
C##DG    PRINT *, ' PRCP(I), INFMAX(I), RUNOFF =', PRCP(I),INFMAX(I),RUNOFF
C##DG    PRINT *, ' SMSOIL =', SMC(1), SMC(2)
C##DG    PRINT *, ' RHSMC =', RHSMC(1)
C##DG  ENDIF
C
C  WE CURRENTLY IGNORE THE EFFECT OF RAIN ON SEA ICE
C
!!
C
C  SOLVE THE TRI-DIAGONAL MATRIX
C
      DO K = 1, KM
        DO I=1,IM
          IF(FLAG(I))  THEN
            RHSMC(I,K) = RHSMC(I,K) * DELT2
            AI(I,K) = AI(I,K) * DELT2
            BI(I,K) = 1. + BI(I,K) * DELT2
            CI(I,K) = CI(I,K) * DELT2
          ENDIF
        ENDDO
      ENDDO
C  FORWARD ELIMINATION
      DO I=1,IM
        IF(FLAG(I)) THEN
          CI(I,1) = -CI(I,1) / BI(I,1)
          RHSMC(I,1) = RHSMC(I,1) / BI(I,1)
        ENDIF
      ENDDO
      DO K = 2, KM
        DO I=1,IM
          IF(FLAG(I)) THEN
            CC = 1. / (BI(I,K) + AI(I,K) * CI(I,K-1))
            CI(I,K) = -CI(I,K) * CC
            RHSMC(I,K)=(RHSMC(I,K)-AI(I,K)*RHSMC(I,K-1))*CC
          ENDIF
        ENDDO
      ENDDO
C  BACKWARD SUBSTITUTTION
      DO I=1,IM
        IF(FLAG(I)) THEN
          CI(I,KM) = RHSMC(I,KM)
        ENDIF
      ENDDO
!!
      DO K = KM-1, 1
        DO I=1,IM
          IF(FLAG(I)) THEN
            CI(I,K) = CI(I,K) * CI(I,K+1) + RHSMC(I,K)
          ENDIF
        ENDDO
      ENDDO
 100  CONTINUE
C
C  UPDATE SOIL MOISTURE
C
      DO K = 1, KM
        DO I=1,IM
          IF(FLAG(I)) THEN
            SMSOIL(I,K) = SMC(I,K) + CI(I,K)
            SMSOIL(I,K) = MAX(SMSOIL(I,K),0.)
            TDIF = MAX(SMSOIL(I,K) - TSAT(I),0.)
            RUNOFF(I) = RUNOFF(I) -
     &                RHOH2O * TDIF * ZSOIL(I,K) / DELT
            SMSOIL(I,K) = SMSOIL(I,K) - TDIF
          ENDIF
        ENDDO
      ENDDO
      DO K = 1, KM
        DO I=1,IM
          IF(FLAG(I)) THEN
            SMC(I,K) = CTFIL1 * SMSOIL(I,K) + CTFIL2 * SMC(I,K)
          ENDIF
        ENDDO
      ENDDO
c       IF(FLAG(I)) THEN
c         CANOPY(I) = CTFIL1 * CANOPY(I) + CTFIL2 * CNPY(I)
c       ENDIF
C     I = 1
C     PRINT *, ' SMC'
C     PRINT 6000, SMC(1), SMC(2)
c6000 FORMAT(2(F8.5,','))
      RETURN
      END
      FUNCTION KTSOIL(THETA,KTYPE)
!
      USE MACHINE     , ONLY : kind_phys
      USE module_progtm , ONLY : TSAT, DFKT
      implicit none
      integer              ktype,kw
      real(kind=kind_phys) ktsoil, theta, w
!
      W = (THETA / TSAT(KTYPE)) * 20. + 1.
      KW = W
      KW = MIN(KW,21)
      KW = MAX(KW,1)
      KTSOIL = DFKT(KW,KTYPE)
     &         + (W - KW) * (DFKT(KW+1,KTYPE) - DFKT(KW,KTYPE))
      RETURN
      END
      FUNCTION FUNCDF(THETA,KTYPE)
!
      USE MACHINE     , ONLY : kind_phys
      USE module_progtm , ONLY : TSAT, DFK
      implicit none
      integer              ktype,kw
      real(kind=kind_phys) funcdf,theta,w
!
      W = (THETA / TSAT(KTYPE)) * 20. + 1.
      KW = W
      KW = MIN(KW,21)
      KW = MAX(KW,1)
      FUNCDF = DFK(KW,KTYPE)
     &         + (W - KW) * (DFK(KW+1,KTYPE) - DFK(KW,KTYPE))
      RETURN
      END
      FUNCTION FUNCKT(THETA,KTYPE)
!
      USE MACHINE     , ONLY : kind_phys
      USE module_progtm , ONLY : TSAT, KTK
      implicit none
      integer             ktype,kw
      real(kind=kind_phys) funckt,theta,w
!
      W = (THETA / TSAT(KTYPE)) * 20. + 1.
      KW = W
      KW = MIN(KW,21)
      KW = MAX(KW,1)
      FUNCKT = KTK(KW,KTYPE)
     &         + (W - KW) * (KTK(KW+1,KTYPE) - KTK(KW,KTYPE))
      RETURN
      END
      FUNCTION THSAT(KTYPE)
!
      USE MACHINE     , ONLY : kind_phys
      USE module_progtm , ONLY : TSAT
      implicit none
      integer             ktype
      real(kind=kind_phys) thsat
!
      THSAT = TSAT(KTYPE)
      RETURN
      END
      FUNCTION TWLT(KTYPE)

      USE MACHINE     , ONLY : kind_phys
!     USE module_progtm
      implicit none
      integer              ktype
      real(kind=kind_phys) twlt
!
      TWLT = .1
      RETURN
      END
