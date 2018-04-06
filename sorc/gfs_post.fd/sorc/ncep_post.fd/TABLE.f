!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE TABLE(PTBL,TTBL,PT                                  &
     &,                RDQ,RDTH,RDP,RDTHE,PL,THL,QS0,SQS,STHE,THE0)
!     ******************************************************************
!     *                                                                *
!     *    GENERATE VALUES FOR LOOK-UP TABLES USED IN CONVECTION       *
!     *                                                                *
!     ******************************************************************
!
       implicit none                                 ! Moorthi
!
       integer,parameter :: ITB=076, JTB=134
       real, parameter   :: THH=365.,PH=105000.                              &
     &,                     PQ0=379.90516,A1=610.78,A2=17.2693882,A3=273.16, &
                            A4=35.86, R=287.04,CP=1004.6,ELIWV=2.683E6,EPS=1.E-9
!
       real,dimension(ITB,JTB),intent(out) :: PTBL
       real,dimension(JTB,ITB),intent(out) :: TTBL
       real,dimension(JTB),intent(out)     :: QS0,SQS
       real,dimension(ITB),intent(out)     :: STHE,THE0
       real, intent(in)                    :: PT,THL
       real, intent(out)                   :: RDQ,RDTH,RDP,RDTHE,PL
       real                                                             &
     &         QSOLD(JTB), POLD  (JTB), QSNEW(JTB)                      &
     &,        Y2P  (JTB), APP   (JTB), AQP  (JTB), PNEW(JTB)           &
     &,        TOLD (JTB), THEOLD(JTB)                                  &
     &,        Y2T  (JTB), THENEW(JTB), APT  (JTB), AQT (JTB), TNEW(JTB)
!
       real    DTH,DP,TH,P,APE,DENOM,QS0K,SQSK,DQS,QS,THEOK,STHEK       &
     &,        the0k, dthe
       integer LTHM,KPM,KTHM1,KPM1,KP,KMM,KTHM,KTH

!--------------COARSE LOOK-UP TABLE FOR SATURATION POINT----------------
      KTHM  = JTB
      KPM   = ITB
      KTHM1 = KTHM-1
      KPM1  = KPM-1
! 
      PL = PT
!
      DTH = (THH-THL) / REAL(KTHM-1)
      DP  = (PH -PL ) / REAL(KPM -1)
!
      RDTH = 1./DTH
      RDP  = 1./DP
      RDQ  = KPM-1
!
      TH = THL - DTH
!-----------------------------------------------------------------------
      DO KTH=1,KTHM
        TH = TH + DTH
        P  = PL - DP
        DO KP=1,KPM
          P     = P + DP
          if (p <= 0.0) then
            pold(1)  = 0.0
            qsold(1) = 0.0
          else
            APE   = (100000./P)**(R/CP)
            DENOM = TH - A4*APE
            IF (DENOM > EPS) THEN
              QSOLD(KP) = PQ0 / P*EXP(A2*(TH-A3*APE)/DENOM)
            ELSE
              QSOLD(KP) = 0.
            ENDIF
!           QSOLD(KP)=PQ0/P*EXP(A2*(TH-A3*APE)/(TH-A4*APE))
            POLD(KP) = P
          endif
        enddo
!
        QS0K       = QSOLD(1)
        SQSK       = QSOLD(KPM) - QSOLD(1)
        QSOLD(1  ) = 0.
        QSOLD(KPM) = 1.
!
        DO KP=2,KPM1
          QSOLD(KP) = (QSOLD(KP)-QS0K)/SQSK
          IF((QSOLD(KP)-QSOLD(KP-1)) < EPS) QSOLD(KP) = QSOLD(KP-1)+EPS
        enddo
!
        QS0(KTH) = QS0K
        SQS(KTH) = SQSK
!-----------------------------------------------------------------------
        QSNEW(1  ) = 0.
        QSNEW(KPM) = 1.
        DQS = 1./REAL(KPM-1)
!
        DO KP=2,KPM1
          QSNEW(KP) = QSNEW(KP-1) + DQS
        enddo
!
        Y2P(1   ) = 0.
        Y2P(KPM ) = 0.
!
        CALL SPLINE(JTB,KPM,QSOLD,POLD,Y2P,KPM,QSNEW,PNEW,APP,AQP)
!
        DO KP=1,KPM
          PTBL(KP,KTH) = PNEW(KP)
        enddo
!-----------------------------------------------------------------------
      enddo
!--------------COARSE LOOK-UP TABLE FOR T(P) FROM CONSTANT THE----------
      P = PL - DP
!     write(0,*)' kpm=',kpm,' P=',P,' DP=',DP,' thl=',thl,' dth=',dth
      DO KP=1,KPM
        P  = P + DP
        TH = THL - DTH
        DO KTH=1,KTHM
          TH    = TH + DTH
          if (p <= 0.0) then
            TOLD(KTH)   = TH
            theold(kth) = th
          else
            APE   = (100000./P)**(R/CP)
            DENOM = TH - A4*APE
            IF (DENOM .GT. EPS) THEN
              QS = PQ0/P*EXP(A2*(TH-A3*APE)/DENOM)
            ELSE
              QS = 0.
            ENDIF
!           QS=PQ0/P*EXP(A2*(TH-A3*APE)/(TH-A4*APE))
            TOLD(KTH) = TH / APE
!           write(0,*)' TH=',TH,' QS=',QS,' TOLD=',TOLD(kth),' kth=',kth
            THEOLD(KTH) = TH*EXP(ELIWV*QS/(CP*TOLD(KTH)))
          endif
        enddo
!         write(0,*)' theold=',theold
!
        THE0K = THEOLD(1)
        STHEK = THEOLD(KTHM) - THEOLD(1)
        THEOLD(1   ) = 0.
        THEOLD(KTHM) = 1.
! 
        DO KTH=2,KTHM1
          THEOLD(KTH)=(THEOLD(KTH)-THE0K)/STHEK
          IF((THEOLD(KTH)-THEOLD(KTH-1)).LT.EPS)      &
              THEOLD(KTH) = THEOLD(KTH-1) +  EPS
        enddo
!
        THE0(KP) = THE0K
        STHE(KP) = STHEK
!-----------------------------------------------------------------------
        THENEW(1  )  = 0.
        THENEW(KTHM) = 1.
        DTHE         = 1./REAL(KTHM-1)
        RDTHE        = 1./DTHE
!
        DO KTH=2,KTHM1
         THENEW(KTH) = THENEW(KTH-1) + DTHE
        enddo
!
        Y2T(1   ) = 0.
        Y2T(KTHM) = 0.
!
        CALL SPLINE(JTB,KTHM,THEOLD,TOLD,Y2T,KTHM,THENEW,TNEW,APT,AQT)
!
        DO KTH=1,KTHM
          TTBL(KTH,KP) = TNEW(KTH)
        enddo
!-----------------------------------------------------------------------
      enddo
!
      RETURN
      END
