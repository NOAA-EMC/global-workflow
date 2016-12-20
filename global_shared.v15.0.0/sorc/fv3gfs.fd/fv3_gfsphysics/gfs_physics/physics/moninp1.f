CFPP$ NOCONCUR R
      SUBROUTINE MONINP1(IX,IM,KM,ntrac,DV,DU,TAU,RTG,
     &     U1,V1,T1,Q1,
     &     PSK,RBSOIL,FM,FH,TSEA,QSS,HEAT,EVAP,STRESS,SPD1,KPBL,
!    &     PSK,RBSOIL,CD,CH,FM,FH,TSEA,QSS,DPHI,SPD1,KPBL,
     &     PRSI,DEL,PRSL,PRSLK,PHII,PHIL,DELTIM,
     &     DUSFC,DVSFC,DTSFC,DQSFC,HPBL,HGAMT,HGAMQ,DKT
!    &,    kinver, ctei_r, ctei_rm, xkzm_m, xkzm_h)
     &,    kinver, xkzm_m, xkzm_h)
!    &,    kinver, oro, ctei_r, ctei_rm, xkzm_m, xkzm_h)
!
      USE MACHINE     , ONLY : kind_phys
      USE PHYSCONS, grav => con_g, RD => con_RD, CP => con_CP
     &,             HVAP => con_HVAP, ROG => con_ROG, FV => con_FVirt
      implicit none
!
!     This code assumes that terrain height is not included in PHII and PHIL
!
!     Arguments
!
      integer IX, IM, KM, ntrac, KPBL(IM)
      integer kinver(im)
!
      real(kind=kind_phys) DELTIM
      real(kind=kind_phys) DV(IM,KM),     DU(IM,KM),
     &                     TAU(IM,KM),    RTG(IM,KM,ntrac),
     &                     U1(IX,KM),     V1(IX,KM),
     &                     T1(IX,KM),     Q1(IX,KM,ntrac),
     &                     PSK(IM),       RBSOIL(IM),
!    &                     CD(IM),        CH(IM),
     &                     FM(IM),        FH(IM),
     &                     TSEA(IM),      QSS(IM),
     &                                    SPD1(IM),
!    &                     DPHI(IM),      SPD1(IM),
     &                     PRSI(IX,KM+1), DEL(IX,KM),
     &                     PRSL(IX,KM),   PRSLK(IX,KM),
     &                     PHII(IX,KM+1), PHIL(IX,KM),
     &                     DUSFC(IM),
     &                     dvsfc(IM),     dtsfc(IM),
     &                     DQSFC(IM),     HPBL(IM),
     &                     HGAMT(IM),     hgamq(IM)
!    &,                    ctei_r(im),    ctei_rm
!    &                     HGAMT(IM),     hgamq(IM), oro(im),
!
!    Locals
!
      integer i,iprt,is,iun,k,kk,kmpbl,lond
!     real(kind=kind_phys) betaq(IM), betat(IM),   betaw(IM),
      real(kind=kind_phys) evap(IM),  heat(IM),    phih(IM),
     &                     phim(IM),  rbdn(IM),    rbup(IM),
     &                     the1(IM),  stress(im),  beta(im),
     &                     the1v(IM), thekv(IM),   thermal(IM),
     &                     thesv(IM), ustar(IM),   wscale(IM)
!    &                     thesv(IM), ustar(IM),   wscale(IM),  zl1(IM)
!
      real(kind=kind_phys) RDZT(IM,KM-1),
     &                     ZI(IM,KM+1),     ZL(IM,KM),
     &                     DKU(IM,KM-1),    DKT(IM,KM-1),
     &                     AL(IM,KM-1),     AD(IM,KM),
     &                     AU(IM,KM-1),     A1(IM,KM),
     &                     A2(IM,KM*ntrac), THETA(IM,KM)
      logical              pblflg(IM),   sfcflg(IM), stable(IM)
!
      real(kind=kind_phys) aphi16,  aphi5,  bet1,   bvf2,
     &                     cfac,    conq,   cont,   conw,
     &                     conwrc,  dk,     dkmax,  dkmin,
     &                     dq1,     dsdz2,  dsdzq,  dsdzt,
     &                     dsig,    dt,     dthe1,  dtodsd,
     &                     dtodsu,  dw2,    dw2min, g,
     &                     gamcrq,  gamcrt, gocp,   gor, gravi,
     &                     hol,     pfac,   prmax,  prmin, prinv,
     &                     prnum,   qmin,   qtend,  rbcr,
     &                     rbint,   rdt,    rdz,
!    &                     rbint,   rdt,    rdz,    rdzt1,
     &                     ri,      rimin,  rl2,    rlam,  rlamun,
     &                     rone,   rzero,   sfcfrac,
     &                              shr2,   spdk2,  sri,
!    &                     sflux,   shr2,   spdk2,  sri,
     &                     tem,     ti,     ttend,  tvd,
     &                     tvu,     utend,  vk,     vk2,
     &                     vpert,   vtend,  xkzo(im,km),   zfac,
     &                     zfmin,   zk,     tem1, xkzm_m, xkzm_h
     &,                    xkzm_loc(im), sflux(im)
!
      parameter (gravi=1.0/grav)
      PARAMETER(g=grav)
      PARAMETER(GOR=G/RD,GOCP=G/CP)
      PARAMETER(CONT=CP/G,CONQ=HVAP/G,CONW=1.0/G)
!     PARAMETER(RLAM=150.0,VK=0.4,VK2=VK*VK,PRMIN=1.0,PRMAX=4.)
      PARAMETER(RLAM=30.0,VK=0.4,VK2=VK*VK,PRMIN=1.0,PRMAX=4.)
!     PARAMETER(RLAM=50.0,VK=0.4,VK2=VK*VK,PRMIN=1.0,PRMAX=4.)
      PARAMETER(DW2MIN=0.0001,DKMIN=0.0,DKMAX=1000.,RIMIN=-100.)
      PARAMETER(RBCR=0.25,CFAC=7.8,PFAC=2.0,SFCFRAC=0.1)
!     PARAMETER(RBCR=0.5,CFAC=7.8,PFAC=2.0,SFCFRAC=0.1)
!     PARAMETER(QMIN=1.E-8,XKZM=1.0,ZFMIN=1.E-8,APHI5=5.,APHI16=16.)
      PARAMETER(QMIN=1.E-8,         ZFMIN=1.E-8,APHI5=5.,APHI16=16.)
!     PARAMETER(GAMCRT=3.,GAMCRQ=2.E-3)
      PARAMETER(GAMCRT=3.,GAMCRQ=0., RLAMUN=150.0)
!     PARAMETER(GAMCRT=3.,GAMCRQ=0., RLAMUN=30.0)
      PARAMETER(IUN=84)
!
C
C-----------------------------------------------------------------------
C
 601  FORMAT(1X,' MONINP LAT LON STEP HOUR ',3I6,F6.1)
 602      FORMAT(1X,'    K','        Z','        T','       TH',
     1     '      TVH','        Q','        U','        V',
     2     '       SP')
 603      FORMAT(1X,I5,8F9.1)
 604      FORMAT(1X,'  SFC',9X,F9.1,18X,F9.1)
 605      FORMAT(1X,'    K      ZL    SPD2   THEKV   THE1V'
     1         ,' THERMAL    RBUP')
 606      FORMAT(1X,I5,6F8.2)
 607      FORMAT(1X,' KPBL    HPBL      FM      FH   HGAMT',
     1         '   HGAMQ      WS   USTAR      CD      CH')
 608      FORMAT(1X,I5,9F8.2)
 609      FORMAT(1X,' K PR DKT DKU ',I5,3F8.2)
 610      FORMAT(1X,' K PR DKT DKU ',I5,3F8.2,' L2 RI T2',
     1         ' SR2  ',2F8.2,2E10.2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     COMPUTE PRELIMINARY VARIABLES
C
      if (IX .lt. im) stop
!
!     IPRT = 0
!     IF(IPRT.EQ.1) THEN
CCC   LATD = 0
!     LOND = 0
!     ELSE
CCC   LATD = 0
!     LOND = 0
!     ENDIF
C
      DT    = 2. * DELTIM
      RDT   = 1. / DT
      KMPBL = KM / 2
!
      do k=1,km
        do i=1,im
          zi(i,k) = phii(i,k) * gravi
          zl(i,k) = phil(i,k) * gravi
        enddo
      enddo
!
      do k=1,kmpbl
        do i=1,im
          theta(i,k) = t1(i,k) * psk(i) / prslk(i,k)
        enddo
      enddo
C
      DO K = 1,KM-1
        DO I=1,IM
          RDZT(I,K) = 1.0 / (ZL(I,K+1) - ZL(I,K))
        ENDDO
      ENDDO
C
      DO I = 1,IM
         DUSFC(I) = 0.
         DVSFC(I) = 0.
         DTSFC(I) = 0.
         DQSFC(I) = 0.
         HGAMT(I) = 0.
         HGAMQ(I) = 0.
         WSCALE(I) = 0.
         KPBL(I) = 1
         HPBL(I) = ZI(I,2)
         PBLFLG(I) = .TRUE.
         SFCFLG(I) = .TRUE.
         IF(RBSOIL(I).GT.0.0) SFCFLG(I) = .FALSE.
      ENDDO
!!
      DO I=1,IM
!        RDZT1    = GOR * prSL(i,1) / DEL(i,1)
!        BET1     = DT*RDZT1*SPD1(I)/T1(I,1)
!        BETA(I)  = DT*RDZT1/T1(I,1)
         BETA(I)  = DT / (zi(i,2)-zi(i,1))
!        BETAW(I) = BET1*CD(I)
!        BETAT(I) = BET1*CH(I)
!        BETAQ(I) = DPHI(I)*BETAT(I)
      ENDDO
C
      DO I=1,IM
!        ZL1(i) = 0.-(T1(I,1)+TSEA(I))/2.*LOG(PRSL(I,1)/PRSI(I,1))*ROG
!        USTAR(I) = SQRT(CD(I)*SPD1(I)**2)
         USTAR(I) = SQRT(STRESS(I))
      ENDDO
C
      DO I=1,IM
         THESV(I)   = TSEA(I)*(1.+FV*MAX(QSS(I),QMIN))
         THE1(I)    = THETA(I,1)
         THE1V(I)   = THE1(I)*(1.+FV*MAX(Q1(I,1,1),QMIN))
         THERMAL(I) = THE1V(I)
!        DTHE1      = (THE1(I)-TSEA(I))
!        DQ1        = (MAX(Q1(I,1,1),QMIN) - MAX(QSS(I),QMIN))
!        HEAT(I)    = -CH(I)*SPD1(I)*DTHE1
!        EVAP(I)    = -CH(I)*SPD1(I)*DQ1
      ENDDO
C
C
C     COMPUTE THE FIRST GUESS OF PBL HEIGHT
C
      DO I=1,IM
         STABLE(I) = .FALSE.
!        ZL(i,1) = ZL1(i)
         RBUP(I) = RBSOIL(I)
      ENDDO
      DO K = 2, KMPBL
        DO I = 1, IM
          IF(.NOT.STABLE(I)) THEN
             RBDN(I)   = RBUP(I)
!            ZL(I,k)   = ZL(I,K-1) - (T1(i,k)+T1(i,K-1))/2 *
!    &                   LOG(PRSL(I,K)/PRSL(I,K-1)) * ROG
             THEKV(I)  = THETA(i,k)*(1.+FV*MAX(Q1(i,k,1),QMIN))
             SPDK2     = MAX((U1(i,k)**2+V1(i,k)**2),1.)
             RBUP(I)   = (THEKV(I)-THE1V(I))*(G*ZL(I,k)/THE1V(I))/SPDK2
             KPBL(I)   = K
             STABLE(I) = RBUP(I).GT.RBCR
          ENDIF
        ENDDO
      ENDDO
C
      DO I = 1,IM
         K = KPBL(I)
         IF(RBDN(I).GE.RBCR) THEN
            RBINT = 0.
         ELSEIF(RBUP(I).LE.RBCR) THEN
            RBINT = 1.
         ELSE
            RBINT = (RBCR-RBDN(I))/(RBUP(I)-RBDN(I))
         ENDIF
         HPBL(I) = ZL(I,K-1) + RBINT*(ZL(I,K)-ZL(I,K-1))
         IF(HPBL(I).LT.ZI(I,KPBL(I))) KPBL(I) = KPBL(I) - 1
      ENDDO
!!
      DO I=1,IM
           HOL = MAX(RBSOIL(I)*FM(I)*FM(I)/FH(I),RIMIN)
           IF(SFCFLG(I)) THEN
              HOL = MIN(HOL,-ZFMIN)
           ELSE
              HOL = MAX(HOL,ZFMIN)
           ENDIF
C
!          HOL = HOL*HPBL(I)/ZL1(I)*SFCFRAC
           HOL = HOL*HPBL(I)/ZL(I,1)*SFCFRAC
           IF(SFCFLG(I)) THEN
!             PHIM = (1.-APHI16*HOL)**(-1./4.)
!             PHIH = (1.-APHI16*HOL)**(-1./2.)
              TEM  = 1.0 / (1. - APHI16*HOL)
              PHIH(I) = SQRT(TEM)
              PHIM(I) = SQRT(PHIH(I))
           ELSE
              PHIM(I) = (1.+APHI5*HOL)
              PHIH(I) = PHIM(I)
           ENDIF
           WSCALE(I) = USTAR(I)/PHIM(I)
!          WSCALE(I) = MIN(WSCALE(I),USTAR(I)*APHI16)
           WSCALE(I) = MAX(WSCALE(I),USTAR(I)/APHI5)
      ENDDO
C
C     COMPUTE THE SURFACE VARIABLES FOR PBL HEIGHT ESTIMATION
C     UNDER UNSTABLE CONDITIONS
C
      DO I = 1,IM
         SFLUX(i)  = HEAT(I) + EVAP(I)*FV*THE1(I)
         IF(SFCFLG(I).AND.SFLUX(i).GT.0.0) THEN
           HGAMT(I)   = MIN(CFAC*HEAT(I)/WSCALE(I),GAMCRT)
           HGAMQ(I)   = MIN(CFAC*EVAP(I)/WSCALE(I),GAMCRQ)
           VPERT      = HGAMT(I) + FV*THE1(I)*HGAMQ(I)
           VPERT      = MIN(VPERT,GAMCRT)
           THERMAL(I) = THERMAL(I) + MAX(VPERT,0.)
           HGAMT(I)   = MAX(HGAMT(I),0.0)
           HGAMQ(I)   = MAX(HGAMQ(I),0.0)
         ELSE
           PBLFLG(I) = .FALSE.
         ENDIF
      ENDDO
C
      DO I = 1,IM
         IF(PBLFLG(I)) THEN
            KPBL(I) = 1
            HPBL(I) = ZI(I,2)
         ENDIF
      ENDDO
C
C     ENHANCE THE PBL HEIGHT BY CONSIDERING THE THERMAL
C
      DO I = 1, IM
         IF(PBLFLG(I)) THEN
            STABLE(I) = .FALSE.
            RBUP(I) = RBSOIL(I)
         ENDIF
      ENDDO
      DO K = 2, KMPBL
        DO I = 1, IM
          IF(.NOT.STABLE(I).AND.PBLFLG(I)) THEN
            RBDN(I)   = RBUP(I)
!           ZL(I,k)   = ZL(I,K-1) - (T1(i,k)+T1(i,K-1))/2 *
!    &                  LOG(PRSL(I,K)/PRSL(I,K-1)) * ROG
            THEKV(I)  = THETA(i,k)*(1.+FV*MAX(Q1(i,k,1),QMIN))
            SPDK2     = MAX((U1(i,k)**2+V1(i,k)**2),1.)
            RBUP(I)   = (THEKV(I)-THERMAL(I))*(G*ZL(I,k)/THE1V(I))/SPDK2
            KPBL(I)   = K
            STABLE(I) = RBUP(I).GT.RBCR
          ENDIF
        ENDDO
      ENDDO
C
      DO I = 1,IM
         IF(PBLFLG(I)) THEN
            K = KPBL(I)
            IF(RBDN(I).GE.RBCR) THEN
               RBINT = 0.
            ELSEIF(RBUP(I).LE.RBCR) THEN
               RBINT = 1.
            ELSE
               RBINT = (RBCR-RBDN(I))/(RBUP(I)-RBDN(I))
            ENDIF
            HPBL(I) = ZL(I,K-1) + RBINT*(ZL(I,k)-ZL(I,K-1))
            IF(HPBL(I).LT.ZI(I,KPBL(I))) KPBL(I) = KPBL(I) - 1
            IF(KPBL(I).LE.1) PBLFLG(I) = .FALSE.
         ENDIF
      ENDDO
!!
!     DO I=1,IM
!       xkzm_loc(i) = max(xkzm_min, min(xkzm, oro(i)*0.001))
!       xkzm_loc(i) = min(xkzm_h, xkzm_min + oro(i)*0.002)
!     ENDDO
      DO K = 1,KM-1
        DO I=1,IM
          if (k .lt. kinver(i)) then
!         if (k < kinver(i) .or. ctei_r(i) > ctei_rm) then
            tem1      = 1.0 - prsi(i,k+1) / prsi(i,1)
            tem1      = tem1 * tem1 * 10.0     ! opr
!           xkzo(i,k) = xkzm_loc(i) * min(1.0, exp(-tem1))
            xkzo(i,k) = xkzm_h * min(1.0, exp(-tem1))
!           xkzo(i,k) = xkzm_h
          else
            xkzo(i,k) = 0.0
          endif
        ENDDO
      ENDDO
!!
!
!     COMPUTE DIFFUSION COEFFICIENTS BELOW PBL
!
      DO K = 1, KMPBL
         DO I=1,IM
            IF(KPBL(I).GT.K) THEN
               PRINV = 1.0 / (PHIH(I)/PHIM(I)+CFAC*VK*.1)
               PRINV = MIN(PRINV,PRMAX)
               PRINV = MAX(PRINV,PRMIN)
               ZFAC = MAX((1.-(ZI(I,K+1)-ZL(I,1))/
     1                (HPBL(I)-ZL(I,1))), ZFMIN)
               DKU(i,k) = xkzm_m + WSCALE(I)*VK*ZI(I,K+1) * ZFAC**PFAC
               DKT(i,k) = (DKU(i,k)-xkzm_m)*PRINV + xkzo(i,k)
               DKU(i,k) = MIN(DKU(i,k),DKMAX)
               DKU(i,k) = MAX(DKU(i,k),DKMIN)
               DKT(i,k) = MIN(DKT(i,k),DKMAX)
               DKT(i,k) = MAX(DKT(i,k),DKMIN)
            ENDIF
         ENDDO
      ENDDO
!
!     COMPUTE DIFFUSION COEFFICIENTS OVER PBL (FREE ATMOSPHERE)
!
      DO K = 1, KM-1
         DO I=1,IM
            IF(K.GE.KPBL(I)) THEN
!              TI   = 0.5*(T1(i,k)+T1(i,K+1))
               TI   = 2.0 / (T1(i,k)+T1(i,K+1))
!              RDZ  = RDZT(I,K)/TI
!              RDZ  = RDZT(I,K) * TI
               RDZ  = RDZT(I,K)

               DW2  = ((U1(i,k)-U1(i,K+1))**2 + (V1(i,k)-V1(i,K+1))**2)
               SHR2 = MAX(DW2,DW2MIN)*RDZ*RDZ
               TVD  = T1(i,k)*(1.+FV*MAX(Q1(i,k,1),QMIN))
               TVU  = T1(i,K+1)*(1.+FV*MAX(Q1(i,K+1,1),QMIN))
!              BVF2 = G*(GOCP+RDZ*(TVU-TVD))/TI
               BVF2 = G*(GOCP+RDZ*(TVU-TVD)) * TI
               RI   = MAX(BVF2/SHR2,RIMIN)
               ZK   = VK*ZI(I,K+1)
!              RL2  = (ZK*RLAM/(RLAM+ZK))**2
!              DK   = RL2*SQRT(SHR2)
!              RL2  = ZK*RLAM/(RLAM+ZK)
!              DK   = RL2*RL2*SQRT(SHR2)
               IF(RI < 0.) THEN ! UNSTABLE REGIME
                  RL2      = ZK*RLAMUN/(RLAMUN+ZK)
                  DK       = RL2*RL2*SQRT(SHR2)
                  SRI      = SQRT(-RI)
                  DKU(i,k) = XKZM_M    + DK*(1+8.*(-RI)/(1+1.746*SRI))
                  DKT(i,k) = XKZO(i,k) + DK*(1+8.*(-RI)/(1+1.286*SRI))
               ELSE             ! STABLE REGIME
                  RL2       = ZK*RLAM/(RLAM+ZK)
!                 tem       = rlam * sqrt(0.01*prsi(i,k))
!                 RL2       = ZK*tem/(tem+ZK)
                  DK        = RL2*RL2*SQRT(SHR2)
                  DKT(i,k)  = XKZO(i,k) + DK/(1+5.*RI)**2
                  PRNUM     = 1.0 + 2.1*RI
                  PRNUM     = MIN(PRNUM,PRMAX)
                  DKU(i,k)  = (DKT(i,k)-XKZO(i,k))*PRNUM + XKZM_M
               ENDIF
C
               DKU(i,k) = MIN(DKU(i,k),DKMAX)
               DKU(i,k) = MAX(DKU(i,k),DKMIN)
               DKT(i,k) = MIN(DKT(i,k),DKMAX)
               DKT(i,k) = MAX(DKT(i,k),DKMIN)
C
CCC   IF(I.EQ.LOND.AND.LAT.EQ.LATD) THEN
CCC   PRNUM = DKU(k)/DKT(k)
CCC   WRITE(IUN,610) K,PRNUM,DKT(k),DKU(k),RL2,RI,
CCC   1              BVF2,SHR2
CCC   ENDIF
C
            ENDIF
         ENDDO
      ENDDO
C
C     COMPUTE TRIDIAGONAL MATRIX ELEMENTS FOR HEAT AND MOISTURE
C
      DO I=1,IM
         AD(I,1) = 1.
         A1(I,1) = T1(i,1)   + BETA(i) * HEAT(I)
         A2(I,1) = Q1(i,1,1) + BETA(i) * EVAP(I)
!        A1(I,1) = T1(i,1)-BETAT(I)*(THETA(i,1)-TSEA(I))
!        A2(I,1) = Q1(i,1,1)-BETAQ(I)*
!    &           (MAX(Q1(i,1,1),QMIN)-MAX(QSS(I),QMIN))
      ENDDO
      if(ntrac.ge.2) then
        do k = 2, ntrac
          is = (k-1) * km
          do i = 1, im
            A2(I,1+is) = Q1(i,1,k)
          enddo
        enddo
      endif
C
      DO K = 1,KM-1
        DO I = 1,IM
          DTODSD = DT/DEL(I,K)
          DTODSU = DT/DEL(I,K+1)
          DSIG   = PRSL(I,K)-PRSL(I,K+1)
!         RDZ    = RDZT(I,K)*2./(T1(i,k)+T1(i,K+1))
          RDZ    = RDZT(I,K)
          tem1   = DSIG * DKT(i,k) * RDZ
          IF(PBLFLG(I).AND.K.LT.KPBL(I)) THEN
!            DSDZT = DSIG*DKT(i,k)*RDZ*(GOCP-HGAMT(I)/HPBL(I))
!            DSDZQ = DSIG*DKT(i,k)*RDZ*(-HGAMQ(I)/HPBL(I))
             tem   = 1.0 / HPBL(I)
             DSDZT = tem1 * (GOCP-HGAMT(I)*tem)
             DSDZQ = tem1 * (-HGAMQ(I)*tem)
             A2(I,k)   = A2(I,k)+DTODSD*DSDZQ
             A2(I,k+1) = Q1(i,k+1,1)-DTODSU*DSDZQ
          ELSE
!            DSDZT = DSIG*DKT(i,k)*RDZ*(GOCP)
             DSDZT = tem1 * GOCP
             A2(I,k+1) = Q1(i,k+1,1)
          ENDIF
!         DSDZ2 = DSIG*DKT(i,k)*RDZ*RDZ
          DSDZ2     = tem1 * RDZ
          AU(I,k)   = -DTODSD*DSDZ2
          AL(I,k)   = -DTODSU*DSDZ2
          AD(I,k)   = AD(I,k)-AU(I,k)
          AD(I,k+1) = 1.-AL(I,k)
          A1(I,k)   = A1(I,k)+DTODSD*DSDZT
          A1(I,k+1) = T1(i,k+1)-DTODSU*DSDZT
        ENDDO
      ENDDO
      if(ntrac.ge.2) then
        do kk = 2, ntrac
          is = (kk-1) * km
          do k = 1, km - 1
            do i = 1, im
              A2(I,k+1+is) = Q1(i,k+1,kk)
            enddo
          enddo
        enddo
      endif
C
C     SOLVE TRIDIAGONAL PROBLEM FOR HEAT AND MOISTURE
C
      CALL TRIDIN(IM,KM,ntrac,AL,AD,AU,A1,A2,AU,A1,A2)
C
C     RECOVER TENDENCIES OF HEAT AND MOISTURE
C
      DO  K = 1,KM
         DO I = 1,IM
            TTEND      = (A1(I,k)-T1(i,k))*RDT
            QTEND      = (A2(I,k)-Q1(i,k,1))*RDT
            TAU(i,k)   = TAU(i,k)+TTEND
            RTG(I,k,1) = RTG(i,k,1)+QTEND
            DTSFC(I)   = DTSFC(I)+CONT*DEL(I,K)*TTEND
            DQSFC(I)   = DQSFC(I)+CONQ*DEL(I,K)*QTEND
         ENDDO
      ENDDO
      if(ntrac.ge.2) then
        do kk = 2, ntrac
          is = (kk-1) * km
          do k = 1, km 
            do i = 1, im
              QTEND = (A2(I,K+is)-Q1(i,K,kk))*RDT
              RTG(i,K,kk) = RTG(i,K,kk)+QTEND
            enddo
          enddo
        enddo
      endif
C
C     COMPUTE TRIDIAGONAL MATRIX ELEMENTS FOR MOMENTUM
C
      DO I=1,IM
!        AD(I,1) = 1.+BETAW(I)
         AD(I,1) = 1.0 + BETA(i) * STRESS(I) / SPD1(I)
         A1(I,1) = U1(i,1)
         A2(I,1) = V1(i,1)
!        AD(I,1) = 1.0
!        tem     = 1.0 + BETA(I) * STRESS(I) / SPD1(I)
!        A1(I,1) = U1(i,1) * tem
!        A2(I,1) = V1(i,1) * tem
      ENDDO
C
      DO K = 1,KM-1
        DO I=1,IM
          DTODSD    = DT/DEL(I,K)
          DTODSU    = DT/DEL(I,K+1)
          DSIG      = PRSL(I,K)-PRSL(I,K+1)
!         RDZ       = RDZT(I,K)*2./(T1(i,k)+T1(i,k+1))
          RDZ       = RDZT(I,K)
          DSDZ2     = DSIG*DKU(i,k)*RDZ*RDZ
          AU(I,k)   = -DTODSD*DSDZ2
          AL(I,k)   = -DTODSU*DSDZ2
          AD(I,k)   = AD(I,k)-AU(I,k)
          AD(I,k+1) = 1.-AL(I,k)
          A1(I,k+1) = U1(i,k+1)
          A2(I,k+1) = V1(i,k+1)
        ENDDO
      ENDDO
C
C     SOLVE TRIDIAGONAL PROBLEM FOR MOMENTUM
C
      CALL TRIDI2(IM,KM,AL,AD,AU,A1,A2,AU,A1,A2)
C
C     RECOVER TENDENCIES OF MOMENTUM
C
      DO K = 1,KM
         DO I = 1,IM
            UTEND    = (A1(I,k)-U1(i,k))*RDT
            VTEND    = (A2(I,k)-V1(i,k))*RDT
            DU(i,k)  = DU(i,k)  + UTEND
            DV(i,k)  = DV(i,k)  + VTEND
            DUSFC(I) = DUSFC(I) + CONW*DEL(I,K)*UTEND
            DVSFC(I) = DVSFC(I) + CONW*DEL(I,K)*VTEND
         ENDDO
      ENDDO
!!
      RETURN
      END
