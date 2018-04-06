      SUBROUTINE CNVC90(CLSTP,IM,IX,RN,KBOT,KTOP,KM,PRSI,
     1                   ACV,ACVB,ACVT,CV,CVB,CVT)
cc
      USE MACHINE, ONLY :kind_phys
      implicit none
      integer              i,ibot,im,itop,km,lc,lz,n,ncc,ix
      real(kind=kind_phys) ah,cc1,cc2,clstp,cvb0,p1,p2,rkbot,rktop,val
cc
      integer              KBOT(IM),KTOP(IM)
      real(kind=kind_phys) RN(IM),  ACV(IM), ACVB(IM), ACVT(IM),
     &                     CV(IM),  CVB(IM), CVT(IM)
      real(kind=kind_phys) prsi(ix,km+1)
      integer              NMD(IM)
      real(kind=kind_phys) PMD(IM)
!
      real (kind=kind_phys), parameter :: cons_100=100.0
      real(kind=kind_phys) R_KBOT_I, R_KTOP_I
!
      PARAMETER(NCC=9)
      real(kind=kind_phys) CC(NCC),P(NCC)
      DATA CC/0.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8/
      DATA P/.14,.31,.70,1.6,3.4,7.7,17.,38.,85./
      DATA CVB0/100./
!
      LZ=0
      LC=0
      IF(CLSTP.GE.1000.) LZ=1
      IF(CLSTP.GE.1100..OR.(CLSTP.LT.1000..AND.CLSTP.GE.100.)) LC=1
      AH=MOD(CLSTP,cons_100)
      IF(LZ.NE.0) THEN
        DO I=1,IM
          ACV(I)  = 0.
          ACVB(I) = CVB0
          ACVT(I) = 0.
        ENDDO
      ENDIF
      IF(LC.NE.0) THEN
        DO I=1,IM
          IF(RN(I).GT.0.) THEN
            ACV(I)  = ACV(I)+RN(I)
            R_KBOT_I= KBOT(I)
            ACVB(I) = MIN(ACVB(I),R_KBOT_I)
            R_KTOP_I= KTOP(I)
            ACVT(I) = MAX(ACVT(I),R_KTOP_I)
          ENDIF
        ENDDO
      ENDIF
      IF(AH.GT.0.01.AND.AH.LT.99.99) THEN
        DO I=1,IM
          IF(ACV(I).GT.0.) THEN
!           CVB(I) = ACVB(I)
!           CVT(I) = ACVT(I)
c....   convert cvt and cvb to pressures
            ITOP   = NINT(ACVT(I))
            CVT(I) = PRSI(i,ITOP+1)
            IBOT   = NINT(ACVB(I))
            CVB(I) = PRSI(i,IBOT)
          ELSE
!           CVB(I) = CVB0
            CVB(I) = 0.
            CVT(I) = 0.
          ENDIF
          PMD(I)   = ACV(I)*(24.E+3/AH)
          NMD(I)   = 0
        ENDDO
        DO N=1,NCC
          DO I=1,IM
            IF(PMD(I).GT.P(N)) NMD(I) = N
          ENDDO
        ENDDO
        DO I=1,IM
          IF(NMD(I).EQ.0) THEN
            CV(I)  = 0.
!           CVB(I) = CVB0
            CVB(I) = 0.
            CVT(I) = 0.
          ELSEIF(NMD(I).EQ.NCC) THEN
            CV(I)  = CC(NCC)
          ELSE
            CC1    = CC(NMD(I))
            CC2    = CC(NMD(I)+1)
            P1     = P(NMD(I))
            P2     = P(NMD(I)+1)
            CV(I)  = CC1 + (CC2-CC1)*(PMD(I)-P1)/(P2-P1)
          ENDIF
        ENDDO
      ENDIF
      RETURN
      END

