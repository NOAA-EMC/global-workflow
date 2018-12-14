      SUBROUTINE STR(ZETA,U,UC,V,VC)
C                                                                       00373800
      COMMON/FIXFLD/XM(51),F(51)
      DIMENSION ZETA(118,52),U(117,51),UC(118,51),V(117,51),VC(118,51)
      DATA     IMAX,JMAX / 117,51 /
      IMAX2 = IMAX + 1
      IMAXM1 = IMAX - 1
C     ... SPREAD THE U COMPONENT FROM 117X51 TO 118X51 GRID             00374400
C     ...   USING ZETA AS SCRATCH STORAGE DURING SPREADING U & V        00374500
C     ... UNDER THE ASSUMPTION THAT U MAY BE EQUIVALENCED TO UC         00374600
C     ...   AND V MAY BE EQUIVALENCED TO VC ARRAY                       00374700
C                                                                       00374800
      DO  10  JM = 1,JMAX
C                                                                       00375000
C                                                                       00375100
      DO  20  IM = 1,IMAX
C                                                                       00375300
      ZETA(IM,JM) = U(IM,JM)
   20 CONTINUE
      ZETA(IMAX2,JM) = 0.0
   10 CONTINUE
      DO  55  JM = 1,JMAX
      DO  50  IM = 1,IMAX2
      UC(IM,JM) = ZETA(IM,JM)
   50 CONTINUE
   55 CONTINUE
C                                                                       00376300
C     ...NOW SPREAD THE V COMPONENTS                                    00376400
      DO  30  JM = 1,JMAX
C                                                                       00376600
C                                                                       00376700
      DO  40  IM = 1,IMAX
C                                                                       00376900
      ZETA(IM,JM) = V(IM,JM)
   40 CONTINUE
      ZETA(IMAX2,JM) = 0.0
   30 CONTINUE
      DO  77  JM = 1,JMAX
      DO  70  IM = 1,IMAX2
      VC(IM,JM) = ZETA(IM,JM)
   70 CONTINUE
   77 CONTINUE
C     ...GRID DISTANCE ALONG THE DIAGONAL AT 22.5 DEG.                  00377900
      C3 = 3.1854E07 * 1.414
C     ...CONVERT TO CM.                                                 00378100
      CC = 51.4791 / 1.414
      KM = IMAX
      LM = JMAX
      KM2 = KM + 1
      LM2 = LM + 1
      DO  200  J = 1,JMAX
      DO  150  I = 1,IMAXM1
      UU = (UC(I,J) - VC(I,J)) * CC
      VV = (UC(I,J) + VC(I,J)) * CC
      UC(I,J) = UU
      VC(I,J) = VV
  150 CONTINUE
      UC(IMAX,J) = UC(1,J)
      UC(IMAX2,J) = UC(2,J)
      VC(IMAX,J) = VC(1,J)
      VC(IMAX2,J) = VC(2,J)
  200 CONTINUE
C     ...CALCULATE FINITE DIFFERENCE VORTICITY FIELD...                 00379900
C     ...ZETA IS THE VORTICITY OF THE U/V FIELD...                      00380000
      DO  350  J = 2,LM
      DO  350  I = 2,KM
      ZETA(I,J)= C3*((VC(I,J-1)+UC(I-1,J-1))/XM(J-1)-(VC(I-1,J)+
     1         UC(I,J)) / XM(J))
  350 CONTINUE
      X1 = C3 / XM(1)
      X2 = C3 / XM(51)
      DO  360  I = 2,KM
      ZETA(I,1) = -X1 * (UC(I-1,1) + VC(I-1,1))
      ZETA(I,LM2) = X2 * (UC(I-1,LM) + VC(I,LM))
  360 CONTINUE
      CALL POTEX(ZETA)
      RETURN
      END
