      SUBROUTINE CLOSES(Z,IMAX,JMAX,S,A,B,M,IUP,LPLMI,IFF,JSTPK,
     1   JSTPL,LOX,ITABMB,ITABFL,MXITR)
C                                                                       00200600
C     ...PURPOSE...A MERCATOR CENTER-FINDER WHICH CALLS SUBR CLOSET     00200700
C     ...AFTER FINDING EACH CENTER IN ORDER TO LABEL CONTOURS           00200800
C     ...IN THE VICINITY OF CENTERS.                                    00200900
C     ...IUP IS THE DISTANCE (IN GRID INTERVALS) FROM THE CENTER        00201000
C     ...WITHIN WHICH CONTOURS WILL BE LABELLED                         00201100
C     ...WHERE LOX=1, SENDS THIS ON TO INDIRECT CONTOUR LABELS          00201200
C     ...      LOX=0  MEANS TO PUT REGULAR CONTOUR LABELS ON.           00201300
C                                                                       00201400
      REAL   INDEF,KDEF1,KDEF2
      DIMENSION IFF(5)
      DIMENSION Z(IMAX,JMAX)
      DIMENSION ITABMB(MXITR),ITABFL(MXITR)
      DIMENSION MARK4(4)
      DIMENSION ZZ(9)
      DIMENSION MARKS(50)
C     ...WHERE MARKS ARRAY TO KEEP TRACK OF NEARBY CONTOUR LABELS       00202200
      DIMENSION JSTPK(30)
C     ...WHERE JSTPK IS AN ARRAY OF J COORDINATES WHICH HAVE STRIP      00202400
C     ...LABELS ALREADY AND DO NOT NEED THESE LABELS.                   00202500
C     ...FIRST ZERO ENDS JSTPK LOOK UP.                                 00202600
      DIMENSION JSTPL(30)
C     ...WHERE JSTPL IS A CORRESPONDING ARRAY TO JSTPK GIVING THE       00202800
C     ...NUMBER OF GRID POINTS ON THIS LINE TO RESERVE.                 00202900
      DATA    INDEF/Z'7FFFFFFFFFFFFFFF'/
      DATA    NSK/30/
C     ...WHERE NSK IS DIMENSION OF JSTPK ARRAY OF J ALREADY LABELLED    00203200
      DATA    MARK4/Z'4',Z'8',Z'10',Z'20'/
      DATA    NFEW/2/
C                                                                       00203500
C     ...INITIALIZE FOR CENTER SEARCH                                   00203600
C                                                                       00203700
      print *,'ENTERING CLOSES '
      IMIN=1
      JMIN=1
      ILOW=0
      IHIGH=0
      DO 11 I=IMIN,IMAX
      MARKS(I)=0
   11 CONTINUE
C     ...TO TEST FOR STRIP LABELLED J ROWS IN FIRST FEW ROWS            00204500
      J=JMIN
      DO 16 ILLE=1,NFEW
      DO 12 ICK=1,NSK
      ICKSV=ICK
      IF(JSTPK(ICK).EQ.0) GO TO 15
      IF(J.EQ.JSTPK(ICK)) GO TO 13
   12 CONTINUE
      GO TO 15
C     ...COMES TO 13 IF MATCHING J FOUND                                00205400
   13 CONTINUE
      IMAD=JSTPL(ICKSV)
      IF(IMAD.LE.0) GO TO 15
      IF(IMAD.GT.IMAX) IMAD=IMAX
      DO 14 IMA=IMIN,IMAD
      MARKS(IMA)=LOR(MARKS(IMA),MARK4(ILLE))
   14 CONTINUE
   15 J=J+1
   16 CONTINUE
C                                                                       00206400
      DO 105 J=JMIN,JMAX
      JP2=J+NFEW
C     ...TEST FOR STRIP LABELS ALREADY ON JP2 ROW                       00206700
      DO 20 ICK=1,NSK
      ICKSV=ICK
      IF(JSTPK(ICK).EQ.0) GO TO 27
      IF(JP2.EQ.JSTPK(ICK)) GO TO 22
   20 CONTINUE
      GO TO 27
   22 CONTINUE
      IMAD=JSTPL(ICKSV)
      IF(IMAD.LE.0) GO TO 27
      IF(IMAD.GT.IMAX) IMAD=IMAX
      DO 24 IMA=IMIN,IMAD
      MARKS(IMA)=LOR(MARKS(IMA),MARK4(3))
   24 CONTINUE
      IF(IMAD.GE.IMAX) GO TO 101
C     ...WHICH SKIPS CENTERFINDING IN THIS J ROW BECAUSE OF STIP LABELS 00208200
   27 CONTINUE
      DO 100 I=IMIN,IMAX
C                                                                       00208500
C     ...TEST FOR BORDER VALUES                                         00208600
C                                                                       00208700
      IF(I.LE.IMIN+1.OR.I.GE.IMAX-1) GO TO 100
      IF(J.LE.JMIN+1.OR.J.GE.JMAX-4) GO TO 100
C                                                                       00209000
      ZZ(1)=Z(I,J)
      ZZ(2)=Z(I+1,J)
C                                                                       00209300
C     ...TEST FOR UNDEFINED VALUES                                      00209400
C                                                                       00209500
      KDEF1=ZZ(1)
      KDEF2=ZZ(2)
      IF((KDEF1.EQ.INDEF).OR.(KDEF2.EQ.INDEF)) GO TO 100
      ZZ(3)=Z(I+1,J+1)
      ZZ(4)=Z(I,J+1)
      ZZ(5)=Z(I-1,J+1)
      ZZ(6)=Z(I-1,J)
      ZZ(7)=Z(I-1,J-1)
      ZZ(8)=Z(I,J-1)
      ZZ(9)=Z(I+1,J-1)
C                                                                       00210600
C     ...TEST FOR LOW CENTER                                            00210700
C                                                                       00210800
      IF(ZZ(1).GE.ZZ(2)) GO TO 30
C     ...OTHERWISE TEST FOR LOW CENTER                                  00211000
      DO 29 IST=3,9
      KDEF1=ZZ(IST)
      IF(KDEF1.EQ.INDEF) GO TO 100
      IF(IST.GE.6) GO TO 28
C     ...WAS .LT.6)                                                     00211500
      IF(ZZ(1).GE.ZZ(IST)) GO TO 100
      GO TO 29
   28 IF(ZZ(1).GT.ZZ(IST)) GO TO 100
   29 CONTINUE
C     ...WHEN IT FALLS THRU THIS LOOP, A LOW CENTER HAS BEEN FOUND      00212000
C                                                                       00212100
C     ...FOUND LOW CENTER                                               00212200
C                                                                       00212300
      ITYPE=1
      ILOW=ILOW+1
      GO TO 50
C                                                                       00212700
C     ...TEST FOR HIGH CENTER                                           00212800
C                                                                       00212900
   30 CONTINUE
      DO 33 IST=2,9
      KDEF1=ZZ(IST)
      IF(KDEF1.EQ.INDEF) GO TO 100
      IF(IST.GE.6) GO TO 32
      IF(ZZ(1).LE.ZZ(IST)) GO TO 100
      GO TO 33
   32 IF(ZZ(1).LT.ZZ(IST)) GO TO 100
   33 CONTINUE
C                                                                       00213900
C     ...FOUND HIGH CENTER                                              00214000
C                                                                       00214100
      ITYPE=2
      IHIGH=IHIGH+1
   50 CONTINUE
C                                                                       00214500
C     ...FOUND CENTER-GO GET CONTOUR LABEL                              00214600
C                                                                       00214700
      IFIX=I
      JFIX=J
      I2=IFIX+IUP
      DO 70 IMI=IFIX,I2
      IF(IMI.GT.IMAX) GO TO 72
      IF(MARKS(IMI).NE.0) GO TO 100
C     ...SKIPS CONTOUR LABELLING FOR THIS CENTER IF OTHERS TOO CLOSE    00215400
   70 CONTINUE
C     ...ALL CLEAR FOR CONTOUR LABELS,  ENTER MARKS FOR THIS SET.       00215600
   72 DO 74 IMI=IFIX,I2
      IF(IMI.GT.IMAX) GO TO 75
      MARKS(IMI)=LOR(MARKS(IMI),MARK4(1))
   74 CONTINUE
   75 CONTINUE
       print * ,' 9999 CLOSET FROM CLOSES'
      CALL CLOSET(Z,IMAX,JMAX,S,A,B,M,IUP,IFIX,JFIX,LPLMI,IFF,
     1   LOX,ITABMB,ITABFL,MXITR)
       print * ,' 9999  exiting CLOSET FROM CLOSES'
  100 CONTINUE
  101 DO 102 IMA=IMIN,IMAX
c      MARKS(IMA)=SHFTR(MARKS(IMA),1)
      MARKS(ima)=ishft(marks(ima),-1)
 187  format(3z20)
  102 CONTINUE
C     ...WHICH PUSHES DOWN THE STACK OF MARKS OFF END                   00216800
  105 CONTINUE
      print *, 'DONE WITH CLOSES 9999'
      RETURN
      END
