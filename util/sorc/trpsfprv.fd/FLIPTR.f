      SUBROUTINE  FLIPTR(STFMX,STFMY,FLD1)
C     ...GIVEN... STFMX(116,51)  STREAMS (IN CM)                        00364200
C     ... TASK... TO FLIP GRID ONTO ITS END                             00364300
C                 AND EXPAND GRID IN MERIDIONAL SENSE                   00364400
C                      ONE ON EACH SIDE OF GREENWICH MERIDIAN ...       00364500
C                 AND TO CLIP THE GRID FROM 51 TO 48 IN LATITUDINAL     00364600
C     ...RESULTS... FLD1(48,119)  READY FOR CNTR (HAVING BEEN SCALED)   00364700
C                                                                       00364800
C     ...THE STFMY(51,117) IS USED AS SCRATCH ...                       00364900
C                                                                       00365000
c      DIMENSION  STFMX(116,51)
      dimension stfmx(117,51)
      DIMENSION  STFMY(51,117)
      DIMENSION  FLD1(48,119)
C                                                                       00365400
      LOGICAL    LGCOL1
      DATA     M2 / 48 /
C     ... WHERE THE M2 OF 48 CUTS PART OF ROW OFF ...                   00365700
      DATA     CMTOM / 0.01 /
      DATA     STDHGT / 111.0 /
      DATA     CINTVL / 0.033333/
C      ...WHERE CONTOUR INTERVAL OF 30M IS 1/30 IS 0.033333             00366100
C                                                                       00366200
      LGCOL1 = .TRUE.
C     ... WHERE LGCOL1 FLAGS THE GREENWICH MERIDIAN FOUND IN COL1       00366400
C     ... WHEN YOU FIGURE OUT WHERE POTEX LEAVES THE GRID               00366500
C     ...   WHETHER GREENWICH IS FIRST OR LAST COL                      00366600
C     ...     THEN YOU CAN GET RID OF ONE OF THESE BRANCHES ...         00366700
      IF(LGCOL1) GO TO 300
C     ... OTHERWISE, ASSUME GREENWICH WAS COL 116                       00366900
      GO TO 400
C                                                                       00367100
  300 CONTINUE
C     ... GREENWICH WAS ALREADY IN COL 1 ...                            00367300
      DO  322  II = 1,116
      DO  311  JJ = 1,51
      JR = 51 + 1 - JJ
      STFMY(JJ,II) = STFMX(II,JR)
  311 CONTINUE
  322 CONTINUE
C     ... IN ORDER TO REPEAT GREENWICH AT 117                           00368000
      II = 1
      K = 117
      DO  333  JJ = 1,51
      JR = 51 + 1 - JJ
      STFMY(JJ,K) = STFMX(II,JR)
  333 CONTINUE
      GO TO 440
C                                                                       00368800
  400 CONTINUE
      K = 1
      II = 116
      DO  411  JJ = 1,51
      JR = 51 + 1 - JJ
      STFMY(JJ,K) = STFMX(II,JR)
  411 CONTINUE
      DO  419  II = 1,116
      K = II + 1
      DO  415  JJ = 1,51
      JR = 51 + 1 - JJ
      STFMY(JJ,K) = STFMX(II,JR)
  415 CONTINUE
  419 CONTINUE
      GO TO 440
C                                                                       00370400
  440 CONTINUE
C     ... NOW STFMY(51,117) CONTAINS THE STREAM FUNCTION FIELD          00370600
C     ... STANDING ON ITS GREENWICH MERIDIAN END ...                    00370700
C                                                                       00370800
      JORIG = 115
C     ... WHERE JORIG POINTS TO ROW IN 117 ROW GRID                     00371000
      DO  448  JDEST = 1,119
      JORIG = JORIG + 1
      IF(JORIG .GT. 116) JORIG = JORIG - 116
      DO  444  II = 1,M2
      FLD1(II,JDEST) = (STFMY(II,JORIG)*CMTOM + STDHGT) * CINTVL
  444 CONTINUE
  448 CONTINUE
C                                                                       00371800
      RETURN
      END
