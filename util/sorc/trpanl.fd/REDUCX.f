      SUBROUTINE REDUCX
C     ...GIVEN ... STREAM FUNCTION FIELD PREPARED BY POTEX              00392400
C     ...             IN /STRPOT/ STFM(118,52)                          00392500
C     ...TASK ... TO SMOOTH, RESCALE, AND TRANSFER TO A SMALLER GRID    00392600
C     ...             IN /ISPACE/ STFMX(116,51)                         00392700
C     ...         IN A FORM READY FOR MERC                              00392800
C     ...WHAT ARE UNITS OF GIVEN FIELD...                               00392900
C     ...WHAT ARE UNITS OF RESULTING FIELD QQQQQ                        00393000
      COMMON /ISPACE/ STFMX(116,51),XTRA(12256)
      COMMON  /STRPOT/ STFM(118,52)
      CFB = 3.14159 / 2.16E+04
      SSF = (980.0 * SQRT(2.0)) / CFB
      DO  200  I = 2,117
      DO  200  J = 1,51
      VAL = STFM(I,J) + STFM(I+1,J) + STFM(I,J+1) + STFM(I+1,J+1)
      IF(J .EQ. 1) GO TO 10
      IF(J .EQ. 51) GO TO 10
      IF(I .EQ. 117) GO TO 17
      STFMX(I,J)=(81.0*VAL - 9.0*(STFM(I,J-1)+STFM(I+1,J-1)+STFM(I+2,J)
     1          +STFM(I+2,J+1)+STFM(I+1,J+2)+STFM(I,J+2)+STFM(I-1,J+1)
     2          +STFM(I-1,J))
     3          +STFM(I-1,J-1) + STFM(I+2,J-1) + STFM(I+2,J+2)
     4          +STFM(I-1,J+2)) / 256.0
      STFMX(I,J) = STFMX(I,J) / SSF
      GO TO 200
   10 IF(I .EQ. 117) GO TO 11
      STFMX(I,J) = VAL / (4.0 * SSF)
      GO TO 200
   11 STFMX(1,J) = VAL / (4.0 * SSF)
      GO TO 200
   17 CONTINUE
      STFMX(1,J) = (81.0*VAL - 9.0*(STFM(I,J-1)+STFM(I+1,J-1)+STFM(3,J)
     1          +STFM(3,J+1)+STFM(I+1,J+2)+STFM(I,J+2)+STFM(I-1,J+1)
     2          +STFM(I-1,J))
     3          +STFM(I-1,J-1)+STFM(3,J-1)+STFM(3,J+2)
     4          +STFM(I-1,J+2)) / 256.0
      STFMX(1,J) = STFMX(1,J) / SSF
  200 CONTINUE
      RETURN
      END
