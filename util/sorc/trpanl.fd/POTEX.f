      SUBROUTINE POTEX(ZETA)
C     GIVEN... VORTICITY FIELD IN ZETA(118,52)                          00381600
C     TASK... TO FORM A STREAM FUNCTION FIELD IN /STRPOT/STRS(118,52)   00381700
C     ...         USING A RELAXATION PROCEDURE...                       00381800
C     ...THEN CALLS ON SUBROUTINE REDUCX TO REFORMAT FOR OUTPUT...      00381900
C     ...WHERE ARE THESE POINTS OF THIS GRID QQQQQ                      00382000
C     ...WHAT ARE THE UNITS OF GIVEN INFO QQQ                           00382100
C     ...WHAT ARE UNITS OF RESULTING INFO QQ                            00382200
      COMMON  /STRPOT/ STRS(118,52)
      DIMENSION  ZETA(118,52)
      DATA AVHGT / 111.0 /
C     ...PROGRAM CONSTANTS ...                                          00382600
      RESMA1 = 0.3E+09
      N3 = 1000
      IM = 117
      JM = 51
      IM2 = IM + 1
      JM2 = JM + 1
C                                                                       00383300
C                                                                       00383400
C                                                                       00383500
C     ...IF NO STREAM FIELD IS AVAILABLE, THEN SET FIELD TO AVERAGE     00383600
C     VALUE  FOR EACH LEVEL (IN CM)...                                  00383700
C                                                                       00383800
C     ... ASSUME STRS ARRAY HAS BEEN INITIALIZED TO A GES VALUE         00383900
C     ...     BY THE MAIN PROGRAM OR BY SUBR GETGES ...                 00384000
C                                                                       00384100
C                                                                       00384200
      X1 = 0.0
C     ...X1 IS MEAN VALUE OF ZETA1(ZETA ON THE EVEN ROWS)               00384400
      X2 = 0.0
C     ...X2 IS MEAN VALUE OF ZETA2(ZETA ON THE ODD ROWS)                00384600
      X3 = 1.0
C     ...X3 IS THE EVEN/ODD FLAG                                        00384800
      DO  222  J = 1,JM2
      DO  200  I = 2,IM
      X3 = -X3
      IF(X3) 166,166,188
  166 X1 = X1 - ZETA(I,J)
      GO TO 200
  188 X2 = X2 - ZETA(I,J)
  200 CONTINUE
      X3 = - X3
  222 CONTINUE
C     ...FORM MEAN VALUE OF ZETA                                        00385900
      X1 = X1 / (58.0 * 52.0 )
      X2 = X2 / (52.0 * 58.0 )
      X3 = 1.0
C                                                                       00386300
C                                                                       00386400
C                                                                       00386500
      DO  288  J = 1,JM2
      DO  277  I = 2,IM
      X3 = -X3
C     ...SET ZETA MEAN=0                                                00386900
      IF(X3) 255,255,266
  255 ZETA(I,J) = ZETA(I,J) + X1
      GO TO 277
  266 ZETA(I,J) = ZETA(I,J) + X2
  277 CONTINUE
      X3 = -X3
  288 CONTINUE
      M1 = 1
      N = 0
  311 CONTINUE
      M = 0
      DO  488  J1 = 1,JM2
      IF(M1) 322,322,333
  322 J = J1
      GO TO 344
  333 J = JM2 + 1 - J1
  344 CONTINUE
      DO  477  I = 2,IM
      IF(J - 1) 411,422,411
  411 IF(J - JM2) 444,433,444
  422 RES = STRS(I+1,J+1)+STRS(I-1,J+1)-STRS(I,J)*2.0-ZETA(I,J)
      IF(ABS(RES) - RESMA1) 466,466,455
  433 RES = STRS(I+1,J-1)+STRS(I-1,J-1)-STRS(I,J)*2.0-ZETA(I,J)
      IF(ABS(RES) - RESMA1) 466,466,455
  444 CONTINUE
      RES = STRS(I+1,J+1)+STRS(I-1,J+1)+STRS(I-1,J-1)+
     XSTRS(I+1,J-1)-STRS(I,J)*4.0-ZETA(I,J)
      IF(ABS(RES) - RESMA1) 466,466,455
  455 M = M + 1
  466 CONTINUE
  477 STRS(I,J) = STRS(I,J) + 0.45*RES
      STRS(1,J) = STRS(IM,J)
      STRS(IM2,J) = STRS(2,J)
  488 CONTINUE
      M1 = -M1
      IF(M .EQ. 0) GO TO 611
      N = N + 1
      IF(N - N3) 311,522,522
  522 CONTINUE
      N2 = N
      N4 = 0
      N1 = 0
      PRINT  544, N
  544 FORMAT(1H0, 10X, '***ERROR RETURN FROM POTEX.  RELAXATION INCOMPLE
     1TE AFTER', I5, 3X, 'ITERATIONS.  STREAM FIELD IS NO GOOD.')
      RETURN
  611 CONTINUE
      PRINT  633, N
  633 FORMAT(1H0, 10X, 'RELAXATION COMPLETED IN POTEX.  USED', I5,
     1       2X, 'ITERATIONS TO GET STREAM FIELD')
      CALL REDUCX
      RETURN
      END
