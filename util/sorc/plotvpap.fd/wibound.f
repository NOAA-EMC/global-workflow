      SUBROUTINE WIBOUND(KRUN,KEIL,KLLGO2,LINTERPQQ,ALAT,ALONG,
     1                    FLD,IMAX,JMAX,HGTM,LBOUND)
C                .      .    .                                       .
C SUBPROGRAM:    WIBOUND    PERFORM BOUNDARY-TEST ON ONE LAT/LON PT 
C   PRGMMR: SHIMOMURA        ORG: W/NP12     DATE: 1997-01-07
C
C ABSTRACT: PERFORM BOUNDARY-TEST ON THE GIVEN ALAT,ALONG LOCATION
C   TO DETERMINE WHETHER THIS OBSERVATION IS WITHIN BOUNDS OR NOT.
C   ALSO INTERPOLATES WITHIN A HEIGHT FIELD (UNDER CERTAIN OPTIONS) 
C   AND RETURNS THE INTERPOLATED HEIGHT VALUE IN 'HGTM' 
C   (FOR USE IN CONVERTING A THICKNESS TO A HEIGHT)
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR  UNKNOWN
C   89-04-27  STEVE LILLY  ADD DOCUMENTATION BLOCK
C   93-05-04  LILLY CONVERT SUBROUTINE TO FORTRAN 77
C   97-01-07  SHIMOMURA - CONVERT TO RUN ON CRAY;
C                       - ADDED DOCUMENTATION AND COMMENTS;
C                       - REPLACED W3FB00, AND W3FB02, WITH W3FB04 CALL
C                       - CHANGED SUBR NAME FROM "TBOUND" TO "WIBOUND"
C                           BECAUSE I MADE MANY CHANGES TO CALL SEQUENCE
C                       - ADDED FLD(IMAX,JMAX) TO CALL SEQUENCE
C                       - ADDED EXPLICIT LOGICAL ARG4: LINTERPQQ
C                           TO PERFORM THE INTERPOLATION IN FLD()
C   97-03-11  SHIMOMURA - MODS FOR 1:20M NRN AND SRN HEMI FOR 250MB PLOT
C
C USAGE:    CALL WIBOUND(KRUN,KEIL,KLLGO2,LINTERPQQ,ALAT,ALONG,
C                       FLD,IMAX,JMAX,HGTM,LBOUND)
C
C   INPUT ARGUMENT LIST:
C     KRUN     - FOR SETTING OPTIONS BY RUN TYPE, SUCH AS =11 TROPIC
C     KEIL     - KEIL SPECIFIES THE GRID FOR CALL TO TRUIJX(,,,,KEIL,) 
C     KLLGO2   - PRIMARY FLOW CONTROLLER TO SELECT THE TYPE OF TESTING
C                     AND THE VALUES OF BOUNDARIES

C                =1;  DO NOTHING AND RETURN IMMEDIATELY;

C                =2 OR =3;  NH 1:40M OR N AMER 1:20M
C                                   (1.0,1.0) (AIMAX,AJMAX)

C                =4;  NH 1:20M      (1.0,1.0) (109.0,81.0)

C                =5;  TROPICAL MAPS OR GOES -- MR4001, GH2601, GH2602
C                                   (60S,0.)   (60N,360.)

C                =6;  SH 1:40M      (1.0,1.0) (AIMAX,AJMAX)

C                =7;  NH 1:60M      (1.0,1.0) (65.0,65.0)

C     LINTERPQQ - = .TRUE.  IF INTERPOLATION IN FLD(IMAX,JMAX) IS
C                              DESIRED
C
C     ALAT     - THE LATITUDE OF THE OBSERVATION
C     ALONG    - THE LATITUDE OF THE OBSERVATION
C
C     FLD(IMAX,JMAX) - REAL FLD(IMAX,JMAX)
C              - GIVEN HGT FIELD WITHIN WHICH I WILL INTERPOLATE
C              - IN ORDER TO OBTAIN 'HGTM'
C                            
C     COMMON   - /WLONG0/ WLONG0 	!... USED ONLY BY GOESXY()
C                               GOESXY() IS CALLED ONLY UNDER KLLGO2=5;
C                                            WHEN KRUN==22, OR KRUN==23
C
C   OUTPUT ARGUMENT LIST:
C     HGTM     - THE INTERPOLATED VALUE, WITHIN FLD(IMAX,JMAX),
C                    TO THE LAT/LON POINT
C     LBOUND   - A TRUE/FALSE RESULT AS TO WHETHER THIS REPORT
C              - PASSES OR FAILS THE BOUNDARY TEST
C
C
C REMARKS:
C   SUBROUTINES CALLED: 
C                       W3FB04(); 	LAT/LON TO GRIDI/GRIDJ 
C                       W3FT01();	INTERP W/I FLD(57,57)
C                       TRUIJX();  	LAT/LON TO GRIDI/GRIDJ
C                       GOESXY(); 	LAT/LON TO GRIDI/GRIDJ
C                       INTERP();	INTERP W/I FLD(65,65)
C
C   THE CALL SEQUENCE WAS CHANGED TO INCLUDE THE FLD(IMAX,JMAX)
C     IN ORDER TO AVOID THE COMMON /JSPACE/ DEPENDENCIES;
C
C     ALSO I WILL USE THESE IMAX,JMAX VALUES FOR BOUNDARY TESTS
C     FOR THOSE CASES OF KLLGO2 =2, =3, OR =6 
C        WHERE THE OLD CODE REFERENCED
C        ...  COMMON /IJMAX/ AIMAX,AJMAX ... WHICH HAS BEEN DELETED
C
C   THE CALL SEQUENCE WAS CHANGED TO INCLUDE THE LOGICAL SWITCH FOR ARG4
C     LOGICAL  LINTERPQQ  -- = .T.  TO PERFORM INTERPOLATION IN FLD()
C                            = .F.  TO NOT INTERPOLATE;
C       WHERE THE OLD ARG4 WAS THE UNIT NO. OF INPUT OBS FILE
C       WHICH WAS COMPARED AGAINST THE UNIT NO. OF SATELLITE DATA
C       TO DETERMINE WHETHER TO LET IT GO TO THE INTERPOLATOR;
C       THIS IS A SECONDARY CONTROL, SO IT IS QUERIED ONLY UNDER
C       CERTAIN VALUES OF THE PRIMARY CONTROL: 'KLLGO2' = [2, 3, 4, 6]
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C

      COMMON /WLONG0/ WLONG0		!... USED BY GOESXY()
C
C
C
C ...      REAL    FLD(65,65)
C ...      EQUIVALENCE(BUFF(13),FLD(1,1))
C
C ...      REAL    FLD1(57,57)
C ...      EQUIVALENCE(BUFF(13),FLD1(1,1))
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C     ... USAGE:CALL WIBOUND(KRUN,KEIL,KLLGO2,LINTERPQQ,ALAT,ALONG,
C     ...                     FLD,IMAX,JMAX, HGTM,LBOUND)
      INTEGER    KRUN		!... RUN TYPE, E.G. KRUN=11 FOR TROPIC
      INTEGER    KEIL		!... GRID TYPE
      INTEGER    KLLGO2		!... PRIMARY FLOW CONTROLLER
      LOGICAL    LINTERPQQ	!... = .T. IF INTERPOLATING IN FLD()
C
      REAL       ALAT
      REAL       ALONG
      REAL       FLD(IMAX,JMAX)

      REAL       HGTM
      LOGICAL    LBOUND
      
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
      REAL       DISSAT
      DATA       DISSAT         / 6.619 /
      REAL       SCALE
      DATA       SCALE          / 26.7858/
C
      REAL       XMESHL
      REAL       ORIENT
      INTEGER    KQUAD

      SAVE  

      AIMAX = FLOAT(IMAX)
      AJMAX = FLOAT(JMAX)
      HGTM = 0.0
      LBOUND = .TRUE.
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
      IF(KLLGO2 .EQ. 1) THEN
C       ... BYPASS BOUNDARY TESTS ... JUMP OUT; DO NOTHING
        RETURN
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
      ELSE IF((KLLGO2 .EQ. 2) .OR. (KLLGO2 .EQ. 3)) THEN
C       ... NH 1:40M AND NA 1:20M ARE BOTH DONE HERE ...
C
        IF(ALAT .LT. 0.0) GO TO 1000

        CALL TRUIJX(ALAT,ALONG,XI,XJ,KEIL,IRET_TIJ)
        IF(IRET_TIJ .NE. 0) GO TO 1000
        IF(XI .LT.   1.0) GO TO 1000
        IF(XI .GT. AIMAX) GO TO 1000
        IF(XJ .LT.   1.0) GO TO 1000
        IF(XJ .GT. AJMAX) GO TO 1000
        IF(.NOT. LINTERPQQ) RETURN
        IF(KRUN .EQ. 9) THEN
C          ... TREAT THE 2-DOT CASE HERE ...
C
           CALL W3FB04(ALAT,ALONG,190.5,105.0,XII,XJJ)

           XII = XII + 29.0
           XJJ = XJJ + 49.0

           CALL W3FT01(XII,XJJ,FLD,HGTM,IMAX,JMAX,0,0)

           RETURN
        ENDIF

        IF(KRUN .EQ. 10) RETURN
C       ... OTHERWISE, KRUN IS NOT 9 AND NOT 10 ...
        GO TO 900		!... TO INTERPOLATE W/I FLD 
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
      ELSE IF(KLLGO2 .EQ. 4) THEN
C       ...  NH 1:20M DONE HERE ...
C
        IF(ALAT .LT. 0.0) GO TO 1000
        CALL TRUIJX(ALAT,ALONG,XI,XJ,KEIL,IRET_TIJ)
        IF(IRET_TIJ .NE. 0) GO TO 1000
        IF(XI .LT.   1.0) GO TO 1000
        IF(XI .GT. 109.0) GO TO 1000
        IF(XJ .LT.   1.0) GO TO 1000
        IF(XJ .GT.  81.0) GO TO 1000
        IF(LINTERPQQ) GO TO 900 		!... TO INTERP IN FLD
        RETURN
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
      ELSE IF(KLLGO2 .EQ. 5) THEN
C       ... TROPICAL PLOT HERE ON MR4001, GH2601, OR GH2602 ...
C
        IF((KRUN .EQ. 22) .OR. (KRUN .EQ. 23)) GO TO 550
        IF(ALAT .LT. -60.0) GO TO 1000
        IF(ALAT .GT.  60.0) GO TO 1000
        IF(ALONG .LT.  0.0) GO TO 1000
        IF(ALONG .GT. 360.) GO TO 1000
        RETURN

  550   CONTINUE
C       ... DO PSEUDO-GOES HERE ...
C
        CALL GOESXY(WLONG0,DISSAT,SCALE,ALAT,ALONG,XI,XJ,IEXIT)
        IF(IEXIT .NE. 0) GO TO 1000

        RETURN
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
      ELSE IF(KLLGO2 .EQ. 6) THEN
C       ... THIS IS A 1:40M PLOT IN THE SH ...
C
        IF(ALAT .GT. 0.0) GO TO 1000
        CALL TRUIJX(ALAT,ALONG,XI,XJ,KEIL,IRET_TIJ)
        IF(IRET_TIJ .NE. 0) GO TO 1000
        if(krun .eq. 20) then
C         ... this is big srn hemi 1:20 M ...
          IF(XI .LT.   1.0) GO TO 1000
          IF(XI .GT. 109.0) GO TO 1000
          IF(XJ .LT.   1.0) GO TO 1000
          IF(XJ .GT. 144.0) GO TO 1000
        else
          IF(XI .LT.   1.0) GO TO 1000
          IF(XI .GT. AIMAX) GO TO 1000
          IF(XJ .LT.   1.0) GO TO 1000
          IF(XJ .GT. AJMAX) GO TO 1000
        ENDIF
        IF(.NOT. LINTERPQQ) RETURN
C
C       ... THE SH PE GRID ...
C
        XMESHL = -381.0		!... NMC STD GRID LENGTH; NEG SIGN (SH)
        ORIENT = 260.0		!... = 80W AT TOP OF SRN HEMI MAP
        CALL W3FB04(ALAT,ALONG,XMESHL,ORIENT,XII,XJJ)
        GO TO 910
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
      ELSE IF(KLLGO2 .EQ. 7) THEN
C       ... THIS IS A 1:60M NH PLOT ...
C
        IF(ALAT .LT. 0.0) GO TO 1000
        CALL TRUIJX(ALAT,ALONG,XI,XJ,KEIL,IRET_TIJ)
        IF(IRET_TIJ .NE. 0) GO TO 1000
        IF(XI .LT.  1.0) GO TO 1000
        IF(XJ .LT.  1.0) GO TO 1000
        IF(XI .GT. 65.0) GO TO 1000
        IF(XJ .GT. 65.0) GO TO 1000
        RETURN
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
      ELSE
C
C       ... ERROR. THE GIVEN KLLGO2 HAS INVALID NUMBER ...
C       ...        BYPASS BOUNDARY TEST.
C
        WRITE(6, 2010) KLLGO2
 2010   FORMAT(1H ,'WIBOUND: ERROR - Given BAD-VALED KLLGO2 = ',I4,
     1      /1H ,'        ACCEPTABLE VALUES = [1 TO 7]')
C
        RETURN

      ENDIF
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
  900 CONTINUE
C
C     ... INTERPOLATE 1000MB HEIGHTS HERE ...
C     ...    BUT BEFORE INTERPOLATING, WHAT IS I/J ??
C
      xmeshl = 381.0		!... 381km = NMC std g.i.
      orient = 80.0		!...  80W longitude is vertical meridian
      CALL W3FB04(ALAT,ALONG,xmeshl,orient,XII,XJJ)

  910 CONTINUE
      XII = XII + 33.0
      XJJ = XJJ + 33.0
      KQUAD = 6
      IF( (XII .LE. 2.0) .OR. (XII .GE. 64.0)) KQUAD = 5
      IF( (XJJ .LE. 2.0) .OR. (XJJ .GE. 64.0)) KQUAD = 5

      CALL INTERP(FLD,imax,jmax,HGTM,XII,XJJ,KQUAD)

      RETURN
C
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
 1000 CONTINUE
C     ... THIS REPORT IS OUT-OF-BOUNDS  FROM A BOUNDARY TEST ...
      LBOUND = .FALSE.
      RETURN
C
C     . . . . . . . . . . . . . . . . . . . . .
C
      END
