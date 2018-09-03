      SUBROUTINE AFZOOM (NAME,KTTYP,PSOWDT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    AFZOOM      SET ZOOM & TYPE FOR AN AFOS PLOTFILE STN
C   PRGMMR:SHIMOMURA         ORG: W/NP12    DATE: 96-08-06
C
C ABSTRACT: THE GIVEN STATION-NAME IS MATCHED AGAINST A TABLE OF 
C   NAMES WITH CORRESPONDING ZOOM-THRESHOLDS, SO THAT IF A MATCH IS
C   FOUND, THEN THAT ZOOM-THRESHOLD VALUE IS PUT INTO THE RESULTS:
C   THE PSOWDT CHARACTER ARRAY.
C   AND THE OBSERVATION TYPE IS FORMATTED FOR THE PSOWDT AND PUT IN
C   THERE.  THE PSOWDT IS A REQUIRED ELEMENT IN THE AFOS PLOTFILE
C   DATA LINE.
C
C PROGRAM HISTORY LOG:
C   89-04-26  ORIGINAL AUTHOR HENRICHSEN
C   89-05-28  HENRICHSEN  ADDED STATION 72233 TO ZOOM 0 LIST.
C   89-07-20  GLORIA DENT CHANGE THE AFOS ZOOM THRESHOLD OF 74732
C               (HOLLOMAN A.F. BASE,N.M.)FROM ZOOM 4-1 TO ZOOM 16-1.
C               ADD THIS STATION TO ZOOM 3 LIST.
C   93-05-05  LILLY CONVERT SUBROUTINE TO FORTRAN 77
C   96-08-06  SHIMOMURA: RE-WRITE FOR THE CRAY; COMBINED 3 TABLES INTO 1
C                        AND PUT INTO SORTED ORDER FOR FASTER SEARCH.
C                        
C
C USAGE:    CALL AFZOOM(NAME,KTTYP,PSOWDT)
C   INPUT ARGUMENT LIST:
C     NAME     - INTEGER  WORD  CONTAINING 5-DIGIT STATION NAME IN
C              - HOLLERITH. IE '72308 '. WHICH IS CHARLSTON SC.
C     KTTYP    - INTEGER   NUMBER  FROM 1 THRU 13 WHICH INDICATES THE
C              - TYPE OF REPORT. THIS IS USED TO SET THE LAST BYTE OF
C              - PSOWDT.
C
C   OUTPUT ARGUMENT LIST:
C     PSOWDT   - CHARACTER*1 PSOWDT(6) -- SIX-CHARACTER ARRAY  
C                     DESTINED FOR ONE OF THE AFOS PLT FORMAT ELEMENTS
C              - WHERE THE FIRST BYTE IS THE ZOOM-THRESHOLD VALUE.
C              - AND WILL BE SET TO 0,1,2, OR 3.
C              - BYTES 2 THRU 4 ARE 0.
C              - BYTE 5 IS SET TO 1 FOR NH PLOT OPTION.
C              - BYTE 6 IS REPORT TYPE. SEE REMARKS.
C                DEFAULT PSOWDT = "10001A"
C                             ...  123456  ...
C                WHERE DEFAULT ZOOM WHEN NO MATCHING STN NAME IN TABLE
C                      DEFAULT TYPE WHEN GIVEN INVALID KTTYP
C
C   OUTPUT FILES:
C     FT06F001 - STANDARD PRINT FILE.
C
C REMARKS: REPORT TYPE CAN HAVE THE FOLLOWING VALUES;
C   A = UPPER LAND, B = UPPER OCEAN, C = RECON., D = AIRCRAFT,
C   G = UPPER BOGUS, E = SIRS, F = SAT WINDS.
C
C   CALLS ON FUNCTION IJBSIRCH TO DO THE TABLE LOOK-UP
C
C                  AREA
C                 SCALE  ZT
C                   1:1  =0    TO DISPLAY AT  ALL ZOOM LEVELS
C                   4:1  =1    T0 DISPLAY AT  4:1 AND BELOW
C                   9:1  =2    TO DISPLAY AT  9:1 AND BELOW
C                  16:1  =3    TO DISPLAY AT 16:1 AND BELOW
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C
C     INTEGER     IJBSIRCH
C     EXTERNAL    IJBSIRCH  		!... DECLARE FUNCTION
      
      INTEGER     MAXNSTNS
      PARAMETER  (MAXNSTNS=46)
      INTEGER     MAXTYPES
      PARAMETER  (MAXTYPES=13)

      INTEGER     NAME  		!... ARG1: BLK+STN_NUM
      INTEGER     KTTYP                 !... ARG2: REPORT TYPE
      CHARACTER*1 PSOWDT(6)  		!... ARG3: PSOWDT W/ P & D & T
C
C   TWO STATIONS (72357 AND 72293) WERE ADDED TO ZOOM0 LIST(89/04/14).
C   STATION (74732) WAS ADDED TO THE ZOOM3 LIST(89/07/20).


      INTEGER    STNZM(2,MAXNSTNS)
      DATA       STNZM      /
     1  "04360   " ,3,        "25399   " ,3,
     2  "70174   " ,3,        "70273   " ,0,        "70398   " ,0,
     3  "71043   " ,0,        "71114   " ,2,        "71801   " ,0,
     4  "71826   " ,0,        "71913   " ,0,        "71928   " ,2,
     5  "72201   " ,0,        "72209   " ,3,        "72221   " ,2,
     6  "72225   " ,3,        "72228   " ,3,        "72232   " ,0,
     7  "72233   " ,0,        "72239   " ,2,        "72250   " ,0,
     8  "72273   " ,3,        "72290   " ,0,        "72293   " ,0,
     9  "72295   " ,3,        "72303   " ,3,        "72304   " ,0,
     A  "72327   " ,0,        "72353   " ,0,        "72355   " ,3,
     B  "72357   " ,0,        "72381   " ,3,        "72385   " ,3,
     C  "72391   " ,3,        "72402   " ,2,        "72408   " ,3,
     D  "72476   " ,0,        "72493   " ,0,        "72606   " ,0,
     E  "72734   " ,0,        "72764   " ,0,        "72785   " ,0,
     F  "74732   " ,3,
     G  "76225   " ,0,
     H  "78016   " ,0,        "78367   " ,0,
     I  "91021   " ,0  /
C

C
C     LTYPE IS IN ASCII WHERE 3*A'S,3*B'S,  C,2*D'S,  G,  E,2*F'S
C
      CHARACTER*1 LTYPE(MAXTYPES)

C                        ...   1   2   3   4   5   6   7   8   9
      DATA        LTYPE      /'A','A','A','B','B','B','C','D','D',
     1                        'G','E','F','F'/
C                        ...  10  11  12  13


      CHARACTER*1  CON_PSOWDT(6)
      DATA         CON_PSOWDT     /'0','0','0','0','1','A'/
C                              ...  1   2   3   4   5   6
      CHARACTER*1  ZOOMS(4)
      DATA         ZOOMS      /'0','1','2','3'/

      CHARACTER*1  ZOOM1      
      DATA         ZOOM1      /'1'/
C
      INTEGER      INDX
C
      LOGICAL      FOUNDNAME
C
      INTEGER      INTNAME
      CHARACTER*8  C8NAME
      EQUIVALENCE (INTNAME,C8NAME)

      CHARACTER*1  C1NAME(8)
      EQUIVALENCE (INTNAME,C1NAME(1))
      
C
      INTNAME = NAME		!... I*8 W/I WHICH LEADING 5-BYTES GOOD
      DO  IC = 6,8
        C1NAME(IC) = ' '   	!... BLANK OUT TRAILING 3 BYTES
      ENDDO

C     ... INITALIZE PSOWDT ...
C
      DO  I = 1,6
        PSOWDT(I) = CON_PSOWDT(I)
      ENDDO
C
      IF((KTTYP .GT. 0) .AND.
     1   (KTTYP .LE. MAXTYPES)) THEN
         PSOWDT(6) = LTYPE(KTTYP)
      ENDIF
C
C
C     THE DEFAULT ZOOM IS ZOOM1 SO LOAD ZOOM1 INTO 1ST BYTE OF PSOWDT.
C
      PSOWDT(1) = ZOOM1
C
C     ... PERFORM TABLE LOOK-UP ON STN NAME ...  
C     ...       IF MATCH FOUND, THEN USE THAT ZOOM INSTEAD OF DEFAULT
 
      FOUNDNAME = .FALSE.
      DO K = 1,MAXNSTNS
          IF (INTNAME .EQ. STNZM(1,K)) THEN 
             FOUNDNAME = .TRUE.
             GO TO 10
          END IF
      END DO

C     JSTNFOUND = ijbsirch(intname,stnzm,2,MAXNSTNS)

 10   IF(FOUNDNAME)  THEN
         INDX = 1 + STNZM(2,K)
         PSOWDT(1) = ZOOMS(INDX)
      ENDIF
        
C
C
  300 CONTINUE
C
      IF(FOUNDNAME) THEN
        WRITE(6,315) C8NAME(1:5),(PSOWDT(I),I=1,6)
 315    FORMAT(1H ,'AFZOOM: FOUND MATCHING STN =',A,'; PSOWDT=',6A1)
      ENDIF
C
      RETURN
      END
