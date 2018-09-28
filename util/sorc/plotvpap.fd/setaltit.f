      SUBROUTINE setaltit(LVLDES,KRUN,ALTMLOW,ALTMHIGH,IRET_ALT)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    setaltit    GET ALTITUDE BOUNDS FOR ACFT FOR LVLDES
C   PRGMMR: SHIMOMURA        ORG: W/NP12     DATE: 97-02-24
C
C ABSTRACT: GET ALTITUDE BOUNDS (IN METERS) FOR AIRCRAFT REPORTS FOR USE 
C   ON THIS MAP LEVEL(LVLDES) AND FOR THIS MAP TYPE(KRUN)
C   
C PROGRAM HISTORY LOG:
C   97-02-24  ORIGINAL AUTHOR -- DAVID SHIMOMURA
C   97-03-11  SHIMOMURA -- FOR KRUN == 15; THEN FOR 32KFt to 39KFT
C
C USAGE:    CALL setaltit(LVLDES, KRUN, ALTMLOW, ALTMHIGH, IRET_ALT)
C
C   INPUT ARGUMENT LIST:
C     LVLDES   - DESIRED MANDATORY LVL MAP
C     KRUN     - MAP TYPE AS A FUNCTION OF KRUN 
C                WHERE KRUN == 11, 22, 23   FOR TROPIC MAP LEVELS 
C                      KRUN == 15; THEN FOR 32KFt to 39KFT
C                ANY OTHER VALUE OF KRUN WILL TRY TO GET
C                      THE NORTH AMERICAN PLOTPAP LEVELS
C
C   OUTPUT ARGUMENT LIST:
C     ALTMLOW   - ALT OF LOWER BOUND (IN METERS) 
C     ALTMHIGH  - ALT OF UPPER BOUND (IN METERS)
C     IRET_ALT - RETURN CODE
C              = 0;  NORMAL RETURN
C              = 1;  FAILED TO FIND A MATCH FOR GIVEN LVLDES 
C                        IN TROPIC TABLE OF LEVELS;
C              = 2;  FAILED TO FIND A MATCH FOR GIVEN LVLDES
C                        IN NORTH AMERICAN TABLE OF LEVELS;
C
C REMARKS:
C   TABLES USED IN HERE ARE DERIVED FROM SUBROUTINE SETUP() IN PLOTPAP
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C
C
      INTEGER    KALTA
      PARAMETER (KALTA=8)
C     ...  WHERE KALTA IS NO. OF LEVELS IN ALTBLA -- NORTH AMERICAN
C
      INTEGER    KALTB
      PARAMETER (KALTB=5)
C     ...  WHERE KALTB IS NO. OF LEVELS IN ALTBLB -- TROPIC
C
C     ALTITUDE TABLE A IS FOR N. AMERICAN CHARTS.
C          XLCP, ALT OF LOWER BOUND, ALT OF UPPER BOUND (IN KFT)
C
      REAL   ALTBLA(3,KALTA) 
      DATA   ALTBLA         / 9.0, 41.0, 53.0,		!... 150MB
     1                        8.0, 34.0, 41.0,		!... 200MB
     2                        7.0, 32.0, 38.0,		!... 250MB
     3                        6.0, 27.9, 34.0,		!... 300MB
     4                        5.0, 20.5, 26.0,		!... 400MB
     5                        4.0, 15.9, 20.0,		!... 500MB
     6                        3.0,  7.0, 12.0,		!... 700MB
     7                        2.0,  2.9,  7.0/		!... 850MB
C
C     ALTITUDE TABLE B IS FOR TROPIC PLOT.
C
      REAL   ALTBLB(3,KALTB)
      data   ALTBLB          / 9.0, 41.0, 99.0,		!... 150MB
     1                         7.0, 27.9, 41.0,		!... 250MB
     2                         4.0, 14.9, 21.0,		!... 500MB
     3                         3.0,  6.9, 12.0,		!... 700MB
     4                         2.0,  2.9,  6.9/		!... 850MB
C
C     LEVEL 9, THE 150MB LEVEL, IS HERE INCLUDED FOR FGGE IN 1979.
C     AT JIM MCDONNEL'S SUGGESTION I AM LETTING THAT MAP ACCEPT
C     A/C OBSERVATIONS (OR OTHER TYPES - LIKE COLBA - THAT LOOK LIKE
C     A/C) UP TO 99,000 FEET.
C
      DATA   CNVFTM        /304.8/
C
C
       IRET_ALT = 0
C
C      ... INITIALIZE RESULTING ALTITUDE-BOUNDS
      
       ALTMLOW  = 0.0
       ALTMHIGH = 0.0 
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       IF((KRUN .EQ. 11) .OR. (KRUN .EQ. 22) .OR. (KRUN .EQ. 23)) then
C        ... LOOK FOR TROPIC MAP LEVEL.
         DO  J=1,KALTB
           IF(LVLDES .EQ. NINT(ALTBLB(1,J))) THEN
             ALTMLOW  = ALTBLB(2,J) * CNVFTM
             ALTMHIGH = ALTBLB(3,J) * CNVFTM
             GO TO 999
           ENDIF
         ENDDO
         IRET_ALT = 1		!... FAILED TO FIND A MATCH - TROPIC
         GO TO 999
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
       ELSE IF(KRUN .EQ. 15) THEN
          
C ...         ALTMLOW  = 35.0 * CNVFTM
C ...         ALTMHIGH = 35.0 * CNVFTM
         ALTMLOW  = 32.0 * CNVFTM
         ALTMHIGH = 39.0 * CNVFTM
         GO TO 999
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
       ELSE       
C        ... LOOK FOR NORTH AMERICAN MAP LEVEL.
         DO  J=1,KALTA
           IF(LVLDES .EQ. NINT(ALTBLA(1,J))) THEN
             ALTMLOW  = ALTBLA(2,J) * CNVFTM
             ALTMHIGH = ALTBLA(3,J) * CNVFTM
             GO TO 999
           ENDIF
         ENDDO
         IRET_ALT = 2		!... FAILED TO FIND A MATCH NORTH AMER
         GO TO 999
       ENDIF
      
  999  CONTINUE
       RETURN
       END
