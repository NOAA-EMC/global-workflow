      SUBROUTINE DIXIE (NDATA,ITM,JTM,IJTH,MSKJJ,NRITSHF,
     1                  IDIXAD,IDIXCO,NDIX,iret_dix)
C                                                 25-JUL-1996/DSS
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    DIXIE       SETS DATA POINTERS FOR THINNING OBS DATA
C   PRGMMR: LILLY            ORG: W/NMC412   DATE: 93-05-09
C
C ABSTRACT: GENERATES DATA POINTERS NEEDED BY SUBR THIK() TO 
C   EFFICIENTLY SCAN THE OBSERVATIONAL DATA FILE WHEN LOOKING FOR
C   NEARBY STATIONS.
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR   UNKNOWN
C   89-04-17  STEVE LILLY  UPDATE DOCUMENTATION BLOCK
C   93-05-09  LILLY CONVERT SUBROUTINE TO FORTRAN 77
C   96-07-25  SHIMOMUR - CONVERT TO RUN ON CRAY; added Arg:return code
C
C USAGE:    CALL DIXIE(NDATA,ITM,JTM,IJTH,MSKJJ,NRITSHF,
C                      IDIXAD,IDIXCO,NDIX,iret_dix)
C
C   INPUT ARGUMENT LIST:
C     NDATA(ITM,JTM) - THE ADP DATA ARRAY
C     IJTH     - POINTS TO THE LOCATION OF THE I, J WORD WITHIN
C              - THE ITM DOUBLE WORDS REPRESENTING EACH DATA POINT
C     MSKJJ    - MASK FOR THE INTEGER-PORTION OF STN J-VALUE
C     NRITSHF  - NO. OF BITS TO SHIFT RIGHTWARD TO RIGHT-JUSTIFY
C                    THAT INTEGER-PORTION OF STN J-VALUE
C   OUTPUT ARGUMENT LIST:
C     IDIXAD   - I = BOTTOM, J TOP POINTS TO THE FIRST OBSERVATION
C              - IN THE I-TH, J-STRIP
C     IDIXCO   - THE TOTAL NUMBER OF OBSERVATION
C     NDIX     - DIMENSION ARRAY IE; IDIXCO(NDIX)
C     iret_dix - return code
C
C REMARKS: 
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C     USAGE:    CALL DIXIE(NDATA,ITM,JTM,IJTH,MSKJJ,NRITSHF,
C                          IDIXAD,IDIXCO,NDIX,iret_dix)
C     ... GIVEN:
      integer*8   NDATA(ITM,JTM)   		!...ARGS 1,2,3
      INTEGER     IJTH                     	!... ARG 4
      integer     MSKJJ   			!... ARG 5
      INTEGER     NRITSHF                  	!... ARG 6
C     ... RESULTS:     
      integer     IDIXAD(NDIX)   		!... ARG 7,9
      integer     IDIXCO(NDIX)   		!... ARG 8,9
      INTEGER     IRET_DIX                	!... ARG 10

C     ...ARRAYS DIMENSIONED ONE MORE THAN MAX NO OF JROWS IN GRID, 
C     ...     BECAUSE THE TOTAL NO OF OBS IS RETURNED IN IDIXCO(NDIX)
C     ...     WHERE VALUE OF NDIX = (MAXGRIDJ + 1)
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

      INTEGER     IJWD
      INTEGER     IJADR
      INTEGER     JCURR
      INTEGER     LEFSHF
      INTEGER     MAXGRIDJ
      INTEGER     NOBSINSTRIP
      LOGICAL     LENDOFDATA


      SAVE

      IRET_DIX = 0

      DO  I = 1,NDIX
        IDIXAD(I) = 0
        IDIXCO(I) = 0
      enddo

      IF(NDIX .LT. 3) THEN
        IRET_DIX = 2
        GO TO 999
      ENDIF
      LEFSHF = -NRITSHF
      MAXGRIDJ = NDIX - 1
      IJADR = 1
      LENDOFDATA = .FALSE.
      NOBSINSTRIP = 0
      JCURR = 1
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     ...LUPA...         DO  ITEM=1,JTM
      ITEM = 1
  307 CONTINUE
        IJWD = NDATA(IJTH,ITEM)
C ...   ...     IF(IJWD) 311,309,311   ...
        if(ijwd .EQ. 0) then
          LENDOFDATA = .TRUE.
C         ...NO MORE OBS...
          GO TO 333
        endif 
C       ... otherwise,  ijwd was non-zero, so
  311   CONTINUE
        JSTN = IAND(IJWD,MSKJJ)
        JSTN = ISHFT(JSTN,LEFSHF)

  322   CONTINUE
        IF(JCURR .LT. JSTN) GO TO 333

        NOBSINSTRIP = NOBSINSTRIP + 1
C       ...WHICH INCREMENTS COUNT OF OBS W/I THIS J STRIP...

        ITEM = ITEM + 1
        IF(ITEM .LE. JTM) GO TO 307

C     ...OTHERWISE, REACHED END OF FULL BIN,  NO MORE OBS...
      LENDOFDATA = .TRUE.
  333 CONTINUE
      IDIXAD(JCURR) = IJADR
C     ...WHERE IDIXAD HAS SUBSCRIPT FOR FIRST OBS W/I THIS ROW...
      IJADR = IJADR + NOBSINSTRIP

      IDIXCO(JCURR) = NOBSINSTRIP
C     ...WHERE IDIXCO HAS COUNT OF OBS IN THIS STRIP...

      NOBSINSTRIP = 0
      JCURR = JCURR + 1
      IF(JCURR .GT. MAXGRIDJ) GO TO 800

      IF(LENDOFDATA) THEN
        GO TO 333
      ELSE
        GO TO 322
      ENDIF
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C     ...THE ENDING...
  800 CONTINUE
      IDIXCO(MAXGRIDJ + 1) = IJADR - 1
C     ...WHICH WOULD BE TOTAL NO OF OBS
      IF(IJADR .LE. 1) THEN
C       ... WARNING: TOTAL COUNT OF OBS IS ZERO ...
        IRET_DIX = 1
      ENDIF
  999 CONTINUE
      RETURN
      END
