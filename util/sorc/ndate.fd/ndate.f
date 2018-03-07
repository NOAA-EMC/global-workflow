      PROGRAM NDATE
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: NDATE        COMPUTE VERIFYING DATE
C   PRGMMR: IREDELL          ORG: NP23        DATE: 1998-08-18
C
C ABSTRACT: PROGRAM TO COMPUTE VERIFYING DATE
C   GIVEN THE FORECAST HOUR AND THE INITIAL DATE.
C
C PROGRAM HISTORY LOG:
C   95-02-28  IREDELL
C   97-09-22  IREDELL  4-DIGIT YEAR ALLOWED; 2-DIGIT YEAR STANDARDIZED
C   98-03-25  IREDELL  4-DIGIT YEAR FOR ALL DATES.  A 2-DIGIT YEAR WILL
C                      BE INTERPRETED AS A YEAR IN THE FIRST CENTURY
C                      WHICH SHOULD BE ALL RIGHT BEFORE THE YEAR 2000.
C                      STANDARD ERROR WARNINGS WILL BE GIVEN FOR SUCH
C                      DATES UNTIL 1 SEPT 1998 AFTER WHICH NDATE ABORTS.
C                      NDATE WILL ALSO ABORT AFTER 1 SEPT 1998 IF THE
C                      NOW IRRELEVANT -Y OPTION IS INVOKED.
C                      THE NEW Y2K-COMPLIANT W3LIB PACKAGE IS USED.
C 1998-08-17  IREDELL  DROP-DEAD DATE RESET TO 1 SEPT 1999
C 1999-01-26  VUONG    CHANGED TO USE 4-DIGIT YEAR AS STANDARDIZED FOR
C                      ALL DATES AND CONVERTED TO IBM RS/6000 SP AND 
C                      MODIFIED PROGRAM TO CALL ROUTINE GETARG INSTEAD OF
C                      USING SUBROUTINE GETARG AS A FUNCTION.
C 1999-09-02  IREDELL  UNDID RESTRICTION ON FORECAST HOUR
C
C USAGE:      ndate [fhour [idate]]
C   INPUT ARGUMENT LIST
C     FHOUR    - FORECAST HOUR (MAY BE NEGATIVE)
C                FHOUR DEFAULTS TO ZERO.
C     IDATE    - INITIAL DATE IN YYYYMMDDHH FORMAT.
C                IDATE DEFAULTS TO THE CURRENT UTC DATE AND HOUR.
C                FIRST CENTURY DATES WILL CAUSE ABORT AFTER 1 SEPT 1999.
C   OUTPUT ARGUMENT LIST:
C     NDATE    - VERIFYING DATE IN YYYYMMDDHH FORMAT.
C   EXIT STATES:
C     0      - SUCCESS
C     1      - FAILURE; INVALID ARGUMENT
C     2      - FAILURE; INCORRECT NUMBER OF ARGUMENTS
C
C SUBPROGRAMS CALLED:
C   IARGC           GET NUMBER OF ARGUMENTS
C   GETARG          GET ARGUMENT
C   W3DIFDAT        RETURN A TIME INTERVAL BETWEEN TWO DATES
C   W3MOVDAT        RETURN A DATE FROM A TIME INTERVAL AND DATE
C   W3PRADAT        FORMAT A DATE AND TIME INTO CHARACTERS
C   W3UTCDAT        RETURN THE UTC DATE AND TIME
C   ERRMSG          WRITE A MESSAGE TO STDERR
C   EXIT            EXIT PROGRAM
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      CHARACTER*256 CARG,CFMT
      INTEGER*4 IARG,LARG,NCARG,NARG,IARGC
      INTEGER IDAT(8),JDAT(8)
      REAL RINC(5)
      LOGICAL W3VALDAT
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET AND CHECK OPTIONAL ARGUMENTS
      NARG=IARGC()
      IARG=1
      LSTOPT=0
      DOWHILE(IARG.LE.NARG.AND.LSTOPT.EQ.0)
        CALL GETARG(IARG,CARG)
        LARG=LEN_TRIM(CARG)
        IARG=IARG+1
        IF(CARG(1:1).NE.'-'.OR.
     &    (CARG(2:2).GE.'0'.AND.CARG(2:2).LE.'9')) THEN
          LSTOPT=1
          IARG=IARG-1
        ELSEIF(LARG.EQ.1) THEN
          CALL ERRMSG('ndate: Invalid option -')
          CALL EUSAGE
          CALL ERREXIT(1)
        ELSE
          L=2
          DOWHILE(L.LE.LARG)
            IF(CARG(L:L).EQ.'-') THEN
              LSTOPT=1
            ELSE
              CALL ERRMSG('ndate: Invalid option '//CARG(L:L))
              CALL EUSAGE
              CALL ERREXIT(1)
            ENDIF
            L=L+1
          ENDDO
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CHECK NUMBER OF ARGUMENTS
      MARG=NARG-IARG+1
      IF(MARG.GT.2) THEN
        CALL ERRMSG('ndate: Incorrect number of arguments')
        CALL EUSAGE
        CALL ERREXIT(2)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET AND CHECK FIRST ARGUMENT (HOUR INCREMENT)
      IF(MARG.GE.1) THEN
        CALL GETARG(IARG,CARG)
        NCARG=LEN_TRIM(CARG)
        WRITE(CFMT,'("(I",I2,")")') NCARG
        READ(CARG,CFMT,IOSTAT=IRET) IHOUR
        IF(IRET.NE.0) THEN
          CALL ERRMSG('ndate: Noninteger forecast hour '//CARG(1:NCARG))
          CALL EUSAGE
          CALL ERREXIT(1)
        ENDIF
      ELSE
        IHOUR=0
      ENDIF
      RINC=0
      RINC(2)=IHOUR
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET AND CHECK SECOND ARGUMENT (INITIAL DATE)
      IF(MARG.GE.2) THEN
        CALL GETARG(IARG+1,CARG)
        NCARG=LEN_TRIM(CARG)
        WRITE(CFMT,'("(I",I2,",3I2)")') NCARG-6
        IDAT=0
        READ(CARG,CFMT,IOSTAT=IRET) IDAT(1),IDAT(2),IDAT(3),IDAT(5)
        IF(IRET.NE.0.OR..NOT.W3VALDAT(IDAT)) THEN
          CALL ERRMSG('ndate: Invalid date '//CARG(1:NCARG))
          CALL EUSAGE
          CALL ERREXIT(1)
        ENDIF
      ELSE
        CALL W3UTCDAT(IDAT)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE AND PRINT NEW DATE
      CALL W3MOVDAT(RINC,IDAT,JDAT)
      ND=LOG10(JDAT(1)+0.5)+1
      WRITE(CFMT,'("(I",I2,",3I2.2)")') ND
      PRINT CFMT,JDAT(1),JDAT(2),JDAT(3),JDAT(5)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CONTAINS
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  WRITE USAGE
      SUBROUTINE EUSAGE
      CALL ERRMSG('Usage: ndate [fhour [idate]]')
      ENDSUBROUTINE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENDPROGRAM
