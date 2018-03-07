      PROGRAM MDATE
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: MDATE        UPDATE A DATE GIVEN INCREMENT IN MINUTES
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2001-09-20
C
C ABSTRACT: PROGRAM TO COMPUTE A VERIFYING DATE GIVEN AN INCREMENT IN
C   MINUTES AND THE INITIAL DATE.
C
C PROGRAM HISTORY LOG:
C 2001-09-20  KEYSER   BASED ON UTILITY PROGRAM NDATE EXCEPTS UPDATES
C                      BY MINUTES INSTEAD OF HOURS
C
C USAGE:      mdate [minutes [idate]]
C   INPUT ARGUMENT LIST
C     MINUTES  - INCREMENT IN MINUTES (MAY BE NEGATIVE)
C                MINUTES DEFAULTS TO ZERO.
C     IDATE    - INITIAL DATE IN YYYYMMDDHHMM FORMAT.
C                IDATE DEFAULTS TO THE CURRENT UTC DATE, HOUR AND
C                MINUTE.  FIRST CENTURY DATES WILL CAUSE ABORT AFTER
C                1 SEPT 1999.
C   OUTPUT ARGUMENT LIST:
C     MDATE    - VERIFYING DATE IN YYYYMMDDHHMM FORMAT.
C   EXIT STATES:
C     0      - SUCCESS
C     1      - FAILURE; INVALID ARGUMENT
C     2      - FAILURE; INCORRECT NUMBER OF ARGUMENTS
C
C SUBPROGRAMS CALLED:
C   IARGC           GET NUMBER OF ARGUMENTS
C   GETARG          GET ARGUMENT
C   W3MOVDAT        RETURN A DATE FROM A TIME INTERVAL AND DATE
C   W3UTCDAT        RETURN THE UTC DATE AND TIME
C   ERRMSG          WRITE A MESSAGE TO STDERR
C   ERREXIT         EXIT PROGRAM (W3LIB)
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
          CALL ERRMSG('mdate: Invalid option -')
          CALL EUSAGE
          CALL ERREXIT(1)
        ELSE
          L=2
          DOWHILE(L.LE.LARG)
            IF(CARG(L:L).EQ.'-') THEN
              LSTOPT=1
            ELSE
              CALL ERRMSG('mdate: Invalid option '//CARG(L:L))
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
        CALL ERRMSG('mdate: Incorrect number of arguments')
        CALL EUSAGE
        CALL ERREXIT(2)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET AND CHECK FIRST ARGUMENT (MINUTE INCREMENT)
      IF(MARG.GE.1) THEN
        CALL GETARG(IARG,CARG)
        NCARG=LEN_TRIM(CARG)
        WRITE(CFMT,'("(I",I2,")")') NCARG
        READ(CARG,CFMT,IOSTAT=IRET) IMINUTE
        IF(IRET.NE.0) THEN
          CALL ERRMSG('mdate: Noninteger minute '//CARG(1:NCARG))
          CALL EUSAGE
          CALL ERREXIT(1)
        ENDIF
      ELSE
        IMINUTE=0
      ENDIF
      RINC=0
      RINC(3)=IMINUTE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET AND CHECK SECOND ARGUMENT (INITIAL DATE YYYYMMDDHHMM)
      IF(MARG.GE.2) THEN
        CALL GETARG(IARG+1,CARG)
        NCARG=LEN_TRIM(CARG)
        WRITE(CFMT,'("(I",I2,",4I2)")') NCARG-8
        IDAT=0
        READ(CARG,CFMT,IOSTAT=IRET) IDAT(1),IDAT(2),IDAT(3),IDAT(5),
     &   IDAT(6)
        IF(IRET.NE.0.OR..NOT.W3VALDAT(IDAT)) THEN
          CALL ERRMSG('mdate: Invalid date '//CARG(1:NCARG))
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
      WRITE(CFMT,'("(I",I2,",4I2.2)")') ND
      PRINT CFMT,JDAT(1),JDAT(2),JDAT(3),JDAT(5),JDAT(6)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CONTAINS
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  WRITE USAGE
      SUBROUTINE EUSAGE
      CALL ERRMSG('Usage: mdate [minutes [idate]]')
      ENDSUBROUTINE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENDPROGRAM
