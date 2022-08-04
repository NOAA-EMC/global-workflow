C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: SYNDAT_GETJTBUL   RETRIEVES JTWC BULLETINS FROM TANK
C   PRGMMR: STOKES                ORG: NP23        DATE: 2013-02-22
C
C ABSTRACT: RETRIEVES TROPICAL CYCLONE POSITION AND INTENSITY
C   INFORMATION FROM JOINT TYPHOON WARNING CENTER/FNMOC.  THESE
C   BULLETINS COME IN TWO PIECES.  THIS PROGRAM READS THEM AND
C   JOINS THEM TOGETHER.  THIS ALLOWS THE DOWNSTREAM PROGRAM
C   QCTROPCY TO PROCESS THEM.
C
C PROGRAM HISTORY LOG:
C 1997-06-23  S. J. LORD ---- ORIGINAL AUTHOR
C 1998-11-24  D. A. KEYSER -- FORTRAN 90/Y2K COMPLIANT
C 1998-12-30  D. A. KEYSER -- MODIFIED TO ALWAYS OUTPUT RECORDS
C               CONTAINING A 4-DIGIT YEAR (REGARDLESS OF INPUT)
C 2000-03-09  D. A. KEYSER -- MODIFIED TO RUN ON IBM-SP; CORRECTED
C               PROBLEM FROM EARLIER CRAY VERSION WHICH RESULTED
C               IN AN INCORRECT JOINING OF PIECES IF THE SAME
C               2-PIECE BULLETIN IS DUPLICATED IN THE ORIGINAL FILE
C               THAT IS READ IN BY THIS PROGRAM
C 2013-02-22  D. C. STOKES -- MINOR DOC CHANGES.  (WCOSS TRANSIITON)
C
C USAGE:
C   INPUT FILES:
C    UNIT 11  - FILE CONTAINING JTWC/FNMOC BULLETINS
C
C   OUTPUT FILES:
C    UNIT 06  - STANDARD OUTPUT PRINT
C    UNIT 51  - FILE CONTAINING JTWC/FNMOC BULLETINS NOW JOINED
C               TOGETHER
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:  - NONE
C     LIBRARY:
C       W3NCO    - W3TAGB W3TAGE ERREXIT
C
C   EXIT STATES:
C     COND =   0  - SUCCESSFUL RUN, DATA RETRIEVED
C          =   1  - SUCCESSFUL RUN -- NO DATA RETRIEVED
C          =  20  - TROUBLE - EITHER READ ERROR WITHIN PROGRAM OR
C                   NUMBER OF RECORDS IN INPUT FILE EXCEEDS PROGRAM
C                   LIMIT.
C
C REMARKS: THE Y2K-COMPLIANT VERSION IS SET-UP TO READ RECORDS WITH
C          EITHER A 2-DIGIT YEAR STARTING IN COLUMN 20 OR A 4-DIGIT
C          YEAR STARTING IN COLUMN 20.  THIS WILL ALLOW THIS PROGRAM
C          TO RUN PROPERLY WHEN JTWC/FNMOC TRANSITIONS RECORDS TO
C          A 4-DIGIT YEAR.
C
C ATTRIBUTES:
C   LANGUAGE  FORTRAN 90
C   MACHINE:  IBM SP and IBM iDataPlex
C
C$$$
      PROGRAM SYNDAT_GETJTBUL

      PARAMETER (NBULS=200)

      CHARACTER*1  INL1(80)
      CHARACTER*9  STNAME
      CHARACTER*18 HEAD(NBULS),CHEKHED
      CHARACTER*37 ENDMSG
      CHARACTER*80 INL,INLS(NBULS)
      CHARACTER*80 DUMY2K
      CHARACTER*95 OUTL

      INTEGER LINE(NBULS)

      EQUIVALENCE (INL1,INL)

      DATA IIN/11/,IOUT/51/,LINE/NBULS*0/

      CALL W3TAGB('SYNDAT_GETJTBUL',2013,0053,0050,'NP23   ')

      WRITE(6,*) ' '
      WRITE(6,*) '===> WELCOME TO SYNDAT_GETJTBUL - F90/Y2K VERSION ',
     $ '02-22-2013'
      WRITE(6,*) ' '
      WRITE(6,*) ' '

      NLINE = 0

      DO N=1,NBULS
         INL1=' '
         READ(IIN,2,END=100,ERR=200)  INL
    2    FORMAT(A80)
         NLINE = N

C AT THIS POINT WE DO NOT KNOW IF A 2-DIGIT YEAR BEGINS IN COLUMN 20
c  OF THE RECORD (OLD NON-Y2K COMPLIANT FORM) OR IF A 4-DIGIT YEAR
c  BEGINS IN COLUMN 20 (NEW Y2K COMPLIANT FORM) - TEST ON LOCATION OF
c  LATITUDE BLANK CHARACTER TO FIND OUT ...

         IF(INL1(26).EQ.' ') THEN

c ... THIS RECORD STILL CONTAINS THE OLD 2-DIGIT FORM OF THE YEAR -
c ... THIS PROGRAM WILL NOW CONVERT THE RECORD TO A 4-DIGIT YEAR USING
c      THE "WINDOWING" TECHNIQUE SINCE SUBSEQUENT LOGIC EXPECTS THIS

            PRINT *, ' '
            PRINT *, '==> This is an old-format record with a 2-digit ',
     $       'year "',INL(20:21),'"'
            PRINT *, ' '
            DUMY2K(1:19) = INL(1:19)
            IF(INL(20:21).GT.'20')  then
               DUMY2K(20:21) = '19'
            ELSE
               DUMY2K(20:21) = '20'
            ENDIF
            DUMY2K(22:80) = INL(20:80)
            INL= DUMY2K
            PRINT *, ' '
            PRINT *, '==> 2-digit year converted to 4-digit year "',
     $       INL(20:23),'" via windowing technique'
            PRINT *, ' '

         ELSE 

c ... THIS RECORD CONTAINS THE NEW 4-DIGIT FORM OF THE YEAR
c ... NO CONVERSION NECESSARY SINCE THIS SUBSEQUENT LOGIC EXPECTS THIS

            PRINT *, ' '
            PRINT *, '==> This is an new-format record with a 4-digit ',
     $       'year "',INL(20:23),'"'
            PRINT *, ' '
            PRINT *, '==> No conversion necessary'
            PRINT *, ' '
         end if

         WRITE(6,3) NLINE,INL
    3    FORMAT(' ...Bulletin line number',I4,' is....',A80,'...')
         INLS(NLINE)=INL
         HEAD(NLINE)=INL(1:18)
         WRITE(6,4)  NLINE,HEAD(NLINE)
    4    FORMAT(' ...   Header for line number',I4,' is ...',A18,'...')
      ENDDO

C  Come here if no. of records in input file exceeds pgm limit ("NBULS")
C  ---------------------------------------------------------------------

      WRITE(6,301) NBULS
  301 FORMAT(' **** Number of records in input File exceeds program ',
     $ 'limit of',I4,'.  Abort')
      ICODE=20
      ENDMSG='SYNDAT_GETJTBUL TERMINATED ABNORMALLY'
      GO TO 900

  100 CONTINUE

C  All records read in
C  -------------------

      IF(NLINE.EQ.0) THEN

C     Come here if ZERO records were read from input file
C     ---------------------------------------------------

         ICODE=1
         WRITE(6,101)
  101    FORMAT(' ...No Bulletins available.')
         ENDMSG='SYNDAT_GETJTBUL TERMINATED NORMALLY  '
         GO TO 900
      ENDIF
  
      IF(MOD(NLINE,2).NE.0)  THEN

C  Come here if number of records read was not even
C  ------------------------------------------------

         WRITE(6,111) NLINE
  111    FORMAT(' **** Number of records read in (=',I4,') is not ',
     $    'even.  Abort')
         ICODE=20
         ENDMSG='SYNDAT_GETJTBUL TERMINATED ABNORMALLY'
         GO TO 900
      ENDIF
      
      PRINT *, ' '
      PRINT *, ' '
      NBULT=NLINE/2
      NBUL=0
      LOOP1: DO NL=1,NLINE
         IF(LINE(NL).EQ.1)  CYCLE LOOP1
         CHEKHED=HEAD(NL)
         IFND = 0
         LOOP1n1: DO NB=NL+1,NLINE
            IF(LINE(NB).EQ.1)  CYCLE LOOP1n1
            NBSAV=NB
            WRITE(6,11) CHEKHED,INLS(NB)(1:18)
   11       FORMAT(' ...message parts are ...',A18,'...',A18,'...')
            IF(CHEKHED .EQ. INLS(NB)(1:18))  THEN
               LINE(NL) = 1
               LINE(NB) = 1
               IFND = 1 
               EXIT LOOP1n1
            ENDIF
         ENDDO LOOP1n1
         IF(IFND.EQ.1)  THEN
            WRITE(6,131)  INLS(NL)(10:10)
  131       FORMAT(' ...inls(nl)(10:10)=',A1,'...')
            IF(INLS(NL)(10:10).eq.' ')  THEN
               LOOP 1n2: DO IB=11,18
                  IS=IB
                  IF(INLS(NL)(IS:IS).NE.' ')  EXIT LOOP 1n2
               ENDDO LOOP 1n2
               STNAME='         '
               STNAME=INLS(NL)(IS:18)
               INLS(NL)(10:18)=STNAME
            ENDIF
            OUTL=INLS(NL)(1:66)//INLS(NBSAV)(33:61)
            WRITE(6,145) OUTL
  145       FORMAT(' ...Complete bulletin is ...',A95,'...')
            WRITE(IOUT,22)  OUTL
   22       FORMAT(A95)
            NBUL=NBUL+1
         ENDIF
         IF(NBUL .EQ. NBULT)  GO TO 150
      ENDDO LOOP1

  150 CONTINUE
      WRITE(6,151)  NBUL
  151 FORMAT(' ...',I4,' bulletins have been made.')
      ICODE=0
      ENDMSG='SYNDAT_GETJTBUL TERMINATED NORMALLY  '
      GO TO 900

  200 continue

C  Come here if error reading a record from input file
C  ---------------------------------------------------

      WRITE(6,201)
  201 FORMAT(' **** ERROR READING RECORD FROM INPUT FILE.  ABORT')
      ICODE=20
      ENDMSG='SYNDAT_GETJTBUL TERMINATED ABNORMALLY'
      
  900 CONTINUE

      WRITE(6,*) ENDMSG

        CALL W3TAGE('SYNDAT_GETJTBUL')

      IF(ICODE.GT.0)  CALL ERREXIT(ICODE)

      STOP

      END  
