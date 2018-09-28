      SUBROUTINE SEPCAR(c80card,ITC,MXITM,NITEMS, NLCH,c12strl,
     1                  NRCH,c12strr, IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    SEPCAR      SEPARATE CHARACTER STRING
C   PRGMMR: LILLY            ORG: W/NMC412   DATE: 93-05-08
C
C ABSTRACT: CREATING SUBSTRINGS CHARACTERS FROM A 20-WORD ARRAY
C   WITH CARD IMAGE CHARACTER STRING.
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR  UNKNOWN
C   88-07-25  GLORIA DENT  PUT IN DOCUMENTATION BLOCK
C   89-04-18  STEVE LILLY  UPDATE DOCUMENTATION BLOCK
C   93-05-08  LILLY CONVERT SUBROUTINE TO FORTRAN 77
C
C USAGE:    CALL SEPCAR  (c80card,ITC,MXITM,NITEMS,NLCH,c12strl,NRCH,
C          &            c12strr,IERR)
C
C   INPUT ARGUMENT LIST:
C     c80card    - A 20-WORD (=80CHARS) ARRAY WITH CARD IMAGE
C              - CHARACTER STRING TO BE SEAPARATED INTO SUBSTRINGS
C     ITC      - CARD COLUMN TO BEGIN THE SEPARATING
C     MXITM    - MAX NO. OF ITEMS EXPECTED ON THE CARD IMAGE
C              - WHERE ITEMS ARE SEPARATED BY COMMAS OR BLANKS
C              - AND MAY BE AN EXPRESSION CONTAINING AN '=' SIGN
C              - AND THE STRING IS TERMINATED BY A SEMI-COLON.
C
C   OUTPUT ARGUMENT LIST:
C     NITEMS   - NO. OF ITEMS FOUND
C     NLCH     - IS CHAR COUNT OF FOLLOWING SUBSTRING
C     c12strl   - CHAR SUBSTRING W/ MAX OF 12 CHARS
C     NRCH     - IS CHAR COUNT OF SUBSTRING FROM RHS OF '=' SIGN IF ANY
C     c12strr   - CHAR STRING FROM RHS OF EQUAL SIGN IF ANY
C     IERR     - = 0 IS NORMAL RETURN
C              - = 1 IF SOME TROUBLE WAS ENCOUNTERED.
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  NAS
C
C$$$
C
C
      INTEGER      NOTRAIL		!... define int function NOTRAIL
      EXTERNAL     NOTRAIL
      INTEGER      LASTCH            	!... define int function LASTCH
      EXTERNAL     LASTCH

      character*(*)   c80card		!... Arg(1)
      integer      itc			!...to start at c80card(itc:itc)
      integer      NITEMS		!...Arg(4) count of items found 
      INTEGER      NLCH(MXITM)		!...Arg(5) Nch left of '='
      character*12 c12strl(MXITM)	!...Arg(6) TXT left of '='

      integer      NRCH(MXITM)		!...Arg(7) Nch right of '='
      character*12 c12strr(MXITM)	!...Arg(8) TXT right of '='
      
      integer      IERR			!...ARG(9) return code

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

      character*12 c12subst
C
      CHARACTER*1  LITEQU
      data         LITEQU        / '=' /
      CHARACTER*1  LITSC
      data         LITSC         / ';' /
      CHARACTER*1  LIT
      character*1  NULL
      character*5  delims
C
C ...    INTEGER    ITABC(64)         /16*0,Z40000000,6*0,Z00005E00,2*0,
C ...     1                              Z0000006B,4*0,Z00007E00,32*0/
C ...    ...WHERE ITABC HAS DELIMITERS BLANK,COMMA,SEMICOLON,EQUAL
C
C
      LOGICAL    DIVERT
      LOGICAL    LHSIN
C
C
      IERR = 0
      NULL = CHAR(0)
      DELIMS(1:5) = ' '//','//';'//'='//NULL

      ITH = ITC
      NCHAR = 12
      MXCH = 80
      lencard = len(c80card)
      if(lencard .LE. 0) then
        write(6,FMT='(1h ,''sepcar: FAILURE.  Given card image was '',
     1                ''not defined as a CHARACTER*80 variable.'')')
        go to 900
      endif 
      IF(LENCARD .LT. MXCH) THEN
        MXCH = LENCARD
      ENDIF
      LOCLASTCH = NOTRAIL(C80CARD(1:MXCH))
      IF(LOCLASTCH .LT. LENCARD) THEN
        L1 = LOCLASTCH + 1
        C80CARD(L1:L1) = NULL
      ENDIF

      DIVERT = .FALSE.
C     ...WHERE DIVERT FLAGS '=' SIGN TO DIVERT NEXT GROUP TO NUMBS

      LHSIN = .FALSE.
C     ...WHERE LHSIN FLAGS WHEN NAME ITEM IS IN

      ITEM = 0
C     ...INITIALIZE CARD INFO SAVE AREAS ...
      DO  144  J = 1,MXITM
        NLCH(J) = -1
        NRCH(J) = -1
        c12strl(J)(1:12) = ' '
        c12strr(J)(1:12) = ' '
  144 CONTINUE

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     ...START LOOPB WHICH SCANS FIELD BY FIELD THRU ONE CARD
  150 CONTINUE			!... comes to here from bottom of loop

C ...      CALL CUTSTR(c80card,MXCH,ITH,NCHAR,delims
C ...     1            N,LIT,c12subst,NXTCH,IEXIT)

      CALL GTNXTWRD(C80CARD(1:LOCLASTCH),ITH,LPCURR,DELIMS,LIT,
     1              C12SUBST,IRET_GTN)

C     PRINT *, 'SEPCAR: IRET_GTN=',IRET_GTN
C
      if(iret_gtn .NE. 0) then
        IF((IRET_GTN .GT. 0) .AND. (IRET_GTN .LE. 3)) THEN
C         ...COMES HERE IF FATAL ERROR IN CALL SEQ ARG
          WRITE(6,164)iret_gtn
  164     FORMAT(1H ,'sepcar::gtnxtwrd: ERROR RETURN=',I3,
     1        /1h ,7X,'given arg is not defined as a character string')
          GO TO 900
        ELSE IF(IRET_GTN .eq. 4) THEN
C         ...COMES HERE IF GIVEN EMPTY SOURCE ...
          WRITE(6,165)iret_gtn
  165     FORMAT(1H ,'sepcar::gtnxtwrd: ERROR RETURN=',I3,
     1          /1h ,7X,'The given card-image source was empty')
          GO TO 900
        ELSE IF(IRET_GTN .eq. -1) THEN
C         ...COMES HERE IF WE HAVE MOVED LPCURR BEYOND END OF DATA
          WRITE(6,166)iret_gtn
  166     FORMAT(1H ,'sepcar::gtnxtwrd: RETURN CODE=',I3,
     1        /1h ,7X,'The current line-pointer is beyond end-of-line')
          GO TO 240		!... to clean up like normal end
        ELSE
C         ... COMES HERE ON UNRECOGNIZED ERROR CONDITION
          WRITE(6,167)iret_gtn
  167     FORMAT(1H ,'sepcar::gtnxtwrd: FAILED on ERROR=',I4)
          go to 900
        endif
      ENDIF

      NXTCH = LPCURR + 1
      N = LASTCH(C12SUBST)
C     PRINT *, ' N=', N, ' LIT=',LIT
C     PRINT *,' NXTCH=', NXTCH, '  mxtch=', mxch
      IF((N .LE. 0) .AND. (LIT .EQ. LITEQU)) THEN
C       ... HIT A DELIMITER FIRST AND VOID SUBSTRING
C       PRINT *, ' DIVERT SET TO TRUE'
        DIVERT = .TRUE.
        GO TO 220
      ENDIF
      
      IF(LIT .EQ. NULL) THEN
C       ... NO DELIMITER FOUND IN SUBSTRING
        PRINT  174
  174   FORMAT(1H ,'sepcar::gtnxtwrd: NO DELIMITER FOUND IN SUBSTRING')
        GO TO 900
      endif


  180 CONTINUE		!... comes here on IEXIT == 0;
C     PRINT *, 'CHECK AT 180'
C     PRINT *,' before ITEM  =', ITEM, ' MXITM=', MXITM
C
      IF(DIVERT) GO TO 190
      ITEM = ITEM + 1
C     PRINT *,' after ITEM =', ITEM, ' MXITM=', MXITM
      IF(ITEM .GT. MXITM) GO TO 240

      NLCH(ITEM) = N
      c12strl(ITEM)(1:N) = c12subst(1:N)

      LHSIN = .TRUE.
      IF(LIT .EQ. LITEQU) THEN
C       ...  THIS ENDED ON A '=' SIGN
        DIVERT = .TRUE.
      ENDIF
      GO TO 220

  190 CONTINUE
C     ...COMES HERE TO MOVE THE NUMBERS FROM THE RIGHT OF '=' SIGN
      IF(LIT .NE. LITEQU) GO TO 198
C     ...OTHERWISE, ERROR SINCE PREVIOUS FIELD ALSO ENDED ON '='
  192 CONTINUE
      PRINT  194
  194 FORMAT(1H ,'SEPCAR: FORMAT ERROR. GIVEN STRING HAD TWO = SIGNS',
     1           ' WITHIN LOGICAL ITEM')
      GO TO 900

  198 CONTINUE
      IF(.NOT. LHSIN) GO TO 192

      NRCH(ITEM) = N
      c12strr(ITEM)(1:N) = c12subst(1:N)

      LHSIN = .FALSE.
      DIVERT = .FALSE.
      GO TO 220
  220 CONTINUE
C     ...COMES HERE FOR END OF INFO OR END OF CARD TEST
      IF(LIT .EQ. LITSC) GO TO 240
      IF(NXTCH .GT. MXCH) GO TO 240
      IF(NXTCH .GT. MXCH) GO TO 240
      ITH = NXTCH
      GO TO 150
C     ...WHICH ENDS LOOPB ON ONE FIELD WITHIN A CARD
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  240 CONTINUE
C     ...ONE CARD,S INFO HAS BEEN SEPARATED ...
C     PRINT *, ' ITEM=', ITEM
      IF(ITEM .LE. 0) GO TO 900
      IF(ITEM .GT. MXITM) THEN
        ITEM=MXITM
      ENDIF
      NITEMS = ITEM
      RETURN
C     ...WHICH IS NORMAL EXIT
C
  900 CONTINUE
      IERR = 1
      RETURN
      END
