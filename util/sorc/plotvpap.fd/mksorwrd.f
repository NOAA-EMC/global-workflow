      SUBROUTINE MKSORWRD(LVLDES,ITOUT,ITCT,IERRA)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MKSORWRD    TO FORM SORTKEY + THIN ARGS
C   PRGMMR: SHIMOMURA         ORG: W/NP12     DATE: 97-03-03
C
C ABSTRACT: GIVEN: OBSERVATIONS FOR ONE LEVEL IN COMMON /OBSLVLDB/
C   TASK ... TO FORM, IN AN ARRAY NAMED "NDATA()", THE SORT KEY 
C   (FOR SORTING BY GRID-J, AND SECONDARILY BY GRID-I WITHIN EACH J-
C   GRID INTERVAL);  AND ARGS NEEDED FOR SUBSEQUENT THINNING
C   (WHICH IS WHY WE NEED TO SORT -- SO THAT THE THINNING WILL
C   GO FASTER.)
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR: DAVID SHIMOMURA
C   89-04-27  STEVE LILLY  ADD DOCUMENTATION BLOCK
C   93-05-07  LILLY CONVERT SUBROUTINE TO FORTRAN 77
C   97-01-30  SHIMOMURA -- CONVERT TO RUN ON CRAY;
C                          STRIPPED DOWN TO A RUDIMENTARY CODE.
C   97-02-25  SHIMOMURA -- RESTORE THE AIRCRAFT OBS WHICH HAD BEEN
C                          DELETED IN THE RUDIMENTARY CODE.
C   97-03-03  SHIMOMURA -- RESTORE SAT-WIND OBS WHICH HAD BEEN
C                          DELETED IN THE RUDIMENTARY CODE.
C
C USAGE:    CALL MKSORWRD(LVLDES,ITOUT,ITCT,IERRA)
C
C   INPUT ARGUMENT LIST:
C     LVLDES   - LEVEL DESIRED BY INDEX NUMBER
C     ITOUT    - INTEGER FLAG WITH RANGE FROM 1 THRU 14 , USED
C              - TO DETERMINE TYPE OF MAP BACKGROUND DATA IS TO BE
C              - DISPLAYED ON. ITOUT IS A FUNCTION OF KRUN AND IS
C              - SET IN SUB KOPTN.

C      COMMON  /OBSLVLDB/NOBSDB,LVLIX,IOBS2PK
C      INTEGER           IOBS2PK(LMTWRDPOB,NDATASIZ)
C
C   OUTPUT ARGUMENT LIST:
C     ITCT     - NO. OF OBS PUT INTO NDATA FROM MRGPAP FILE
C     IERRA    - TO MARK ERROR RETURNS
C
C      COMMON  /JSPACE/IDREC,NDATA
C      INTEGER         IDREC(6)
C      INTEGER         NDATA(3,NDATASIZ)
C      WHERE    NDATA    - BIG BIN FOR RESULTING DATA(3,NDATASIZ)
C      AND NDATASIZ    - J DIMENSION OF NDATA (NO. OF OBS IT CAN HOLD+1)

C     COMMON   - / DATE /NYR,NMON,NDAY,NHR
C     COMMON   - / DAT1 /IHR1,IDA1,IMO1,IYR1
C     COMMON   - /WLONG0/ WLONG0
C
C REMARKS:  
C   THIS 97-01-30 VERSION IS STRIPPED DOWN TO GET SOMETHING TO FLOW;
C     IT HAS LIMITED APPLICATIONS.
C   THIS DETERMINES A LOCALLY USED KEIL-VALUE FROM THE GIVEN ITOUT
C     AND USES THE LCL_KEIL AS ARG TO TRUIJX() TO DETERMINE I,J FROM
C     LAT,LONG.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C
C     -----------------------------------------------------------------
      INTEGER    MAXOBS
      PARAMETER (MAXOBS=20000)
      INTEGER    NDATASIZ
      PARAMETER (NDATASIZ=MAXOBS+1)
      INTEGER    LMTWRDPOB
      PARAMETER (LMTWRDPOB=10)
      INTEGER    LMTHFWPOB
      PARAMETER (LMTHFWPOB=2*LMTWRDPOB)		!... =(20)

      COMMON  /OBSLVLDB/NOBSDB,LVLIX,IOBS2PK
      INTEGER           IOBS2PK(LMTWRDPOB,NDATASIZ)

      COMMON  /JSPACE/IDREC,NDATA
      INTEGER         IDREC(6)
      INTEGER         NDATA(3,NDATASIZ)

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


C     ================================================================= 
      COMMON   / DATE /NYR,NMON,NDAY,NHR
      COMMON   / DAT1 /IHR1,IDA1,IMO1,IYR1
      COMMON   /WLONG0/ WLONG0
C
      INTEGER*8  DBLPKD
      INTEGER*8  DBLWDT
      INTEGER*8  DBLWRD
      INTEGER*8  IDEWRD
      INTEGER*8  IUVDT
      INTEGER*8  LLWRD
      INTEGER*8  MRGPAP

      CHARACTER*8  NEWPAP
      DATA         NEWPAP         /'NEWPAP  '/
C
C
      LOGICAL    LDEWAS,LDEWDO
C
      LOGICAL    LNEWPA
C
C
      INTEGER    IDX(6)
      INTEGER    IUPK(12)
      INTEGER    KBITRA(5)
      DATA       KBITRA        /8,8,4,2,1/
      INTEGER    LAGS(3)
      INTEGER    LMTM(5)
      DATA       LMTM          /0300,0900,1500,1800,2100/
C
C
      DATA     ABGMXI         /109.0/
      DATA     ABGMXJ         / 81.0/

      DATA     DISSAT         /6.619/
      DATA     KTBIT          /2/
      DATA     KTBUT          /Z'20'/
      DATA     KTDBIT         /1/
C ...     DATA     KTDBUT         /Z'10'/
      DATA     KWBIT          /8/
      DATA     KWBUT          /Z'80'/
      DATA     KZBIT          /4/
      DATA     KZBUT          /Z'40'/
      DATA     LIMT1          /0450/
      DATA     LIMT2          /1800/
      DATA     LOLDTM         /0000/
      DATA     LSYNTM         /1200/
C
      DATA     MAXDFE         /255/
C     ...WHERE MAXDFE IS USED IN FK00 AND FK01 FILE OPENERS...
C
      DATA     MAXFL          /616/
C     ...WHERE MAXFL IS THE ITH DIMENSION (IN SINGLE WORDS) OF INDAT
C
      DATA     MDPMIS         /Z'0000007F'/
      DATA     MLAGS          /Z'F000F000'/
      DATA     MSKOFF         /Z'0000000F'/
      DATA     MSKTIM         /Z'0000FFF0'/
      DATA     MSK9           /Z'1FF'/
C
      DATA     MSK9IN         /Z'3FE00'/
C     ...WHERE MSK9IN IS OCT 777000
C
      DATA     MSK18          /Z'3FFFF'/
C
C ...      DATA     MXINBN         /304/
C     ...WHERE MXINBN IS MAX NO. OF OBS IN ONE PAP RECORD
C     ...   304 DOUBLE WORDS + 4 PRECEDING MAKES 308 DOUBLE WORDS
C
      DATA     NO17           /Z'F'/
      DATA     NO360          /Z'F0'/
      DATA     NO37           /Z'0000001F'/
      DATA     NO3777         /Z'7FF'/
      DATA     NO77           /Z'3F'/
      DATA     NO7777         /Z'FFF'/
      DATA     SCALE          /26.7858/
      DATA     TWOB9          /512.0/
      DATA     XSHIFT         /10.0/
      DATA     YSHIFT         /10.0/
C
      INTEGER   I4WORK(LMTWRDPOB)		!...(10)
      INTEGER   LONELVL(LMTHFWPOB)		!...(20)
      REAL      FLONELVL(LMTHFWPOB)		!...(20)
C     ... TO UNPACK THE 64-BIT WORDS INTO 32-BIT WORDS,
C ... CALL GBYTES(JUPABUF,I4UPABIN,NOFFSET,NBITSGRP,NPADBITS,NGRPS2DO)
      INTEGER       NOFFSET
      DATA          NOFFSET       /  0 /
      INTEGER       NBITSGRP
      DATA          NBITSGRP      / 32 /
      INTEGER       NPADBITS
      DATA          NPADBITS      /  0 /
      INTEGER       NGRPS2DO

      INTEGER       NEGSIGNEXT
      DATA          NEGSIGNEXT     / X'FFFFFFFF00000000' /
      INTEGER       MSK1TO6BY
      DATA          MSK1TO6BY      / X'FFFFFFFFFFFF0000' /
      INTEGER       MSK12B
      DATA          MSK12B         / X'0000000000000FFF' /
      INTEGER       IPKDTIME
      REAL          TIME_HRS
      INTEGER       IDWRD
      INTEGER       IACC
      INTEGER       MQ
      INTEGER       ITCT
      INTEGER       LCL_KEIL
      REAL          AIMIN,AIMAX
      REAL          AJMIN,AJMAX
      REAL          ALAT,ALONG
C
      SAVE
C
C     . . .   S T A R T   . . . . . . . . . . . . . . . . . . . . . . 

      IERRA = 0
      ITCT = 0
      LNEWPA = .TRUE.

      AIMIN = 1.0
      AJMIN = 1.0
      AIMAX = 47.0
      AJMAX = 51.0

      IF((ITOUT .LT. 1) .OR. (ITOUT .GT. 14)) THEN
         GO TO 702		!... ERROR ON ITOUT OUT-OF-RANGE
      ENDIF
      GO TO 709

  702 CONTINUE
      PRINT 704, ITOUT
  704 FORMAT(1H , 'MKSORWRD: FAILED!!  GIVEN OUT-OF-RANGE ITOUT=', I8,
     1      /1H , 'UNABLE TO PROCESS ANY OBSERVATIONS' )
      IERRA = 1
      RETURN

  709 CONTINUE
      NPTYPS = 3
      LDEWDO = .FALSE.
C             1   2   3   4   5   6   7   8   9  10  11  12  13
C ... GO TO (710,720,730,740,750,760,770,780,702,790,795,798,798),ITOUT
      if(itout .eq. 1) then			!...710
        LCL_KEIL = 1
C       ...FOR LFM ...
        IF(LVLDES .GT. 8) GO TO 799
        IF(.NOT. LNEWPA) GO TO 799
        NPTYPS = 4
        LDEWDO = .TRUE.
        GO TO 799

      else if (itout .eq. 2) then		!... 720
        LCL_KEIL = 1
C       ...FOR LFM ....
        IF(LVLDES .GT.8) GO TO 799
        IF(.NOT. LNEWPA) GO TO 799
        NPTYPS = 4
        LDEWDO = .TRUE.
        GO TO 799

      else if (itout .eq. 3) then		!... 730
C       ...IQSY ON N HEMI ...
        LCL_KEIL = 2
        GO TO 799

      else if (itout .eq. 4) then		!... 740
        LCL_KEIL = 15
        AIMAX = 55.0
        AJMAX = 42.0
        GO TO 799

      else if (itout .eq. 5) then		!... 750
        LCL_KEIL = 3
        GO TO 799

      else if (itout .eq. 6) then		!... 760
        LCL_KEIL = 2
C       ...FOR SIRS PLOTTED CHART ...
        GO TO 799

      else if (itout .eq. 7) then		!... 770
C       ...INTIIALIZE FOR TROPIC PLOT ON MERCATOR
        LCL_KEIL = 6
        GO TO 799

      else if (itout .eq. 8) then		!... 780
C       ...INITIALIZE FOR LARGE 1/20M BACKGROUND.
        LCL_KEIL = 7
        GO TO 799

      else if (itout .eq. 9) then		!... No ITOUT==9
        go to 702				!... ERROR

      else if (itout .eq. 10) then		!... 790
C       ...INITIALIZE FOR GENERALIZED PLOT ON NH 1/40M BACKGROUND.
        LCL_KEIL = 2
        GO TO 799

      else if (itout .eq. 11) then		!... 795
C       ...INITIALIZE FOR NH 1/60M BACKGROUND...
        AIMAX = 65.0
        AJMAX = 65.0
        LCL_KEIL = 14
        GO TO 799

      else if ((itout .eq. 12) .or. (itout .eq. 13)) then	!...798
C       ...GOES-TYPE DISPLAY.
        LCL_KEIL = 0
        GO TO 799

      else if (itout .eq. 14) then
C       ...INITIALIZE FOR PLOT ON SH 1/20M BACKGROUND.
        LCL_KEIL = 16
        AIMAX =  109.0
        AJMAX =  144.0
        GO TO 799

      else
        GO TO 702
      endif
      go to 799
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  799 CONTINUE
      LVL = LVLDES
C     ...BETTER CHECK FOR LVL W/I VALID RANGE...
      IF((LVL .GT. 0) .AND. (LVL .LT. 22)) GO TO 810
C       ... OTHERWISE, BAD MB-LEVEL ...
      PRINT 800,LVL
  800 FORMAT(1H ,'MKSORWRD: FAILED.  OUT-OF-RANGE LVL =',I8,
     X      /1H ,'UNABLE TO INITIALIZE MAPPING CONSTANTS')
      IERRA = 2
      RETURN
C
  810 CONTINUE
      NDIM = NDATASIZ - 1

C     ...IN THE CDC6600 THE IDREC WAS ALWAYS JUST BEFORE NDATA BIG BIN
C     ...   IN CORE AND THERE WAS SOMETHING ABOUT THE AJM SORTER WHICH
C     ...   LOOKED OFF THE FRONT END OF THE BIN, SO IDREC(1) WAS CHANGED
C     ...   HERE TO BE SURE THE SORTER SAW SOMETHING W/I RANGE...

      MDIM = NDIM + 1
      DO  55  MK = 1,MDIM
        NDATA(1,MK) = 0
        NDATA(2,MK) = 0
        NDATA(3,MK) = 0
   55 CONTINUE
C     ...WHICH ZEROD THE BIG BIN
C     ...INITIALIZE COUNTER FOR NDATA OBS COUNT
      MK = 1
C
C     *     *     *     *     *     *     *     *     *     *     *
C
   82 CONTINUE
C     ...TO PROCESS THIS BIN OF OBSERVATIONS ...
      KRDL = NOBSDB
C     ...WHERE KRDL HAS COUNT OF OBS CONTAINED IN THIS DATA BIN
      IF(KRDL .LE. 0) GO TO 84
      IF(KRDL .GT. MAXOBS) GO TO 84
C     ...OTHERWISE, COUNT OF OBS WITHIN ONE BIN IS WITHIN RANGE...
      GO TO 88
C
   84 CONTINUE
C     ...COMES TO 84 IF COUNT OF OBS IN BIN WAS ERRONEOUS...
      PRINT  86, NOBSDB
   86 FORMAT(1H , 'MKSORWRD:*** ERROR RETURN=3 ***.' 
     1      /1h , 'COMMON/OBSLVLDB/NOBSDB COUNTER HAS OUT-OF-RANGE '
     2            'COUNT= ', I8)
      IERRA = 3
      RETURN
C
   88 CONTINUE

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     ...INITIALIZE COUNTER FOR NDATA OBS COUNT
      MK = 1
      LUPEND = KRDL
      DO  200  N = 1,LUPEND
        IF(N .GE. NDIM) GO TO 500

        DO  I = 1,LMTWRDPOB
          I4WORK(I) = IOBS2PK(I,N)	!... GOT ONE STNS OBS FOR ONE LV
        ENDDO

        NGRPS2DO = LMTHFWPOB
        CALL GBYTES(I4WORK,LONELVL,NOFFSET,NBITSGRP,NPADBITS,NGRPS2DO)

        do ihw = 1,NGRPS2DO
          IF(BTEST(LONELVL(IHW),31)) THEN	!... HI-ORDER BIT
            LONELVL(IHW) = IOR(NEGSIGNEXT,LONELVL(IHW))
          ENDIF
        ENDDO

        DO IB = 1,NGRPS2DO
          IF((IB .EQ. 11) .OR. (IB .EQ. 12)) THEN
            FLONELVL(IB) = 0.0
          ELSE
            FLONELVL(IB) = FLOAT(LONELVL(IB)) / 4096.0
          ENDIF
        ENDDO
        
        IDWRD = IAND(I4WORK(6),MSK1TO6BY)		!... NAME
        IOFFTIME = 5			!... SET OFFTIME-CODE = 5
        IDWRD = IOR(IDWRD,IOFFTIME)
        TIME_HRS = FLONELVL(4)
        IPKDTIME = NINT(100.0*TIME_HRS)
        IPKDTIME = IAND(IPKDTIME,MSK12B)
        IPKDTIME = ISHFT(IPKDTIME,4)
        IDWRD = IOR(IDWRD,IPKDTIME)  	!... /NAME/TIME/OFFTIME/
C                                       !... / 48 / 12 / 4     /

        ALAT = FLONELVL(1)
        ALONG = FLONELVL(2)
        IELEV = NINT(FLONELVL(7))
        IRS = 0
        ITYPE = ISHFT(LONELVL(9),-12)

        IF(ITOUT .EQ. 3) ITYPE=IAND(ITYPE,NO77)
C     ...WHICH DID A SPECIAL ERASER OF TRASH IN HI-ORDER OF TYPE IF UAB

        IF(ITYPE .GT. NO77) GO TO 200
C       ...WHICH DISCARDS ALL SFC OBS...

        IF(.NOT. LNEWPA) GO TO 190

C ...        IF((ITYPE .EQ. 41) .OR. (ITYPE .EQ. 45)) GO TO 200
C ...    TYPE 45 IS MY OWN DESIGNATION (IN ACPROC AND PRESOR) FOR ACARS.
C     ...WHICH DISCARDS ALL AIRCRAFT.  REMOVE THIS AFTER BETTER THINNING
C       ... REMOVED ON 97-02-25 ...

C ...        IF(ITYPE .EQ. 62) GO TO 200		!... SAT winds
C ...        IF(ITYPE .EQ. 63) GO TO 200
C           ...TEMPORARY PATCH TO ELIM. SATWINDS UNTIL ALTITUDE TESTED

  190   CONTINUE
        IF(ITYPE .NE. 61) GO TO 92
        IF(ITOUT .EQ. 1) GO TO 90
        IF(ITOUT .EQ. 2) GO TO 90
        GO TO 92

   90   CONTINUE
C       ...TEST FOR SIRS ATTACHED TO REF LVL ON 1DOT  AND 2DOT BFR USE
        IF(IRS .NE. 0) GO TO 200
C
   92   CONTINUE
        ISHTYP = ISHFT(ITYPE,42)		!... 10+32 = 42
        IF(ITOUT .EQ. 7) GO TO 130

        IF((ITOUT .EQ. 12) .OR. (ITOUT .EQ. 13)) GO TO 140

        IF(LCL_KEIL .EQ. 3) GO TO 100
C       ... otherwise,
        IF(ALAT .LT. 0.0) THEN
          GO TO 200
C         ... WHICH DISCARDED THIS SRN HEMISPHERE OBS ...
        ELSE
C         ... THIS IS A NORTHERN HEMI OBS ...
          GO TO 120
        ENDIF

  100   CONTINUE
C       ...COMES HERE IF DOING SRN HEMISPHERE, SO SAVE SRN HEMI
        if(ALAT .GT. 0.0) THEN
          GO TO 200
C         ...WHICH DISCARDED ALL NRN HEMI.
        ENDIF
C       ... OTHERWISE, THIS IS A SOUTHERN HEMISPHERE OBS ...
        GO TO 120

C       . . . . . . . . . . . . . . . . . . . . . . . . 
  120   CONTINUE
        IF(LCL_KEIL .EQ. 7) GO TO 125		!... big NHemi

        CALL TRUIJX(ALAT,ALONG,XI,XJ,LCL_KEIL,IRET_TIJ)
        IF(XI .LT. AIMIN) GO TO 200
        IF(XI .GT. AIMAX) GO TO 200
        IF(XJ .LT. AJMIN) GO TO 200
        IF(XJ .GT. AJMAX) GO TO 200
        GO TO 160

  125   CONTINUE
        CALL TRUIJX(ALAT,ALONG,XI,XJ,LCL_KEIL,IRET_TIJ)
        IF(XI .LT. AIMIN) GO TO 200
        IF(XI .GT. ABGMXI) GO TO 200
        IF(XJ .LT. AJMIN) GO TO 200
        IF(XJ .GT. ABGMXJ) GO TO 200
        GO TO 160

  130   CONTINUE
C       ...COMES TO 130 FOR I/J ON MERC ...
        CALL MERCXY(ALAT,ALONG,TX,TY,IERR)
        IF(IERR .NE. 0) GO TO 200
        XI = 45.0 - TY
        XJ = TX
        IF(XI .LT. 1.0) GO TO 200
        IF(XI .GT. 44.0) GO TO 200
        GO TO 160

  140   CONTINUE
C       ...COME HERE FOR I/J ON PSEUDO-GOES.
        CALL GOESXY(WLONG0,DISSAT,SCALE,ALAT,ALONG,XI,XJ,IEXIT)
        IF(IEXIT .EQ. 1) GO TO 200
C
        XI = XI + XSHIFT
        XJ = XJ + YSHIFT
        XI = XI * 2.0 + 1.0
        XJ = XJ * 2.0 + 1.0
C
C       THIS CONVERTS FROM INCHES TO GRID INTERVALS.
C
        GO TO 160

  160   CONTINUE
C       ...TO PACK IJ INTO WORD 1 OF THREE...
        ISTN = NINT(XI*TWOB9)
        ISTN = IAND(ISTN,MSK18)
        MQ = ISHFT(ISTN,14)
C       ...TO POSITION  III.III LEFT-JUSTIFIED 18 BITS OF 32-BIT WORD

        JSTN = NINT(XJ*TWOB9)
        JSTN = IAND(JSTN,MSK18)
        J3 = IAND(JSTN,MSK9IN)		!... J3 IS INTEGER PORTION OF J
        J3 = ISHFT(J3,-9)		!... RIGHT-JUSITFIED
        J3 = ISHFT(J3,32)		!... RT-JUST IN LHS 32 BITS

        JFR = IAND(JSTN,MSK9)
        JFR = ISHFT(JFR,5)

        IACC = IOR(J3,ISHTYP)		!... IACC IS HI-ORDER 32 STUFF
        MQ = IOR(MQ,JFR)		!... MQ IS LO-ORDER 32 STUFF
        IACC = IOR(IACC,MQ)

        NDATA(1,MK) = IACC
C       ...WHERE NDATA(1,MK) IS A DOUBLE WORD CONTAINING THE IJ
C       ...   /ZEROS/ITYPE/UNUSED/JJJ/  III/III/JJJ/ZEROS/
C       ...   /16   /6    /  1   / 9 /   9 / 9 / 9 /  5  /
C       ...       WHERE SECOND III AND JJJ ARE FRACTIONAL BITS...
        NDATA(2,MK) = IDWRD
        NDATA(3,MK) = N
        mk = mk + 1
C     *     *      *     *     *     *     *     *     *     *     *
C ...        IF(ITOUT .NE. 3) GO TO 420
C       ...OTHERWISE, ITOUT = 3 WHICH IS IQSY WITH UABPAP DATA...

  200 CONTINUE
C     ...   where 200 is ENDDO on  N = 5,LUPEND   . . . . . . . . . . .

      GO TO 600

  500 CONTINUE
      PRINT  320,NDIM,LVL
  320 FORMAT(1H ,'INPUT OBSERV DATA WILL BE TRUNCATED AT ', I5,' OBS AT
     1LEVEL = ', I2)

  600 CONTINUE
      ITCT = MK
      PRINT  340, ITCT
  340 FORMAT(1H , 8X, 'TOTAL OBSERVATIONS KEPT BY MKSORWRD FOR THIS ',
     1           ' LEVEL = ', I5)

      RETURN
      END
