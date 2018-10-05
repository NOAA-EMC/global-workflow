      SUBROUTINE TSTBND( ISTN,JSTN,KDDGD,ISPEED,NWOK,IOPTN,IRET_WNB)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    TSTBND      MAKES SURE STATION IS ON MAP            
C   PRGMMR: LARRY SAGERAR      ORG: NP12      DATE:2006-03-30
C
C ABSTRACT: MAKES SURE STATION IS ON GRAPHIC.  IF NOT, STATION IS   
C   FLAGGED BY A NON ZERO RETURN CODE AND IS DROPPED.
C
C PROGRAM HISTORY LOG:
C 2006-03-30  LARRY SAGER   CONVERTED SUBROUTINE WNDBRK TO TSTBND             
C
C USAGE:    CALL TSTBND( ISTN,JSTN,KDDGD,ISPEED,NWOK,IOPTN,IRET_WNB )
C   INPUT ARGUMENT LIST:
C     ISTN     - ICOORDINATE OF WIND PLOT IN DOTS.
C     JSTN     - JCOORDINATE OF WIND PLOT IN DOTS.
C     KDDGD    - WIND DIRECTION TO NEAREST 10 DEGREES.
C     ISPEED   - WIND SPEED IN KNOTS.
C     IOPTN    - FLAG TO DETERMINE IF TEMPERATURE AND DEW POINT
C              - PLOTS ARE IN THE WAY OF PLOTTING WIND FLAGS.
C
C   OUTPUT ARGUMENT LIST:
C     NWOK     - COUNTER OF NUMBER OF LABELS ADDED TO LABEL ARRAY.
C     IRET_WNB - RETURN CODE
C              =  0;  NORMAL RETURN
C              = -1;  PARITY ERROR WHILE TRYING TO OUTPUT LABEL ARRAY;
C                       USER SHOULD ABORT ON THIS SERIOUS I/O ERROR.
C              = -2;  PUTL_WR::HAFPAKRA: FAILED TO HALF-PACK LABEL ARRAY
C                       SERIOUS ERROR.  USER SHOULD ABORT.  LOGIC ERR.
C
C              =  1;  TSTBND: WARNING ... BAD IDDGD
C              =  2;  TSTBND: WARNING ... BAD ISPEED
C              =  3;  TSTBND: WARNING ... BAD CUTOFF VALUE
C              =  4;  TSTBND: WARNING ... OUT-OF-RANGE FLAG COUNT
C              =  5;  TSTBND: WARNING ... LOCATION OFF MAP
C
C   INPUT FILES:
C
C   OUTPUT FILES:
C
C REMARKS: BLOCK DATA WNDCON IS USED BY THIS SUBR.
C     CALLS SUBR PUTL_WR(), WHICH CALLS SUBR HAFPAKRA()
C     CAUTION:  CALL SEQ. CHANGE WITH ADDED IRET_WNB
C
C ATTRIBUTES:
C   LANGUAGE: F90.
C   MACHINE:  IBM
C
C$$$
C
C

      INTEGER    LBLTAP
      PARAMETER (LBLTAP=55)

      INTEGER    LMAX
      PARAMETER (LMAX=1024)
      INTEGER    LMAX2
      PARAMETER (LMAX2=2*LMAX)

      COMMON  /KPLOT/ LABEL,LABIX,NOBUF,IDRA(50)
      INTEGER      LABEL(2,LMAX)
      INTEGER      JARRAY(LMAX2)
      EQUIVALENCE (JARRAY(1),LABEL(1,1))
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

      EXTERNAL   WNDCON    	!... FOR COMMON /WINCON/
C                        	!...   DEFINED IN BLOCK DATA WNDCON
      COMMON  /WINCON/ KWNDFL(5,36),KWNDDV(5,36),KWNDBA(10,9)
      INTEGER          KWNDFL
      INTEGER          KWNDDV
      INTEGER          KWNDBA

C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C USAGE:    CALL WNDBRK( ISTN,JSTN,KDDGD,ISPEED,NWOK,IOPTN,iret_wnb )
C   INPUT ARGUMENT LIST:
       INTEGER    ISTN,JSTN
C               	!... I-,J-COORDINATES OF WIND PLOT IN DOTS.
       INTEGER    KDDGD
C               	!... WIND DIRECTION TO NEAREST 10 DEGREES.
       INTEGER    ISPEED
C              		!... WIND SPEED IN KNOTS.
       INTEGER    IOPTN
C        		!... FLAG TO DETERMINE IF TEMPERATURE AND DEW-
C        		!...    POINT PLOTS ARE IN THE WAY OF PLOTTING 
C         		!...    WIND FLAGS.
C
C   OUTPUT ARGUMENT LIST:
       INTEGER    NWOK
C        		!... COUNTER OF NUMBER OF LABELS ADDED TO 
C        		!...    LABEL ARRAY.
       INTEGER    IRET_WNB
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C
      INTEGER      IARWUP
C     ...WHICH IS 'ARROW-UP' CHAR FONT CHANGE BIT...
      DATA         IARWUP      / X'00010000' /

      INTEGER      IPRBA
      DATA         IPRBA       / X'00006000' /
C     ...FOR WIND BARBS/STAFF ... PRIORITY = 3, NO 'ARROW-UP' BIT...

      INTEGER      IPRFL(2)
      DATA         IPRFL       /X'00014000',X'00018000'/
C     ... WHERE IPRFL IS PRIORITY ASSIGNED TO WIND-FLAG FONT ....
C     ... WHICH ARE 'ARROW UP' BIT AND PRIORITY = 2 AND 4
C     ... WHERE IPRFL ALSO INCLUDES ARROW-UP FOR WIND-FLAG FONT ...


      INTEGER      IRUBA
      DATA         IRUBA       / X'01200000' /
C     ...WHICH IS TEXT WORD FOR BLANK FROM FONT=01 FOR ERASER UNDER FLAG

      INTEGER      IRUBB
      DATA         IRUBB       / X'14010000' /
C     ...WHICH IS TEXT WORD FOR BLANK FROM FONT=20 FOR ERASER UNDER BARB

      INTEGER      IRUBC
      DATA         IRUBC       / X'03200000' /
C     ...WHICH IS TEXT WORD FOR BLANK FROM FONT=03 FOR ERASER UNDER FLAG

      INTEGER      IXXK
      DATA         IXXK        / 5 /
C     ...WHICH IS THE XXTH LOOK TABLE BEFORE THE WIND-FLAG FONTS

      INTEGER      ICORN
      DATA         ICORN       /0/

      integer      ICORNFLAG(2)
      DATA         ICORNFLAG   / -1, 0 /
C     ... WHERE ICORNFLAG(IHEMIS) MOVES NRN HEMI WIND-FLAG LEFTWARD -1
C     ...                         BUT SRN HEMI WIND-FLAG NEEDS NO ADJ

      INTEGER      ILLIM
      DATA         ILLIM       /   10 /
      INTEGER      IRLIM
      DATA         IRLIM       / 4000 /
      INTEGER      JLLIM
      DATA         JLLIM       /    1 /
      INTEGER      JTLIM 
      DATA         JTLIM       / 7020 /

C     ... TO CONVERT WIND DIRECTION INT DD INTO AN ASCII CHAR
C     ...    WHICH WILL GET THE CORRECT BIT-MAPPED CHARAC ...
C     ... TO CONVERT A BINARY INT X'01' ==> 'A'
C                                 X'1A' ==> 'Z'
C                                 X'1B' ==> '0'
C                                 X'24' ==> '9'
      CHARACTER*1  CTTBL(36)
C                           ... 01  02  03  04  05  06  07  08  09 ...
      DATA         CTTBL      / 'A','B','C','D','E','F','G','H','I',
     1                          'J','K','L','M','N','O','P','Q','R',
     2                          'S','T','U','V','W','X','Y','Z','0',
     3                          '1','2','3','4','5','6','7','8','9' /
C                           ... 28  29  30  31  32  33  34  35  36 ...

      INTEGER      KDOLR(4)
      DATA         KDOLR       /X'00000000',X'00000000',
     X                          X'00000000',X'00000000'/
C     ...WHICH IS NULL IN 4 BYTE POSITIONS...

      INTEGER      KDEWPT
      DATA         KDEWPT      / X'00000008' /
C     ...DEW POINT PLOT INFO BIT...
      INTEGER      KTEMP
      DATA         KTEMP       / X'00000004' /
C     ...TEMP PLOT INFO BIT...

      INTEGER      KDR220
      DATA         KDR220      /22/
      INTEGER      KDR260
      DATA         KDR260      /26/
      INTEGER      KDR300
      DATA         KDR300      /30/
C     ...WIND DIR LIMITS  OF WIND OVERPLOTTED BY DEW PT, TEMP...


      INTEGER      LGAPK(5,8)
      DATA         LGAPK       / -7,-8,-11,-8,X'09410000',
     1                           -9,-7,-11,-8,X'09420000',
     2                          -10,-6,-11,-8,X'09430000',
     3                          -11,-4,-11,-5,X'07440000',
     4                          -11,-2,-11,-5,X'07450000',
     5                          -11, 0,-11, 0,X'07460000',
     6                          -11, 2,-11, 0,X'07470000',
     7                          -10, 4,-11, 0,X'07480000'/
C     ...PENTAPACKED /IGAP,JGAP ORIGIN DISPLACEMENTS FOR OTHER WND PART/
C     ...DI,DJ TO LL FOR GAP FILLER LINE CHAR/LABEL TEXT FOR GAP FILLER/


      INTEGER      ISDIF(4)
      DATA         ISDIF       / 6, 12, 9, 12 /
C     ... DISPLACEMENTS FOR REVERSED WIND FLAGS USED IN SOUTHERN
C     ...    HEMISPHERE IN ORDER BY FLAG TYPE ...

      INTEGER      LSDI(36)
      DATA         LSDI      / +2, +1, +1, +3, +5,+09, +7, +5, +5, 
     1                         +4, +5, +6,+12,+15,+18,+21,+20,+20,
     2                        +20,+21,+18,+15,+14, +7, +6, +6, +5, 
     3                         +6, +8, +9, +6, +3, +1, +1, +3, +2 /
C
C ...  FURTHER DISPLACEMENTS TO ADJUST SOUTHERN HEMISPHERE WIND FLAGS
C

      INTEGER      MSK16B
      DATA         MSK16B      / X'0000FFFF' /

      INTEGER      MSKI
      DATA         MSKI        / X'00001FFF' /
C     ...WHERE MSKI ALLOWS 13 BITS ...

      INTEGER      MSKJ
      DATA         MSKJ        / X'00007FFF' /
C     ...WHERE MSKJ ALLOWS FOR 15 BITS...

      INTEGER      MXKUT
      DATA         MXKUT       / 120 /
C
      LOGICAL      LPACK_RAQ
      LOGICAL      LCLEAN_AFTQ

      INTEGER      IACC,MQ
C
      INTEGER      KUTOFF

      INTEGER      LABWOK(2,15)

      INTEGER      IJWRD_ERAB
      INTEGER      ITXTWRD_ERAB

C
C---------------------------------------------------------------------
      SAVE
C
      NWOK = 0
      IRET_WNB = 0

C     ... SET NORTHERN HEMISPHERE FLAG = 1
      IHEMIS = 1 		!... NORTHERN HEMISPHERE MODE
      IDDGD = KDDGD
C     ... IT IS ASSUMED THAT A ZERO OCCURS FOR WIND DIRECTION ONLY
C     ...   IN THE NORTHERN HEMISPHERE (I.E.  IDDGD = +0)
      IF ( IDDGD .EQ. 0 ) IDDGD = 36
      IF ( IDDGD .GE. 0 ) GO TO 100

C     ... OTHERWISE, IDDGD .LT. 0;  WHICH FLAGGED SOUTHERN HEMI MODE
C     ... SET SOUTHERN HEMISPHERE FLAG = 2
      IHEMIS = 2   		!... SOUTHERN HEMISPHERE MODE
      IDDGD = IABS(IDDGD)
C     ... SAVE FOR POSITIONING FLAGS
      LDDSH = IDDGD
C     ... IDDSH POINTS TO VECTOR WHICH WHEN REVERSED WILL BE THE
C     ... SOUTHERN HEMISPHERE VECTOR FOR IDDGD ...
      IDDSH = 36 - IDDGD
      IF ( IDDSH .EQ. 0 ) IDDSH = 36
  100 CONTINUE
      IF(IDDGD .GT. 36) GO TO 911
      IF(ISPEED .LT. 0) GO TO 922
      IF(ISPEED .GT. 300) GO TO 922
C     ...STEP (1)... TO DETERMINE NO. OF FLAGS AND NO. OF BARBS......
      FFIVES = (FLOAT(ISPEED))/5.0
      IFIVES = FFIVES + 0.5
C     ...WHICH ROUNDS TO NEAREST 5-KNOT-UNIT  AND FIXES
      NFLAGS = IFIVES/10
      NBARBS = MOD(IFIVES,10)
C     ...WHERE NO. OF BARBS RANGES FROM 0 TO 9 (FOR NONE TO 45K)
C     ...STEP(1B) ... TEST FOR MOVING WIND VECTOR PLOT  FOR OVERPLOT
      IGAP = 0
      JGAP = 0
      IF(NBARBS .LE. 2) GO TO 177
C
C     ... IF WIND DIR LESS THAN 220 DEGREES, THEN  SKIP TO 177 ...
C
      IF(IDDGD .LT. KDR220) GO TO 177
C
C     ... IF WIND DIR LESS THAN 260 DEGREES, THEN SKIP TO 140 ...
C     ... AND CHECK TO SEE IF THE DEW POINT PLOT FLAG IS SET.
C
      IF(IDDGD .LT. KDR260) GO TO 140
C
C     ...IF WIND DIR LESS THAN 300 DEGREES, THEN SKIP TO 144 ...
C     ...  AND CHECK TO SEE IF THE TEMP PLOT FLAG IS SET.
C
      IF(IDDGD .LT. KDR300) GO TO 144
      GO TO 177

  140 CONTINUE
C     ... GET DEW POINT PLOT FLAG ...
      IAC3 = IAND(IOPTN,KDEWPT)
      IF(IAC3)150,177,150

  144 CONTINUE
C     ... GET TEMP PLOT FLAG.
      IAC4 = IAND(IOPTN,KTEMP)
      IF(IAC4) 150,177,150
  150 CONTINUE
C     ...COMES HERE IF WIND BARBS MUST BE MOVED WESTWARD TO NOT OVERPLOT
      LLX = IDDGD - KDR220 + 1
      IGAP  = LGAPK(1,LLX)
      JGAP  = LGAPK(2,LLX)
      ILL   = LGAPK(3,LLX) + ISTN
      JLL   = LGAPK(4,LLX) + JSTN
      ITEXT = LGAPK(5,LLX)
      ILL = ILL + ICORN

      IF(ILL .LT. ILLIM) GO TO 955
      IF(ILL .GE. IRLIM) GO TO 955
      IF(JLL .LT. JLLIM) GO TO 955
      IF(JLL .GE. JTLIM) GO TO 955

      JLL  = IAND(JLL,MSKJ)
      ILL  = IAND(ILL,MSKI)
      JLS  = ISHFT(JLL,17)
      IJLL = IOR(JLS,ILL)
      NWOK = NWOK + 1
      LABWOK(1,NWOK) = IOR(IJLL,IPRFL(1))
      LABWOK(2,NWOK) = ITEXT

  177 CONTINUE

C     ....STEP (2) ... TO GET CUT-OFF CONSTANT FOR BARBS...
      NBONE = NBARBS + 1
      IF(IHEMIS .EQ. 2) IDDGD = IDDSH
      IHOLD  = MOD(IDDGD,9)
      IHOLD  = IHOLD + 1
      KUTOFF = KWNDBA(NBONE,IHOLD)
      IF(KUTOFF .LT. 0) GO TO 933
      IF(KUTOFF .GT. MXKUT) GO TO 933
      IF(IHEMIS .EQ. 2) KUTOFF = -KUTOFF
      IACC = IAND(IDDGD,MSK16B)
      IACC = ISHFT(IACC,16)
      MQ = IAND(KUTOFF,MSK16B)
      IACC = IOR(IACC,MQ)
      ITEXT = IACC
C     ...STAFF INFO PACKED WITH DD IN FIRST 2 BYTES, KUTOFF RIGHT-JUSTIF

C     ...STEP (3)... TO POSITION STAFF WITH LOWER LEFT CORNER....
C     ...KWNDDV FORMAT ... ISTART,JSTART,KWIDTH,KHGT,MXV...
      ISTART = KWNDDV(1,IDDGD)
      JSTART = KWNDDV(2,IDDGD)
      KWIDTH = KWNDDV(3,IDDGD)
      IF ( IHEMIS .EQ. 2 ) ISTART = KWIDTH - ISTART
      ILL = ISTN - KWIDTH + ISTART
      ILL = ILL + IGAP + ICORN
      IF(ILL .LT. ILLIM) GO TO 955
      IF(ILL .GE. IRLIM) GO TO 955

      JLL = JSTN - JSTART + 1
      JLL = JLL + JGAP
      IF(JLL .LT. JLLIM) GO TO 955
      IF(JLL .GE. JTLIM) GO TO 955

      JLL = IAND(JLL,MSKJ)
      ILL = IAND(ILL,MSKI)
      JLS = ISHFT(JLL,17)
      IJLL = IOR(JLS,ILL)
C     NWOK = NWOK + 1
C     LABWOK(1,NWOK) = IOR(IJLL,IPRBA)		!... there goes BARBS
C     LABWOK(2,NWOK) = ITEXT

C     ... insert the  eraser under the BARBs ...
      IJWRD_ERAB = 0
      IJWRD_ERAB = IOR(IJLL,IARWUP)
      ITXTWRD_ERAB = IRUBB
C     NWOK = NWOK + 1
C     LABWOK(1,NWOK) = IJWRD_ERAB		!... ERASER UNDER BARB
C     LABWOK(2,NWOK) = ITXTWRD_ERAB
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      IF(NFLAGS .LE. 0) GO TO 500

C     ...OTHERWISE, WE HAVE SOME FLAGS TO POSITION...
C     STEP (4)... TO GET FLAG INFO FOR PRTITL
C     ...DI TO LL,DJ TO LL,DI TO NEXT FLAG, DJ TO NEXT, FLAG TYPE.
      ITYPFL = KWNDFL(5,IDDGD)
      ISDI   = KWNDFL(1,IDDGD)
      IF(IHEMIS .EQ. 2) THEN
        ISDI = KWNDFL(1,LDDSH) - ISDIF(ITYPFL) + LSDI(LDDSH)
      ENDIF

      ILLFL = ISTN + ISDI
      ILLFL = ILLFL + IGAP + ICORN
      ILLFL = ILLFL + ICORNFLAG(IHEMIS)
      JLLFL = JSTN + KWNDFL(2,IDDGD)
      JLLFL = JLLFL + JGAP
      IF(ILLFL .LT. ILLIM) GO TO 955
      IF(ILLFL .GE. IRLIM) GO TO 955
      IF(JLLFL .LT. JLLIM) GO TO 955
      IF(JLLFL .GE. JTLIM) GO TO 955
      JLLFL = IAND( JLLFL,MSKJ)
      ILLFL = IAND( ILLFL,MSKI)
      JLS   = ISHFT(JLLFL,17)
      IJLFL = IOR(JLS,ILLFL)
      IPOSN = IOR(IJLFL,IPRFL(IHEMIS))

C     ...STEP (5)... TO GET TEXT FOR FLAG
      ICLASS = IXXK + KWNDFL(5,IDDGD)
C     ...WHERE ICLASS IS USED (R1) AFTER ARROW TO POINT TO TABLE IN PRT

C     ==================================================================
      IACC = ISHFT(ICLASS,24)

      MQ = MOVA2I(CTTBL(IDDGD)) 	!... TRANSLATE DD-BINARY TO ASCII CHAR
      IAC2 = ISHFT(MQ,16)    	!... MOVE TO 2ND BYTE FROM HI-END OF I*2
      IACC = IOR(IACC,IAC2)
      ITEXT = IOR(IACC,KDOLR(3))
C     ==================================================================
C     NWOK = NWOK + 1
C     LABWOK(1,NWOK) = IPOSN
C     LABWOK(2,NWOK) = ITEXT

C     ...TO ERASE UNDER 50K FLAG
      IAPCT = IRUBA
      ILLER = ILLFL
      JLLER = JLLFL
      IJLER = IJLFL

C     ...      GO TO (415,419,426,424),ITYPFL
      IF(ITYPFL .EQ. 1) THEN
        ILLER = ILLFL - 2
        IF(ILLER .LE. 0) THEN
           ILLER = 1
        ENDIF
        JLS   = ISHFT(JLLER,17)
        IJLER = IOR(JLS,ILLER)

      ELSE IF(ITYPFL .EQ. 2) THEN
        IAPCT = IRUBC
        JLLER = JLLFL - 2
        IF(JLLER .LE. 0) THEN
          JLLER = 1
        ENDIF
        JLS   = ISHFT(JLLER,17)
        IJLER = IOR(JLS,ILLER)

      ELSE IF(ITYPFL .EQ. 3) THEN
        GO TO 426

      ELSE IF(ITYPFL .EQ. 4) THEN
        IAPCT = IRUBC
      ENDIF   
      GO TO 426

  426 CONTINUE
      IJLER = IOR(IJLER,IARWUP)
C     NWOK = NWOK + 1
C     LABWOK(1,NWOK) = IJLER
C     LABWOK(2,NWOK) = IAPCT
      IF(NFLAGS .LE. 1) GO TO 500

C     ...STEP (6) TO GET FLAGS FOR 100KT AND UP...
      NEXDI = KWNDFL(3,IDDGD)
      IF ( IHEMIS .EQ.2 ) NEXDI = -NEXDI
      NEXDJ = KWNDFL(4,IDDGD)
      MORE = NFLAGS - 1
      IF(MORE .GT. 5) GO TO 944
      DO  433  I=1,MORE
        ILLFL = ILLFL + NEXDI
        JLLFL = JLLFL + NEXDJ
        IF(ILLFL .LT. ILLIM) GO TO 955
        IF(ILLFL .GE. IRLIM) GO TO 955
        IF(JLLFL .LT. JLLIM) GO TO 955
        IF(JLLFL .GE. JTLIM) GO TO 955

        JLLFL = IAND(JLLFL,MSKJ)
        ILLFL = IAND(ILLFL,MSKI)
        JLS = ISHFT(JLLFL,17)
        IJLFL = IOR(JLS,ILLFL)
C       NWOK = NWOK + 1
C       LABWOK(1,NWOK) = IOR( IJLFL,IPRFL(IHEMIS) )
C       LABWOK(2,NWOK) = ITEXT

C       ...NOW FOR THE ERASER UNDER THAT FLAG...
        ILLER = ILLER + NEXDI
        IF(ILLER .LE. 0) THEN
           ILLER = 1
        ENDIF
        JLLER = JLLER + NEXDJ
        IF(JLLER .LE. 0) THEN
           JLLER = 1
        ENDIF
        JLS = ISHFT(JLLER,17)
        IJLER = IOR(JLS,ILLER)
C       NWOK = NWOK + 1
C       LABWOK(1,NWOK) = IOR(IJLER,IARWUP)
C       LABWOK(2,NWOK) = IAPCT
  433 CONTINUE
C     ...WHEN IT FALLS OUT OF THIS LOOP, ALL FINISHED
      GO TO 500

  500 CONTINUE
C     ...COMES HERE TO TRANSFER FROM LABWOK TO LABEL AND OUTPUT IF NECES
      IF((LABIX+NWOK) .LE. LMAX) THEN
         DO  ISS = 1,NWOK
            LABIX = LABIX + 1
            LABEL(1,LABIX) = LABWOK(1,ISS)
            LABEL(2,LABIX) = LABWOK(2,ISS)
         ENDDO
         GO TO 999
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      ELSE
C        ...OTHERWISE IT WILL EXCEED LABEL BUFFER, SO TEST EACH WORD.
         DO  522  ISS = 1,NWOK
            LABIX = LABIX + 1
            IF(LABIX .LE. LMAX) THEN
              GO TO 520

            ELSE
C              ... OTHERWISE, BIN IS FULL, SO OUTPUT THE FULL BIN
C              ...   AND CLEAR THE BIN BEFORE PASSING THIS ISS-TH ITEM
               WRITE(6,503)
  503          FORMAT(1H ,'WNDBRK: LABEL ARRAY FULL' )
               LCKPT = 503
               LPACK_RAQ = .TRUE.
               LCLEAN_AFTQ = .TRUE.
C              ... ZERO THE IN-CORE LABEL-ARRAY AFTER WRITING ...
C              ... FOR CRAY VERSION, I MUST HALF-PACK  BEFORE WRITING,

C              CALL PUTL_WR(LBLTAP,NOBUF,JARRAY,LMAX2,LMAX,LPACK_RAQ,
C    1                      LCLEAN_AFTQ,IRET101)

               IF(IRET101 .NE. 0) THEN
                  IF(IRET101 .EQ. -1) THEN
                    GO TO 900
                  ELSE
                    WRITE(6,FMT='('' WNDBRK::PUTL_WR::HAFPAKRA: '',
     1                     ''FAILED WHEN HAF-PACKING LABEL ARRAY'')')
                    IRET_WNB = -2
                    GO TO 966
                  ENDIF
               ENDIF
C
               LABIX = 1
            ENDIF

  520       CONTINUE
C           ... TO MOVE THE ISS-TH ITEM FROM LABWOK INTO THE LABEL ARRAY
C           LABEL(1,LABIX) = LABWOK(1,ISS)
C           LABEL(2,LABIX) = LABWOK(2,ISS)
  522    CONTINUE
         GO TO 999
      ENDIF
      GO TO 999
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  900  CONTINUE
C      ... PARITY ERROR ON WRITING LABEL TAPE55  ...
       WRITE(6,905) LBLTAP,NOBUF,LABIX,LCKPT
  905  FORMAT(1H ,'WNDBRK:PARITY ERROR WRITING LABEL-TAPE',I2,
     1               /1H ,7X,'NOBUF=',I5,'; LABIX=',I5,'; LCKPT=', I3)

       IRET_WNB = -1     
       GO TO 966
C              ... 

  911  CONTINUE
       IRET_WNB = 1
       WRITE(6,913) IDDGD,ISTN,JSTN,ISPEED
  913  FORMAT(1H ,'WNDBRK ERROR 1...BAD IDDGD = Z ',Z8, 3X,
     1       'AT I/J = Z ', Z8,1H/, Z8, 3X, 'SPEED = Z ',Z8)
       GO TO 966

  922  CONTINUE
       IRET_WNB = 2
       WRITE(6,924) ISPEED, ISTN, JSTN, IDDGD
  924  FORMAT(1H ,'WNDBRK ERROR 2...BAD ISPEED = Z ', Z8, 3X,
     1        'AT I/J = Z ', Z8, 1H/, Z8, 3X, 'IDDGD = ', Z8)
       GO TO 966

  933  CONTINUE
       IRET_WNB = 3
       WRITE(6,935) KUTOFF,ISTN,JSTN,IDDGD,ISPEED
  935  FORMAT(1H ,'WNDBRK ERROR 3...BAD CUTOFF CONST FOR WND STAFF =
     1 Z ', Z8, 3X, 'AT I/J = Z ', Z8,1H/,Z8,3X, 'DIR/SPD = Z', Z8,
     2      1H/, Z8)
      GO TO 966

  944 CONTINUE
      IRET_WNB = 4
      WRITE(6,946) NFLAGS, ISTN,JSTN,IDDGD,ISPEED
  946 FORMAT(1H ,'WNDBRK ERROR 4...OUT-OF-RANGE FLAG COUNT = Z',
     1       Z8, 3X, 'AT I/J = Z ', Z8,1H/,Z8,3X,'DIR/SPD = Z ', Z8,
     2       1H/, Z8)
      GO TO 966
  955 CONTINUE
C     COMES TO 955 IF I/J IS OUT OF RANGE OF MAP
      IRET_WNB = 5
      WRITE(6,957) ISTN, JSTN, IDDGD, ISPEED
  957 FORMAT(1H ,'WNDBRK ERROR 5...LOCATION OFF MAP. ISTN = Z ',
     1       Z8, 3X, 'JSTN = Z ', Z8, 3X, 'DIR/SPD = Z ', Z8, 1H/, Z8)
      GO TO 966
  966 CONTINUE
      NWOK = 0
      GO TO 999

  999 CONTINUE
      RETURN
      END
