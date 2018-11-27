      SUBROUTINE W3FP02(ID,KTITLE,N)
C                                                   15-MAY-1996/DSS
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM:  W3FP02        NMC TITLING PROGRAM
C   PRGMMR: SHIMOMURA        ORG: W/NP12      DATE: 96-05-17
C
C ABSTRACT: GENERATES A TITLE FROM THE GIVEN DATA-FIELD IDENTIFIER 
C   FORMULATED ACCORDING TO NMC O.N. 84. THE RESULTING TITLE USES
C   SPACE UP TO 328 CHARACTERS (41 I*8 WORDS). 
C
C PROGRAM HISTORY LOG:
C  YR-MO-DA  ORIGINAL AUTHOR(S)'S NAME: ??
C  83-10-12  S. COBBS -- ADDED SUBR VALUE   (RENAMED TO FP02_VAL)
C  85-01-25  D. MILLER -- ADDED SUBR TCHECK (RENAMED TO DT_UNITS)
C  YR-MO-DA  S. COBBS -- MODIFIED TO RUN ON HDS-9040
C  95-01-31  ???? -- LAST MODIFIED OPNL VERSION  
C  96-05-17  SHIMOMURA -- MODIFIED TO RUN ON CRAY
C USAGE: CALL W3FP02 (ID, KTITLE, N)
C
C   INPUT ARGUMENT LIST:
C     'ID' - INTEGER*8   12-WORD FIELD-IDENTIFIER DESCRIBING THE DATA
C                     (IN THIS CRAY VERSION THE 32-BIT DATA WORDS ARE
C                      POSITIONED IN THE LOW-ORDER HALF OF THE CRAY
C                      64-BIT INTEGERS.)
C     ' N' - INTEGER*8   NUMBER OF LINES OF OUTPUT DESIRED
C              = 1  FIRST LINE (11 I*8 WORDS): THE PLAIN ENGLISH
C                     TITLE; 
C                    (88-BYTE LINE-1 STARTS AT KTITLE(1));
C
C              = 2  FIRST LINE PLUS SECOND LINE;
C                     SECOND LINE (16 I*8 WORDS): DECIMAL VALUES 
C                     OF THE PARAMETERS;
C                    (128-BYTE LINE-2 STARTS AT KTITLE(12));
C
C              = 3  FIRST LINE + SECOND LINE + THIRD LINE
C                     THIRD LINE (14 I*8 WORDS): HEXADECIMAL DUMP 
C                     OF THE 12 WORD FIELD IDENTIFIER; 
C                    (112-BYTE LINE-3 STARTS AT KTITLE(55));
C             NOTE: THERE ARE NO LINE DELIMITERS TO SEPARATE
C                     THE LINES.
C   OUTPUT ARGUMENT LIST:
C     'KTITLE' - INTEGER*8  KTITLE(41) 
C                ARRAY TO CONTAIN THE RESULTING TITLE IN ASCII
C
C REMARKS:
C   SEE NMC OFFICE NOTE 84 FOR DATA FIELD IDENTIFIER SPECS
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:  FP02_LN1,FP02_LN2,FP02_LN3,FP02_VAL,DT_UNITS
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C
C
      integer   ID(12)
      integer   KTITLE(41)   		!... 41 CRAY I*8 == 82 IBM I*4 
C ...      integer   KTITLE(82)

      INTEGER      KLANKS
      CHARACTER*8  CLANKS
      EQUIVALENCE (KLANKS,CLANKS)
      DATA         CLANKS  / '        ' /
C                          ...12345678...
C
      SAVE
C

      DO I = 1,11
        KTITLE(I) = KLANKS
      ENDDO

      CALL FP02_LN1(ID,KTITLE)

      IF(N .GT. 1) GO TO 10
      RETURN
C
   10 CONTINUE

      DO I = 12,27
        KTITLE(I) = KLANKS
      ENDDO

      CALL FP02_LN2(ID,KTITLE)

      IF(N.GT.2) GO TO 20
      RETURN
C
   20 CONTINUE

      DO I = 28,41
        KTITLE(I) = KLANKS
      ENDDO

      CALL FP02_LN3(ID,KTITLE)

      RETURN
      END

      SUBROUTINE FP02_LN1(ID,JTITLE)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: FP02_LN1          CREATES THE FIRST LINE OF TITLE
C   AUTHOR: COBBS,S          ORG: W/NMC42    DATE: 85-01-25
C
C ABSTRACT: CREATES THE FIRST LINE OF THE TITLE FROM THE
C   ID WORDS.
C
C
C USAGE:  CALL FP02_LN1 (ID,JTITLE)
C
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     ID          ID WORDS                                  ARGUMENT
C
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     JTITLE      TITLE ARRAY                               ARGUMENT
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C
C
C     . . . . . . . . . . . . .
      INTEGER    ID(12)
      INTEGER    JTITLE(41)
C     . . . . . . . . . . . . .
      INTEGER    DTIME_CODE
      INTEGER    JKEEP(15)

      INTEGER        KTITLE(41)
      CHARACTER*328  CTITLE
      EQUIVALENCE   (KTITLE(1),CTITLE)

      integer   MASK(8)
C
      DATA MASK(1)/ X'0000000F' /
      DATA MASK(2)/ X'000000FF' /
      DATA MASK(3)/ X'00000FFF' /
      DATA MASK(4)/ X'0000FFFF' /
      DATA MASK(5)/ X'000FFFFF' /
      DATA MASK(6)/ X'00FFFFFF' /
      DATA MASK(7)/ X'0FFFFFFF' /
      DATA MASK(8)/ X'FFFFFFFF' /

 
      INTEGER        TEMPR
      CHARACTER*1    ISTORE(8)
      EQUIVALENCE   (TEMPR,ISTORE(1))

      INTEGER    C1,C2,E1,E2,S1,S2,Q,M,G
C
      CHARACTER*8  INUM1
      CHARACTER*8  INUM2
      CHARACTER*8  KWRITE(3)
      character*6  QWRITE
C
      CHARACTER*1    LTEMP(7)
      CHARACTER*1    MTEMP(7)

      INTEGER        NSHIFT
      INTEGER        NMASK
      INTEGER        NID
C
C
C     IDWORDS:  MASK CONTROL
C
C     . . . . . . . . . . . . . . . . . . .
      INTEGER    SHFMSK(15)

      DATA SHFMSK( 1)/X'00020100'/
      DATA SHFMSK( 2)/X'08020700'/
      DATA SHFMSK( 3)/X'10020700'/
      DATA SHFMSK( 4)/X'18020700'/
      DATA SHFMSK( 5)/X'08050200'/
      DATA SHFMSK( 6)/X'00020200'/
      DATA SHFMSK( 7)/X'08050400'/
      DATA SHFMSK( 8)/X'00020400'/
      DATA SHFMSK( 9)/X'1C010300'/
      DATA SHFMSK(10)/X'08030100'/
      DATA SHFMSK(11)/X'08030300'/
      DATA SHFMSK(12)/X'14030100'/
      DATA SHFMSK(13)/X'00020700'/
      DATA SHFMSK(14)/X'10020800'/
C
C     . . . . . . . . . . . . . . . . . . .
C     MONTH TABLE.
C
      CHARACTER*4    CMONTH(12)
      DATA CMONTH( 1)/ ' JAN'/
      DATA CMONTH( 2)/ ' FEB'/
      DATA CMONTH( 3)/ ' MAR'/
      DATA CMONTH( 4)/ ' APR'/
      DATA CMONTH( 5)/ ' MAY'/
      DATA CMONTH( 6)/ ' JUN'/
      DATA CMONTH( 7)/ ' JUL'/
      DATA CMONTH( 8)/ ' AUG'/
      DATA CMONTH( 9)/ ' SEP'/
      DATA CMONTH(10)/ ' OCT'/
      DATA CMONTH(11)/ ' NOV'/
      DATA CMONTH(12)/ ' DEC'/
C
C     . . . . . . . . . . . . . . . . . . .
C     . . . . . . . . . . . . . . . . . . .
C     REFERENCE TABLE FOR SNAME.
C
      INTEGER    S_CODE(17)
      DATA S_CODE(1)/1/
      DATA S_CODE(2)/2/
      DATA S_CODE(3)/6/
      DATA S_CODE(4)/7/
      DATA S_CODE(5)/8/
      DATA S_CODE(6)/16/
      DATA S_CODE(7)/19/
      DATA S_CODE(8)/128/
      DATA S_CODE(9)/129/
      DATA S_CODE(10)/130/
      DATA S_CODE(11)/144/
      DATA S_CODE(12)/145/
      DATA S_CODE(13)/146/
      DATA S_CODE(14)/147/
      DATA S_CODE(15)/148/
      DATA S_CODE(16)/131/
      DATA S_CODE(17)/132/
C
C     . . . . . . . . . . . . . . . . . . .
C     SNAME TABLE.
      character*4  SNAME(18)
C
      DATA SNAME( 1)/ ' GPM'/
      DATA SNAME( 2)/ ' PA '/
      DATA SNAME( 3)/ ' M  '/
      DATA SNAME( 4)/ ' M  '/
      DATA SNAME( 5)/ ' MB '/
      DATA SNAME( 6)/ ' DEG'/
      DATA SNAME( 7)/ ' POT'/
      DATA SNAME( 8)/ ' MSL'/
      DATA SNAME( 9)/ ' SFC'/
      DATA SNAME(10)/ ' TRO'/
      DATA SNAME(11)/ ' BDY'/
      DATA SNAME(12)/ ' TRS'/
      DATA SNAME(13)/ ' STS'/
      DATA SNAME(14)/ ' QCP'/
      DATA SNAME(15)/ ' SIG'/
      DATA SNAME(16)/ 'MWSL'/
      DATA SNAME(17)/ 'PLYR'/
      DATA SNAME(18)/ '    '/
C
C     . . . . . . . . . . . . . . . . . . .
C     . . . . . . . . . . . . . . . . . . .
C     REFERENCE TABLE FOR QNAME.
      INTEGER    Q_CODE(108)
C
      DATA Q_CODE( 1)/  1/
      DATA Q_CODE( 2)/  2/
      DATA Q_CODE( 3)/  6/
      DATA Q_CODE( 4)/  8/
      DATA Q_CODE( 5)/ 16/
      DATA Q_CODE( 6)/ 17/
      DATA Q_CODE( 7)/ 18/
      DATA Q_CODE( 8)/ 19/
      DATA Q_CODE( 9)/ 20/
      DATA Q_CODE(10)/ 21/
      DATA Q_CODE(11)/ 40/
      DATA Q_CODE(12)/ 41/
      DATA Q_CODE(13)/ 42/
      DATA Q_CODE(14)/ 43/
      DATA Q_CODE(15)/ 44/
      DATA Q_CODE(16)/ 48/
      DATA Q_CODE(17)/ 49/
      DATA Q_CODE(18)/ 50/
      DATA Q_CODE(19)/ 51/
      DATA Q_CODE(20)/ 52/
      DATA Q_CODE(21)/ 53/
      DATA Q_CODE(22)/ 54/
      DATA Q_CODE(23)/ 55/
      DATA Q_CODE(24)/ 56/
      DATA Q_CODE(25)/ 57/
      DATA Q_CODE(26)/ 58/
      DATA Q_CODE(27)/ 59/
      DATA Q_CODE(28)/ 60/
      DATA Q_CODE(29)/ 72/
      DATA Q_CODE(30)/ 73/
      DATA Q_CODE(31)/ 74/
      DATA Q_CODE(32)/ 80/
      DATA Q_CODE(33)/ 81/
      DATA Q_CODE(34)/ 88/
      DATA Q_CODE(35)/ 89/
      DATA Q_CODE(36)/ 90/
      DATA Q_CODE(37)/ 91/
      DATA Q_CODE(38)/ 92/
      DATA Q_CODE(39)/ 93/
      DATA Q_CODE(40)/ 94/
      DATA Q_CODE(41)/ 95/
      DATA Q_CODE(42)/ 96/
      DATA Q_CODE(43)/112/
      DATA Q_CODE(44)/113/
      DATA Q_CODE(45)/114/
      DATA Q_CODE(46)/115/
      DATA Q_CODE(47)/120/
      DATA Q_CODE(48)/121/
      DATA Q_CODE(49)/160/
      DATA Q_CODE(50)/161/
      DATA Q_CODE(51)/162/
      DATA Q_CODE(52)/163/
      DATA Q_CODE(53)/164/
      DATA Q_CODE(54)/165/
      DATA Q_CODE(55)/166/
      DATA Q_CODE(56)/167/
      DATA Q_CODE(57)/168/
      DATA Q_CODE(58)/169/
      DATA Q_CODE(59)/170/
      DATA Q_CODE(60)/171/
      DATA Q_CODE(61)/176/
      DATA Q_CODE(62)/177/
      DATA Q_CODE(63)/178/
      DATA Q_CODE(64)/184/
      DATA Q_CODE(65)/185/
      DATA Q_CODE(66)/186/
      DATA Q_CODE(67)/187/
      DATA Q_CODE(68)/188/
      DATA Q_CODE(69)/384/
      DATA Q_CODE(70)/385/
      DATA Q_CODE(71)/386/
      DATA Q_CODE(72)/387/
      DATA Q_CODE(73)/388/
      DATA Q_CODE(74)/389/
      DATA Q_CODE(75)/390/
      DATA Q_CODE(76)/391/
      DATA Q_CODE(77)/ 97/
      DATA Q_CODE(78)/ 98/
      DATA Q_CODE(79)/ 99/
      DATA Q_CODE(80)/100/
      DATA Q_CODE(81)/101/
      DATA Q_CODE(82)/102/
      DATA Q_CODE(83)/103/
      DATA Q_CODE(84)/148/
      DATA Q_CODE(85)/172/
      DATA Q_CODE(86)/200/
      DATA Q_CODE(87)/201/
      DATA Q_CODE(88)/202/
      DATA Q_CODE(89)/203/
      DATA Q_CODE(90)/392/
      DATA Q_CODE(91)/  7/
      DATA Q_CODE(92)/ 61/
      DATA Q_CODE(93)/104/
      DATA Q_CODE(94)/173/
      DATA Q_CODE(95)/174/
      DATA Q_CODE(96)/175/
      DATA Q_CODE(97)/304/
      DATA Q_CODE(98)/305/
      DATA Q_CODE(99)/400/
      DATA Q_CODE(100)/401/
      DATA Q_CODE(101)/402/
      DATA Q_CODE(102)/403/
      DATA Q_CODE(103)/404/
      DATA Q_CODE(104)/405/
      DATA Q_CODE(105)/  9/
      DATA Q_CODE(106)/105/
      DATA Q_CODE(107)/116/
      DATA Q_CODE(108)/106/
C
C     . . . . . . . . . . . . . . . . . . .
C     QNAME TABLE:
      character*6  QNAME(108)
C
      DATA QNAME( 1)/ ' HGT  '/
      DATA QNAME( 2)/ ' P ALT'/
      DATA QNAME( 3)/ ' DIST '/
      DATA QNAME( 4)/ ' PRES '/
      DATA QNAME( 5)/ ' TMP  '/
      DATA QNAME( 6)/ ' DPT  '/
      DATA QNAME( 7)/ ' DEPR '/
      DATA QNAME( 8)/ ' POT  '/
      DATA QNAME( 9)/ ' T MAX'/
      DATA QNAME(10)/ ' T MIN'/
      DATA QNAME(11)/ ' V VEL'/
      DATA QNAME(12)/ ' NETVD'/
      DATA QNAME(13)/ ' DZDT '/
      DATA QNAME(14)/ ' OROW '/
      DATA QNAME(15)/ ' FRCVV'/
      DATA QNAME(16)/ ' U GRD'/
      DATA QNAME(17)/ ' V GRD'/
      DATA QNAME(18)/ ' WIND '/
      DATA QNAME(19)/ ' T WND'/
      DATA QNAME(20)/ ' VW SH'/
      DATA QNAME(21)/ ' U DIV'/
      DATA QNAME(22)/ ' V DIV'/
      DATA QNAME(23)/ ' WDIR '/
      DATA QNAME(24)/ ' WWND '/
      DATA QNAME(25)/ ' SWND '/
      DATA QNAME(26)/ ' RATS '/
      DATA QNAME(27)/ ' VECW '/
      DATA QNAME(28)/ ' SFAC '/
      DATA QNAME(29)/ ' ABS V'/
      DATA QNAME(30)/ ' REL V'/
      DATA QNAME(31)/ ' DIV  '/
      DATA QNAME(32)/ ' STRM '/
      DATA QNAME(33)/ ' V POT'/
      DATA QNAME(34)/ ' R H  '/
      DATA QNAME(35)/ ' P WAT'/
      DATA QNAME(36)/ ' A PCP'/
      DATA QNAME(37)/ ' P O P'/
      DATA QNAME(38)/ ' P O Z'/
      DATA QNAME(39)/ ' SNO D'/
      DATA QNAME(40)/ ' ACPCP'/
      DATA QNAME(41)/ ' SPF H'/
      DATA QNAME(42)/ ' L H2O'/
      DATA QNAME(43)/ ' LFT X'/
      DATA QNAME(44)/ ' TOTOS'/
      DATA QNAME(45)/ ' K X  '/
      DATA QNAME(46)/ ' C INS'/
      DATA QNAME(47)/ ' L WAV'/
      DATA QNAME(48)/ ' S WAV'/
      DATA QNAME(49)/ ' DRAG '/
      DATA QNAME(50)/ ' LAND '/
      DATA QNAME(51)/ ' KFACT'/
      DATA QNAME(52)/ ' 10TSL'/
      DATA QNAME(53)/ ' 7TSL '/
      DATA QNAME(54)/ ' RCPOP'/
      DATA QNAME(55)/ ' RCMT '/
      DATA QNAME(56)/ ' RCMP '/
      DATA QNAME(57)/ ' ORTHP'/
      DATA QNAME(58)/ ' ALBDO'/
      DATA QNAME(59)/ ' ENFLX'/
      DATA QNAME(60)/ ' HEATR'/
      DATA QNAME(61)/ ' LAT  '/
      DATA QNAME(62)/ ' LON  '/
      DATA QNAME(63)/ ' RADIC'/
      DATA QNAME(64)/ ' PROB '/
      DATA QNAME(65)/ ' CPROB'/
      DATA QNAME(66)/ ' USTAR'/
      DATA QNAME(67)/ ' TSTAR'/
      DATA QNAME(68)/ ' MIXHT'/
      DATA QNAME(69)/ ' WTMP '/
      DATA QNAME(70)/ ' WVHGT'/
      DATA QNAME(71)/ ' SWELL'/
      DATA QNAME(72)/ ' WVSWL'/
      DATA QNAME(73)/ ' WVPER'/
      DATA QNAME(74)/ ' WVDIR'/
      DATA QNAME(75)/ ' SWPER'/
      DATA QNAME(76)/ ' SWDIR'/
      DATA QNAME(77)/ ' RRATE'/
      DATA QNAME(78)/ ' TSTM '/
      DATA QNAME(79)/ ' CSVR '/
      DATA QNAME(80)/ ' CTDR '/
      DATA QNAME(81)/ ' MIXR '/
      DATA QNAME(82)/ ' PSVR '/
      DATA QNAME(83)/ ' MCONV'/
      DATA QNAME(84)/ ' SIG  '/
      DATA QNAME(85)/ ' ENRGY'/
      DATA QNAME(86)/ ' RDNCE'/
      DATA QNAME(87)/ ' BRTMP'/
      DATA QNAME(88)/ ' TCOZ '/
      DATA QNAME(89)/ ' OZMR '/
      DATA QNAME(90)/ ' ICSEA'/
      DATA QNAME(91)/ ' DEPTH'/
      DATA QNAME(92)/ ' GUST '/
      DATA QNAME(93)/ ' VAPP '/
      DATA QNAME(94)/ ' TOTHF'/
      DATA QNAME(95)/ ' SEHF '/
      DATA QNAME(96)/ ' SORAD'/
      DATA QNAME(97)/ ' UOGRD'/
      DATA QNAME(98)/ ' VOGRD'/
      DATA QNAME(99)/ ' HTSGW'/
      DATA QNAME(100)/ ' PERPW'/
      DATA QNAME(101)/ ' DIRPW'/
      DATA QNAME(102)/ ' PERSW'/
      DATA QNAME(103)/ ' DIRSW'/
      DATA QNAME(104)/ ' WCAPS'/
      DATA QNAME(105)/ ' PTEND'/
      DATA QNAME(106)/ ' NCPCP'/
      DATA QNAME(107)/ ' HIFTX'/
      DATA QNAME(108)/ ' ICEAC'/
C
C     . . . . . . . . . . . . . . . . . . .
C     . . . . . . . . . . . . . . . . . . .
C  REFERENCE TABLE FOR G (GENERATING PROGRAM NAME)
C
      INTEGER    KK(3)

      DATA       KK(1)/57/
      DATA       KK(2)/58/
      DATA       KK(3)/59/
C
C     . . . . . . . . . . . . . . . . . . .
C  G TABLE (GENERATING PROGRAM NAME):
C
      CHARACTER*8  KNAME(9)

      DATA         KNAME    / '   ECMWF', 
     2                        ' READING',
     3                        ', UK.   ',
     4                        '    FNOC',
     5                        ' MONTERE',
     6                        'Y, CA.  ',
     7                        '  AFGWC ', 
     8                        'OFFUTT A',
     9                        'FB, NB. ' /
C
C     . . . . . . . . . . . . . . . . . . .
C     . . . . . . . . . . . . . . . . . . .
      CHARACTER*8  KNAME1(3)
      DATA         KNAME1   / '   WMC N',
     2                        'CEP WASH',
     3                        'INGTON  ' /
C
C     . . . . . . . . . . . . . . . . . . .
      character*6  QNAME1
      DATA         QNAME1   / ' THCK ' /
C
      logical   lcheckout
C
      SAVE
C     . . . .    S T A R T   . . . . . . . . . . . . . . . . . . . 
C
      lcheckout = .FALSE.

C        1.   UNPACK ID WORDS.
C
      DO 1 N=1,14
        TEMPR = SHFMSK(N)
        NSHIFT = 0
        NSHIFT = MOVA2I(ISTORE(5))  		!... CRAY
C ...        JSTORE(4) = ISTORE(1)
        NMASK = 0
        NMASK = MOVA2I(ISTORE(6))   		!... CRAY
C ...        KSTORE(4) = ISTORE(2)
        NID = 0
        NID = MOVA2I(ISTORE(7))  		!... CRAY
C ...        LSTORE(4) = ISTORE(3)
        JKEEP(N)=IAND(MASK(NMASK),ISHFT(ID(NID),-NSHIFT))
    1 CONTINUE
C
      C1 = JKEEP(5)
      C2 = JKEEP(7)
      E1 = JKEEP(6)
      E2 = JKEEP(8)
      S1 = JKEEP(10)
      S2 = JKEEP(11)
      Q  = JKEEP(12)
      M  = JKEEP(9)
      G  = JKEEP(14)
C
      IF(E1 .GT. 128) THEN
         E1 = -(JKEEP(6)-128)
      ENDIF
      IF(E2 .GT. 128) THEN
         E2 = -(JKEEP(8)-128)
      ENDIF
C     
      if(lcheckout)then
        WRITE(6,505)(JKEEP(I),I=1,14),E1,E2
  505   FORMAT(1H ,'LINE-1: THE JKEEP ARRAY FOLLOWS ...',
     1      /1H ,'     (1)      (2)      (3)      (4)      (C1)',
     2           '     (E1)     (C2)     (E2)',
     3      /1H ,8Z9.8, /1H ,8Z9.8,
     4      /1H ,'     (M)      (S1)     (S2)     (Q)      (13)',
     5           '     (G)     (E1B)    (E2B)')
      endif
C
C        2.   FIND WHICH SURFACE IS INDICATED BY THE ID WORDS
C                  AS BEING THE FIRST SURFACE.
C
      DO 2 I=1,17
        K1 = I
        IF(S1 .EQ. S_CODE(I))GO TO 4
 2    CONTINUE
      K1=18
      GO TO 4

 4    CONTINUE
C
C        3.   BEGIN PROCESSING OF A ONE-SURFACE TITLE
C
      IF(M .NE. 0)GO TO 200
C
      K2=K1

      CALL FP02_VAL(S1,C1,E1,INUM1)

      WRITE(CTITLE(1:20),107) INUM1(1:7)
 107  FORMAT(13X,A)

      GO TO 73
C
C        4.   FIND WHICH SURFACE IS INDICATED BY THE ID WORDS
C                  AS BEING THE SECOND SURFACE.
C
 200  DO 22 I=1,17
        IF(S2 .EQ. S_CODE(I))GO TO 33
 22   CONTINUE
      K2=18
      GO TO 44
 33   K2=I
 44   CONTINUE
C
C
C        5.   BEGIN PROCESSING OF A TWO-SURFACE TITLE
C
      CALL FP02_VAL(S1,C1,E1,INUM1)
      CALL FP02_VAL(S2,C2,E2,INUM2)
      WRITE(CTITLE(1:20),103) INUM1(1:7),SNAME(K1)(1:4),INUM2(1:7)
 103  FORMAT(' ',A,A,' ',A)
C
C        6.   FIND WHICH PARAMETER (Q) IS INDICATED BY THE ID WORDS.
C
 73   CONTINUE
      DO 300 N=1,108
        IF(Q .EQ. Q_CODE(N))GO TO 310
 300  CONTINUE
C     ... fell thru without finding a matching Q_CODE ...
      WRITE(6,108) Q
 108  FORMAT(' Q IS AN ILLEGAL NUMBER. Q IS ',I3)
      GO TO 1000

 310  QWRITE(1:6) = QNAME(N)(1:6)
C
      IF(Q .EQ. 1  .AND.  M .EQ. 1 .AND.  S1 .EQ. 8) then
        QWRITE(1:6) = QNAME1(1:6)
      endif
C
C        7.   SET DATE/TIME FIELDS.
C
C          A.   SET DTIME_CODE FOR HRS,HALF DAYS OR DAYS
C                   FROM F1 AND F2

      CALL DT_UNITS(ID, DTIME_CODE)
C
      IF (DTIME_CODE  .EQ.  0) then
        GO TO 76
      ELSE IF (DTIME_CODE  .EQ.  1) then
        JKEEP(1) = JKEEP(1) * 12
        GO TO 76
      ELSE IF (DTIME_CODE  .EQ.  2) then
        JKEEP(1) = JKEEP(1) * 24
        go to 76
      ELSE
        GO TO 76
      endif
C
 76   continue
      IHUN   = JKEEP(1)/100
      irmndr = mod(jkeep(1),100)
      iten   = irmndr/10
      iunit  = mod(irmndr,10)

      IFST  = JKEEP(13)/10
      isec  = mod(jkeep(13),10)
C
C        8.   SET GENERATING PROGRAM NAME.
C
      DO 80 K=1,3
        IF (G .EQ. KK(K)) THEN
C         ...  this is one of the other met centers ...
          DO  L=1,3
            KWRITE(L) = KNAME( 3*(K-1) + L)    
          ENDDO
          GO TO 100
        endif
  80  CONTINUE
C
      DO 99 L=1,3
        KWRITE(L) = KNAME1(L)   	!... wmc nmc washington
  99  CONTINUE
      GO TO 100
C
C        9.   ASSEMBLE THE REMAINDER OF THE TITLE LINE
C                   VIA INTERNAL WRITE STATEMENT INTO CTITLE(21:88)
C
 100  CONTINUE
      WRITE(CTITLE(21:88),105) SNAME(K2)(1:4),QWRITE(1:6),
     A                         IHUN,ITEN,IUNIT, IFST,ISEC,
     B          JKEEP(2),CMONTH(JKEEP(3)),JKEEP(4),(KWRITE(L),L=1,3)
 105  FORMAT(A4,1X,A6,' FOR ',3I1,' HRS AFTER ',2I1,'Z ',I2,A4,1X,I2,
     1       3A8)
C
      if(lcheckout) then
      write(6,535)ctitle(1:63),ctitle(64:88)
  535 format(1h ,'at completion of LINE-1, CTITLE(1:88) contains ...',
     1      /1h ,'"',A,'"', /1h ,'"',A,'"')
      endif

C
 1000 CONTINUE
C
C         10.  REASSIGN KTITLE FOR OUTPUT.
C
      DO 1001 K = 1,11  		!... 11 I*8 == 22 I*4
        JTITLE(K) = KTITLE(K)
 1001 CONTINUE
C
      RETURN
      END

      SUBROUTINE FP02_VAL(S,C,E,C8NUM)
C                                                     15-MAY-1996/DSS
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: FP02_VAL          CREATES VALUE OF SURFACE FROM IDS
C   AUTHOR: COBBS,S.        ORG: W/NMC42    DATE: 83-10-12
C
C ABSTRACT: CONVERTS THE NUMERICAL VALUE FOR THE SURFACE INTO 
C   A TEXT STRING TO BE USED IN THE FIRST LINE OF THE TITLE.
C
C USAGE:  CALL FP02_VAL(S,C,E,C8NUM)
C
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C
C     S           INTEGER NUMBER OF SURFACE                 ARGUMENT
C     C,E         NUMERICAL VALUE OF THE SURFACE            ARGUMENT
C                    SURFACE = S * 10 ** E
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C
C     C8NUM       VALUE OF THE SURFACE FOR THE TITLE        ARGUMENT
C                    CHARACTER*8  C8NUM
C
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
C
C ... 960515 SHIMOMURA -- IN MODIFYING FOR CRAY, I CHANGED THE ARG4
C                         FROM REAL*8 TO CHARACTER*8, SINCE IT WAS
C                         FOR AN 8-CHAR STRING;
C                         ALSO RENAMED FROM "VALUE" TO "FP02_VAL"
C     . . . . . . . . . . . . . . . . . . . . . . . . .
C ... USAGE:  CALL FP02_VAL(S,C,E,C8NUM)
      integer        S
      integer        C
      integer        E
      CHARACTER*8    C8NUM    		!... RESULT

C     . . . . . . . . . . . . . . . . . . . . . . . . .
      CHARACTER*8     JZEROS
      DATA            JZEROS/ ' 0.0000 ' /

      CHARACTER*8     KLANKS
      DATA            KLANKS/ '        ' /
C
C
      CHARACTER*7  LTEMP
      CHARACTER*1  POINT
      DATA POINT/'.'/
C
      integer*8     INUM
      CHARACTER*8   CNUM
      EQUIVALENCE  (INUM,CNUM)
C
C
C
      C8NUM = KLANKS

      IF (S .GE. 128 .AND. S .LE. 132) THEN
         C8NUM = KLANKS
         GO TO 150
      ENDIF

C     ... OTHERWISE,    	!... (S .LT.128) .OR. (S .GT. 132) ...
      IF(C .EQ. 0) THEN
         C8NUM = JZEROS
         GO TO 150
      ELSE
C        ...   C IS NON-ZERO ...
         WRITE (LTEMP(1:7),101) C
 101     FORMAT(I6,1H )
         J = E+6
         K = J+1
         CNUM(1:J)   = LTEMP(1:J)
         CNUM(K:K)   = POINT
         CNUM(K+1:8) = LTEMP(K:7)
         C8NUM(1:8) = CNUM(1:8)
         GO TO 150
      ENDIF
C
 150  CONTINUE
      RETURN
      END

      SUBROUTINE FP02_LN2(ID,JTITLE)
C                                                     15-MAY-1996/DSS
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: FP02_LN2          CREATES THE SECOND LINE OF TITLE
C   AUTHOR:                  ORG: W/NMC42    DATE: 83-10-25
C
C ABSTRACT: CREATES THE SECOND LINE OF THE TITLE FROM THE
C   ID WORDS.
C
C USAGE:  CALL FP02_LN2(ID,JTITLE)
C
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     ID          ID WORDS                                  ARGUMENT
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     JTITLE      TITLE ARRAY                               ARGUMENT
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN
C   SOURCE STATEMENTS: 50    CORE SIZE:  2300
C
C$$$
C
C
ckumar       integer (kind=8) ibm2cray
ckumar       external  ibm2cray

C
      INTEGER   ID(12)
      INTEGER   JTITLE(41)

      INTEGER        KTITLE(41)
      CHARACTER*328  CTITLE
      EQUIVALENCE   (KTITLE(1),CTITLE)
C
      integer   MASK(8)
C
      DATA MASK(1)/ X'0000000F' /
      DATA MASK(2)/ X'000000FF' /
      DATA MASK(3)/ X'00000FFF' /
      DATA MASK(4)/ X'0000FFFF' /
      DATA MASK(5)/ X'000FFFFF' /
      DATA MASK(6)/ X'00FFFFFF' /
      DATA MASK(7)/ X'0FFFFFFF' /
      DATA MASK(8)/ X'FFFFFFFF' /

C
      INTEGER        TEMPR
      CHARACTER*1    ISTORE(8)
      EQUIVALENCE   (TEMPR,ISTORE(1))
C
      DATA  NEG   / X'FFFFFFFFFFFF0000' /
C
C     IDWORDS:  MASK CONTROL (INTEGER  )
C
      INTEGER        IKEEP(17)
      INTEGER        SHFMSK(17)
      DATA SHFMSK( 1)/X'1C010300'/
      DATA SHFMSK( 2)/X'1C010200'/
      DATA SHFMSK( 3)/X'1C010400'/
      DATA SHFMSK( 4)/X'00020100'/
      DATA SHFMSK( 5)/X'00020300'/
      DATA SHFMSK( 6)/X'18020500'/
      DATA SHFMSK( 7)/X'10020500'/
      DATA SHFMSK( 8)/X'08020500'/
      DATA SHFMSK( 9)/X'00020500'/
      DATA SHFMSK(10)/X'18080600'/
      DATA SHFMSK(11)/X'18020800'/
      DATA SHFMSK(12)/X'10020800'/
      DATA SHFMSK(13)/X'00040800'/
      DATA SHFMSK(14)/X'10040900'/
      DATA SHFMSK(15)/X'00040900'/
      DATA SHFMSK(16)/X'00080A00'/
      DATA SHFMSK(17)/X'00040B00'/

      real      crayreal
      integer   loneibm
      integer   nbitoffset
      data      nbitoffset   / 32 /
      integer   nitems
      data      nitems       /  1 /
      logical   lcheckout
C
      save

      lcheckout = .FALSE.

C    UNPACK ID WORDS.
C
      DO 1 N=1,17
        TEMPR=SHFMSK(N)
C
        NSHIFT = 0
        nshift = mova2i(istore(5))
C
        NMASK = 0
        nmask = mova2i(istore(6))
C
        NID = 0
        nid = mova2i(istore(7))
C
        IKEEP(N)=IAND(MASK(NMASK),ISHFT(ID(NID),-NSHIFT))
    1 CONTINUE
C
C    CONVERT 16 BIT SIGNED INTEGER INTO FULLWORD INTEGER
C
      IF (BTEST(IKEEP(17),15)) THEN
        IKEEP(17) = IOR(IKEEP(17),NEG)
      ENDIF
C
C     ... the A= reference value in ikeep(16) is, I think,
C     ...   in IBM REAL*4 format right-justified in longword; 
C     ...   so convert it into CRAY REAL 
       crayreal = 0.0
       loneibm = IKEEP(16)

ckumar       iret = ibm2cray(2,nitems,loneibm,nbitoffset,crayreal)
C                 type=2; for IBM REAL*4

ckumar       if(iret .LT. 0) then
ckumar         write(6,315)iret
ckumar  315    format(1h ,'FP02_LN2::ibm2cray FAILED with return code=',I8)
C        ... TO LET IT GO ON ANYWAY,
         crayreal = 0.0
ckumar       endif
ckumar       if (lcheckout) then
ckumar         write(6,325)loneibm, crayreal
ckumar  325    format(1h ,'FP02_LN2::ibm2cray: R*4  ibm = HEX',Z17.16,
ckumar     1         /1h ,'                    R*8 cray = DEC',E13.5)
ckumar       endif

C    ASSEMBLE THE SECOND LINE OF THE TITLE
C
      
      WRITE(CTITLE(89:216),101)(IKEEP(I),I=1,15),crayreal,ikeep(17)
 101  FORMAT(' M=',I1,' T=',I1,' N=',I2,' F1=',I3,' F2=',I3,' CD=',I3,'
     1CM=',I3,' KS=',I3,' K=',I3,' UN=',I5,' R= ',I3,' G=',I3,' J=',I5,'
     2 B=',I5,' Z=',I5,' A=',E15.8,' N=',I5,'  ')
C
C    REASSIGN THE SECOND LINE OF THE TITLE TO THE OUTPUT ARRAY
C
C ...      DO 1001 K = 23,54
      DO 1001 K = 12,27
        JTITLE(K) = KTITLE(K)
 1001 CONTINUE
C
      RETURN
      END
      SUBROUTINE FP02_LN3(ID,JTITLE)
C                                                     15-MAY-1996/DSS
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: FP02_LN3          CREATES THE THIRD LINE OF TITLE
C   AUTHOR:                  ORG: W/NMC42    DATE: 25 OCT 83
C
C ABSTRACT: CREATES THE THIRD LINE OF THE TITLE FROM THE
C   ID WORDS.
C
C USAGE:  CALL FP02_LN3 (ID,JTITLE)
C
C
C - - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     ID          ID WORDS                                  ARGUMENT
C
C - - - - - - - - - O U T P U T   V A R I A B L E S - - - - - - - - - -
C
C     NAMES       MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C     -----       ----------------------------------        ---------
C     JTITLE      TITLE ARRAY                               ARGUMENT
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C
      DIMENSION      ID(12)
      DIMENSION      JTITLE(41)

      DIMENSION      KTITLE(41)
      CHARACTER*328  CTITLE
      EQUIVALENCE   (KTITLE(1),CTITLE)
C
C    ... ASSEMBLE THE THIRD LINE OF THE TITLE.
C
      WRITE(CTITLE(217:324),1)(ID(I),I=1,12)
    1 FORMAT(12Z9.8)
C
C    ... REASSIGN THE THIRD LINE OF THE TITLE TO AN OUTPUT ARRAY.
C
C ...      DO 1001 K = 55,81
      DO 1001 K = 28,41
        JTITLE(K) = KTITLE(K)
 1001 CONTINUE
C
      RETURN
      END
      SUBROUTINE DT_UNITS(ID, DTIME_CODE)
C                                                     15-MAY-1996/DSS
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    DT_UNITS    SET DELTA TIME CODE FROM ID 
C   PRGMMR: SHIMOMURA        ORG: W/NP12      DATE: 96-05-17
C
C ABSTRACT: DETERMINES DELTA-TIME UNITS OF THE FORECAST 
C    WHETHER IN HOURS, HALF-DAYS, OR FULL-DAYS;
C    ONLY CALLED FROM W3FP02().
C
C PROGRAM HISTORY LOG:
C   85-01-25  ORIGINAL AUTHOR: D. MILLER
C   96-05-17  SHIMOMURA -- CHANGED NAME FROM "TCHECK" TO "DT_UNITS"
C                           AND REPROGRAMMED FOR CRAY
C 
C USAGE:  CALL DT_UNITS (ID, DTIME_CODE)
C   INPUT ARGUMENT LIST:
C      ID   - INTEGER*8 ID(12): OFFICE NOTE 84 IDENTIFIER
C
C   OUTPUT ARGUMENT LIST:
C      DTIME_CODE - INTEGER*8  DTIME_CODE
C                 =0;  IN HOURS
C                 =1;  IN HALF-DAY UNITS
C                 =2;  IN FULL-DAY UNITS
C
C   OUTPUT FILES:
C     FT06F001 - FOR COMMENT IF ERRONEOUS DATA
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN 77
C   MACHINE:   CRAY
C
C$$$
C
       INTEGER    ID(12)
       INTEGER    DTIME_CODE

       integer   MASK(8)
C
       DATA MASK(1)/ X'0000000F' /
       DATA MASK(2)/ X'000000FF' /
       DATA MASK(3)/ X'00000FFF' /
       DATA MASK(4)/ X'0000FFFF' /
       DATA MASK(5)/ X'000FFFFF' /
       DATA MASK(6)/ X'00FFFFFF' /
       DATA MASK(7)/ X'0FFFFFFF' /
       DATA MASK(8)/ X'FFFFFFFF' /
C
       INTEGER    ISTORE(3,2)
       DATA  ISTORE /  28,     1,    2,
     2                 28,     1,    4 /
C            ...     NSHIFT, NMASK, NID   ...

       INTEGER    TKEEP(2)
C
C    UNPACK "T" AND "N".
C
       DO 1 N=1,2
          NSHIFT = ISTORE(1,N)
          NMASK  = ISTORE(2,N)
          NID    = ISTORE(3,N)
          TKEEP(N)=IAND(MASK(NMASK),ISHFT(ID(NID),-NSHIFT))
  1    CONTINUE
C
       IF ((TKEEP(1) .EQ. 6) .OR. (TKEEP(1) .EQ. 7)) THEN
         DTIME_CODE = 1
         GO TO 120

       ELSE IF (TKEEP(1) .EQ. 10) THEN
         DTIME_CODE = 2
         IF (TKEEP(2)  .EQ.  15) THEN
           WRITE(6,111)
111        FORMAT(1H ,'DT_UNITS: "T" AND "N" INDICATE CONFLICTING ',
     A           /1H ,'   TIME INTERVAL VALUES(CODE OPTS FOR "T").')
 
           GO TO 120
         ENDIF
         GO TO 120

       ELSE IF (TKEEP(2) .EQ. 15) THEN
         DTIME_CODE = 1
         GO TO 120

       ELSE
         DTIME_CODE = 0
         GO TO 120
       ENDIF
C
120    RETURN
       END
