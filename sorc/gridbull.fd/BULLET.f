      SUBROUTINE BULLET (CNAME,NCAT,NDATA,KPDS,NEXT,NBLK)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    BULLET       FORMATS FM 47-V GRID CODE BULLETIN
C   PRGMMR: BOSTELMAN        ORG: W/NMC42    DATE: 92-03-09
C
C ABSTRACT: FORMAT A BULLETIN IN FM 47-V GRID CODE FROM THE MANUAL ON
C   CODES, VOLUME I (WMO 306), FOLLOWING OFFICE NOTE 167 GUIDELINES,
C   AND BLOCK THE BULLETIN UNDER OFFICE NOTE 100 GUIDELINES.
C
C PROGRAM HISTORY LOG:
C   81-12-20  R. ALLARD ORGINATOR
C   92-03-09  W. BOSTELMAN - UPGRADED DOCBLOCK
C   98-06-29  Gilbert      - Made Y2K compliant.  Check for year = 100.
C
C
C USAGE:    CALL BULLET (NAME,NCAT,NDATA,LABEL,NEXT,NBLK)
C   INPUT ARGUMENT LIST:
C         CNAME - ADDRESS OF BULLETIN NAME IN GLOBAL FORMAT (TTAAII)
C         NCAT  - BULLETIN CATALOG NUMBER (INTEGER*4)
C         NDATA - ADDRESS OF 285 PT INPUT DATA ARRAY (INTEGER*4)
C         KPDS  - GRIB PDS Array in w3fi63 layout
C
C   OUTPUT ARGUMENT LIST:
C         NBLK  - ADDRESS OF OUTPUT DATA ARRAY THAT CAN HOLD TWO 1280
C                      CHARACTER RECORDS.
C         NEXT  - INTEGER*4 VARIABLE FOR USE AS A W3AI19 PARAMETER.
C
C   INPUT FILES:
C     NONE
C
C   OUTPUT FILES:
C     NONE
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
      CHARACTER*4 KON
      CHARACTER*4 K1,K2,K7,K8,K9,K10,K15,K20,K25,MINUS,NPOS
      CHARACTER*4 NTYPE(6),NA4,NS
      CHARACTER*1 CIS
      CHARACTER*2 CMM
      CHARACTER*2 CN1N2
      CHARACTER*2 LTYPH
      CHARACTER*2 NAME2
      CHARACTER*2 NAME3
      CHARACTER*2 NNNQH
      CHARACTER*1  LTYPE(6)
      CHARACTER*1  NAME(6)
      CHARACTER*1  NNNQ(12)
      CHARACTER*3  NEOL
      CHARACTER*3  CNNN
      CHARACTER*3  CFCST
      CHARACTER*4  CJJMM
      CHARACTER*4  CKKNN
      CHARACTER*4  YYGG
      CHARACTER*6  AREG
      CHARACTER*6  CNAME
      CHARACTER*6  DTYPE
      CHARACTER*6  I3J3
      CHARACTER*8  CBPRF
      CHARACTER*5  GROUP
      CHARACTER*12 NNNQQ
      CHARACTER*21 WMOHDR
      CHARACTER*40 CHDR
      CHARACTER*72 CLINE
C
      CHARACTER*19 SECT0
      CHARACTER*36 SECT1
      CHARACTER*42 SECT1P
      CHARACTER*18 SECT3
      CHARACTER*30 SECT3H
      CHARACTER*25 SECT5
C
      CHARACTER*2280  DBLK
      CHARACTER*2560  NBLK
C
      INTEGER(8)  BULPRF
      INTEGER   ISUBS(6)
      INTEGER   IGP(0:199)
      INTEGER   KKNNX(0:2)
      INTEGER   KPDS(*)
      INTEGER   NCA(6)
      INTEGER   NCB(6)
      INTEGER   NCHRS(6)
      INTEGER   NDATA(285)
C
      LOGICAL  IPOLE
C
      EQUIVALENCE  ( CBPRF(1:1), BULPRF)
C
C
C /////////////////////////////////////////////////////////////////////
C
C                               HEADINGS
C
C         .............................................
C
C         '1XXXXX                                           (CHED)
C
C         TTAAII KWBC YYGG00<<@                             (HED)
C
C         GRID 07NNN 10101<<@                               (HED0)
C
C         111 1AAAA 2PP99 6JJMM 7YYGG 0MM99<<@              (HED1)
C
C         111 1AAAA 5BB00 6JJMM 7YYGG 0MM99<<@
C
C         111 1AAAA 2PP99 6JJMM 7YYGG 81TTT 0MMGG<<@        (HED1P)
C
C         111 1AAAA 5BB00 6JJMM 7YYGG 81TTT 0MMGG<<@
C
C         333 1151I 2NN20<<@                                (HED3)
C
C         333 1151I 2NN20 30S00 0RRRR<<@                    (HED3H)
C
C         555 07NNN 10101<<@777<<@%                         (HED5)
C
C         .............................................
C
C BULLETIN PREFIX
C
C     For Little_endian
C
      DATA  BULPRF /Z'2030303030303127' /
C
C     For Big_endian
C     DATA  BULPRF /Z'2731303030303020' /
C
C BULLETIN ID.  (NAME, ORIGINATING CENTER, AND DAY-TIME GROUP)
C
      DATA WMOHDR/ 'TTAAII KWBC YYGG00<<@' /
C
C SECTION 0.  (GRID CODE MESSAGE ID)
C
      DATA SECT0 / 'GRID 07NNN 10101<<@' /
C
C SECTION 1.  (GRID CODE DATA ID)
C
      DATA SECT1 / '111 1AAAA 2PP99 6JJMM 7YYGG 0MM99<<@' /
C
      DATA SECT1P/ '111 1AAAA 2PP99 6JJMM 7YYGG 81TTT 0MM99<<@' /
C
C SECTION 3.  (GRID CODE DATA FORMAT ID)
C
      DATA SECT3 / '333 1151I 2NN20<<@' /
C
      DATA SECT3H/ '333 1151I 2NN20 30S00 0RRRR<<@' /
C
C SECTION 5.  (GRID CODE MESSAGE TRAILER)
C
      DATA SECT5 / '555 07NNN 10101<<@777<<@%' /
C
C /////////////////////////////////////////////////////////////////////
C
C            NUMBER OF GRID POINTS PER DATA LINE SEGMENT
C
C           N2     1    2    3    4    5    6
C
      DATA NCA /  19,  19,  19,  13,  11,   9  /
      DATA NCB /   0,   0,   0,   6,   8,  10  /
C
C /////////////////////////////////////////////////////////////////////
C
C                  PRESS   HEIGHT  TEMP    DDDFF   UUVV    REL HUM
C
      DATA DTYPE / 'PHTWUR' /
      DATA LTYPE / 'P',    'H',    'T',    'W',    'U',    'R'     /
      DATA NTYPE / '0100', '0200', '0400', '2200', '2324', '1300'  /
      DATA NCHRS /  3,      3,      2,      5,      5,      2      /
      DATA ISUBS /  2,      2,      3,      2,      1,      2      /
C
C     (THE ABOVE ARRAYS MUST HAVE CORRESPONDING ENTRIES.)
C
C /////////////////////////////////////////////////////////////////////
C
C            N3     1   2   3   4   5   6   7   8   9  10  11  12
C
      DATA NNNQ / 'A','B','C','D','E','F','G','H','I','J','K','L' /
      DATA NNNQQ / 'ABCDEFGHIJKL' /
C
C /////////////////////////////////////////////////////////////////////
C         Model Run IDs updated 6/21/96
C
      DATA IGP   / 5*99, 11, 4*99, 80, 8*99, 48, 19*99, 48, 99, 99, 11 
     &           , 11, 11, 8*99, 48, 10*99, 11, 3*99, 50, 50, 80, 2*99
     &           , 4*80, 2*50, 80, 50, 2*11, 3*48, 3*80, 111*99  /
C
      DATA K1  / '0100' /
      DATA K2  / '0200' /
      DATA K7  / '0700' /
      DATA K8  / '0800' /
      DATA K9  / '0900' /
      DATA K10 / '1000' /
      DATA K15 / '1500' /
      DATA K20 / '2000' /
      DATA K25 / '2500' /
C
      DATA MINUS/ '1   ' /
      DATA NPOS / '0   ' /
C
      DATA NEOL / '<<@' /
      DATA AREG / '      '/, ICAT /0/, IBCKUP/0/, IDATYP/0/
C
      DATA KKNNX / 9901, 0, 9801 /
C
C
C /////////////////////////////////////////////////////////////////////
C
C
C                            INITIALIZATION
C
C
C LENGTH OF HEADINGS IN NUMBER OF CHARACTERS.
C
      LCHED  = 40
      LHED   = 21
      LHED0  = 19
      LHED1  = 36
      LHED1P = 42
      LHED3  = 18
      LHED3H = 30
      LHED5  = 25
C
      LL     = 18 * 4
      N1     = 285
      NB     = 640 * 4
C
      NEXT   = 0
C
C EXTRACT FORECAST HOUR FROM DATA FIELD LABEL. (ZERO IF ANALYSIS)
C
      NFCST = KPDS(14)
C
C EXTRACT LEVEL FROM DATA FIELD LABEL.
C
      LEVEL = KPDS(7)
C
C DETERMINE THE SUBSCRIPT FOR ARRAYS LTYPE, NTYPE, NCHRS AND ISUBS.
C
      I =  INDEX (DTYPE, CNAME(2:2))
      IS=  ISUBS(I)
      NA4= NTYPE(I)
      N2 = NCHRS(I)
C
      MARK=0
      IF (I .EQ. 2 .AND. (LEVEL.GT.0 .AND.
     &      LEVEL .LE. 300 .OR.  LEVEL .EQ. 1000)) THEN
          MARK = 1
      ENDIF
C
      IF (I.EQ.3)  THEN
C
C CHANGE GROUP CHR. LENGTH AND SIGN INDICATOR IF TEMPS BELOW 500 MB.
C
        IDS1 = KPDS(6)
        IF(IDS1.EQ.102.OR.IDS1.EQ.1 .OR. LEVEL .GT. 500) THEN
          N2=3
          IS=1
        ENDIF
      ENDIF
C
C DETERMINE THE VALUE OF NNN.
C
      N3  = INDEX (NNNQQ, CNAME(3:3))
      NNN = N3
      N1N2= N2 * 10
      IF (I .EQ. 5) N1N2 = 22
C
C DETERMINE QUADRANT BELT (NQB), WHERE N.H. = 0, TROP = 1, AND S.H. =2.
C
      NQB=(NNN-1)/4
C
C                      FORMAT AND BLOCK HEADINGS
C
C FORMAT VARIABLE AND BLOCK CHED.
C
C     ..... XXXXX (CATALOG NUMBER)
C
      NBLK = ' '
CCC   CALL W3FI61 (CHDR, NCAT, AREG, IBCKUP, IDATYP, IERR)
      NBLK(1:LCHED) = CBPRF  ! Replaces W3FI61
      NEXT  = LCHED
C
C FORMAT VARIABLES AND BLOCK HED.
C
C     ..... TTAAII (BULLETIN NAME)
C
      WMOHDR(1:6) = CNAME
C
C     ..... YYGG (DAY-TIME GROUP)
C
      IGG   = KPDS(11)
      IYY   = KPDS(10)
      IYYGG = IYY*100 + IGG
      CALL BIN2CHAR (IYYGG, YYGG, 4)
      WMOHDR(13:16) = YYGG
      NBLK(NEXT+1:NEXT+LHED) = WMOHDR
      NEXT  = NEXT + LHED
C
C FORMAT VARIABLE AND BLOCK HED0.
C
C     ..... NNN (CATALOG NUMBER OF GRID)
C
      CALL BIN2CHAR (NNN, CNNN, 3)
      SECT0(8:10) = CNNN(1:3)
      NBLK(NEXT+1:NEXT+LHED0) = SECT0
      NEXT  = NEXT + LHED0
C
C FORMAT VARIABLES AND BLOCK HED1.
C
C     ..... AAAA (BULLETIN DATA TYPE(S))
C
      SECT1(6:9) = NA4
      IF(LEVEL .GT. 0) THEN
C
C     ..... PP (PRESSURE LEVEL IN HUNDREDS OF MILLIBARS - GROUP 2PP99)
C
        IF (LEVEL .EQ. 1000) THEN
          LPP = 0
        ELSE
          LPP=LEVEL*10
        ENDIF
        LSFC=20099+LPP
      ELSEIF (LEVEL .EQ. 0) THEN
C
C     ..... BB (SPECIAL SFC - MSL, EARTH, OR TROPOPAUSE - GROUP 5BB00)
C           SET MARK = 1 WHEN .NE. TEMPS
C
        IDS1 = KPDS(6)
        IF (IDS1 .EQ. 102) THEN
          LSFC = 56000
          IF (NA4 .NE. NTYPE(3)) MARK = 1
        ELSEIF (IDS1 .EQ. 1) THEN
          LSFC = 50100
          IF (NA4 .NE. NTYPE(3)) MARK = 1
        ELSEIF (IDS1 .EQ. 7) THEN
          LSFC = 50700
        ELSE
          LSFC = 59900
        ENDIF
      ENDIF
C
C     .....INSERT EITHER GROUP 2PP99 OR 5BB00 IN SECTION 1 HEADING.
C
      CALL BIN2CHAR (LSFC, GROUP, 5)
      SECT1(11:15) = GROUP(1:5)
C
C     ..... JJMM (YEAR OF CENTURY AND MONTH)
C
      JJ   = KPDS(8)
      if (jj.eq.100) jj=0
      MM   = KPDS(9)
      JJMM = JJ*100 + MM
      CALL BIN2CHAR (JJMM, CJJMM, 4)
      SECT1(18:21) = CJJMM
C
C     ..... YYGG (DAY OF MONTH AND TIME GROUP)
C
      SECT1(24:27) = YYGG
C
C     ..... MM (PROCEDURE OR MODEL USED TO GENERATE THE DATA FIELD)
C
      M2 = KPDS(2)
      IF (M2 .GT. 199) M2 = 199
      MM = IGP(M2)
C
      CALL BIN2CHAR (MM, CMM, 2)
      SECT1(30:31) = CMM(1:2)
      IF (NFCST .GT. 0) THEN
C
C     ..... TTT (USE HED1P HEADING FOR FORECAST BULLETINS)
C
        SECT1P(1:27) = SECT1(1:27)
        CALL BIN2CHAR (NFCST, CFCST, 3)
        SECT1P(31:33) = CFCST(1:3)
        SECT1P(35:39) = SECT1(29:33)
        NBLK(NEXT+1:NEXT+LHED1P) = SECT1P
        NEXT  = NEXT + LHED1P
      ELSE
        NBLK(NEXT+1:NEXT+LHED1) = SECT1
        NEXT  = NEXT + LHED1
      ENDIF
C
C FORMAT VARIABLES AND BLOCK HED3. (HED3H IF ADDITIVE CONSTANT USED)
C
C     ..... I (SIGN INDICATOR)
C
      CIS = CHAR (IS + 48)
      SECT3(9:9) = CIS
C
C     ..... NN (NUMBER OF DIGITS EACH GRID POINT FOR A1A1A2A2)
C
      CALL BIN2CHAR (N1N2, CN1N2, 2)
      SECT3(12:13) = CN1N2(1:2)
C
      IF (MARK .EQ. 0) THEN
        NBLK(NEXT+1:NEXT+LHED3) = SECT3
        NEXT  = NEXT + LHED3
      ELSE
        SECT3H(1:13) = SECT3(1:13)
C
        NS=NPOS
        IF (LEVEL .EQ. 1000) THEN
          NS = MINUS
          KON = K1
        ELSEIF (LEVEL .EQ. 300) THEN
          KON = K7
        ELSEIF (LEVEL .EQ. 250) THEN
          KON = K8
        ELSEIF (LEVEL .EQ. 200) THEN
          KON = K9
        ELSEIF (LEVEL .LE. 150 .AND. LEVEL .GT. 70) THEN
          KON = K10
        ELSEIF (LEVEL .LE. 70  .AND. LEVEL .GT. 30) THEN
          KON = K15
        ELSEIF (LEVEL .LE. 30  .AND. LEVEL .GT. 10) THEN
          KON = K20
        ELSEIF (LEVEL .EQ. 10)                      THEN
          KON = K25
        ELSEIF (LEVEL .LT. 10  .AND. LEVEL .GT. 0)  THEN
          KON = K20
        ELSEIF (LEVEL .EQ. 0) THEN
          KON = K2
        ENDIF
C
C     ..... S (SIGN OF RRRR)
C
        SECT3H(19:19) = NS
C
C     ..... RRRR (ADDITIVE CONSTANT - DECAMETERS)
C
        SECT3H(24:27) = KON
        NBLK(NEXT+1:NEXT+LHED3H) = SECT3H
        NEXT  = NEXT + LHED3H
      ENDIF
C
C       F O R M A T   A N D   B L O C K   D A T A   L I N E S
C
      CALL BIN2EBCD (NDATA, DBLK, N1, N2, '1')
      IJ =  0
      INC= 10
      K  = NCA(N2)
      KK =  0
      NN = 19
      I3J3 = ' '
      IPOLE = .FALSE.
      IF (NQB .EQ. 0 .OR. NQB .EQ. 2) IPOLE = .TRUE.
C
      JLAST = 15
      DO J = 1, JLAST
        IF (J .NE. JLAST .OR. (J.EQ.JLAST .AND. .NOT.IPOLE)) THEN
          KK    = KK + 1
          KKNN  = KK * 100  + NN
          NC1   = NCA(N2)
          NC2   = NCB(N2)
        ELSEIF (J .EQ. JLAST .AND. IPOLE) THEN
          KKNN = KKNNX(NQB)
          NC1   = 1
          NC2   = 0
        ENDIF
        CALL BIN2CHAR (KKNN, CKKNN, 4)
        N   = 0
        CLINE = ' '
        CLINE(N+1:N+4) = CKKNN
        N   = N + 5
        CALL BIN2CHAR (IJ, I3J3, 6)
        IJ = IJ + INC
        CLINE(N+1:N+6) = I3J3
        N = N + 7
        I1 = (J - 1) * NN * N2
        CLINE(N+1:N+N2*NC1) = DBLK(I1+1:I1+N2*NC1)
        NBLK(NEXT+1:NEXT+N+N2*NC1+3) = CLINE(:N+N2*NC1) // NEOL
        NEXT = NEXT + N + N2 * NC1 + 3
        IF (NC2 .GT. 0) THEN
          I2 = I1 + N2 * NC1
          NBLK(NEXT+1:NEXT+N2*NC2+3) = DBLK(I2+1:I2+N2*NC2) // NEOL
          NEXT = NEXT + N2 * NC2 + 3
        ENDIF
      END DO
C
C FORMAT VARIABLE AND BLOCK TRAILER.
C
C
C     ..... NNN (CATALOG NUMBER OF GRID)
C
      SECT5(7:9) = CNNN
      NBLK(NEXT+1:NEXT+LHED5) = SECT5
      NEXT = NEXT + LHED5
C
      RETURN
      END
