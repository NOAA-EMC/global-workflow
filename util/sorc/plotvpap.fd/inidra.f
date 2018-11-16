      SUBROUTINE INIDRA(IMAP,ILVLT,KHMBS,ITOUT,KRUN1)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INIDRA               IDRA(1-19)
C   PRGMMR: LILLY            ORG: W/NMC412   DATE: 93-05-12
C
C ABSTRACT: ASSIGNING INFORMATION TO IDRA(1-19) IE; MAP
C   BACKGROUND, DATE/TIME, ETC...
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR  UNKNOWN
C   89-04-25  STEVE LILLY  DOCUMENT
C   93-05-12  LILLY CONVERT SUB. TO FORTRAN 77
C
C USAGE:    CALL INIDRA(IMAP,ILVLT,KHMBS,ITOUT,KRUN1)
C
C   INPUT ARGUMENT LIST:
C     IMAP     - INTEGER   WORD NUMBER OF UPPER AIR LEVELS TO
C              - PROCESS
C     ILVLT    - INTEGER   10 WORD ARRAY CONTAINING THE LIST
C              - OF LEVELS(S) TO PROCESS
C     KHMBS    - TABLE LISTING VARIOUS MAP BACKGROUNDS
C     KRUN1    - RUN OPTION READ IN FROM THE FIRST DATA CARD
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     ITOUT    - INTEGER FLAG WITH RANGE FROM 1 THRU 13 , USED
C              - TO DETERMINE TYPE OF MAP BACKGROUND DATA IS TO BE
C              - DISPLAYED ON. ITOUT IS A FUNCTION OF KRUN AND IS
C              - SET IN SUB KOPTN.
C     COMMON /BOBIN / LOCT(256),IDTBL(1539)
C     COMMON / DATE / NYR,NMO,NDA,NHR
C     COMMON /KPLOT / LABEL(2,1024),LABIX,NOBUF,IDRA(50)
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  NAS
C
C$$$
C
      COMMON /BOBIN / LOCT(256),IDTBL(1539)
      COMMON / DATE / NYR,NMO,NDA,NHR
      COMMON /KPLOT / LABEL(2,1024),LABIX,NOBUF,IDRA(50)
C
      INTEGER    JBCD(2)
      CHARACTER*12  IBCD
C
      INTEGER      IDMAP(50)
      DATA         IDMAP
     1      /-1, 50,   8H       B,8H        ,8H    MM/D,
     2      8H    D/YY,8H     12Z,8H        ,8H    100M,8H    B   ,
     3      8H    UABP,8H    LOT ,8H        ,8H        ,
     4      8H    NH40,8H    04  ,
     5      Z'0000000B',Z'00000004',Z'4A0C100C', 31*Z'00000000'/
C
      INTEGER    IHDTG(3)
      CHARACTER*4  KHIQSY(2)
      DATA         KHIQSY          /'UABP','LOT ' /
      CHARACTER*8  KHMBS(29)
C
      CHARACTER*4  KHRUN(4,23)
      DATA         KHRUN
     *          /'RADA','T   ','NH40','04  ','OPNL','    ','NH40','04  '
     1,         'OVR4','8OPN','NH40','04  ','FINA','L   ','NH40','04  ',
     2          'LFM ','    ','NH25','01  ','PFAX','    ','NH40','05  ',
     3          'BKUP','PFAX','NH40','05  ','1DOT','FAX ','NH20','02  ',
     4          '2DOT','FAX ','NH20','02  ','SIRS','PLOT','NH40','04  ',
     5          'TROP','IC1 ','MR40','02  ','TROP','IC2 ','MR40','02  ',
     6          'US1P','LOT ','NH10','01  ','UABP','LOT ','NH40','04  ',
     7          'NHEM','I20 ','NH20','03  ','WIND','PLOT','PN26','01  ',
     8          'NHEM','I40 ','NH40','04  ','SHEM','I40 ','SH40','02  ',
     9          'UABP','LOT ','NH60','01  ','RAOB','    ','NH25','01  ',
     A          'SFC ','PLOT','NH20','05  ','GOES','PLOT','GH26','01  ',
     B          'GOES','PLOT','GH26','02  '/
C     ...WHERE KHRUN IS SEQUENCED ACCORDING TO KRUN1...
C
      INTEGER    MAPNO(7)
      DATA       MAPNO           /80,81,82,83,84,85,86/
C
      DATA       K000Z           /Z'0000005A'/
      INTEGER    MSK00FF
      DATA       MSK00FF         /Z'00000000FFFFFFFF'/

      CHARACTER*2 CYR,CMO,CDY,CHR
      INTEGER   IDRX(10)
      CHARACTER*8 CWORK 
  
      EQUIVALENCE (CWORK,IWORK)

  100 FORMAT(A4)
C
      DO  211  I = 1,50
          IDRA(I) = IDMAP(I)
  211 CONTINUE
C
      IDRA(2) = MAPNO(IMAP)
      call byteswap(KHMBS(ILVLT), 8, 1)
      CALL GBYTES(KHMBS(ILVLT),IDRA(9),0,32,0,2)
      READ(KHRUN(1,KRUN1),100) IDRA(11)
      READ(KHRUN(2,KRUN1),100) IDRA(12)
      call byteswap(IDRA(11), 8, 2)
      IDRA(11) = ISHFT(IDRA(11),-32)
      IDRA(12) = ISHFT(IDRA(12),-32)
      IF(ITOUT .NE. 3) GO TO 222
C     ...OTHERWISE, THIS IS IQSY RUN...
      READ(KHIQSY(1),100) IDRA(11)
      READ(KHIQSY(2),100) IDRA(12)
      call byteswap(IDRA(11), 8, 2)
      IDRA(11) = ISHFT(IDRA(11),-32)
      IDRA(12) = ISHFT(IDRA(12),-32)
  222 CONTINUE
      READ(KHRUN(3,KRUN1),100) IDRA(15)
      READ(KHRUN(4,KRUN1),100) IDRA(16)
      call byteswap(IDRA(15), 8, 2)
      IDRA(15) = ISHFT(IDRA(15),-32)
      IDRA(16) = ISHFT(IDRA(16),-32)
C     ...WHICH IS THE MAP BKRND NAME
      IDRA(17) = ILVLT
      IDRA(18) = KRUN1
      IDRA(19) = IDTBL(3)
C
      
      CALL BIN2CH(NYR,CYR,2,'A99')
      CALL BIN2CH(NMO,CMO,2,'A99')
      CALL BIN2CH(NDA,CDY,2,'A99')
      CALL BIN2CH(NHR,CHR,2,'A99')
      
      PRINT *,' YR MT DY HR ',CYR,CMO,CDY,CHR

      IWORK = 0
      CWORK(5:8) = CMO // '/' //  CDY(1:1)
      IDRA(5) = IWORK
      CWORK(5:8) = CDY(2:2) // '/' // CYR
      IDRA(6) = IWORK
      CWORK(5:8) = ' ' // CHR // 'Z'
      IDRA(7) = IWORK
      call byteswap(IDRA(8),8, 9) 
      PRINT  300, (IDRA(IR),IR=3,19)
      call byteswap(IDRA(3), 8, 14)
      print *,  (IDRA(IR),IR=3,19)
  300 FORMAT(1H0, 10X, 14A8, 2X,Z16, 2X, Z16, 2X, Z16)
      RETURN
      END
