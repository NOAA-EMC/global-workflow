#if !defined (_BIG_ENDIAN) && !defined (_LITTLE_ENDIAN)
#error 'Neither _BIG_ENDIAN nor _LITTLE_ENDIAN is defined.'
#endif
      SUBROUTINE IW3GETV(LUGRBIX, LUGRB, LABEL, FLD, MTITLE, IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: IW3GETV       GET AND UNPACK CRAY GRIB FILES.
C   AUTHOR: KRISHNA KUMAR         ORG: W/NP12    DATE: 1999-08-01
C
C ABSTRACT: GET AND UNPACK CRAY GRIB FILES FROM AN OFFICE 84 6 WORD ID.
C           PLEASE READ THE REMARKS BEFORE USING IT.
C
C HISTORY LOG:
C   95-10-04  LUKE LIN
C   95-10-31  LUKE LIN      MODIFY FOR TESTING DIFFERENT GRID TYPE.
C   95-11-27  LUKE LIN      ADD AN OPTION FOR NOT TEST FCST HOUR -- CAC 96H/C
C                           AND MODIFY TO READ RGL(NGM) MODEL.
C   96-01-30  LUKE LIN      MODIFY TO CONVERT FROM 1 DEGREE TO 65*65 BY LINEAR.
C   96-02-14  LUKE LIN      MODIFY FOR NGM BOUNDARY LAYER RH CHART.
C   96-02-23  LUKE LIN      MODIFY FOR NGM SURFACE TO 500 MB LIFTED INDEX.
C   96-02-26  LUKE LIN      MODIFY FOR AVMER OR AVPOLAR AND OUTPUT WILL BE
C                           EITHER 2.5 OR 5.0 DEGREE DEPENDING ON OPTION
C   96-03-07  LUKE LIN      MODIFY FOR NGM SUPER C GRID.
C   96-03-29  LUKE LIN      MODIFY FOR AVPOLAR 2.5 DEGREE TO GET NH.
C   96-04-25  LUKE LIN      FIX A BUG FOR AVTYPE.
C   96-05-31  LUKE LIN      MODIFY FOR TYPE 29 NH 2.5 AND TYPE 30 SH 2.5.
C   96-06-20  LUKE LIN      CHANGE FOR NAM LIFTED INDEX
C   96-07-05  LUKE LIN      ADD GFS 129*129 LOGIC.
C   96-07-09  LUKE LIN      ADD IPOLATE OPTIONS.  
C   96-10-01  LUKE LIN      ADD TYPE 08, 116X44, AND TYPE 53, 117X51.
C   97-01-08  LUKE LIN      ADD TYPE 05, 53X57.
C   97-01-15  LUKE LIN      ADD FOR MODEL FINAL.
C   97-03-12  LUKE LIN      ADD FOR ECMWF, TDL PROB. THUNER/SERVERE WX,
C                           GFS 6-10 DAY MEAN DN, UKMET
C   97-03-21  LUKE LIN      ADD FOR GFS ANOMALY THICKNESS.
C   97-06-03  LUKE LIN      MODIFY FOR NAM BOUNDARY LAYER FIELDS.
C 1999-08-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM
C                           RS/6000. MODIFIED TO READ 700 MB UKMET
C                           REL HUM (RH) DATA. 
C 1999-12-20  KRISHNA KUMAR KBMS VALUES SHOULD BE SET TO LOGICAL*1 
C               
C INPUT ARGUMENTS:
C   LUGRBIX    - AN UNIT NUMBER POINTS TO ONE DEGREE GRIB INDEX FILE.
C   LUGRB      - AN UNIT NUMBER POINTS TO ONE DEGREE GRIB FILE. SEE 
C                REMARK.
C   LABEL      - 12 WORDS LABEL ON INPUT.  WORDS 1-5 SHOULD CONTAIN
C                LABEL ID OF DESIRED FIELD FLD.
C
C OUTPUT ARGEMENTS:
C   FLD     -- BUFF CONTAINS UNPACK FIELD.
C   LABEL   -- ON OUTPUT THE FULL 12 WORDS OF THE LABEL WILL CONTAIN
C              APPROPRIATE VALUES.
C   MTITL   -- CHARACTER*132 MAP TITLE FROM INDEX FILE.
C              (1:86) IS MAP TITLE, FOLLOWINGS ARE MAP ID.
C   IERR    -- RETURN STATUS
C   IRETUR   - RETURN CONDITION;
C            0 - ALL OK
C            1 - W3FP13/GRIB BLOCK 0 NOT CORRECT
C            2 - W3FP13/LENGTH OF PDS NOT CORRECT
C            3 - W3FP13/COULD NOT MATCH TYPE INDICATOR
C            4 - W3FP13/GRID TYPE NOT IN TABLES
C            5 - W3FP13/COULD NOT MATCH TYPE LEVEL
C            6 - W3FP13/COULD NOT INTERPRET ORIGINATOR OF CODE
C            7 - GRIB TYPE ERROR -- UNKNOWN GRIB TYPE.
C           10 - UNCOGNIZE DATA TYPE FROM LABEL
C           11 - W3FT32/ MAPIN NOT RECOGNIZED
C           12 - W3FT32/ MAPOUT NOT RECOGNIZED
C           13 - W3FT32/ PARTICULAR POLA MAPOUT NOT RECOGNIZED
C           14 - W3FT32/ PARTICULAR LOLA MAPOUT NOT RECOGNIZED
C           15 - W3FT32/ PARTICULAR LOLA MAPIN NOT RECOGNIZED
C           16 - W3FT32/ PARTICULAR POLA MAPOUT NOT RECOGNIZED
C           17 - W3FT32/ PARTICULAR LOLA MAPIN NOT RECOGNIZED
C           18 - W3FT32/ PARTICULAR LOLA MAPOUT NOT RECOGNIZED
C           96 - GETGB/ERROR READING INDEX FILE
C           97 - GETGB/ERROR READING GRIB FILE
C           98 - GETGB/NUMBER OF DATA POINTS GREATER THAN JF
C           99 - GETGB/REQUEST NOT FOUND
C           -1 - GETGB/OTHER  W3FI63 GRIB UNPACKER RETURN CODE
C          101 - W3FP11/NON-FATAL ERROR, MTITLE NOT CORRECT
C
C USAGE:
C   INPUT FILES:
C     UNIT LUGRBIX -  INPUT ONE DEGREE GRIB INDEX FILE.  SEE REMARK.
C     UNIT LUGRB   -  INPUT ONE DEGREE GRIB FILE.  SEE REMARK.
C
C   OUTPUT FILES:
C     FT06F001 -   PRINT OUTPUT (STANDARD FORTRAN OUTPUT FILE)
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:     NONE
C
C     LIBRARY:
C       SPECIAL  - NONE
C       W3LIB    - W3FP11 , W3FP13, GETGB , GBYTES , W3FM08
C                - W3FI63 , W3FT43V,W3FT32, SBYTES
C
C   REMARKS:
C       ***** VERY IMPORTANT ******
C       1. ONLY DEAL WITH INPUT DATA TYPE 08, 26, 27 , 29, 30, 53, 101, 104.
C          OTHER DATA TYPE MAY HAVE FATAL ERROR RETURN AT PRESENT TIME.
C       2. CONCERNING NAM CHARTS, THE 6TH WORD ID SHOULD BE '00000127'.
C       3. CONCERNING GFS CHARTS, THE 6TH WORD ID SHOULD BE '0000084D'.
C       4. FOR NO FORECAST HOUR CHECKING SUCH AS CAC 96H/C, THE THIRD BYTE
C          OF THE 6TH WORD ID SHOULD BE '96'.
C
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
C
C      .... THIS SET IS FOR UOR_DOT
       COMMON /UOR2D/ UGRIDT1,UXPO,UYPO,UXADJUS,UYADJUS,UCU2GI,UORFG,
     1                IP,IPOPT
       REAL     UGRIDT1,UXPO,UYPO,UXADJUS,UYADJUS,UCU2GI
       LOGICAL  UORFG
C
C
      PARAMETER (IPTS=65160,II=360,JJ=181,MM=361)
      PARAMETER (IPTS2=10512,II2=144,JJ2=73)
      PARAMETER (MXSIZE=66000)
      PARAMETER (MXSIZ2=MXSIZE*2)
      PARAMETER (IIK=289,JJK=145,NPTS=IIK*JJK)
      PARAMETER (IJK=145,LJK=73,MPTS=IJK*LJK)
C
      parameter(ji=360*181,ig27=27,jo27=65*65,jo27_129=129*129)
      parameter(ig26=6)
      parameter(ig28=28)
      parameter(ig08=08,jo08=116*44)
      parameter(ig53=53,jo53=117*51)
C
      real rlat_03(ji),rlon_03(ji)
      logical lo_03(ji)
C
      real rlat_27(jo27),rlon_27(jo27)
      equivalence( rlat_27(1), rlat_03(1) )
      equivalence( rlon_27(1), rlon_03(1) )
      logical lo_27(jo27)
      equivalence( lo_27(1), lo_03(1) )
C
      real rlat_27_129(jo27_129),rlon_27_129(jo27_129)
      equivalence( rlat_27_129(1), rlat_03(1) )
      equivalence( rlon_27_129(1), rlon_03(1) )
      logical lo_27_129(jo27_129)
      equivalence( lo_27_129(1), lo_03(1) )
C
      real rlat_08(jo08),rlon_08(jo08)
      equivalence( rlat_08(1), rlat_03(1) )
      equivalence( rlon_08(1), rlon_03(1) )
      logical lo_08(jo08)
      equivalence( lo_08(1), lo_03(1) )
C
      real rlat_53(jo53),rlon_53(jo53)
      equivalence( rlat_53(1), rlat_03(1) )
      equivalence( rlon_53(1), rlon_03(1) )
      logical lo_53(jo53)
      equivalence( lo_53(1), lo_03(1) )
C
      integer ibi,ibo
      integer         kgdsi(22)
C
C
      COMMON /PDSCOM/ IFWPDS
      INTEGER         IFWPDS(27)

      INTEGER      KGDSO(22)
      CHARACTER    GDSO(42)
      INTEGER      LENGDS
      REAL         DIF,SMLNO
C
C
      INTEGER         LUGI,LUGRBIX
      INTEGER         LUGB,LUGRB
      INTEGER         LABEL(12)
      REAL            FLD(*)
      REAL            B129(16900)
      CHARACTER * 132 MTITLE
      INTEGER         IERR
      INTEGER         IRET
      INTEGER         ITAU
C
      REAL            C(MXSIZE)
      COMMON /BITM/   KBMS(66000)
      LOGICAL*1        KBMS
C
      REAL            CC(II,JJ)
      REAL            CC2(II2,JJ2)
      REAL            D(MXSIZE)
      REAL            DD(361,181)
      REAL            D2(MXSIZE)
      REAL            DD2(145,73)
      REAL            DD2N(145,37)
      REAL            DD2N1(5365)
      REAL            EE(4225)
      REAL            FF(4225)
      REAL            BLOLA(NPTS), CLOLA(MPTS)
      REAL            GG(73,37)
      REAL            GG2(2701)
C
      INTEGER         IGRIB(16500)
      CHARACTER * 1   GRIB(MXSIZ2)
C
      INTEGER         IPDS(4)
      INTEGER         JPDS(4)
      INTEGER         JGDS(100)
      INTEGER         MPDS(25)
      INTEGER         KGDS(100)
      INTEGER         KPDS(25)
      CHARACTER * 1   PDS(28)
      CHARACTER * 1   PDSL(28)
C
C
       CHARACTER*1     CTEMP(8)
       INTEGER         ITEMP
       INTEGER         NTEMP
C
       INTEGER         LIDREC(6)
       CHARACTER*8     CGRIB
       INTEGER         IMODE
C
      INTEGER      LABELP(6)
      CHARACTER*1  CLABELP(48)
      EQUIVALENCE  (LABELP(1),CLABELP(1))
C
      CHARACTER*1  IFLAG
      CHARACTER*1  IDPDSC(28)
      INTEGER*8    IDPDS(4)
      INTEGER      MODELNH
      INTEGER      SFCMEANRH
      INTEGER      MSK1, MSK2, MSK3, MSK4
      INTEGER      MSK2F
C
      INTEGER      MSKNH, MSKNGM, MSK25, MSKFNL, MSKTRAJ
      INTEGER      TRAJVD
      INTEGER      ITYPE, IMODEL, FLAG
      INTEGER      CAC96HC
      INTEGER      GFS97HC
      INTEGER      GFS98TK
      INTEGER      BLRHHDS
      INTEGER      BLRHGRB
      INTEGER      MSKFFFF
      INTEGER      MSK0FFF
      INTEGER      LFTFLG
      INTEGER      AVPOLRV
      INTEGER      AVTYPE
      INTEGER      ETAMOD 
      INTEGER      ETABOUND
      INTEGER     MSKECMWF
      DATA        MSKECMWF  /Z'000000000000004B'/
      INTEGER     MSKTHUND
      DATA        MSKTHUND  /Z'00000000000000C1'/
      INTEGER     MSKUK   
      DATA        MSKUK     /Z'000000000000004A'/
      INTEGER     MSKUK1
      DATA        MSKUK1    /Z'000000000000007D'/
C
C MSKUK1 HAS BEEN SPECIFICALLY DEFINED TO PLOT 700 MB REL HUM
C WITH UK MET DATA
C
      SAVE
C
      EQUIVALENCE     (C(1),CC(1,1),CC2(1,1))
      EQUIVALENCE     (D(1),DD(1,1))
      EQUIVALENCE     (D2(1),DD2(1,1))
      EQUIVALENCE     (DD2N1(1),DD2N(1,1))
      EQUIVALENCE     (GRIB(1),IGRIB(1))
      EQUIVALENCE     (IPDS(1),PDS(1))
      EQUIVALENCE     (JPDS(1),PDSL(1),IGRIB(2))
      EQUIVALENCE     (CTEMP,ITEMP)
      EQUIVALENCE     (IDPDSC(1),IDPDS(1))
      EQUIVALENCE     (GG2(1),GG(1,1))
C
      DATA        MODELNH   /Z'00000000034E0000'/
      DATA        SFCMEANRH /Z'0000000005809100'/
      DATA        MSK1      /Z'00000000FFFFFF00'/
      DATA        MSK2      /Z'00000000000000FF'/
      DATA        MSK3      /Z'000000000000FFFF'/
      DATA        MSK4      /Z'0000000000FF0000'/
      DATA        MSKNH     /Z'000000000000084D'/
      DATA        MSKNGM    /Z'0000000000000127'/
      DATA        MSKTRAJ   /Z'00000000000000C0'/
      DATA        TRAJVD    /Z'0290000000000000'/
      DATA        MSKFNL    /Z'0000000000000052'/
      DATA        MSK25     /Z'0000000000000A87'/
      DATA        CAC96HC   /Z'0000000000960000'/
      DATA        GFS97HC   /Z'0000000000970000'/
      DATA        GFS98TK   /Z'0000000000980000'/
      DATA        BLRHHDS   /Z'346C006400000000'/
      DATA        BLRHGRB   /Z'346B265F00000000'/
      DATA        MSKFFFF   /Z'FFFFFFFF00000000'/
      DATA        MSK0FFF   /Z'00FFFFFF00000000'/
      DATA        LFTFLG    /Z'8365326400000000'/
      DATA        AVPOLRV   /Z'0000000000990000'/
      DATA        ETAMOD    /Z'0000000000800000'/
      DATA        ETABOUND  /Z'006C616400000000'/
      DATA        AVTYPE    / 130 /
      DATA        MSK2F     /Z'FFFF000000000000'/
C
C***********************************************************************
C
C
      IRET   = 0
      IERR = 0
      LUGI = LUGRBIX
      LUGB = LUGRB
C
              WRITE(6,224)(LABEL(I),I=1,12)
C
      ITYPE = IAND(LABEL(5),MSK2)
      IFCSTHR = IAND(LABEL(1),MSK2)
      PRINT *, ' ITYPE=', ITYPE
      IMODEL = IAND(LABEL(6),MSK3)
      FLAG = IAND(LABEL(6),MSK4)
C
C     if (itype .eq. avtype) then
C         print *, ' got a avpolrv or avmerv data type....'
C     endif
C
      IF (FLAG.EQ.AVPOLRV .OR. FLAG.EQ.ETAMOD) THEN
          LABEL(6) = IAND(LABEL(6),MSK3)
      ENDIF
C
      IF (IMODEL .EQ. MSKNH) THEN
              LABEL(7) = 0
              LABEL(8) = MODELNH
              IF (FLAG .NE. 0) LABEL(6) = MSKNH
      ELSE IF (IMODEL .EQ. MSKNGM) THEN
              LABEL(6) = 0
              LABEL(7) = 0
              LABEL(8) = 39
      ELSE IF (IMODEL .EQ. MSKFNL) THEN
              LABEL(6) = 0
              LABEL(7) = 0
      ELSE IF (IMODEL .EQ. MSK25) THEN
              LABEL(7) = 0
              LABEL(8) = MODELNH
              IF (FLAG .NE. 0) LABEL(6) = MSK25
      ENDIF
C
#if defined (_LITTLE_ENDIAN)
           CALL SBYTESCCS(LABELP,LABEL,0,32,0,8)
#else
              CALL SBYTES(LABELP,LABEL,0,32,0,8)
#endif
C             ..... PACK 8-WORD ID TO 4-WORD CRAY ID
              IFLAG    = CHAR(128)
C             .... 128 FOR GDS, 64 FOR BITMAP, 192 FOR BOTH
              ICENT = 20
              ISCALE = 0
              CALL W3FP12(LABELP, IFLAG, IDPDSC, ICENT, ISCALE, IERR)
C             .... CONVERT OFFICE 84' ID TO PDS .....
              IF (IERR .NE. 0) THEN
                  IF (IMODEL .EQ. MSKTRAJ) THEN
                     PRINT *, ' ** RETURN FROM W3FP12 =', IERR
                     PRINT *, ' PASS THIS TRAJETORY ID'
                     IERR = 0
                  ELSE
                     PRINT *, ' **FATAL RETURN FROM W3FP12 =', IERR
                     WRITE(6,224)(LABEL(I),I=1,12)
                     WRITE(6,226)(LABELP(I),I=1,4)
                     WRITE(6,227)(IDPDS(I),I=1,4)
                     GO TO 999
                  ENDIF
              ENDIF
C
              WRITE(6,224)(LABEL(I),I=1,12)
  224         FORMAT(1X,'LABEL=',/,2(1X,6(Z16,1X),/))
              WRITE(6,226)(LABELP(I),I=1,4)
  226         FORMAT(1X,'LABELP=',/,(1X,4(Z16,1X),/))
              WRITE(6,227)(IDPDS(I),I=1,4)
  227         FORMAT(1X,'IDPDS=',/,(1X,4(Z16,1X),/))
         IF (IMODEL .EQ. MSKNH) THEN
              IDPDSC(4) = CHAR(02)
              IDPDSC(6) = CHAR(78)
              IDPDSC(7) = CHAR(03)
              IF (IFCSTHR.GT.168) IDPDSC(7)= CHAR(02)
#if defined (_LITTLE_ENDIAN)
      ISCRATCH = IDPDS(2)
      CALL SWAP_BYTES_IN_PLACE (ISCRATCH, 8_8, 1_8)
      ITEMP = IAND (ISCRATCH, MSKFFFF)
#else
              ITEMP = IAND(IDPDS(2),MSKFFFF)
#endif
              IF (FLAG .EQ. CAC96HC) THEN
                 print *, ' **try to read cac blend grib'
                IDPDSC(6) = CHAR(80)
                IDPDSC(7) = CHAR(27)
                IDPDSC(8) = CHAR(192)
                IF (IDPDSC(9).EQ.CHAR(210).AND.
     1                 IDPDSC(10).EQ.CHAR(100)) THEN
                    IDPDSC(9) = CHAR(27)
                ENDIF
              ELSE IF (FLAG .EQ. GFS97HC) THEN
                 print *, ' **try to read gfs blend grib'
                IDPDSC(6) = CHAR(80)
                IDPDSC(7) = CHAR(02)
                IDPDSC(8) = CHAR(128)
                IF (IDPDSC(9).EQ.CHAR(210).AND.
     1                 IDPDSC(10).EQ.CHAR(100)) THEN
                    IDPDSC(9) = CHAR(27)
                ENDIF
              ELSE IF (FLAG .EQ. GFS98TK) THEN
                 print *, ' **try to read gfs THK       '
                IDPDSC(6) = CHAR(80)
                IDPDSC(7) = CHAR(03)
                IDPDSC(8) = CHAR(128)
                IF (IDPDSC(9).EQ.CHAR(210).AND.
     1                 IDPDSC(10).EQ.CHAR(100)) THEN
                    IDPDSC(9) = CHAR(27)
                ENDIF
              ELSE IF (ITYPE.EQ.27 .AND. ITEMP.EQ.LFTFLG) THEN
                 PRINT *,' **GET GFS 500 LIFTED INDEX'
                 IDPDSC(10) = CHAR(01)
                 IDPDSC(11) = CHAR(00)
                 IDPDSC(12) = CHAR(00)
              ENDIF
         ELSE IF (IMODEL .EQ. MSK25) THEN
              IDPDSC(4) = CHAR(02)
              IDPDSC(6) = CHAR(77)
              IDPDSC(7) = CHAR(03)
              IF (IFCSTHR.GT.168) IDPDSC(7)= CHAR(02)
         ELSE IF (IMODEL .EQ. MSKNGM) THEN
              IDPDSC(4) = CHAR(02)
              IDPDSC(6) = CHAR(39)
              IDPDSC(7) = CHAR(06)
              IF (ITYPE .EQ. 05) THEN
                 IDPDSC(7) = CHAR(05)
              ENDIF
              IF (ITYPE .EQ. 101) THEN
                 IDPDSC(7) = CHAR(101)
                 PRINT *, ' TRY TO GET A NGM C-GRID'
              ENDIF
              IF (ITYPE .EQ. 104) THEN
                 IDPDSC(7) = CHAR(104)
                 PRINT *, ' TRY TO GET A NGM SUPER C-GRID'
              ENDIF
#if defined (_LITTLE_ENDIAN)
      ISCRATCH = IDPDS(2)
      CALL SWAP_BYTES_IN_PLACE (ISCRATCH, 8_8, 1_8)
      ITEMP = IAND (ISCRATCH, MSKFFFF)
#else
              ITEMP = IAND(IDPDS(2),MSKFFFF)
#endif
              IF (ITEMP .EQ. BLRHHDS) THEN
                 PRINT *,'  **GET NGM BOUNDARY LAYER RH'
                 ITEMP = BLRHGRB
                 IDPDSC(9) = CTEMP(1)
                 IDPDSC(10) = CTEMP(2)
                 IDPDSC(11) = CTEMP(3)
                 IDPDSC(12) = CTEMP(4)
             ENDIF
             IF (FLAG .EQ. ETAMOD) THEN
C                ... FOR NAM BOUNDARY LAYER
#if defined (_LITTLE_ENDIAN)
      ISCRATCH = IDPDS(2)
      CALL SWAP_BYTES_IN_PLACE (ISCRATCH, 8_8, 1_8)
      NTEMP = IAND (ISCRATCH, MSK0FFF)
#else
                 NTEMP = IAND(IDPDS(2),MSK0FFF)
#endif
                 IF (NTEMP .EQ. ETABOUND) THEN
                     PRINT *, ' ..GET A NAM BOUNDARY LAYER FIELD'
                     IDPDSC(10) = CHAR(116)
                     IDPDSC(11) = CHAR(30)
                     IDPDSC(12) = CHAR(0)
                 ENDIF
             ENDIF
C
         ELSE IF (IMODEL .EQ. MSKTRAJ) THEN
              IDPDSC(4) = CHAR(02)
              IDPDSC(6) = CHAR(141)
              IDPDSC(7) = CHAR(05)
              IDATATP = IAND(LABELP(1),MSK2F)
              IF (IDATATP .EQ. TRAJVD) THEN
C              ... FOR THE VERTICAL REPLACEMENT
                  PRINT *,' FOR TRAJECTORY VERTICAL REPLACEMENT'
                  IDPDSC(9) = CHAR(183)
                  IDPDSC(10) = CHAR(100)
                  IDPDSC(11) = CHAR(02)
                  IDPDSC(12) = CHAR(188)
                  IDPDSC(20) = CHAR(24)
                  IDPDSC(19) = CLABELP(12)
                  IDPDSC(21) = CHAR(2)
              ELSE IF (IDPDSC(9).EQ.CHAR(17)) THEN
                  IDPDSC(9)=CHAR(152)
                  IF (IDPDSC(11).EQ.CHAR(0).AND.IDPDSC(12).EQ.CHAR(0))
     1               IDPDSC(10) = CHAR(01)
C                 ... FOR TRAJ DEW-PTS TEMP 700MB AND SFC
              ELSE IF (IDPDSC(9).EQ.CHAR(11)) THEN
                  IDPDSC(9)=CHAR(148)
              ELSE IF (IDPDSC(9).EQ.CHAR(133)
     1                 .AND.IDPDSC(10).EQ.CHAR(101)) THEN
C                      ....for k index ....
                  PRINT *,' FOR TRAJECTORY INDEX'
                  IDPDSC(9) = CHAR(182)
                  IDPDSC(10) = CHAR(01)
                  IDPDSC(11) = CHAR(00)
                  IDPDSC(12) = CHAR(00)
              ENDIF
C
         ELSE IF (IMODEL .EQ. MSKFNL) THEN
              IDPDSC(4) = CHAR(02)
              IDPDSC(6) = CHAR(82)
              IDPDSC(7) = CHAR(03)
         ELSE IF (IMODEL .EQ. MSKUK ) THEN
c              print *, ' **try to read ukmet grid 500 MB RH'
              IDPDSC(5) = CHAR(74)
              IDPDSC(6) = CHAR(15)
              IDPDSC(7) = CHAR(02)
              IF (IDPDSC(9) .EQ. CHAR(52)) THEN
C                ... THIS UKMET 500 MB RH
                  IF (IDPDSC(10) .EQ. CHAR(108)) IDPDSC(10)=CHAR(100)
                  IF (IDPDSC(11) .EQ. CHAR(33)) IDPDSC(11)=CHAR(1)
                  IF (IDPDSC(11) .EQ. CHAR(44)) IDPDSC(11)=CHAR(1)
                  IF (IDPDSC(12) .EQ. CHAR(100)) IDPDSC(12)=CHAR(244)
              ENDIF
         ELSE IF (IMODEL .EQ. MSKUK1 ) THEN
              print *, ' **try to read ukmet grid 700 MB RH'
              IDPDSC(5) = CHAR(74)
              IDPDSC(6) = CHAR(15)
              IDPDSC(7) = CHAR(02)
              IF (IDPDSC(9) .EQ. CHAR(52)) THEN
C                ... THIS UKMET 700 MB RH
                  IF (IDPDSC(10) .EQ. CHAR(108)) IDPDSC(10)=CHAR(100)
                  if(idpdsc(11).eq.char(33)) idpdsc(11)=char(2)
                  if(idpdsc(12).eq.char(100)) idpdsc(12)=char(188)
              ENDIF

         ELSE IF (IMODEL .EQ. MSKECMWF ) THEN
              print *, ' **try to read ecmwf grid'
              IDPDSC(4) = CHAR(01)
              IDPDSC(5) = CHAR(98)
              IDPDSC(6) = CHAR(152)
              IDPDSC(7) = CHAR(02)
              IF (IDPDSC(9) .EQ. CHAR(02)) THEN
C                ... THIS ECMWF PRESSURE AT THE SURFACE
                  IDPDSC(10) = CHAR(01)
              ENDIF
C
         ELSE IF (IMODEL .EQ. MSKTHUND) THEN
              PRINT *,' GET NGM MOS THUNDER/SEVERE WX ID....'
              IDPDSC(6) = CHAR(141)
              IDPDSC(8) = CHAR(192)
              IF (IDPDSC(9).EQ.CHAR(60)) THEN
                  IDPDSC(9) = CHAR(149)
              ELSE IF (IDPDSC(9).EQ.CHAR(59)) THEN
                  IDPDSC(9) = CHAR(151)
              ENDIF
                  IDPDSC(10) = CHAR(1)
                  IDPDSC(20) = CHAR( IAND(LABEL(3),MSK2))
C
         ELSE IF (ITYPE.EQ.8 .OR. ITYPE.EQ.53) THEN
                PRINT *,' ****TRY TO GET A MERCTOR GRID.'
              IDPDSC(4) = CHAR(02)
              IDPDSC(6) = CHAR(77)
              IDPDSC(7) = CHAR(03)
              IF (IFCSTHR.GT.168) IDPDSC(7)= CHAR(02)
         ENDIF
C
              IF ( IAND(LABEL(1),MSK1) .EQ. SFCMEANRH) THEN
C                IDPDSC(7) = CHAR(202)
                 IF (IMODEL .NE. MSKNGM.AND.IMODEL.NE.MSKUK1) THEN
                    IDPDSC(11) = CHAR(44)
                 ENDIF
C                print *, ' get a sfc-500 mean rh'
              ENDIF
C             .... MODIFY PDS TO POINT TO 1-DEGREE GRIB
              DO I=1, 28
                 PDS(I) = IDPDSC(I)
              ENDDO
C
      JREW   = 0
      MPDS   = -1
      MPDS(3) = mova2i(PDS(7))
      MPDS(5) = mova2i(PDS(9))
      MPDS(6) = mova2i(PDS(10))
      MPDS(7) = mova2i(PDS(11)) * 256 + mova2i(PDS(12))
      IF (IMODEL .EQ. MSKTRAJ) THEN
           IDATATP = IAND(LABELP(1),MSK2F)
C          PRINT *,' SET MPDS FOR TRAJVD='
           IF (IDATATP .EQ. TRAJVD) THEN
C              PRINT *,' SET MPDS FOR TRAJVD'
C              MPDS(14) = mova2i(PDS(19))
               MPDS(15) = mova2i(PDS(20))
C              MPDS(16) = mova2i(PDS(21))
           ENDIF
      ELSE IF (FLAG.NE.CAC96HC .AND. FLAG.NE.GFS97HC) THEN
         MPDS(14) = mova2i(PDS(19))
C     ..... FOR THE FORECAST HOUR CHECKING
      ELSE IF (IMODEL .EQ. MSKTHUND) THEN
C              MPDS(14) = mova2i(PDS(19))
               MPDS(15) = mova2i(PDS(20))
C              MPDS(16) = mova2i(PDS(21))
      ENDIF
C
      IF (FLAG .EQ. AVPOLRV) THEN
         IDPDSC(7) = CHAR(03)
         MPDS(3) = 3
C     ..... FOR AVPOLARV
      ENDIF
C
C     READ I DEGREE GRIB FILE USING INDEX FILE
C
C     print *,' mpds='
C     write (*,203)(mpds(k),k=1,25)
C 203 format( 5(5(1x,z16),/) )
C     print *,' jgds='
C     write (*,204)(jgds(k),k=1,100)
C 204 format( 20(5(1x,z16),/) )
C
C
      print *, ' Pds = '
      write(*, 12)(ipds(i),i=1,4)
  12  format( 2x, 4(z16,1x))
C
C     CALL GETGB1(LUGB,LUGI,MXSIZE,JREW,MPDS,JGDS,
C    &      GRIB,KBYTES,KREW,KPDS,KGDS,KBMS,C,IRET)
C
      CALL GETGB1(LUGB,LUGI,ji,JREW,MPDS,JGDS,
     &      GRIB,KBYTES,KREW,KPDS,KGDSi,KBMS,CC,IRET)
C
C
      IF (IRET .NE. 0) THEN
          PRINT *,' **FATAL ERROR FROM GETGB.', IRET
          IERR = IRET
          GO TO 999 
      ENDIF
C
C     print *,'after getgb  kpds = ',kpds
      print *, ' Jds = '
      write(*, 16)(jpds(i),i=1,4)
  16  format( 2x, 4(z16,1x))
C
      IF (KPDS(16) .EQ. 0) THEN
         ITAU = KPDS(14)
      ELSE IF (KPDS(16) .EQ. 2) THEN
         ITAU = KPDS(15)
      ELSE IF (KPDS(16) .EQ. 3) THEN
         ITAU = KPDS(15)
      ELSE IF (KPDS(16) .EQ. 4) THEN
         ITAU = KPDS(15)
      ELSE IF (KPDS(16) .EQ. 10) THEN
         ITAU = KPDS(14)
      ELSE
         PRINT *,' **ERROR - CAN NOT GET TAU.  KPDS(16)=',KPDS(16)
      ENDIF
C
      print *,' ITAU=', itau
      print *, ' Kds = '
      write(*, 14)(kpds(i),i=1,24)
  14  format( 6(2x, 4(z16,1x),/))
C
C     print *,'after getgb  mpds = ',mpds
C     PRINT *,'KBYTES = ',KBYTES
C     PRINT *,'RECORD NO. OF GRIB RECORD IN INPUT FILE = ',KREW
C
  700 CONTINUE
C
C     flip the grid so 1st point is S.pole at Greenwich
C
      IF (KPDS(3) .EQ. 3) THEN
         PRINT *, ' GET A ONE DEGREE GRIB'
         DO N1=1,181
            DO M1=1,360
               DD(M1,181-N1+1) = CC(M1,N1)
            ENDDO
         ENDDO
C
         DO N1 = 1, 181
            DD(361,N1) = DD(1,N1)
         ENDDO
C
         IF (ITYPE .EQ. AVTYPE) THEN
C           print *, '  convert from one-degree to 5 degree ...'
            LL1 = 0
            DO L1 = 1, 361, 5
               LL1 = LL1 + 1
               II1 = 0
               DO I1 = 1, 181, 5
                  II1 = II1 + 1
                  GG(LL1,II1) = DD(L1,I1)
               ENDDO
            ENDDO
C           ... LOAD IT TO OUTPUT BUFFER
            DO III=1,2701
               FLD(III) = GG2(III)
            ENDDO
C
         ELSE IF (FLAG .EQ. AVPOLRV) THEN
C           .... CONVERT ONE DEGREE TO 2.5 DEGREE
            CALL W3FT3X2P5(DD,BLOLA,CLOLA)
C           DO NN=1, 5365
C              FLD(NN) = CLOLA(NN)
C           ENDDO
            DO NN=5221, 10585
               FLD(NN-5220) = CLOLA(NN)
            ENDDO
C           print *,' successfully get avpolar field from 1'
C           .... FOR N.H. ONLY
         ELSE IF (ITYPE .EQ. IG08) THEN
c           define 116x44 grid
            call makgds(ig08,kgdso,gdso,lengds,iret)
            if(iret.ne.0) call errexit(iret)   ! kumar modified for ibm RS/6000 the call to exit
C           kgdso(4)=-20826! fix w3fi71 error  ! errexit(iret) is the ibm equiv of exit(iret)
C
               print *,'kgdso=',kgdso
               print *,'kgdsi=',kgdsi
               ipopt=0
               ip = 0
               call ipolates(ip,ipopt,kgdsi,kgdso,ji,jo08,1,
     1                 ibi,kbms,cc,ko,rlat_08,rlon_08,
     2                 ibo,lo_08,fld,iret)
               if(iret.ne.0) call errexit(iret)
C
         ELSE IF (ITYPE .EQ.26) THEN
c           define 53x45 grid
            PRINT *, ' CONVERT ONE-DEGREE TO LFM'
            call makgds(ig26,kgdso,gdso,lengds,iret)
            if(iret.ne.0) PRINT *,' IRET=',IRET
C
            print *,'kgdso=',kgdso
            print *,'kgdsi=',kgdsi
            ipopt=0
            ip = 0
C           call ipolates(ip,ipopt,kgdsi,kgdso,ji,jo,1,
C    1              ibi,kbms,cc,ko,rlat,rlon,ibo,lo,fld,iret)
            call ipolates(ip,ipopt,kgdsi,kgdso,ji,jo27,1,
     1                 ibi,kbms,cc,ko,rlat_27,rlon_27,
     2                 ibo,lo_27,fld,iret)

            if(iret.ne.0) PRINT *,' IRET=',IRET
C
         ELSE IF (ITYPE .EQ. IG27) THEN
c           define 65x65 grid
            call makgds(ig27,kgdso,gdso,lengds,iret)
            if(iret.ne.0) call errexit(iret)
            kgdso(4)=-20826! fix w3fi71 error
C
            IF (UORFG .AND. UXPO.EQ.65 .AND. UYPO.EQ.65) THEN
C              ... GET 129*129
               kgdso(2) = 129
               kgdso(3) = 129
               kgdso(8) = kgdso(8)/2
               kgdso(9) = kgdso(9)/2
               print *,'kgdso=',kgdso
               print *,'kgdsi=',kgdsi
               call ipolates(ip,ipopt,kgdsi,kgdso,ji,jo27_129,1,
     1               ibi,kbms,cc,ko,rlat_27_129,rlon_27_129,
     2               ibo,lo_27_129, fld,iret)
               if(iret.ne.0) call errexit(iret)
C              CALL W3FM08(b129,fld,129,129)
            ELSE IF (.NOT. UORFG) THEN
C              ... GET 65*65
               print *,'kgdso=',kgdso
               print *,'kgdsi=',kgdsi
               ipopt=0
               ip = 0
               call ipolates(ip,ipopt,kgdsi,kgdso,ji,jo27,1,
     1                 ibi,kbms,cc,ko,rlat_27,rlon_27,
     2                 ibo,lo_27,fld,iret)
               if(iret.ne.0) call errexit(iret)
             ENDIF
C
C
         ELSE IF (ITYPE .EQ. IG28) THEN
c           define 65x65 grid
            call makgds(ig28,kgdso,gdso,lengds,iret)
            if(iret.ne.0) call errexit(iret)
            kgdso(4)=20826   ! fix w3fi71 error
C
            IF (UORFG .AND. UXPO.EQ.65 .AND. UYPO.EQ.65) THEN
C              ... GET 129*129
               kgdso(2) = 129
               kgdso(3) = 129
               kgdso(8) = kgdso(8)/2
               kgdso(9) = kgdso(9)/2
               print *,'kgdso=',kgdso
               print *,'kgdsi=',kgdsi
               call ipolates(ip,ipopt,kgdsi,kgdso,ji,jo27_129,1,
     1               ibi,kbms,cc,ko,rlat_27_129,rlon_27_129,
     2               ibo,lo_27_129, fld,iret)
               if(iret.ne.0) call errexit(iret)
C              CALL W3FM08(b129,fld,129,129)
            ELSE IF (.NOT. UORFG) THEN
C              ... GET 65*65
               print *,'kgdso=',kgdso
               print *,'kgdsi=',kgdsi
               ipopt=0
               ip = 0
               call ipolates(ip,ipopt,kgdsi,kgdso,ji,jo27,1,
     1                 ibi,kbms,cc,ko,rlat_27,rlon_27,
     2                 ibo,lo_27,fld,iret)
               if(iret.ne.0) call errexit(iret)
             ENDIF
C
         ELSE IF (ITYPE .EQ.29) THEN
C           .... CONVERT ONE DEGREE TO 2.5 DEGREE
            CALL W3FT3X2P5(DD,BLOLA,CLOLA)
            DO NN=5221, 10585
               FLD(NN-5220) = CLOLA(NN)
            ENDDO
            print *,' successfully get a nh 2.5 degree field.'
C           .... FOR N.H. ONLY
         ELSE IF (ITYPE .EQ.30) THEN
C           .... CONVERT ONE DEGREE TO 2.5 DEGREE
            CALL W3FT3X2P5(DD,BLOLA,CLOLA)
            DO NN=1, 5365
               FLD(NN) = CLOLA(NN)
            ENDDO
            print *,' successfully get a sh 2.5 degree field.'
C           .... FOR S.H. ONLY
         ELSE IF (ITYPE .EQ. IG53) THEN
c           define 117x51 grid
            call makgds(ig53,kgdso,gdso,lengds,iret)
            if(iret.ne.0) call errexit(iret)
C           kgdso(4)=-20826! fix w3fi71 error
C
            print *,'kgdso=',kgdso
            print *,'kgdsi=',kgdsi
            ipopt=0
            ip = 0
            call ipolates(ip,ipopt,kgdsi,kgdso,ji,jo53,1,
     1                 ibi,kbms,cc,ko,rlat_53,rlon_53,
     2                 ibo,lo_53,fld,iret)
            if(iret.ne.0) call errexit(iret)
C
         ELSE
            PRINT *, ' ** UNRECOGNIZED DATA TYPE -- ', ITYPE
            IERR = 10
            GO TO 999
         ENDIF
C
      ELSE IF (KPDS(3) .EQ. 2) THEN
C        PRINT *, ' GET A 2.5 DEGREE GRIB'
         DO N1=1,73
            DO M1=1,144
               DD2(M1,73-N1+1) = CC2(M1,N1)
            ENDDO
         ENDDO
C
         DO N1 = 1, 73
            DD2(145,N1) = DD2(1,N1)
         ENDDO
C
         IF (ITYPE .EQ. AVTYPE) THEN
C           print *, '  convert from 2.5-degree to 5 degree ...'
            LL1 = 0
            DO L1 = 1, 145, 2
               LL1 = LL1 + 1
               II1 = 0
               DO I1 = 1, 73, 2
                  II1 = II1 + 1
                  GG(LL1,II1) = DD2(L1,I1)
               ENDDO
            ENDDO
C           ... LOAD IT TO OUTPUT BUFFER
            DO III=1,2701
               FLD(III) = GG2(III)
            ENDDO
         ELSE IF (FLAG .EQ. AVPOLRV) THEN
            DO N1=37, 73
               DO M1=1,145
                  DD2N(M1,N1-36) = DD2(M1,N1)
               ENDDO
            ENDDO
C           .... LOAD NH 2.5 DEGREE
            DO NN=1, 5365
               FLD(NN) = DD2N1(NN)
            ENDDO
C           print *,' successfully get avpolar field from 2.5.'
         ELSE IF (ITYPE .EQ. 27) THEN
            DO N1=37, 73
               DO M1=1,145
                  DD2N(M1,N1-36) = DD2(M1,N1)
               ENDDO
            ENDDO
C           ..... N.H.
            CALL W3FT05V(DD2N,EE,1)
C           .... convert 2.5 degree to office 84 65*65
C           CALL W3FM08(EE,FF,65,65)
C           ... smoother.....
            DO III=1,4225
               FLD(III) = EE(III)
            ENDDO
         ELSE IF (ITYPE.EQ.28) THEN
            DO N1=1,37
               DO M1=1,145
                  DD2N(M1,N1) = DD2(M1,N1)
               ENDDO
            ENDDO
C           ..... S.H.
            CALL W3FT05V(DD2N,EE,1)
C           .... convert 2.5 degree to office 84 65*65
C           CALL W3FM08(EE,FF,65,65)
C           ... smoother.....
            DO III=1,4225
               FLD(III) = EE(III)
            ENDDO
         ELSE IF (ITYPE .EQ. 29) THEN
            DO N1=37, 73
               DO M1=1,145
                  DD2N(M1,N1-36) = DD2(M1,N1)
               ENDDO
            ENDDO
C           .... LOAD NH 2.5 DEGREE
            DO NN=1, 5365
               FLD(NN) = DD2N1(NN)
            ENDDO
            print *,' successfully get a nh 2.5 grid from 2.5.'
         ELSE IF (ITYPE .EQ. 30) THEN
            DO N1=1, 37
               DO M1=1,145
                  DD2N(M1,N1) = DD2(M1,N1)
               ENDDO
            ENDDO
C           .... LOAD SH 2.5 DEGREE
            DO NN=1, 5365
               FLD(NN) = DD2N1(NN)
            ENDDO
            print *,' successfully get a sh 2.5 grid from 2.5.'
         ELSE
            PRINT *, ' ** UNRECOGNIZED DATA TYPE -- ', ITYPE
            IERR = 10
            GO TO 999
         ENDIF
C
      ELSE IF (KPDS(3) .EQ.08 .AND. ITYPE .EQ. 08) THEN
          DO III=1, 5104
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ.27 .AND. ITYPE .EQ. 27) THEN
          DO III=1, 4225
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ.28 .AND. ITYPE .EQ. 28) THEN
          DO III=1, 4225
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ. 5 .AND. ITYPE .EQ.  5) THEN
          DO III=1, 3021
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ. 6 .AND. ITYPE .EQ. 26) THEN
          DO III=1, 2385
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ. 26 .AND. ITYPE .EQ. 26) THEN
          DO III=1, 2385
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ.29 .AND. ITYPE .EQ. 29) THEN
          DO III=1, 5365
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ.30 .AND. ITYPE .EQ. 30) THEN
          DO III=1, 5365
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ.53 .AND. ITYPE .EQ. 53) THEN
          DO III=1, 5967
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ. 101 .AND. ITYPE .EQ. 101) THEN
          DO III=1, 10283
             FLD(III) = C(III)
          ENDDO
      ELSE IF (KPDS(3) .EQ. 104 .AND. ITYPE .EQ. 104) THEN
          DO III=1, 16170
             FLD(III) = C(III)
          ENDDO
      ELSE
           PRINT *,' GRID ID TYPE =', KPDS(3)
           PRINT *,' **ABEND, CANNOT PROCESS THIS GRID TYPE**'
           IERR = 7
           GO TO 999
      ENDIF
C
C     write(*,810)(buff(i),i=1,1440)
C810  format( 180(8(f10.3,2x),/))
C
      IF (IMODEL .EQ. MSKTRAJ) THEN
              PDSL(6) = CHAR(18)
              IF (PDSL(9).EQ.CHAR(152)) THEN
                  PDSL(9)=CHAR(17)
              ELSE IF (PDSL(9).EQ.CHAR(148)) THEN
                  PDSL(9)=CHAR(11)
              ELSE IF (PDSL(9).EQ.CHAR(182)) THEN
                  PDSL(9)=CHAR(133)
                  PDSL(10)=CHAR(101)
                  PDSL(11)=CHAR(70)
                  PDSL(12)=CHAR(85)
              ELSE IF (PDSL(9).EQ.CHAR(183)) THEN
                  PDSL(9)=CHAR(133)
              ENDIF
              CALL W3FP11 (IGRIB,JPDS,MTITLE,IRET)
              IRET = 0
C
      ELSE IF (FLAG .EQ. GFS97HC .OR. FLAG.EQ.GFS98TK) THEN
                  PDSL(9)=CHAR(07)
                  PDSL(5)=CHAR(08)
              CALL W3FP11 (IGRIB,IGRIB(2),MTITLE,IRET)
      ELSE IF (IMODEL .EQ. MSKTHUND) THEN
                  PDSL(5)=CHAR(08)
                  PDSL(9)=CHAR(07)
                  PDSL(8) = CHAR(128)
              CALL W3FP11 (IGRIB,IGRIB(2),MTITLE,IRET)
C
      ELSE IF (IMODEL .EQ. MSKTRAJ) THEN
                  PDSL(9)=CHAR(11)
                  PDSL(5)=CHAR(08)
              CALL W3FP11 (IGRIB,IGRIB(2),MTITLE,IRET)
      ELSE
              CALL W3FP11 (IGRIB,IGRIB(2),MTITLE,IRET)
C     .... to get a map title
      ENDIF
      IF (IRET .NE. 0) THEN
         PRINT *, ' ** NOT A FATAL ERROR FROM W3FP11 -- ', IRET
         IERR = 101
      ENDIF
C
C     print *,' '
C     print *,title(1:86)
C     print *,' '
C
         CGRIB(1:1) = CHAR(71)
         CGRIB(2:2) = CHAR(82)
         CGRIB(3:3) = CHAR(73)
         CGRIB(4:4) = CHAR(66)
         CGRIB(5:5) = CHAR(0)
         CGRIB(6:6) = CHAR(0)
         CGRIB(7:7) = CHAR(0)
         CGRIB(8:8) = CHAR(1)
C
C        PDS(7) = CHAR(27)
         IF (ITYPE.EQ.26) ITYPE=6
         GRIB(15) = CHAR(ITYPE)
C
         CALL W3FP13(CGRIB,JPDS,LIDREC,IRET)
         CALL W3FI69(JPDS, IFWPDS)
         KCENT=IFWPDS(23)

         IF (IRET .NE. 0) THEN
            PRINT *,' ERROR RETURN FROM W3FP13:', IRET
            IERR = IRET
         ELSE
            CALL GBYTES(LIDREC,LABEL,0,32,0,12)
            LABEL(1) = IOR(IAND(MSK1,LABEL(1)),ITAU)
            print *,' label after ='
            write (*,224)(label(k),k=1,12)
         ENDIF
C
C
  999 CONTINUE
      RETURN
      END
