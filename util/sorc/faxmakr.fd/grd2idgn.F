      SUBROUTINE GRD2IDGN(DGNSED,KEYDAT,
     1                    LUGRB,LUGRBIX,OUTDGN,
     2                    INBYTE,INTEXT,GRIDT1,
     3                    KDATE,IEXIT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GRD2IGN     GENERATE AN INTERGRAPH DESIGN FORMAT FILE
C   PRGMMR: KRISHNA KUMAR      ORG: W/NP12    DATE: 1999-08-01
C
C ABSTRACT: GENERATE AN INTERGRAPH DESIGN FORMATED INTERMEDIATE
C   METAFILE THAT WILL BE MADE INTO TO AN AFOS UGF MAP OR A FAX MAP.
C
C
C PROGRAM HISTORY LOG:
C   94-05-26  ORGIONAL AUTHOR LUKE LIN
C   94-06-17  HENRICHSEN    REORDER ARGS AND ADD NEW ARGS, DOCUMENT FOR
C                           PRODUCTION
C   94-06-22  HENRICHSEN    ADD LOGIC FOR PRODUCING GULF TITLE WITH
C                           NEXT LINE LOGIC INBEDDED IN TEXT.
C   94-08-15  HENRICHSEN    CORRECT AN ERROR THAT CAUSED THE CONTOR
C                           LABELS TO BE MISSING ON THE NORTH AMERICAN
C                           MAPS.
C   94-08-31  HENRICHSEN    MODIFY TO ALOW GRID TYPE 05 LFM ANALYSIS
C   94-09-12  HENRICHSEN    ALOW AND EXTRA TITLE LINE AFTER THE VALID
C                           PART OF THE TITLE LABEL.
C   94-09-20  HENRICHSEN    CHANGED LOGIC TO OUTPUT MAP TITLE BEFORE
C                           CONTOUR LABELS.
C   94-09-28  HENRICHSEN    ADDED NEW ARG JFID TO CALL TO FRMFAXT
C                           AND IN COMMOM /LLABFX/
C   94-12-20  LUKE LIN      CONVERT IT TO CFT-77.
C   95-01-18  LUKE LIN      CHANGE Z3 FORMAT TO A1 FORMAT
C   95-02-14  HENRICHSEN    ADDED IEXIT ARG TO SUB TWOFLD AND ADDED LOGIC
C                           TO MAKE VORTICITY FROM U AND V FIELDS.
C                           ADDED NEW VAR (XMESHL) TO COMMON /POLE/
C                           CHANGED NAME OF COMMON/ISPACE/ TO /KSPACE/
C                           AND RENAMED BUFFV TO SAV1ST
C                           AND BUFFU TO SAV2ND
C  96-02-01   LUKE LIN      CHANGE TO CALL GETFLD/IW3GET
C  96-06-03   HENRICHSEN    ADD PRINT STATEMENTS AROUND CALL TO GULFTL.
C                           ADD LOGIC TO USE JFID PASSED IN COMMON LLABFX.
C  96-06-07   HENRICHSEN    ADD LOGIC TO BYPASS CALLS TO GULFTL AND
C                           FRMFAXT FRMFAXT ON 2ND OR 3RD CALL TO
C                           GRD2IDGN. ADDED LOGIC TO USE CENTERED ON 
C                           TIME FOR 5 DAU MEAN MAPS.
C  96-06-17   LUKE LIN      MODIFY FOR PRECIP BECAUSE FLD2 COMES FROM DIFFERENT
C                           FORECAST HOUR.
C  96-06-20   HENRICHSEN    ADD LOGIC TO GET FIELD LEVEL FOR CERTAIN TYPE
C                           MAP SO THAT THE GULF TITLE WILL HAVE 4 LINES. 
C  96-06-21   HENRICHSEN    MODIFY COMMON /LLABFX/ TO PASS TITLE "THEIGHT"
C                           HGTFLG.
C  96-06-24   LUKE LIN      ADD CNTR OPTIONS
C  96-07-01   LUKE LIN      ADD FAX STRIP TITLES.
C  96-07-09   LUKE LIN      CHANGE THE UNPACK GRID BUFF SIZE FROM 16380 TO 16900
C                           AND CHANGE GFS FROM 65*65 TO 129*129
C  96-08-12   LUKE LIN      ADD SMOOTHER FOR CONTOUR LABELS AND CENTERS.
C  96-08-29   LUKE LIN      MODIFY FOR NH2005 ON GFS GRID
C  96-09-11   LUKE LIN      MODIFY FOR NH2005 ON NGM CGRID
C  96-01-03   LUKE LIN      MODIFY FOR NH4006 AND ROTATE THE INPUT GRID.
C  96-05-29   LUKE LIN      MODIFY FOR SFC PRESSURE CHANGE
C  97-07-11   LUKE LIN      MODIFY FOR SETTING UNDEFINED ON GRID TYPE 5, TDL MOS
C                           THUNDERSTORM/SEVERE WX, AND TRAJECTORY 4-PANEL.
C  97-12-10   C. CARUSO     WHERE CHECK IS DONE TO SEE IF KEYIDX = 11 (FOR WIND
C                           PLOT), ADDED CHECK TO SEE IF KEYIDX = 17 (NEW VALUE
C                           FOR SFC GEOSTROPHIC WIND).
C 1999-08-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM
C                           RS/6000. ASSIGNED PROPER VALUE TO XINDF
C                           USING RANGE FUNCTION FOR IBM RS/6000 FOR
C                           COMPILE OPTIONS xlf -qintsize=8 -qrealsize=8
C   99-09-27  BILL FACEY    MODIFIED CODE FOR CASE WHERE KEYIDX GT 50.
C 
C USAGE:    CALL GRD2IDGN(DGNSED,KEYDAT,
C                         LUGRB,LUGRBIX,OUTDGN,
C    1                    INBYTE,INTEXT,GRIDT1,
C    2                    KDATE,IEXIT)
C   INPUT ARGUMENT LIST:
C     IDFLD    - INTEGER*4 12 WORD ARRAY THAT CONTAINS THE IDS FOR
C              - GETTING THE PACKED DATA FIELD(S). THE FIRST 6 WORDS
C              - CONTAIN THE IDS FOR GETTING THE FIRST DATA FIELD.
C              - WHILE WORDS 6-12 CONTAIN THE THE IDS FOR GETTING THE
C              - 2ND DATA FIELD.
C     DGNSED   - INTEGER*4 UNIT NUMBER OF DESIGN SEED FILE.
C     KEYDAT   - INTEGER*4 NUMBER DESCRIBING DATA TYPE.
C     LUGRB    - UNIT NO POINTS TO ONE-DEGREE GRIB FILE
C     LUGRBIX  - UNIT NO POINTS TO ONE-DEGREE GRIB INDEX FILE.
C     OUTDGN   - INTEGER*4 UNIT NUMBER OF OUTPUT DESIGN FILE.
C     INBYTE   - INTEGER*4 NUMBER OF CHARACTERS IN MAP TITLE (INTEXT).
C     INTEXT   - CHARRACTER*80 CONTAINS MAP TITLE.
C     GRIDT1   - REAL NUMBER DESCRIBING THE GRID INTERVAL.
C
C   OUTPUT ARGUMENT LIST:
C     KDATE    - INTEGER*4 2 WORD ARRAY THAT HOLD THE BASE YEAR, MONTH,
C              - DAY AND HOUR IN WORD 1 AND THE VALID YEAR, MONTH, DAY
C              - AND HOUR IN WORD 2 BOTH ARE PACKED IN O.N. 84 FORMATED
C              - ID WORDS. (FORMATED LIKE ID(7) WORD).
C     IEXIT    - EXIT STATUS
C              -= 0 SUCCESSFUL RUN
C              -= 1 ABEND FROM GETFLD/W3FK43, GRID FIELD NOT AVAILABLE
C              -= 2 ABEND FROM CNTOR - LACK OF WORKING BIN FOR
C              -    INTEPOLATION.
C              -= 3 ABEND FROM SUBR/SUBFLD
C              -= 4 OUTPUT BUFFER IS FULL
C              -= 5 FATAL ERR -- NOT ENOUGH WORKING BUFFER FOR OUT-DGN
C              -= 6 UNABLE TO OPEN AN OUTPUT DESIGN FILE.
C              -= 7 ERROR WHILE WRITING OUT ONTO AN OUTPUT DESIGN FILE.
C              -= 8 ERROR OPENING INPUT DESIGN FILE
C              -= 9 ERROR WHILE SCANING INPUT DESIGN FILE
C                ......... FATAL ERROR ABOVE .............
C              -= 910, NONFATAL ERR -- INVALID OPEN TYPE FOR INDFPI
C              -= 933, NONFATAL ERR -- ATTEMP TO ADD BAD ELEMENT TO DGN
C
C
C   INPUT FILES:
C     FTXXF001 - HOLDS A DESIGN SEED FILE.  XX HAS VALUE OF DGNSED.
C
C   OUTPUT FILES:
C     OUTDGN   - WILL CONTAIN THE DESIGN FORMATTED INTERMEDIATE
C              - METAFILE THAT WILL BE CONVERTED INTO AFOS UGF
C              - FORMAT BY THE NEXT SET OF SUBS.
C     FT06F001 - CHECK OUT STATEMENTS AND ERROR MESSAGES
C
C
C REMARKS:
C          IF PLOTFG = ' ', PLOT THE FIELD INSTEAD CONTOUR.
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
      COMMON / ILCON / MAP(15)
      INTEGER       MAP
C
C
       integer       iacc
       character*8   cacc
       equivalence  (iacc,cacc)
       character*6   bgname
C
      COMMON / CLRFLD / CLRLAT,CLRLOL,CLRUPR,LCLEAR,ECLEAR,NCLEAR
C
      COMMON / DGNBIN / VECBIN,MAXBIN,IMDEX,ITOTWD
C
      COMMON /DGNTX/ ZLOWTXT,ZHGHTXT
      INTEGER        ZLOWTXT,ZHGHTXT
C     .....          ZLOWTXT AND ZHGHTXT ARE INTEGERS TO BE STORE IN
C          Z-COORDINATES FOR SPECIFYING TEXT STRINGS CHARACTERISTICS.
C          LOW TWO DIGITS FOR CHARACTER FONTS, 100 OVER FOR BIG H/L,
C          1000 OVER FOR ERASE, 10000 OVER FOR REVERSE BLOCK, ......
C
      COMMON /ICON/ UA1(50),UA2(50),UA3(50),UM1(50),UM2(50),UM3(50),
     1              KTYPE(50)
C
      COMMON /ISPACE/SBFLD1(16900),SBFLD2(16900),IDREC(12),BUFF(16900),
     &               SAV1ST(16900),SAV2ND(16900)
      REAL   SBFLD1, SBFLD2, BUFF, SAV1ST, SAV2ND
      REAL   SBFLDX(16900)
      REAL   SMOFLD(16900)
C
C
C     ... 12 ID WORDS UNPACKED FROM 84'S DATA FILE; BUFF CONTAINS
C         UNPACKED DATA FOR THAT PARTICULAR FIELD;
C
C

      COMMON /LABG/ GULPXX(2),GULPXY(2),LVFLG,NUMG,GLAB
C
      INTEGER       GULPXX,GULPXY,LVFLG,NUMG
      CHARACTER*24  GLAB
C
      COMMON /LLABFX/ TITPXX,TITPXY,HGTFLG,THEIGHT,NUMT,JBYT,
     1              TITLE,JFID,EXVALID              
C
      INTEGER       TITPXX,TITPXY,HGTFLG,NUMT,JBYT
      REAL          THEIGHT
      CHARACTER*152 TITLE
      CHARACTER*48  JFID
      CHARACTER*32  EXVALID

      
      COMMON /MEANCON/ MEANFG,LKMEAN,MEANTYP
C
     
      COMMON /MCON/ IADDB,IADDTAU,NUMTYP,NUMKEY,
     1              MTAU(18),MEAN(18),LMEAN,KDN,LDN
C
      COMMON /MUTCON/ KEYIDX,UA1V,UA2V,UA3V,UM1V,UM2V,UM3V,LINEVU,LINEP,
     X               IGRIDP,T1
C
C     ...KEY TYPE INDEX, CONSTANT, AND CONTOUR LINE VALUE
C
      COMMON  /FLDCON/ IDFLD(16),NUMFLD,NDEGNS
C
      COMMON  /POLE/ XPOL,YPOL,GDTYPE
ckumar
      COMMON /POLENEW/ XMESHL
ckumar
C
C     ...THE POLE POSITION IN GRID(65,65) IS AT GRID(33,33).
C     ... GRID MESH LENGHT = XMESHL = 381.0 KLM
C     ...THE POLE POSITION IN GRID(53,45) IS AT GRID(27,49).
C     ... GRID MESH LENGHT = XMESHL = 190.5 KLM
C     ...THE POLE POSITION IN GRID(53,57) IS AT GRID(27,49).
C     ... GRID MESH LENGHT = XMESHL = 190.5 KLM
C
C
      COMMON   /THINN / NOTHIN,MIDIST,MINPTS
C
C     ... NOTHIN: NO OF TIME TO BE THINNED; MIDIST: MIN. DISTANCE
C     ... BETWEEN TWO PTS;  MINPTS: MIN. PTS TO BE PROCESS.
      COMMON /ID6WDS/ID8FLD(8),FLNAME,IOPEN
             CHARACTER*8  FLNAME
C     .... 6 ID WORDS TO GET DESIRED FIELD IN INVENTORY
C     .... INPUT INVENTORY FILE NAME
C
      COMMON/HDCONS/ITAU,IDUMMY(3)
C     ..... ITAU COMES FROM SUBR RD1GRID.F
C
C
      COMMON /GRB2/ ALUGRB,ALUGRBIX,AFCSTHR,AKEYIDX,CALFLAG,GRB2FLAG
C
      INTEGER       ALUGRB
      INTEGER       ALUGRBIX 
      INTEGER       AFCSTHR
      INTEGER       AKEYIDX
      INTEGER       CALFLAG
      LOGICAL       GRB2FLAG
C     .... FOR 2ND GRB UNIT NUMBER SUCH AS PRECIP FROM DIFFERENT FCST HOUR
C
C
C      .... THIS SET IS FOR UOR_DOT
       COMMON /UOR2D/ UGRIDT1,UXPO,UYPO,UXADJUS,UYADJUS,UCU2GI,UORFG,
     1                IP,IPOPT
       REAL     UGRIDT1,UXPO,UYPO,UXADJUS,UYADJUS,UCU2GI
       LOGICAL  UORFG
C
C     ... FOR CONTOR OPTIONS
      COMMON /CNTROP/ NDVD, NDIV, NCNTRFILT, NCENTFILT,SHADIV
      INTEGER SHADIV
C     ... NDVD FOR INTERPOLATION, NDIV FOR DIVISION
C     ... NCNTRFILT FOR CONTOUR FILTER, NCENTFILT FOR CENTER FILTER
      COMMON/LABARG/IPOS(500),JPOS(500),TEXT1(500),
     X   VAL(500),NCH(500),NSAVED
      CHARACTER*4 TEXT1,TEXTIN
C
C
      COMMON /PLOT/   PLOTFG, IMAXP, JMAXP, I1BIG, J1BIG, IGPLOT(2),
     1                ISTART, JSTART
      LOGICAL         PLOTFG
C
C
      CHARACTER*80 INTEXT
      CHARACTER*77 LGULF
      CHARACTER*60  BLANKS
      CHARACTER*60 ERRMSG
      
      CHARACTER*40  CELLIB     
C     
      CHARACTER*20 VERSON
      CHARACTER*12 CITIMES
ckumar
      integer iitimes
      equivalence (iitimes,citimes)
ckumar
C     .... INPUT FILE NAME
      CHARACTER*8  CHWORK
      CHARACTER*8  CWORK
      CHARACTER*4  CHTYPE
      CHARACTER*4  KTYPE
      CHARACTER*4  MEAN
      CHARACTER*4  MEANTYP

C
      CHARACTER*1  NULL
      CHARACTER*1  WBLANK
      CHARACTER*1  WBARB
      CHARACTER*1  WISOTA
C
      INTEGER   IBIGG, JBIGG, ILMAX, JLMAX
      INTEGER   CLRLOL(2)
      INTEGER   CLRUPR(2)
      INTEGER   DGNSED
      INTEGER   GDTYPE
      INTEGER   INBYTE
      INTEGER   IDFLDW(8)
      INTEGER   IDFLD1(12)
      INTEGER   IDFLD2(12)
C
C     .... 6 ID WORDS TO GET DESIRED PACKED DATA FIELD.
C
      COMMON /RANG/   RANGFG, ICBEG, ICEND
      LOGICAL         RANGFG
      INTEGER         ICBEG, ICEND
C
C
      COMMON /PCLP/   PCLPFG
      LOGICAL         PCLPFG
C
      INTEGER   INTIME(2)
      INTEGER   ITIMES(3)
      INTEGER   IVLDNO
      INTEGER   JTIME
      INTEGER   KTIME
      INTEGER   JDATE
      INTEGER   KDATE(2)
      REAL      RINC(5)
      INTEGER   IDAT(8), JDAT(8)
      INTEGER      CTIME
      CHARACTER*4 JCDAT, KCDAT
      EQUIVALENCE (CTIME,KCDAT)
      EQUIVALENCE (JDATE,JCDAT)
      INTEGER   NUMBYT
      INTEGER   OUTDGN
      INTEGER   SIGNAT
C
      INTEGER    FOMAT
      INTEGER    OPTYPE
      INTEGER    UNUSE1
      INTEGER    UNUSE6
      INTEGER    UNUSE8
      INTEGER    VECBIN(409800)
C
      REAL       ALEVEL
      REAL       CLRLAT
      REAL       GRIDT1
      REAL       WORKZ(324200)
      REAL       XINDF
C
      LOGICAL      HILOFG
      LOGICAL      ZS4TO1
C
      LOGICAL      AFOS
      LOGICAL      ECLEAR
      LOGICAL      KDN
      LOGICAL      LCLEAR
      LOGICAL      NCLEAR
      LOGICAL      LDN
      LOGICAL      LMEAN
      LOGICAL      LKMEAN
      LOGICAL      NEWCON
      LOGICAL      PACKED
      LOGICAL      WORKL(648400)
C
C     ... WORKZ AND WORKL ARE NEEDED BY CNTOR ....
C
C
      CHARACTER*1  IFLAG
      CHARACTER*1  IDPDSC(28)
      INTEGER      IDPDS(4)
      EQUIVALENCE  (IDPDSC(1),IDPDS(1))
      INTEGER      IMODEL
      INTEGER      IDFLDP(4)
C
C
      REAL      DSCALE
      REAL      ANGLE
      REAL      DEFALT
      LOGICAL   LINEAR
      LOGICAL   LDEFQQ
C
      DATA          BLANKS
     1/'                                                            '/
C     /'123456789012345678901234567890123456789012345678901234567890'/
      DATA         ERRMSG
     1/'NWS/GRD2IDGN - ONE INTERGRAPH DESIGN FILE UNABLE TO BE MADE'/
      DATA         WBLANK   /' '/
      DATA         WBARB    /'W'/
      DATA         WISOTA   /'I'/
      DATA         XINDF   / 1.0E307 /

      DATA        MSKTRAJ   /Z'00000000000000C0'/
C
      DATA        MSK3      /Z'000000000000FFFF'/

C
      DATA        IZERO     /Z'0000000000000000'/
      EQUIVALENCE (ALEVEL,KLEVEL)
      EQUIVALENCE (CITIMES,INTIME)
      EQUIVALENCE (JTIME,CWORK)
      EQUIVALENCE (KTIME,CHWORK)
      EQUIVALENCE (WORKZ(1),BUFF(1))
C
C      ... WHERE 409800 HALFWORDS = 400 VAX BLOCKS
C
C
C      FILL TITLE WITH BLANKS.
C
         TITLE = BLANKS//BLANKS//BLANKS
C
C???  DATA         NULL     /Z00/
       NULL = CHAR(00)
C
       MAXBIN = 409800
       NINDX = 0
       ISTOP = 0
       MAPERR = 0
       MWHICH = 1
       IEXIT = 0
       T1 = GRIDT1
       ZLOWTXT = 0
       ZHGHTXT = 0
C      MINPTS = 10
       KEYIDX = KEYDAT
       NSAVED = 0
       NCLEAR = .FALSE.
C
C       UNLOAD THE 2 SETS OF ID WORDS FROM IDFLD ARRAY
C
          DO I=1, 6
             IDFLDW(I) =  IDFLD(I)
          ENDDO
          DO I=1, 6
             IDFLD1(I) =  IDFLD(I)
             IDFLD2(I) = IDFLD(8+I)
          ENDDO
          DO I=7, 12
             IDFLD1(I) = 0
             IDFLD2(I) = 0
          ENDDO
C
         IMODEL = IAND(IDFLD1(6),MSK3)
C
C         CALL GETCON
C
C
          IF (GDTYPE.EQ.26) THEN
              IBIGG = 53
              JBIGG = 45
              ILMAX = 53
              JLMAX = 45
              ICOR = 1
              JCOR = 1
              XPOL = 27.0
              YPOL = 49.0
              KEYCLR = 12
              XMESHL = 190.5
          ELSE IF (GDTYPE.EQ.05) THEN
              IBIGG = 53
              JBIGG = 57
              ILMAX = 53
              JLMAX = 57
              ICOR = 1
              JCOR = 1
              XPOL = 27.0
              YPOL = 49.0
              KEYCLR = 12
              XMESHL = 190.5
          ELSE IF (GDTYPE.EQ.27 .OR. GDTYPE.EQ.28) THEN
              IBIGG = 65
              JBIGG = 65
              ILMAX = 65
              JLMAX = 65
              ICOR = 1
              JCOR = 1
              XPOL = 33.0
              YPOL = 33.0
              KEYCLR = 14
              XMESHL = 381.0
C
              IF (UORFG .AND. UXPO.EQ.65 .AND. UYPO.EQ.65) THEN
                 IBIGG = 129
                 JBIGG = 129
                 ILMAX = 129
                 JLMAX = 129
                 ICOR = 1
                 JCOR = 1
                 XPOL = 65.0
                 YPOL = 65.0
                 KEYCLR = 14
                 XMESHL = 190.5
                 IF (MAP(5) .GT. 4000) THEN
                     KEYCLR = 1
                     NCLEAR = .TRUE.
                 ENDIF
              ENDIF
          ELSE IF (GDTYPE.EQ.101) THEN
              IBIGG = 113
              JBIGG = 91
              ILMAX = 113
              JLMAX = 91
              ICOR = 1
              JCOR = 1
              XPOL = 58.5
              YPOL =  92.5
              KEYCLR = 12
              XMESHL = 91.452
          ELSE IF (GDTYPE.EQ.104) THEN
              IBIGG = 147
              JBIGG = 110
              ILMAX = 147
              JLMAX = 110
              ICOR = 1
              JCOR = 1
              XPOL = 75.5
              YPOL = 109.5
              KEYCLR = 12
              XMESHL = 90.75464
          ELSE
              WRITE(6,FMT='('' GRD2IDGN: **** ERROR '',I2,'' IS'',
     1        '' UNKNOWN GRID TYPE:'')')GDTYPE
              GO TO 800
          ENDIF
C      ... INITIALIZE ADDITIVE AND MULTIPLICATIVE CONSTANTS
      IFLDNO = 1
C
C
C
      CALL GETFLD(IFLDNO,IDFLD1,LUGRB,LUGRBIX,IEXIT)
C
      IF (IEXIT .NE. 0) THEN
           WRITE(6,FMT='('' GRD2IDGN: **** ERROR RETURN= '',I2,
     1        '' FROM GETFLD :'')')IEXIT
           RETURN
      ENDIF
C
C          IF NEED GET SECOND DATA FIELD
C
        IF (NUMFLD .EQ. 2) THEN
           IFLDNO = 2
C
           IF (GRB2FLAG .AND. KEYIDX.EQ.AKEYIDX) THEN 
              CALL GETFLD(IFLDNO,IDFLD2,ALUGRB,ALUGRBIX,IEXIT)
           ELSE
              CALL GETFLD(IFLDNO,IDFLD2,LUGRB,LUGRBIX,IEXIT)
C             .... FLD 2 FROM DIFFERENT FORECAST HOUR, SUCH AS PRECIP
           ENDIF
C              ...GET A REAL DATA FIELD FROM INVENTORY
           IF (IEXIT .NE. 0) THEN
             WRITE(6,FMT='('' GRD2IDGN: **** ERROR RETURN= '',I2,
     1         '' FROM GETFLD WITH :'')')IEXIT
             RETURN
           ENDIF
           IF ( .NOT. PLOTFG    ) THEN
             CALL TWOFLD(BUFF,SAV1ST,SAV2ND,IBIGG,JBIGG,KEYIDX,IEXIT)
           ENDIF
           IF (IEXIT .NE. 0) THEN
             WRITE(6,FMT='('' GRD2IDGN: **** ERROR RETURN= '',I2,
     1        '' FROM TWOFLD, WITH KEYIDX='',I3)')IEXIT,KEYIDX
             RETURN
           ENDIF
        ENDIF
C
          IF (IEXIT .NE. 0) RETURN
          IF (KEYIDX.EQ.7 .OR. KEYIDX.EQ.30 .OR. KEYIDX.EQ.37
     1        .OR. KEYIDX.EQ.19 .OR. KEYIDX.EQ.49) THEN
             CALL PRECIP(BUFF,SAV1ST,IBIGG,JBIGG)
          ELSE IF (KEYIDX .EQ. 44) THEN
C            ... FOR VERTICAL WIND SHIED
             CALL ABSVWS(BUFF,SAVIST,IBIGG,JBIGG)
          ENDIF
C
C
C
         IACC = MAP(1)
         BGNAME = CACC(1:6)
         PRINT *, ' GRD2IDGN: BGNAME =', BGNAME
         IF (GDTYPE.EQ.27 .AND. BGNAME.EQ.'NH2005') THEN
C             ...ROTATE FROM 80W TO 105W FOR NH2005
              ANGLE = -25.0
              DSCALE = 1.0
              LINEAR = .TRUE. 
              LDEFQQ = .FALSE.
              CALL W3FT07(BUFF,IBIGG,JBIGG,XPOL,YPOL,XPOL,YPOL,
     A          DSCALE,ANGLE,LINEAR,LDEFQQ,DEFALT,SMOFLD,IBIGG,JBIGG)
C
              CALL SUBFLD(SMOFLD,IBIGG,JBIGG,SBFLD1,ILMAX,JLMAX,
     1                    ICOR,JCOR,UM1V,UA1V,UM2V,UA2V,IEXIT)
         ELSE IF (GDTYPE.EQ.27 .AND. BGNAME.EQ.'NH4006') THEN
C             ...ROTATE FROM 80W TO 105W FOR NH2005
              ANGLE = -25.0
              DSCALE = 1.0
              LINEAR = .TRUE. 
              LDEFQQ = .FALSE.
              CALL W3FT07(BUFF,IBIGG,JBIGG,XPOL,YPOL,XPOL,YPOL,
     A          DSCALE,ANGLE,LINEAR,LDEFQQ,DEFALT,SMOFLD,IBIGG,JBIGG)
C
              CALL SUBFLD(SMOFLD,IBIGG,JBIGG,SBFLD1,ILMAX,JLMAX,
     1                    ICOR,JCOR,UM1V,UA1V,UM2V,UA2V,IEXIT)
         ELSE IF (GDTYPE.EQ.05 .AND. BGNAME.EQ.'PN2601') THEN
C             ...ROTATE FROM 105W TO 98W FOR NH2005
              ANGLE =  7.0
              DSCALE = 1.0
              LINEAR = .TRUE.
              LDEFQQ = .FALSE.
              CALL W3FT07(BUFF,IBIGG,JBIGG,XPOL,YPOL,XPOL,YPOL,
     A          DSCALE,ANGLE,LINEAR,LDEFQQ,DEFALT,SMOFLD,IBIGG,JBIGG)
C
              CALL SUBFLD(SMOFLD,IBIGG,JBIGG,SBFLD1,ILMAX,JLMAX,
     1                    ICOR,JCOR,UM1V,UA1V,UM2V,UA2V,IEXIT)
         ELSE
C
C            EXTRACT AND RECSALE A SUBGRID.
C
           IF ( .NOT. PLOTFG ) THEN
             PRINT *, ' GRD2IDGN: CALL SUBFLD'
             CALL SUBFLD(BUFF,IBIGG,JBIGG,SBFLD1,ILMAX,JLMAX,
     1                   ICOR,JCOR,UM1V,UA1V,UM2V,UA2V,IEXIT)
           ENDIF
         ENDIF
C
          IF (IEXIT .NE. 0) THEN
              IEXIT = 3
              RETURN
          ENDIF
C
C            NOW GET THE MAP TITLE READY
          AFOS = .FALSE.
          PACKED = .FALSE.
          ITIMES(1) = ITAU
          JTAU = ITAU
          IF(LVFLG.GT.0)THEN
C
C          GET DATA LEVEL FROM 2ND ID WORD
C         
            ILEVEL     = IDREC(2)
            WRITE(6,FMT='('' GRD2IDGN: LEVEL ID WORD='',Z17)')ILEVEL 
            CALL GBYTE(ILEVEL,KLEVEL,36,20)
            LEVEL = KLEVEL/100
            WRITE(6,FMT='('' GRD2IDGN: LEVEL='',I6,'' KLEVEL='',I6,
     1      '' KLEVEL HX='',Z17)')LEVEL,KLEVEL,KLEVEL
          ENDIF
          KTIME     = IDREC(7)
          ITIMES(2) = IDREC(7)
          ITIMES(3) = IDREC(8)
C
           CTIME = IZERO
           WRITE(6,FMT='('' GRD2IDGN: ITIMES='',3(Z17))')ITIMES
C1211     FORMAT(' KTIME,JDATE=', 3(2X,Z16))
#if defined (_LITTLE_ENDIAN)
          CALL SBYTESCCS(CTIME,KTIME,0,32,0,1)
          CALL SWAP_BYTES_IN_PLACE (CTIME, 8_8, 1_8)
#else
          CALL SBYTES(CTIME,KTIME,0,32,0,1)
#endif
C-------------------------------------------------------
C    FOLLOWING WAS ADDED TO REPLACE CALL TO W3FS15 WITH CALL
C    TO W3MOVDAT FOR Y2K COMPATABILITY.
            IDAT=0
            JDAT=0
            RINC=0
            RINC(2)=JTAU
            ICENT=1900
            IDAT(1)=mova2i(KCDAT(1:1))
            IF(IDAT(1) .LT. 96) ICENT=2000
            IDAT(1)=IDAT(1)+ICENT
            IDAT(2)=mova2i(KCDAT(2:2))
            IDAT(3)=mova2i(KCDAT(3:3))
            IDAT(5)=mova2i(KCDAT(4:4))
ckumar
            print*,'In GRD2IDGN - before W3MOVDAT '
            print *, 'IDAT=', IDAT(1), IDAT(2), IDAT(3), IDAT(5)
            print *, 'RINC=', RINC(1),RINC(2),RINC(3),RINC(4),RINC(5)
ckumar
            CALL W3MOVDAT(RINC,IDAT,JDAT)
            print *, 'JDAT=', JDAT(1), JDAT(2), JDAT(3), JDAT(5)
            JDAT(1)=JDAT(1)-ICENT 
            JCDAT(1:1)=CHAR(JDAT(1))
            JCDAT(2:2)=CHAR(JDAT(2))
            JCDAT(3:3)=CHAR(JDAT(3))
            JCDAT(4:4)=CHAR(JDAT(5))
C
C     END OF CHANGES FOR W3MOVDAT
C-----------------------------------------------------
          KDATE(1) = KTIME
          KDATE(2) = JDATE
C
           WRITE(6,FMT='('' GRD2IDGN: KDATE/JTAU='',3(Z17))')KDATE,JTAU
          print *, 'keyidx=',keyidx
          IF (KEYIDX .LT. 51) THEN
            CHTYPE    = KTYPE(KEYIDX)
          ELSE
            CHTYPE = ' '
          ENDIF
C 
ckumar          CALL SBYTES(CITIMES,ITIMES,0,32,0,3)
#if defined (_BIG_ENDIAN)
           CALL SBYTES(IITIMES,ITIMES,0,32,0,3)
#else
           CALL SBYTESCCS(IITIMES,ITIMES,0,32,0,3)
#endif
           WRITE(6,FMT='('' GRD2IDGN: INTIME='',2(Z17))')INTIME
C
C          CHECK TO SEE IF WE SHOULD BYPASS CALLS TO THESE SUBS
C
          IF (NDEGNS .EQ. 1) THEN
C         
C          CHECK TO SEE IF THIS IS A MEAN FIELD
C  
           IF(LKMEAN) THEN
             LMEAN = .FALSE.
             DO KK=1,NUMTYP
              IF(MEAN(KK).EQ.MEANTYP)THEN
               LMEAN = .TRUE.
               ITIMES(1) = MTAU(KK)
ckumar               CALL SBYTES(CITIMES,ITIMES,0,32,0,3) 
#if defined (_BIG_ENDIAN)
           CALL SBYTES(IITIMES,ITIMES,0,32,0,3)
#else
           CALL SBYTESCCS(IITIMES,ITIMES,0,32,0,3)
#endif
           WRITE(6,FMT='('' GRD2IDGN: INTIME='',2(Z17))')INTIME
C
           WRITE(6,FMT='('' GRD2IDGN: THIS IS A '',A,'' MEAN MAP.'',
     1        '' THE NEW TAU IS'',I4,'' ITIMES='',3(Z17))')MEANTYP,
     2        MTAU(KK),ITIMES
              GOTO 10
C
              ENDIF
             ENDDO
           ENDIF
  10      CONTINUE
#if defined (_LITTLE_ENDIAN)
          CALL SWAP_BYTES_IN_PLACE (INTIME, 8_8, 2_8)
#endif
          CALL GULFTL(CITIMES,CHTYPE,PACKED,AFOS,LGULF)
C
           WRITE(6,FMT='('' GRD2IDGN: BEFORE CALL TO SUB FRMFAXT'',
     1         '' JFID='',A)')JFID(1:JBYT)
          CALL FRMFAXT(LGULF,INBYTE,LEVEL,LMEAN,INTEXT)                
C
C
C
               WRITE(6,FMT='('' GRD2IDGN: TITLE='',A,/,'' HEX='',
     1          8(/,20A1))')TITLE(1:NUMT),(TITLE(IJ:IJ),IJ=1,NUMT)
C
               WRITE(6,FMT='('' GRD2IDGN: GLAB='',A,/,'' HEX='',
     1            17A1)')GLAB(1:NUMG),(GLAB(IJ:IJ),IJ=1,NUMG)
C
C              ....PUT TITLES ON THE MAP
C
               CALL MTITLE(IDFLD1)
C
C              ....PUT FAX STRIP TITLES 
C
               CALL STRPTITL(CITIMES,PACKED)
C
           ENDIF
C
           FOMAT = 0
           OPTYPE = 0
           CELLIB(1:1) = NULL
           CALL INDFPI(UNUSE1,DGNSED,OPTYPE, CELLIB, FOMAT,
     &               UNUSE6, IEXIT, UNUSE8)
C         ..... OPEN A DGN FILE .....
           IF (IEXIT .NE. 0  .AND. IEXIT .LE. 10) THEN
                 PRINT *,' FATAL ERROR FROM INDFPI=', IEXIT
                 RETURN
           ENDIF
C                 INITIALIZATION
           IMZ = (ILMAX-1)*NDIV + 1
           IML = 2*IMZ-1
C
           IF ( .NOT. PLOTFG    ) THEN
              CALL UPDOWN(SBFLD1,SBFLD2,ILMAX,JLMAX)
C            ... UPSIDE DOWN THE GRID DATA FOR CNTOR
           ENDIF
C
           ITOT=ILMAX * JLMAX
ckumar
           PRINT *, 'In GRD2IDGN: filter and contour'
           PRINT *, 'GDTYPE, NCNTRFILT,NCENTFILT', 
     &      GDTYPE, NCNTRFILT,NCENTFILT 
ckumar
           IF (GDTYPE.EQ.27 .OR. GDTYPE.EQ.28) THEN
C             FILTER THE CONTOUR
              IF ( NCNTRFILT .GT. 0) THEN
                 DO KL=1,NCNTRFILT
                    CALL W3FM08(SBFLD2,SBFLDX,ILMAX,JLMAX)
                 ENDDO
              ENDIF
C             FILTER THE CENTERS
          PRINT *, 'GDTYPE, NCNTRFILT,NCENTFILT',
     &              GDTYPE, NCNTRFILT,NCENTFILT
                 IF ( NCENTFILT .GT. 0) THEN
                    DO KL=1,NCENTFILT
                      CALL W3FM08(SBFLD1,SBFLDX,ILMAX,JLMAX)
                    ENDDO
                 ENDIF
           ELSE IF (KEYIDX.EQ.22) THEN
C             FILTER THE CONTOUR FOR PRESSURE CHANGE
              IF ( NCNTRFILT .GT. 0) THEN
                 DO KL=1,NCNTRFILT
                    CALL W3FM08(SBFLD2,SBFLD2,ILMAX,JLMAX)
                 ENDDO
              ENDIF
C             FILTER THE CENTERS
              IF ( NCENTFILT .GT. 0) THEN
                 CALL W3FM08(SBFLD1,SBFLD1,ILMAX,JLMAX)
                 IF ( NCENTFILT .GT. 1) THEN
                    DO KL=1,NCENTFILT-1
                       CALL W3FM08(SBFLD1,SBFLD1,ILMAX,JLMAX)
                    ENDDO
                 ENDIF
              ENDIF
           ENDIF
C
C
           IF ( .NOT. PLOTFG    ) THEN
             print *, 'call cntor'
                CALL CNTOR(SBFLD2,ILMAX,JLMAX,NDVD,NDIV,WORKZ,
     &               IMZ,WORKL,IML,SHADIV,KEYIDX,IEXIT)
           ENDIF
           IF (IEXIT .NE. 0 .AND. IEXIT .LE. 5) RETURN
C
           IF (GDTYPE.EQ.27 .OR. GDTYPE.EQ.28) THEN
              ISML = 46
              JSML = 51
              ISKP = 10
              JSKP = 8
              ICOR = 13
C
              IF (UORFG .AND. UXPO.EQ.65 .AND. UYPO.EQ.65) THEN
                 ISML = 91
                 JSML = 101
                 ISKP = 20
                 JSKP = 16
                 ICOR = 25
              ENDIF
C
C            CHECK TO SEE IF THE FEILD NEEDS TO BE CLEARED
C
              IF(LCLEAR)THEN
                 WRITE(6,FMT='('' GRD2IDGN: CLEAR FIELD OF SIZE'',
     1                2I6)')ILMAX,JLMAX
                 CALL CLEAR(CLRLAT,XINDF,SBFLD1,
     1                      ILMAX,JLMAX,KEYCLR,IRTN)
                 PRINT *,' CALL CLEAR'
              ELSE
               WRITE(6,FMT='('' GRD2IDGN: INDEF FIELD OF SIZE'',
     1            2I6,'' ISML/JSML'',2I6,'' ISKP/JSKP='',2I6,
     2                '' ICOR='',I6)')
     3           ILMAX,JLMAX,ISML,JSML,ISKP,JSKP,ICOR
                 CALL INDEFF(SBFLD1,ILMAX,JLMAX,ISML,JSML,
     1                  ISKP,JSKP,ICOR,XINDF)
                 PRINT *,' CALL INDEFF,XINDF=',XINDF
              ENDIF
           ELSE
              IF(ECLEAR)THEN
                   WRITE(6,FMT='('' GRD2IDGN: CLEARING FIELD OF'',
     1              '' SIZE '',2I6,'' WITH CORNER POINTS '',4I4)')
     2              ILMAX,JLMAX,CLRLOL,CLRUPR
                    MKEY   =  1
                    CALL CLREDG(SBFLD1,XINDF,CLRLOL,CLRUPR,MKEY,
     1                         ILMAX,JLMAX,IRTN)
              ENDIF
           ENDIF
C
           DOTSGI = T1
           A1 = UA3V
           B1 = UM3V
C
C           FIND CONTOUR LABELS
C               ABOVE CENTERS AND ALONG CONTOURS
C
           IF ( .NOT. PLOTFG    ) THEN
C
              IF (PCLPFG.AND.GDTYPE.EQ.05)
     1            CALL XLABEL(SBFLD1,IBIGG,JBIGG,XINDF)
C             ... FOR TYPE 5 TO CLIP POLE AREA
              IF (RANGFG.AND.KEYIDX.EQ.46)
     1           CALL XLABEL(SBFLD1,IBIGG,JBIGG,XINDF)
C             ... FOR TDL MOS THUNDERSTORM/SEVERE WX CHART
              IF (IMODEL .EQ. MSKTRAJ) THEN
                  IF (KEYIDX.EQ.51.OR.KEYIDX.EQ.47 .OR.
     1                     KEYIDX.EQ.48) THEN
C                 .... FOR TRAJECTORY 4-PANEL CHART
                     CALL XLABEL(SBFLD1,IBIGG,JBIGG,XINDF)
                     CALL TJLABEL(SBFLD1,IBIGG,JBIGG,DOTSGI,A1,B1,
     1                           KEYIDX)
                  ENDIF
              ELSE
                   CALL LABELCON(SBFLD1,IBIGG,JBIGG,DOTSGI,A1,B1,
     1              KDN,LDN,IEXIT)
              ENDIF
C
              IF (IEXIT .NE. 0  .AND. IEXIT .LE. 5) THEN
                  PRINT *,' FATAL ERROR FROM LBCENCON=', IEXIT
                  RETURN
              ENDIF
C
           ELSE
C               FOR ISOTACHS TO PLOT WIND DIRECTIONS...
                  VA1V = UA1(KEYIDX)
                  VM1V = UM1(KEYIDX)
                  VA2V = UA2(KEYIDX)
                  VM2V = UM2(KEYIDX)
                  IMAX = IMAXP
                  JMAX = JMAXP
C
                  CALL SUBFLD(  BUFF,IBIGG,JBIGG,SBFLD1, IMAX,
     1                 JMAX,ICOR,JCOR, VM1V,VA1V,VM2V,VA2V,IEXIT)
C                 ....GET U-COMPONENT
                  IF (IEXIT .NE. 0) THEN
                     IEXIT = 3
                     GO TO 800
                  ENDIF
C
                  IF (KEYIDX.EQ.11.OR.KEYIDX.EQ.17) THEN
                     CALL SUBFLD(SAV1ST,IBIGG,JBIGG,SBFLD2, IMAX,
     &                   JMAX,ICOR,JCOR,VM1V,VA1V,VM2V,VA2V,IEXIT)
C                  ....GET V-COMPONENT
                     CALL PTWNDX(SBFLD1,SBFLD2, IMAX, JMAX,
     1                           I1BIG,J1BIG,IGPLOT,ISTART,JSTART,T1)
                  ELSE IF (KEYIDX.EQ.38) THEN
                     CALL PLTRH(SBFLD1,IMAX,JMAX,
     1                          I1BIG,J1BIG,IGPLOT,ISTART,JSTART,T1)
                  ENDIF
C
                  IF (IEXIT .NE. 0) THEN
                       IEXIT = 3
                       GO TO 800
                  ENDIF
C
           ENDIF
C
C
C                 WRITE AND CLOSE THE DESIGN FILE
C
           CALL WRTDGN(OUTDGN, IEXIT)
           IF (IEXIT .NE. 0  .AND. IEXIT .LE. 7) THEN
               PRINT *,' FATAL ERROR FROM WRTDGN=', IEXIT
           ELSE
               RETURN
           ENDIF
  800 CONTINUE
C
         PRINT *,ERRMSG
C
  900 CONTINUE
C
      RETURN
      END
