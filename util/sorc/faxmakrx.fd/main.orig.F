#if !defined (_BIG_ENDIAN) && !defined (_LITTLE_ENDIAN)
#error 'Neither _BIG_ENDIAN nor _LITTLE_ENDIAN is defined.'
#endif
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C     
C MAIN PROGRAM: FAXMAKRX
C   PRGMMR: FACEY            ORG: NP12        DATE: 2001-09-20
C
C ABSTRACT: MAKE VARIAN/FAX MAPS.
C
C PROGRAM HISTORY LOG:
C   95-05-05  ORIGIONAL AUTHOR LUKE LIN  
C   95-08-15  LUKE LIN      ADD LINE ATTRIBUTES
C   96-01-31  LUKE LIN      MODIFY TO GENERTE MULTIPLE CHARTS
C   96-03-27  LUKE LIN      ADD PRINT TITLE.
C   96-05-10  LUKE LIN      POINT TO FONTLIB AND CNTRLIB
C   96-06-04  HENRICHSEN    ADD LOGIC TO GET A USER TITLE FROM THE PARM.
C                           ADDED LOGIC TO CALL CONSOL.
C   96-06-21  LUKE LIN      MODIFY FOR UOR_DOT.
C   96-06-24  HENRICHSEN    MODIFY TO GET 5DAY MEAN TITLES, ALSO ADD AN EXTRA
C                           LINE TO GULF TITLE. MODIFY COMMON/LLABFX/ TO
C                           PASS TITLE HEIGHT "THEIGHT" AND HGTFLG.
C   96-06-25  LUKE LIN      MODIFY FOR GENERATING MULTIPLE CHARTS.
C   96-06-26  HENRICHSEN    MODIFY TO GET THE SUBSET NUMBERS OF THE MAPS MADE
C                           INTO A WORK ARRAY FOR CALL TO CONSOL.
C   96-06-27  LUKE LIN      ADDED DAVIDS LOGIC TO MAKE FAX CUTS.
C   96-06-28  LUKE LIN      ADDED OPTION CARDS AND LOGIC TO MAKE FAX CUTS.
C   96-07-23  LUKE LIN      POINT TO THE PRODUCTION BACKGROUND LIBRARY.
C   96-10-01  LUKE LIN      MODIFY FOR NGM CGRID MAPS.
C   96-10-18  LUKE LIN      MODIFY FOR VMAPMAKR  MAPS.
C   96-11-18  LUKE LIN      MODIFY FOR GFS PFAX1 CHARTS.
C   97-01-21  LUKE LIN      MODIFY TO ADD TAPE 54 FOR PLOT OBSERVATIONS.
C   97-02-21  LUKE LIN      ADD PROPER EXIT STATUS.
C   97-03-13  LUKE LIN      MODIFY TO GENERATE UKMET, ECMWF, 6-10 DAY DN,
C                           CAC BLENDED, TDL TSTM/SVR AND HGT MEAN CHARTS.
C   97-03-21  LUKE LIN      MODIFY TO GENERATE GFS ANOMALY THICKNESS, 250MB FDPLT,
C                           AND NAM/NGM BOUNDARY LAYER WIND/RH PLOTS.
C   97-04-02  LUKE LIN      ADD OPTION FOR USER EXTRA TITLE.
C   97-04-18  LUKE LIN      FIX ECMWF D+3/D+8 MAP TITLES. 
C   97-05-06  LUKE LIN      ADD LAND-SEA MASKING TO MOS TTM/SVR WX MAPS.
C   97-06-03  LUKE LIN      MODIFY FOR 12 HOUR SFC PRESSURE CHANGE.     
C   97-07-11  LUKE LIN      MODIFY FOR SETTING UNDEFINED GRID ON THE POLE AREA
C                           FOR TYPE 5 AND MAKE TRAJECTORY 4-PANEL CHART.
C   97-08-28  LUKE LIN      ADD STATISTIC REPORT AT THE END OF PROGRAM.
C   98-06-15  FACEY         CONVERT TO F90
C 1999-08-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM
C                           RS/6000. ASSIGNED PROPER VALUE TO INDEFF
C                           USING RANGE FUNCTION FOR IBM RS/6000 FOR
C                           COMPILE OPTIONS xlf -qintsize=8 -qrealsize=8.
C                           MODIFIED TO USE DIRECT ACCESS I/O ON
C                           FAX FILE FOR IBM SP AS A REQUIREMENT FOR 
C                           OSO. USED LOCAL DIRECTORY FOR MAP BACKGROUND, 
C                           HENCE EACH SCRIPT MUST COPY THE 
C                           APPROPRIATE MAP BACKGROUND FILES FROM
C                           PRODUCTION DIRECTORY
C                           /nwprod/util/fix/graph_gphbg
C                           OR IN CHECK OUT USER MAY USE OWN LOCAL
C                           BACKGROUND DIRECTORY SUCH AS
C                           /nfsuser/g02/wx12ph/util/fix/graph_gphbg.
C                           Introduced a call to ENDMAP to put the
C                           end record on the fax output. ENDMAP writes
C                           the end record in direct access mode.
C   00-11-21 KRISHNA KUMAR  New version of faxmakr to accomodate  chnge in 
C                           octet 10 of PDS for sfx prsr which was changed 
C                           from 102 (MSL) to 01 (SFC).
C   01-08-06 BILL FACEY     MODIFY VALID TIMES OF MEAN D-3,D+0,D+3,D+6,
C                           AND D+8 CHARTS. 
C
C USAGE:
C   INPUT FILES:
C     FT05F001 - PROGRAM CONTROL CARDS.
C     FT12F001 - THE DESIGN SEED FILE.
C
C   OUTPUT FILES:
C     FT06F001 - CHECK OUT STATEMENTS AND ERROR MESSAGES
C     FT51F001 - HOLDS THE DESIGN FORMATED INTERMEDIATE METAFILE.
C     FT61F001 - HOLDS THE COMPLETED VARIAN/FAX MAPS.
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - CLEAR   CNTOR    CUDFPI DGNOPN DGNSCN
C                - FCLIP   FRMFAXT  GETCON GETFLD 
C                - GLOSEC  GRD2IDGN IJ2UOR INDEFF INDFPI 
C                - MTITLE  MODEC3   MODEC5 MODEC8 NONVIS
C                - LBCNCON LNDFPI   LSDFPI LSDFPZ
C                - PLTVEC  PRECIP   REROTA SWABYT TWOFLD TXDFPI
C                - TYPE66  UNPKHD   UPDOWN VARIAN VHILO  VTHIN  
C                - WNDPLT  WRTDGN
C     LIBRARY:
C       W3LIB    - W3AI01 W3AS00 W3AS01 W3AS02 W3AS03
C                - W3AI35 W3AQ15 W3FK15 W3FK40 W3FQ02 W3FQ03
C                - W3FM01
C                - W3LOG  W3TAGB W3TAGE W3AK19 W3AQ09
C      GRAPHICS  - BIN2EB GULFTL CONSOL
C
C   EXIT STATES:
C     ISTOP=   0 - SUCCESSFUL RUN
C              2 - NO FAX MAPS MADE, CHECK MAP REGISTRATION CONSTANTS!
C          =   3 - NO FAX MAPS PROCESSED, CHECK DATA CARDS AND PRINT
C          
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM
C
C$$$
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       INTEGER      MAXIWORD
       PARAMETER   (MAXIWORD=64)
       INTEGER      MAXJSLINE
       PARAMETER   (MAXJSLINE=3040)

       INTEGER    LMAX
       PARAMETER (LMAX=1024)

       INTEGER    LMAX2
       PARAMETER (LMAX2 = 2*LMAX)       !... = 2048

C
      COMMON / CLRFLD / CLRLAT,CLRLOL,CLRUPR,LCLEAR,ECLEAR,NCLEAR
      LOGICAL      ECLEAR
      LOGICAL      LCLEAR
      LOGICAL      NCLEAR
      REAL         CLRLAT
C
      COMMON / FLDCON / IDFLD(16), NUMFLD, NDEGNS
C
      COMMON / KPLOT / LABEL(2,1024),LABIX,NOBUF,IDRA(50)
C
C
      COMMON / ILCON / MAP(15)
      INTEGER       MAP

C
      COMMON /ICON/ UA1(50),UA2(50),UA3(50),UM1(50),UM2(50),UM3(50),
     1       KTYPE(50)
      CHARACTER*4   KTYPE
C
      COMMON /LABG/ GULPXX(2),GULPXY(2),LVFLG,NUMG,GLAB
     1              
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
      CHARACTER*1   CIFID(48)
      CHARACTER*32  EXVALID

C
      COMMON /MCON/ IADDB,IADDTAU,NUMTYP,NUMKEY,
     1              MTAU(18),MEAN(18),LMEAN,KDN,LDN
      INTEGER       MTAU
      LOGICAL       LDN
      LOGICAL       KDN
      LOGICAL       LMEAN
      CHARACTER*4   MEAN 
      CHARACTER*104 PARM     
C
      COMMON /MUTCON/ KEYIDX,UA1V,UA2V,UA3V,UM1V,UM2V,UM3V,LINEVU,LINEP,
     X               IGRIDP,T1
C
      COMMON  /POLE/ XPOL,YPOL,GDTYPE
ckumar
      COMMON  /POLENEW/ XMESHL
ckumar
C
C
C     ...THE POLE POSITION IN GRID(65,65) IS AT GRID(33,33).
C     ... GRID MESH LENGHT = XMESHL = 381.0 KLM
C     ...THE POLE POSITION IN GRID(53,45) IS AT GRID(27,49).
C     ... GRID MESH LENGHT = XMESHL = 190.5 KLM
C     ...THE POLE POSITION IN GRID(53,57) IS AT GRID(27,49).
C     ... GRID MESH LENGHT = XMESHL = 190.5 KLM
C

      INTEGER    IMAGSIZ_WRDS
      PARAMETER (IMAGSIZ_WRDS=MAXIWORD*MAXJSLINE)
C
ckumar
        INTEGER IBITPLANE(IMAGSIZ_WRDS)
ckumar
        INTEGER       LBGUNIT
C       ... LBGUNIT -- INPUT BACKGROUND UNIT; NTOTWD
        INTEGER       NTOTWDS
C       ... TOTAL WORDS IN BACKGROUND FILE
        INTEGER       NOLINES
C       ... NO OF SCAN LINES IN THE BCAKGROUND FILE
        INTEGER       NOWIDTH
C       ... THE WIDTH OF THE BACKGROUND FILE IN WORDS
        INTEGER       NOPIXELS
C       ... THE WIDTH OF THE BACKGROUND FILE IN PIXELS
C
      COMMON /UUNAME/ USRBYT,LUNAM,USRNAM
C
      INTEGER       USRBYT
      LOGICAL       LUNAM
      CHARACTER*96  USRNAM
C
C      .... THIS SET IS FOR UOR_DOT
       COMMON /UOR2D/ UGRIDT1,UXPO,UYPO,UXADJUS,UYADJUS,UCU2GI,UORFG,
     1                IP,IPOPT
       REAL     U
       LOGICAL  UORFG
C
C     ... FOR CONTOR OPTIONS
      COMMON /CNTROP/ NDVD, NDIV, NCNTRFILT, NCENTFILT,SHADIV
      INTEGER SHADIV
C     ... NDVD FOR INTERPOLATION, NDIV FOR DIVISION
C     ... NCNTRFILT FOR CONTOUR FILTER, NCENTFILT FOR CENTER FILTER
C
C
      COMMON /CPOB/ CPOBFG,ITAPOB,ILVLT,KRUN,IOPTRA(2)
      LOGICAL        CPOBFG
C
      REAL         GRIDT1
C
      CHARACTER*80  INTEXT
C
      COMMON /MESSG/ NUMGOD,SUBNUMS,ERRMSG,EXTRATIT
C
      INTEGER       NUMGOOD,SUBNUMS(40)
C
      CHARACTER*55  ERRMSG(05)
      CHARACTER*95  EXTRATIT

      CHARACTER*55  ENDMSG
C
      CHARACTER*40  BLANKS
      CHARACTER*12  CLEAR
      CHARACTER*8   BLANK8
C
      INTEGER      FAXFIL
      INTEGER      BDATE
      INTEGER      CLRLOL(2)
      INTEGER      CLRUPR(2)
      INTEGER      DGNSED
      INTEGER      GDTYPE
      INTEGER      MAXMAP
C
C     ...THE POLE POSITION IN GRID(65,65) IS AT GRID(33,33).
C     ...THE POLE POSITION IN GRID(53,45) IS AT GRID(27,49).
C
      INTEGER      IGRIDT
      INTEGER      KDATE(2)
      INTEGER      OUTDGN,OUTDGN1,OUTDGN2,OUTDGN3,OUTDGN4,NUMDGN
      INTEGER      IFAXNO
      INTEGER      KFLAG
      INTEGER      IFCBF(180)
      CHARACTER*8  GRIDTP
C
      COMMON / GRB1 / LUGRB, LUGRBIX, IFCSTHR
      INTEGER     LUGRB, LUGRBIX
      INTEGER     GRPWHICH
      INTEGER     GRPMAP
C
      INTEGER     KROT_PRI(2),ICMD,IPT,JPT,NCHAR
      REAL        HEIGHT,ANGLE
      CHARACTER*4 CTEXT
C
      INTEGER      IWINDOW(30)
      LOGICAL      LEBCDIC
      LOGICAL      EXTRA
       INTEGER       IACC
       CHARACTER*8   CACC
       EQUIVALENCE  (IACC,CACC)
C
C
      CHARACTER*40 CPATHBGND
      CHARACTER*1  NULL
      INTEGER     LUNBGD
      PARAMETER  (LUNBGD=41)           !... INPUT UNIT FOR MAP-BGND
C 
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      COMMON  /ALT_LBL/ LBL_INCOREQ,LBL_EMPTYQ,LABEL_PKD(LMAX)
      LOGICAL   LBL_INCOREQ
      LOGICAL   LBL_EMPTYQ
      INTEGER   LABEL_PKD
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ...   FOR PLOTTED STRIP-TITLES IN IMAGE_STR(I,J)  ...
       INTEGER      MAXIWORD_STR
       PARAMETER   (MAXIWORD_STR=27)            !... 27 I*8 = 1728 PELS

       INTEGER      MAXJSLINE_STR
       PARAMETER   (MAXJSLINE_STR=800)          !... LIMIT STRIP TITLES

ckumar       COMMON   /STITLES/ IMAGE_STR
       INTEGER           IMAGE_STR(MAXIWORD_STR,MAXJSLINE_STR)

       COMMON   /STITLPLT/NRECSTART_STR, NITMPLTED_STR,
     1                    MXJVAL_STR, MXJLABITM_STR
       INTEGER           NRECSTART_STR          !... PTR IN LABEL FILE
       INTEGER           NITMPLTED_STR          !... COUNT STRITM PLTED
       INTEGER           MXJVAL_STR             !... MAXJ STR PLTED
       INTEGER           MXJLABITM_STR          !... AND THAT LABL ITEM
C
      INTEGER      IL_STR(15)
      INTEGER      IWINDOW_STR(30)      !... map const: STRIP TITLES
      INTEGER      IMAGSIZ_STR
      INTEGER      NPXLOUT_STR
      INTEGER      NROWSHOW_STR
C
      LOGICAL      LCHKSORTQQ
      LOGICAL      LANYTITLESQQ
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . .   Statisitcs about LABEL array via LOOK_LAB()   . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       integer      lmtmxmn
       parameter   (lmtmxmn=8)

       INTEGER      LMTPRIOR
       PARAMETER   (LMTPRIOR=8)

       COMMON      /STAT_LAB/NITEM_TOT,NITEM_TITLE,NPRIOR_LAB,
     1                       MAXIJ_LAB,MINIJ_LAB

       INTEGER      NITEM_TOT     !... TOTAL COUNT OF LABEL-ARRAY ITEMS
       INTEGER      NITEM_TITLE
       INTEGER      NPRIOR_LAB(LMTPRIOR,2)
       integer      MAXIJ_LAB(LMTMXMN,2)
       integer      MINIJ_LAB(LMTMXMN,2)
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . used by bufow512() and bf512_wr()  . . . . .

       COMMON /ARBFOX6B/ LUNX6B,LUX6BOPNQ,NBUFX6B,IPTR_X6BF
       INTEGER       LUNX6B
       LOGICAL       LUX6BOPNQ
       INTEGER       NBUFX6B
       INTEGER       IPTR_X6BF
C      . . . . . . . . . . . . . . . . . . . . . . . . .
       INTEGER    J_FR_MAX
       PARAMETER (J_FR_MAX=4)
       INTEGER    IWRD_FR_MAX
       PARAMETER (IWRD_FR_MAX=15)
       INTEGER    IPXL_FR_MAX
       PARAMETER (IPXL_FR_MAX=16)

       INTEGER    LUNEXT6B
       PARAMETER (LUNEXT6B=80)
C
C ..........   FOR FAX CUT
C
      COMMON /FAXICK/ ISCHED(8,50),INXISCHED
      COMMON /FLFSRP/ INXLFSTRP,LSNUMBYT(20),LSINTEXT(20),LSTITPX(20),
     1                LSTITPY(20),LSDCKPX(20),LSDCKPY(20),
     2                LSDCKOPT(20),LSFONT(20)
      CHARACTER*120   LSINTEXT
      REAL            LSFONT
      COMMON /FRTSRP/ INXRTSTRP,RSNUMBYT(20),RSINTEXT(20),RSTITPX(20),
     1                RSTITPY(20),RSFONT(20),RSFCSTHR(20)
      CHARACTER*120   RSINTEXT
      INTEGER         RSNUMBYT,RSTITPX,RSTITPY,RSFCSTHR
      REAL            RSFONT
C
      COMMON/IOUTFAX/IFAXOUT,NRECFAX  ! ADDED FOR DIRECT ACCESS FOR FAX OUTPUT
C
      INTEGER IFAXOUT,NRECFAX
C
      CHARACTER*8     BGNAME
      EQUIVALENCE     (BGNAME,MAP(1))
C
C     ... OPTN BITS TO MATCH THOSE GIVEN IN MAP(2) ...
      INTEGER    K0008X
      DATA       K0008X   / X'0008' /   !... LEAVE OUTP OPEN AT EXIT
      INTEGER    K0010X
      DATA       K0010X   / X'0010' /   !... ADDING ONTO LEFT-OPEN OUTP

      INTEGER      MYOPTNBITS
C
ckumar
ckumar used block data statements separately !!!!
ckumar      DATA       ISCHED / 165, 0, 0, 1800, X'8000',X'D800',0,0,
ckumar     1                    392*0/
ckumar
      DATA          BLANKS
     1                    /'                                        '/
C     /'123456789012345678901234567890123456789012345678901234567890'/
      DATA          CLEAR    /'CLEAR CLEARE'/
      DATA          BLANK8   /'        '/
      DATA          KFLAG    /Z'FFFFFC0000000000'/
C
      EQUIVALENCE  (IGRIDT,GRIDTP)
C
C--------------------- PROGRAM STARTS -------------------------------
C
C
      SAVE
      CALL W3TAGB('FAXMAKRX',2001,0263,0080,'NP12')
C
      PARM = ' '
      IERR = 0
      
      CALL W3AS00(NC,PARM,iret)
C
C     CHECK TO SEE IF THERE IS A USER NAME IN THE PARM FIELD.
C
       LUNAM = .FALSE.
      IF(NC.GT.5)THEN
       WRITE(6,FMT='('' MAIN: PARM="'',A,''".'')')PARM(1:NC)
       IF(PARM(1:5).EQ.'NAME=')THEN
        USRBYT = NC - 5
        USRNAM(1:USRBYT) = PARM(6:NC)
C
         WRITE(6,FMT='('' USER NAME HAS'',I3,'' BYTES AND IS:"'',
     1   A,''".'')')USRBYT,USRNAM(1:USRBYT)
        LUNAM = .TRUE.    
       ELSE
        WRITE(6,FMT='('' NO USER NAME IN PARM!'')')
       ENDIF
C     
      ENDIF 
      NULL = CHAR(0)
      NUMGOOD = 0
      EXTRA = .FALSE.
      ISTOP = 0
      LUNX6B = LUNEXT6B
      IEXIT = 0
C
        CALL GETOVL(MAXMAP,DGNSED,MAP)
        print*,'In main after GETOVL  '
C       ... READ THE OVERALL MANDATORY INFORMATION IN 
C       PRINT 85, (MAP(NN),NN=2,15)
C 85    FORMAT (1H , 14(1X, I6))

C
C          START MAIN LOOP...
C
      GRPMAP = 0
      GRPWHICH = 0
      MWHICH = 1
C     ... GRPMAP FOR THE NUMBER OF MAP OF EACH FCST HOUR
C     ... MIWHICH IS THE INDEX FOR EACH GROUP OF FCST HOUR
C
C
ckumar
      NRECFAX  = 0
ckumar
C
 100  CONTINUE
C 
C                  ... STARTING NEXT MAP ....
C
      KEXIT = 0
C
      IF (MWHICH .GT. MAXMAP) GO TO 900
C              ... IF FINISH ALL MAPS, EXIT TO THE MAIN LOOP ...
C
          PRINT 110,MWHICH
  110     FORMAT(/,1H0,'*** THIS IS THE ',I4,' MAP.')
C
C
          PRINT *,' GRPWHICH=',GRPWHICH, ' GRPMAP=',GRPMAP
          IF (GRPWHICH .EQ. GRPMAP) THEN
C              ...GET ANOTHER FORECAST GROUP INFORMATION
               CALL GETGRP(GRPMAP,LUGRB,LUGRBIX,IFCSTHR)
               GRPWHICH = 0
          ENDIF
C      . . . . .   CONSTANTS FOR THE STRIP-TITLE WINDOW  . . . . . .

          IL_STR(1) = 0            !... NO MAP BGND FOR STRIP-TITLE REGION
          IL_STR(2) = 0

          IL_STR(3) = 72           !... 1800-(72)=1728pels =27 longwords
          IL_STR(4) = 0
          IL_STR(5) = 1728
          IL_STR(6) = MAXJSLINE_STR                !... = 800 scanline max

          IL_STR(7) = 72
          IL_STR(8) = 0
          IL_STR(9) = 1728
          IL_STR(10) = MAXJSLINE_STR

          IL_STR(11) = 0
          IL_STR(12) = 0
          IL_STR(13) = 0
          IL_STR(14) = -7399               !... delta-j to position titles
          IL_STR(15) = 0

          CALL CONSTA(IL_STR,IWINDOW_STR)

          IWINDOW_STR(21) = 1              !... flags this as strip-titles
C
C
         CALL CONSTA(MAP,IWINDOW)
C        . . .  COMPUTE SCALE AND ADJUSTMENT CONSTANTS ...
C
         NOLINES = IWINDOW(4)
         NOWIDTH = IWINDOW(15)
         NOPIXELS = IWINDOW(16)
         NTOTWDS = NOWIDTH * NOLINES

C
         MYOPTNBITS = 0

         IF(IAND(MAP(2),K0010X) .NE. 0) THEN
C          ... IF ADDING-ON TO PREVIOUSLY LEFT-OPEN PRODUCT FILE,
           MYOPTNBITS = IBSET(MYOPTNBITS,0)        !... MULTI-PANEL
           MYOPTNBITS = IBSET(MYOPTNBITS,1)        !... ADDING ON TO EXISTG
         ENDIF
         IF(IAND(MAP(2),K0008X) .NE. 0) THEN
C          ... IF LEAVING THE OUTPUT FILE OPEN AT EXIT,
           MYOPTNBITS = IBSET(MYOPTNBITS,0)        !... MULTI-PANEL
           MYOPTNBITS = IBSET(MYOPTNBITS,2)        !... LEAVE OPEN AT END
         ENDIF

C
          IF((NOWIDTH .LE. 0) .OR. (NOLINES .LE. 0)) THEN
           WRITE(ERRMSG(2)(1:55),
     1     FMT='(''NO FAX MAPS MADE, CHECK MAP REGISTRATION CONS'',
     2     ''TANTS!!!!:'')')
C    1            '123456789012345678901234567890123456789012345',
C    2     ''6789012345'')')
        WRITE(6,FMT='(1H ,''CNTR::CONSTA: DIMENSIONS OF IMAGE= ('',
     1              I4,'','',I6,'') ... WHICH ARE WRONG ! ! !'',
     2             /1H ,'' GO CHECK THE MAP REGISTRATION CONSTANTS!'',
     3             /1H ,'' * * * *   E R R O R   S T O P   * * * *'')')
     A        NOWIDTH,NOLINES
            IEXIT= 2
            GO TO 999
          ENDIF
C
          DO  I = 1,NTOTWDS
            IBITPLANE(I) = 0
          ENDDO
C

C      ...   THE FOLLOWING STRIP-TITLE INITIALIZATION NEEDS TO HAVE
C      ...     SOME IF TESTS FOR FIRST PANEL OF MULTI-CALL-CNTR PRODUCT
          DO  J = 1,MAXJSLINE_STR
            DO  I = 1,MAXIWORD_STR
              IMAGE_STR(I,J) = 0
            ENDDO
          ENDDO
C
          NRECSTART_STR = 0          !... PTR IN LABEL FILE
          NITMPLTED_STR = 0
          MXJVAL_STR = 0
          MXJLABITM_STR = 0
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C       INITIALIZE LABEL ARRAY
C
          LABIX = 0
          NOBUF = 0
C
          IPT = 1
          JPT = 0
          HEIGHT = 1.0
          ANGLE = 0.0
          KROT_PRI(1) = 0
          KROT_PRI(2) = 0
          CTEXT(1:1) = '?'
          CTEXT(2:2) = CHAR(1)
          CTEXT(3:3) = '$'
          CTEXT(4:4) = CHAR(0)
          NCHAR = 2
          ICMD = -2
          CALL PUTLAB(IPT,JPT,HEIGHT,CTEXT,ANGLE,NCHAR,KROT_PRI,
     X                ICMD)
C
          INXRTSTRP = 0
          INXISCHED = 1
          INXLFSTRP = 0
          DO LL = 2, 50
             DO NN = 1, 8
                ISCHED(NN,LL) = 0
             ENDDO
          ENDDO
C         ... DEFAULT FOR MAPPING CONSTANTS
          UGRIDT1 = 37.5
          UORFG = .FALSE.
          UXPO = 33.0
          UYPO = 33.0
          UXADJUS = -9.0
          UYADJUS = -7.0
          UCU2GI = 3810.0
C         ... DEFAULT FOR CONTOUR OPTIONS AND FILTERS
          NDVD = 2
          NDIV = 4
          NCNTRFILT = 0
          NCENTFILT = 0
C
          INTEXT = BLANKS//BLANKS
C
  200  CONTINUE
C
C
C          INITIALIZE POINTERS
C
          JEXIT = 0
         CALL GETCON(INBYTE,INTEXT,NUMDGN,OUTDGN1,OUTDGN2,
     1               FAXFIL,GRIDT1,IFAXNO)
ckumar
      IFAXOUT = FAXFIL
ckumar
ckumar     OPEN THE OUTPUT FAX FILE IN DIRECT ACCESS MODE 
ckumar     REQUIREMENT FOR OSO
ckumar 
       OPEN (IFAXOUT, ACCESS='DIRECT', RECL=1440 )
ckumar
          OUTDGN3 = 78
          OUTDGN4 = 79
C
         IF (NDEGNS .EQ. 1) THEN
            OUTDGN = OUTDGN1
            IF (CPOBFG) THEN
               CALL RDOBVT(IERROR)
C              ..... READ OBSERVATION TAPE
               PRINT 212,IERROR
 212           FORMAT(1H0,'ERROR RETURN FROM RDOBVT  ',I5)
            ENDIF
C
C             LOAD THE BACKGROUND IN 
C
            CPATHBGND(1:40) = ' '
ckumar     
ckumar Map background files are copied to the working local directory.
ckumar User must copy the relevant map background files from 
ckumar       /nwprod/util/fix/graph_gphbg 
ckumar to the running directory in the script.
ckumar
            CPATHBGND(1:) = './' //NULL
ckumar
            CALL GETBGND(LUNBGD,CPATHBGND,IWINDOW,
     1                   IBITPLANE,IRET_BG)
            IF (BGNAME(1:6) .EQ. 'SH4001') CALL SHPLOT
C           .... PLOT SPECIAL STATION ON SH4001 BACKGROUND
            IF (GULPXX(2).EQ.-49 .AND. GULPXY(2).EQ.-49) CALL POLSTNEX
            IF (GULPXX(2).EQ.-48 .AND. GULPXY(2).EQ.-48) CALL POLSTNEX
C           .... PLOT SPECIAL STATION ON NH4005 FOR PFAX1

         ELSE IF (NDEGNS .EQ. 2) THEN
            OUTDGN = OUTDGN2
         ELSE IF (NDEGNS .EQ. 3) THEN
            OUTDGN = OUTDGN3
         ELSE IF (NDEGNS .EQ. 4) THEN
            OUTDGN = OUTDGN4
         ELSE
            PRINT *, ' THE OUTPUT DESIGN FILE IS WRONG--',NDEGNS
            OUTDGN = OUTDGN1
         ENDIF
C
C       GET AND CHECK GRID TYPE FOR A VALID ONE.
C
         IGRIDT = IDFLD(5)
#if defined (_LITTLE_ENDIAN)
         CALL SWAP_BYTES_IN_PLACE (IGRIDT, 8_8, 1_8)
#endif
         IGRIDP = mova2i(GRIDTP(8:8))
         GDTYPE = IGRIDP
         IF (KEXIT .EQ. 0) THEN
           IF (GDTYPE.EQ.26 .OR. GDTYPE.EQ.27 .OR. GDTYPE.EQ.05 .OR.
     1       GDTYPE.EQ.101 .OR. GDTYPE.EQ.104 .OR. GDTYPE.EQ.28) THEN
             print *,' lugrb=',lugrb,' lugrbix=',lugrbix
             print *,' GDTYPE=', GDTYPE
             PRINT *, 'NCNTRFILT, NCENTFILT = ', NCNTRFILT, NCENTFILT
             CALL GRD2IDGN(DGNSED,KEYIDX,LUGRB,
     1       LUGRBIX,OUTDGN,INBYTE,INTEXT,GRIDT1,
     2       KDATE,JEXIT)
             IF (JEXIT.NE.0) THEN
                IEXIT = JEXIT
                KEXIT = JEXIT
                PRINT *,' '
                PRINT *,'--------------------------------------------'
                WRITE(6,FMT='('' FATAL ERROR RETURN FROM GRD2DGN ='',
     1          I4,/,'' WILL NOT MAKE AND FAX  CHART FOR: '',/,A)')
     2          IEXIT,INTEXT(1:INBYTE)
                PRINT *,'--------------------------------------------'
                PRINT *,' '
             ENDIF
           ELSE
C
C           THIS WAS NOT A VALID GRID TYPE SO READ TO END OF
C           THIS SET OF DATA CARDS
C
              PRINT *,' '
              PRINT *,'--------------------------------------------'
              WRITE(6,FMT='('' FATAL ERROR BAD GRID TYPE ='',
     1           I4,/,'' WILL NOT MAKE AND FAX CHART FOR '',/,A)')
     2           GDTYPE,INTEXT(1:INBYTE)
              PRINT *,'--------------------------------------------'
              PRINT *,' '
              KEXIT = 1
              IEXIT = 1
           ENDIF
C
C               CLOSE THE DESIGN SEED FILE AND
C               AND THE INTERMEDIATE DESIGN FILE.
C
           CLOSE (UNIT=DGNSED)
           CLOSE (UNIT=OUTDGN)
         ENDIF
C
         IF (NDEGNS .LT. NUMDGN ) GO TO 200
C              .... GO BACK TO GENERATE SECOND DESIGN FILE
C
C 
C    =============== COME HERE, ALL THE DESIGN FILE BEING GENERATED ========
C                WE NEED TO SCAN LINE ELEM AND LINE STRINGS FIRST
C                THEN TEXT STRING AFTER ABOVE ITEMS FOR BOTH DESIGNS
C
C
           IF (KEXIT .EQ. 0) THEN
C
C             OPEN THE INTERMEDIATES DESIGN FILE 1 FOR CONVERTING
C
              CALL DGNOPN(OUTDGN1,IRTN)
              IF(IRTN.EQ.0)THEN
C             print *, ' **** design 1 for line string *****'
C
C             SUCCESS WITH OPENING THE INTERMEDIATE DESIGN FILE 1 .
C
              IELETYP = 3
              CALL DGNSCN(IELETYP,IBITPLANE,NOWIDTH,NOLINES,STATUS)
C             print *,' after scanning line elems', irtn
C             ...for line element....
              IELETYP = 4
              CALL DGNSCN(IELETYP,IBITPLANE,NOWIDTH,NOLINES,STATUS)
C             print *,' after scanning line string', irtn
C                 ... FOR LINE STRING
              ELSE
                 PRINT *, ' ERROR WHILE TRY TO OPEN DGN FILE 1'
              ENDIF
              CLOSE (UNIT=OUTDGN1)
           ENDIF
C
           IF (NUMDGN .GT. 1 .AND. KEXIT .EQ. 0) THEN
C
C             OPEN THE INTERMEDIATES DESIGN FILE 2 FOR CONVERTING
C
              CALL DGNOPN(OUTDGN2,IRTN)
              IF(IRTN.EQ.0)THEN
C
C             SUCCESS WITH OPENING THE INTERMEDIATE DESIGN FILE 2 .
C
C                print *, ' **** design 2 for line string *****'
                 IELETYP = 3
              CALL DGNSCN(IELETYP,IBITPLANE,NOWIDTH,NOLINES,STATUS)
C                print *,' after scanning line elems', irtn
C                ...for line element....
                 IELETYP = 4
              CALL DGNSCN(IELETYP,IBITPLANE,NOWIDTH,NOLINES,STATUS)
C             print *,' after scanning line string', irtn
C                ... FOR LINE STRING
              ELSE
                 PRINT *, ' ERROR WHILE TRY TO OPEN DGN FILE 2'
              ENDIF
              CLOSE (UNIT=OUTDGN2)
           ENDIF
C
           IF (NUMDGN .GT. 2 .AND. KEXIT .EQ. 0) THEN
C
C             OPEN THE INTERMEDIATES DESIGN FILE 3 FOR CONVERTING
C
              PRINT *, 'GULPXX(2) =', GULPXX(2)
              PRINT *, 'GULPXY(2) =', GULPXY(2)
C
              IF (GULPXX(2).EQ.-49 .AND. GULPXY(2).EQ.-49) THEN
C
C               .... FOR THE TEMPERATURE PLOT -PFAX1 WITH A CIRCLE
                 PRINT *,' ***NOT PLOT THE THIRD CONTOUR***'
C
              ELSE IF (GULPXX(2).EQ.-48 .AND. GULPXY(2).EQ.-48) THEN
C
C               .... FOR THE TEMPERATURE PLOT -PFAX1 WITH A BOX
                 PRINT *,' ***NOT PLOT THE THIRD CONTOUR***'
C
              ELSE
              CALL DGNOPN(OUTDGN3,IRTN)
              IF(IRTN.EQ.0)THEN
C
C             SUCCESS WITH OPENING THE INTERMEDIATE DESIGN FILE 3 .
C
C                print *, ' **** design 3 for line string *****'
                 IELETYP = 3
              CALL DGNSCN(IELETYP,IBITPLANE,NOWIDTH,NOLINES,STATUS)
C                print *,' after scanning line elems', irtn
C                ...for line element....
                 IELETYP = 4
              CALL DGNSCN(IELETYP,IBITPLANE,NOWIDTH,NOLINES,STATUS)
C             print *,' after scanning line string', irtn
C                ... FOR LINE STRING
              ELSE
                 PRINT *, ' ERROR WHILE TRY TO OPEN DGN FILE 3'
              ENDIF
              CLOSE (UNIT=OUTDGN3)
              ENDIF
           ENDIF
C
           IF (NUMDGN .GT. 3 .AND. KEXIT .EQ. 0) THEN
C
C             OPEN THE INTERMEDIATES DESIGN FILE 3 FOR CONVERTING
C
              PRINT *, 'GULPXX(2) =', GULPXX(2)
              PRINT *, 'GULPXY(2) =', GULPXY(2)
C
              IF (GULPXX(2).EQ.-49 .AND. GULPXY(2).EQ.-49) THEN
C
C               .... FOR THE TEMPERATURE PLOT -PFAX1 WITH A CIRCLE
                 PRINT *,' ***NOT PLOT THE THIRD CONTOUR***'
C
              ELSE IF (GULPXX(2).EQ.-48 .AND. GULPXY(2).EQ.-48) THEN
C
C               .... FOR THE TEMPERATURE PLOT -PFAX1 WITH A BOX
                 PRINT *,' ***NOT PLOT THE THIRD CONTOUR***'
C
              ELSE
              CALL DGNOPN(OUTDGN3,IRTN)
              IF(IRTN.EQ.0)THEN
C
C             SUCCESS WITH OPENING THE INTERMEDIATE DESIGN FILE 3 .
C
C                print *, ' **** design 3 for line string *****'
                 IELETYP = 3
              CALL DGNSCN(IELETYP,IBITPLANE,NOWIDTH,NOLINES,STATUS)
C                print *,' after scanning line elems', irtn
C                ...for line element....
                 IELETYP = 4
              CALL DGNSCN(IELETYP,IBITPLANE,NOWIDTH,NOLINES,STATUS)
C             print *,' after scanning line string', irtn
C                ... FOR LINE STRING
              ELSE
                 PRINT *, ' ERROR WHILE TRY TO OPEN DGN FILE 3'
              ENDIF
              CLOSE (UNIT=OUTDGN3)
              ENDIF
           ENDIF
C
C
C       CLOSE THE LABEL TAPE
C
          ICMD = -7
          CALL PUTLAB(IPT,JPT,HEIGHT,CTEXT,ANGLE,NCHAR,KROT_PRI,
     X                ICMD)
          ICMD = 999
          CALL PUTLAB(IPT,JPT,HEIGHT,CTEXT,ANGLE,NCHAR,KROT_PRI,
     X                ICMD)
C
C
        IF (KEXIT .EQ. 0) THEN
C            *** SORT THE LABEL TAPE
C     . . .   S T E P (1.)   SORT THE LABEL-ARRAY DATA
C                                WHETHER IN-CORE OR OUT ON FILE FT55
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


C     ***          ***                   ***
C     ... THE LABEL LIST IS ON A SEQUENTIAL FILE ...
C
C       ... TO SORT/MERGE THE EXTERNAL FILE-55 LABEL-ARRAY FILE,
        REWIND 55
        CALL MERGES(LUNINP,IRET_MRG)
        IF(IRET_MRG .NE. 0) THEN
C         ... IN OLD VERSION, THAT USED TO STOP WITHIN MERGE
          WRITE(6,FMT='(1H ,''MAIN::MERGES: SERIOUS ERROR ... '',
     1                      ''RETURNED WITH CODE='', I5)')
     A            IRET_MRG
          REWIND LUNINP

        ELSE
           WRITE(6,FMT='(1H ,''AINR: normal return from merges '',
     1                      ''with sorted LABEL-data on ITAPE='',I4)')
     A            LUNINP
        ENDIF

C     ... TO GATHER STATISTICS ABOUT THE LABEL-ARRAY ITEMS ...
       NITEM_TOT = 0
       NITEM_TITLE = 0
       LANYTITLESQQ = .FALSE.
       do  j = 1,2
         do  i = 1,LMTPRIOR
           NPRIOR_LAB(I,J) = 0
         ENDDO
       ENDDO
       do  j = 1,2
         do  i = 1,LMTMXMN
           MAXIJ_LAB(I,J) = 0
         ENDDO
       ENDDO
       do  j = 1,2
         do  i = 1,LMTMXMN
           MINIJ_LAB(I,J) = 0
         ENDDO
       ENDDO
      if(.NOT. LBL_EMPTYQ) THEN
        LCHKSORTQQ = .TRUE.

        call look_lab(LUNINP, LCHKSORTQQ, IRET_look)
C         ... which works for case of LBL_INCOREQ also,
C         ... by querying COMMON /ALT_LBL/ ...

C       WRITE(6,FMT='(1H ,''MAIN::look_lab: RETURNED WITH TOTAL '',
C    1                    '' SORTED ITEM COUNT='',I8,
C    2               /1h ,7X,''look_lab() RETURN-CODE ='',I4)')
C    A          NITEM_TOT,IRET_look

        IF(IRET_LOOK .NE. 0) THEN
          WRITE(6,FMT='(1H ,''MAIN:... WARNING ...:LOOK_LAB FAILED '',
     1                      ''WITH RETURN CODE='',I3)')
     A            IRET_LOOK
        ELSE
C         ... OTHERWISE, NORMAL return from  LOOK_LAB ...
          IF(NITEM_TITLE .LE. 0) THEN
            LANYTITLESQQ = .FALSE.
          ELSE
            LANYTITLESQQ = .TRUE.
          ENDIF
        ENDIF
      ENDIF
C
C
C            *** CALL PRINT TITLE TO OUTPUT TAPE54
C
          LEBCDIC   = .FALSE.
          DO   IPRIO = 1, 5
               LOOPRIOR = IPRIO - 1
               CALL PRTITLE(LUNINP,LOOPRIOR,IWINDOW,LEBCDIC,
     1                      IBITPLANE,NOWIDTH,NOLINES,IRTN)
C              PRINT *,' PRTITLE: RETN-CODE=', IRTN

               IF(LANYTITLESQQ) THEN
                  NRECSTART_STR = minij_lab(7,2)
                  ICOUNTSTRIP = NPRIOR_LAB(IPRIO,2)
                  IF(ICOUNTSTRIP .GT. 0) THEN
C            WRITE(6,FMT='(1H ,''MAIN::PRTITLE: FOR LOOPRIOR='',I3,
C    1                  '';  COUNT OF STRIP-TITLE ITEMS='',I5,
C    2             /1H ,'' SO WE MUST CALL PRTITLE(FOR STRIP TITLES)'',
C    3             /1h ,'' WHICH BEGIN IN RECORD NUMBER='',I5)')
C    A            LOOPRIOR,ICOUNTSTRIP,NRECSTART_STR
               CALL PRTITLE(LUNINP,LOOPRIOR,IWINDOW_STR,LEBCDIC,
     1                  IMAGE_STR,MAXIWORD_STR,MAXJSLINE_STR,IRTN)
                    PRINT *,' PRTITLE: RETN-CODE=', IRTN

C                ELSE
C            WRITE(6,FMT='(1H ,''MAIN::PRTITLE: FOR LOOPRIOR='',I3,
C    1                         '';  NO STRIP-TITLES TO PROCESS'')')
C    A               LOOPRIOR
                 ENDIF
             ENDIF
          ENDDO
C
C            *** CONVERT IT TO NMC 6-BIT ****
C
          IF (KEXIT .EQ. 0 .AND. IRTN .EQ. 0) THEN
C
C      ... to convert image to Extended 6-bit format and output it ...
C                 ... checkout only to output pure-raster image files ...
             PRINT *,' NTOTWDS=',NTOTWDS
             PRINT *,' NOWIDTH=', NOWIDTH
             PRINT *,' NOLINES=', NOLINES
C            LUNRAS = 89
C            WRITE(LUNRAS,ERR=900) (IBITPLANE(I),I=1,NTOTWDS)
C?????????????????????????????????????????????????
            imgtyp = 0    !... main body of map
C
            CALL img2x6t(imgtyp,IBITPLANE,NTOTWDS,NOWIDTH,NOLINES,
     1              IWINDOW(IPXL_FR_MAX),NOLINES,iret_ras2)
C
            if(iret_ras2 .NE. 0) then
             write(6,FMT='(1h ,''main::img2x6t:(main) Failed with '',
     1                     ''retn code='',I5)')
     A           iret_ras2

              IRET_CNT = 4
              go to 800
            endif
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... WAS ANY STRIP TITLES  WRITTEN TO IMAGE_STR ??
             IF(NITMPLTED_STR .GT. 0) THEN

              IMAGSIZ_STR = MAXIWORD_STR * MAXJSLINE_STR
              NPXLOUT_STR = 1728
              NROWSHOW_STR = MAXJSLINE_STR   !... = 800
              LNCOUNT_STR = 800              !... might be reduced by dataj

              imgtyp = 1             !... passing the strip-title image
              CALL img2x6t(imgtyp,IMAGE_STR,IMAGSIZ_STR, maxiword_STR,
     1                     lncount_STR,
     2                     npxlout_STR, nrowshow_STR,iret_ras2)

              if(iret_ras2 .NE. 0) then
           write(6,FMT='(1h ,''main::img2x6t:(strp-titles) Failed '',
     1                       ''with retn code='',I5)')
     A             iret_ras2
                IRET_CNT = 5
                go to 800
              endif
C           ELSE
C        write(6,FMT='(1h ,''main: skipped img2x6t() for converting '',
C    1                     ''strip-title image into X6B code'',
C    2                /1h ,7X,''because no title data was plotted'')')
            ENDIF
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C      ================================================================

            ISCHED(1,1) = IFAXNO
            IFAXFG = 0
            CALL COMIFID(IFAXNO,JFID,IFAXFG,CIFID)
            PRINT *,' CIFID=',(CIFID(M),M=1,48)
            MYOPTNBITS = IBSET(MYOPTNBITS,7)  !... GIVEN IFID IS IN ASCII
ckumar
            NUMRECFAX = NRECFAX
            LUNIPK6 = IFAXOUT

            CALL REBLKFX4(LUNEXT6B,LUNIPK6,NUMRECFAX,ISCHED,CIFID,
     1      MYOPTNBITS, iret_reb)
ckumar
ckumar We decrement NRECFAX by 1 to eliminate zero filled record
ckumar
            NRECFAX = NUMRECFAX-1
ckumar
C
C           write(6,FMT='(1h ,''main::reblkfx4: return code='',I6)')
C    A         iret_reb

             NUMGOOD = NUMGOOD + 1
             SUBNUMS(NUMGOOD) = IFAXNO
            ENDIF
           ENDIF
C
  800      CONTINUE
           MWHICH = MWHICH + 1
           GRPWHICH = GRPWHICH + 1
           CLOSE(UNIT=55)
C
           IF (IRET_RAS2 .NE. 0) IERR = IRET_RAS2
           IF (IRET_REB  .NE. 0) IERR = IRET_REB 
C
      GO TO 100
C
  900 CONTINUE
C
C     ....FINISH ALL MAPS
C
ckumar
ckumar      IFCBF(1) = KFLAG
ckumar      WRITE(IFAXOUT)IFCBF
ckumar Writing the final record in direct access mode.
ckumar
        CALL ENDMAP(IFAXOUT,NRECFAX)
        CLOSE (IFAXOUT)
C
C     .....WRITE END-OF-MAP AT THE END VARIAN FILE
C
      IF(NUMGOOD.LE.0)THEN
          WRITE(ERRMSG(3)(1:55),
     1     FMT='('' FATAL ERROR - NO FAX MAP HAS BEEN MADE. '',
     2     ''PLEASE CHECK:'')')
C    1            '123456789012345678901234567890123456789012345',
C    2     ''6789012345'')')
          NUMGOOD = 0
          IEXIT = 3
          PRINT *,' '
          PRINT *,'-------------------------------------------------'
          PRINT *,ERRMSG(3)
      ELSE
C
          WRITE(ERRMSG(1)(1:55),
     1     FMT='(''MADE'',I3,'' OF'',I3,'' FAX MAPS THE SUBSET '',
     2     ''NUMBERS ARE:         '')')NUMGOOD,MAXMAP
C    1        '123456789012345678901234567890123456789012345',
          PRINT *,' '
          PRINT *,'-------------------------------------------------'
          WRITE(6,FMT='('' MAIN:'',A)')ERRMSG(1)(1:55)
          IF(NUMGOOD.GE.1)THEN
           EXTRA = .TRUE.
          ELSE
           EXTRA = .FALSE.
          ENDIF
      ENDIF
C
  999 CONTINUE
C
C
      IF(IEXIT.EQ.0)THEN
       ISTOP = 1
      ELSE
       ISTOP = IEXIT
       IERR = IEXIT
      ENDIF
C
      ENDMSG = ERRMSG(ISTOP)
      CALL CONSOL(ENDMSG)
C
      IF(EXTRA)THEN
C
           NUMT = 0
           NUMT = NUMGOOD/10
           IREM = MOD(NUMGOOD,10)
           IF(IREM.NE.0)THEN
            NUMT = NUMT + 1
           ENDIF
           ISRT  = 1
           IF(NUMGOOD.LE.10)THEN
            IEND = NUMGOOD
            LEFT = 0
            JEND = NUMGOOD
           ELSE
            IEND = 10
            JEND = 10
            LEFT = NUMGOOD - 10
           ENDIF
C
C            WRITE(6,FMT='('' MAIN: MAKING'',I2,'' EXTRA CONSOL ''
C    1        ''MESSAGES BECAUSE !'')')NUMT
C
         DO KK = 1,NUMT
             EXTRATIT = ' '
             WRITE(EXTRATIT(1:95),FMT='(''SUBN='',20(I4,1X))')
     1       (SUBNUMS(II),II=ISRT,IEND)
C
               KLOC = JEND*5+5
C
             EXTRATIT(KLOC:KLOC) = ':'
             WRITE(6,FMT='('' MAIN:'',A)')EXTRATIT(1:KLOC)
             CALL CONSOL(EXTRATIT)
C   
C             CHECK TO SEE IF WE ARE FINISHED
C
             IF(LEFT.EQ.0)THEN
             ELSE
               ISRT = IEND + 1
               IF(LEFT.LE.10)THEN
                 IEND = NUMGOOD
                 JEND = NUMGOOD
                 LEFT = 0
               ELSE
                 IEND = ISRT + 10
                 LEFT = NUMGOOD - IEND
               ENDIF
             ENDIF
         ENDDO
      ENDIF
C
      PRINT *,"-------------------------------------------------"
C
      CALL W3TAGE('FAXMAKRX')
C     CALL W3AS02 (ISTOP)
      CALL ERREXIT(IERR)              ! cray EXIT modified for IBM/SP      
      STOP
      END
