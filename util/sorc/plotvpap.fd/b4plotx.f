      SUBROUTINE b4plotx(LWNDONLY,lvldes,idelta,itout,LMARGIN,
     1                   AFOS,IERROR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    b4plotx     DECODE DATA / FORM AFOS PLOTFILE
C   PRGMMR: shimomura        ORG: W/NP12     DATE: 97-03-10
C
C ABSTRACT: GIVEN THE OBSERVATIONAL DATA PACKED INTO REAL*8 NDATA ARRAY
C   THE TASKS ARE TO DECODE EACH OBS IN NDATA, INTERPRET THE DATA,
C   POSITION THE STN ON THE MAP, DECIDE WHAT QUANTITIES ARE
C   TO BE PLOTTED, REFORMAT DATA AND CALL ON SUBROUTINE PLTDAT
C   TO PLOT THE OBS ACCORDING TO THE PLOTTING MODEL.
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR: DAVID SHIMOMURA
C   88-07-22  GLORIA DENT CHANGE THE AFOS ZOOM THRESHOLD OF 72273 (FT.
C               HUACHUCA) FROM ZOOM1 TO ZOOM 16. CHANGE THE AFOS ZOOM
C               THRESHOLD OF 76458 (MAZATLAN,MX) FROM ZOOM 3 TO ZOOM 1
C   88-08-11  GLORIA DENT MOVE THE PLOTTING PARAMETERS DOWN OF STATION
C               72273 (FT. HUACHUA) SO THAT THEY WILL NOT OVERPLOT
C               STATION 72274 (TUCSON) ON THE VARIAN CHART.
C   89-04-26  HENRICHSEN ADDED ARG LMARGIN  A LOGICAL   WORD THAT IS
C                        SET = .TRUE. IF STATIONS ARE TO BE PLOTTED
C                        IN MARGIN TO AVOID OVER PLOTTING.
C                        CHANGED LAST ARG TO A LOGICAL   WORD THAT IS
C                        SET = .TRUE. IF AFOS PLOTFILE IS TO BE MADE.
C                        CLEANED UP AND DOCUMENTED. ADDED NEW SUB
C                        AFZOOM.
C   89-06-09  LILLY MOVED THE PLOTTING PARAMETERS WEST OF STATION
C                        72435 (PADUCAH) AND EAST OF STATION 72327
C                        (NASHVILLE/BERRY) SO THAT THEY WILL NOT
C                        OVERPLOT ON THE VARIAN CHART.
C   89-06-12  LILLY MOVED THE PLOTTING PARAMETERS NORTH OF STATION
C                        72429 (DAYTON) AND SOUTH OF STATION 72425
C                        (HUNYINHYON) SO THAT THEY WILL NOT
C                        OVERPLOT ON THE VARIAN CHART.
C   89-06-21  LILLY NOT TO PLOT STATION CIRCLE/WIND BAR IF STATION
C                        74671 (FORT CAMPBELL) OCCURRS.
C   89-07-21  LILLY NOT TO PLOT STATION CIRCLE/WIND BAR IF STATIONS
C                        72269 (WHITE SANDS) OR 74734 (WHITE SANDS
C                        MISSILE) OCCURRS
C   89-09-13  LILLY ADDED LOGIC WHICH MAY TEST THE PLOTTING LOCATION
C                        OF 2 NEARBY STATIONS.  NWS STATIONS
C                        OBSERVATION HAVE A HIGHER PRIORITY OVER
C                        MILITARY.  HENCE, IF BOTH NWS AND MILITARY
C                        OBSERVATIONS ARE REPORTED, THE MILITARY
C                        OBSERVATIONS IS MOVED TO PREVENT OVERPLOTTING.
C                        OTHERWISE, THE MILITARY REMAINS.
C   89-11-06  LILLY ADD LOGIC WHICH PREVENTS SPECIFIED MILITARY
C                        UPPER AIR REPORTS FROM BEING PLOTTED AT
C                        THE TOP OF THE VARIAN PRODUCT DUE TO DATA
C                        OVERWRITING DATA RECEIVED FROM A NWS STATION.
C                        THIS LOGIC CONFIRMS WHETHER BOTH, ONLY ONE,
C                        OR NEITHER OBSERVATIONS ARE PRESENT.  IF BOTH
C                        STATIONS REPORT, THE MILITARY OBSERVATION IS
C                        MOVED TO THE TOP TO THE VARIAN PRODUCT.  IF
C                        THE NWS DOES NOT REPORT, THE MILITARY
C                        OBSERVATION IS PLOTTED NORMALLY.
C   90-04-23  LILLY ADD LOGIC WHICH WILL PREVENT REPEATING PREVIOUS
C                        OBSERVATION FOR LEVELS WHICH ARE MISSING.
C   93-05-08  LILLY CONVERT SUB. TO FORTRAN 77
C   97-01-24  SHIMOMURA: Convert to run on CRAY;
C                        Changed call sequence to split items which
C                          had been hidden in half words:
C                        OLD ARG(1) LVLDET is now LWNDONLY,LVLDES;
C                        OLD ARG(3) ITOUCH is now IDELTA,ITOUT; 
C                        Changed expected value in LWNDONLY
C                          which used to be integer = 1 or =2;
C                          changed to a logical LWNDONLY = .T. OR .F. 
C                        Changed source of input data;
C                        Input OBS data includes 12-hr hgt chg, so I do
C                           not need RIDOBS array.
C                        Removed MOPTN (which was for extended IOPTN)
C                         because CRAY I*8 has enough room in same word.
C                        Extracted AFOS PLOTFILE generating and put
C                           that logic into subr GENAFPLT()
C                        Removed OLD ARG: KADDZ -- additive constant for
C                           converting D-value to actual Height is not
C                           necessary because I have the Height in data.
C 
C   97-02-10  LUKE LIN:  Added external plot-position adjustments.
C                        Re-activated sections of commented out code
C                        near the end;
C                        Commented out the AFOS Plotfile generator;
C
C   97-02-27  SHIMOMURA: Changed to plot only 2-digits when zero-valued
C                           temperature instead of 3 zeros;
C                        Corrected test for excessive hgt-chg values;
C                        Corrected to allow "LV" to plot in lower-right;
C
C   97-03-10  SHIMOMURA: Added ITOUT=14 for large shemi 1:20M map
C
C   97-04-17  SHIMOMURA: Changed 12-hr hgt-chg value to be decameters
C                           for all levels.
C
C USAGE:   CALL b4plotx(LWNDONLY,lvldes,idelta,itout,LMARGIN,AFOS)
C
C   INPUT ARGUMENT LIST:
C     LWNDONLY - LOGICAL option: plotting model
C              _ LWNDONLY = .T.  FOR WIND-ONLY PLOTTING MODEL
C              - LWNDONLY = .F.  FOR MORE ELABORATE PLOTTING MODEL
C     lvldes   - THE LEVEL DESIRED (How indicated?)
C
C
C     IDELTA   - specifies the displacement (in pixels) to move the plot 
C              -         to the right-hand panel of a 2-panel chart;
C              - =0; For the more common case of single chart.
C
C     ITOUT    - CONTROLS SEVERAL OPTIONS ON MAP AND TYPE OF
C              -    PLOTTING.  ACCEPTABLE VALUES [1 TO 14]
C
C     LMARGIN  - LOGICAL switch 
C              = .TRUE. IF PLOTTING in the margin IS
C              -         DESIRED FOR FAX CHART TO AVOID OVER PLOTTING.
C     AFOS     - LOGICAL switch
C              = .TRUE. IF AFOS PLOTFILES ARE TO BE MADE FROM THIS DATA;
C              = .FALSE.  if no AFOS output;
C
C     COMMON /ADJUST/ IXADJ,IYADJ
C
C   OUTPUT ARGUMENT LIST:
C     COMMON   - /WLONG0/ WLONG0
C     COMMON   - /JSPACE/IDREC(6),NDATA(3,4001)
C     COMMON   - / MTNS /NAMTMP(9)
C     COMMON   - / LEVEL/NLEVEL,LCNTR
C
C   INPUT FILES:
C     FT26F001 - A LIST OF STATIONS FOR POSSIBLE MARGIN PLOTTING AT TOP.
C            ... WHEN 1-DOT/2-DOTS VARIAN/FAX CHARTS ARE GENERATED, 
C            ... THE PROGRAM CHECKS THE REPORTING STATION ACROSS NORTH
C            ... AMERICA (STATIONS ID BETWEEN 70000 AND 75000) AGAINST
C            ... A TABLE OF KNOWN OVERLAYING STATIONS(PLOTMLTY)
C            ... WHICH IS READ IN FROM UNIT:26
C
C   OUTPUT FILES:
C     FT06F001 - STANDARD PRINT FILE.
C     COMMON   - /ISPACE/LBLOCK,ICNTOT
C
C REMARKS:
C   Some of the confusion in this is a result of double-use of zones
C   in the plotting model;  
C      the lower-left zone is for the dew-point depression;
C         but in some cases it is used for wind direction in text;
C         and in some cases it is used for observation time:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C
C     -----------------------------------------------------------------

      EXTERNAL   LASTCH
      INTEGER    LASTCH			!... INT FUNCTION

      INTEGER    LUNOVRL		!... U:CONSTANTS OVRLAPPING STNS
      PARAMETER (LUNOVRL=26)


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

      COMMON  /ADJUST/ IXADJ,IYADJ

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C     ...    FOR UNPACKING THE RUNNING 32-BIT WORDS ...

      INTEGER     IOBS2PKWORK(LMTWRDPOB)

      INTEGER     LONELVL(LMTHFWPOB)
      REAL        FLONELVL(LMTHFWPOB)
      INTEGER     NOFFSET
      DATA        NOFFSET    /  0 /
      INTEGER     NBITSGRP
      DATA        NBITSGRP   / 32 /
      INTEGER     NPADBITS
      DATA        NPADBITS   /  0 /
      INTEGER     NGRPS2DO
      DATA        NGRPS2DO   / 20 /
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      INTEGER     KPTR_LAT
      DATA        KPTR_LAT    / 1 /
      INTEGER     KPTR_LON
      DATA        KPTR_LON    / 2 /
 
      INTEGER     KPTR_ZZ
      DATA        KPTR_ZZ    / 13 /
      INTEGER     KPTR_TT
      DATA        KPTR_TT    / 14 /
      INTEGER     KPTR_TD
      DATA        KPTR_TD    / 15 /
      INTEGER     KPTR_DDD
      DATA        KPTR_DDD   / 16 /
      INTEGER     KPTR_FFF
      DATA        KPTR_FFF   / 17 /
      INTEGER     KPTR_DZ
      DATA        KPTR_DZ    / 18 /
      integer     kptr_qmark
      data        kptr_qmark     / 19 /
      integer     kptr_bitwrd
      data        kptr_bitwrd    / 20 /

      INTEGER     NEGSIGNEXT
      DATA        NEGSIGNEXT  / X'FFFFFFFF00000000' /
      INTEGER     MISGB12
      DATA        MISGB12     / X'000000007FFFF000' /
      INTEGER     MISGB0
      DATA        MISGB0      / X'000000000007FFFF' /

      REAL        TT_T, DEW_T, TDEWDEPR
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

      COMMON  /JSPACE/IDREC(6),NDATA
      INTEGER  NDATA(3,NDATASIZ)
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .



      COMMON  /WLONG0/ WLONG0
      COMMON  / MTNS /NAMTMP(9)


      COMMON  / LKTLBS/TLIM,LOOKT
      INTEGER   LOOKT(9,63),TLIM
      COMMON  / LEVEL/NLEVEL,LCNTR
C
C     . . . . . . . .   C A L L   S E Q U E N C E   . . . . . . . . . 
C USAGE:   CALL b4plotx(LWNDONLY,lvldes,idelta,itout,LMARGIN,AFOS)
      LOGICAL   LWNDONLY
      INTEGER   LVLDES
      INTEGER   IDELTA
      INTEGER   ITOUT
      LOGICAL   LMARGIN
      LOGICAL   AFOS

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C     . . . . CALL SEQUENCE FOR CALLING GENAFPLT()  . . . . . . . . . 
C     
C      USAGE: CALL GENAFPLT(ITOUT,LVLDES,C8NAME,KTTYP,IOPTN,
C ...     1              ALAT,ALONG,L,ND1_AF,
C ...     2              IHSSS_AF,IHZZ,IHTT,IHDEWPT,IHFFF,IRET_AFP)
C
     
C ...      INTEGER      ITOUT
C ...      INTEGER      LVLDES
C ...      CHARACTER*8  C8NAME
      INTEGER      KTTYP
C ...      INTEGER      IOPTN
      REAL         ALAT,ALONG
      CHARACTER*1  L
      INTEGER      ND1_AF
      CHARACTER*4  IHSSS_AF
      CHARACTER*4  IHTT
      CHARACTER*4  IHDEWPT		!... IDEWPT(1:NDCHAR)
      CHARACTER*4  IHFFF
      CHARACTER*4  IHZZ
      INTEGER      IRET_AFP

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

      character*4   ihdewdep
      integer*8     intdd
      equivalence  (intdd,ihdewdep)

      character*4   ihzzz
      character*4   ihtd

      REAL          XI,XJ
      REAL          VSCALE
      INTEGER       JO
      INTEGER       JCORN

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C
      LOGICAL    LNEGQ
C
      LOGICAL      BRNTST
      LOGICAL      LSWPLT
      LOGICAL      PLTDTA
C

C ...      EQUIVALENCE(IACC,INGLES(1))
C ...      EQUIVALENCE(MQ,INGLES(2))

      integer      iacc			!... NOT equivalenced to

      INTEGER      ijwrd		!... adata(1,M)
      INTEGER      namwrd		!... adata(2,M)
      integer      iptrwrd		!... adata(3,M)
C
      integer      nbitwrd
      character*1  c1nbitwrd(8)
      equivalence (nbitwrd,c1nbitwrd(1))

      INTEGER    JFLAG(12,100)
      INTEGER    KFLAG(12,100)
      INTEGER    KAME(100)
      CHARACTER*8  JAMSTN(100)
      CHARACTER*8  KAMSTN(100)
      INTEGER    JIDDGD(12,100)
      INTEGER    KIDDGD(12,100)

      character*4  Jihdewdep(12,100)
      character*4  Kihdewdep(12,100)
      integer    Jnorwndir(12,100)
      integer    Knorwndir(12,100)
      character*4  JIHFFF(12,100)
      character*4  KIHFFF(12,100)
      character*4  JIHTT(12,100)
      character*4  KIHTT(12,100)
      character*4  JIHZZZ(12,100)
      character*4  KIHZZZ(12,100)

      INTEGER    JIO(12,100)
      INTEGER    KIO(12,100)
      INTEGER    JJOPTN(12,100)
      INTEGER    KKOPTN(12,100)
      INTEGER    KIOPTN(12,100)
      INTEGER    JJHT1(12,100)
      INTEGER    KJHT1(12,100)
      INTEGER    JJO(12,100)
      INTEGER    KJO(12,100)
      INTEGER    JKEIL(12,100)
      INTEGER    KKEIL(12,100)

C     ... FOLLOWING CONSTANTS INPUT FROM UNIT:LUNOVRL ...
      CHARACTER*80 COMMENT_CARD
      INTEGER      NLIST_1		!... COUNT OF LIST-1 ITEMS
      CHARACTER*6  OVRSTN(100)		!... STN NAMES FROM LIST-1
      CHARACTER*7  ILATLON(100)		!... FROM LIST-1 "12N456W"

      INTEGER      NLIST_2		!... COUNT OF LIST-2 ITEMS
      CHARACTER*6  NWSSTN(100)		!... STN NAMES FROM LIST-2(LHS)
      CHARACTER*7  JLATLON(100)         
      CHARACTER*6  MILSTN(100)		!... STN NAMES FROM LIST-2(RHS)
      CHARACTER*7  KLATLON(100)
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

      INTEGER    KNDD(12,100)
      INTEGER    JNSSS(12,100)
      INTEGER    KNSSS(12,100)
 
      INTEGER       NUNPAK(9)

      INTEGER       LOOKA
      INTEGER       LOOKB

      INTEGER*8     NAMSTN
      CHARACTER*8   C8NAMSTN
      EQUIVALENCE  (NAMSTN,C8NAMSTN)

C     . . . . . . . .   SPECIAL STATIONS  . . .

C  STATION FORT CAMPBELL
      CHARACTER*8  CMPBLL
      DATA         CMPBLL    / '74671   '/
      CHARACTER*8  BRNSVL
      DATA         BRNSVL    / '72250   ' /
      CHARACTER*8  STNDWN
      DATA         STNDWN    / '72273   ' /
C  STATION PADUCAH (72435)
      CHARACTER*8  STNWST
      DATA         STNWST    / '72435   ' /
C  STATION NASHVILLE/BERRY (72327)
      CHARACTER*8  STNEST
      DATA         STNEST    / '72327   ' /
C  STATION DAYTON (72429)
      CHARACTER*8  STNNTH
      DATA         STNNTH    / '72429   ' /
C  STATION HUNTINGTON (72425)
      CHARACTER*8  STNSTH
      DATA         STNSTH    / '72425   ' /
C  STATION WHITE SANDS
      CHARACTER*8  WSNDS
      DATA         WSNDS     / '72269   ' /
C  STATION WHITE SANDS MISSILE
      CHARACTER*8  WSNDM
      data         WSNDM     / '74734   ' /

C     . . . . . . . . . . . . . . . . . . . . .

      INTEGER      NAME
      CHARACTER*8  C8NAME
      EQUIVALENCE (NAME,C8NAME)

        INTEGER      NAME_B
        CHARACTER*8  C8NAME_B
        EQUIVALENCE (NAME_B,C8NAME_B)

      INTEGER    TEXT
      INTEGER    ICNT
      INTEGER    JCOUNT
      INTEGER    NLEVEL
      INTEGER    LCNTR
C
C


      CHARACTER*1  LITBLK
      DATA         LITBLK    / ' ' /

      CHARACTER*1  LITPND
      DATA         LITPND    / '#' /



      CHARACTER*1  LIT0
      DATA         LIT0 /'0'/
      CHARACTER*1  LIT8
      DATA         LIT8 /'8'/
      CHARACTER*1  LITA
      DATA         LITA /'A'/
      CHARACTER*1  LITS
      DATA         LITS /'S'/
C
      integer      kbar1
      CHARACTER*8  KBAR
      DATA         KBAR             / '######  ' /
      equivalence (kbar1,kbar)

      INTEGER      NAMRC1
      CHARACTER*6  NAMRC
      EQUIVALENCE (NAMRC1,NAMRC)
      DATA         NAMRC            / '#R####  ' /

      INTEGER      NAMBOG1
      CHARACTER*6  NAMBOG
      EQUIVALENCE (NAMBOG1,NAMBOG)
      DATA         NAMBOG           / '#B####  ' /

      INTEGER      NAMCAR1
      CHARACTER*6  NAMCAR
      EQUIVALENCE (NAMCAR1,NAMCAR)
      DATA         NAMCAR           / '#A####  ' /

      INTEGER*8    NAMOV

      INTEGER      NSPL
      PARAMETER   (NSPL=4)

      INTEGER*8    NAMSPL(NSPL)
      DATA         NAMSPL     / X'3732343033200800',	!... 72403
     2                          X'3732323636201000',    !... 72262
     3                          X'3732323539201000', 	!... 72259
     4                          X'3730333631200800' / 	!... 70361


      CHARACTER*4  C4TL
      DATA         C4TL    /'TL$ '/
C
      CHARACTER*4  C4NR
      DATA         C4NR    /'NR$ '/

      CHARACTER*4  C4MISF
      DATA         C4MISF  /'M$  '/

      CHARACTER*4  C4LVRB
      DATA         C4LVRB  /'LV$ '/
C

      INTEGER       JBCD
      CHARACTER*8   CBCD
      EQUIVALENCE  (JBCD,CBCD)


C

C
      DATA     ITOPMX  /90/
C     ...WHERE ITOPMX IS MAX N0. OF STNS PLOTTED AT TOP OF PAGE...
      DATA     KROWMX  /30/
C     ...WHERE KROWMX IS MAX NO. OF STNS PER ROW AT TOP-OF-PAGE PLOT
C
      DATA     KBLK6    /Z'00200000'/
      DATA     KBLK56   /Z'20200000'/

      DATA     MSK56    /Z'FFFF0000'/
      DATA     MSKBYT6   /Z'00FF0000'/
      DATA     MSKBYT5   /Z'FF000000'/

       INTEGER     MSKLHS
       DATA        MSKLHS        / X'FFFFFFFF00000000' /
       INTEGER     MSKRHS
       DATA        MSKRHS        / X'00000000FFFFFFFF' /
       INTEGER     MSKCH1TO5
       DATA        MSKCH1TO5     / X'FFFFFFFFFF000000' /

      DATA     IKDOT    /46/
      DATA     JKDOT    /30/

      DATA     KSTRI    /190/
C     ...CAUTION...KSTRI MUST CHANGE W/ INDENT VALUE OF VARIAN

      DATA     KSTRJ    /1875/
C     ...WHERE KSTRJ IS TOP OF REGULAR MAP BCKGRND AS IN IL(10)

      DATA     LMXWND    /27/

      DATA     KZBUT  /Z'40'/
      DATA     KTBUT  /Z'20'/

      DATA     KWBIT    /8/
      DATA     KZBIT    /4/
      DATA     KTBIT    /2/
      DATA     KTDBIT   /1/
C     ...ABOVE 4 BITS ARE FOR GRANPA FORMAT IIII BIT TEST...

      INTEGER  NBITS		!... VS. KWBIT, KZBIT, KTBIT, TTDBIT

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


C     ... FOLLOWING BITS ARE FOR IOPTN FOR S/R PLTDAT ...
      integer      jsouth
      DATA         JSOUTH    / X'0000000100000000' /
C     ...TO MOVE ALL PARAMETER SOUTH 4 DOTS EXCEPT STA CIRCLE/WIND BARB
      integer      kdown
      DATA         KDOWN     / Z'80000000' /
C     ...TO MOVE ALL PARAMETER DOWN 12 DOTS EXCEPT STA CIRCLE/WIND BARB
      integer      iwest
      DATA         IWEST     / Z'40000000' /
C     ...TO MOVE ALL PARAMETER WEST 4 DOTS EXCEPT STA CIRCLE/WIND BARB
      integer      ieast
      DATA         IEAST     / Z'20000000' /
C     ...TO MOVE ALL PARAMETER EAST 4 DOTS EXCEPT STA CIRCLE/WIND BARB
      integer      jnorth
      DATA         JNORTH    / Z'10000000' /
C     ...TO MOVE ALL PARAMETER NORTH 4 DOTS EXCEPT STA CIRCLE/WIND BARB
      INTEGER      KMVTR
      DATA         KMVTR     / Z'08000000' /
C     ...WHERE KMVTR: TO MOVE TT TO UPPER RIGHT OF STN CIRCLE
      INTEGER      KMEDW
      DATA         KMEDW     / Z'04000000' /
C     ...WHERE KMEDW IS  MED-SMALL CHAR FOR HGT ON WINDS ALOFT CHART
      INTEGER      KMTMP
      DATA         KMTMP     / Z'02000000' /
C     ...TO PLOT 850 MB CALC TMP ABOVE REPORTED TMP
C     ...TO MOVE BRACKETS CLOSER TO DATA ON SIRS PLOTTED CHART.
      INTEGER      KCLOS
      DATA         KCLOS     / Z'01000000' /
C     ...WHERE KCLOS MOVE BOGUS 'B' OR ACAR 'A' CLOSER TO STN CIRCLE...
      INTEGER      KANGB
      DATA         KANGB     / Z'00800000' /
C     ...WHERE KANGB IS TO SET ANGLE TO 90.0 FOR SIDEWAYS MODEL
      INTEGER      KSMNB
      DATA         KSMNB     / Z'00400000' /
C     ...WHERE KSMNB IS FOR SMALL CHARS IN STN NAME...
      INTEGER      KCLZB
      DATA         KCLZB     / Z'00200000' /
C     ...WHERE KCLZB IS A CALCULATED GEOPOTENTIAL HGT
      INTEGER      KCLTB
      DATA         KCLTB     / Z'00100000' /
C     ...WHERE KCLTB IS FOR A CALCULATED TEMPERATURE
      INTEGER      KMEDB
      DATA         KMEDB     / Z'00080000' /
C     ...WHERE KMEDB IS MEDIUM-SMALL CHAR FOR TEMP ON IQSY PLOT
      INTEGER      KMVWB
      DATA         KMVWB     / Z'00040000' /
C     ...FOR MOVING WIND BARBS FARTHER OUT THE STAFF TO PREVENT OVRPLOT
      INTEGER      KSMLB
      DATA         KSMLB     / Z'00020000' /
      INTEGER      KMVNB
      DATA         KMVNB     / Z'00010000' /
      INTEGER      KTALB
      DATA         KTALB     / Z'00008000' /
      INTEGER      KSTRB
      DATA         KSTRB     / Z'00004000' /
      INTEGER      KSQRB
      DATA         KSQRB     / Z'00002000' /
      INTEGER      KMVDB
      DATA         KMVDB     / Z'00001000' /     
      INTEGER      KMVZB
      DATA         KMVZB     / Z'00000800' /
      INTEGER      KDOVB
      DATA         KDOVB     / Z'00000400' /
C     ...WHERE KDOVB FOR WIND DIR DIGIT PLOTTED NR END OF STAFF
      INTEGER      KMVTB     
      DATA         KMVTB     / Z'00000200' /
      integer      ksmcb
      data         ksmcb     / Z'00000100' /
      INTEGER      KVECB
      DATA         KVECB     / Z'00000080' /
      INTEGER      KNAMB
      DATA         KNAMB     / Z'00000040' /
      INTEGER      KZZZB
      DATA         KZZZB     / Z'00000020' /
      INTEGER      KFFFB
      DATA         KFFFB     / Z'00000010' /
      INTEGER      KDEWB
      DATA         KDEWB      / Z'00000008' /
      INTEGER      KTTB
      DATA         KTTB      / Z'00000004' /
      INTEGER      KOCB
      DATA         KOCB      / Z'00000002' /
      INTEGER      KCIQB
      DATA         KCIQB     / Z'00000001' /
C     ...ABOVE BITS ARE FOR IOPTN FOR S/R PLTDAT
      

      INTEGER      MIOPTN
      data         MIOPTN     / Z'00000081' /

      INTEGER      ISCHRQ
      INTEGER      IMVWQ
      INTEGER      IANGLQ
      integer      IOPTN
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      character*4  khdry
      DATA         KHDRY    / '#X$ ' /
C     ...FOR DEWPT DEPRESS PLOT OF X INSTEAD OF 30 FOR DRY AIR


      DATA     KDOL1 /Z'00000024'/
      DATA     KDOL2 /Z'00002400'/
      DATA   INAMRC  /X'5200002300000000'/
C                       R      #

      INTEGER      MSKBST			!... BLK+STN NAME MASK
      DATA         MSKBST      / X'FFFFFFFFFFFF0000' /
      INTEGER      MSK6BITS
      DATA         MSK6BITS    / X'0000003F' /
      DATA     MSKOFF/Z'0000000F'/
      DATA     MSKPRI/Z'1F'/
      DATA     MSKTYP/Z'3F'/
      DATA     MSK8  /Z'FF'/
      DATA     MSK12 /Z'FFF'/
      DATA     MSK18 /Z'3FFFF'/
      DATA     NO777 /Z'1FF'/
      DATA     NO17  /Z'F'/
      DATA     NO360 /Z'F0'/

      INTEGER  MASK				!... LBIT=16
      DATA     MASK        / Z'00010000' /
C     ... MASK IS A BIT IN THE NAMSTN LONGWORD TO SIGNAL THAT
C     ...A CLOSED STATION CIRCLE IS DESIRED...

      DATA     MSKBLK    /Z'200000'/
      DATA     NEGEXT    /X'FFFFFFFFFFFFFFC0'/
C     ...WHICH IS USED FOR SIGN EXTENSION OF NEW TYPE...
      DATA     NEGEX2    /X'FFFFFFFFFFFFF000'/

      DATA     MSKFF     /Z'FF000000'/
      INTEGER    BLANK

      DATA     BLANK     /Z'40'/
      DATA     MSKL8     /Z'FF000000'/
      DATA     MRKR13    /Z'1000'/

      DATA     CNVFT     /3.28083333/
      DATA     CNVDFT    /32.80833333/
      DATA     TWO9      /512.0/
      DATA     DISSAT    /6.619/
      DATA     SCALE     /26.7858/
C
      INTEGER  LBIT_TOSSED
      DATA     LBIT_TOSSED     / 41 /

      INTEGER  IPRIOR(2)		!... for PUTLAB Arg
      DATA     IPRIOR          / 0, 1 /
C
      SAVE
C
C     . . . .   S T A R T   . . . . . . . . . . . . . . . . . . . . . 
      IERROR = 0

      LCNTR = 0
      PLTDTA=.FALSE.
C
      IDEWQQ = 1
      IMVWQ = KMVWB
      ITOPLQ = 0
C     ...ITOPLQ IS WHETHER TOP PLOTTING IS DESIRED...
      ITOPI = 0
C     ...WHERE ITOPI INDEXES THE TOSSED OBS TO PLOT AT TOP OF PAGE...
      ISCHRQ = 0
C     ... TO HOLD SMALL-CHAR OPTION BIT...
      IANGLQ = 0
C     ...TO HOLD SIDEWAYS PLOTTING OPTION BIT ...
C
      ICORN = 0
      JCORN = 0
C     ...TO DETERMINE KEIL AND VERTLN FROM GIVEN ITOUT...
      IF(ITOUT .LT. 1) GO TO 910
      IF(ITOUT .GT. 14) GO TO 910
C             1   2   3   4   5   6   7   8   9  10  11  12  13
C ... GO TO (110,120,130,140,150,160,170,180,190,200,205,220,220),ITOUT

      IF(ITOUT .EQ. 1) THEN
        KEIL = 1
        VERTLN = 105.0
        VSCALE = 37.5
        ICORN = 2 + IXADJ
        JCORN = 2 + IYADJ

      ELSE IF(ITOUT .EQ. 2) THEN		!... 2-DOT FROM LUKE
        KEIL = 1
        VERTLN = 105.0
        VSCALE = 37.5
C ...        ICORN = 113
C ...        JCORN = 145
        ICORN = IXADJ
        JCORN = IYADJ

      ELSE IF(ITOUT .EQ. 3) THEN
C       ...COMES HERE FOR IQSY UAB PLOTTED NRN HEMI ...
        KEIL = 2
C       ...FOR STD NMC OCTAGON...
        VERTLN = 80.0
        VSCALE = 37.5
        IMVWQ = 0
C       ...DO NOT MOVE WIND BARBS OUT ON STAFF IF IQSY MAP...
        ITOPLQ = 1
C       ...PLOT TOSSED STNS AT TOP OF PAGE
        ISCHRQ = KSMLB
        ICORN = IXADJ
        JCORN = IYADJ

      ELSE IF(ITOUT .EQ. 4) THEN
        KEIL = 15
        ICORN = 2 + IXADJ
        JCORN = 2 + IYADJ
        VERTLN = 105.0
        VSCALE = 37.5

      ELSE IF(ITOUT .EQ. 5) THEN
        KEIL = 3
C       ...FOR SRN HEMI...
        VERTLN = 80.0
        VSCALE = 37.5
        ISCHRQ = KSMLB
        ICORN = IXADJ
        JCORN = IYADJ

      ELSE IF(ITOUT .EQ. 6) THEN
C       ...COMES HERE FOR SIRS PLOTTED CHART
        KEIL = 2
        VERTLN = 80.0
        VSCALE = 37.5
        ISCHRQ = KSMLB
        ICORN = IXADJ
        JCORN = IYADJ

      ELSE IF(ITOUT .EQ. 7) THEN
C       ...COMES HERE FOR TROPIC PLOT ON MERC
        KEIL = 6
        VERTLN = 0.0
        VSCALE = 30.0
        ISCHRQ = KSMLB
        IMVWQ = 0
        IANGLQ = KANGB
C ...        ICORN = 5 + 12		!... 97-02-20
C ...        JCORN = 32
        ICORN = IXADJ
        JCORN = IYADJ

      ELSE IF(ITOUT .EQ. 8) THEN
C       ...COMES HERE FOR NH PLOT ON LARGE 1/20M MAP.
        KEIL = 7
        VERTLN = 105.0
        VSCALE = 37.5
        ICORN = 2 + IXADJ
        JCORN = 2 + IYADJ

      ELSE IF(ITOUT .EQ. 9) THEN
C       ...COMES HERE FOR WINDS ALOFT PLOT ON PN2601 ...
        KEIL = 4
        VERTLN = 98.0
        VSCALE = 28.846154
        IMVWQ=0
        IF(LWNDONLY) THEN
          IMVWQ = 0
          IF(LVLDES .EQ. LMXWND) GO TO 240
          IF(LVLDES .EQ. 25) GO TO 240
          PLTDTA=.TRUE.
        ELSE
          ISCHRQ=KSMLB
        ENDIF
        ICORN = IXADJ
        JCORN = IYADJ

      ELSE IF(ITOUT .EQ. 10) THEN	!... on Luke's NH 1:40M; krun=17
C       ...COMES HERE FOR GENERAL PLOTTING ON NH 1/40M BACKGROUND...
        KEIL = 2
        VERTLN = 80.0
        VSCALE = 37.5
        ISCHRQ = KSMLB
        ICORN = IXADJ
        JCORN = IYADJ

      ELSE IF(ITOUT .EQ. 11) THEN
C       ...COMES HERE FOR 1/60M PLOT...
        KEIL = 14
        VERTLN = 80.0
        VSCALE = 25.0
        ICORN = IXADJ
        JCORN = IYADJ
     
      ELSE IF((ITOUT .EQ. 12) .OR. (ITOUT .EQ. 13)) THEN
C       ...COME HERE FOR PSEUDO-GOES IMAGE ON GH2601 OR GH2602.
        KEIL = 6
C       ...THAT IS A DUMMY KEIL UNDER THESE OPTIONS.
        VERTLN = 0.0
        VSCALE = 50.0
        ISCHRQ = KSMLB
        IMVWQ = 0
        ICORN = IXADJ
        JCORN = IYADJ

      ELSE IF(ITOUT .EQ. 14) THEN
        KEIL = 16		!... for big 1:20M SHEMI SH2001 
C       ...FOR SRN HEMI...
        VERTLN = 260.0
        VSCALE = 37.5
        ISCHRQ = 0
        ICORN = IXADJ
        JCORN = IYADJ

      ENDIF
C
  240 CONTINUE
C
      IF(.NOT.LMARGIN) GOTO 241
C     ... A CONSTANTS LIST OF ALWAYS OVERLAPPING STATIONS ARE
C     ... READ IN FROM FROM UNIT:LUNOVRL TO WHICH IS ASSIGNED
C     ... THE CONSTANTS FILE: 'plotmlty.txt'
C     ... CONTAINING BLK+STNNUMB NAME; AND LATLONS 
C
      REWIND LUNOVRL
      lckpt = 700
      READ(LUNOVRL,700,ERR=920,END=930) COMMENT_CARD
  700 FORMAT(A)
      WRITE(6,701)LUNOVRL
  701 FORMAT(1H ,'b4plotx: Started reading constant overlap station ',
     1           'list from UNIT=',I4)
      WRITE(6,702)COMMENT_CARD(1:LASTCH(COMMENT_CARD))
  702 FORMAT(A)
      lckpt = 705
      READ(LUNOVRL,705,ERR=920,END=930) NLIST_1
      LCKPT = 705
C     PRINT *, ' b4plotx:at LCKPT=',LCKPT,'  NLIST_1=',NLIST_1
  705 FORMAT(1X,I3)
      lckpt = 715
      DO 710 I=1,NLIST_1
        READ(LUNOVRL,715,ERR=920,END=930) OVRSTN(I), ILATLON(I)
  715   FORMAT(1X,A6,A7)

C*    WRITE(6,761) OVRSTN(I),ILATLON(I)
C*761 FORMAT(1X,'IPROB1=',1X,A6,1X,'ILATLON=',1X,A7)
  710 CONTINUE
C
      lckpt = 721
      READ(LUNOVRL,700,ERR=920,END=930)COMMENT_CARD
      lckpt = 725
      READ(LUNOVRL,725,err=920,end=930) NLIST_2
  725 FORMAT(6X,I3)
      LCKPT = 725
C     PRINT *, ' b4plotx:at LCKPT=',LCKPT,' NLIST_2=',NLIST_2

      lckpt = 728
      DO 729 I=1,NLIST_2
        READ(LUNOVRL,728,ERR=920,END=930) NWSSTN(I), JLATLON(I),
     2         MILSTN(I), KLATLON(I)
  728   FORMAT(1X,A6,A7,1X,A6,A7)

C*    WRITE(6,762) NWSSTN(I),JLATLON(I)
  762 FORMAT(1X,'NWS=',1X,A6,1X,'JLATLON=',1X,A7)
C     WRITE(6,763) MILSTN(I),KLATLON(I)
  763 FORMAT(1X,'MILSTN=',1X,A6,1X,'KLATLON=',1X,A7)
  729 CONTINUE
C
C  KCNT - DEFINES THE CORRESPONDING LAT/LONG OF THE STATION WHICH
C         WAS MOVED.
C
      KCNT=0
      JCOUNT = 0
      ICNT = -1
      LCNTR = LCNTR + 1
      LCKPT = 739
      print *,' b4plotx:at LCKPT=',LCKPT,' lcntr0=',lcntr
C
      DO 739 KK=1,100
        JFLAG(LCNTR,KK) = 0
        KFLAG(LCNTR,KK) = 0
  739 CONTINUE
C      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  241 CONTINUE
      DO  777  M = 1,NOBSDB
C       ... fetch one obs from NDATA(,M) ...
        ijwrd   = NDATA(1,M)
        namwrd  = NDATA(2,M)
        iptrwrd = ndata(3,M)
        NAME = IAND(NAMWRD,MSKBST)
C       print 1099,NAME         
 1099   format(' NAME = ',A8)
        IF(ijwrd .EQ. 0) then
          write(6,FMT='(1H ,''b4plotx: Terminating scan thru data '',
     1               ''due to zero word at NDATA(1,'',I6,'')'',
     2            /1H ,7X,''NDATA(2,)= '',Z16,''  NDATA(3,)= '',Z16,
     3            /1H  )')
     A            M,NAMWRD,IPTRWRD
          GO TO 779
C         ...WHICH IS THE NORMAL EXIT ...
        ENDIF

        JTHISREP = iand(iptrwrd,mskrhs)
        DO I = 1,LMTWRDPOB
          IOBS2PKWORK(I) = IOBS2PK(I,JTHISREP)
        ENDDO
        NAME_B = IAND(IOBS2PKWORK(6),MSKBST)
        IF(NAME .NE. NAME_B) THEN
          WRITE(6,243)C8NAME,M,iptrwrd,C8NAME_B
  243     FORMAT(1H ,'b4plotx: ERROR ... database pointer has been ',
     1               'corrupted for stn=',A6,
     2          /1h ,7X,'ADATA(3,',I6,')=hex',Z16,
     3               '  which points to stn=',A6)
          IERROR = 10
          GO TO 999
        ENDIF
        
        CALL GBYTES(IOBS2PKWORK,LONELVL,NOFFSET,NBITSGRP,NPADBITS,
     1              NGRPS2DO)

        M2 = NGRPS2DO - 2		!... DO NOT EXT SIGN OF QUAL MRK
        DO  IHW = 1,M2		
          IF(BTEST(LONELVL(IHW),31)) THEN
             LONELVL(IHW) = IOR(NEGSIGNEXT,LONELVL(IHW))
          ENDIF
        ENDDO
        DO  IHW = 1,NGRPS2DO
          IF(IHW .EQ. 11 .OR. IHW .EQ. 12) THEN		!... STN NAME
            FLONELVL(IHW) = 0.0
          ELSE
            FLONELVL(IHW) = FLOAT(LONELVL(IHW)) / 4096.0
          ENDIF
        ENDDO
C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
        ALAT  = FLONELVL(1)
        ALONG = FLONELVL(2)
C
        IPRIOR = IAND(ijwrd,MSKPRI)
        LSWPLT = .FALSE.
        BRNTST = .FALSE.

        IF(BTEST(IJWRD,LBIT_TOSSED)) THEN
C         ...THIS STN WAS TOSSED IN THINNING...
          IF(ITOPLQ .EQ. 0) GO TO 777
          LSWPLT = .TRUE.
C         ...THIS MARKS A TOSSED STN FOR PLOTTING AT TOP OF PAGE...
        ENDIF

        KTTYP = ISHFT(ijwrd,-42)
        KTTYP = IAND(KTTYP,MSKTYP)
        LNEGQ = BTEST(KTTYP,5)
        IF(LNEGQ) KTTYP = IOR(KTTYP,NEGEXT)
C       ...WHICH SIGN-EXTENDED THOSE SFC OBS TYPES...

        IF(.NOT. LSWPLT) GO TO 260
C       ...OTHERWISE, THIS IS A TOSSED STN...
        IF(KTTYP .LT. 1) GO TO 777
        IF(KTTYP .GT. 2) GO TO 777
C       ... OTHERWISE, KTTYP=1 OR =2  ONLY ...
        IOFFTM = IAND(namwrd,MSKOFF)
        IF(IOFFTM .NE. 5) GO TO 777
C     ...PLOT AT TOP OF PAGE TOSSED STNS WHICH ARE ONTIME,LANDOR FXDSHIP
        ITOPI = ITOPI + 1
        IF(ITOPI .GT. ITOPMX) GO TO 777
  260   CONTINUE
        L = LIT0
        IOPTN = 0

        IOPTN = IOR(IOPTN,ISCHRQ)
C       ...WHICH SETS CHAR SIZE OF PRINT IN PLOTTING MODEL...
        IOPTN = IOR(IOPTN,IMVWQ)
        IOPTN = IOR(IOPTN,IANGLQ)
C
C       ...TEST FOR BROWNSVILLE
        IF(C8NAME(1:5) .EQ. BRNSVL(1:5)) BRNTST=.TRUE.
C       ...TEST FOR SPECIAL STN BY NAME FOR MOVING SOME PLOTTED INFO
        IF(LSWPLT) GO TO 290
C       ...WHICH BYPASSES TEST FOR SPECIAL STNS IF TOP OF PAGE PLOT...
       
      DO  270  I = 1,NSPL
        NAMOV = NAMSPL(I)
        NAMOV = IAND(NAMSPL(I),MSKBST)
        NOTMSK = NOT(MSKBST)
        MOVBIT = IAND(NAMSPL(I),NOTMSK)
        IF(NAME .EQ. NAMOV) THEN
          IOPTN = IOR(IOPTN,MOVBIT)
          GO TO 290		!... JUMP OUT OF LOOP ON MATCH FOUND
        ENDIF
  270 CONTINUE		!... ENDDO ON LOOKING FOR A MATCH ON SPECIAL STN
      GO TO 290

  290 CONTINUE
C     ... CONVERT I AND J TO VARIAN DOT UNITS...
C     ...THIN REFORMATTED IJWRD:     /JJJ//JFR/III/IFR/other/
C                                    / 9 // 9 / 9 / 9 / 5   /
C     ...            OLD VERSION HAD /III/IFR/JJJ/JFR/
C     ...
        jgjf = ishft(ijwrd,-23)
        jgjf = iand(jgjf,msk18)
        xj   = float(jgjf) / two9
        JP = NINT((XJ - 1.0) * VSCALE)
        JP = JP + JCORN
        JO = JP
        igif = ishft(ijwrd,-5)
        igif = iand(igif,msk18)
        xi   = float(igif) / two9
        IP = nint((XI - 1.0) * VSCALE)
        IP = IP + ICORN + IDELTA
        IO = IP
C ...      LCKPT = 290
C ...      PRINT *,' b4plotx:at LCKPT=',LCKPT,' IO=',IO,' JO=',JO
C       ...NOW IO AND JO ARE IN VARIAN DOT UNITS...
C       ... if I knew which itout is for the 2-dot, then I could patch
C       --------------------------------------------------------------

        IF(LSWPLT) THEN
C         ...TO RECOMPUTE IO AND JO FOR TOP-OF-PAGE PLOT
          JRELP = (ITOPI - 1) / KROWMX
          IRELP = MOD((ITOPI - 1), KROWMX)
          IO = KSTRI + IRELP * IKDOT
          JO = (KSTRJ + JKDOT) + (JRELP * JKDOT)
        ENDIF
        
        NBITS = IAND(lonelvl(kptr_bitwrd),MSK6BITS) 	!... OLD NBITS

        nbitwrd = ishft(lonelvl(kptr_bitwrd),-12)
        MYDBBITS = MOVA2I(C1NBITWRD(8))
        NUBITS = mova2i(c1nbitwrd(7))
        NUBITS = 0		!... I HAVE NOT SET NUBITS YET
C       ...WHERE NUBITS HAVE BEEN SET IF CALCULATED DATA ...
        IF(NBITS .EQ. 0) GO TO 777
C       ...TO UNPACK THE OBSERVED QUANTITIES ...

        DO  I = 1,9
          NUNPAK(I) = 0
        ENDDO

        IF(LONELVL(KPTR_DDD) .EQ. MISGB12) THEN
          NUNPAK(1) = MISGB0
        ELSE
          NUNPAK(1) = NINT(FLONELVL(KPTR_DDD))	!... WAS U; IS DDD
        ENDIF

        IF(LONELVL(KPTR_FFF) .EQ. MISGB12) THEN
          NUNPAK(2) = MISGB0
        ELSE
          NUNPAK(2) = NINT(FLONELVL(KPTR_FFF))	!... WAS V; IS FFF
        ENDIF

        IF(LONELVL(KPTR_ZZ) .EQ. MISGB12) THEN
          NUNPAK(3) = MISGB0
        ELSE
          NUNPAK(3) = NINT(FLONELVL(KPTR_ZZ))	!... WAS D-VAL; IS HGT
        ENDIF

        NUNPAK(5) = MISGB0		!... initialize dewpoint to MISG
        IF(LONELVL(KPTR_TT) .EQ. MISGB12) THEN
          NUNPAK(4) = MISGB0
        ELSE
          TT_T = FLONELVL(KPTR_TT)
          NUNPAK(4) = NINT(TT_T)	!... WAS 10*TT; IS TT
          
          IF(LONELVL(KPTR_TD) .NE. MISGB12) THEN
            DEW_T = FLONELVL(KPTR_TD)
            TDEWDEPR = TT_T - DEW_T
            IF(TDEWDEPR .LT. 0.0) THEN
               TDEWDEPR = 0.0			!... supersaturated
            ELSE IF(TDEWDEPR .GE. 30.0)THEN
               TDEWDEPR = 30.0
            ENDIF
            NUNPAK(5) = NINT(TDEWDEPR)		!... IS DEWPT DEPRESS
          ENDIF
        ENDIF

        IF(LONELVL(KPTR_DZ) .EQ. MISGB12) THEN
          NUNPAK(9) = MISGB0
        ELSE
          NUNPAK(9) = NINT(FLONELVL(KPTR_DZ))	!... IS 12-HR HGT CHG
        ENDIF
       
        

C       ...INITIALIZE PLTDAT ARGS TO MISSING...
        TT = 99.0
        NDD = 99
        IDDGD = 99

        ND1_AF=999
C       ...FOR AFOS WIND DIRECTION
        IHSSS_AF = '999 '
C       ...FOR AFOS WIND SPEED

        IHZZZ = KBAR(1:4)
        ihdewdep = KBAR(1:4)
        IOPTN = IOR(IOPTN,KCIQB)
        L = LIT0
        IF(LVLDES.EQ.LMXWND) GO TO 333

C       * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C       ...     TEMPERATURE SECTION ...

        ITEMP = IAND(NBITS,KTBIT)
        IF(ITEMP .EQ. 0) GO TO 333
C       ...OTHERWISE, A TEMPERATURE WAS REPORTED...
        IF((ITOUT .NE. 7) .AND. (ITOUT .NE. 12) .AND. (ITOUT .NE. 13))
     X    GO TO 322
C       ...OTHERWISE, THIS IS TROP PLOT ON MERC OR A P-GOES PROJECTION.
        IF(KTTYP .EQ. 12) GO TO 333
        IF(KTTYP .EQ. 13) GO TO 333
C       ...WHICH DISCARDS TEMPS OF SAT WIND ESTIMATES IF TROPIC
  322   CONTINUE
C       PRINT *,' ITOUT KTTYP ARE ',ITOUT,KTTYP
C       IF((ITOUT .EQ. 8) .AND. ((KTTYP.EQ.8) .OR. (KTTYP.EQ.9)))THEN
C         GO TO 333
C         ...WHICH DISCARDS TEMPS OF A/C IF 1/20M, 300MB MAP...
C       ENDIF
        IOPTN = IOR(IOPTN,KTTB)
C       ...        TT = (FLOAT(NUNPAK(4))) / 10.0


        TT = FLONELVL(KPTR_TT)

        NTT = NINT(TT)
        IF(ITOUT .NE. 3) GO TO 3335
C       ...OTHERWISE, THIS IS IQSY SO USE DIFFERENT LPLMI
C       ...   TO PRINT + SIGN  BUT NO SIGN FOR NEGATIVE...

        CALL BIN2CH(NTT,CBCD,3,'A+##')

        IOPTN = IOR(IOPTN, KMEDB)
        IHTT(1:) = ' '
        IHTT(1:3) = CBCD(1:3)
        GO TO 333

 3335   CONTINUE
      IF( (ITOUT .EQ. 5) .OR. (ITOUT .EQ. 10) ) IOPTN=IOR(IOPTN,KMEDB)
      IF(.NOT.(PLTDTA)) GO TO 334
C     ...OTHERWISE TMPS WILL BE PLOTTED IN UPR RT FOR FAX WINDS ALOFT

      CALL BIN2CH(NTT,CBCD,3,'A#-#')
      IHTT(1:) = ' '
      IHTT(1:3) = CBCD(1:3)

      ITEMP = IAND(NUBITS,KTBUT)
      IF(ITEMP .NE. 0) IOPTN = IOR(IOPTN,KCLTB)
      IOPTN=IOR(IOPTN,KMVTR)

      CALL BIN2CH(NTT,CBCD,3,'A+-#')

      NTT = JBCD
      GO TO 333

  334 CONTINUE

      CALL BIN2CH(NTT,CBCD,3,'A#-#')

      IHTT(1:) = ' '
      IHTT(1:3) = CBCD(1:3)

      CALL BIN2CH(NTT,CBCD,3,'A+-+')

      NTT = JBCD
      ITEMP = IAND(NUBITS,KTBUT)
      IF(ITEMP .NE. 0) IOPTN = IOR(IOPTN,KCLTB)
      IF((ITOUT .EQ. 7) .OR. (ITOUT .EQ. 12) .OR. (ITOUT .EQ. 13))THEN
        IOPTN = IOR(IOPTN,KMEDB)
      ENDIF

  333 CONTINUE
C       * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C       ...     WIND SECTION ...    (ERROR IN SOME BAD WINDS THRU)

      IHFFF(1:4) = ' '
      ITEMP = IAND(NBITS,KWBIT)
      IF(ITEMP .NE. 0) GO TO 336
C     ...OTHERWISE, WIND IS MISSING

      IF( (ITOUT .EQ. 9) .OR. (LVLDES .EQ. LMXWND) ) THEN
        IHFFF = C4NR
        IOPTN = IOR(IOPTN,KFFFB)
        NSSS = 1
C       ...FAKE FOR PLTDAT TO PLOT NR FOR MAX WIND NOT REPORTED...
        GO TO 351
      ENDIF

      IHFFF = C4MISF
      IOPTN = IOR(IOPTN,KFFFB)
      NSSS = 1
C     ...WHICH IS FAKE FOR PLTDAT TO ALLOW FFF PLOT OF M FOR MISSING
      ND1_AF=999
C     ...WHICH WILL SKIP MSG WIND FOR AFOS
      GO TO 351

  336 CONTINUE
      IF( (ITOUT .EQ. 9) .OR. (LVLDES .EQ. LMXWND) ) THEN
        ITEMP = IAND(NUBITS,KCLTB)
        IF(ITEMP .NE. 0) THEN
C         ... MAX WIND OF WINDS ALOFT CHART IS TERMINATING LEVEL...
          IHFFF = C4TL
          IOPTN = IOR(IOPTN,KFFFB)
          NSSS = 1
C         ...FAKE FOR PLTDAT TO PLOT TL FOR TERMINATING LEVEL...
        ENDIF
      ENDIF

C ...      U = (FLOAT(NUNPAK(1))) / 4.0
C ...      V = (FLOAT(NUNPAK(2))) / 4.0

C ...      CALL WNDR(U, V, NDD, NSSS)

C     ...WHICH RETURNS WITH NDD AN INTEGER IN TENS OF DEGREES...

      NSSS = NINT(FLONELVL(KPTR_FFF))
      call bin2ch(NSSS,CBCD,3,'A999')
      ihsss_af = cbcd(1:3) 

      NDD = NINT(FLONELVL(KPTR_DDD) / 10.0)
      FD = NDD * 10.0

  340 CONTINUE
      IF(NDD .GT. 36) then
        NDD = NDD - 36
        GO TO 340
      endif

      IF(NDD .LE. 0) then
        NDD = NDD + 36
        GO TO 340
      endif

C     ...NOW NDD IS IN RANGE BETWN 1 AND 36

      IF(NSSS .LT. 3) THEN
        ND1_AF=NDD
C       ...SAVE DIRECTION FOR AFOS PLOT OF LGHT VRBL WIND
        NDD = 99
C       ...FOR LIGHT VRBL WIND INCLUDING CALM
        IHFFF = C4LVRB
        IOPTN = IOR(IOPTN,KFFFB)
      ENDIF


C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     . . . .   CAUTION:  THIS PARAGRAPH IS WRONG   . . . . . . . . .

      CALL BIN2CH(NDD,CBCD,2,'A999')
      ihdewdep(1:) = ' '
      ihdewdep = CBCD(1:2)
C     PRINT *,' STORING DIR ',ihdewdep,' into dewdep '
      norwndir = intDD
C        !...     this seems wrong ?????  
C             ... Some option expecting wind dir printed in lower left?
C             ...   instead of dewpoint depression ????

      CALL BIN2CH(NSSS,CBCD,3,'A999')

C ...      ... since some strange results from the following,
C ...      ... let us try commenting it out.

C ...      IHFFF = CBCD(1:3)//'$'
		
C     ... I think the wrong AFOS hgt-chg = "000" came from here
C     ...     what is this for ?????????????????????????
C
C
      IF(NSSS.LT.3) GO TO 346
C       ...SKIP THIS IF WIND IS CALM OR LIGHT AND VARIABLE
C                    ? ? ? ? ? ? ? ? ? ? ? ?
  346 CONTINUE
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

      IF(LSWPLT) GO TO 3349
C     PRINT *,' IDEWQQ is ',IDEWQQ,' ihdewdep is ',ihdewdep
      IF(IDEWQQ) 349,348,349
C     ...IDEWQQ=0, SO PLOT THIS DIRECTION IN LOWER LEFT...
  348 CONTINUE
C     ... on bad 70mb it did not come this way, since IDEWB was not set
C      PRINT *,' KDEWB STORED IN IOPTN '
      IOPTN = IOR(IOPTN,KDEWB)
  349 CONTINUE
C     PRINT *,' ITOUT IS ',ITOUT       
      IF(ITOUT .NE. 7) GO TO 3490
C     ...OTHERWISE, ITOUT = 7 WHICH IS TROPIC PLOT ON MERC
      EQUTRI = 25.85
      DDDGR = FD - 90.0
      GO TO 3497

 3490 CONTINUE
      IF((ITOUT .EQ. 12) .OR. (ITOUT .EQ. 13)) THEN
C       ... THIS IS PSEUDO-GOES...
        XI = (XI - 1.0) * 0.50
        XJ = (XJ - 1.0) * 0.50
        XI = XI - 10.0
        XJ = XJ - 10.0
C
C       ... MUST CONVERT FROM I/J TO CENTER DISPLACEMENT IN INCHES 
C       ...    FOR GOESLL.
C
C ...      CALL GOESLL(WLONG0,DISSAT,SCALE,XI,XJ,ALAT,ALONG,IERR)

C ...      IF(IERR .NE. 0) THEN
C ...        PRINT 3491,IERR
C ... 3491 FORMAT(1H ,'IERR = ',I2,' RETURNED FROM GOESLL IN b4plotx.')
C ...      ENDIF

        DDDGR = FD
C
C       ...INITIALLY I WILL DO NOTHING SPECIAL FOR DRAWING WIND VECTORS
C       ...   ON MY PSEUDO-GOES BACKGROUND.
C
        GO TO 3497
      ENDIF

C ...      CALL TRULL(XI,XJ,ALAT,ALONG,KEIL)
      DDDGR = FD + (ALONG - VERTLN)
      IF(ITOUT .EQ. 5) THEN
        DDDGR = FD - (ALONG - VERTLN)
      ENDIF

 3497 CONTINUE
      IF(NSSS.GE.3) ND1_AF=NDD
C     ... OTHERWISE, WIND SPEED IS .LT. 3
      IF((NSSS.NE.0).OR.(ND1_AF.NE.36)) THEN

        call bin2ch(NSSS,CBCD,3,'A999')
        ihsss_af = cbcd(1:3) 
        GO TO 3399
      ENDIF
C     ...OTHERWISE WE HAVE CALM WIND
      ND1_AF=0
      IHSSS_AF = '000 '

C     ...RESET NSSS TO TRICK PLTDAT IN PLOTTING LV
      NSSS=1
 3399 CONTINUE
      IF(NSSS.LT.3) GO TO 351
      IF(DDDGR .GT. 360.0) DDDGR = DDDGR - 360.0
      IF(DDDGR .LE. 0.0) DDDGR = DDDGR + 360.0
      IDDGD = (DDDGR + 5.0) / 10.0
      IF(IDDGD .EQ. 0) IDDGD = 36
      IF( (ITOUT .EQ. 7) .AND. (XI .GT. EQUTRI) ) IDDGD = -IDDGD
      IF(ITOUT .EQ. 5) IDDGD = -IDDGD
      IF(((ITOUT .EQ. 12) .OR. (ITOUT .EQ. 13)) .AND. (ALAT .LT. 0.0))
     X IDDGD = -IDDGD
C     ...PLOT BROWNSVILLE WITH NO VECTOR FOR WINDS ALOFT(FAX)
      IF(ITOUT.NE.9) GO TO 3351
      IF(.NOT.LWNDONLY) GO TO 3351
      IF(.NOT.(BRNTST)) GO TO 3351
      IOPTN=IOR(IOPTN,KFFFB)
      IOPTN=IOR(IOPTN,KDEWB)
      GO TO 3353
 3351 CONTINUE
      IOPTN = IOR(IOPTN,KVECB)
 3353 CONTINUE
      IF(ITOUT .EQ. 8) IOPTN = IOR(IOPTN,KDOVB)
      IF(ITOUT .EQ. 9) IOPTN=IOR(IOPTN,KDOVB)
      GO TO 351
 3349 CONTINUE
C     ...COMES HERE FOR TOP PLOTTING WITH NO VECTOR...
      IOPTN = IOR(IOPTN,KFFFB)
      IOPTN = IOR(IOPTN,KDEWB)
      GO TO 351

  351 CONTINUE
C       * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C       ...     HEIGHT SECTION ...
      LCKPT = 3511
      IF(PLTDTA) GO TO 361

      ITEMP = IAND(NBITS,KZBIT)
      IF(ITEMP .EQ. 0) THEN
        LCKPT = 3513
C       ... the case of the corrupted NBITS ...
C       ... NBITS was somehow getting the right-shifted bits from
C       ... the preceding observation.  Now fixed.
        WRITE(6,3513) NBITS,lonelvl(kptr_bitwrd)
 3513   FORMAT(1H ,'b4plotx: at 3513, NBITS= HEX ',Z16,
     1        /1H ,'   lonelvl(kptr_bitwrd)= HEX ',Z16 )
        GO TO 360		!... all skipped 300mb came thru here
      ENDIF

      IF((ITOUT .EQ. 7) .OR. (ITOUT .EQ. 12) .OR. (ITOUT .EQ. 13)) THEN
C       ... THIS IS TROPIC OR PSEUDO-GOES PLOT ...
        IF(KTTYP .EQ. 11) THEN
          LCKPT = 3515
          GO TO 360		!... IS SIRS, SO SKIP Z
C         ...WHICH DISCARDS HGTS OF SIRS IF TROPIC
        ENDIF
      ENDIF

      IF(LONELVL(KPTR_ZZ) .EQ. MISGB12) THEN
        LCKPT = 3517
        GO TO 360
      ENDIF
C     ... OTHERWISE, REPORTED HGT IS NOT MISSING ...

      HHM = FLONELVL(KPTR_ZZ)		!... GOT THE ZZZ HGT IN METERS
      IZZ = NINT(HHM)


C     ...TEST FOR SAT WIND EST TYPE AND TREAT PRESS-ALT LIKE A/C HGT
C     ...TEST FOR A/C TYPE...

      IF((KTTYP .EQ. 8) .OR. (KTTYP .EQ. 9) .OR.	!... A/C
     1   (KTTYP .EQ. 12) .OR. (KTTYP .EQ. 13)) THEN	!... SAT WND
C       ...THIS IS AIRCRAFT OR SAT WND.  CONVERT HGT TO ALT IN FT

C ...           IF(LVLDES .LT. 4) THEN
C            ...OTHERWISE, BLO 500MB SO GIVEN HGT IN METERS...

        IHTFT = NINT(HHM * CNVFT)

C ...           ELSE
C ...             IHTFT = HHM * CNVDFT
C ...           ENDIF

C       ...TO RND TO NEAREST THSD FT AND GET RESULTS IN HUNDREDS OF FT
        IZZ = ((IHTFT + 500) / 1000) * 10
        IF(IZZ .GT. 0) THEN
          IF(IZZ .GT. 990) THEN
            LCKPT = 3541
            GO TO 360
          ENDIF
C           ... THE USUAL GOOD ACFT WITHIN RANGE ...
          LCKPT = 3543
          GO TO 355
        ELSE
C         ... IZZ .LE. 0; SO ...
          LCKPT = 3545
          GO TO 360
        ENDIF

      ENDIF		!... END OF AIRCRAFT / SAT WIND ALTITUDES

 3548 CONTINUE

      IF (LVLDES .GE. 4) THEN
         IZZ = NINT(FLOAT(IZZ) / 10.)
      ENDIF

      IZZ = MOD(IZZ,1000)		!... CLIP TO 3-DIGITS

  355 CONTINUE
      CALL BIN2CH(IZZ,CBCD,4,'A*-9')
      IHZZZ = CBCD(1:4)

      CALL BIN2CH(IZZ,CBCD,3,'A999')
      IHZZ = CBCD(1:3)			!... IHZZ IS FOR AFOS

      IOPTN = IOR(IOPTN,KZZZB)		!... DID NOT DO THIS ON 300MB

  360 CONTINUE
C ...      WRITE(6,3605)LCKPT,C8NAME(1:5),LONELVL(KPTR_ZZ)
C 3605 FORMAT(1H ,'b4plotx: at 360, from LCKPT=',I5,'; NAM=',A5,2X,Z8)

      IF((ITOUT.EQ.9).AND.(.NOT. LWNDONLY)) IOPTN=IOR(IOPTN,KMEDW)
      ITEMP = IAND(NUBITS,KZBUT)
      IF(ITEMP .NE. 0) IOPTN=IOR(IOPTN,KCLZB)	!... CALCULATED Z
      GO TO 361

  361 CONTINUE
C ...      WRITE(6,3609)LCKPT,C8NAME(1:5),LONELVL(KPTR_ZZ)
C . 3609 FORMAT(1H ,'b4plotx: at 361, from LCKPT=',I5,'; NAM=',A5,2X,Z8)

C       * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C       ...     DEWPOINT-TEMPERATURE SECTION  ...
C       ...             (MANY PLTDAT PLOTS "87" FOR MISSING DEWPOINT)
      lckpt = 3611
      IF(LVLDES.EQ.LMXWND) GO TO 366
      ITEMP = IAND(NBITS,KTDBIT)
      lckpt = 3613
      IF(ITEMP .EQ. 0) GO TO 366		!...TD out on 70MB here
C     ... BUT WHAT IS MISSING DEW-POINT CONDITION FLAG TO PLTDAT?
C     ... BUT WHAT IS MISSING DEW-POINT CONDITION FLAG TO genafplt?

      IDEWPT = NUNPAK(5)		!... GET DEWPOINT DEPRESSION

C     ...WHERE IDEWPT IS DEWPT DEPRESSION IN DEGREES
      lckpt = 3615
      if(idewpt .eq. MISGB0) GO TO 366

      IF(ITOUT .EQ. 3) GO TO 3610
      IF((ITOUT.EQ.8) .AND. ((KTTYP.EQ.8) .OR. (KTTYP.EQ.9))) GO TO 3615
      GO TO 3628

 3610 CONTINUE
C     ...OTHERWISE, THIS IS IQSY...
      IF(LSWPLT) GO TO 3627
C     ...DO OBS TIME PLOT.  IDEWPT CONTAINS OBS TIME IN HRS.
      namwrd = NDATA(2,M)
      IOFFTM = IAND(namwrd,MSKOFF)
      IF(IOFFTM .EQ. 5) GO TO 3624
      IF(IOFFTM .EQ. 6) GO TO 3624
C     ...OTHERWISE, THIS STN ID OFF TIME SO LEAVE OPEN CIRCLE,
C     ...   AND PRINT THE TIME...
      GO TO 363
 3624 CONTINUE
C     ...COMES HERE IF OBS IS ON TIME...
      IF(KTTYP .GT. 4) GO TO 363
C     ...OTHERWISE, LAND STN OR FIXED SHIP SO FILL CIRCLE AND SKIP PRNT
      IOPTN = IOR(IOPTN,KOCB)
      L = LIT8
      lckpt = 2626
      GO TO 366
 3627 CONTINUE
C     ...COMES HERE ONLY FOR STN PLOT AT TOP OF PAGE...
      IOPTN = IOR(IOPTN,KOCB)
      L = LIT8
      NAMSTN = NDATA(2,M)
      IOPTN = IOR(IOPTN,KNAMB)
      GO TO 422

 3615 CONTINUE
C     ...DO OBS TIME PLOT...
      GO TO 363

 3628 CONTINUE
C     ...IF YOU WANT TO TEST FOR 500MB LEVEL, LVLDES=4
      IF(IDEWPT .GT. 5) GO TO 363
      IF(KEIL .EQ. 14) GO TO 363
C     ...USE FILLED STN CIRCLE FOR MOIST AIR REGIONS
      IOPTN = IOR(IOPTN,KOCB)
      L=LIT8
  363 CONTINUE
      lckpt = 363
      IF(PLTDTA) GO TO 366
      CALL BIN2CH(IDEWPT,CBCD,2,'W999')
      IHTD = CBCD(1:2)//'$'

      IF(IDEWQQ .NE. 0) THEN
  364   CONTINUE
        ihdewdep = IHTD
C       ...WHICH PUTS DEW PT IN PLACE OF WIND DIR FOR PLOT IN LOW LEFT.
        IF( (IDEWPT .NE. 30) .OR. (ITOUT .EQ. 11) ) GO TO 365
C         ...OTHERWISE, DEWPT DEPRESS=30 WHICH IS DRY AIR
          ihdewdep = KHDRY
  365   CONTINUE
C       ...on bad 70MB it never came this way to set the dewpoint option
        IOPTN = IOR(IOPTN,KDEWB)
        NDCHAR = 2
        IF(IDEWPT .LT. 10) NDCHAR = 1
        CALL BIN2CH(IDEWPT,CBCD,NDCHAR,'I999')
        IHDEWPT(1:) = CBCD(1:NDCHAR)//'$'
      ENDIF
      lckpt = 365
      GO TO 366

C       ... END OF DEWPOINT-TEMPERATURE SECTION ...
C       * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  366 CONTINUE
C     WRITE(6,3661)LCKPT,C8NAME(1:5),LONELVL(KPTR_TD),ioptn,intDD
 3661 FORMAT(1H ,'b4plotx: at 366, from LCKPT=',I5,'; NAM=',A5,2X,Z8,
     1      /1h ,'        IOTPN= HEX ',Z16,'  intDD= HEX ',Z16)
      IF(ITOUT.EQ.6) IOPTN=IOR(IOPTN,KSMLB)
      NAMSTN = NDATA(2,M)
C     print 1822 ,namstn
 1822 format(' after 366 ',A8)
      IF(KEIL .NE. 14) GO TO 367
      
C ...      IF(IAND(NAMSTN,MASK) .EQ. 0) GO TO 367 ...
      IF(BTEST(NAMSTN,16)) THEN
C       ... I HAVE A BIT FLAG IN THE STATION NAME TO SIGNAL THAT
C       ... A CLOSED STATION CIRCLE IS DESIRED...
        C8NAMSTN(6:6) = ' '
        IOPTN = IOR(IOPTN,KOCB)
        L=LIT8
      ENDIF

  367 CONTINUE
C     ...IF PLOT OF NAMSTN IS DESIRED  THEN .OR KNAMB INTO IOPTN HERE
      IF(KTTYP .LT. 4) GO TO 374

      IF(KTTYP .EQ. 4) GO TO 373
C     ...WHICH TEST FOR FIXED SHIPS

      IF(KTTYP .EQ. 7) GO TO 372
C     ...WHICH TESTS FOR RECCO...

      IF(KTTYP.EQ.10) GO TO 375
C     ...WHICH TESTS FOR BOGUS

      IF((KTTYP .EQ. 8) .OR. (KTTYP .EQ. 9)) GO TO 371
      IF(KTTYP .LT. 11) GO TO 370
      IF(KTTYP .GT. 13) GO TO 370
C     ...OTHERWISE, SATELLITE OBS...
      L = LITS
      IOPTN = IOR(IOPTN,KSTRB)
C     ...WHICH SETS A STAR FOR STN CIRCLE...
      IF(KTTYP .NE. 11) GO TO 370
C     ...OTHERWISE, THIS IS A SIRS-TYPE SATELLITE...
      IF((ITOUT .EQ. 7) .OR. (ITOUT .EQ. 12) .OR. (ITOUT .EQ. 13))
     X GO TO 368
      IOPTN = IOR(IOPTN,KNAMB)
  368 CONTINUE
      NOTMSK = NOT(KFFFB)
      IOPTN = IAND(IOPTN,NOTMSK)
C     ...WHICH ERASES THE WIND MISG FROM SIRS, SINCE ALWAYS MISG.
      IF(ITOUT .GT. 2) IOPTN = IOR(IOPTN,KMVNB)
      IOPTN = IOR(IOPTN,KSMNB)
  370 CONTINUE
      GO TO 374
  371 IOPTN = IOR(IOPTN,KSQRB)
      L = LITA
      IF(KTTYP .NE. 9) GO TO 3719
C
C     MARK ACAR DATA WITH A STATION NAME OF "A".
C
C                         "N"         "A"
C ...      DATA    NACAR /ZD5000000,Z00C10000/
C      
      IF((C8NAMSTN(1:1) .EQ. 'N') .AND. (C8NAMSTN(6:6) .EQ. 'A')) THEN
C       ... ASSUME THIS TO BE AN ACAR REPORT ...
C
        NAMSTN = NAMCAR1
        IOPTN = IOR(IOPTN,KCLOS)
        IOPTN = IOR(IOPTN,KNAMB)
        GO TO 374

      ENDIF

 3719 CONTINUE
      IF((ITOUT .NE. 7) .OR. (LVLDES .NE. 9)) GO TO 374
C     ...FOR FGGE I AM PLOTTING COLBA AT 150 MB IN THE TROPICS.
C     ...COLBA LOOK LIKE A/C FOR THAT PURPOSE AND I WANT TO PLOT
C     ...THEIR IDENTIFYING NUMBERS...
      IOPTN = IOR(IOPTN,KNAMB)
      GO TO 374

  372 CONTINUE
      L = LITA
      IOPTN = IOR(IOPTN,KSQRB)
      IF((ITOUT .NE. 1) .AND. (ITOUT .NE. 2)) THEN
        NAMSTN = NAMRC1
C       print 1823,namstn
 1823   format(' at 372 ',A8)
      ELSE
C       ... WHAT IS THIS PARA DOING ? ? ? ? ? ? ? ? ?
C       ITEMP = IAND(NAMSTN,MSKLHS)
C       ITEMP = ISHFT(ITEMP,8)
C       ITEMP = IOR(ITEMP,INAMRC)
C       print *,' itout  ',itout
C       print 1824,namstn
C       NAMSTN = IOR(ITEMP,IAND(NAMRC1,MSKRHS))
C       print 1824,namstn
 1824   format(' after 372 else namstn is ',a8)
    
      ENDIF
      GO TO 373

  373 CONTINUE
      IOPTN = IOR(IOPTN,KNAMB)
      GO TO 374

  375 CONTINUE
      NAMSTN=NAMBOG1
      IOPTN=IOR(IOPTN,KNAMB)
      IOPTN=IOR(IOPTN,KCLOS)
      GO TO 374



  374 CONTINUE
C     ...SECTION FOLLOWING IS FOR 12HR HGT CHG INTO FFF SLOT.
      IF(KTTYP .EQ. 1) GO TO 401
      IF(KTTYP .NE. 4) GO TO 422
  401 CONTINUE
C     ...COMES HERE IF LAND STATION OR FIXED SHIP...
      ITEMP = IAND(IOPTN,KFFFB)
      IF(ITEMP .EQ. 0) GO TO 4015
      IF(IHFFF(1:4) .EQ. C4LVRB) GO TO 422
C     ...OTHERWISE, WIND IS NOT LV, SO GO PUT HGT CHG IN FFF SLOT
 4015 CONTINUE
C
C     ... IN CRAY VERSION, LARRY SAGER HAS INCLUDED THE 12-HR HGT CHG
C     ...   IN THE 6TH OBSERV QUANT, SO JUST FETCH FROM THERE

      IF(LONELVL(KPTR_DZ) .NE. MISGB12) THEN
C ...         HGTCHGM = NINT(FLONELVL(KPTR_DZ))	!... IS 12-HR HGT CHG
         HGTCHGM = FLONELVL(KPTR_DZ)	!... IS 12-HR HGT CHG

C ...         if(lvldes .GE. 4) then    
C        ...CONVERT HGT CHG FROM METERS TO DECAMETERS for all levels
         HGTCHGM = HGTCHGM / 10.0
C ...         endif

         ITECHG = NINT(HGTCHGM)
         IF(iabs(itechg) .GT. 99) THEN
            PRINT  419, LVLDES,C8NAMSTN(1:6),LONELVL(KPTR_DZ)
  419       FORMAT(1H , 'b4plotx:EXCESSIVE 12HR HGT CHG AT LVLDES=',I4,
     1                  '; FOR STATION=',A6,
     2                  '; BAD HGT-CHG VALUE= HEX',Z16)
            GO TO 422
         ENDIF
C        ... OTHERWISE, HGT CHG IS WITHIN ACCEPTABLE RANGE, SO

         CALL BIN2CH(ITECHG,CBCD,3,'L+-+')
         
         IHFFF(1:4) = CBCD(1:3)//'$'

         IOPTN = IOR(IOPTN,KFFFB)
         IOPTN = IOR(IOPTN,KTALB)
         GO TO 422
      ENDIF
      GO TO 422



  422 CONTINUE
C     ...SEARCH FOR 850 MB CALC TMPS FOR 9 STNS ABOVE 1200 METERS
      IF(LVLDES.NE.2) GO TO 430

      NAMI=IAND(NAMSTN,MSKCH1TO5)
      DO 500 I=1,9
        ISAV=I
        NAMJ=IAND(NAMTMP(I),MSKCH1TO5)
        IF(NAMI.EQ.NAMJ) GO TO 510
  500 CONTINUE
      GO TO 430
  510 CONTINUE
      LCKPT = 515
      PRINT 515,LCKPT,C8NAMSTN(1:5)
  515 FORMAT(1X,'b4plotx: at LCKPT=',I5,'FOUND ELEVATED STATION  ',A5)

      ITEMP = IAND(NAMTMP(ISAV),MRKR13)		!... 001000
      IF(ITEMP .EQ. 0) GO TO 430
      NUNTMP=IAND(NAMTMP(ISAV),MSK12)		!... 000FFF
      LNEGQ=BTEST(NUNTMP,11)
      IF(LNEGQ) NUNTMP=IOR(NUNTMP,NEGEX2)
      TT=(FLOAT(NUNTMP))/10.0
      NTT=NINT(TT)
      CALL BIN2CH(NTT,CBCD,3,'A#-#')
      ITEMP = IAND(IOPTN,KTTB)
      IF(ITEMP .EQ. 0) THEN
        IOPTN=IOR(IOPTN,KTTB)
        IOPTN=IOR(IOPTN,KCLTB)
        IHTT(1:4) = CBCD(1:4)
        GO TO 430
      ELSE
C       ...OTHERWISE OBSERVED TMP ALREADY IN TEMPERATURE SLOT, SO PLOT
C       ...CALCULATED TMP ABOVE IT
        IOPTN=IOR(IOPTN,KMTMP)
        JHT1=JBCD
        GO TO 430
      ENDIF

  430 CONTINUE
      IF(.NOT.AFOS) GO TO 440

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      ALAT  = FLONELVL(KPTR_LAT)
      ALONG = FLONELVL(KPTR_LON)
C     ... In Luke Lin's FAX application, he will not use the AFOS option
C     ...    so he commented the following call
C
C     ... otherwise, generate the AFOS PLTFILE entry for this one stn
C ...      CALL GENAFPLT(ITOUT,LVLDES,C8NAME,KTTYP,IOPTN,
C ...     1              ALAT,ALONG,L,ND1_AF,IHSSS_AF,
C ...     2              IHZZ,IHTT,IHDEWPT,IHFFF,IRET_AFP)

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
  440 CONTINUE
C
C   CHECK TO SEE IF 1 DOT/2 DOT FLAG IS TURN ON.
C   IF FLAG IS ON...SKIP CHECK FOR FT HUACHUCA
C   IF FLAG IS OFF...CHECK FOR FT HUACHUCA
C
      IF(LMARGIN) GOTO 442
C
C   CHECK FOR FT HUACHUCA FOR MOVING PARAMETERS DOWN
      IF (C8NAME(1:5) .EQ. STNDWN(1:5)) THEN
        IOPTN = IOR(IOPTN,KDOWN)
        LCKPT = 440
        PRINT 441,LCKPT,C8NAME(1:5),IO,JO,ihdewdep(1:3),norwndir,IDDGD,
     1            IHFFF(1:3),NSSS,IHTT(1:3),JHT1,
     1            IHZZZ(1:3), IOPTN,KEIL
  441 FORMAT(1H ,'B4PLOTX:at LCKPT=',I5,' WILL CALL PLTDAT AT STA ',A5,
     1           ' WITH PARAMETERS= ',2I5,
     2      /1H ,A3,2I5,2X,A3,I5,2X,A3,I5,1X,A3,2X,Z10,2X,I5)
      ENDIF

  442 CONTINUE
C
      IF(.NOT.LMARGIN) GOTO 445
C
C  ...WHEN 1-DOT/2-DOTS VARIAN/FAX CHARTS ARE GENERATED, THE
C     FOLLOWING CHECKS THE REPORTING STATION ACROSS NORTH
C     AMERICA (STATIONS ID BETWEEN 70000 AND 75000) AGAINST
C     A TABLE OF KNOWN OVERLAYING STATIONS(PLOTMLTY).
C
C  TEST TO DETERMINE IF ANY 2 NEARBY STATIONS SHOULD BE FURTHER
C EXAMINED.
C
C  CHECKING TO SEE IF NWS IS PRESENT
      DO 730 LK=1,NLIST_2
        IF(C8NAME(1:5) .EQ. NWSSTN(LK)(1:5)) THEN	!... FOUND MATCH
C         ...          NWS STATIONS:
          JAMSTN(LK) = C8NAMSTN
          JIDDGD(LCNTR,LK) = IDDGD
          Jihdewdep(LCNTR,LK) = ihdewdep
          Jnorwndir(LCNTR,LK) = norwndir
          JIHFFF(LCNTR,LK) = IHFFF
          JIHTT(LCNTR,LK) = IHTT
          JIHZZZ(LCNTR,LK) = IHZZZ
          JIO(LCNTR,LK) = IO
          JJOPTN(LCNTR,LK) = IOPTN
          JJHT1(LCNTR,LK) = JHT1
          JJO(LCNTR,LK) = JO
          JKEIL(LCNTR,LK) = KEIL
          JNSSS(LCNTR,LK) = NSSS
          JFLAG(LCNTR,LK) = 1
            GOTO 777		!... JUMP OUT OF THIS LOOP AFTER FOUND

        ENDIF

  730 CONTINUE    	!... ENDDO ON  LK=1,NLIST_2
C
C  CHECKING TO SEE IF MILITARY IS PRESENT
      DO 732 LK=1,NLIST_2
        IF(C8NAME(1:5) .EQ. MILSTN(LK)(1:5)) GOTO 733
        GOTO 732
  733 CONTINUE

C ...  734 FORMAT(A4)
C                   MILITARY STATIONS:
          IF(LK .GT. 100) GO TO 777
          KAME(LK) = NAME
          KAMSTN(LK) = C8NAMSTN
          KIDDGD(LCNTR,LK) = IDDGD
          Kihdewdep(LCNTR,LK) = ihdewdep
          Knorwndir(LCNTR,LK) = norwndir
          KIHFFF(LCNTR,LK) = IHFFF
          KIHTT(LCNTR,LK) = IHTT
          KIHZZZ(LCNTR,LK) = IHZZZ
          KIO(LCNTR,LK) = IO
          KKOPTN(LCNTR,LK) = IOPTN
          KIOPTN(LCNTR,LK) = MIOPTN
          KJHT1(LCNTR,LK) = JHT1
          KJO(LCNTR,LK) = JO
          KKEIL(LCNTR,LK) = KEIL
          KNDD(LCNTR,LK) = NDD
          KNSSS(LCNTR,LK) = NSSS
          KFLAG(LCNTR,LK) = 1
          GOTO 777
  732 CONTINUE
C
      IDOTS = 0
      JDOTS = 0
      IERR  = 0
C
      CALL MOVEID(IO,JO,NAME,IPROB,ICNT,NLIST_1,IDOTS,JDOTS,KCNT,
     1       JCOUNT,IERR)

      LCKPT = 732
      PRINT *, ' b4plotx::movid:at LCKPT=',LCKPT,
     1         ' IDOTS=',IDOTS,' JDOTS=',JDOTS,' IERR=',IERR
      IF(IERR.EQ.0) GOTO 445
C
C   OTHERWISE...MOVE OVERLAYING STATION TO TOP OF PAGE
C   NDD GIVES THE TRUE WIND DIRECTION...NORTH BEING TOP OF PAGE
C   WHERE IDDGE GIVES TRUE WIND DIRECTION ACCORDING TO GRIDDED MAP
C
C     print *,' PLTD 732    ihdewdep is ',ihdewdep
C     CALL PLTDAT(IDOTS,JDOTS,ihdewdep,norwndir,NDD,IHFFF,NSSS,IHTT,JHT1,
C    1           IHZZZ,c8namstn,IOPTN,KEIL)
      CALL PLTDAT(IDOTS,JDOTS,ihdewdep,NDD,IHFFF,NSSS,IHTT,JHT1,
     1           IHZZZ,c8namstn,IOPTN,KEIL,norwndir)
C
C*    CALL PUTLAB(IDOTS-18,JDOTS-45,0.12,NAME,0.0,5,iprior,0)
C*    CALL PUTLAB(IDOTS-30,JDOTS-60,0.12,ILAT(KCNT),0.0,3,iprior,0)
C*    CALL PUTLAB(IDOTS,JDOTS-60,0.12,ILONG(KCNT),0.0,4,iprior,0)
C
      IXX = IDOTS - 18
      JYY = JDOTS - 45
      CALL PUTLAB(IXX,JYY,0.12,C8NAME,0.0,5,iprior,0)
      IXX = IDOTS - 30
      JYY = JDOTS - 60
      CALL PUTLAB(IXX,JYY,0.12,ILATLON(KCNT)(1:3),0.0,3,iprior,0)
      IXX = IDOTS
      JYY = JDOTS - 60
      CALL PUTLAB(IXX,JYY,0.12,ILATLON(KCNT)(4:7),0.0,4,iprior,0)
C
C   IF FORT CAMPBELL (74671) OCCURRS, DO NOT PRINT STATION
C   CIRCLE/WIND BAR...JUST MOVE ENTIRE DATA TO TOP OF VARIAN
C   CHART.
C
      IF(C8NAME(1:5) .EQ. CMPBLL(1:5)) GOTO 777
C
C   IF WHITE SANDS (72269) OCCURRS, DO NOT PRINT STATION
C   CIRCLE/WIND BAR...JUST MOVE ENTIRE DATA TO TOP OF VARIAN
C   CHART.
C
      IF(C8NAME(1:5) .EQ. WSNDS(1:5)) GOTO 777
C
C   IF WHITE SANDS MISSILE (74734) OCCURRS, DO NOT PRINT STATION
C   CIRCLE/WIND BAR...JUST MOVE ENTIRE DATA TO TOP OF VARIAN
C   CHART.
C
      IF(C8NAME .EQ. WSNDM(1:5)) GOTO 777
C
C     PLACING A NUMBER AND A CIRCLE IN THE LOCATION WHERE
C     THE OVERWRITING STATION SHOULD HAVE BEEN SEEN.
C
      LCKPT = 762
      PRINT *,'b4plotx:at LCKPT=',LCKPT,' IO=',IO,' JO=',JO
C     print *,' PLTD 762    ihdewdep is ',ihdewdep
C     CALL PLTDAT(IO,JO,ihdewdep,norwndir,IDDGD,IHFFF,NSSS,IHTT,JHT1,
C    1           IHZZZ,C8NAMSTN,MIOPTN,KEIL)
      CALL PLTDAT(IO,JO,ihdewdep,IDDGD,IHFFF,NSSS,IHTT,JHT1,
     1           IHZZZ,C8NAMSTN,MIOPTN,KEIL,norwndir)
C
C   PLACING THE STATION ID UNDER THE CILCLE
      IXX = IO - 10
      JYY = JO - 15
C*    CALL PUTLAB(IO-10,JO-15,0.06,NAME,0.0,5,iprior,0)
      CALL PUTLAB(IXX,JYY,0.06,C8NAME(1:5),0.0,5,iprior,0)
C
      GOTO 777
  445 CONTINUE
C
C   CHECKING FOR PADUCAH...IF TRUE, MOVE PARAMETERS WESTWARD
      IF (C8NAME(4:8) .EQ. STNWST(1:5)) THEN
        IOPTN = IOR(IOPTN,IWEST)
      ENDIF
C
C   CHECKING FOR NASHVILL/BERRY..IF TRUE, MOVE PARAMETERS WESTWARD
      IF (C8NAME(4:8) .EQ. STNEST(1:5)) THEN
        IOPTN = IOR(IOPTN,IEAST)
      ENDIF
C
C   CHECKING FOR DAYTON...IF TRUE, MOVE PARAMETERS NORTHWARD
      IF (C8NAME(4:8) .EQ. STNNTH(1:5)) THEN
        ioptn = IOR(ioptn,jnorth)
      ENDIF
C
C   CHECKING FOR HUNTINGTON...IF TRUE, MOVE PARAMETERS SOUTHWARD
      IF (C8NAME(4:8) .EQ. STNSTH(1:5)) THEN
        ioptn = IOR(ioptn,jsouth)
      ENDIF
C
C ...      LCKPT = 768
C ...      PRINT *,'b4plotx:at LCKPT=',LCKPT,' IO=',IO,' JO=',JO
C      print *,' PLTD 777    ihdewdep is ',ihdewdep, IOPTN
C      print 104,norwndir                              
 104  FORMAT(' norwndir at 777 in b4plotx is ',a8)
C     CALL PLTDAT(IO,JO,ihdewdep,norwndir,IDDGD,IHFFF,NSSS,IHTT,JHT1,
C    1            IHZZZ, C8NAMSTN,IOPTN,KEIL)
      CALL PLTDAT(IO,JO,ihdewdep,IDDGD,IHFFF,NSSS,IHTT,JHT1,
     1            IHZZZ, C8NAMSTN,IOPTN,KEIL,norwndir)
      GO TO 777
  777 CONTINUE
  779 CONTINUE
C
C  TEST TO DETERMINE WHERE THE 2 NEARBY STATIONS SHOULD BE PLOTTED
C  USING K FOR INDEXING INSTEAD OF L WHICH IS PREVIOUSLY DEFINED LOGICAL
C  * 1. G.R.D. (12/26/89)*****
C
      IF(NLIST_2 .EQ. 0) THEN
         WRITE(6,945)
  945    FORMAT(1X,'b4plotx: K = 0  JFLAG = 0  KFLAG = 0')
      ENDIF

      IF(NLIST_2 .EQ. 0) GOTO 909

      DO 782 K=1,NLIST_2
      WRITE(6,929) LCKPT,K, JFLAG(LCNTR,K), KFLAG(LCNTR,K)
  929 FORMAT(1X,'b4plotx:at LCKPT=',i5,'K =',1X,I6,3X,
     1          'JFLAG  =',I6,3X,'KFLAG=',I6)
        LCKPT = 782
C       PRINT *,'B4PLOTX:at LCKPT=',LCKPT,' LCNTR=',LCNTR,' K=',K
C*      IF(JFLAG(LCNTR,K).EQ.0.AND.KFLAG(LCNTR,K).EQ.0) GOTO 782
        IF(JFLAG(LCNTR,K).EQ.0.AND.KFLAG(LCNTR,K).EQ.1) GOTO 783
        IF(JFLAG(LCNTR,K).EQ.1.AND.KFLAG(LCNTR,K).EQ.0) GOTO 784
        IF(JFLAG(LCNTR,K).EQ.1.AND.KFLAG(LCNTR,K).EQ.1) GOTO 785
C
  783 CONTINUE
      lckpt = 783
C     PRINT *,'b4plotx:at LCKPT=',LCKPT,' KIO()=',KIO(LCNTR,K),
C    1        ' KJO()=',KJO(LCNTR,K)
C     print *,' PLTD 783    jihdewdep is ',kihdewdep(lcntr,k)
C     CALL PLTDAT(KIO(LCNTR,K),KJO(LCNTR,K),Kihdewdep(LCNTR,K),
C    1     Knorwndir(LCNTR,K),KIDDGD(LCNTR,K),KIHFFF(LCNTR,K),
C    2     KNSSS(LCNTR,K),KIHTT(LCNTR,K),KJHT1(LCNTR,K),
C    3     KIHZZZ(LCNTR,K),KAMSTN(K),KKOPTN(LCNTR,K),
C    4     KKEIL(LCNTR,K))
      CALL PLTDAT(KIO(LCNTR,K),KJO(LCNTR,K),Kihdewdep(LCNTR,K),
     1     KIDDGD(LCNTR,K),KIHFFF(LCNTR,K),
     2     KNSSS(LCNTR,K),KIHTT(LCNTR,K),KJHT1(LCNTR,K),
     3     KIHZZZ(LCNTR,K),KAMSTN(K),KKOPTN(LCNTR,K),
     4     KKEIL(LCNTR,K),knorwndir(LCNTR,K))
C
      IXX = KIO(LCNTR,K) - 10
      JYY = KJO(LCNTR,K) - 20
C**   CALL PUTLAB(KIO(LCNTR,K)-10,KJO(LCNTR,K)-20,0.06,KAME(K),
C*** 1            0.0,5,iprior,0)

      CALL PUTLAB(IXX,JYY,0.06,KAME(K),0.0,5,iprior,0)
C
       GOTO 782
C
  784 CONTINUE
C
C     print *,' PLTD 784    jihdewdep is ',kihdewdep(lcntr,k)
C     CALL PLTDAT(JIO(LCNTR,K),JJO(LCNTR,K),Jihdewdep(LCNTR,K),
C    1     Jnorwndir(LCNTR,K),JIDDGD(LCNTR,K),JIHFFF(LCNTR,K),
C    2     JNSSS(LCNTR,K),JIHTT(LCNTR,K),JJHT1(LCNTR,K),
C    3     JIHZZZ(LCNTR,K),JAMSTN(K),JJOPTN(LCNTR,K),
C    4     JKEIL(LCNTR,K))
      CALL PLTDAT(JIO(LCNTR,K),JJO(LCNTR,K),Jihdewdep(LCNTR,K),
     1     JIDDGD(LCNTR,K),JIHFFF(LCNTR,K),
     2     JNSSS(LCNTR,K),JIHTT(LCNTR,K),JJHT1(LCNTR,K),
     3     JIHZZZ(LCNTR,K),JAMSTN(K),JJOPTN(LCNTR,K),
     4     JKEIL(LCNTR,K),jnorwndir(LCNTR,K))
C
       GOTO 782
C
  785 CONTINUE
C
      CALL MOVOBS(KIO(LCNTR,K),KJO(LCNTR,K),ICNT,KIDOTS,KJDOTS,JCOUNT)
C
C     print *,' PLTD moveob kihdewdep is ',kihdewdep(lcntr,k)
C     CALL PLTDAT(KIDOTS,KJDOTS,Kihdewdep(LCNTR,K),Knorwndir(LCNTR,K),
C    1     KNDD(LCNTR,K),KIHFFF(LCNTR,K),
C    2     KNSSS(LCNTR,K),KIHTT(LCNTR,K),KJHT1(LCNTR,K),
C    3     KIHZZZ(LCNTR,K),KAMSTN(K),KKOPTN(LCNTR,K),
C    4     KKEIL(LCNTR,K))
      CALL PLTDAT(KIDOTS,KJDOTS,Kihdewdep(LCNTR,K),
     1     KNDD(LCNTR,K),KIHFFF(LCNTR,K),
     2     KNSSS(LCNTR,K),KIHTT(LCNTR,K),KJHT1(LCNTR,K),
     3     KIHZZZ(LCNTR,K),KAMSTN(K),KKOPTN(LCNTR,K),
     4     KKEIL(LCNTR,K),knorwndir(LCNTR,K))
C
C*    CALL PUTLAB(KIDOTS-18,KJDOTS-45,0.12,KAME(K),0.0,5,iprior,0)
C*    CALL PUTLAB(KIDOTS-30,KJDOTS-60,0.12,KLAT(K),0.0,3,iprior,0)
C*    CALL PUTLAB(KIDOTS,KJDOTS-60,0.12,KLONG(K),0.0,4,iprior,0)
C
      IXX = KIDOTS - 18
      JYY = KJDOTS - 45
      CALL PUTLAB(IXX,JYY,0.12,KAME(K),0.0,5,iprior,0)
      IXX = KIDOTS - 30
      JYY = KJDOTS - 60
      CALL PUTLAB(IXX,JYY,0.12,KLATLON(K)(1:3),0.0,3,iprior,0)
      IXX = KIDOTS
      JYY = KJDOTS - 60
      CALL PUTLAB(IXX,JYY,0.12,KLATLON(K)(4:7),0.0,4,iprior,0)

C     ... PLACING A NUMBER AND A CIRCLE IN THE LOCATION WHERE
C     ... THE OVERWRITING STATION SHOULD HAVE BEEN SEEN.
C
C     print *,' PLTD pre786 kihdewdep is ',kihdewdep(lcntr,k)
C     CALL PLTDAT(KIO(LCNTR,K),KJO(LCNTR,K),Kihdewdep(LCNTR,K),
C    1     Knorwndir(LCNTR,K),KIDDGD(LCNTR,K),KIHFFF(LCNTR,K),
C    2     KNSSS(LCNTR,K),KIHTT(LCNTR,K),KJHT1(LCNTR,K),
C    3     KIHZZZ(LCNTR,K),KAMSTN(K),KIOPTN(LCNTR,K),
C    4     KKEIL(LCNTR,K))
      CALL PLTDAT(KIO(LCNTR,K),KJO(LCNTR,K),Kihdewdep(LCNTR,K),
     1     KIDDGD(LCNTR,K),KIHFFF(LCNTR,K),
     2     KNSSS(LCNTR,K),KIHTT(LCNTR,K),KJHT1(LCNTR,K),
     3     KIHZZZ(LCNTR,K),KAMSTN(K),KIOPTN(LCNTR,K),
     4     KKEIL(LCNTR,K),knorwndir(LCNTR,K))
C
C     ... PLACING THE STATION ID UNDER THE CIRCLE
      IXX = KIO(LCNTR,K) - 10
      JYY = KJO(LCNTR,K) - 15
C**   CALL PUTLAB(KIO(LCNTR,K)-10,KJO(LCNTR,K)-15,0.06,KAME(K),
C**  1            0.0,5,iprior,0)
      CALL PUTLAB(IXX,JYY,0.06,KAME(K),0.0,5,iprior,0)
C
  786 CONTINUE
C     print *,' PLTDAT 786  jihdewdep is ',jihdewdep(lcntr,k)
C     CALL PLTDAT(JIO(LCNTR,K),JJO(LCNTR,K),Jihdewdep(LCNTR,K),
C    1     Jnorwndir(LCNTR,K),JIDDGD(LCNTR,K),JIHFFF(LCNTR,K),
C    2     JNSSS(LCNTR,K),JIHTT(LCNTR,K),JJHT1(LCNTR,K),
C    3     JIHZZZ(LCNTR,K),JAMSTN(K),JJOPTN(LCNTR,K),
C    4     JKEIL(LCNTR,K))
      CALL PLTDAT(JIO(LCNTR,K),JJO(LCNTR,K),Jihdewdep(LCNTR,K),
     1     JIDDGD(LCNTR,K),JIHFFF(LCNTR,K),
     2     JNSSS(LCNTR,K),JIHTT(LCNTR,K),JJHT1(LCNTR,K),
     3     JIHZZZ(LCNTR,K),JAMSTN(K),JJOPTN(LCNTR,K),
     4     JKEIL(LCNTR,K),jnorwndir(lcntr,k))
  782 CONTINUE
  909 CONTINUE
C
      go to 999			!... is this the normal return??

C     ...ERROR RETURNS
  910 CONTINUE
C     ...COMES TO 910 IF GIVEN ITOUT WAS OUT OF RANGE
      PRINT  912, ITOUT
  912 FORMAT(1H , 'b4plotx:ERROR RETURN.  GIVEN AN OUT-OF-RANGE ',
     1            'ITOUT = HEX', Z8)
      IERROR = 1
      go to 999

  920 continue
      write(6,925)LCKPT,LUNOVRL
  925 format(1h ,'b4plotx: READ-PARITY ERROR AT LCKPT=',I5,
     1           '; ON OVRL-CONS UNIT:',I4)
      IERROR = 2
      GO TO 999

  930 continue
      write(6,935)LCKPT,LUNOVRL
  935 format(1h ,'b4plotx: HIT END-OF-FILE AT LCKPT=',I5,
     1           '; ON OVRL-CONS UNIT:',I4)
      IERROR = 3
      GO TO 999
 

  999 continue
      RETURN
      END
