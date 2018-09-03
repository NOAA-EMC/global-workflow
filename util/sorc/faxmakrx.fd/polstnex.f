       SUBROUTINE POLSTNEX
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    POLSTNEX    LIST OF STN NAMES AND IDOT/JDOT LOCATIONS
C   PRGMMR: KRISHNA KUMAR       ORG: W/NP12    DATE: 1999-08-01
C
C ABSTRACT: LIST OF STATION NAMES AND IDOT/JDOT LOCATIONS AND PLOT THEM
C           ON THE NH4005 BACKGROUND FOR PFAX1 CHARTS.
C   IN A FORMAT WHICH WOULD FACILITATE CALLING PUTLAB()
C
C PROGRAM HISTORY LOG:
C   88-04-18  BOB HOLLERN
C   96-10-31  SHIMOMURA   -- CONVERTED FROM LABEL-ARRAY FORMAT TO
C                            AN EXPANDED FORMAT 
C   96-11-05  LUKE LIN    -- ADD THE PLOTTING LOGIC.
C 1999-08-01  KRISHNA KUMAR  CONVERTED THIS CODE FROM CRAY TO IBM
C                            RS/6000. ASSIGNED PROPER VALUE TO INDEFF
C                            USING RANGE FUNCTION FOR IBM RS/6000 FOR
C                            COMPILE OPTIONS xlf -qintsize=8 -qrealsize=8
C                            ADDED A BLOCK DATA TO MAKE IT RUN ON
C                            IBM RS/6000 SP SYSTEM.
C
C USAGE:   /POLSTN/  COMMON BLOCK NAME IN PROGRAM PEPFAX
C
C   OUTPUT ARGUMENT LIST:
C       COMMON /POLSTN/MAXNNAM,IJANDNAM
C       INTEGER   MAXNNAM
C       DATA      MAXNNAM   /219/ 
C       INTEGER   IJANDNAM(8,220)
C            I      J  ROT PRI ARW FON  NC
C       IJANDNAM(1,J) = IDOT
C       IJANDNAM(2,J) = JDOT
C       IJANDNAM(3,J) = 2-BIT ROTATION CODE
C       IJANDNAM(4,J) = 3-BIT PRIORITY
C       IJANDNAM(5,J) = 1-BIT FLAG FOR EXPLICIT CHAR-SET MODE 
C       IJANDNAM(6,J) = CHARACTER-SET INDEX
C       IJANDNAM(7,J) = COUNT OF CHARACTERS IN THE FOLLOWING WORD
C       IJANDNAM(8,J) = TEXT WORD OF UP TO 8 CHARACTERS, LEFT-JUSTIFIED
C
C REMARKS: 
C       IN USERS SOURCE CODE INCLUDE:
C          EXTERNAL  POLSTNEX
C
C       WHICH WILL FORCE THE LINKAGE-EDITOR  TO LOOK FOR  polstnex.o *
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM
C
C$$$
C
C      ... THE OLD ASM VERSION HAD THE LABEL-ARRAY FORMATTED DATA ...
C ...         TITLE 'COMMON /POLSTN/LABSTN -- STN LOCATORS '
C ...  BUT THIS FORTRAN VERSION HAS THE ARGUMENTS FOR PUTLAB() INSTEAD


       COMMON /POLSTN/MAXNNAM,IJANDNAM
       INTEGER   MAXNNAM
       INTEGER   IJANDNAM(8,220)
       
       INTEGER      IGROUP01(8,20)
       EQUIVALENCE (IJANDNAM(1,1),IGROUP01(1,1))
       INTEGER      IGROUP02(8,20)
       EQUIVALENCE (IJANDNAM(1,21),IGROUP02(1,1))
       INTEGER      IGROUP03(8,20)
       EQUIVALENCE (IJANDNAM(1,41),IGROUP03(1,1))
       INTEGER      IGROUP04(8,20)
       EQUIVALENCE (IJANDNAM(1,61),IGROUP04(1,1))
       INTEGER      IGROUP05(8,20)
       EQUIVALENCE (IJANDNAM(1,81),IGROUP05(1,1))
       INTEGER      IGROUP06(8,20)
       EQUIVALENCE (IJANDNAM(1,101),IGROUP06(1,1))
       INTEGER       IGROUP07(8,20)
       EQUIVALENCE (IJANDNAM(1,121),IGROUP07(1,1))
       INTEGER       IGROUP08(8,20)
       EQUIVALENCE (IJANDNAM(1,141),IGROUP08(1,1))
       INTEGER       IGROUP09(8,20)
       EQUIVALENCE (IJANDNAM(1,161),IGROUP09(1,1))
       INTEGER       IGROUP10(8,20)
       EQUIVALENCE (IJANDNAM(1,181),IGROUP10(1,1))
       INTEGER       IGROUP11(8,20)
       EQUIVALENCE (IJANDNAM(1,201),IGROUP11(1,1))

*         I      J  ROT PRI ARW FON  NC
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      EXTERNAL  LOOKTLB
       COMMON  /LKTLBS/LMTSETNUM,LOOKT
       INTEGER   LOOKT(9,63)
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       INTEGER      IPUTL_ARG(8)
       CHARACTER*8  CPUTL_ARG(8)
       EQUIVALENCE (IPUTL_ARG(1),CPUTL_ARG(1))
C

       INTEGER      IDOTSTN
       EQUIVALENCE (IPUTL_ARG(1),IDOTSTN)
       INTEGER      JDOTSTN
       EQUIVALENCE (IPUTL_ARG(2),JDOTSTN)
       INTEGER      IROTSTN
       EQUIVALENCE (IPUTL_ARG(3),IROTSTN)
       INTEGER      IPRIORSTN
       EQUIVALENCE (IPUTL_ARG(4),IPRIORSTN)
       INTEGER      IEXPLICITMODE
       EQUIVALENCE (IPUTL_ARG(5),IEXPLICITMODE)
       INTEGER      IFONTINDEX
       EQUIVALENCE (IPUTL_ARG(6),IFONTINDEX)
       INTEGER      NCHSTN
       EQUIVALENCE (IPUTL_ARG(7),NCHSTN)
       CHARACTER*8  CNAMESTN
       EQUIVALENCE (CPUTL_ARG(8),CNAMESTN)


       INTEGER    IROTPRISTN(2)
       INTEGER      IDELTA
       INTEGER      JDELTA

C
       IDELTA = 36 * 9
       JDELTA = 36 * 7
C
       MAXNNAM = 178 
       DO  JSTN = 1,MAXNNAM
         DO  I = 1,8
           IPUTL_ARG(I) = IJANDNAM(I,JSTN)
         ENDDO
         HEIGHT = FLOAT(IFONTINDEX)
         ANGLE = 0.0
         IF(IEXPLICITMODE .NE. 0) THEN
           ITAG = 0
           IF(IFONTINDEX .GT. 0 .AND. IFONTINDEX .LE. LMTSETNUM) THEN
C            ... IFONTINDEX IS W/I RANGE ...
             IF(LOOKT(3,IFONTINDEX) .LT. 0) THEN
C              ... THIS IS A SIDEWAYS FONT ...
               ANGLE = 90.0
             ELSE
               ANGLE = 0.0
             ENDIF
           ENDIF
         ELSE
           ITAG = 1
         ENDIF

         IROTPRISTN(1) = IROTSTN
         IROTPRISTN(2) = IPRIORSTN

         NCHARS = NCHSTN
         IF(CNAMESTN(NCHARS:NCHARS) .EQ. '$') THEN
           NCHARS = NCHARS - 1
         ENDIF
         IF(NCHARS .GT. 0) THEN

         IPOS = IDOTSTN + IDELTA
         JPOS = JDOTSTN + JDELTA
C
           CALL PUTLAB(IPOS,JPOS,HEIGHT,CNAMESTN(1:NCHARS),
     1               ANGLE,NCHARS,IROTPRISTN,ITAG)

         ENDIF
       ENDDO
C
       RETURN
       END
ckumar
ckumar ##################block data polstnex_data
ckumar
       BLOCK DATA POLSTNEX_DATA
C
       COMMON /POLSTN/MAXNNAM,IJANDNAM
       INTEGER   MAXNNAM
       INTEGER   IJANDNAM(8,220)

       INTEGER      IGROUP01(8,20)
       EQUIVALENCE (IJANDNAM(1,1),IGROUP01(1,1))
       INTEGER      IGROUP02(8,20)
       EQUIVALENCE (IJANDNAM(1,21),IGROUP02(1,1))
       INTEGER      IGROUP03(8,20)
       EQUIVALENCE (IJANDNAM(1,41),IGROUP03(1,1))
       INTEGER      IGROUP04(8,20)
       EQUIVALENCE (IJANDNAM(1,61),IGROUP04(1,1))
       INTEGER      IGROUP05(8,20)
       EQUIVALENCE (IJANDNAM(1,81),IGROUP05(1,1))
       INTEGER      IGROUP06(8,20)
       EQUIVALENCE (IJANDNAM(1,101),IGROUP06(1,1))
       INTEGER       IGROUP07(8,20)
       EQUIVALENCE (IJANDNAM(1,121),IGROUP07(1,1))
       INTEGER       IGROUP08(8,20)
       EQUIVALENCE (IJANDNAM(1,141),IGROUP08(1,1))
       INTEGER       IGROUP09(8,20)
       EQUIVALENCE (IJANDNAM(1,161),IGROUP09(1,1))
       INTEGER       IGROUP10(8,20)
       EQUIVALENCE (IJANDNAM(1,181),IGROUP10(1,1))
       INTEGER       IGROUP11(8,20)
       EQUIVALENCE (IJANDNAM(1,201),IGROUP11(1,1))
C
       DATA      MAXNNAM   /178/
C
       DATA         IGROUP01 /
*            I      J  ROT PRI ARW FON  NC
     A      990,   658,  0,  0,  1,  4,  6, ")BGBW$",
     1      937,   698,  0,  0,  1,  4,  6, ")BGSF$",
     2      852,   766,  0,  0,  1,  4,  6, ")BGTL$",
     3     1043,   758,  0,  0,  1,  5,  6, ")BIKF$",
     4     1034,   779,  0,  0,  1,  5,  6, ")BIKR$",
     5      970,   557,  0,  0,  1,  4,  6, ")CYCA$",
     6      620,   586,  0,  0,  1,  4,  6, ")CYEG$",
     7      878,   639,  0,  0,  1,  4,  6, ")CYFB$",
     8      958,   449,  0,  0,  1,  4,  6, ")CYHZ$",
     9      893,   456,  0,  0,  1,  4,  6, ")CYQB$",
     A      941,   431,  0,  0,  1,  4,  6, ")CYQI$",
     1     1007,   516,  0,  0,  1,  4,  6, ")CYQX$",
     2      787,   750,  0,  0,  1,  4,  6, ")CYRB$",
     3      876,   439,  0,  0,  1,  4,  6, ")CYUL$",
     4      540,   591,  0,  0,  1,  4,  6, ")CYVR$",
     5      705,   504,  0,  0,  1,  4,  6, ")CYWG$",
     6      750,   591,  0,  0,  1,  4,  6, ")CYYQ$",
     7      951,   545,  0,  0,  1,  4,  6, ")CYYR$",
     8     1025,   509,  0,  0,  1,  4,  6, ")CYYT$",
     9      829,   416,  0,  0,  1,  4,  6, ")CYYZ$" /

       DATA         IGROUP02 /
     A      533,   665,  0,  0,  1,  4,  6, ")CYZP$",
     1     1221,   860,  0,  0,  1,  5,  6, ")EBBR$",
     2     1232,   888,  0,  0,  1,  5,  6, ")EDDF$",
     3     1193,   898,  0,  0,  1,  5,  6, ")EDDH$",
     4     1254,   911,  0,  0,  1,  5,  6, ")EDDM$",
     5     1112,   975,  0,  0,  1,  5,  6, ")EFHK$",
     6     1188,   819,  0,  0,  1,  5,  6, ")EGCC$",
     7     1210,   827,  0,  0,  1,  5,  6, ")EGLL$",
     8     1162,   810,  0,  0,  1,  5,  6, ")EGPK$",
     9     1206,   863,  0,  0,  1,  5,  6, ")EHAM$",
     A     1193,   775,  0,  0,  1,  5,  6, ")EICK$",
     1     1183,   775,  0,  0,  1,  5,  6, ")EINN$",
     2     1171,   914,  0,  0,  1,  5,  6, ")EKCH$",
     3     1049,   915,  0,  0,  1,  5,  6, ")ENBO$",
     4     1123,   904,  0,  0,  1,  5,  6, ")ENGM$",
     5     1136,   874,  0,  0,  1,  5,  6, ")ENZV$",
     6     1171,   917,  0,  0,  1,  5,  6, ")ESMM$",
     7     1126,   940,  0,  0,  1,  5,  6, ")ESSA$",
     8     1435,   608,  0,  0,  1,  5,  6, ")GCLP$",
     9     1592,   498,  0,  0,  1,  5,  6, ")GOOO$" /

       DATA         IGROUP03 /
     A     1525,   444,  0,  0,  1,  5,  6, ")GVAL$",
     1      914,   395,  0,  0,  1,  4,  6, ")KACK$",
     2      779,   296,  0,  0,  1,  4,  6, ")KATL$",
     3      904,   406,  0,  0,  1,  4,  6, ")KBOS$",
     4      616,   225,  0,  0,  1,  4,  6, ")KBRO$",
     5      622,   249,  0,  0,  1,  4,  6, ")KCRP$",
     6      649,   310,  0,  0,  1,  4,  6, ")KDAL$",
     7      604,   418,  0,  0,  1,  4,  6, ")KDEN$",
     8      797,   399,  0,  0,  1,  4,  6, ")KDTW$",
     9      599,   523,  0,  0,  1,  4,  6, ")KGTF$",
     A      850,   360,  0,  0,  1,  4,  6, ")KIAD$",
     1      654,   270,  0,  0,  1,  4,  6, ")KIAH$",
     2      882,   383,  0,  0,  1,  4,  6, ")KJFK$",
     3      497,   429,  0,  0,  1,  4,  6, ")KLAS$",
     4      454,   427,  0,  0,  1,  4,  6, ")KLAX$",
     5      599,   255,  0,  0,  1,  4,  6, ")KLOI$",
     6      823,   191,  0,  0,  1,  4,  6, ")KMIA$",
     7      691,   379,  0,  0,  1,  4,  6, ")KMKC$",
     8      719,   442,  0,  0,  1,  4,  6, ")KMSP$",
     9      711,   258,  0,  0,  1,  4,  6, ")KMSY$" /

       DATA         IGROUP04 /
     A      651,   343,  0,  0,  1,  4,  6, ")KOKC$",
     1      757,   400,  0,  0,  1,  4,  6, ")KORD$",
     2      516,   558,  0,  0,  1,  4,  6, ")KPDX$",
     3      870,   373,  0,  0,  1,  4,  6, ")KPHL$",
     4      506,   383,  0,  0,  1,  4,  6, ")KPHX$",
     5      824,   378,  0,  0,  1,  4,  6, ")KPIT$",
     6      455,   407,  0,  0,  1,  4,  6, ")KSAN$",
     7      619,   275,  0,  0,  1,  4,  6, ")KSAT$",
     8      532,   572,  0,  0,  1,  4,  6, ")KSEA$",
     9      454,   488,  0,  0,  1,  4,  6, ")KSFO$",
     A      554,   459,  0,  0,  1,  4,  6, ")KSLC$",
     1      729,   366,  0,  0,  1,  4,  6, ")KSTL$",
     2      796,   221,  0,  0,  1,  4,  6, ")KTPA$",
     3     1359,  1128,  0,  0,  1,  5,  6, ")LCNC$",
     4     1326,   829,  0,  0,  1,  5,  6, ")LECB$",
     5     1328,   776,  0,  0,  1,  5,  6, ")LEMD$",
     6     1358,   746,  0,  0,  1,  5,  6, ")LEZL$",
     7     1306,   858,  0,  0,  1,  5,  6, ")LFML$",
     8     1283,   811,  0,  0,  1,  5,  6, ")LFBD$",
     9     1274,   816,  0,  0,  1,  5,  6, ")LFBG$" /

       DATA         IGROUP05 /
     A     1243,   842,  0,  0,  1,  5,  6, ")LFPO$",
     1     1328,  1014,  0,  0,  1,  5,  6, ")LGWC$",
     2     1256,   968,  0,  0,  1,  5,  6, ")LHBP$",
     3     1282,   888,  0,  0,  1,  5,  6, ")LIMC$",
     4     1326,   918,  0,  0,  1,  5,  6, ")LIRF$",
     5     1336,   936,  0,  0,  1,  5,  6, ")LIRN$",
     6     1389,  1160,  0,  0,  1,  5,  6, ")LLLD$",
     7     1282,   575,  0,  0,  1,  5,  6, ")LPAZ$",
     8     1254,   573,  0,  0,  1,  5,  6, ")LPLA$",
     9     1333,   721,  0,  0,  1,  5,  6, ")LPPT$",
     A     1277,  1029,  0,  0,  1,  5,  6, ")LRBS$",
     1     1274,   867,  0,  0,  1,  5,  6, ")LSGG$",
     2     1309,  1063,  0,  0,  1,  5,  6, ")LTBA$",
     3     1284,   982,  0,  0,  1,  5,  6, ")LYBE$",
     4     1000,     6,  0,  0,  1,  4,  6, ")MACC$",
     5      667,    42,  0,  0,  1,  4,  6, ")MGGT$",
     6      716,    26,  0,  0,  1,  4,  6, ")MHTG$",
     7     1022,   111,  0,  0,  1,  4,  6, ")MJSJ$",
     8      872,    80,  0,  0,  1,  4,  6, ")MKJP$",
     9     1085,   109,  0,  0,  1,  4,  6, ")MKPA$" /

       DATA         IGROUP06 /
     A     1139,    60,  0,  0,  1,  4,  6, ")MKPB$",
     1     1124,    12,  0,  0,  1,  4,  6, ")MKPP$",
     2      566,   145,  0,  0,  1,  4,  6, ")MMMX$",
     3      989,   300,  0,  0,  1,  4,  6, ")MXKF$",
     4      858,   182,  0,  0,  1,  4,  6, ")MYNN$",
     5      477,   851,  0,  0,  1,  4,  6, ")PACD$",
     6      441,   941,  0,  0,  1,  4,  6, ")PADK$",
     7      594,   801,  0,  0,  1,  4,  6, ")PAFA$",
     8      520,   823,  0,  0,  1,  4,  6, ")PAKN$",
     9      555,   797,  0,  0,  1,  4,  6, ")PANC$",
     A      516,   798,  0,  0,  1,  4,  6, ")PANH$",
     1      462,  1000,  0,  0,  1,  4,  6, ")PASY$",
     2      202,  1525,  0,  0,  1,  4,  6, ")PGUM$",
     3       76,   736,  0,  0,  1,  4,  6, ")PHNL$",
     4       62,   692,  0,  0,  1,  4,  6, ")PHTO$",
     5       11,  1323,  0,  0,  1,  4,  6, ")PKMA$",
     6      159,   982,  0,  0,  1,  4,  6, ")PMDY$",
     7       94,  1212,  0,  0,  1,  4,  6, ")PWAK$",
     8      239,  1323,  0,  0,  1,  4,  6, ")PZMI$",
     9      563,  1561,  0,  0,  1,  4,  6, ")RCTP$" /

       DATA         IGROUP07 /
     A      351,  1436,  0,  0,  1,  4,  6, ")RJAW$",
     1      521,  1415,  0,  0,  1,  4,  6, ")RJFF$",
     2      485,  1375,  0,  0,  1,  4,  6, ")RJOO$",
     3      485,  1283,  0,  0,  1,  4,  6, ")RJSM$",
     4      456,  1340,  0,  0,  1,  4,  6, ")RJTT$",
     5      576,  1390,  0,  0,  1,  4,  6, ")RKSS$",
     6      501,  1516,  0,  0,  1,  4,  6, ")ROAH$",
     7      514,  1708,  0,  0,  1,  4,  6, ")RPMM$",
     8     1108,  1003,  0,  0,  1,  5,  6, ")ULLL$",
     9     1133,  1057,  0,  0,  1,  5,  6, ")UUWW$",
     A      641,  1626,  0,  0,  1,  4,  6, ")VHHH$",
     1      817,  1775,  0,  0,  1,  4,  6, ")VTBD$",
     2      989,   447,  0,  0,  1,  4,  3, ")SA",
     3      913,   551,  0,  0,  1,  4,  3, ")KL",
     4      756,   474,  0,  0,  1,  4,  3, ")QT",
     5      531,   625,  0,  0,  1,  4,  3, ")ZT",
     6      495,   897,  0,  0,  1,  4,  2, ")K",
     7     1217,   783,  0,  0,  1,  4,  3, ")RR",
     8     1232,   810,  0,  0,  1,  5,  5, ")JEY$",
     9     1242,   792,  0,  0,  1,  5,  5, ")FRQ$" /

       DATA         IGROUP08 /
     A     1287,   752,  0,  0,  1,  5,  3, ")LG",
     1     1031,   705,  0,  0,  1,  5,  2, ")A",
     2      990,   600,  0,  0,  1,  4,  2, ")B",
     3     1091,   626,  0,  0,  1,  4,  2, ")C",
     4     1126,   525,  0,  0,  1,  4,  2, ")D",
     5     1136,   400,  0,  0,  1,  4,  2, ")E",
     6     1096,   747,  0,  0,  1,  4,  2, ")I",
     7     1154,   707,  0,  0,  1,  5,  2, ")J",
     8     1242,   694,  0,  0,  1,  5,  2, ")K",
     9     1060,   865,  0,  0,  1,  5,  2, ")M",
     A      264,   572,  0,  0,  1,  5,  2, ")N",
     1      455,   723,  0,  0,  1,  4,  2, ")P",
     2      912,   355,  0,  0,  1,  4,  2, ")H",
     3      641,   863,  0,  0,  1,  4,  6, ")160W$",
     4      610,   771,  0,  0,  1,  4,  6, ")140W$",
     5      626,   657,  0,  0,  1,  4,  6, ")120W$",
     6      627,   551,  0,  0,  1,  4,  6, ")110W$",
     7      417,   896,  0,  0,  1,  4,  5, ")50N$",
     8      628,   896,  0,  0,  1,  4,  5, ")70N$",
     9     1022,   898,  0,  0,  1,  5,  5, ")70N$" /

       DATA         IGROUP09 /
     A     1278,  1160,  0,  0,  1,  5,  5, ")40E$",
     1     1339,  1085,  0,  0,  1,  5,  5, ")30E$",
     2     1295,  1128,  0,  0,  1,  5,  5, ")40N$",
     3     1376,   995,  0,  0,  1,  5,  5, ")20E$",
     4     1363,   587,  0,  0,  1,  5,  5, ")20W$",
     5      930,   308,  0,  0,  1,  4,  5, ")70W$",
     6     1157,   323,  0,  0,  1,  4,  5, ")50W$",
     7     1322,   305,  0,  0,  1,  4,  5, ")40W$",
     8      826,   487,  0,  0,  1,  4,  5, ")50N$",
     9      826,   595,  0,  0,  1,  4,  5, ")60N$",
     A      826,   698,  0,  0,  1,  4,  5, ")70N$",
     1      826,   798,  0,  0,  1,  4,  5, ")80N$",
     2      826,   994,  0,  0,  1,  4,  5, ")80N$",
     3      278,  1356,  0,  0,  1,  4,  6, ")150E$",
     4      276,  1096,  0,  0,  1,  4,  6, ")170E$",
     5      635,  1056,  0,  0,  1,  4,  6, ")150E$",
     6      646,  1000,  0,  0,  1,  4,  6, ")160E$",
     7      640,   964,  0,  0,  1,  4,  6, ")170E$",
     8      673,  1066,  0,  0,  1,  4,  3, ")A1",
     9      675,   624,  0,  0,  1,  4,  3, ")A2" /

       DATA         IGROUP10 /
     A      261,  1069,  0,  0,  1,  4,  3, ")A3",
     1      261,   620,  0,  0,  1,  4,  3, ")A4",
     2      538,  1052,  0,  0,  1,  4,  3, ")B1",
     3     1394,  1056,  0,  0,  1,  4,  3, ")B2",
     4      537,   180,  0,  0,  1,  5,  3, ")B3",
     5     1386,   179,  0,  0,  1,  4,  3, ")B4",
     6     1200,  1058,  0,  0,  1,  4,  3, ")C1",
     7     1205,  1175,  0,  0,  1,  5,  3, ")C2",
     8     1394,  1056,  0,  0,  1,  5,  3, ")C3",
     9     1395,  1177,  0,  0,  1,  5,  3, ")C4",
     A      542,   836,  0,  0,  1,  5,  3, ")D1",
     1     1393,   838,  0,  0,  1,  4,  3, ")D2",
     2      541,   284,  0,  0,  1,  5,  3, ")D3",
     3     1393,   288,  0,  0,  1,  4,  3, ")D4",
     4      673,  1397,  0,  0,  1,  4,  3, ")E1",
     5      676,   419,  0,  0,  1,  4,  3, ")E2",
     6      259,  1398,  0,  0,  1,  4,  3, ")E3",
     7      256,   418,  0,  0,  1,  4,  3, ")E4",
     8      622,  1000,  0,  0,  1,  4,  3, ")F1",
     9      622,   136,  0,  0,  1,  4,  3, ")F2" /

       DATA         IGROUP11 /
     A      118,  1002,  0,  0,  1,  4,  3, ")F3",
     1     1217,   957,  0,  0,  1,  4,  3, ")G1",
     2     1218,    93,  0,  0,  1,  5,  3, ")G2",
     3      731,   958,  0,  0,  1,  4,  3, ")G3",
     4      727,    93,  0,  0,  1,  4,  3, ")G4",
     5      568,  1061,  0,  0,  1,  4,  3, ")H1",
     6     1397,  1057,  0,  0,  1,  4,  3, ")H2",
     7      566,    45,  0,  0,  1,  5,  3, ")H3",
     8     1148,    10,  0,  0,  1,  4,  3, ")H4",
     9      241,  1160,  0,  0,  1,  4,  3, ")I1",
     A     1065,  1172,  0,  0,  1,  4,  3, ")I2",
     1      246,   519,  0,  0,  1,  5,  3, ")I3",
     2     1067,   521,  0,  0,  1,  4,  3, ")I4",
     3      566,  1031,  0,  0,  1,  4,  3, ")J1",
     4     1394,  1030,  0,  0,  1,  4,  3, ")J2",
     5     1389,   808,  0,  0,  1,  5,  3, ")K1",
     6     1534,   811,  0,  0,  1,  5,  3, ")K2",
     7     1415,   375,  0,  0,  1,  5,  3, ")K3",
     8     1532,   393,  0,  0,  1,  4,  3, ")K4",
     9      8*0 /

	END BLOCK DATA POLSTNEX_DATA
