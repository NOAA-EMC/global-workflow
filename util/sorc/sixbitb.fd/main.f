            program SIXBITB 
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK  ***
C
c MAIN PROGRAM: GRAPH_SIXBITB
C   PRGMMR: KUMAR            ORG: NP12        DATE: 2000-02-14
C
C ABSTRACT:  Packs raw bit image into 6 bit bedient format &
c            adds NOAA DUCK.  Image resolution is passed
c            in a header in the bit image (read from fortran
c            units 11 and 12)
C
C PROGRAM HISTORY LOG:
c   96-12-18  vandenberghe        Program was originally avpolar
C   86-08-12  HLYWIAK
C   88-02-03  RUSSELL E. JONES   CHANGE FORTXDAM-W3FK00 I/O PACKAGE
C                                TO VSAM-W3FK40, W3FT19 TO W3FT59
C   88-02-03  HENRICHSEN PETER   USE W3FQ06 REMOVE INLINE GRAPHIC
C                                SUBROUTINES.
C   91-03-15  LILLY              ADD CALL TO GULFTL WHICH PROVIDES
C                                THE GENERATING MODEL.
C   95-05-05  LILLY              CONVRT TO FORTRAN 77.  ADD LOGIC
C                                FOR PRODUCING AN ATLANTIC AND A
C                                PACIFIC FAX CUT FOR FL450, FL390,
C                                FL340, FL300, FL240, AND FL180.
C   95-09-06  LILLY              ADD SUBROUTINE DUCK IN ORDER TO
C                                DISPLAY NCEP INSTEAD OF NMC ON
C                                VARIOUS CHARTS.
C    96-12-18 vandenberghe       removed graphics portion and
c                                left six bit packing logic.  The
c                                ancestor of this program is avpolarv
c                                the only remaining graphics is
c                                a call to DUCK
c                                 this program is expected to
c                                 do all  conversion of imported
c                                 raster images to 6 bit graphics
c                                 however fax image size is currently
c                                 hardwired; a holdover from the
c                                 cntr ancestery of avpolar; the
c                                 hardwiring probably should be removed
c                                 so master image size(s) can be
c                                 specified in fort.5
c
c    97-01-17                    added capability to read multiple map
c                                images of varying resolution as 
c                                suggested above. However the resolution
c                                information is stored in the map image
c                                and no fort.5 is needed (yet)
C
C   97-02-18                     retrofitted capability to build
c                                IPAK field in product in response
c                                to OSO requirement.  This only
c                                works for FDFAX and WNDANFTF
c                                products so far.   All done in
c                                a call to ipak just before cntr call
c
c  1999-12-21 KRISHNA KUMAR      Converted this code for IBM RS/6000
c                                from CRAY. Program has been modified
c                                to write the Fax output in direct
c                                access mode.
C USAGE:
C USAGE:
c      INPUT:
c          fort.18        fax cut information
c          fort.15        map naming information (ignored)
c          mapback.pur    map image (just read for map header)
c                         CNTR package will reopen this as unit 
c                      	   11 (see below)
c          ./mapback.pur (unit 11)  (hidden within cntr libraries)
c                         map background which for this
c                         program contains ALL of the graphics
c                         the name of the background is
c                         mapback.pur on the programs current
c                         working directory and it is specified
c                         in a data stattement
c   (optional)
c          ./image002    All of these are processed as fort.9 and 
c          ./image003    then reopened as fort.11 by CNTR package 
c          ./image004    CNTR usage of fort.11 is supposed to be 
c          ./image005    transparent to caller but unit 11  must
c               ..       not be open when  CNTR is called. 
c               ..       THIS REQUIREMENT IS MET BY THIS CODE!!
c          ./imagennn
c      OUTPUT:
c          fort.81         6bit bedient file
c      SCRATCH:
c          fort.60, 61, 62, 63, 71, 72, 52,  and 55
c                         all are assigned on cray as -sunblocked
C
C   SUBPROGRAMS CALLED:
C     UNIQUE: ECHBAR  GETSXX  NPOLUV  PLTTLL
C             PLTWLL  POLSTN
C             RNCNTL  MERCIN  DOFRST  AVTTLS  SLTCRD
C             FAXSHD  WNDRNH  IPAK
C
C     LIBRARY:
C       SPECIAL - CONSOL  IAND    IOR     ISHFT
c       SYSTEM  - GETENV (in ipak only)
C       W3LIB   - W3AI24  W3AQ13  W3AQ09  W3AS00
C               - W3FQ06
C                 W3FB04  W3FK40  W3FT59  W3FA11  W3FA13
C                 W3FT08  W3FT09  W3FT10  W3FT11  W3FA12  W3FT12
c                   found on /nwprod/w3lib on all three cray
C       GRAPHIC -
C                 CNTR    ISP2EB  ZPLOT
C                 AFDMP   AFNC    AFST    CHARAC  CMSK    CNTR
C                 CNTRI   CONST   DAYOWK  DCD4    DSH     ENDMAP
C                 ENDAFO  HILO    IFS     INPUT   INTRP   IROT
C                 IRVR    ISORT   LKTLBS  MERGE   NCD8    OUTP
C                 PRTITL  PUTLAB RAISE    SCAN    SHADE   SNAP
C                 TRANSA  UPDATR  VECT    WINCON  WNDBRK  WNDIRT
c        found on /nwprod/gphlib/gphcntr
c                 /nwprod/gphlib/gphfont
c                 /nwprod/gphlib/gphlib
c                    on cray4 (exec works on all crays)
c
C
C   EXIT STATES:  96  Bad GRIB index file; 99 bad GRIB file
C
C
C   LANGUAGE: F90
C   MACHINE : IBM    )
C
c    12/18/96    New program requirements (ignore above)
c                fort.8   fax cut information
c                fort.5   map naming information (ignored)
c                fort.11 (hidden within cntr libraries)
c                         map background which for this
c                         program contains ALL of the graphics
c                         the name of the background is
c                         mapback.pur on the programs current
c                         working directory and it is specified
c                         in a data statement
c
c                         a future version will allow multiple
c                         backgrounds for multiple images
C$$$
C
      COMMON/ISPACE/BUFF(5400),XTRA(8400)
      COMMON/MFILE/LANL(256),IDANL(1539)
      COMMON/PACKRA/JTAU(2),IUHR,ITAG,MONAT,JAHR,IHOUR,IDAY,IMONTH,IYR
      COMMON/KPLOT/lABEL(2,1024),LABIX,NOBUF,IDRA(50)
      COMMON/PUTARG/PUTHGT,PUTANG,IPRPUT,ITAPUT
      COMMON/ICNTR/IP1(2),IP2(2),IP3(2)
      COMMON/ICNTR2/IP4(2),IP5(2),IP6(2)
C      COMMON/MRGARG/YDIS,DUMMY(3),XO,YO
      COMMON/MRGARG/YDIS,DUMMY(2),iret_zpl,XO,YO
      COMMON/XZPLAX/XZPLOT,YZPLOT,IORITY
      COMMON/POLST2/LABSTN(2,145)
ckumar      dimension LABSTN(2,145)
      COMMON/NSCHED/ISLOTA,IPANA,ISUB,IFLAB,INSET,IRLAB,ISCHED(8,60)
      COMMON/TITLES/KTITLE(28),LPOLAR,LOWLEV
      parameter(MAXIWORD=36,MAXJSLINE=5828)
      PARAMETER (IMAGSIZ_WRDS=MAXIWORD*MAXJSLINE)
      dimension IMAGE(IMAGSIZ_WRDS) 
       character*8 clabss(2,145)
      equivalence (labstn,clabss)
ckumar
      COMMON/IOUTFAX/IFAXOUT,NRECFAX  ! ADDED FOR DIRECT ACCESS FOR FAX OUTPUT
C
      INTEGER IFAXOUT,NRECFAX
ckumar
      REAL d12(145*37),d22(145*37),d32(145*37),d42(145*37)
c
      INTEGER d11(6)
      INTEGER d13(2),d14(2),d15(20)
      INTEGER d23(2),d24(2),d25(20)
      INTEGER d33(2),d34(2),d35(20)
      INTEGER d43(2),d44(2),d45(20)

C
      DIMENSION ICK(3)
      INTEGER ichk(3)
C ADDED THIS...............................
      DIMENSION ITAUI(3),IOUTIN(2), ILVLI(3), IFLDI(3)
      DIMENSION ITAU(3),LVL(3),ITYPE(3)
      DIMENSION MTAU(5),MLVL(8),KLVL(8),MTYPE(3)
C
      DIMENSION IL(15)
      DIMENSION INDEX(6)
      DIMENSION INAM1(3)
      character*8 cnam1(3),cnam2(3),cnam3(3)
      character*12 cn12(3)
      dIMENSION INAM2(3)
      DIMENSION INAM3(3)
      equivalence (cnam1,inam1),(cnam2,inam2),(cnam3,inam3)
      DIMENSION IBCDT(2)
      dIMENSION IBCD(80)
      DIMENSION IFSRP(80)
      DIMENSION IBCD1(3)
      DIMENSION IBCD2(4)
      DIMENSION IBCD3(5)
      DIMENSION IBCD4(4)
      DIMENSION IDESC(9)
      DIMENSION IDDC(5)
      DIMENSION IBCDZ(2)
      DIMENSION ITITL3(4)
      DIMENSION ITITL4(2)
      DIMENSION IMONT(12)
ckumar      DIMENSION IFID(14)
      integer ifid(6)
ckumar      character*1 cifid(56)
      character*1 cifid(24)
ckumar      character*56 cifid56
      character*24 cifid24
ckumar      equivalence (cifid56,cifid)
      equivalence (cifid24,cifid)
      equivalence (cifid,IFID)
      INTEGER   JFID(14)
      character*4 cjpp(2,14)
      character*1  cjfid(56)
       equivalence (cjpp,jfid)
      character*56 cjpp56
      equivalence (cjpp56,cjfid)
      equivalence (cjfid,jfid)
      DIMENSION  KWNCTL(3,19)
      DIMENSION  KTTCTL(3,19)
      DIMENSION FLD1(145,37)
      DIMENSION FLD2(145,37)
      DIMENSION IVLD(2)
      INTEGER   MESS1(11),MESS2(11),MESS3(10)
C   THIS INCLUDES THE ATLANTIC HALF AND PACF HALF
      DIMENSION IAREA(8,9)
C
C
C    THIS IS A TEST TO SEE IF I CAN REPLACE CALL ENDMAP
C    WHICH THE FOLLOWING SUBSTITUTION????
      INTEGER SFL
      DATA  SFL    /X'FFFFFC00'/
C
C
      CHARACTER*44  IMESS2
      CHARACTER*40  IMESS3
      CHARACTER*56  KJFID
      CHARACTER*1 LPARM(100), CPARM(100)
      CHARACTER*1 LI
      DATA  LI    /'I'/
      CHARACTER*4  NSTOP
C
      INTEGER   ITITL1
      INTEGER   I00Z
      INTEGER   I12Z
      INTEGER   IDENT(12)
      INTEGER   IFULL
      INTEGER   J00Z
      INTEGER   J12Z
      INTEGER   LU
C
      INTEGER   IP1,IP2,IP3
      INTEGER   IP4,IP5,IP6
      INTEGER   IHAVL(2)
C
      LOGICAL   LPOLAR
      LOGICAL   LOWLEV
      DATA      IDENT/12*0/
C
C.....FOLLOWING ARE THE CUT DEFINITIONS FOR ISCHED...
C 1=XX14,2=VFULL,3=XX15,4=PN28,5=NT22,6=NT23,7=PN29,8=ATLN,9=PACF
C STRT(SCANS),HEIGHT(S),FLAGS,INDENT(BYTES),WIDTH(B),FAXIND(B),
C STRT OF INSET(S),INSET NUMBER
C
C AND THIS....................................
C
      DATA MTAU /18,24,30,36,48/
      DATA MLVL /850,700,500,400,300,250,200,150/
      DATA KLVL /4H 850,4H 700,4H 500,4H 400,4H 300,
     1           4H 250,4H 200,4H 150/
      DATA MTYPE /48,49,16/
C
      DATA NUMTAU /5/
      DATA NUMFLD /3/
      DATA NUMLVL /8/
C
      DATA MESS1/4HNWS ,4H;@20,4H**  ,4HAVPO,4HLAR ,4HCOMP,4HILED,
     1           4H  2/,4H18/8,4H8. *,4H* ;:/
      DATA MESS2/4HNWS ,4H;@20,4HPOLA,4HR AV,4HIATI,4HON M,4HAPXX,
     1           4HXX W,4HAS M,4HISSE,4HD ;:/
      DATA MESS3/4HNWS ,4H;@20,4H** X,4HX OF,4H XX ,4HMAPS,4H COM,
     1           4HPLET,4HED  ,4H**;:/
      DATA IAREA/216,2000,129,41,216,0,0,0,
     2           1,3600,128,0,216,0,0,0,
     3           655,2096,129,130,216,0,0,0,
     4           790,1950,129,81,108,108,1,99,
     5           790,1950,064,67,108,0,0,0,
     6            55,1710,129,40,216,0,0,0,
     7           655,1435,129,130,216,0,0,0,
     8           100,2800,129,0,216,0,0,0,
     9           100,2800,129,-128,216,0,0,0/
      DATA ICK/0,13800,1/
      DATA MASK8/X'000000FF'/
C     DATA NU/1/
      DATA NU/61/
      DATA IDESC/4HUNSI,4HGNED,4H TEM,4HPERA,4HTURE,
     X          4HS AR,4HE NE,4HGATI,4HVE. /
      DATA     KWNCTL / 00, 1, 2,  05, 1, 2,
     1                  10, 1, 2,  15, 1, 2,
     2                  20, 1, 2,  25, 1, 2,
     3                  30, 1, 2,  35, 1, 2,
     4                  40, 1, 2,  45, 1, 2,
     5                  50, 1, 2,  55, 1, 4,
     6                  60, 1, 4,  65, 1, 4,
     7                  70, 1, 4,  75, 5, 8,
     8                  80, 1,16,  85,29,36,
     9                  90,73,73  /
      DATA LBLTAP/55/
      DATA     KTTCTL / 00, 1, 4,  05, 3, 4,
     1                  10, 1, 4,  15, 3, 4,
     2                  20, 1, 4,  25, 3, 4,
     3                  30, 1, 4,  35, 3, 4,
     4                  40, 1, 4,  45, 3, 4,
     5                  50, 1, 4,  55, 3, 4,
     6                  60, 1, 4,  65, 5, 8,
     7                  70, 1, 8,  75, 5, 8,
     8                  80, 1,16,  85,29,36,
     9                  90,73,73  /
C     ... WHERE KTTCTL HAS LAT/ M1 /M3 OF LOOP CONTROLS FOR TEMPERATURE
      DATA INDEX/51,23,0,-47,0,23/
C
C     CONSTANTS FOR BACKGROUND NH2500
C
c     12/29/96  GWV
c     variables to handle multiple map backgrounds
c     (one per image up to 99 images) 
c     file ./cnames(k).pur will be opened in the map loop
c     the cfiles variable is a convenient way to handle
c     the ./ prefix and the .pur suffix.
      character*8 cl(15)
      equivalence (cl,il)
      character*8 cnames(100)
      character*14 cfiles(100)
c     variable to store resolution from map background
c     (rather than hardwiring it)
      character*80 chead
c     end 12/29/96 insert
      DATA IL(1),IL(2)/8HMAPBACK ,0  /
c      DATA IL(3),IL(4),IL(5),IL(6)/-1080,120,2760,3000/
c      DATA IL(7),IL(8),IL(9),IL(10)/-1080,120,2760,3000/
      DATA IL(3),IL(4),IL(5),IL(6)/0,0,1792,2440/
      DATA IL(7),IL(8),IL(9),IL(10)/0,0,1792,2440/         
       
C
C     CONTOUR ADJUST
C
      DATA IL(11),IL(12)/0,0/
C
C     LABELS ADJUST
C
      DATA IL(13),IL(14)/0,0/
      DATA JFID/4H   V,4HSSSS,4H CCZ,4H MM/,4HDD T,4HTHR ,4HLLLM,
     X          4HB PO,4HLAR ,4HAVIA,4HTION,4H W/T,4H    ,4H    /
      DATA INAM1/4HPROP,4HERTY,4H OF /
      DATA INAM2/4HK. H,4HLYWI,4HAK  /
      DATA INAM3/4HBIN ,4HA4 W,4HWB  /
      DATA cn12/'PROPERTY OF ','K. HLYWIAK  ','BIN A4 WWB  '/
      DATA ITITL1/4H    /
      DATA ITITL3/4H  HR,4H PRO,4HG VA,4HLID /
      DATA I00Z/4H00Z /
      DATA I06Z/4H06Z /
      DATA I12Z/4H12Z /
      DATA I18Z/4H18Z /
      DATA IMONT/4HJAN ,4HFEB ,4HMAR ,4HAPR ,
     X           4HMAY ,4HJUN ,4HJUL ,4HAUG ,
     X           4HSEP ,4HOCT ,4HNOV ,4HDEC /
      DATA J00Z/4H 00Z/
      DATA J12Z/4H 12Z/
      DATA ITITL4/4HBASE,4HD ON/
      DATA IDDC/4HNWS-,4HNCEP,4HWASH,4HINGT,4HON. /
      DATA IDID/600/
      DATA IGIANT/15/
      DATA ISMALL/10/
      DATA IVLD/4H VAL,4HID  /
      DATA INCR/25/
C     DATA IONE/1/,LU/8/
      DATA IONE/1/
      DATA LU/18/
      DATA IBLANK/4H    /
C  IA IS DEFINITION , JA IS THE NUMBER OF CUTS
      DATA IA/8/,JA/9/
      DATA KVAR/2/,IMAPER/0/
C
C.......ADDED ON 5 JUNE 1986...........
      DATA IDFAX/4H   B/
      DATA IFFAX/4H   F/
      DATA IVFAX/4H   V/
      DATA IMASK3/X'FF000000'/
      DATA MAPT7/X'E5000000'/
C
      EQUIVALENCE (IFULL,IHAVL(1))
      EQUIVALENCE(T1,IL(15))
      EQUIVALENCE(FLD1(1,1),BUFF(1))
      EQUIVALENCE(FLD2(1,1),XTRA(1))
      EQUIVALENCE(IMESS2,MESS2(1))
      EQUIVALENCE(IMESS3,MESS3(1))
      EQUIVALENCE(KJFID,JFID(1))
      integer kvv(2)
C
C   SET FLAG FOR POLAR MAP
      character*3 cst6,cst68
      CALL W3TAGB('GRAPH_SIXBITB',2000,0045,0093,'NP12')                   
      IEXIT=0 
ckumar
      NRECFAX = 0
      IFAXOUT = 81
ckumar
c     OPEN THE OUT PUT FAX FILE.
  
      OPEN (IFAXOUT, ACCESS='DIRECT', RECL=1440 )
c
ckumar

c CODE ADDED TO CONVERT EBCDIC STATION ID SPECIFIED 
C IN HEX DATA STATEMENTS, TO ASCII
      do 7 k=1,14
      id=(K-1)*4 +1
      cjpp56(id:id+3)=cjpp(1,k)
c      print 1777,cjpp(1,k),cjpp(2,k),cjpp56
1777  format(' DISPLCODE ',2a8,' nx ',a56)
7     continue
c      convert labstn
c      do 10,k=2,145
c      cst68=clabss(2,k)(6:8)
c we don't need conversion on IBM machines.
c      juh=ibm2cray(6,3,cst68,0,junc,1,cst6)
c 197  format(' LABSTN ',a3)
c      clabss(2,k)(6:8)=cst6
c 10   continue
C
c     load filenames for map backgrounds
       cfiles(1)='mapback.pur'
       cnames(1)='MAPBACK '
       do 2,k=2,100
       write(cnames(k),1019)k
       write(cfiles(k),1039)k
 1019    format('image',i3.3)
 1039  format('image',i3.3,'.pur')
c 1039  format('./image',i3.3,'.pur')
 2      continue
      LPOLAR=.TRUE.
C
C     INITIALIZE TAPE
C
      REWIND NU
C     CALL ERRMSG(9)
      T1=60.0
C???   CALL CONSOL(MESS1)
Corig PRINT(MESS1)
      WRITE(6,3482)
 3482 FORMAT(1X,'BEFORE RNCNTL')
      CALL RNCNTL(NMAPS,IDATC,IOPN,ISWTCH,KRUN1,ITOU1,ICYC1,
     1            INOPN1,INOPN2,INOPNA,INOPNB)
      WRITE(6,3483)
 3483 FORMAT(1X,'AFTER  RNCNTL')
      IF(ISWTCH.EQ.0)GOTO 105
C
C     FALL THRU-SEARCH THROUGH PEPFAXA SCHEDULE FOR DESIRED
C     RUN CARD ("1" CARD).
C
 2000 READ(LU,2005,ERR=2055,END=2060)ICARD3,IIRUN1,IIRUN2,IIJOB1,
     1IIJOB2,IICYC,NMAPS,KCODE1,KCODE2
 2005 FORMAT(I1,2(A4,A1),A4,I5,3X,A4,A1)
      IF(ICARD3.EQ.IONE)GOTO 2020
Corig PRINT 2010,LU
C     WRITE(6,2010)LU
C2010 FORMAT('0BAD RUN CARD FORMAT ON UNIT',I2)
      CALL W3TAGE('GRAPH_SIXBITB') 
      STOP 2012
C
C     OTHERWISE, RUN CARD FORMAT FOUND AND OK.
C
 2020 IF(INOPNA.EQ.IIJOB1.AND.INOPNB.EQ.IIJOB2)GOTO 2090
C
C     FALL THRU-READ THROUGH UNWANTED PART OF SCHEDULE.
C
c***
      DO 2050 I=1,NMAPS
      READ(LU,2030)NSLOTS
 2030 FORMAT(68X,I4)
      DO 2050 J=1,NSLOTS
      READ(LU,2040)I1
 2040 FORMAT(A1)
 2050 CONTINUE
c***
      GOTO 2000
C2055 PRINT 2057,LU
 2055 CONTINUE
C     WRITE(6,2057) LU
 2057 FORMAT('0ERROR READING FILE',I2,'...PROGRAM STOPS')
      CALL W3TAGE('GRAPH_SIXBITB') 
      STOP 2058
C2060 PRINT 2062,LU
 2060 CONTINUE
C     WRITE(6,2062)LU     
 2062 FORMAT('0END OF FILE ON UNIT',I2,' WITHOUT FINDING OPERATIONAL JOB
     1TYPE')
      CALL W3TAGE('GRAPH_SIXBITB') 
      STOP 2065
C2090 PRINT 2092,LU,IIJOB1,IIJOB2
 2090 CONTINUE
      WRITE(6,2092) LU,IIJOB1,IIJOB2
 2092 FORMAT('0OPERATIONAL JOB FORMAT FOUND ON UNIT',I2,'---',A4,A1)
C
C * * * * * * * * * * BASIC MAP LOOP* * * * * * * * * * * * * * * *
C
  105 CONTINUE
c
c***
      DO 1306 IMP=1,NMAPS
      cl(1)=cnames(IMP)
ckumar
ckumar Reading the map background - Unit number is changed to
ckumar 12 from 9
ckumar       open(12,form='unformatted',file=cfiles(imp))
       open(12,form='unformatted',access='direct',recl=80,
     1 file=cfiles(imp))
c***
       read(12,rec=1) chead
       print*,'chead',chead
      read(chead,101)nscan,ipixel
 101  format(36x,i4,6x,i2)
      ipixel=ipixel*64
      il(6)=nscan
      il(5)=ipixel+64
      print *,' MAP RESOLUTION WILL BE ',il(5),il(6)
      il(9)=il(5)
      il(10)=il(6)
c     new VFULL cut modification 2/24/97
      iarea(2,2)=nscan
c     end new VFULL cut mod
      print *,' MAP RESOLUTION WILL BE ',il(5),il(6),' cut', iarea(2,1)
ckumar
      close  (12)
ckumar
C
C     ZERO ISCHED ARRAY
C
      DO 1300 I=1,8
      DO 1300 J=1,60
      ISCHED(I,J)=0
 1300 CONTINUE
c***
Corig PRINT 1302
      WRITE(6,1302)
 1302 FORMAT('0ISCHED ARRAY HAS BEEN INITIALIZED FOR NEXT MAP')
C     INITIALIZE PUTLAB ARGUMENTS
C
      PUTHGT=1.0
      PUTANG=0.0
      IPRPUT=0
      ITAPUT=0
C
C     INITIALIZE FOR ZPLOT
C
      YDIS=0.0
      XZPLOT=1.0
      YZPLOT=1.0
      IORITY=2
C
C     INITIALIZE LABEL ARRAY
C
      REWIND LBLTAP
C
C     SETUP FOR REGULAR CHARACTERS
C
      CALL ZPLOT(0.0,0.0,-3)
C
C
C     INSERT ICOA STATIONS IN LABEL ARRAY
C
      M2=LABSTN(1,1)+1
ckumar
      print*,'M2 in Main   =  ',M2
c
c***
      DO 107 IZ=2,M2
      LABIX=LABIX+1
      LABEL(1,LABIX)=LABSTN(1,IZ)
      LABEL(2,LABIX)=LABSTN(2,IZ)
 107  CONTINUE
c
c***
C
C     INSERT NAME ON MAP
C
      IF(ISWTCH.EQ.1)GOTO 108
      IXL=1150
      IXL1=180
      JXL=250
      HT=11.0
      NCHAR=12
      IHAVL(1)=0
      IHAVL(2)=0
      ITAPUT=IFULL
c 3/15/96 George VandenBerghe  changed INAM to CNAM characters in putlab
cj      CALL PUTLAB(IXL,JXL,HT,CNAM1(1),0.0,NCHAR,0,ITAPUT)
C      CALL PUTLAB(IXL1,JXL,HT,CNAM1(1),0.0,NCHAR,0,ITAPUT)
C      JXL=JXL-35
C      CALL PUTLAB(IXL,JXL,HT,CNAM2(1),0.0,NCHAR,0,ITAPUT)
C      CALL PUTLAB(IXL1,JXL,HT,CNAM2(1),0.0,NCHAR,0,ITAPUT)
C      JXL=JXL-35
C      CALL PUTLAB(IXL,JXL,HT,CNAM3(1),0.0,NCHAR,0,ITAPUT)
C      CALL PUTLAB(IXL1,JXL,HT,CNAM3(1),0.0,NCHAR,0,ITAPUT)
C
c    3/27/96  George V. Changed CNAM to CN12 in putlab calls 
c    and changed integer arg#9 in putlab to two word array
      kvv(1)=0
      kvv(2)=0
      CALL PUTLAB(IXL,JXL,HT,CN12(1),0.0,NCHAR,kvv,ITAPUT)
      CALL PUTLAB(IXL1,JXL,HT,CN12(1),0.0,NCHAR,kvv,ITAPUT)
      JXL=JXL-35
      CALL PUTLAB(IXL,JXL,HT,CN12(2),0.0,NCHAR,kvv,ITAPUT)
      CALL PUTLAB(IXL1,JXL,HT,CN12(2),0.0,NCHAR,kvv,ITAPUT)
      JXL=JXL-35
      CALL PUTLAB(IXL,JXL,HT,CN12(3),0.0,NCHAR,kvv,ITAPUT)
      CALL PUTLAB(IXL1,JXL,HT,CN12(3),0.0,NCHAR,kvv,ITAPUT)
C     CALL MERCIN TO READ METEOROLOGICAL INFORMATION FOR THIS MAP
C
 108  CALL MERCIN(ISWTCH,NUMF,MAPON,NSLOTS,ILVLI,
     1            IFLDI,ITAUI,ICODIN,INAMIN,IOUTIN,IINFA,IINFB)
c***
      DO 100 I = 1,NUMFLD
        DO 110 J = 1,NUMTAU
           IF(ITAUI(I).EQ.J) ITAU(I) = MTAU(J)
  110   CONTINUE
  100 CONTINUE
c***
c
c***
      DO 120 I = 1,NUMFLD
        DO 130 J = 1,NUMLVL
          IF(ILVLI(I).EQ.J) LVL(I) = MLVL(J)
  130   CONTINUE
  120 CONTINUE
c***
c
c***
      DO 150 I = 1,NUMFLD
        DO 151 J = 1,NUMFLD
          IF(IFLDI(I).EQ.J) ITYPE(I) = MTYPE(J)
  151    CONTINUE
  150 CONTINUE
c***
C     READ IN TEMPERATURE FIELD
C
cc      CALL GETSXX(ITAU(3),LVL(3),ITYPE(3),FLD2,IDENT,IEXIT)
C     IF(IEXIT.NE.0) GO TO 350
C
C     PROCESS DATE
C
      JTAU(1)=IAND(IDENT(1),MASK8)
      IHOUR=IAND(IDENT(7),MASK8)
      IDAY=IAND(ISHFT(IDENT(7),-8),MASK8)
      IMONTH=IAND(ISHFT(IDENT(7),-16),MASK8)
      IYR=ISHFT(IDENT(7),-24)+1900
Corig PRINT 125,JTAU(1),IHOUR,IDAY,IMONTH,IYR
      WRITE(6,125) JTAU(1),IHOUR,IDAY,IMONTH,IYR
 125  FORMAT(1H0,'  TAU INCREMENTS=  ',I3,'  BASED ON IHOUR=  ',I3,
     X       '  IDAY=  ',I3,'  IMONTH=  ',I3,'  IYR=  ',I4)
C
C     UPDATE
C
c     CALL UPDATR
Corig PRINT 126,IUHR,ITAG,MONAT,JAHR
      WRITE(6,126) IUHR,ITAG,MONAT,JAHR
 126  FORMAT(1H0,'  UPDATE-IUHR=  ',I3,'  ITAG=  ',I3,
     X       '  MONAT=  ',I3,'  JAHR=  ',I4)
C
      GO TO 400
C
C     ERROR GETTING TEMPERATURES
C
 350  CONTINUE
Corig PRINT 360,IEXIT
      WRITE(6,360) IEXIT
 360  FORMAT(1H0,'  ERROR GETTING TEMPERATURE-IEXIT=  ',I3)
      GOTO 670
C
C     START PROCESSING DATA
C
 400  CONTINUE
Corig PRINT 410
      WRITE(6,410)
 410  FORMAT(1H0,'  DATA PROCESSING STARTED')
C
C     PLOT TEMPERATURES
C
c***
      CALL DOFRST(LVL(1),IDENT)
c***
      print 1934,'AF DOFRS'
         print 1934,(KTITLE(k),k=1,14)
1934  format('KTTL ',14a8)
C
C   WHICH SET UP FOR TITLES AND PUT OUT 'GULF' LABELS
cc      CALL PLTTLL(FLD2,KTTCTL,LVL(1))
Corig PRINT 420
      WRITE(6,420)
 420  FORMAT(1H0,'  PLOT OF TEMPERATURE COMPLETED  ')
C
C     READ IN U FIELD
C
cc      CALL GETSXX(ITAU(1),LVL(1),ITYPE(1),FLD1,IDENT,IEXIT)
      IF(IEXIT.NE.0) GO TO 600
C
C     READ IN V FIELD
C
cc     CALL GETSXX(ITAU(2),LVL(2),ITYPE(2),FLD2,IDENT,IEXIT)
      IF(IEXIT.NE.0) GO TO 650
      GO TO 700
C
C     ERROR GETTING U
C
 600  CONTINUE
Corig PRINT 610,IEXIT
      WRITE(6,610) IEXIT
 610  FORMAT(1H0,'  ERROR GETTING U-IEXIT=  ',I3)
      GOTO 670
C
C     ERROR GETTING V
C
 650  CONTINUE
Corig PRINT 660,IEXIT
      WRITE(6,660) IEXIT
 660  FORMAT(1H0,'  ERROR GETTING V-IEXIT=  ',I3)
C670  PRINT 675,IMP
 670  CONTINUE
      WRITE(6,675) IMP
 675  FORMAT('0SKIP MAP ',I2)
c
c***
      DO 690 IYUK=1,NSLOTS
      IF(ISWTCH.EQ.1) GO TO 685
Cori  READ 680, ICARD2,ISLOTA,ISLOTB,IPANA,IPANB,IAREA1,IAREA2,ISUB,
      READ (LU,680) ICARD2,ISLOTA,ISLOTB,IPANA,IPANB,IAREA1,IAREA2,ISUB,
     1   IFLAB,INSET,IRLAB,IREM1A,IREM1B,IREM1C,IREM2A,IREM2B,
     2   IREM3A,IREM3B,IREM3C,IREM4A,IREM4B,IMANOP,IMANOQ
 680  FORMAT(A1,3(A4,A1),4I5,2A4,A2,4A4,A2,4A4)
      GO TO 690
 685  READ (LU,680) ICARD2,ISLOTA,ISLOTB,IPANA,IPANB,IAREA1,IAREA2,ISUB
     1  ,IFLAB,INSET,IRLAB,IREM1A,IREM1B,IREM1C,IREM2A,IREM2B,
     2   IREM3A,IREM3B,IREM3C,IREM4A,IREM4B,IMANOP,IMANOQ
 690  CONTINUE
c***
      WRITE(IMESS2(25:32),FMT='(2HAP,I4,2H W)')MAPON
Corig  CALL CONSOL(MESS2)
C     PRINT(MESS2)
      GOTO 1306
C
C     PROCESS U,V S
C
 700  CONTINUE
      IMAPER=IMAPER+1
      IOPTN=1
c      CALL PLTWLL(FLD1,FLD2,KWNCTL,KTTCTL,IOPTN)
Corig PRINT 750
      WRITE(6,750)
 750  FORMAT(1H0,'  U AND V PROCESSED  ')
      INCR1=0
      NNN=0
      JX2=7375
C
C - - - - - - - - - - -SLOT LOOP- - - - - - - - - - - - - - - - - -
C
c******
      DO 900 ISLT=1,NSLOTS
c***
      CALL SLTCRD(ISWTCH,ISLOTA,ISLOTB,IPANA,IPANB,IRLAB,IREM3A,
     1   IREM3B,IREM3C,IREM4A,IREM4B,IAREA3,ISUB,IFLAB,INSET,IREM1A,
     1   IREM1B)
      IF(ISLT.EQ.1) ISLOT1=ISLOTA
      IF(ISLT.EQ.1) ISUB1=ISUB
         print 1934,'b avttl'
         print 1934,(KTITLE(k),k=1,14)
c      CALL AVTTLS(ISLOTA,ISLOTB,IPANA,INSET,IRLAB,JX2,INCR)
ckumar
       idot=1030
       jdot=30
       kang=0
       kflag=7
       call duck (idot,jdot,kang,kflag)
ckumar
          print 1934,' a avt'
         print 1934,(KTITLE(k),k=1,14)
         print 1934,'DUMMY '
         print 1934,'DUMMY '
C     fILL ISCHED ARRAY WITH INFORMATION FROM IAREA ARRAY
 895  CALL FAXSHD(IAREA,IA,JA,IAREA3,INCR1,INCR,JLAST,NNN)
      IF(IAREA3.EQ.KVAR)GOTO 900
      INCR1=INCR1+INCR
      IF(JX2.EQ.JLAST)GOTO 900
Corig PRINT 897,JX2,JLAST
      WRITE(6,897) JX2,JLAST
 897  FORMAT('  JX2=',I5,'  JLAST=',I5)
 900  CONTINUE
C
C     SETUP FOR 360 TYPEWRITER MESSAGE
C
      IVAR=IAND(ISLOT1,IMASK3)
      IF(NSLOTS.GT.1) GO TO 135
C........SO NSLOTS=1, EITHER FAX OR VARIAN
      IF(IVAR.NE.MAPT7) JFID(1)=IFFAX
      IF(IVAR.EQ.MAPT7) JFID(1)=IVFAX
      GO TO 145
 135  IF(IVAR.NE.MAPT7) GO TO 140
      IF(ISUB1.EQ.99) GO TO 140
C........TO ARRIVE HERE, IVAR=V AND ISUB1.NE.99, BOTH FAX AND VARIAN
      JFID(1)=IDFAX
      GO TO 145
 140  CONTINUE
      JFID(1)=IFFAX
      GO TO 145
 145  CONTINUE
C
      WRITE(KJFID(5:28),FMT='(I4,1X,I2,2HZ ,I2,1H/,I2,1X,I2,
     A2HHR,I4,1HM)') ISUB,IUHR,MONAT,ITAG,JTAU(1),LVL(1)
C
c       print 10177,(cjfid(k),k=1,56)
10177 format(' CDC DISPLCODE ARG ',56a1)
c      CALL ASC2ISP(56,KJFID,ciFID(1),IERR)
c      CALL ASC2ISP(56,JFID(1),IFID(1),IERR)
cc       cifid56=kjfid
c   GWV 2/18/97  additional ipak code (single line)
ckumar       call ipak(cifid56,isub)
      call ipak(cifid24,isub)
       ierr=0
      IF(IERR.EQ.0) GO TO 160
Corig PRINT 155
      WRITE(6,155)
 155  FORMAT(1H0,'  ERROR FORMAT FROM ASC2ISP  ')
 160  CONTINUE
Corig PRINT 165,(JFID(I),I=1,14)
      WRITE(6,165) (JFID(I),I=1,14)
 165  FORMAT(1H0,'  360 TYPEWRITER COMMENT=  ',14A4)
C???  CALL PDUMP(ISCHED(1,1),ISCHED(8,50),0)
C
C     OUTPUT MAP
C
C???  CALL PDUMP(LABEL(1,1),LABEL(2,LABIX),0)
      ICNT=0
      IRTRY=2
      ICK(1)=-1
      ICK(3)=0
      LCKPT=100
Corig PRINT 1210,LCKPT
      WRITE(6,1210) LCKPT
 1210 FORMAT(1H ,10X,'ARRIVED AT CHECKPOINT=',I4)
      CALL ZPLOT(0.0,0.0,999)
      LABEL(1,1)=-1
C
C??????? 1201 CALL CNTR(IL,LABEL,ICK,IFID,ISCHED,INDEX,ICNT)
       rewind (55)
c       print 9266,IMAGSIZ_WRDS,(il(kkk),kkk=1,15)
 9266  format(' TEMP SIZE',i14)
       print 1939,isched 
       print 1939
 1939  format(50(/,'isched ',8(Z16,1x)))
c
c***
       nflds=0
c
       call CNTR(ireg_cnt,IMAGE,IMAGSIZ_WRDS,
     1           il,LABEL,ichk,ifid,isched,d11,
     1           nflds,
     1           d12,d13,d14,d15,
     1           d22,d23,d24,d25,
     1           d32,d33,d34,d35,
     1           d42,d43,d44,d45) 
c***
C
 1201 CONTINUE
      IF(ICK(1).EQ.-1) GO TO  1205
      IF(ICK(1).EQ.0) GO TO 1202
      IF(ICK(1).EQ.1) GO TO 706
      STOP 1206
 1202 IRTRY=IRTRY-1
      IF(IRTRY.EQ.0) GO TO 707
      GO TO 1201
 1205 CONTINUE
      LCKPT=101
Corig PRINT 1210,LCKPT
      WRITE(6,1210) LCKPT
 1306 CONTINUE
        print *, 'CALLING ENDMAP'
      CALL ENDMAP(IFAXOUT,NRECFAX)
      CLOSE (IFAXOUT)
      LABEL(1,1) = SFL
      ENDFILE NU
C
C     CLOSE S24 FILE
C
      CALL GETSXX(999,500,ITYPE,FLD2,IDENT,IEXIT)
C
C     INTERFACE LOGIC
C
C
      WRITE(6,770) IMAPER,NMAPS
 770  FORMAT(5X,I2,' OF ',I2,' MAPS COMPLETED - THANK YOU')
      WRITE(IMESS3(9:20),FMT='(3H** ,I2,4H OF ,I2,1H )')IMAPER,NMAPS
C???   CALL CONSOL(MESS3)
Corig PRINT(MESS3)
      GO TO 780
C706  PRINT 761
 706  CONTINUE
      WRITE(6,761)
 761  FORMAT(1H0,'BAD BACKGROUND INPUT DISK')
      ISTOP = 3050
       GO TO 9999
C707  PRINT 765
 707  CONTINUE
      WRITE(6,765)
 765  FORMAT(1H0,'BAD OUTPUT TAPE')
      ISTOP  = 3060
       GO TO 9999
 780  CONTINUE
      IF(IMAPER.NE.NMAPS) ISTOP = 1
9999  CONTINUE
      CALL W3TAGE('GRAPH_SIXBITB')
C      CALL W3AS02(ISTOP)
Corig WRITE(NSTOP,FMT='(I4)') ISTOP
      STOP
      END

