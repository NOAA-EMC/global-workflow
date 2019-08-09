C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
C                .      .    .                                       .  
C MAIN PROGRAM: GRAPH_TRPSFPRV
C   PRGMMR: KUMAR            ORG: NP12        DATE: 2000-02-16
C ABSTRACT: READS INTERPOLATED SPECTRAL DATA PREPARED BY RUSS JONES     
C   IN A MERCATOR GRID(117X51).  IT PRODUCES ANY NUMBER OF COMBINATIONS 
C   OF FIELDS AND LEVELS OUT TO 48 HOURS ON A MERCATOR BACKGROUND FOR   
C   FAX ONLY.                                                           
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   YY-MM-DD  ORIGINAL AUTHOR(S)'S NAME(S) HERE                         
C   86-07-30  MILLER      DESCRIPTION OF CHANGE                         
C   88-03-24  FARLEY      REPLACED XDAM I/O WITH VSAM I/O               
C   98-06-30  KRISHNA KUMAR. Modified the contour labelling in the
C             fax products output from trpanl. The contours are now
C             labelled every 10 grid points beginning in 5 and ending
C             in 115 in the J - direction (longitude)
C 2000-02-04  KRISHNA KUMAR Converted this code from CRAY to IBM RS/6000
C                           SP system. 
C                                                                       
C USAGE:                                                                
C   INPUT FILES:                                                        
C     TRPGRD   -   FORECAST DATA FIELDS FILE                            
C     FT02F001 -   GRAPHICS BACKGROUND FILE                             
C     FT05F001 -   INTERFACE CONTROL CARD                               
C     FT08F001 -   MAP SCHEDULE CONTRO FILE                             
C                                                                       
C   OUTPUT FILES:                                                       
C     FT01F001 -   GRAPHICS OUTPUT MAPS (VARIAN,FAX)                    
C     FT06F001 -   ALL PRINT OUTPUT, CHECKPOINTS, ETC.                  
C     FT09F001 -   ALL DUMPS, ERROR MESSAGES.                           
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C     UNIQUE:     CENTRE CLOSEF CLOSES CLOSET FAXSHD FLIPTR GETGES      
C                 ISOTAC ISP2EB MAPX MERCIN POTEX REDFLD REDUCX RNCNTL  
C                 SLTCRD SPCHK  STR TITLES UVCLIP WNDPLO WWNDR          
C     LIBRARY:                                                          
C       COMMON   -   CONSOL ENCODE DECODE ERRMSG                        
C       W3LIB    -   W3AI01 W3AI15 W3AS00 W3FK40 W3FK43 W3FK41 W3AQ13   
C                    W3FM03 W3FP02
C       GRAPHICS -   CNTR   DUCK   WNDBRK                               
C                                                                       
C   EXIT STATES:                                                        
C     COND =   0 SUCCESSFUL RUN                                         
C          =  18 DESIRED FIELD NOT IN DISK TABLE                        
C          =  19 RETRIEVED DATA HAS WRONG ID                            
C          =  74 BAD RUN CARD                                           
C          = 711 END OF FILE ON INPUT SCHEDULE FILE                     
C          = 715 WRONG TIME                                             
C          = 761 BAD BACKGROUND INPUT DISK                              
C          = 765 BAD OUTPUT FILE                                        
C                                                                       
C   REMARKS: 24 AND 48 HR 1000MB STREAMS WITH PLOTTED WIND BARBS AT     
C     THE 12Z CYCLE ARE THE ONLY TWO CHARTS NOW PRODUCED OPERATIONALLY. 
C     DUCKLING ADDED DECEMBER 4, 1985.                                  
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                        
C   MACHINE:  IBM RS/6000 SP                                                       
C                                                                       
C$$$                                                                    
C                                                                       
C     ...TEST MERCIN                                                    
      COMMON/BOBINS/LMERC(256),IDMERC(1539)
      COMMON/XZPLAX/XZPLOT,YZPLOT,IORITY
      COMMON/KPLOT/LABEL(2,1024),LABIX,NOBUF,IDRA(50)
      COMMON/PACKRA/IRAS(10)
      COMMON/PUTARG/PUTHGT,PUTANG,IPRPUT,ITAPUT
      COMMON /NSCHED/ ISLOTA,IPANA,ISUB,IFLAB,INSET,IRLAB,ISCHED(8,50)
      COMMON/ADJ2/XIDID,YJDID
      COMMON/ADJ5/IRTKOR,IUPKOR
      COMMON/FIXFLD/XIM(51),F(51)
      COMMON   /ISPACE/WORKS(18172)
      COMMON/YUCK/FLD1(48,119),FLD2(48,119),FLD3(48,119),
     &                                      fld4(48,119)   
      COMMON/ILY/IT1A,LV1A,IFID(14),JFID(14)
      character*56 cjfid,cifid
      equivalence (cjfid,jfid),(cifid,ifid)
      character*4 cvar4
      DIMENSION IL(15)
      DIMENSION INDEX(6)
      DIMENSION ICK(3)
      DIMENSION ILVLI(3),IFLDI(3),ITAUI(3),IOUTIN(2)
      DIMENSION NTAU (5)
ckumar      DIMENSION MSG1(12)
ckumar      DIMENSION MESS4(11)
      character*4 msg1(12),mess4(11)
      character*48 xmsg1
      character*44 xmess4
      equivalence (xmsg1, msg1)
      equivalence (xmess4, mess4)
ckumar
      DIMENSION IARR2(3)
c      character*4 iarr2(3)
ckumar
      DIMENSION LICYL(2)
      DIMENSION IFLO(5),IFHI(5),IFF(5),IFC(5),IFG(5)
      DIMENSION JSTPK(30),JSTPL(30),ITABMB(7),ITABFL(7),KSTPK(30),
     1   KSTPL(30)
ckumar      DIMENSION ICFIX(6),JCFIX(10),KCFIX(6),LTHERM(4),LVWS(6),LFL(6)
      DIMENSION ICFIX(12),JCFIX(12),KCFIX(12),LTHERM(12),LVWS(12),
     1   LFL(12)
      DIMENSION IAREA(8,26)
      INTEGER   DASH1(2),DW1(2),SHAD(2),DASH2(2),DW2(2),DASH3(2),DW3(2)
ckumar
      integer  dash4(2),dw4(2),shad1(2),shad2(2),shad3(2),shad4(2)
ckumar
ckumar      INTEGER   ISCHED(8,50)
       integer ihash  
       data ihash/Z'FFFFFFFFFFFF0000' /
ckumar      REAL*8 MERCI
      CHARACTER*8 MERCI
ckumar
      DIMENSION ITITLE(17,8)
      DIMENSION IPTITL(4)
      INTEGER   IHOUR(5)
      INTEGER   ITIM
      INTEGER   IHAVL(2)
      INTEGER   TITLE(17)
      INTEGER   ME (2)
      data me     /'4hBIN ','4hA3  '/
      INTEGER   MON(12)
c      LOGICAL   LPARM(100)                                             
      character*1 lparm(100)
c      LOGICAL   LBLANK                                                 
       character*1 lblank
c      LOGICAL   LEV(4)                                                 
      character*1 lev(4)
      LOGICAL  PFAXMP,PVARMP,PAFOMP
      EQUIVALENCE (IHAVL(1),KPRIOR)
      EQUIVALENCE (T1,IL(15))
      EQUIVALENCE (LEV(1),LV1B)
      DATA ITITLE/
     1           '24 H','R  7','00 M','B   ',7*'    ','   V','ALID',
     2            4*'    ',
     3           '24 H','R  5','00 M','B ST','REAM','S  I','SOTA',
     4           'CHS ',' TEM','PERA','TURE','S  V','ALID',4*'    ',
     5           '24 H','R  4','00 M','B   ',7*'    ','   V','ALID',
     6           4*'    ',
     7           '24 H','R  3','00 M','B   ',7*'    ','   V','ALID',
     8           4*'    ',
     9           '24 H','R  2','00 M','B   ',7*'    ','   V','ALID',
     X           4*'    ',
     1           '24 H','R  2','50 M','B ST','REAM','S  I','SOTA',
     2           'CHS ',' TEM','PERA','TURE','S  V','ALID',4*'    ',
     3           '24 H','R  T','ROPO','PAUS','E PR','ESSU','RE T',
     4           'EMPS',' VER','T WN','D SH','R  V','ALID',4*'    ',
     5           '24 H','R 10','00 M','B ST','REAM','S   ',5*'    ',
     6           '   V','ALID',4*'    '/
      DATA    IMAX/48/,JMAX/119/
      DATA    LBLTAP/55/
      DATA    T1/30.0/
      DATA    LEV/' ',' ',' ',' '/
      DATA    LBLANK/' '/
      DATA    IL(1),IL(2)/'MR4002',0/
      DATA    IL(3),IL(4),IL(5),IL(6)/97,0,1440,3540/
c      DATA    IL(3),IL(4),IL(5),IL(6)/97,0,1440,3840/
      DATA    IL(7),IL(8),IL(9),IL(10)/97,0,1440,3540/
c      DATA    IL(11),IL(12)/-60,-2/
c      DATA    IL(11),IL(12)/30,298/
      DATA    IL(11),IL(12)/00,0 /
c      DATA    IL(11),IL(12)/00,-302/
c      DATA    IL(11),IL(12)/30,-2/
      DATA    IL(13),IL(14)/-4,0/
      DATA    INDEX/119,26,1,48,-2,24/
ckumar
ckumar used block data statements separately !!!!
ckumar      DATA ISCHED/4390,1,0,3600,Z'8000',Z'D800',0,0,392*0/              

      DATA    ICK/0,18172,0/
ckumar      DATA    MSG1/4HNWS ,4H;@20,4H**  ,4HTROP,4HICAL,4H PRG,4HCOMP,
ckumar     1   4HILED,4H 04/,4H20/8,4H1***,4H  ;:/
      data xmsg1 /
     &  'NWS ;@20TROPICAL PRG COMPILED 04 20_8   1***  ;:'/
ckumar
      DATA    MASKFF/Z'FF'/
      DATA    NU/1/
      DATA    JU/8/
      DATA    MERCI/'TRPGRD  '/
      DATA    MXREC2/255/
      DATA IHOUR/2H18,2H24,2H30,2H36,2H48/
      DATA NTAU /4H18 H,4H24 H,4H30 H,4H36 H,4H48 H/
      DATA MON/  3HJAN,3HFEB,3HMAR,3HAPR,3HMAY,3HJUN,
     13HJUL,3HAUG,3HSEP,3HOCT,3HNOV,3HDEC/
c      LOGICAL   LA/'A'/                                                
      character*1 la
      data la/'A'/
ckumar      DATA    MESS4/4HNWS ,4H;@20,4H**  ,4H   1,4H  OF,4H   1,4H  MA,
ckumar     1              4HPS C,4HOMPL,4HETED,4H**;:/
      data xmess4/
     &  'NWS ;@20 **  1  OF  1   MAPS  COMPLETED **;;'/
ckumar
      DATA    ITBSE/7400/
      DATA    INCR/30/
      DATA    MXITR/7/
      DATA    S/.5/
      DATA    LICYL/3H00Z,3H12Z/
      DATA    iXLPLMI/3HI+-/
      DATA    iYLPLMI/3HS99/
      DATA    iZLPLMI/3HS+-/
              character*3 xlplmi,ylplmi,zlplmi
               equivalence(xlplmi,ixlplmi)
               equivalence(ylplmi,iylplmi)
               equivalence(zlplmi,izlplmi)
      DATA    IFLO/4H(1HC,4H,1H$,4H,A2),2*0/
      DATA    IFHI/4H(1HA,4H,1H$,4H,A2),2*0/
      DATA    IFF/4H(A3,,4H2HK$,4H,A2),2*0/
      DATA    IFC/4H(1HF,4H,A3,,4H1H$),2*0/
      DATA    IFG/4H(A3,,4H1H$),3*0/
ckumar      DATA    JSTPK/36,68,72,80,100,114,24*0/
      DATA    JSTPK/5,15,25,35,45,55,65,75,85,95,105,115,18*0/
ckumar      DATA    JSTPL/44,44,44,44,44,44,24*0/
      DATA    JSTPL/44,44,44,44,44,44,44,44,44,44,44,44,18*0/
      DATA    ITABMB/400,350,300,250,200,150,100/
      DATA    ITABFL/240,270,300,340,390,450,530/
ckumar      DATA    KSTPK/28,48,58,76,90,108,24*0/
      DATA    KSTPK/5,15,25,35,45,55,65,75,85,95,105,115,18*0/
ckumar      DATA    KSTPL/6*44,24*0/
      DATA    KSTPL/12*44,18*0/
ckumar      DATA    ICFIX/36,68,72,80,100,114/
      DATA    ICFIX/5,15,25,35,45,55,65,75,85,95,105,115/
ckumar      DATA    JCFIX/38,52,60,66,70,82,90,98,106,112/
      DATA    JCFIX/5,15,25,35,45,55,65,75,85,95,105,115/
ckumar      DATA    KCFIX/28,48,58,76,90,108/
      DATA    KCFIX/5,15,25,35,45,55,65,75,85,95,105,115/
ckumar      DATA    LTHERM /9,32,60,86/
      DATA LTHERM/5,15,25,35,45,55,65,75,85,95,105,115/
ckumar      DATA    LVWS /17,49,64,72,98,110/
      DATA    LVWS /5,15,25,35,45,55,65,75,85,95,105,115/
ckumar      DATA    LFL /21,36,53,76,102,114/
      DATA    LFL /5,15,25,35,45,55,65,75,85,95,105,115/
      DATA    MAPT7/Z'5600000000000000'/
c      DATA    MAPT7/Z'E5000000'/                                       
      DATA    JA/26/
      DATA    IA/8/
      DATA    ITHOU/'1000'/
      DATA    LV1B/'    '/
      DATA    IAREA/
     1   2320,677,Z'83',68,108,0,0,0,2320,1172,Z'83',68,108,0,1172,0,
     2   1,126,Z'5',68,108,0,0,1299,2891,601,Z'83',70,108,0,601,0,
     3   2891,193,Z'45',178,23,0,0,589,1,126,Z'5',70,108,0,0,730,
     4   967,1083,Z'83',92,109,0,0,0,967,1083,Z'83',95,53,0,0,0,
     5   1934,1160,Z'83',75,108,0,0,0,3094,400,Z'83',73,108,0,400,0,
     6   1,126,Z'5',73,108,0,0,526,2308,775,Z'83',78,108,0,0,0,
     7   1,3600,Z'80',0,216,0,0,0,1,1800,Z'80',0,216,0,0,0,
     8   967,1083,Z'81',92,109,0,0,0,1015,386,Z'83',92,109,0,0,0,
     9   2027,772,Z'81',112,103,0,0,0,2652,772,Z'81',112,103,0,0,0,
     1   1934,890,Z'83',75,108,0,0,0,2272,1220,Z'83',68,108,0,1220,0,
     2   1,176,Z'5',68,108,0,0,1397,1,772,Z'81',112,103,0,0,0,
     3   670,772,Z'81',112,103,0,0,0,1347,772,Z'81',112,103,0,0,0,
     4   1,3485,Z'83',75,108,0,0,0,1158,1352,Z'83',Z'3C',Z'6C',0,0,0/
      DATA    IBCHK/4H    /
      DATA    IONE/1H1/
      DATA    IPTITL/'MB S','TRM/','ITAC','H   '/
c -----------------new gwv cntr stuff
       INTEGER      MAXIWORD
       PARAMETER   (MAXIWORD=36)
 
       INTEGER      MAXJSLINE
       PARAMETER   (MAXJSLINE=5828)
 
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     ... ALLOCATE A BIG 1-D AREA for the image bitplane ...
 
      INTEGER    IMAGSIZ_WRDS
      PARAMETER (IMAGSIZ_WRDS=MAXIWORD*MAXJSLINE)
 
      COMMON   /PICTURE/IMAGE
      INTEGER           IMAGE(IMAGSIZ_WRDS)
       dimension krot_pri(2)
       character*4 ctext
ckumar
      COMMON/IOUTFAX/IFAXOUT,NRECFAX  ! ADDED FOR DIRECT ACCESS FOR FAX OUTPUT
C
      INTEGER IFAXOUT,NRECFAX
C
c---------------------------------------------------------- end new gwv 
C                                                                       
      IL(15)=30000 
      CALL W3TAGB('GRAPH_TRPSFPRV',2000,0047,0074,'NP12') 
ckumar
      NRECFAX = 0
      IFAXOUT = 81
ckumar
c     OPEN THE OUT PUT FAX FILE.

      OPEN (IFAXOUT, ACCESS='DIRECT', RECL=1440 )
c
ckumar
      CALL ERRMSG(9)
      CALL CONSOL(MSG1)
      REWIND NU
C                                                                       
C     ...INITIALIZE PUTLAB ARGUMENTS FOR CLABEL,CENTER CLOSED           
C                                                                       
      PUTHGT=3.0
      PUTANG=90.0
      IPRPUT=0
      ITAPUT=0
      ISUBV=0
      PAFOMP=.FALSE.
      PFAXMP=.FALSE.
      PVARMP=.FALSE.
c      ititialize label array
c      XZPLOT=1.0
c      YZPLOT=1.0
c      IORITY=2
c           CALL ZPLOT(0.0,0.0,-3)
c  now do it right!!
C
C       INITIALIZE LABEL ARRAY
C
c          LABIX = 0
c          NOBUF = 0
cC
c          IPT = 1
c          JPT = 0
c          HEIGHT = 1.0
c          ANGLE = 0.0
c          KROT_PRI(1) = 0
c          KROT_PRI(2) = 0
c          CTEXT(1:1) = '?'
c          CTEXT(2:2) = CHAR(1)
c          CTEXT(3:3) = '$'
c          CTEXT(4:4) = CHAR(0)
c          NCHAR = 2
c          ICMD = -2
c          CALL PUTLAB(IPT,JPT,HEIGHT,CTEXT,ANGLE,NCHAR,KROT_PRI,
c     X                ICMD)
c          CALL PUTLAB(IPT,JPT,HEIGHT,CTEXT,ANGLE,NCHAR,KROT_PRI,
c     X                -7)
c          do 18,k=1,9999
c          nchar=12
c          CALL PUTLAB(IPT,JPT+k,
c     1 HEIGHT,' now is the time',ANGLE,NCHAR,KROT_PRI,
c     X                ICMD)
c 18      continue
c          stop 'putlab'
C
C                                                                       
C     ...INITIALIZE CONSTANTS                                           
      CALL MAPX
C     ...WHERE MAPX FILLS /FIXFLD/ WITH LATITUDE DEPENDENT MERCATOR     
C     ...MAP CONSTANTS FOR EACH J ROW WHERE J=51                        
C                                                                       
      CALL RNCNTL(NMAPS,IDATC,IOPN,ISWTCH,KRUN1,ITOUT1,ICYC1,
     1   INOPN1,INOPN2,INOPNA,INOPNB)
C     ...INITIALIZE APPROPRIATE ID TABLES FOR INPUT FIELD DATA          
C                                                                       
c      CALL W3FK40(MERCI,LMERC,MXREC2)
c      CALL W3FK41(MERCI,IDMERC,MXREC2)
      ICYCLT=IDMERC(3)
      CALL GETGES(IRET)
      PRINT 222,IRET
  222 FORMAT(1H0,10X,'CALLED GETGES TO NPREPARE GES STREAM FOR POTEX
     1AND RETURNED  WITH IRET CODE = ',I2)
      IF(ISWTCH.EQ.0) GO TO 7025
 7000 CONTINUE
      READ(JU,7050,END=7020)ICARD3,IIRUN1,IIRUN2,IIJOB1,IIJOB2,IICYC,
     1   NMAPS
 7050 FORMAT(A1,2(A4,A1),A4,I5)
      IF(ICARD3.EQ.IONE) GO TO 7005
      PRINT 7003
 7003 FORMAT(1H0,'BAD RUN CARD FORMAT ON TAPE8 FILE')
      CALL W3TAGE('GRAPH_TRPSFPRV') 
      STOP 74
C                                                                       
C     ...FOUND CARD FORMAT ON TAPE8-RIGHT JOB-QQ                        
 7005 CONTINUE
      IF(INOPNA.EQ.IIJOB1 .AND. INOPNB.EQ.IIJOB2) GO TO 7010
      DO 7006 IY=1,NMAPS
      READ(JU,7200)ICARD1,LVL1A,LVL1B,IFLD1A,IFLD1B,ITAU1A,ITAU1B,
     2             LVL2A,LVL2B,IFLD2A,IFLD2B,ITAU2A,ITAU2B,LVL3A,
     3             LVL3B,IFLD3A,IFLD3B,ITAU3A,ITAU3B,ICODEA,
     3             ICODEB,IINFA,IINFB,IOUTA,IOUTB,MAPON,NSLOTS
 7200 FORMAT(A1,10(A4,A1),3X,2(A4,A1),2I4)
      DO 7006 IX=1,NSLOTS
      READ(JU,8200)ICARD2,ISLOTA,ISLOTB,IPANA,IPANB,IAREA1,IAREA2,ISUB,
     1   IFLAB,INSET,IRLAB,IREM1A,IREM1B,IREM1C,IREM2A,IREM2B,
     2   IREM3A,IREM3B,IREM3C,IREM4A,IREM4B,IMANOP,IMANOQ
c       print 82000,' 9999 SLOT PRINT icard2,islota,islotb,ipana',
c     1  icard2,islota,islotb,ipana,ipanb,' ', ' ', 'a1,3(a4,a1)'
c       print 82001,'9999 SLOT PRINT icard2',icard2
c       print 82001,'9999 SLOT PRINT islota ',islota
c       print 82001,'9999 SLOT PRINT islotb ',islotb
c       print 82001,'9999 SLOT PRINT ipana',ipana
c       print 82001,'9999 SLOT PRINT ipanb ',ipanb
82001  format(a30,a5)
      IF(IX .EQ. 1)ISUBV=ISUB
      CALL IDCHCK(NSLOTS,IX,ISUBV,ISUB,PVARMP,PFAXMP,PAFOMP)
 7006 CONTINUE
      GO TO 7000
 7020 CONTINUE
      PRINT 7007
 7007 FORMAT(1H0,'END OF FILE ON TAPE8 BEFORE FINDING OPNL JOB TYPE')
      CALL W3TAGE('GRAPH_TRPSFPRV') 
      STOP 711
 7010 CONTINUE
      PRINT 7015,IIJOB1,IIJOB2
 7015 FORMAT(1H0,'OPNL JOB FORMAT FOUND ON TAPE8=  ',A4,A1)
 7025 CONTINUE
      IMAPER=0
      DO 100 IT=1,NMAPS
      CALL MERCIN(ISWTCH,NUMF,MAPON,NSLOTS,ILVLI,
     1     IFLDI,ITAUI,ICODIN,INAMIN,IOUTIN,IINFA,IINFB)
      CALL REDFLD(ISWTCH,NUMF,ILVLI,IFLDI,ITAUI,IARR2,IERR)
      IF(IERR.EQ.0) GO TO 93
      PRINT 95, MAPON,IERR
   95 FORMAT('0ID ERROR W3FK43-MAPON= ',I4,2X,I3,2X,'IERR=',I3)
      PRINT 353
  353 FORMAT(1H0,'MAP WAS SKIPPED')
      DO 97 KK=1,NSLOTS
      IF(ISWTCH.EQ.1) GO TO 99
      READ 8200, ICARD2,ISLOTA,ISLOTB,IPANA,IPANB,IAREA1,IAREA2,ISUB,
     1   IFLAB,INSET,IRLAB,IREM1A,IREM1B,IREM1C,IREM2A,IREM2B,
     2   IREM3A,IREM3B,IREM3C,IREM4A,IREM4B,IMANOP,IMANOQ
 8200 FORMAT(A1,3(A4,A1),4I5,2A4,A2,4A4,A2,4A4)
82000  FORMAT(a50,A1,3(A4,A1),a20,4I5,2A4,A2,4A4,A2,4A4)
      IF(KK .EQ. 1)ISUBV=ISUB
      CALL IDCHCK(NSLOTS,KK,ISUBV,ISUB,PVARMP,PFAXMP,PAFOMP)
      GO TO 97
   99 CONTINUE
      READ(JU,8200) ICARD2,ISLOTA,ISLOTB,IPANA,IPANB,IAREA1,IAREA2,ISUB,
     1   IFLAB,INSET,IRLAB,IREM1A,IREM1B,IREM1C,IREM2A,IREM2B,
     2   IREM3A,IREM3B,IREM3C,IREM4A,IREM4B,IMANOP,IMANOQ
      IF(KK .EQ. 1)ISUBV=ISUB
      CALL IDCHCK(NSLOTS,KK,ISUBV,ISUB,PVARMP,PFAXMP,PAFOMP)
   97 CONTINUE
      GO TO 100
   93 CONTINUE
      IMAPER=IMAPER+1
      INCR1=0
      NNN=0
      MLAST=ITBSE-INCR
      DO 1340 J=1,50
      DO 1340 I=1,8
      ISCHED(I,J)=0
 1340 CONTINUE
      DO 1500 JK=1,NSLOTS
      CALL SLTCRD(ISWTCH,ISLOTA,ISLOTB,IPANA,IPANB,IRLAB,IREM3A,IREM3B,
     1   IREM3C,IREM4A,IREM4B,IAREA3,ISUB,IFLAB,INSET,IREM1A,IREM1B)
C                                                                       
C     ...PUT IPAK INFORMATION INTO IFID(1)                              
C                                                                       
      IF(JK .EQ. 1)ISUBV=ISUB
      CALL IDCHCK(NSLOTS,JK,ISUBV,ISUB,PVARMP,PFAXMP,PAFOMP)
c     2/11/97 gwv  build ipak information in cjfid (part 1)
       write(cvar4,144)ifid(1)
 144   format(a4)
       write(cjfid(1:4),144) cvar4
c   end 2/11 mod 1
C                                                                       
      CALL TITLES(ISLOTA,ISLOTB,IPANA,IOUTIN,ILVLI,IARR2,IRLAB,INCR,
     1   INCR1,MLAST,ITAUI,IVAR,JLAST,INOPN1,INOPN2,IREM3A,IREM3B,IREM1A
     1,IREM1B
     1, IFLD1A,IFLD1B)
      CALL FAXSHD(IAREA,IA,JA,IAREA3,INCR1,INCR,JLAST,NNN)
      IF(IVAR.EQ.MAPT7) GO TO 1500
      INCR1=INCR1+INCR
      MLAST=JLAST
 1500 CONTINUE
C                                                                       
C     ...INSERT DUMMY STRIP                                             
C                                                                       
      IF(IVAR.EQ.MAPT7) GO TO 1501
      JLAST=JLAST+INCR
      JXL=JLAST
      IXL=1
      NCHAR=4
      KTEXT=IBCHK
C     CALL PUTLAB(IXL,JXL,1.0,KTEXT,0.0,NCHAR,0,0)                      
 1501 CONTINUE
      IX=ILVLI(1)
      JX=ITAUI(1)
      DO 1505 I=1,17
      TITLE(I)=ITITLE(I,IX)
 1505 CONTINUE
      ITIM=IHOUR(JX)
      TITLE(1) = NTAU(JX)
      CALL W3AI15(IRAS(3),IRHR,1,2,1H-)
      CALL W3AI15(IRAS(4),IRDA,1,2,1H-)
      IRMO=IRAS(5)
      CALL W3AI15(IRAS(6),IRYR,1,2,1H-)
      CALL ENCODE(TITLE(14),16)
      WRITE(99,122)IRHR,MON(IRMO),IRDA,IRYR
  122 FORMAT(1X,A2,2HZ ,A3,1X,A2,3H 19,A2)
C     CALL PUTLAB(5,1,11.0,TITLE(1),0.0,68,1,0)                         
C     CALL PUTLAB(20,70,11.0,ME,0.0,6,1,0)                              
C                                                                       
C     ...CONSTRUCT IPAK TITLE                                           
C                                                                       
      DO 1506 I7=2,14
         IFID(I7) = IBCHK
 1506 CONTINUE
C                                                                       
C     ...PREPARE BASE TIME FOR TYPEWRITER COMMENT                       
C                                                                       
      KMO = IRAS(9)
      CALL W3AI15(IRAS(8),KDA,1,2,1H-)
      CALL W3AI15(IRAS(7),KHR,1,2,1H-)
      LV1B = LV1A
      IF(LV1B .EQ. ITHOU)GO TO 1507
        LEV(4)=LEV(3)
        LEV(3)=LEV(2)
        LEV(2)=LEV(1)
        LEV(1)=LBLANK
 1507 CALL ENCODE(IFID(3),36)
      WRITE(99,1405)KMO,KDA,KHR,IT1A,LV1B,(IPTITL(I3),I3=1,4)
 1405 FORMAT(I2,1H/,A2,1H/,A2,4HZ   ,6A4)
c  gwv 2/11/97  build ipak title (part 2)
      write(cjfid(9:56),1405)
     1 KMO,KDA,KHR,IT1A,LV1B,(IPTITL(I3),I3=1,4)
c  end 2/11 mod part 2
      PRINT 1409,(IFID(I9),I9=1,14)
 1409 FORMAT(1H0,'360 TYPEWRITER COMMENT=  ',14A4)
C                                                                       
c      CALL EB2ISP(56,IFID(1),JFID(1),IERR)
c      CALL ASC2ISP(56,IFID(1),JFID(1),IERR)
c gwv 2/11/97  commented out conversion below since it's  now done
c  in cntr package
c      CALL ASC2ISP(56,cifid,cjfid,IERR)
C                                                                       
      PRINT 511
  511 FORMAT('0SCHEDULE BIN FOLLOWS')
c      CALL PDUMP(ISCHED(1,1),ISCHED(8,50),0)
C                                                                       
C     ...SETUP FOR MAP OUTPUT                                           
C                                                                       
C     ...CONTOUR LABEL SUBROUTINES FOLLOW                               
C     ...CLOSES CONTOURS LABELS FROM CENTER VALUES                      
C     ...CLOSET CONTOURS LABELS FROM CONTOUR STRIP VALUES               
C                                                                       
C                                                                       
C     ...LOX IA A FLAG FOR DIRECT(=0)/INDIRECT(=1) CONTOUR LABEL VALUES 
C                                                                       
C     ...ICK(1)=1 GOOD MAP                                              
C     ...ICK(1)=2 BAD OUTPUT TAPE                                       
C     ...ICK(1)=3 BAD BACKGROUND TAPE                                   
C                                                                       
      IRTRY=3
      ICHOOS=IOUTIN(2)
      print *,'ICHOOS  9999',ichoos
      gO TO (1140,1110,1110,1120,1130,1130),ICHOOS
C                                                                       
C                                                                       
C     ...STREAMS, ISOTACHS-ISOTHERMS- WITH PLOTTED WINDS                
C                                                                       
C     ...IOUTIN(2)=3                                                    
C     ...WHERE FLD1=STREAMS                                             
C     ...WHERE FLD2=ISOTACHS                                            
C     ...WHERE FLD3=ISOTHERMS                                           
C                                                                       
 1110 CONTINUE
      print *,' AFTER 1110 CONTINUE 9999'
C                                                                       
C     ...FIND STREAM CENTERS                                            
C                                                                       
      ICEN1=1
      XLIM=-1.
c      XS=-T1
      xs=30.
      M=1
      XIDID=-5.0
      YJDID=-37.0
      A1=111.0
      B1=60.0
      PUTHGT=11.0
      IHAVL(1)=1
C  ROTATE CENTER LABELS 90 DEGREES                                      
      IHAVL(2)=1
      IPRPUT=KPRIOR
      print *,'BEFORE CENTRE 9999'
      CALL CENTRE(FLD1,IMAX,JMAX,XS,A1,B1,M,XLIM,ICEN1,XLPLMI,IFLO,
     1        IFHI)
      print *,'AFTER  CENTRE 9999'
C                                                                       
C     ...FIND ISOTACH CONTOUR LABELS FROM CENTERS                       
C                                                                       
      M=3
      LOX=0
      IUP=4
      IRTKOR=-6
      IUPKOR=-20
      A1=-6.5
      B1=20.0
      IPRPUT=2
      PUTHGT=3.0
      CALL CLOSES(FLD2,IMAX,JMAX,S,A1,B1,M,IUP,YLPLMI,IFF,JSTPK,
     1        JSTPL,LOX,ITABMB,ITABFL,MXITR)
C                                                                       
C     ...FIND ISOTACH CONTOUR LABELS FROM STRIPS                        
C                                                                       
      LOX=0
      IFIX=2
      IUP=42
      M=3
      PUTHGT=3.0
      IPRPUT=0
      IRTKOR=-6
      IUPKOR=-20
ckumar      DO 1111 ILAB=1,6
      DO 1111 ILAB=1,12
      JFIX=ICFIX(ILAB)
      CALL CLOSET(FLD2,IMAX,JMAX,S,A1,B1,M,IUP,IFIX,JFIX,YLPLMI,IFF,LOX,
     1        ITABMB,ITABFL,MXITR)
 1111 CONTINUE
C                                                                       
C     ...FIND ISOTHERM CONTOUR LABELS FROM CENTERS                      
C                                                                       
      M=3
      LOX=0
      IUP=4
      IRTKOR=-1
      IUPKOR=-35
      A1=0.0
      B1=5.0
      IPRPUT=2
      PUTHGT=3.0
      CALL CLOSES(FLD3,IMAX,JMAX,S,A1,B1,M,IUP,ZLPLMI,IFG,KSTPK,KSTPL,
     1            LOX,ITABMB,ITABFL,MXITR)
C                                                                       
C     ...FIND ISOTHERM CONTOUR LABELS FROM STRIPS                       
      LOX=0
      IFIX=2
      IUP=42
      M=3
      PUTHGT=3.0
      IPRPUT=0
      IRTKOR=-1
      IUPKOR=-35
ckumar      DO 1123 ILAB=1,6
      DO 1123 ILAB=1,12
      JFIX=KCFIX(ILAB)
      CALL CLOSET(FLD3,IMAX,JMAX,S,A1,B1,M,IUP,
     1 IFIX,JFIX,ZLPLMI,IFG,LOX,
     1   ITABMB,ITABFL,MXITR)
 1123 CONTINUE
C                                                                       
C     ...SETUP FOR OUTPUT MAP-STREAMS/ISOTACHS/ISOTHERMS WITH           
C     ...PLOTTED WINDS                                                  
C                                                                       
      ICNT=3
      DW1(1)=2
      DW1(2)=0
C     ...FOR REGULAR LINES                                              
      DW2(1)=0
      DW2(2)=0
      DW3(1)=0
      DW3(2)=0
C     ...TO MAKE DOUBLE WEIGHT LINES                                    
      DASH1(1)=0
      DASH1(2)=0
      DASH3(1)=0
      DASH3(2)=0
C     ...NO DASHED LINES                                                
      SHAD(1)=0
      SHAD(2)=0
C     ...NO SHADING                                                     
      DASH2(1)=7
      DASH2(2)=4
      DASH2(1)=20
      DASH2(2)=12
c      dash2(1)=ihash
c       dash2(2)=ihash
C     ...FOR DASHED LINES                                               
      GO TO 1400
C                                                                       
C                                                                       
C                                                                       
C     ...STREAMS -WITH PLOTTED WINDS                                    
C                                                                       
C     ...IOUTIN(2)=4                                                    
C     ...WHERE FLD1=STREAMS                                             
C                                                                       
C                                                                       
C                                                                       
 1120 CONTINUE
       print *,' 9999  AFTER 1120 CONTINUE'
C                                                                       
C     ...FIND STREAM CENTERS                                            
C                                                                       
      ICEN1=1
      XLIM=-1.
      XS=-T1
      xs=-30
      M=1
      XIDID=-5.0
      YJDID=-37.0
      A1=111.0
      B1=30.0
      PUTHGT=11.0
      IHAVL(1) = 1
      IHAVL(2) = 1
      IPRPUT=KPRIOR
      CALL CENTRE(FLD1,IMAX,JMAX,XS,A1,B1,M,XLIM,ICEN1,XLPLMI,IFLO,
     1        IFHI)
C                                                                       
C     ...SETUP FOR OUTPUT MAP-STREAMS WITH PLOTTED WINDS                
C                                                                       
      WRITE(6,92)
   92 FORMAT(    10X,'***** PASSING THROUGH STREAMS-WINDS SECTION')
      ICNT=1
      DW1(1)=2
      DW1(2)=0
C     ...FOR DOUBLE WEIGHT LINES                                        
      DASH1(1)=0
      DASH1(2)=0
C     ...NO DASHED LINES                                                
      SHAD(1)=0
      SHAD(2)=0
C     ...NO SHADING                                                     
      GO TO 1200
C     ...TROPOPAUSE PRESSURE                                            
C                                                                       
C     ...IOUTIN(2)=5                                                    
C     ...FLD1=TROPOPAUSE PRESSURE                                       
C                                                                       
 1130 CONTINUE
        print *,' after 1130 continue 9999'
C                                                                       
C     ...FIND TROPOPAUSE PRESSURE CONTOUR LABELS FROM STRIPS            
C     ...USE INDIRECT FLIGHT LEVEL VALUES                               
      LOX=1
      IFIX=2
      IUP=42
      M=3
      A3=0.0
      B3=50.0
      PUTHGT=3.0
      IPRPUT=0
      IRTKOR= 0
      IUPKOR=-50
ckumar      DO 1122 ILAB=1,6
      DO 1122 ILAB=1,12
      JFIX=LFL(ILAB)
      CALL CLOSET(FLD1,IMAX,JMAX,S,A3,B3,M,IUP,IFIX,JFIX,YLPLMI,IFC,LOX,
     1        ITABMB,ITABFL,MXITR)
 1122 CONTINUE
C                                                                       
C******************************************************************     
C FIND ISOTHERM CONTOUR LABELS FOR CENTERS                              
C******************************************************************     
      M = 3
      LOX = 0
      IUP = 4
      IRTKOR = -1
      IUPKOR = -31
      A3 = 0.0
      B3 = 5.0
      IPRPUT = 2
      PUTHGT = 3.0
      CALL CLOSES(FLD2,IMAX,JMAX,S,A3,B3,M,IUP,ZLPLMI,IFG,KSTPK,ISTPL,
     +            LOX,ITABMB,ITABFL,MXITR)
C                                                                       
C******************************************************************     
C FIND ISOTHERM CONTOUR LABELS FROM STRIPS                              
C******************************************************************     
      IUPKOR = -45
      IRTKOR =   0
      IPRPUT =   0
      PUTHGT =   3.0
      IFIX   =   2
      LOX    =   0
      IUP    =  42
      M      =   3
ckumar      DO 800  ILAB=1,4
         DO 800  ILAB=1,12
         JFIX = LTHERM(ILAB)
         CALL CLOSET(FLD2,IMAX,JMAX,S,A3,B3,M,IUP,IFIX,JFIX,ZLPLMI,IFG,
     +               LOX,ITABMB,ITABFL,MXITR)
  800 CONTINUE
C                                                                       
C******************************************************************     
C FIND VWS CONTOUR LABELS IN STRIPS                                     
C*  ***************************************************************     
      IUPKOR = -40
      IRTKOR = -5
      PUTHGT =   3.0
      IFIX   =   2
      IUP   =  42
      LOX   =   0
      A3    =   0.0
      B3    =   2.0
      M     =   3
ckumar      DO 805  ILAB=1,6
         DO 805  ILAB=1,12
         JFIX = LVWS(ILAB)
         CALL CLOSET(FLD3,IMAX,JMAX,S,A3,B3,M,IUP,IFIX,JFIX,ZLPLMI,IFG,
     +               LOX,ITABMB,ITABFL,MXITR)
  805 CONTINUE
C                                                                       
C     ...SETUP FOR OUTPUT MAP-TROP PRESSURE                             
C                                                                       
      ICNT=3
C PRESSURE LINES: DW, NO DASH                                           
      DW1(1) = 2
      DW1(2) = 0
      DASH1(1)=0
      DASH1(2)=0
C TEMP LINES: REGULAR, DASH                                             
      DW2(1) = 0
      DW2(2) = 0
      DASH2(1) = 0
      DASH2(2) = 0
C VERTICAL WIND SHEAR: REGULAR, NO DASH                                 
      DASH3(1) = 7
      DASH3(2) = 4
      DW3(1) = 0
      DW3(2) = 0
C NO SHADING                                                            
      SHAD(1) = 0
      SHAD(2) = 0
      GO TO 1400
C                                                                       
C     ...STREAMS, ISOTACHS- WITH PLOTTED WINDS                          
C                                                                       
C     ...IOUTIN(2)=1                                                    
C     ...WHERE FLD1=STREAMS                                             
C     ...WHERE FLD2=ISOTACHS                                            
C                                                                       
 1140 CONTINUE
       print *,' AFTER 1140 continue 9999'
C                                                                       
C     ...FIND STREAM CENTERS                                            
C                                                                       
      ICEN1=1
      XLIM=-1.
      XS=-T1
      xs=-30.0
      M=1
      XIDID=-5.0
      YJDID=-37.0
      A1=111.0
      B1=60.0
      PUTHGT=11.0
      IHAVL(1)=1
C  ROTATE CENTER LABELS 90 DEGREES                                      
      IHAVL(2)=1
      IPRPUT=KPRIOR
      print *,' BEFORE 1140 CENTRE CALL 9999 '
c      call ct53(fld1)
      CALL CENTRE(FLD1,IMAX,JMAX,XS,A1,B1,M,XLIM,ICEN1,XLPLMI,IFLO,
     1        IFHI)
      print *,' after  1140 CENTRE CALL 9999 '
C                                                                       
c      stop 'CENTRE 1140'
C     ...FIND ISOTACH CONTOUR LABELS FROM CENTERS                       
C                                                                       
      M=3
      LOX=0
      IUP=4
      IRTKOR=-6
      IUPKOR=-20
      A1=-6.5
      B1=20.0
      IPRPUT=2
      PUTHGT=3.0
      CALL CLOSES(FLD2,IMAX,JMAX,S,A1,B1,M,IUP,YLPLMI,IFF,JSTPK,
     1        JSTPL,LOX,ITABMB,ITABFL,MXITR)
C                                                                       
C     ...FIND ISOTACH CONTOUR LABELS FROM STRIPS                        
C                                                                       
      LOX=0
      IFIX=2
      IUP=42
      M=3
      PUTHGT=3.0
      IPRPUT=0
      IRTKOR=-6
      IUPKOR=-20
ckumar      DO 1141 ILAB=1,6
      DO 1141 ILAB=1,12
      JFIX=ICFIX(ILAB)
c      call ct49 (fld2) 
c       call frame
c      do kj=1,118
c      do ki=1,49
c       fld2(kj,ki)=fld2(ki,kj)*1.94
c      end do
c      end do
      CALL CLOSET(FLD2,IMAX,JMAX,S,A1,B1,M,IUP,IFIX,JFIX,YLPLMI,IFF,LOX,
     1        ITABMB,ITABFL,MXITR)
c      do kj=1,118
c      do ki=1,49
c       fld2(kj,ki)=fld2(ki,kj)/1.94
c      end do
c      end do
 1141 CONTINUE
C                                                                       
C     ...SETUP FOR OUTPUT MAP-STREAMS/ISOTACHS/ WITH                    
C     ...PLOTTED WINDS                                                  
C                                                                       
      ICNT=2
      DW1(1)=2
      DW1(2)=0
C     ...FOR DOUBLE WT LINES                                            
      DW2(1)=0
      DW2(2)=0
      dw2(1)=2
      dw1(1)=0
cccccccccccccccccc      dw2(1)=2
C     ...TO MAKE REGULAR LINES                                          
      DASH1(1)=0
      DASH1(2)=0
C     ...NO DASHED LINES                                                
c      DASH2(1)=7
c      DASH2(2)=4
      DASH2(1)=20
      DASH2(2)=14
C     ...DASHED LINES                                                   
      SHAD(1)=0
      SHAD(2)=0
C     ...NO SHADING                                                     
      GO TO 1300
C                                                                       
C     ...OUTPUT MAP-FLD1                                                
C                                                                       
 1200 CONTINUE
c      CALL PDUMP(LABEL(1,1),LABEL(2,LABIX),0)
C     ...CLOSE OUT LABEL ARRAY AND WEF TAPE55                           
      CALL lPUTLAB(0,0,1.0,1HA,0.0,1,0,999)
c      CALL PUTLAB(0,0,1.0,1HA,0.0,1,0,999)
      ICK(3)=0
      IF(IT.EQ.NMAPS) ICK(3)=1
 1201 CONTINUE
        nflds=icnt
cc      CALL CNTR(IL,-1,ICK,JFID,ISCHED,INDEX,ICNT,FLD1,DASH1,DW1,SHAD)
c       call cntr(iret_cnt, IMAGE, IMAGSIZ_WRDS,
c     1            map,LABEL,ichk,ifid,isched,index,
c     1            nflds,
c     x      FLD1,DASH1,DW1,SHAD)
         print *,' from first cntr ',nflds
       rewind (55)
ckumar
       print*,'In Main before CNTR'
ckumar
       call cntr(iret_cnt, IMAGE, IMAGSIZ_WRDS,
     1           il,-1,ick,jfid,isched,index,
     1            nflds,
     x      FLD1,DASH1,DW1,SHAD,
     x      fld2,dash2,dw2,shad2,      ! added by kumar
     x      fld3,dash3,dw3,shad3,      ! added by kumar
     x      fld4,dash4,dw4,shad4)      ! added by kumar
ckumar
      IF(ICK(1)-2) 100,1202,706
 1202 IRTRY=IRTRY-1
      IF(IRTRY.EQ.0) GO TO 707
      GO TO 1201
C                                                                       
C     ...OUTPUT MAP-FLD1/FLD2/FLD3                                      
C                                                                       
 1400 CONTINUE
c      CALL PDUMP(LABEL(1,1),LABEL(2,LABIX),0)
C     ...CLOSE OUT LABEL ARRAY AND WEF TAPE55                           
c      CALL PUTLAB(0,0,1.0,1HA,0.0,1,0,999)
      CALL lPUTLAB(0,0,1.0,1HA,0.0,1,0,999)
      ICK(3)=0
      IF(IT.EQ.NMAPS) ICK(3)=1
 1301 CONTINUE
        nflds=icnt
       rewind (55)
       do kjj=1,50
       print 1077,(isched(jm,kjj),jm=1,8)
       end do
 1077  format(5z17)
cc      CALL CNTR(IL,-1,ICK,JFID,ISCHED,INDEX,ICNT,FLD1,DASH1,DW1,SHAD,
cc     1        FLD2,DASH2,DW2,SHAD,FLD3,DASH3,DW3,SHAD)
         print *,' from second  cntr ',nflds
       call lcntr(iret_cnt, IMAGE, IMAGSIZ_WRDS,
     1           il,-1,ick,jfid,isched,index,
     1            nflds,
     x      FLD1,DASH1,DW1,SHAD,
     x      FLD2,DASH2,DW2,SHAD,
     x      FLD3,DASH3,DW3,SHAD,
     x      fld4,dash4,dw4,shad)    
      IF(ICK(1)-2) 100,1302,706
 1302 IRTRY=IRTRY-1
      IF(IRTRY.EQ.0) GO TO 707
      GO TO 1301
C                                                                       
C     ...OUTPUT MAP-FLD1/FLD2                                           
C                                                                       
 1300 CONTINUE
c      CALL PDUMP(LABEL(1,1),LABEL(2,LABIX),0)
C     ...CLOSE OUT LABEL ARRAY AND WEF TAPE55                           
      CALL lPUTLAB(0,0,1.0,1HA,0.0,1,0,999)
c      CALL PUTLAB(0,0,1.0,1HA,0.0,1,0,999)
      ICK(3)=0
      IF(IT.EQ.NMAPS) ICK(3)=1
 1401 CONTINUE
        nflds=icnt
       rewind (55)
       do kjj=1,50
       print 1077,(isched(jm,kjj),jm=1,8)
       end do
      
cc      CALL CNTR(IL,-1,ICK,JFID,ISCHED,INDEX,ICNT,FLD1,DASH1,DW1,SHAD,
cc     1        FLD2,DASH2,DW2,SHAD)
         print *,' from third   cntr ',nflds
           print *,'FLD 1 ',(FLD1(45,jjj),jjj=1,45)
           print *,'FLD 2 ',(FLD2(45,jjj),jjj=1,45)
cj         call ct49(fld1)
c c         call frame
c         call ct49(fld2)
c         call frame
       call lcntr(iret_cnt, IMAGE, IMAGSIZ_WRDS,
     1           il,-1,ick,jfid,isched,index,
     1            nflds,
     x      FLD2,DASH2,DW1,SHAD,
     x      FLD1,DASH1,DW2,SHAD,
     x      FLD3,DASH3,DW3,SHAD,         ! added by kumar
     x      fld4,dash4,dw4,shad)

      IF(ICK(1)-2) 100,1402,706
 1402 IRTRY=IRTRY-1
      IF(IRTRY.EQ.0) GO TO 707
      GO TO 1401
  100 CONTINUE
C                                                                       
C     ...                                                               
C                                                                       
   96 CONTINUE
      CALL CONSOL('NWS,TROPICAL SURFACE ANALYSIS FAX ENDED:')
      PRINT 750,IMAPER,NMAPS
  750 FORMAT(1H1,I3,' OF ',I3,' MAPS COMPLETED')
c     gwv 2/25/97  END CNTR 6bit file PROPERLY!!
ckumar
      CALL ENDMAP (IFAXOUT,NRECFAX)
      CLOSE (IFAXOUT)
ckumar
c     end 2/25 insert
      CALL ENCODE(MESS4(4),12)
      WRITE(99,770)IMAPER,NMAPS
  770 FORMAT(I4,2X,2HOF,I4)
      CALL CONSOL(MESS4)
      CALL W3TAGE('GRAPH_TRPSFPRV') 
      STOP 0

  706 CONTINUE
      PRINT 761
  761 FORMAT('1BAD BACKGROUND INPUT  TAPE OR DISC ')
      CALL W3TAGE('GRAPH_TRPSFPRV') 
      STOP 761
  707 CONTINUE
      PRINT 765
  765 FORMAT('1BAD OUTPUT TAPE')
      CALL W3TAGE('GRAPH_TRPSFPRV') 
      STOP 765
      END

