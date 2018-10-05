      SUBROUTINE REDFLD(ISWTCH,NUMF,ILVLI,IFLDI,ITAUI,IARR2,IERR)
C     ...THIS SOUBROUTINE WILL FORM THE ID'S TO RETRIEVE THE PROPER     
C     ...FIELDS FROM MERC                                               
      COMMON/BOBINS/LMERC(256),IDMERC(1539)
       common /get3com/ctitlE
      Common/grbpds/kpds(25)
      COMMON/KPLOT/LABEL(2,1024),LABIX,NOBUF,IDRA(50)
      COMMON/PACKRA/IRAS(10)
      COMMON/PUTARG/PUTHGT,PUTANG,IPRPUT,ITAPUT
      COMMON/ADJ2/XIDID,YJDID
      COMMON/FIXFLD/XIM(51),F(51)
      COMMON/STRPOT/STFM(118,52)
      COMMON/YUCK/FLD1(48,119),FLD2(48,119),FLD3(48,119),
     &                                      fld4(48,119)
      COMMON/ADJ4/IRTCOR,IUPCOR
      COMMON/ISPACE/UC(118,51),VC(118,51),ZETA(118,52)
      COMMON/SHTST/  ISSH
C     ...WHICH MAKES SPACE OF 18172 WORDS = 2*6018 +6136L 500           
      DIMENSION IBUF1(2996)
      DIMENSION U(117,51),V(117,51)
      dimension vold(117,51)
c      DIMENSION STFMX(116,51)
      DIMENSION STFMX(117,51)
      DIMENSION STFMY(51,117)
      DIMENSION VSIDE(48,119)
      DIMENSION ILVLI(3),IFLDI(3),ITAUI(3)
      DIMENSION IINID(3,8)
      DIMENSION ILVL(2,9)
      DIMENSION IITAU(5)
      DIMENSION IRUNTA(3)
      DIMENSION ICONTA(3)
      DIMENSION IDCHK1(3,8)
      DIMENSION IDCHK(6)
      DIMENSION NEWS1(12)
      DIMENSION ITITLE(81)
      character*132 ctitle
      DIMENSION JTITL1(3),JTITL2(3),JTITL3(3),JTITL4(3),IARR2(3)
      DIMENSION IFM(5)
      DIMENSION KCFIX(5)
      INTEGER   KTL  (99)
      data ktl            /99*'    '/
      DIMENSION ITABMB(7),ITABFL(7)
      DIMENSION CNST(4),DNST(4),CNST1(4),CNST2(4),CNST3(4)
      DIMENSION KTBL(9),JTBL(9)
      EQUIVALENCE (NEWS1(1),IBUF1(1))
      EQUIVALENCE (U(1,1),UC(1,1))
      EQUIVALENCE (STFMX(1,1),UC(1,1))
      EQUIVALENCE (V(1,1),VC(1,1))
      EQUIVALENCE (STFMY(1,1),VC(1,1))
      EQUIVALENCE (IBUF1(1),ZETA(1,1))
      REAL*8 MERCI
      DATA    MXREC2/255/
      DATA    IMAX/48/
      DATA    JMAX/119/
      DATA    IRCSZ1/2996/
      DATA    IDOTS/30/
C                                                                       

C     ...FIELD IDENT CONTOUR/LABEL CONSTANTS                            
C                                                                       
C                                                                       
C                                                                       
C     1=STREAM(DUMMY)  2=TEMPERATURE  3=U  4=V  5=ISOTACH(DUMMY)        
C     6=TROP PRESSURE  7=TROP TEMPERATURE 8=TROP VERT WIND SHEAR        
C                                                                       
C                                                                       
C                                                                       
      DATA    IINID/
C     ...STREAM(DUMMY)                                                  
     1   3*0,
C     ...TEMPERATURE                                                    
     2   Z'01000800',Z'00C35082',53,
C     ...U                                                              
     3   Z'03000800',Z'00C35082',53,
C     ...V                                                              
     4   Z'03100800',Z'00C35082',53,
C     ...ISOTACH(DUMMY)                                                 
     5   3*0,
C     ...TROP PRESSURE                                                  
     6   Z'00808200',0,53,
C     ...TROP TEMPERATURE                                               
     7   Z'01008200',0,53,
C     ...TROP VERT WIND SHEAR                                           
     8   Z'03408200',0,53/
C                                                                       
C     ...LEVEL CONTROL TABLE                                            
C                                                                       
      DATA    ILVL/
     1   70000,Z'82',
     2   50000,Z'82',
     3   40000,Z'82',
     4   30000,Z'82',
     5   20000,Z'82',
     6   25000,Z'82',
     7   0,0,
     8   10000,Z'81',
     9   85000,Z'82'/
C                                                                       
C     ...TAU PERIOD CONTROL TABLE                                       
C                                                                       
      DATA    IITAU/00,24,30,36,48/
      dimension iutau(2,5)
      data iutau/12,22,13,23,14,24,15,25,16,26/
      DATA    IDCHK1/24*0/
      DATA    IDCHK/6*0/
      DATA    LBLTAP/55/
      DATA    MERCI/'TRPGRD  '/
      DATA    ITITLE/81*4H    /
      DATA    MASKFF/Z'FF'/
      DATA JTITL1/4HE 10,4H00MB,2HWI/
      DATA JTITL2/4HE 50,4H0MB ,2HWI/
      DATA JTITL3/4HE 25,4H0MB ,2HWI/
      DATA JTITL4/4HE 85,4H0MB ,2HWI/
      DATA    IFM/4H(A3,,4H1H$),3*0/
      DATA    XLPLMI/3HS#-/
      DATA    KCFIX/13,45,68,94,106/
      DATA    ITABMB/400,350,300,250,200,150,100/
      DATA    ITABFL/240,270,300,340,390,450,530/
      DATA    DEF/Z'7FFFFFFF'/
      DATA CNST/0.,1.,10.,0./
      DATA CNST1 /0.,50.,50.,0./
      DATA CNST2 /0.,1., 5.,0./
      DATA CNST3 /0.,1., 1.,0./
      DATA DNST/ 273.,5.,10.,0./
      DATA KTBL/-1,51,117,117,117,1,1,0,51/
      DATA JTBL/-1,48,119,119,119,1,1,0,48/
      DATA KRECT/1/
      DATA KCONTR/12/
      data lun,luni /15,25/
      dimension ibar(9)
      parameter (nlons=360,nlats=181)
      dimension  f3(nlons,nlats)
      data ibar/700,500,400,300,200,250,0,1000,850/
      dimension iin(8)
      data iin/35,11,33,34,32,39,52,136/
c     specify grib stream,T,U,V,dummy,W(pa/sec),RH,dummy
c     new grib variables
      dimension kkds(25,9),kgds(22)
      dimension kds(25)
      dimension jpds(25)
      equivalence(jpds,kds)
      character*4 ctextt 
       dimension krot_pri(2)
C                                                                       
c*************************  END OF SPECIFICATIONS  ***********
      
      do 9,k=1,9
      do 9 kk=1,25
  9   kkds(kk,k)=-1
C******************************************************************     
C 8/20/81 -- CHANGES: ADD 2650,56009,60399                              
C                     CHANGE 10750,10800,56010,60400                    
C 8/21/81 -- CHANGES: ADD 60301 TO 60305                                
C                     CHANGE 60000 FROM .2 TO 1.0                       
C******************************************************************     
C                                                                       
C     ...GENERATE IDENT TO OBTAIN OUTPUT FIELD REQUESTS                 
C                                                                       
c   control vbls  ITAUI,IFLDI,ILVLI 
c  control forecast time for each field, field number (type), and 
c  pressure surface of that field
c  itaui(1)=1,
c
c    ifldi(1)=3,ilvli(1)=8 
c transforms to fcsthour=itau(itaui(1))=18
c fieldnumber=iinid(ifldi(1),1:3) (an algorithm extracts it
c from column 1 and then extracts other ON84 info from columns 2
c and 3 (col.3 is map number) ))
c
c  ilvli(1)=8 passes the pressure level information
c  pressure level is in ilvl(ilvli(8),1) in pascals
c  the second word of this row is used for ON84 logic
c  and is not needed here.
C                                                                       
C     ...FIELD 1                                                        
C                                                                       
      ISSH = 0
c     gwv 2/26/97 reverse SH wind barb directio
      ISSH = 1
      ITAUA=ITAUI(1)
      IRUNTA(1)=IITAU(ITAUA)
c  irunta is the actual forecast time!!
c  store this in kds(14) for grib extraction
      IFLDA=IFLDI(1)
      ICONTA(1)=IFLDA
c   IFLDA defines the variable (u,v,t,etc.) we will extract
      LVL11=ILVLI(1)
c   logical pressure was  changed to bytes 2:3 of 
c   ILVL(LVL11,1) divided by 100
c     now change this to  ibar(lvl11)
      izz=ibar(lvl11)  
      print *,'9999 FROM TITLE lvl11,ibar(lvl11)',lvl11,ibar(lvl11)
c   this becomes KDS word 7
c     if izz is zero, set kds word 6 to integer seven
c   (defining tropopause rather than 100 isobaric sfc) 
      IDALL=LOR(SHFTL(ILVL(1,LVL11),8),ILVL(2,LVL11))
c   now get field type iin(iflda)
      ity=iin(iflda) 
       print *,'ity,iflda ,ifldi ', ity,iflda, 'ifldi array ',ifldi
c     This becomes kds word 5.                     
       kkds(5,1)=ity
       kkds(7,1)=izz
       if(izz .eq. 0) kkds(6,1)=7
       kkds(14,1)=irunta(1)
       if(irunta(1) .eq. 00) then
         lun=12
         luni=22
       endif 
       if(irunta(1) .eq. 48) then
         lun=15
         luni=25
       endif 
c      IDCHK1(1,1)=LOR(IINID(1,IFLDA),IRUNTA(1))
c      IDCHK1(2,1)=IDALL
c      IDCHK1(3,1)=IINID(3,IFLDA)
c      IDCHK1(1,4)=LOR(IINID(1,3),IRUNTA(1))
c      IDCHK1(2,4)=IDALL
c      IDCHK1(3,4)=IINID(3,3)
c      IDCHK1(1,5)=LOR(IINID(1,4),IRUNTA(1))
c      IDCHK1(2,5)=IDALL
c      IDCHK1(3,5)=IINID(3,4)
c      IDCHK1(1,6)=LOR(IINID(1,2),IRUNTA(1))
c      IDCHK1(2,6)=IDALL
c      IDCHK1(3,6)=IINID(3,2)
c      IDCHK1(1,7)=LOR(IINID(1,7),IRUNTA(1))
c      IDCHK1(2,7)=IINID(2,7)
c      IDCHK1(3,7)=IINID(3,7)
ccC                                                                       
C     ...FIELD 2 (IF NEEDED)                                            
C                                                                       
      LVL22=ILVLI(2)
c
      IF(LVL22.EQ.0) GO TO 27
      ITAUB=ITAUI(2)
      IRUNTA(2)=IITAU(ITAUB)
c
      IFLDB=IFLDI(2)
       print *,' IFLDB,IFLDI(2) ',ifldb,ifldi(2)
      ICONTA(2)=IFLDB
      IDALL=LOR(SHFTL(ILVL(1,LVL22),8),ILVL(2,LVL22))
      IDCHK1(1,2)=LOR(IINID(1,IFLDB),IRUNTA(2))
      IDCHK1(2,2)=IDALL
      IDCHK1(3,2)=IINID(3,IFLDB)
   27 CONTINUE
       izz=ibar(lvl22)
       ity=iin(ifldb) 
       kkds(7,2)=izz
       if(izz .eq. 0) kkds(6,2)=7
       kkds(14,2)=irunta(2)
       kkds(5,2)=ity
       
       print *,'ity,ifldb ,ifldi ', ity,ifldb, 'ifldi array ',ifldi
C                                                                       
C     ...FIELD 3 (IF NEEDED)                                            
      LVL33=ILVLI(3)
      IF(LVL33.EQ.0) GO TO 28
      ITAUC=ITAUI(3)
      IRUNTA(3)=IITAU(ITAUC)
      IFLDC=IFLDI(3)
      ICONTA(3)=IFLDC
      IDALL=LOR(SHFTL(ILVL(1,LVL33),8),ILVL(2,LVL33))
      IDCHK1(1,3)=LOR(IINID(1,IFLDC),IRUNTA(3))
      IDCHK1(2,3)=IDALL
      IDCHK1(3,3)=IINID(3,IFLDC)
      IDCHK1(1,8) = IDCHK1(1,3)
      IDCHK1(2,8) = 0
      IDCHK1(3,8) = IINID(3,IFLDC)
   28 CONTINUE
       izz=ibar(lvl33)
       ity=iin(ifldc)
       kkds(7,3)=izz
       if(izz .eq. 0) kkds(6,3)=7
       kkds(14,3)=irunta(3)
       kkds(5,3)=ity
       print *,'ity,ifldc ,ifldi ', ity,ifldc, 'ifldi array ',ifldi
       print *, '9999 kkds ',kkds(5,1),kkds(5,2),kkds(5,3)
C                                                                       
C******************************************************************     
C IF THIS MAP IS A TROP CHART, SET NUMF=1 TO PASS THRU LOOP ONCE        
C AND AVOID INCORRECT ISOTACH AND ISOTHERM SEGMENTS AT 200 AND 300.     
C******************************************************************     
      IF (LVL11 .EQ. 7) NUMF = 1
      DO 1000 ISET=1,NUMF
      GO TO(100,200,300),ISET
C                                                                       
C     ...ID ERROR MESSAGES FROM W3FK43                                  
C     ...PRINT MESSAGE AND SKIP THIS MAP                                
C                                                                       
C                                                                       
C                                                                       
C     ...PROCESS INPUT DATA FOR WIND PLOT(EXCEPT FOR TROP MAP)          
C     ...PROCESS INPUT DATA FOR 1ST FIELD FOR CONTOURING STREAM FIELD   
C     ...EXCEPT FOR TROP LEVEL WHERE WE CONTOUR TROP PRESSURE.          
C                                                                       
C                                                                       
C                                                                       
  100 CONTINUE
C                                                                       
C     ...INITIALIZE LABEL ARRAY                                         
C                                                                       
      REWIND LBLTAP
      DO 130 L=1,1024
      LABEL(1,L)=0
      LABEL(2,L)=0
  130 CONTINUE
      LABIX=0
      NOBUF=0
C
c gwv inserted initialization code
C       INITIALIZE LABEL ARRAY
C
          LABIX = 0
          NOBUF = 0
cC
          IPT = 1
          JPT = 0
          HEIGHT = 1.0
          ANGLE = 0.0
          KROT_PRI(1) = 0
          KROT_PRI(2) = 0
          CTEXTt(1:1) = '?'
          CTEXTt(2:2) = CHAR(1)
          CTEXTt(3:3) = '$'
          CTEXTt(4:4) = CHAR(0)
          NCHAR = 2
          ICMD = -2
          CALL PUTLAB(IPT,JPT,HEIGHT,CTEXTt,ANGLE,NCHAR,KROT_PRI,
     X                ICMD)

C                                                                       
C     ...PREPARE FOR PLOT OF WINDS (AVERAGE WHEN NEEDED)                
C                                                                       
      IZSET=1
      IF(LVL11.EQ.8) IZSET=1
      IF(LVL11.EQ.2) IZSET=2
      IF(LVL11.EQ.6) IZSET=3
      IF(LVL11.EQ.7) IZSET=4
      IF(LVL11.EQ.9) IZSET=5
      kds(7)=ibar(lvl11)
       print *,' 9999 set kds(7) ' , kds(7)
      CALL RDOBVT(KDS(7),IERROR)
      GO TO (140,150,160,170,1800),IZSET
C                                                                       
C     ...GET 1000 MB U FIELD IN ARRAY U                                 
  140 CONTINUE
      IARR2(1)=JTITL1(1)
      IARR2(2)=JTITL1(2)
      IARR2(3)=JTITL1(3)
      IDCHK(1)=IDCHK1(1,4)
      IDCHK(2)=IDCHK1(2,4)
      IDCHK(5)=IDCHK1(3,4)
      PRINT 51, (IDCHK(I),I=1,6)
   51 FORMAT('0MAP IDENTS REQUESTED ARE  ',6(2X,Z8))
c
c      CALL W3FK43(MERCI,IDMERC,IDCHK,IBUF1,MXREC2,IRCSZ1,IERR)
c  replacement to w3fk43 follows
      do 311,n=1,25
 311  kds(n)=-1
      kds(7)=ibar(lvl11)
                print *,' 9999 set kds(7) ' , kds(7)
     1 ,lvl11,ibar(lvl11)
c special for u
      kds(5)=33
      kds(6)=100
c            kds(7)=1000
      if(lvl11 .eq. 8) kds(7)=1000
       print *,' 9999 kds7 in 1000 goto ',kds(7)
      kds(14)=irunta(1)
C GETGB
      call get3(lun,luni,jpds,f3,ctitle)
      call i3to53(f3,u)
      IF(IERR.NE.0) GO TO 180
c      CALL W3FP02(NEWS1,ITITLE,1)
      PRINT 120,(ITITLE(I),I=1,22)
  120 FORMAT(1H0,10X,'INPUT FIELD FOUND = ',22A4)
c      CALL W3AI01(IBUF1,U,NEWS1)
C     ...SET UP DATE FOR TITLE                                          
C                                                                       
      print 1977,jpds
 1977 format(4z20)
c      stop 'jpds'
c       IRAS(1)=LAND(NEWS1(1),MASKFF)
c      IRAS(7)=LAND(NEWS1(7),MASKFF)
c      IRAS(8)=LAND(SHFTR(NEWS1(7),8),MASKFF)
c      IRAS(9)=LAND(SHFTR(NEWS1(7),16),MASKFF)
c      IRAS(10)=SHFTR(NEWS1(7),24)
c      iras(7)=kpds(11)
c      iras(8)=kpds(10)
c      iras(9)=kpds(9)
c      iras(10)=kpds(8)
c      iras(1)=24
      CALL UPDATR
      PRINT 125, IRAS(1),(IRAS(I),I=7,10)
  125 FORMAT('0TAU INCREMENT=  ',I3,'  BASED ON IHOUR=  ',I3,
     1   '  IDAY=  ',I3,'  IMONTH=  ',I3,'  IYR=  ',I5)
C                                                                       
C     ...GET 1000 MB V FIELD IN ARRAY V                                 
C                                                                       
      IDCHK(1)=IDCHK1(1,5)
      IDCHK(2)=IDCHK1(2,5)
      IDCHK(5)=IDCHK1(3,5)
      PRINT 51, (IDCHK(I),I=1,6)
c special for v
      do 313,n=1,25
 313  kds(n)=-1
      kds(7)=ibar(lvl11)
      kds(5)=34
      kds(6)=100
c           kds(7)=1000
      if(lvl11 .eq. 8) kds(7)=1000
      kds(14)=irunta(1)
C GETGB
c      CALL W3FK43(MERCI,IDMERC,IDCHK,IBUF1,MXREC2,IRCSZ1,IERR)
      call get3(lun,luni,jpds,f3,ctitle)
      call i3to53(f3,v)
      call i3to53(f3,vold)
      IF(IERR.NE.0) GO TO 180
c      CALL W3AI01(IBUF1,V,NEWS1)
      GO TO 190
C                                                                       
C     ...GET 500 MB U FIELD IN ARRAY U                                  
C                                                                       
  150 CONTINUE
      IARR2(1)=JTITL2(1)
      IARR2(2)=JTITL2(2)
      IARR2(3)=JTITL2(3)
      IDCHK(1)=IDCHK1(1,4)
      IDCHK(2)=IDCHK1(2,4)
      IDCHK(5)=IDCHK1(3,4)
      PRINT 51, (IDCHK(I),I=1,6)
      do n=1,25
      kds(n)=-1
      end do
      kds(5)=33
      kds(6)=100
      kds(7)=500
      kds(14)=irunta(1)
C GETGB
c      CALL W3FK43(MERCI,IDMERC,IDCHK,IBUF1,MXREC2,IRCSZ1,IERR)
      call get3(lun,luni,jpds,f3,ctitle)
      call i3to53(f3,u)
      IF(IERR.NE.0) GO TO 180
      CALL W3FP02(NEWS1,ITITLE,1)
      PRINT 120,(ITITLE(I),I=1,22)
c      CALL W3AI01(IBUF1,U,NEWS1)
C     ...SET UP DATE FOR TITLE                                          
C                                                                       
c      IRAS(1)=LAND(NEWS1(1),MASKFF)
c      IRAS(7)=LAND(NEWS1(7),MASKFF)
c      IRAS(8)=LAND(SHFTR(NEWS1(7),8),MASKFF)
c      IRAS(9)=LAND(SHFTR(NEWS1(7),16),MASKFF)
c      IRAS(10)=SHFTR(NEWS1(7),24)
c      CALL UPDATR
      PRINT 125, IRAS(1),(IRAS(I),I=7,10)
C                                                                       
C     ...GET 500 MB V FIELD IN ARRAY V                                  
C                                                                       
      IDCHK(1)=IDCHK1(1,5)
      IDCHK(2)=IDCHK1(2,5)
      IDCHK(5)=IDCHK1(3,5)
      PRINT 51, (IDCHK(I),I=1,6)
      do n=1,25
      kds(n)=-1
      end do
      kds(5)=34
      kds(6)=100
      kds(7)=500
      kds(14)=irunta(1)
C GETGB
c      CALL W3FK43(MERCI,IDMERC,IDCHK,IBUF1,MXREC2,IRCSZ1,IERR)
      call get3(lun,luni,jpds,f3,ctitle)
      call i3to53(f3,v)
      call i3to53(f3,vold)
      IF(IERR.NE.0) GO TO 180
c      CALL W3AI01(IBUF1,V,NEWS1)
      GO TO 190
C                                                                       
C     ...GET 250 MB U FIELD IN ARRAY U                                  
C                                                                       
  160 CONTINUE
      IARR2(1)=JTITL3(1)
      IARR2(2)=JTITL3(2)
      IARR2(3)=JTITL3(3)
      IDCHK(1)=IDCHK1(1,4)
      IDCHK(2)=IDCHK1(2,4)
      IDCHK(5)=IDCHK1(3,4)
      PRINT 51, (IDCHK(I),I=1,6)
      do n=1,25
      kds(n)=-1
      end do
      kds(5)=33
      kds(6)=100
      kds(7)=250
      kds(14)=irunta(1)
C GETGB
c     CALL W3FK43(MERCI,IDMERC,IDCHK,IBUF1,MXREC2,IRCSZ1,IERR)
      call get3(lun,luni,jpds,f3,ctitle)
      call i3to53(f3,u)
      IF(IERR.NE.0) GO TO 180
      CALL W3FP02(NEWS1,ITITLE,1)
      PRINT 120,(ITITLE(I),I=1,22)
c      CALL W3AI01(IBUF1,U,NEWS1)
C     ...SET UP DATE FOR TITLE                                          
C                                                                       
c      IRAS(1)=LAND(NEWS1(1),MASKFF)
c      IRAS(7)=LAND(NEWS1(7),MASKFF)
C      IRAS(8)=LAND(SHFTR(NEWS1(7),8),MASKFF)
c      IRAS(9)=LAND(SHFTR(NEWS1(7),16),MASKFF)
c      IRAS(10)=SHFTR(NEWS1(7),24)
c      CALL UPDATR
      PRINT 125, IRAS(1),(IRAS(I),I=7,10)
C                                                                       
C     ...GET 250 V FIELD IN ARRAY V                                     
C                                                                       
      IDCHK(1)=IDCHK1(1,5)
      IDCHK(2)=IDCHK1(2,5)
      IDCHK(5)=IDCHK1(3,5)
      PRINT 51, (IDCHK(I),I=1,6)
      do n=1,25
      kds(n)=-1
      end do
      kds(5)=34
      kds(6)=100
      kds(7)=250
      kds(14)=irunta(1)
C GETGB
c      CALL W3FK43(MERCI,IDMERC,IDCHK,IBUF1,MXREC2,IRCSZ1,IERR)
      call get3(lun,luni,jpds,f3,ctitle)
      call i3to53(f3,v)
      call i3to53(f3,vold)
      IF(IERR.NE.0) GO TO 180
c      CALL W3AI01(IBUF1,V,NEWS1)
      GO TO 190
C                                                                       
C     ...GET 850 MB U FIELD IN ARRAY U                                  
 1800 CONTINUE
      IARR2(1)=JTITL4(1)
      IARR2(2)=JTITL4(2)
      IARR2(3)=JTITL4(3)
      IDCHK(1)=IDCHK1(1,4)
      IDCHK(2)=IDCHK1(2,4)
      IDCHK(5)=IDCHK1(3,4)
      PRINT 51, (IDCHK(I),I=1,6)
      do n=1,25
      kds(n)=-1
      end do
      kds(5)=33
      kds(6)=100
      kds(7)=850
      kds(14)=irunta(1)
C GETGB
c      CALL W3FK43(MERCI,IDMERC,IDCHK,IBUF1,MXREC2,IRCSZ1,IERR)
      call get3(lun,luni,jpds,f3,ctitle)
      call i3to53(f3,u)
      IF(IERR.NE.0) GO TO 180
      CALL W3FP02(NEWS1,ITITLE,1)
      PRINT 120,(ITITLE(I),I=1,22)
c      CALL W3AI01(IBUF1,U,NEWS1)
C     ...SET UP DATE FOR TITLE                                          
C                                                                       
c      IRAS(1)=LAND(NEWS1(1),MASKFF)
c      IRAS(7)=LAND(NEWS1(7),MASKFF)
c      IRAS(8)=LAND(SHFTR(NEWS1(7),8),MASKFF)
c      IRAS(9)=LAND(SHFTR(NEWS1(7),16),MASKFF)
c      IRAS(10)=SHFTR(NEWS1(7),24)
c      CALL UPDATR
      PRINT 125, IRAS(1),(IRAS(I),I=7,10)
C                                                                       
C     ...GET 850 MB V FIELD IN ARRAY V                                  
C                                                                       
      IDCHK(1)=IDCHK1(1,5)
      IDCHK(2)=IDCHK1(2,5)
      IDCHK(5)=IDCHK1(3,5)
      PRINT 51, (IDCHK(I),I=1,6)
      do n=1,25
      kds(n)=-1
      end do
      kds(5)=34
      kds(6)=100
      kds(7)=850
      kds(14)=irunta(1)
C GETGB
c      CALL W3FK43(MERCI,IDMERC,IDCHK,IBUF1,MXREC2,IRCSZ1,IERR)
      call get3(lun,luni,jpds,f3,ctitle)
      call i3to53(f3,v)
      call i3to53(f3,vold)
      IF(IERR.NE.0) GO TO 180
c      CALL W3AI01(IBUF1,V,NEWS1)
      GO TO 190
C                                                                       
C     ...TROP LEVEL PLOTS NO WINDS -- SO GET OUT                        
C                                                                       
  170 CONTINUE
      GO TO 190
  180 CONTINUE
      PRINT 181
  181 FORMAT('0ID ERROR W3FK43 FOR U/V FIELD-EXIT')
      RETURN
  190 CONTINUE
      iF(IZSET.NE.4) GO TO 195
C                                                                       
C     ...GET TROPOPAUSE PRESSURE FIELD IN ARRAY V                       
      IDCHK(1)=IDCHK1(1,1)
      IDCHK(2)=IDCHK1(2,1)
      IDCHK(5)=IDCHK1(3,1)
      PRINT 51, (IDCHK(I),I=1,6)
      do n=1,25
      kds(n)=-1
      end do
      kds(5)=01
      kds(6)=7
c      kds(7)=
      kds(14)=irunta(1)
C GETGB
c      CALL W3FK43(MERCI,IDMERC,IDCHK,IBUF1,MXREC2,IRCSZ1,IERR)
      call get3(lun,luni,jpds,f3,ctitle)
      call i3to53(f3,v)
      IF(IERR.EQ.0) GO TO 145
      PRINT 146
  146 FORMAT('0ID ERROR W3FK43 FOR TROP PRES FIELD-EXIT')
      RETURN
  145 CONTINUE
C      CALL W3AI01(IBUF1,V,NEWS1)
C                                                                       
c      IRAS(1)=LAND(NEWS1(1),MASKFF)
c      IRAS(7)=LAND(NEWS1(7),MASKFF)
c      IRAS(8)=LAND(SHFTR(NEWS1(7),8),MASKFF)
C      IRAS(9)=LAND(SHFTR(NEWS1(7),16),MASKFF)
c     IRAS(10)=SHFTR(NEWS1(7),24)
c      CALL UPDATR
      PRINT 125, IRAS(1),(IRAS(I),I=7,10)
      UM = .02
      UA =0.0
      B1=50.0
      A1=0.0
      JFLD=1
      CALL UVCLIP(U,V,VSIDE,FLD1,IMAX,JMAX,UM,UA,JFLD)
C     ...TROP PRESS NOW IN FLD1                                         
C                                                                       
C     ...GET TROPOPAUSE TEMPERATURE FIELD IN ARRAY U.                   
C                                                                       
      IDCHK(1)=IDCHK1(1,7)
      IDCHK(2)=IDCHK1(2,7)
      IDCHK(5)=IDCHK1(3,7)
      PRINT 51, (IDCHK(I),I=1,6)
      do n=1,25
      kds(n)=-1
      end do
      kds(5)=11
      kds(6)=7
c      kds(7)=
      kds(14)=irunta(1)
C GETGB
c      CALL W3FK43(MERCI,IDMERC,IDCHK,IBUF1,MXREC2,IRCSZ1,IERR)
      call get3(lun,luni,jpds,f3,ctitle)
      call i3to53(f3,u)
      IF(IERR.EQ.0) GO TO 148
      PRINT 147
  147 FORMAT('0ID ERROR W3FK43 FOR TROP TEMP FIELD-EXIR')
      RETURN
  148 CONTINUE
c      CALL W3AI01(IBUF1,U,NEWS1)
      UM=1.0
      UA=-273.
      JFLD=1
      CALL UVCLIP(V,U,VSIDE,FLD2,IMAX,JMAX,UM,UA,JFLD)
C    ...TROP TEMP NOW IN FLD2                                           
C                                                                       
C******************************************************************     
C GET TROPOPAUSE VERTICAL WIND SHEAR FIELD IN ARRAY U . . .             
C******************************************************************     
      IDCHK(1) = IDCHK1(1,8)
      IDCHK(2) = 0
      IDCHK(5) = IDCHK1(3,8)
      PRINT 51,(IDCHK(I),I=1,6)
      do n=1,25
      kds(n)=-1
      end do
      kds(5)=45
c     WARNING, THIS IS JUST U SHEAR
      kds(6)=7
c      kds(7)=
      kds(14)=irunta(1)
C GETGB
c      CALL W3FK43(MERCI,IDMERC,IDCHK,IBUF1,MXREC2,IRCSZ1,IERR)
      call get3(lun,luni,jpds,f3,ctitle)
      call i3to53(f3,u)
      IF (IERR .EQ. 0) GO TO 707
      PRINT 708
  708 FORMAT('0ID ERROR W3FK43 FOR TROP VWS FIELD -- EXIT')
      RETURN
C                                                                       
  707 CONTINUE
c      CALL W3AI01(IBUF1,U,NEWS1)
      UM = 592.
      UA = 0.
      JFLD = 1
      CALL UVCLIP(V,U,VSIDE,FLD3,IMAX,JMAX,UM,UA,JFLD)
C                                                                       
C . . . TROP VWS NOW IN FLD3 . . .                                      
C                                                                       
C                                                                       
C     ...FIND TROPPAUSE TEMPERATURE VALUES AND                          
C     ...PLACE ON TROPOPAUSE PRESSURE CONTOUR STRIPS                    
C                                                                       
C                                                                       
      DO 1430 IGOR=1,IMAX
      DO 1430 JGOR=1,JMAX
      INTEG=FLD2(IGOR,JGOR)+0.5
      IF(INTEG.EQ.99) FLD1(IGOR,JGOR)=DEF
 1430 CONTINUE
      PUTHGT=1.0
      PUTANG=0.0
      IPRPUT=1
      ITAPUT=0
      M=3
      IFIX=2
      IUP=42
      LOX=0
      A2=0.0
      B2= 1.0
      S=.5
      IRTCOR=0
      IUPCOR=-50
      MXITR=7
      DO 149 ILAB=1,5
      JFIX=KCFIX(ILAB)
      CALL CLOSEF(FLD1,FLD2,IMAX,JMAX,S,A2,B2,M,IFIX,JFIX,XLPLMI,IFM,
     1   IUP,LOX,ITABMB,ITABFL,MXITR)
  149 CONTINUE
      DO 142  I=1,IMAX
         DO 141  J=1,JMAX
            FLD2(I,J) = FLD2(I,J) * .2
  141    CONTINUE
  142 CONTINUE
      GO TO 800
C                                                                       
C     ...PROCESS INPUT DATA FOR 1ST FIELD FOR CONTOURING-STREAMS        
C                                                                       
  195 CONTINUE
C     ...BUT FIRST WE MUST SCALE,TURN SIDEWAYS,AND CLIP U,V&PLOT WINDS  
      UM=1.9424
      UA=0.0
      JFLD=2
      CALL UVCLIP(U,V,FLD1,VSIDE,IMAX,JMAX,UM,UA,JFLD)
C     ...NOW U SIDEWAYS IS IN FLD1(48,119)                              
C     ...AND V SIDEWAYS IS IN VSIDE(48,119)                             
C     ...IBUF1 STILL HAS PACKED V                                       
C     ...ORIGINAL U FIELD IS STILL INTACT                               
C                                                                       
C     ...NOW PLOT WINDS                                                 
      CALL WNDPLO(FLD1,VSIDE,IMAX,JMAX)
c    isotac call moved up here from position
c    30 lines down since fld1 is clobbered
c    by streamfunction .. not good 
c       CALL ISOTAC(FLD1,VSIDE,48,119,FLD2)
      CALL ISOTAC(FLD1,VSIDE,48,119,FLD2)
      DO 1750 J=1,119
      DO 1750 I=1,48
      FLD2(I,J)=FLD2(I,J)*.05+6.5
 1750 CONTINUE

C                                                                       
C     ...TO RESTORE V                                                   
c      CALL W3AI01(IBUF1,V,NEWS1)
c      to restore v
       do 389,k=1,51
       do 389 j=1,117
 389   v(j,k)=vold(j,k)
c
      DO 390 J=1,51
      DO 390 I=1,117
      V(I,J)=V(I,J)*1.9424
  390 CONTINUE
C                                                                       
C     ...FORM STREAMS                                                   
        print 1999,kds
 1999   format(5z20)
c      CALL STR(ZETA,U,UC,V,VC)
c   obtain 360x181 streamfunction on f3
c   from u and v fields in lun
       call stream3(kds(7),kds(14),lun,luni,f3)
c
c  streamfunction is now M**2/SEC**2.  We need to convert to
c Z.  The conversion factor is just f/g.  The apparent
c  assumption from old stremafunction source is that f
c  is held constant at its 45 degree value
c
        pi=3.1415927
        sin45=sqrt(2.)/2
        omega=2*pi/86400.
        g=9.81
        fcoril=2*omega*sin45
        factor=fcoril/g
c  downstream  code has cgs logic and expects
c  centimeters.  adjust factor
        factor=factor*100
c    divide by another two if level is 300 mbars or higher
       if(kds(7) .lt. 301) factor=factor/2.
        do j=1,nlats
        do k=1,nlons
           f3(k,j)=f3(k,j)   * factor
        end do
        end do
c END CGS CONVERSION
c      interpolate to type 53 mercator grid
       call i3to53(f3,stfmx)
c draw  equator bulls eye for location confirmation.
ccc       stfmx(90,26)=50000.
C                                                                       
C     ...STFMX CONTAINS THE RESULTING STREAM FUNCTION FIELD             
c      CALL FLIPTR(STFMX,STFMY,FLD1)
      CALL FLIPTR(STFMX,STFMY,FLD1)
       if(kds(7)  .eq. 1000) go to 800
c      IF(IZSET.EQ.1) GO TO 800
C     ...CUT CNTR INTERVAL IN HALF FOR EVERY LEVEL EXCEPT 1000 MB       

      print *,'9999 cntr cutter at ', kds(7)
      DO 165 J=1,119
      DO 165 I=1,48
      FLD1(I,J)=FLD1(I,J)*0.5
  165 CONTINUE
      GO TO 800
C                                                                       
C     ...PROCESS INPUT DATA FOR 2ND FIELD FOR CONTOURING-ISOTACH        
C                                                                       
  200 CONTINUE
c      CALL ISOTAC(FLD1,VSIDE,48,119,FLD2)
c      DO 175 J=1,119
c      DO 175 I=1,48
c      FLD2(I,J)=FLD2(I,J)*.05+6.5
c  175 CONTINUE
      GO TO 800
C                                                                       
C    ...PROCESS INPUT DATA FOR 3RD FIELD CONTOURING-ISOTHERMS           
C                                                                       
  300 CONTINUE
      IDCHK(1)=IDCHK1(1,3)
      IDCHK(2)=IDCHK1(2,3)
      IDCHK(5)=IDCHK1(3,3)
      PRINT 51, (IDCHK(I),I=1,6)
       izz=ibar(lvl33)
       ity=iin(ifldc)
       kds(7)=izz
       if(izz .eq. 0) kds(6)=7
       kds(14)=irunta(3)
       kds(5)=ity
c GETGB
c      CALL W3FK43(MERCI,IDMERC,IDCHK,IBUF1,MXREC2,IRCSZ1,IERR)
      IF(IERR.EQ.0) GO TO 310
      PRINT 312, IERR
  312 FORMAT('0ID ERROR W3FK43 FOR TMPS--IERR = ',I4)
      RETURN
  310 CONTINUE
      CALL W3AI01(IBUF1,U,NEWS1)
      UM=1.0
      UA=0.0
      JFLD=1
      CALL UVCLIP(V,U,VSIDE,FLD3,IMAX,JMAX,UM,UA,JFLD)
      DO 750  IT=1,48
         DO 749  JT=1,119
            FLD3(IT,JT) = (FLD3(IT,JT) - 273.)/5.
  749     CONTINUE
  750 CONTINUE
C     ...ISOTHERMS NOW IN FLD3                                          
  800 CONTINUE
 1000 CONTINUE
      RETURN
      END
