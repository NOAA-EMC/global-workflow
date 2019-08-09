       SUBROUTINE TITLES(ISLOTA,ISLOTB,IPANA,IOUTIN,ILVLI,IARR2,IRLAB,
     1   INCR,INCR1,MLAST,ITAUI,IVAR,JLAST,INOPN1,INOPN2,IREM3A,
     2   IREM3B,IREM1A,IREM1B,
     1 ifld1a,ifld1b)
C                                                                       00268900
C     ...THIS SUBROUTINE PREPARES VARIAN/FAX MAP TITLES                 00269000
C                                                                       00269100
      COMMON/PACKRA/IRAS(10)
       common /get3com/ctitleg
       character*20 cgulf(3)
       Character*132 ctitleg
       dimension kprior(2)
      COMMON/KPLOT/LABEL(2,1024),LABIX,NOBUF,IDRA(50)
      COMMON/CRUNCH/IBCD2(40)
      character*4  cbcd2(40)
      character*160 cbcd4 
      equivalence (cBCD2,cbcd4)
      character*8 cbcd22(25)
      equivalence (cbcd22,ibcd2)
      COMMON/WEFAX/ IAPT
      DIMENSION IISUB(11)
      DIMENSION IBCD(25),IBCD1(40)
      character*8 cbcd1(40),cbcd(25)
      equivalence (cbcd,ibcd),(cbcd1,ibcd1)
      DIMENSION ILVL(4,9)
      DIMENSION ITITLE(8,7)
      character*8 CTITLE(8,7),clvl(4,9),cisub(11)  
      character*8 ctitl1(3),ctitl2(3),ctitl3(3),ctitl4(3),
     1 ctitl5(3),ctitl6(3),ctitl7(3)
      DIMENSION IMONT(12)
      DIMENSION ITITL1(3),ITITL2(3),ITITL3(3),ITITL4(3)
      DIMENSION ITITL5(3),ITITL6(3),ITITL7(3)
      DIMENSION IARR1(3),IARR2(3)
      DIMENSION IOUTIN(2),ILVLI(3),ITAUI(3)
      DIMENSION ITITSM(4)
      character*8 ctitsm(4)
      DIMENSION IHOUR(4)
      DIMENSION ILEV(2,4)
      INTEGER   ISCHED(10)
      character*8 cdayw
c     +/4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,3*4H    ,00271100
c data above was cut from data below.  Appears to be MVS logic error
      INTEGER   IDESC(11,5)
      character*8 cdesc(11,5)
      DATA IDESC
     +/4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,3*4H    ,
     + 4H    ,4HSTRE,4HAMS ,4H/ IS,4HOTAC,4HHS /,4H TEM,4HPS  ,3*4H    ,
     + 4H    ,4HSTRE,4HAMS ,4H    ,4H    ,4H    ,4H    ,4H    ,3*4H    ,
     + 4H    ,4HPRES,4HSURE,4H / T,4HEMPS,4H / V,4HWS  ,4H    ,3*4H    ,
     + 4H    ,4HPRES,4HSURE,4H / V,4HORTI,4HCITY,4HW   ,4H    ,3*4H    /
      DATA    IISUB/1H ,2HP1,2HP2,2HP3,2HP4,2HP5,2HP6,2HI1,2HI2,2HI3,
     1   2HB2/
      DATA    ILVL/4H700 ,1,4H   7,4H00MB,4H500 ,2,4H   5,4H00MB,
     1             4H400 ,3,4H   4,4H00MB,4H300 ,4,4H   3,4H00MB,
     2             4H200 ,5,4H   2,4H00MB,4H250 ,6,4H   2,4H50MB,
     1             4HTROP,7,4HTROP,4H-VWS,4H1000,8,4H  10,4H00MB,
     1             4H850 ,9,4H   8,4H50MB/
C     DATA    ITITLE/2*4H    ,4H   S,4HTREA,4HMS/I,4HSOTA,4HCHS ,4H    ,00272400
C    1   4H 200,4HMB S,4HTREA,4HMS/W,4HINDS,4H/TRO,4HP PR,4HES  /       00272500
      DATA ITITLE
     1     /4H    ,4H    ,4H   S,4HTREA,4HMS/I,4HSOTA,4HCHS ,4H    ,
     2      4H 200,4HMB S,4HTREA,4HMS/W,4HINDS,4H/TRO,4HP PR,4HES  ,
     3      4H    ,4HMB S,4HTREA,4HMS/I,4HSOTA,4HCHS/,4HTEMP,4HS   ,
     4      4H    ,4HMB S,4HTREA,4HMS  ,4H    ,4H    ,4H    ,4H    ,
     5      4H    ,4H PRE,4HSSUR,4HE/TE,4HMPS/,4HVWS ,4H    ,4H    ,
     6      4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,
     7      4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
      DATA    IMONT/4H JAN,4H FEB,4H MAR,4H APR,4H MAY,4H JUN,
     1        4H JUL,4H AUG,4H SEP,4H OCT,4H NOV,4H DEC/
      DATA    ITITL1/4H24HR,4H PRO,2HG /
      DATA    ITITL2/4HV.T.,4H    ,2H  /
      DATA    ITITL3/4H  BA,4HSED ,2HON/
      DATA    ITITL4/4HNWS-,4HNMC ,2HWA/
      DATA    ITITL5/4HSHIN,4HGTON,2H. /
      DATA    ITITL6/4H    ,4HTROP,2H. /
      DATA    ITITL7/4H   2,4H00MB,2H. /
      DATA    IGIANT/15/
      DATA    IREGU/10/
      DATA    IARR1/4HARRO,4HWS G,2HIV/
      DATA    IARR3/4HNDS./
c      DATA    ICIRC/Z'4CE05B00'/
      DATA     ICIRC / 4H($   /
      DATA    IBOX/Z'7D4F5B00'/
      DATA    IBCHK/4H    /
      DATA    MAPT7/Z'5600000000000000'/
c      DATA    MAPT7/Z'E5000000'/
      DATA    MASK8/Z'FF00000000000000'/
      DATA    MAPT13/Z'4000000000000000'/
      DATA    ITITSM/4*8H        /
c      DATA    ITITSM/4*4H    /
      DATA    IHOUR/4H18HR,4H24HR,4H30HR,4H36HR/
      DATA    ILEV/4H 700,4H600 ,4H 500,4H400 ,4H 300,4H250 ,4H 200,
     1   4H200 /
      character*8 cblank
      integer iblank
      equivalence ( cblank,iblank )
      data cblank/'        ' /
      do 5,k=1,20
      iax=(k-1)*8+1
 5    cbcd4(iax:iax+7)=cblank
      continue
      print *, ' ENTERING TITLE 9999'
      print 1966,(iras(k),k=3,10)
 1966  format(4i8,' <adv 9999 curr >',4i8)
      do 10,k=1,25
 10   ibcd(k)=iblank
       ipany = 0
      call byteswap(ISLOTA, 8, 1)
      IVAR=LAND(ISLOTA,MASK8)
       print *,' 9999 TITLES ipana,ipany,ivar',ipana,ipany,ivar
       print 955,'9999 spec titles ipana ipany ivar',ipana,
     1 ipany,ivar,islota,IVAR
       call byteswap(ISLOTA, 8, 1)
 955   format(a50,1x,'ipana >',a12,1x,' pan >',i2, 1x,' ivar >',a9,2z20)
      IF((IVAR.EQ.MAPT7).OR.(IVAR.EQ.MAPT13)) GO TO 1350
c go to end of routine (1425) if next ifs are true
      IF(IPANA.EQ.IISUB(3)) GO TO 1425
      IF(IPANA.EQ.IISUB(5)) GO TO 1425
      IF(IPANA.EQ.IISUB(7)) GO TO 1425
      IF(IPANA.EQ.IISUB(8)) GO TO 1425
      IF(IPANA.EQ.IISUB(9)) GO TO 1425
      IF(IPANA.EQ.IISUB(10)) GO TO 1425
 1350 CONTINUE
      IPANY=0
      IVARY=0
      IF((IVAR.EQ.MAPT7).OR.(IVAR.EQ.MAPT13)) IVARY=1
      IF((IVAR.EQ.MAPT7).OR.(IVAR.EQ.MAPT13))
     1     print *,' 9999 MAPT7=IVARY'
      IF(IPANA.NE.IBCHK) IPANY=1
      IUHR1=IRAS(3)
      CALL W3AI15(IUHR1,IUHR1,1,2,1H-)
      IDAY1=IRAS(4)
      IMON=IRAS(5)
      IYRR=IRAS(6)
      CALL W3AI15(IDAY1,IDAY,1,2,1H-)
C
CADDED BY KUMAR BEGIN
C
      IYRR = IYRR + 1900
      IF(IYRR.LT.1950) THEN
        IYRR = IYRR + 100
      ENDIF
C
CADDED BY KUMAR END
C
c      CALL DAYOWK(IDAY1,IMON,IYRR,IDAYB,IDAYW)
      CALL DAYOWK(IDAY1,IMON,IYRR,IDAYB,cDAYW)
      print *,'9999 dayowk ',iday1,imon,iyrr,idayb,cdayw
      IOUT1=IOUTIN(1)
      LVL11=ILVLI(1)
      LVL1=ILVL(1,LVL11)
      CALL ENCODE(IBCD(1),20)
      WRITE(99,1405) ISLOTA,ISLOTB,IREM3A
 1405 FORMAT(1X,A4,A1,4X,A4,1X,5HFCST )
      IF(IOUT1.GE.2) GO TO 1360
      CALL ENCODE(IBCD(6),20)
      WRITE(99,1400)ITITLE(1,IOUT1),LVL1,ITITLE(4,IOUT1),ITITLE(5,IOUT1)
 1400 FORMAT(2A4,4HMB S,2A4)
      GO TO 1403
 1360 CONTINUE
      CALL ENCODE(IBCD(6),20)
      WRITE(99,1401)LVL1,(ITITLE(I,IOUT1),I=2,5)
 1401 FORMAT(   A4,4A4)
 1403 CONTINUE
      CALL ENCODE(IBCD(11),48)
      WRITE(99,1402)(ITITLE(I,IOUT1),I=6,8),IUHR1,IDAYW,IDAY1,
     1     IMONT(IMON),IYRR
 1402 FORMAT(3A4,5HVALID,1X,A2,2HZ ,A3,1X,I2,A4,3H   ,I5,11X)
      NCHAR=87
      NCHAR=67
      IF(IVARY.EQ.0) GO TO 1440
C                                                                       00279900
C     ...PUT VARIAN MAP TITLE INTO LABEL ARRAY                          00280000
C                                                                       00280100
C     CALL ENCODE(IBCD(1),8)                                            00280200
C     WRITE(99,1407) INOPN1,INOPN2                                      00280300
C1407 FORMAT(A4,A1,3X)                                                  00280400
      IXL=30
      JXL=960
      HT=11.0
      print *,'before 1410 9999 print'
      PRINT 1410, (IBCD(I),I=1,22)
        DO 1500 IB2 = 1,25
          IBCD2(IB2) = IBCD(IB2)
          write(cbcd2(ib2),1995)ibcd(ib2)
c         cbcd22(ib2)='    '
c         cbcd22(1)='stuf'
 1995    format(a4)
 1500   CONTINUE
        write(cbcd4(1:5),1901)islota,islotb
        write(cbcd4(7:10),1903)iras(1),'HR'
c        write (cbcd4(12:100),1711)ctitleg(11:14),ctitleg(20:21),
C     1  ctitleg(30:31),ctitleg(43:46)
         write(cgulf(1),2711)iras(1),'HR',ctitleg(11:14),'MB'
         write(cgulf(2),2712)iras(3),cdayw
         write(cgulf(3),2713)iras(4),imont(iras(5)),iyrr,'AVN'
 2711    format(i2,a2,x,a4,a2)
 2712    format('VT ',i2.2,'Z',1x,a3)
 2713    format(i2.2,a4,' ',i5,1x,a3) 
        if(iout1 .ne. 4) then
        write (cbcd4(12:100),1711)ctitleg(11:14),
     1   iras(3),cdayw,iras(4),IMONT(iras(5)),iyrr 
             else
        write (cbcd4(12:100),1714)ctitleg(11:14),
     1   iras(3),cdayw,iras(4),IMONT(iras(5)),iyrr 
             endif
1711    format(' FCST ',a4,' STREAMS/ISOTACH ','VALID ',i2.2,'Z ',
     1  a3,1x,i2.2,' ',a4,' ',i5)
1714    format(' FCST ',a4,' STREAMS ','VALID ',i2.2,'Z ',
     1  a3,1x,i2.2,' ',a4,' ',i5)
 1903   format(i2,a2)
 1901   format(a4,a1)
        CALL SPCHK(IXL,JXL,IAPT,IPANA,IPANY,IVARY)
       cbcd4(87:87)  = 'X'
      CALL lPUTLAB(IXL,JXL,HT,cbcd4(1:nchar),0.0,NCHAR,1,0)
      kprior(1)=1
      kprior(2)=1 
      ht=1.0
      call putlab(600,2400,1.,cgulf(1),90.,20,kprior,0)
      call putlab(620,2400,1.,cgulf(2),90.,20,kprior,0)
      call putlab(640,2400,1.,cgulf(3),90.,20,kprior,0)
      ht=11.0
c      CALL lPUTLAB(IXL,JXL,HT,IBCD2,0.0,NCHAR,1,0)
c      CALL PUTLAB(IXL,JXL,HT,IBCD2,0.0,NCHAR,1,0)
 1410 FORMAT('0VARIAN TITLE=  ',22A4)
      CALL ENCODE(ITITSM(1),16)
      WRITE(99,1408)IREM3A,LVL1,IUHR1,IMON,IDAY
 1408 FORMAT(A4,1H/,A3,A2,1HZ,I2,1H/,A2)
      write(ctitsm(1),1956)ititsm(1),ititsm(2)
      write(ctitsm(3),1956)ititsm(3),ititsm(4)
 1956 format(2a4)
       print *,' 9999  ctitsm(1-4) ' ,ctitsm
      CALL lPUTLAB(622,2543,3.0,ctitsm(1),90.0,8,1,0)
      CALL lPUTLAB(635,2543,3.0,ctitsm(3),90.0,8,1,0)
c      CALL PUTLAB(622,2543,3.0,ITITSM(1),90.0,8,1,0)
c      CALL PUTLAB(635,2543,3.0,ITITSM(3),90.0,8,1,0)
      GO TO 1425
C                                                                       00282100
C     ...FAX MAP STRIP TITLES                                           00282200
C                                                                       00282300
 1440 CONTINUE
c  build new (george vandenberghe) title
        write(cbcd4(1:5),1901)islota,islotb
       print 9988,'9999 ISLOTA AND B ',islota,islotb
 9988  format(a25,a4,a1)
        write(cbcd4(7:10),1903)iras(1),'HR'
c        write (cbcd4(12:100),1711)ctitleg(11:14),ctitleg(20:21),
C     1  ctitleg(30:31),ctitleg(43:46)
        write (cbcd4(12:100),1711)ctitleg(11:14),
     1   iras(3),cdayw,iras(4),IMONT(iras(5)),iyrr
      print *,' 9999 strip ',cbcd4(1:100)
      NSTRIP=1
      IF(IRLAB.GE.1) NSTRIP=2
      DO 1449 IX=1,NSTRIP
      IXL=31
      MLAST=MLAST+INCR
      JXL=MLAST
      HT=11.0
      IF(IPANY.EQ.1)HT=1.0
      NCHAR=6
      IBCD1(1)=IBCD(1)
      IBCD1(2)=IBCD(2)
      IF(IPANY.EQ.1)
     1   CALL lPUTLAB(IXL,JXL,HT,cbcd4(1:NCHAR),0.0,NCHAR,1,0)
c      IF(IPANY.EQ.1)CALL lPUTLAB(IXL,JXL,HT,IBCD1(1),0.0,NCHAR,1,0)
c      IF(IPANY.EQ.1)CALL PUTLAB(IXL,JXL,HT,IBCD1(1),0.0,NCHAR,1,0)
      IF(IPANY.NE.1)IXL=IXL+NCHAR*IGIANT
      IF(IPANY.EQ.1)IXL=IXL+NCHAR*IREGU
      NCHAR=10
      IBCD1(3)=ILVL(3,LVL11)
      IBCD1(4)=ILVL(4,LVL11)
      IBCD1(5)=IBCHK
      IF(IPANY.EQ.1)
     1   CALL lPUTLAB(IXL,JXL,HT,cbcd4(9:nchar+9-1),0.0,NCHAR,1,0)
c      IF(IPANY.EQ.1)CALL PUTLAB(IXL,JXL,HT,IBCD1(3),0.0,NCHAR,1,0)
      IF(IPANY.NE.1)IXL=IXL+NCHAR*IGIANT
      IF(IPANY.EQ.1)IXL=IXL+NCHAR*IREGU
      NCHAR=15
      IJHOUR=24
      IF(ITAUI(1).EQ.5) IJHOUR=48
      IF(ITAUI(1).EQ.3) IJHOUR=30
      IF(ITAUI(1).EQ.4) IJHOUR=36
      IF(ITAUI(1).EQ.1) IJHOUR=18
      CALL ENCODE(IBCD1(6),16)
      WRITE(99,1452)IJHOUR,ITITL1(2),ITITL1(3),ITITL2(1)
 1452 FORMAT(I2,2HHR,A4,A2,A4,2X)
      HT=1.0
      IF(IPANY.EQ.1)
     1   CALL lPUTLAB(IXL,JXL,HT,cbcd4(21:nchar+21-1),0.0,NCHAR,1,0)
c      IF(IPANY.EQ.1)CALL PUTLAB(IXL,JXL,HT,IBCD1(6),0.0,NCHAR,1,0)
      IXL=IXL+NCHAR*IREGU
      NCHAR=18
      HT=11.0
      CALL ENCODE(IBCD1(10),20)
      WRITE(99,1441)IUHR1,IDAYW,IDAY1,IMONT(IMON),IYRR
 1441 FORMAT(A2,2HZ ,A3,1X,I2,A4,1X,I5,3X)
      IF(IPANY.EQ.1) 
     1  CALL lPUTLAB(IXL,JXL,HT,cbcd4(37:nchar+37-1),0.0,NCHAR,1,0)
c      IF(IPANY.EQ.1)CALL PUTLAB(IXL,JXL,HT,IBCD1(10),0.0,NCHAR,1,0)
      IF(IPANY.NE.1)IXL=IXL+NCHAR*IGIANT
      IF(IPANY.EQ.1)IXL=IXL+NCHAR*IREGU
      IF(IPANY.EQ.1) GO TO 1445
C                                                                       00286700
C     ...FOUND WHOLE MAP                                                00286800
C                                                                       00286900
      NCHAR=40
      HT=1.0
      CALL W3AI15(IRAS(7),IUHR2,1,2,1H-)
      CALL ENCODE(IBCD1(15),40)
      WRITE(99,1443)ITITL3(1),ITITL3(2),IUHR2,IRAS(8),IBCHK,
     1        IBCHK,IBCHK,IBCHK
 1443 FORMAT(2A4,2HON,1X,A2,2HZ/,I2,3H.  ,2A4,2H  ,2A4,2H  )
C     CALL PUTLAB(IXL,JXL,HT,IBCD1(57:nchar+57-1),0.0,NCHAR,1,0)                   00287700
      IXL=IXL+NCHAR*IREGU
      IF (IOUT1 .GT. 2) GO TO 1457
      NCHAR=24
      CALL ENCODE(IBCD1(25),24)
      WRITE(99,1454)(IARR1(I),I=1,3),(IARR2(I),I=1,3),IARR3
 1454 FORMAT(2(2A4,A2),A4)
C     CALL PUTLAB(IXL,JXL,HT,IBCD1(25),0.0,NCHAR,1,0)                   00288400
      DO 1456 I=31,35
      IBCD1(I)=IBCHK
 1456 CONTINUE
       print *,' before IOUTIN 1424'
      IF(IOUTIN(2).EQ.1) GO TO 1424
       print *,' after  IOUTIN 1424'
      IXL=IXL+NCHAR*IREGU
      IXL1=IXL
      NCHAR=20
      CALL ENCODE(IBCD1(31),20)
      WRITE(99,1458)(ITITL6(I),I=1,3),(ITITL7(I),I=1,3)
 1458 FORMAT(2(2A4,A2))
C     CALL PUTLAB(IXL,JXL,HT,IBCD1(31),0.0,NCHAR,1,0)                   00289500
      IXL=IXL1+IREGU
      NCHAR=2
      HT=11.0
      print *,' 9999 cbox and ccir from titel ',cbox,ccirc
      CALL lPUTLAB(IXL,JXL,HT,cbox,0.0,NCHAR,2,0)
c      CALL PUTLAB(IXL,JXL,HT,IBOX,0.0,NCHAR,2,0)
      IXL=IXL1+10*IREGU
      CALL lPUTLAB(IXL,JXL,HT,ccirc,0.0,NCHAR,2,0)
c      CALL PUTLAB(IXL,JXL,HT,ICIRC,0.0,NCHAR,2,0)
      stop 'circle'
      GO TO 1424
 1457 CONTINUE
      CALL ENCODE(IBCD1(25),44)
      WRITE(99,1459) (IDESC(I,IOUT1),I=1,11)
 1459 FORMAT(11A4)
C     CALL PUTLAB(IXL,JXL,HT,IBCD1(25),0.0,44,1,0)                      00290700
      GO TO 1424
C                                                                       00290900
C     ...FOUND PANEL MAP                                                00291000
C     ...GENERATE LEFT PANEL STRIP TITLE FOR LABEL ARRAY                00291100
C                                                                       00291200
 1445 CONTINUE
      NCHAR=24
      HT=1.0
      CALL ENCODE(IBCD1(15),24)
      WRITE(99,1454)(IARR1(I),I=1,3),(IARR2(I),I=1,3),IARR3
       print *, '9999 before 1445 putlab '
      stop 'after 1445'
      CALL lPUTLAB(IXL,JXL,HT,cbcd4(57:nchar+57-1),0.0,NCHAR,1,0)
c      CALL PUTLAB(IXL,JXL,HT,IBCD1(15),0.0,NCHAR,1,0)
      IXL=IXL+NCHAR*IREGU
C                                                                       00292000
C     ...PRINT LEFT PANEL STRIP TITLE                                   00292100
C                                                                       00292200
      PRINT 1446, (IBCD1(I),I=1,20)
 1446 FORMAT('0LEFT PANEL STRIP TITLE=  ',20A4)
C                                                                       00292500
C     ...GENERATE RIGHT PANEL STRIP TITLE FOR LABEL ARRAY               00292600
C                                                                       00292700
      NCHAR=10
      HT=11.0
      CALL ENCODE(IBCD1(21),12)
      WRITE(99,1455)IREM1B
 1455 FORMAT(2X,A4,2HMB,4X)
      CALL lPUTLAB(IXL,JXL,HT,cbcd4(81:nchar+81-1),0.0,NCHAR,1,0)
c      CALL PUTLAB(IXL,JXL,HT,IBCD1(21),0.0,NCHAR,1,0)
      IXL=IXL+NCHAR*IGIANT
      NCHAR=15
      HT=1.0
      CALL ENCODE(IBCD1(24),16)
      WRITE(99,1453)IREM1A,ITITL1(2),ITITL1(3),ITITL2(1)
 1453 FORMAT(2A4,A2,A4,2X)
      CALL lPUTLAB(IXL,JXL,HT,cbcd4(93:nchar+93-1),0.0,NCHAR,1,0)
c      CALL PUTLAB(IXL,JXL,HT,IBCD1(24),0.0,NCHAR,1,0)
      IXL=IXL+NCHAR*IREGU
      NCHAR=18
      HT=11.0
      IF(IX.NE.1) GO TO 472
C     ...SAVE OLD TAU                                                   00294500
      IOLD=IRAS(1)
      IRAS(1)=24
      IF(IREM1A.EQ.IHOUR(1)) IRAS(1)=18
      IF(IREM1A.EQ.IHOUR(3)) IRAS(1)=30
      IF(IREM1A.EQ.IHOUR(4)) IRAS(1)=36
      CALL UPDATR
      JUHR1=IRAS(3)
      CALL W3AI15(JUHR1,JUHR1,1,2,1H-)
      JDAY1=IRAS(4)
      JMON=IRAS(5)
      JYRR=IRAS(6)
      CALL W3AI15(JDAY1,JDAY,1,2,1H-)
      CALL DAYOWK(JDAY1,JMON,JYRR,JDAYB,JDAYW)
      print*,'9999 DAYOWK # 2  ',JDAY1,JMON,JYRR,JDAYB,JDAYW
  472 CONTINUE
      CALL ENCODE(IBCD1(28),20)
      WRITE(99,1441)JUHR1,JDAYW,JDAY1,IMONT(JMON),JYRR
c      CALL PUTLAB(IXL,JXL,HT,IBCD1(28),0.0,NCHAR,1,0)
      CALL lPUTLAB(IXL,JXL,HT,cbcd4(109:109+nchar-1 ),0.0,NCHAR,1,0)
      IXL=IXL+NCHAR*IGIANT
      IXL1=IXL
      HT=1.0
      NCHAR=24
      CALL ENCODE(IBCD1(33),24)
      JLEV=IBCHK
      DO 462 JX=1,4
      IF(IREM1B.EQ.ILEV(1,JX)) JLEV=ILEV(2,JX)
  462 CONTINUE
      CALL ENCODE(IBCD1(33),24)
      WRITE(99,1462)(IARR1(I),I=1,3),JLEV,IARR3
 1462 FORMAT(2A4,A2,2HE ,A3,5HMB WI,A4)
c      CALL PUTLAB(IXL,JXL,HT,IBCD1(129:129+nchar-1),0.0,NCHAR,1,0)
      CALL lPUTLAB(IXL,JXL,HT,cbcd4(129:129+nchar-1),0.0,NCHAR,1,0)
C     ...REPLACE TAU WITH OLD VALUE                                     00297600
      IRAS(1)=IOLD
      CALL UPDATR
C                                                                       00297900
C     ...PRINT RIGHT PANEL STRIP TITLE                                  00298000
C                                                                       00298100
      PRINT 1447, (IBCD1(I),I=21,38)
 1447 FORMAT('0RIGHT PANEL STRIP TITLE=  ',18A4)
      GO TO 1449
C                                                                       00298500
C     ...PRINT WHOLE MAP STRIP TITLE                                    00298600
C                                                                       00298700
 1424 CONTINUE
      print *,' 9999 at 1424'
      PRINT 1448,(IBCD1(I),I=1,24)
      PRINT 1451,(IBCD1(I),I=25,35)
 1451 FORMAT('0WHOLE MAP STRIP TITLE CONTINUED=  ',11A4)
 1448 FORMAT('0WHOLE MAP STRIP TITLE=  ',24A4)
        IXL = 35
        IF(IAPT .EQ. 1) IXL = 30
        HT = 11.0
        IF(IAPT .EQ. 1) HT = 1.0
        DO 1501 IB2 = 1,40
          IBCD2(IB2) = IBCD1(IB2)
 1501   CONTINUE
C -------- ONE PANEL FAX SECTION---------                               00300000
C  CRUNCH OUT SPACES IN FAX TITLES, PUT ON NOAA INFO & PRINT OUT RESULT 00300100
        CALL SPCHK(IXL,JXL,IAPT,IPANA,IPANY,IVARY)
        PRINT 1510, (IBCD2(I),I=1,25)
 1510   FORMAT(1X,'IBCD2= ',25A4)
c        CALL PUTLAB(IXL,JXL,HT,IBCD2(1),0.0,87,1,0)
        CALL lPUTLAB(IXL,JXL,HT,cbcd4(1:87),0.0,87,1,0)
c        CALL lPUTLAB(IXL,JXL,HT,cbcd4(1:47),0.0,47,1,0)
c        CALL lPUTLAB(IXL,JXL,HT,cbcd4(1:47),0.0,47,0,0)
c        CALL lPUTLAB(100,1000,HT,cbcd4(1:47),0.0,47,0,0)
      print *,' EXITING TITLES 9999'
      RETURN
 1449 CONTINUE
        IXL = 40
C  PUT NOAA INFO CENTERED ON MULTIPLE PANEL CHARTS                      00300900
        CALL SPCHK(IXL,JXL,IAPT,IPANA,IPANY,IVARY)
 1425 CONTINUE
      print *,' EXITING TITLES 99992'
      RETURN
      END
