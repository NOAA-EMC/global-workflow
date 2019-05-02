      SUBROUTINE MERCIN(ISWTCH,NUMF,MAPON,NSLOTS,ILVLI,
     1     IFLDI,ITAUI,ICODIN,INAMIN,IOUTIN,IINFA,IINFB)
C                                                                       00162200
      COMMON/ILY/ITAU1A,LVL1A,IFID(14),JFID(14)
C                                                                       00162400
      DIMENSION ILVLI(3),IFLDI(3),ITAUI(3),IOUTIN(2)
      DATA IBCHK/4H    /
      DATA LU/8/
C                                                                       00162800
C     LEVEL CONTROL TABLE                                               00162900
C                                                                       00163000
      DIMENSION ILVL(3,9)
      DATA    NLVLS/9/
      DATA    ILVL/4H700 ,1H ,1,4H500 ,1H ,2,4H400 ,1H ,3,4H300 ,1H ,4,
     1             4H200 ,1H ,5,4H250 ,1H ,6,4HTROP,1H ,7,4H1000,1H ,8,
     2             4H850 ,1H ,9/
C                                                                       00163600
C     FIELD CONTROL TABLE                                               00163700
C                                                                       00163800
      DIMENSION IFLD(3,8)
      DATA    NFLDS/8/
      DATA    IFLD/4HSTRM,1H ,1,4HTEMP,1HS,2,4HU   ,1H ,3,4HV   ,1H ,4,
     1             4HITAC,1HH,5,4HTP P,1HR,6,4HTP T,1HT,7,4HTP W,1HS,8/
C                                                                       00164300
C     TAU CONTROL TABLE                                                 00164400
C                                                                       00164500
      DIMENSION ITAU(3,5)
      DATA    NTAUS/5/
      DATA    ITAU/4H18HR,1H ,1,4H24HR,1H ,2,4H30HR,1H ,3,4H36HR,1H ,4,
     1             4H48HR,1H ,5/
C                                                                       00165000
C     OPERATIONAL RUN TYPE TABLE                                        00165100
C                                                                       00165200
      DIMENSION ICODES(3,5)
      DATA    NCODES/5/
      DATA ICODES/4HOPN0,1H9,1,4HOPN1,1H0,2,4HOPN2,1H6,3,4HOPN2,1H8,4,
     1            4HOPN2,1H5,5/
C                                                                       00165700
C     FILE CONTROL TABLE                                                00165800
C                                                                       00165900
      DIMENSION IFILER(3,2)
      DATA    NFILES/2/
      DATA    IFILER/4HMERC,1HI,1,4H    ,1H ,2/
C                                                                       00166300
C     OUTPUT TYPE CONTROL TABLE                                         00166400
C                                                                       00166500
      DIMENSION IIOUT(4,6)
      DATA    NOUTS/6/
      DATA    IIOUT/4HS-I ,1H ,1,1,4HS-I-,1HP,2,2,4HS-I-,1HT,3,3,
     1              4HS   ,1H ,4,4,4HP-T-,1HW,5,5,4HP-V ,1H ,6,6/
C                                                                       00167000
C     BASIC MAP CONTROLS (SET FOR EACH MAP)                             00167100
C                                                                       00167200
C     WHERE ICARD1=          CARD CONTROL(BLANK-A1)                     00167300
C     WHERE LVL1A,LVL1B=     1ST LEVEL (A4,A1)                          00167400
C     WHERE IFLD1A,IFLD1B=   1ST FIELD TYPE (A4,A1)                     00167500
C     WHERE ITAU1A,ITAU1B=   1ST TAU PERIOD (A4,A1)                     00167600
C     WHERE LVL2A,LVL2B=     2ND LEVEL (A4,A1)                          00167700
C     WHERE IFLD2A,IFLD2B=   2ND FIELD TYPE (A4,A1)                     00167800
C     WHERE ITAU2A,ITAU2B=   2ND TAU PERIOD (A4,A1)                     00167900
C     WHERE LVL3A,LVL3B=     3RD LEVEL (A4,A1)                          00168000
C     WHERE IFLD3A,IFLD3B=   3RD FIELD TYPE (A4,A1)                     00168100
C     WHERE ITAU3A,ITAU3B=   3RD TAU PERIOD (A4,A1)                     00168200
C     WHERE ICODEA,ICODEB=   OPERATIONAL CODE TYPE (A4,A1)              00168300
C     WHERE IINFA,IINFB=     INPUT FILE NAME (A4,A1)                    00168400
C     WHERE IOUTA,IOUTB=     OUTPUT FIELD COMBINATION                   00168500
C     WHERE MAPON=           MAP NUMBER (I4)                            00168600
C     WHERE NSLOTS=          NUMBER OF MAP PARTS (I4)                   00168700
C                                                                       00168800
 7025 CONTINUE
      IF(ISWTCH.EQ.1) GO TO 7040
C                                                                       00169100
C     USE DATA CARD INPUT                                               00169200
C                                                                       00169300
 7030 CONTINUE
      READ 7200, ICARD1,LVL1A,LVL1B,IFLD1A,IFLD1B,ITAU1A,ITAU1B,LVL2A,
     1           LVL2B,IFLD2A,IFLD2B,ITAU2A,ITAU2B,LVL3A,LVL3B,IFLD3A,
     2           IFLD3B,ITAU3A,ITAU3B,ICODEA,ICODEB,IINFA,IINFB,IOUTA,
     3           IOUTB,MAPON,NSLOTS
 7200 FORMAT(A1,10(A4,A1),3X,2(A4,A1),2I4)
      GO TO 7045
C                                                                       00170100
C     USE DATA SET CONTROL FILE INPUT                                   00170200
C                                                                       00170300
 7040 CONTINUE
      READ(LU,7200) ICARD1,LVL1A,LVL1B,IFLD1A,IFLD1B,ITAU1A,ITAU1B,
     2              LVL2A,LVL2B,IFLD2A,IFLD2B,ITAU2A,ITAU2B,LVL3A,
     3              LVL3B,IFLD3A,IFLD3B,ITAU3A,ITAU3B,ICODEA,
     4              ICODEB,IINFA,IINFB,IOUTA,IOUTB,MAPON,NSLOTS
 7045 CONTINUE
      NUMF=3
      IF(LVL3A.EQ.IBCHK.AND.LVL3B.EQ.IBCHK) NUMF=2
      IF(LVL2A.EQ.IBCHK.AND.LVL2B.EQ.IBCHK.AND.LVL3A.EQ.IBCHK.AND.
     1LVL3B.EQ.IBCHK) NUMF=1
      PRINT 7201,MAPON,NUMF
 7201 FORMAT(1H1,'MAP NUMBER=  ',I3,'  NUMBER OF INPUT FIELDS=  ',I3)
      PRINT 7202,LVL1A,LVL1B,IFLD1A,IFLD1B,ITAU1A,ITAU1B,LVL2A,LVL2B,
     1           IFLD2A,IFLD2B,ITAU2A,ITAU2B,LVL3A,LVL3B,IFLD3A,
     2           IFLD3B,ITAU3A,ITAU3B
 7202 FORMAT(1H0,'DATA TYPES=  ',9(A4,A1))
      PRINT 7203,ICODEA,ICODEB,IINFA,IINFB,IOUTA,IOUTB,NSLOTS
 7203 FORMAT(1H0,'OPERATIONAL CODE=  ',A4,A1,'  INPUT FILE=  ',A4,A1,
     X'OUTPUT COMB. TYPE=  ',A4,A1,'  NUMBER OF FAX VARIAN CUTS=  ',I3)
      DO 7210 IX=1,3
      ILVLI(IX)=0
      IFLDI(IX)=0
      ITAUI(IX)=0
 7210 CONTINUE
      ICODIN=0
      INAMIN=0
      DO 7211 IY=1,2
      IOUTIN(IY)=0
 7211 CONTINUE
C                                                                       00173300
C     CHECK FOR REASONABLE LEVEL REQUESTS                               00173400
C                                                                       00173500
C                                                                       00173600
C     CHECK LEVEL 1                                                     00173700
C                                                                       00173800
      DO 7215 IK=1,NLVLS
      IF(LVL1A.EQ.IBCHK.AND.LVL1B.EQ.IBCHK) GO TO 7205
      IF(LVL1A.NE.ILVL(1,IK)) GO TO 7215
      IF(LVL1B.NE.ILVL(2,IK)) GO TO 7215
      ILVLI(1)=ILVL(3,IK)
      IP=ILVLI(1)
      PRINT 7204,IP
 7204 FORMAT(1H0,'LEVEL 1 INDEX FOUND=  ',I2)
      GO TO 7220
 7205 PRINT 7206
 7206 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR LEVEL 1-WAS BLANK')
       CALL W3TAGE('TRPSFPRV')
      STOP 207
 7215 CONTINUE
      PRINT 7216
 7216 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR LEVEL 1')
       CALL W3TAGE('TRPSFPRV')
      STOP 217
 7220 CONTINUE
C                                                                       00176000
C     CHECK LEVEL 2                                                     00176100
C                                                                       00176200
      DO 7235 IK=1,NLVLS
      IF(LVL2A.EQ.IBCHK.AND.LVL2B.EQ.IBCHK) GO TO 7240
      IF(LVL2A.NE.ILVL(1,IK)) GO TO 7235
      IF(LVL2B.NE.ILVL(2,IK)) GO TO 7235
      ILVLI(2)=ILVL(3,IK)
      IP=ILVLI(2)
      PRINT 7234,IP
 7234 FORMAT(1H0,'LEVEL 2 INDEX FOUND=  ',I2)
      GO TO 7240
 7235 CONTINUE
      PRINT 7236
 7236 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR LEVEL 2')
       CALL W3TAGE('TRPSFPRV')
      STOP 237
 7240 CONTINUE
C                                                                       00177900
C     CHECK LEVEL 3                                                     00178000
C                                                                       00178100
      DO 7255 IK=1,NLVLS
      IF(LVL3A.EQ.IBCHK.AND.LVL3B.EQ.IBCHK) GO TO 7260
      IF(LVL3A.NE.ILVL(1,IK)) GO TO 7255
      IF(LVL3B.NE.ILVL(2,IK)) GO TO 7255
      ILVLI(3)=ILVL(3,IK)
      IP=ILVLI(3)
      PRINT 7254,IP
 7254 FORMAT(1H0,'LEVEL 3 INDEX FOUND=  ',I2)
      GO TO 7260
 7255 CONTINUE
      PRINT 7256
 7256 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR LEVEL 3')
       CALL W3TAGE('TRPSFPRV')
      STOP 257
 7260 CONTINUE
C                                                                       00179800
C     CHECK FOR REASONABLE FIELD REQUESTS                               00179900
C                                                                       00180000
C                                                                       00180100
C     CHECK FIELD 1                                                     00180200
C                                                                       00180300
      DO 7315 IK=1,NFLDS
      IF(IFLD1A.EQ.IBCHK.AND.IFLD1B.EQ.IBCHK) GO TO 7305
      IF(IFLD1A.NE.IFLD(1,IK)) GO TO 7315
      IF(IFLD1B.NE.IFLD(2,IK)) GO TO 7315
      IFLDI(1)=IFLD(3,IK)
      IP=IFLDI(1)
      PRINT 7314,IP
 7314 FORMAT(1H0,'FIELD 1 INDEX FOUND=  ',I2)
      GO TO 7320
 7305 PRINT 7306
 7306 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR FIELD 1-WAS BLANK')
       CALL W3TAGE('TRPSFPRV')
      STOP 307
 7315 CONTINUE
      PRINT 7316
 7316 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR FIELD1')
       CALL W3TAGE('TRPSFPRV')
      STOP 317
 7320 CONTINUE
C                                                                       00182500
C     CHECK FIELD 2                                                     00182600
C                                                                       00182700
      DO 7335 IK=1,NFLDS
      IF(IFLD2A.EQ.IBCHK.AND.IFLD2B.EQ.IBCHK) GO TO 7340
      IF(IFLD2A.NE.IFLD(1,IK)) GO TO 7335
      IF(IFLD2B.NE.IFLD(2,IK)) GO TO 7335
      IFLDI(2)=IFLD(3,IK)
      IP=IFLDI(2)
      PRINT 7334,IP
 7334 FORMAT(1H0,'FIELD 2 INDEX FOUND=  ',I2)
      GO TO 7340
 7335 CONTINUE
      PRINT 7336
 7336 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR FIELD 2')
       CALL W3TAGE('TRPSFPRV')
      STOP 337
 7340 CONTINUE
C                                                                       00184400
C     CHECK FIELD 3                                                     00184500
C                                                                       00184600
      DO 7355 IK=1,NFLDS
      IF(IFLD3A.EQ.IBCHK.AND.IFLD3B.EQ.IBCHK) GO TO 7360
      IF(IFLD3A.NE.IFLD(1,IK)) GO TO 7355
      IF(IFLD3B.NE.IFLD(2,IK)) GO TO 7355
      IFLDI(3)=IFLD(3,IK)
      IP=IFLDI(3)
      PRINT 7354,IP
 7354 FORMAT(1H0,'FIELD 3 INDEX FOUND=  ',I2)
      GO TO 7360
 7355 CONTINUE
      PRINT 7356
 7356 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR FIELD 3')
       CALL W3TAGE('TRPSFPRV')
      STOP 357
 7360 CONTINUE
C                                                                       00186300
C     CHECK FOR REASONABLE TAU REQUESTS                                 00186400
C                                                                       00186500
C                                                                       00186600
C     CHECK TAU 1                                                       00186700
C                                                                       00186800
      DO 7415 IK=1,NTAUS
      IF(ITAU1A.EQ.IBCHK.AND.ITAU1B.EQ.IBCHK) GO TO 7415
      IF(ITAU1A.NE.ITAU(1,IK)) GO TO 7415
      IF(ITAU1B.NE.ITAU(2,IK)) GO TO 7415
      ITAUI(1)=ITAU(3,IK)
      IP=ITAUI(1)
      PRINT 7414,IP
 7414 FORMAT(1H0,'TAU 1 INDEX FOUND=  ',I2)
      GO TO 7420
 7405 PRINT 7406
 7406 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR ITAU 1-WAS BLANK')
       CALL W3TAGE('TRPSFPRV')
      STOP 407
 7415 CONTINUE
      PRINT 7416
 7416 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR ITAU 1')
       CALL W3TAGE('TRPSFPRV')
      STOP 417
 7420 CONTINUE
C                                                                       00189000
C     CHECK TAU 2                                                       00189100
C                                                                       00189200
      DO 7435 IK=1,NTAUS
      IF(ITAU2A.EQ.IBCHK.AND.ITAU2B.EQ.IBCHK) GO TO 7440
      IF(ITAU2A.NE.ITAU(1,IK)) GO TO 7435
      IF(ITAU2B.NE.ITAU(2,IK)) GO TO 7435
      ITAUI(2)=ITAU(3,IK)
      IP=ITAUI(2)
      PRINT 7434,IP
 7434 FORMAT(1H0,'TAU 2 INDEX FOUND=  ',I2)
      GO TO 7440
 7435 CONTINUE
      PRINT 7436
 7436 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR ITAU 2')
       CALL W3TAGE('TRPSFPRV')
      STOP 437
 7440 CONTINUE
C                                                                       00190900
C     CHECK TAU 3                                                       00191000
C                                                                       00191100
      DO 7455 IK=1,NTAUS
      IF(ITAU3A.EQ.IBCHK.AND.ITAU3B.EQ.IBCHK) GO TO 7460
      IF(ITAU3A.NE.ITAU(1,IK)) GO TO 7455
      IF(ITAU3B.NE.ITAU(2,IK)) GO TO 7455
      ITAUI(3)=ITAU(3,IK)
      IP=ITAUI(3)
      PRINT 7454,IP
 7454 FORMAT(1H0,'TAU 3 INDEX FOUND=  ',I2)
      GO TO 7460
 7455 CONTINUE
      PRINT 7456
 7456 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR ITAU 3')
       CALL W3TAGE('TRPSFPRV')
      STOP 457
 7460 CONTINUE
C                                                                       00192800
C     CHECK FOR REASONABLE OPERATIONAL CODE REQUESTS                    00192900
C                                                                       00193000
      DO 7515 IK=1,NCODES
      IF(ICODEA.EQ.IBCHK.AND.ICODEB.EQ.IBCHK) GO TO 7505
      IF(ICODEA.NE.ICODES(1,IK)) GO TO 7515
      IF(ICODEB.NE.ICODES(2,IK)) GO TO 7515
      ICODIN=ICODES(3,IK)
      PRINT 7514,ICODIN
 7514 FORMAT(1H0,'CODE INDEX FOUND=  ',I2)
      GO TO 7520
 7505 PRINT 7506
 7506 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR OPNL CODE-WAS BLANK')
       CALL W3TAGE('TRPSFPRV')
      STOP 507
 7515 CONTINUE
      PRINT 7516
 7516 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR OPNL CODE')
       CALL W3TAGE('TRPSFPRV')
      STOP 517
 7520 CONTINUE
C                                                                       00195100
C     CHECK FOR REASONABLE FILE REQUESTS                                00195200
C                                                                       00195300
      DO 7535 IK=1,NFILES
      IF(IINFA.EQ.IBCHK.AND.IINFB.EQ.IBCHK) GO TO 7525
      IF(IINFA.NE.IFILER(1,IK)) GO TO 7535
      IF(IINFB.NE.IFILER(2,IK)) GO TO 7535
      INAMIN=IFILER(3,IK)
      PRINT 7524,INAMIN
 7524 FORMAT(1H0,'FILE INDEX FOUND=  ',I2)
      GO TO 7540
 7525 PRINT 7526
 7526 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR FILE-WAS BLANK')
       CALL W3TAGE('TRPSFPRV')
      STOP 527
 7535 CONTINUE
      PRINT 7536
 7536 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR FILE')
       CALL W3TAGE('TRPSFPRV')
      STOP 537
 7540 CONTINUE
C                                                                       00197400
C     CHECK FOR REASONABLE OUTPUT REQUESTS                              00197500
C                                                                       00197600
      DO 7555 IK=1,NOUTS
      IF(IOUTA.EQ.IBCHK.AND.IOUTB.EQ.IBCHK) GO TO 7545
      IF(IOUTA.NE.IIOUT(1,IK)) GO TO 7555
      IF(IOUTB.NE.IIOUT(2,IK)) GO TO 7555
      IOUTIN(1)=IIOUT(3,IK)
      IOUTIN(2)=IIOUT(4,IK)
      IP=IOUTIN(1)
      IQ=IOUTIN(2)
      PRINT 7554,IP,IQ
 7554 FORMAT(1H0,'OUTPUT TYPE INDEX FOUND=  ',I2,'OUTPUT MAP SET FOUND
     X=  ',I2)
      GO TO 7560
 7545 CONTINUE
      PRINT 7546
 7546 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR OUTPUT TYPE-WAS BLANK')
       CALL W3TAGE('TRPSFPRV')
      STOP 547
 7555 CONTINUE
      PRINT 7556
 7556 FORMAT(1H0,'ERROR ON MAP CONTROL INPUT FOR OUTPUT TYPE')
       CALL W3TAGE('TRPSFPRV')
      STOP 557
 7560 CONTINUE
      print *,' 9999 ioutin' ,ioutin
      RETURN
      eND
