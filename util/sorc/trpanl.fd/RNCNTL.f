      SUBROUTINE RNCNTL(NMAPS,IDATC,IOPN,ISWTCH,KRUN1,ITOUT1,ICYC1,
     1                  INOPN1,INOPN2,INOPNA,INOPNB)
      DIMENSION LKRUN(3,4)
      DIMENSION LICYL(2,2)
      DIMENSION LITOUT(3,4)
      DIMENSION ICODES(3,5)
      DIMENSION JCODES(2,5)
      DATA      NKRUN/4/
C     ...RUN TYPE CONTROL TABLE                                         00145800
C                                                                       00145900
      DATA      LKRUN/4HOPNL,1H ,1,4HOPNL,1HB,2,4HCOUT,1HD,3,4HCOUT,1HT,
     1               4/
C                                                                       00146200
C     ...OUTPUT CYCLE CONTROL TABLE                                     00146300
C                                                                       00146400
      DATA    LICYL/3H00Z,1,3H12Z,2/
      DATA    NITOUT/4/
C                                                                       00146700
C     ...OUTPUT TAU CONTROL TABLE                                       00146800
C                                                                       00146900
      DATA    LITOUT/4H1824,1HH,1,4H3036,1HH,2,4HALL ,1H ,3,4H2448,1HH,4
     1     /
      DATA     NICYL/2/
      DATA     NMAPM/100/
      DATA     NOPCDS/5/
      DATA     ICODES/4HOPN0,1H9,1,4HOPN1,1H0,2,4HOPN2,1H5,3,4HOPN2,1H6,
     1        4,4HOPN2,1H8,5/
      DATA    JCODES/4HJ080,1H4,4HJ082,1H4,4HJ087,1H4,4HJ999,1H8,
     +               4HJ999,1H9/
C     ...THIS SUBROUTINE READS IN THE BASIC JOB RUN                     00147900
C     ...CONTROLS AND CHECKS THEM FOR PROPER CLASSIFICATIONS            00148000
C                                                                       00148100
C     ...INPUT CONTROLS                                                 00148200
C     (1)      KRUNA,KRUNB   (A4,A1)                                    00148300
C     WHERE         KRUN=1   OPNL 18/24 OR 24/48 RUN                    00148400
C                  2    OPNL 30/36 RUN BACKUP                           00148500
C                  3    CHECKOUT RUN(DISK)                              00148600
C                  4    CHECKOUT RUN(TAPE)                              00148700
C   (2)  ITOUTA,ITOUTB  (A4,A1)                                         00148800
C   WHERE    ITOUT=1    OUTPUT 18/24 HR REGULAR                         00148900
C                  2    OUTPUT 30/36 HR BACKUP                          00149000
C                  3    ALL                                             00149100
C                  4    OUTPUT 24/48 HR REGULAR                         00149200
C   (3)  ICYCLA,ICYCLB  (A4,A1)                                         00149300
C   WHERE   ICYCLE=1   00Z                                              00149400
C212                                                                    00149500
C                                                                       00149600
C   (4)  NMAPS       (I5)                                               00149700
C   WHERE    NMAPS=NO. OF MAPS TO BE PROCESSED                          00149800
C                                                                       00149900
C   (5)  IDATC       (I5)                                               00150000
C   WHERE    IDATC=0    NO DATE/TIME TEST                               00150100
C                  1    DATE/TIME TEST                                  00150200
C   (6)  IOPN        (I5)                                               00150300
C   WHERE    IOPN=0     CHECKOUT RUN                                    00150400
C                 1     OPERATIONAL RUN                                 00150500
C                                                                       00150600
C   (7)  INTAPE      (I5)                                               00150700
C   WHERE  INTAPE=0     INPUT FROM DISK-OPERATIONAL                     00150800
C                 1     INPUT SPECTRAL FILES FROM TAPE                  00150900
C                                                                       00151000
C   (8)  INOPNA,INOPNB (A4,A1)                                          00151100
C   WHERE    INOPN=     OPERATIONAL JOB TYPE                            00151200
C                                                                       00151300
C   (9)  ISWTCH=0       INPUT SCHEDULE CONTROLS FROM CARDS              00151400
C               1       INPUT SCHEDULE CONTROLS FORM DISK               00151500
C                                                                       00151600
C     ...BASIC JOB RUN CONTROLS (SET FOR EACH JOB)                      00151700
C                                                                       00151800
C                                                                       00151900
C                                                                       00152000
      READ 5500,KRUNA,KRUNB,ITOUTA,ITOUTB,ICYCLA,ICYCLB,NMAPS,IDATC,
     1          IOPN,INTAPE,INOPNA,INOPNB,ISWTCH
 5500 FORMAT(3(A4,A1),4I5,A4,A1,I5)
      PRINT 5509
 5509 FORMAT('1BASIC JOB RUN CONTROLS (SET FOR EACH JOB)')
      PRINT 5510, KRUNA,KRUNB,ITOUTA,ITOUTB,ICYCLA,ICYCLB,NMAPS
 5510 FORMAT('0OPERATIONAL RUN TYPE=  ',A4,A1,'  OUTPUT TAU=  ',A4,A1,'
     1RUN CYCLE=  ',A4,A1,'  NUMBER OF MAPS=  ',I5)
      PRINT 5511,IDATC,IOPN,INTAPE
 5511 FORMAT('0DATE CHECK=  ',I5,'  OPERATIONAL FLAG=  ',I5,'  INPUT TAP
     1E FLAG=  ',I5)
      PRINT 5513,ISWTCH,INOPNA,INOPNB
 5513 FORMAT('0INPUT SCHEDULE CONTROL SWITCH=  ',I5,'  OPNL JOB TYPE=  '
     1,A4,A1)
      DO 5550 IK=1,NKRUN
      IF((KRUNA.NE.LKRUN(1,IK)).OR.(KRUNB.NE.LKRUN(2,IK))) GO TO 5550
      KRUN1=LKRUN(3,IK)
      GO TO 5553
 5550 CONTINUE
      PRINT 5540
 5540 FORMAT('0ERROR ON CONTROL INPUT CARD FOR KRUN-FIX THEN RESTART')
       CALL W3TAGE('TRPSFPRV')
      STOP 541
 5553 CONTINUE
      DO 5555 IK=1,NITOUT
      IF((ITOUTA.NE.LITOUT(1,IK)).OR.(ITOUTB.NE.LITOUT(2,IK))) GO TO
     15555
      ITOUT1=LITOUT(3,IK)
      GO TO 5560
 5555 CONTINUE
      PRINT 5543
 5543 FORMAT('0ERROR ON CONTROL INPUT CARD FOR ITOUT-FIX THEN RESTART')
       CALL W3TAGE('TRPSFPRV')
      STOP 544
 5560 CONTINUE
      DO 5565 IK=1,NICYL
      IF(ICYCLA.NE.LICYL(1,IK)) GO TO 5565
      ICYC1=LICYL(2,IK)
      GO TO 5570
 5565 CONTINUE
      PRINT 5545
 5545 FORMAT('0ERROR ON CONTROL INPUT CARD FOR ICYCLE-FIX THEN RESTART')
       CALL W3TAGE('TRPSFPRV')
      STOP 546
 5570 CONTINUE
      IF((NMAPS.LE.NMAPM).AND.(NMAPS.GT.0)) GO TO 5575
      PRINT 5547
 5547 FORMAT('0ERROR ON CONTROL INPUT CARD FOR NMAPS-FIX THEN RESTART')
       CALL W3TAGE('TRPSFPRV')
      STOP 552
 5575 CONTINUE
      IF((IDATC.EQ.0).OR.(IDATC.EQ.1)) GO TO 5620
      PRINT 5600
 5600 FORMAT('0ERROR ON CONTROL INPUT CARD FOR IDATC-FIX THEN RESTART')
       CALL W3TAGE('TRPSFPRV')
      STOP 601
 5620 CONTINUE
      IF((IOPN.EQ.0).OR.(IOPN.EQ.1)) GO TO 5630
      PRINT5602
 5602 FORMAT('0ERROR ON CONTROL INPUT CARD FOR IOPN-FIX THEN RESTART')
       CALL W3TAGE('TRPSFPRV')
      STOP 603
 5630 CONTINUE
      IF((INTAPE.GT.-1).AND.(INTAPE.LE.1)) GO TO 5640
      PRINT 5604
 5604 FORMAT('0ERROR ON CONTROL INPUT CARD FOR INTAPE-FIX THEN RESTART')
       CALL W3TAGE('TRPSFPRV')
      STOP 605
 5640 CONTINUE
      IF((ISWTCH.GT.-1).AND.(ISWTCH.LE.1)) GO TO 5650
      PRINT 5606
 5606 FORMAT('0ERROR ON CONTROL INPUT CARD FOR ISWTCH-FIX THEN RSTART')
       CALL W3TAGE('TRPSFPRV')
      STOP 607
 5650 CONTINUE
      DO 5651 IK=1,NOPCDS
      IF((INOPNA.NE.ICODES(1,IK)).OR.(INOPNB.NE.ICODES(2,IK))) GO TO
     15651
      INOP=ICODES(3,IK)
      INOPN1=JCODES(1,INOP)
      INOPN2=JCODES(2,INOP)
      GO TO 5654
 5651 CONTINUE
      PRINT 5652
 5652 FORMAT('0ERROR ON CONTROL INPUT CARD FOR INOPN-FIX THEN RESTART')
       CALL W3TAGE('TRPSFPRV')
      STOP 653
 5654 CONTINUE
      RETURN
      END
