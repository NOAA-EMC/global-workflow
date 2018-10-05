      SUBROUTINE CLOSEF(Z,Z1,IMAX,JMAX,S,A,B,M,IX,JY,LPLMI,IFF,
     1        IUP,LOX,ITABMB,ITABFL,MXITR)
C                                                                       00217400
C     ...THIS SUBROUTINE FINDS ALL OF THE CONTOUR POSITIONS IN FIELD Z  00217500
C     ...ALONG THE FIXED JY STRIP AND THEN USES THE POSITIONS FOR EACH  00217600
C     ...TO FIND THE CORRESPONDING POSITION AND VALUE IN FIELD Z1,      00217700
C     ...THESE VALUES ARE FORMATTED AND STORED IN THE LIST ARRAY FOR    00217800
C     ...PROCESSING AT CNTR TIME.                                       00217900
C                                                                       00218000
      COMMON/PUTARG/PUTHGT,PUTANG,IPRPUT,ITAPUT
      COMMON/ADJ4/IRTCOR,IUPCOR
      DIMENSION Z(IMAX,JMAX)
      DIMENSION Z1(IMAX,JMAX)
      DIMENSION IFF(5)
      DIMENSION ITEXT(3),JTEXT(3)
      character*8 citext(3),cjtext(3)
      equivalence(citext,itext)
      equivalence (cjtext,jtext) 
      DIMENSION ITABMB(MXITR),ITABFL(MXITR)
      REAL   IDEF,KDEF1,KDEF2,KDEF3,KDEF4,INDEF
      DATA    INDEF/Z'7FFFFFFFFFFFFFFF'/
      DATA    ITEXT/3*0/
      DATA    IKCIR/-10/
      DATA    JKCIR/-2/
      DATA    IBOX/2H)$/
    
       character*8 cibox
      equivalence(cibox,ibox)
C     ...WHERE ITABMB CONTAINS THE MATCH VALUE AND ITABFL THE LABEL ITSF00219400
      print *,' 9999 ENTERING CLOSEF'
      KCON4=20
      JMIN=1
    6 KBEG=IX
      KLIM=IX+IUP
      IJFIX=JY
   10 Q=S*20.
      REM=1.
      KCON6=FLOAT(IJFIX-1)*Q*3.0
C     ...WHERE KCON6 IS TRUE LOCN (IN DOTS) OF THE PT ON FIXED COORD.   00220300
      JCAL1=KCON6+IUPCOR
C     ...THIS JCAL1 IS IDOTS FOR PUTLAB FOR CONST J CASE, BUT NOT FOR I 00220500
      Q1=1./(2.*Q*Q)
      Q2=(Q+1.)/2.
      Q3=1./Q
      N=10
      KLAST=0
      KSTART=KBEG
      DO 11 K=KBEG,KLIM
      IDEF=Z(K,IJFIX)
      IF(IDEF.NE.INDEF) GO TO 12
      KSTART=KSTART+1
   11 CONTINUE
      GO TO 500
   12 L=Z(KSTART,IJFIX)+10000.
   20 DO 200 K=KSTART,KLIM
      Q4=REM/2.
      Q5=1.-REM
      X=Q-Q5
      IINC=X
      REM=X-FLOAT(IINC)
C                                                                       00222500
C     ...CHECK FOR IMBEDDED GRID                                        00222600
C                                                                       00222700
      KDEF1=Z(K,IJFIX)
      KDEF2=Z(K+1,IJFIX)
      KDEF3=Z(K-1,IJFIX)
      KDEF4=Z(K+2,IJFIX)
      YD2=0
      IF(KDEF1.EQ.INDEF) GO TO 21
      IF(KDEF2.EQ.INDEF) GO TO 21
      IF(KDEF3.EQ.INDEF) GO TO 21
      IF(KDEF4.EQ.INDEF) GO TO 21
      YD2=(Z(K+2,IJFIX)-Z(K+1,IJFIX)-Z(K,IJFIX)+Z(K-1,IJFIX))*Q1
   21 IF(KDEF1.EQ.INDEF) GO TO 200
      IF(KDEF2.EQ.INDEF) GO TO 200
      YD1=(Z(K+1,IJFIX)-Z(K,IJFIX))*Q3-(Q2*YD2)+Q5*YD2
      YVAL=Z(K,IJFIX)+Q5*Q4*YD1+10000.
   35 DO 130 KK=1,IINC
      MM=YVAL
      IF((MM-L).EQ.0) GO TO 125
C                                                                       00224500
C     ...COMPUTE A LABEL                                                00224600
C                                                                       00224700
      XVAL=MM-10000
      IF((IABS(MM)-IABS(L)).LT.0) XVAL=L-10000
      KCON1=(KK-1)*3
      KCON7=FLOAT(K-1)*Q*3.0
      ICAL=KCON7+KCON1+IRTCOR
      ILLCIR=ICAL+IKCIR
      JLLCIR=JCAL1+JKCIR
C                                                                       00225500
C     ...CALCULATE POSITION WHERE CONTOUR IN Z FIELD CUTS I GRID, WHERE 00225600
C     ...I CONTOUR CUT=I+IDELI                                          00225700
C     ...ADJUST FIELD Z1 VALUE USING STIRLING INTERPOLATION SOLVE FOR   00225800
C     ...DELI ONLY SINCE DELJ IS FIXED BECAUSE J IS FIXED ON AN INTEGRAL00225900
C     ...VALUE, THAT IS J=1,JMAX                                        00226000
C                                                                       00226100
      XKK=KK
      XIINC=IINC
      DELI=XKK/XIINC
      I=K
      J=IJFIX
      ZDELI=Z1(I,J)+0.5*DELI*(Z1(I+1,J)-Z1(I-1,J))+0.5*DELI*DELI*
     1(Z1(I+1,J)-2.0*(Z1(I,J))+Z1(I-1,J))
      HOLD=(ZDELI+A)*B
      ITEXT(1)=SIGN((ABS(HOLD)+0.5),HOLD)
      KPOSX=KCON4
   50 IF((ICAL-KLAST).LT.KPOSX) GO TO 125
      IF(LOX.EQ.1) GO TO 109
C     ...WHICH BRANCHES TO INDIRECT LABELS IF LOX                       00227400
      KLAST=ICAL
   55 CONTINUE
   75 CONTINUE
      INTG=ITEXT(1)
      NCHAR=M
C                                                                       00228000
C     ...FORMAT STRIP LABEL FROM CENTER POSITION                        00228100
C                                                                       00228200
   90 CONTINUE
c      CALL BIN2EB(INTG,JTEXT,NCHAR,LPLMI)
      CALL BIN2EB(INTG,cJTEXT,NCHAR,LPLMI)
   95 CONTINUE
      N=12
      CALL ENCODE(ITEXT(1),N)
      WRITE(99,IFF) JTEXT(1)
      HT=3.0
      ANGLE=90.
      IPR=1
c      CALL PUTLAB(ICAL,JCAL1,HT,ITEXT,ANGLE,N,IPR,ITAPUT)
      CALL lPUTLAB(ICAL,JCAL1,HT,cITEXT,ANGLE,N,IPR,ITAPUT)
      HT=10.0
      ANGLE=0.
      NCHAR=2
c      CALL PUTLAB(ILLCIR,JLLCIR,HT,IBOX,ANGLE,NCHAR,IPRPUT,ITAPUT)
      CALL lPUTLAB(ILLCIR,JLLCIR,HT,cIBOX,ANGLE,NCHAR,IPRPUT,ITAPUT)
      GO TO 125
C                                                                       00229800
C     ...USE INDIRECT LABELS                                            00229900
C                                                                       00230000
  109 CONTINUE
      DO 110 ITR=1,MXITR
      ITSAVE=ITR
      IF(ITABMB(ITR).EQ.ITEXT(1)) GO TO 112
  110 CONTINUE
      GO TO 125
  112 KLAST=ICAL
      ITEXT(1)=ITABFL(ITSAVE)
      INTG=ITEXT(1)
      NCHAR=M
C                                                                       00231100
C     ...FORMAT STRIP LABEL FROM CENTER POSITION                        00231200
C                                                                       00231300
  114 CONTINUE
      CALL BIN2EB(INTG,cJTEXT,NCHAR,LPLMI)
  115 CONTINUE
      N=12
      CALL ENCODE(ITEXT(1),N)
      WRITE(99,IFF)JTEXT(1)
      HT=3.0
      ANGLE=90.
      IPR=1
c      CALL PUTLAB(ICAL,JCAL1,HT,cITEXT,ANGLE,N,IPR,ITAPUT)
      CALL lPUTLAB(ICAL,JCAL1,HT,ITEXT,ANGLE,N,IPR,ITAPUT)
      HT=10.0
      ANGLE=0.
      NCHAR=2
c      CALL PUTLAB(ILLCIR,JLLCIR,HT,IBOX,ANGLE,NCHAR,IPRPUT,ITAPUT)
      CALL lPUTLAB(ILLCIR,JLLCIR,HT,cIBOX,ANGLE,NCHAR,IPRPUT,ITAPUT)
  125 L=MM
      YD1=YD1+YD2
      YVAL=YVAL+YD1
  130 CONTINUE
  200 CONTINUE
  500 continue
      print *,'EXITING CLOSEF 9999'
      RETURN
      END
