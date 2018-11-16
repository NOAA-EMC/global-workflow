      SUBROUTINE CLOSET(Z,IMAX,JMAX,S,A,B,M,IUP,IX,JY,LPLMI,IFF,LOX,
     1   ITABMB,ITABFL,MXITR)
C                                                                       00326600
C     ...THIS SUBROUTINE FINDS THE NEAREST CONTOUR LABEL ABOVE A        00326700
C     ...FIXED I,J POSITION IN THE GIVEN ARRAY.  THIS FIXED POSITION    00326800
C     ...IS THE GRID POINT USED TO FIND THE VALID CENTER IN THE GIVEN   00326900
C     ...ARRAY                                                          00327000
C                                                                       00327100
      character*3 lplmi
       character*90 cifile
      COMMON/PUTARG/PUTHGT,PUTANG,IPRPUT,ITAPUT
      COMMON/ADJ5/IRTKOR,IUPKOR
      DIMENSION Z(IMAX,JMAX)
      DIMENSION ITABMB(MXITR),ITABFL(MXITR)
C     ...WHERE ITABMB CONTAINS THE MATCH VALUE AND ITABFL THE LABEL ITSF00327600
      DIMENSION IFF(5)
       character*40 ciff
      dimension ifform(5)
      equivalence (ciff,ifform)
      DIMENSION ITEXT(3),JTEXT(3)
      character*8 cjtext(3)
      equivalence (cjtext,jtext)
      character*8 ctext(3)
      character*12 ctext12
      equivalence (ctext,itext)
cccc      equivalence (ctext12,ctext)
      REAL   INDEF,IDEF,KDEF1,KDEF2,KDEF3,KDEF4
      DATA    INDEF/Z'7FFFFFFFFFFFFFFF'/
      DATA    ITEXT/3*0/
      print *,' ENTERING CLOSET 9999'
       print *,'S=',S
       print *,'A=',a
       print *,'b=',b
       print *,'m=',m
       print *,'iup=',iup
       print *,'ix    =',ix
       print *,'iy    =',jy
       print *,'lox=    =',lox
      do  1,k=1,5
 1     ifform(k)=iff(k)
      KCON4=20
      JMIN=1
      KBEG=IX
      KLIM=IX+IUP
      IJFIX=JY
      Q=S*20.
      REM=1.
      KCON6=FLOAT(IJFIX-1)*Q*3.0
C     ...WHERE KCON6 IS TRUE LOCN(IN DOTS) OF THE PT. ON FIXED COORD.   00329000
      JCAL1=KCON6+IUPKOR
C     ...THIS JCAL1 IS IDOTS FOR PUTLAB FOR CONST J CASE, BUT NOT FOR I 00329200
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
      DO 200 K=KSTART,KLIM
      Q4=REM/2.
      Q5=1.-REM
      X=Q-Q5
      IINC=X
      REM=X-FLOAT(IINC)
C                                                                       00331200
C     ...CHECK FOR IMBEDDED GRID                                        00331300
C                                                                       00331400
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
      DO 130 KK=1,IINC
      MM=YVAL
      IF((MM-L).EQ.0) GO TO 125
C                                                                       00333200
C     ...COMPUTE A LABEL                                                00333300
C                                                                       00333400
      XVAL=MM-10000
      IF((IABS(MM)-IABS(L)).LT.0) XVAL=L-10000
      KCON1=(KK-1)*3
      KCON7=FLOAT(K-1)*Q*3.0
      ICAL=KCON7+KCON1+IRTKOR
      HOLD=(XVAL+A)*B
      ITEXT(1)=SIGN((ABS(HOLD)+0.5),HOLD)
      KPOSX=KCON4
      IF((ICAL-KLAST).LT.KPOSX) GO TO 125
      IF(LOX.EQ.1) GO TO 109
      KLAST=ICAL
   55 CONTINUE
   75 CONTINUE
      INTG=ITEXT(1)
      NCHAR=M
C                                                                       00335000
C     ...FORMAT STRIP LABEL FROM CENTER POSITION                        00335100
C                                                                       00335200
   90 CONTINUE
c      CALL BIN2EB(INTG,CJTEXT,NCHAR,'I99')
      CALL BIN2EB(INTG,CJTEXT,NCHAR,LPLMI)
   95 CONTINUE
      N=12
c      CALL ENCODE(ITEXT(1),N)
        write(cifile,1956) itext(1)  
 1956   format(i3)
         Print *,' 9999 CIFF',ciff
         write(ctext12,ciff ) cifile
c      ctext12(1:8)=ctext(1)(1:8) 
      WRITE(99,IFF) JTEXT(1)
      print *,
     1 ' 9999 FROM CLOSET DIRECT LABEL nchar and INTG',nchar,cjtext
     1, intg
       print 1999,' nchar, intg, cjtext, ctext12,itext',  
     1 nchar,intg,cjtext,ctext12,itext 
 1999  format(a50,i5,1x,i20,1x,' cjtext > ',3a9,'ctext12 --> ',
     1 a14,'itext --> ',3i4)
       N=3
      if(intg .lt. 100) N=2
      narg=3-N+1
      print *,' NARG ',narg,' ',N,' ' ,ctext12(narg:3) 
      print * ,' NARG ',' one ',ctext12(1:1),' two ',ctext12(2:2),     
     1 ' three ',ctext12(3:3),' four ',ctext12(4:4)
      CALL lPUTLAB(ICAL,JCAL1,22.0
     1,ctext12(narg:3),PUTANG,N,IPRPUT,ITAPUT)
       N=12
c      CALL lPUTLAB(ICAL,JCAL1,PUTHGT,ctext12,PUTANG,N,IPRPUT,ITAPUT)
c      CALL lPUTLAB(ICAL,JCAL1,PUTHGT,ITEXT,PUTANG,N,IPRPUT,ITAPUT)
c      CALL PUTLAB(ICAL,JCAL1,PUTHGT,ITEXT,PUTANG,N,IPRPUT,ITAPUT)
      GO TO 125
C                                                                       00336100
C     ...USE INDIRECT LABELS                                            00336200
C                                                                       00336300
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
C                                                                       00337400
C     ...FORMAT STRIP LABEL FROM CENTER POSITION                        00337500
C                                                                       00337600
  114 CONTINUE
      CALL BIN2EB(INTG,cJTEXT,NCHAR,LPLMI)
      stop 'bin2eb'
  115 CONTINUE
      N=12
      CALL ENCODE(ITEXT(1),N)
      ctext12(1:8)=ctext(1)(1:8) 
      WRITE(99,IFF)JTEXT(1)
      print *,
     1 ' 9999 FROM CLOSET inDIRECT LABEL nchar and INTG',nchar,cjtext
      CALL lPUTLAB(ICAL,JCAL1,PUTHGT,cTEXT12,PUTANG,N,IPRPUT,ITAPUT)
c      CALL lPUTLAB(ICAL,JCAL1,PUTHGT,ITEXT,PUTANG,N,IPRPUT,ITAPUT)
c      CALL PUTLAB(ICAL,JCAL1,PUTHGT,ITEXT,PUTANG,N,IPRPUT,ITAPUT)
  125 L=MM
      YD1=YD1+YD2
      YVAL=YVAL+YD1
  130 CONTINUE
  200 CONTINUE
  500 continue 
      print *,' EXITING CLOSET 9999'
      RETURN
      END
