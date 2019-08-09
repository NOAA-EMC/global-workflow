      SUBROUTINE FAXSHD(IAREA,IA,JA,INDEX,INCR1,INCR,JLAST,NNN)
      COMMON/NSCHED/ISLOTA,IPANA,ISUB,IFLAB,INSET,IRLAB,ISCHED(8,50)
      common/nsched2/jsched
      DIMENSION IAREA(IA,JA)
      DIMENSION IISUB(11)
      DIMENSION ISUBA(3)
      DIMENSION ISUBB(3)
      INTEGER   ISCHED
      character*1 JSCHED(16,50)
c      LOGICAL   JSCHED(16,50)                                          
c      EQUIVALENCE (ISCHED(1,1),JSCHED(1,1))
      DATA    IISUB/1H ,2HP1,2HP2,2HP3,2HP4,2HP5,2HP6,2HI1,2HI2,2HI3,
     1   2HB2/
      DATA    ISUBA/3HP1A,3HP3A,3HP5A/
      DATA    ISUBB/2HP1,2HP3,2HP5/
      DATA    IBCHK/4H    /
      DATA    ITBSE/7400/
      DATA    MAPT7/Z'E5000000'/
      DATA    MASK2/Z'FF000000'/
C                                                                       
C     COMMON AREA VARIABLES  /NSCHED/                                   
C                                                                       
C     WHERE ISLOTA,B= FAX SLOT NUMBER (CARD IMAGE FORMAT)               
C     WHERE IPANA,B= FAX PANEL INDICATOR (CARD IMAGE FORMAT)            
C     WHERE ISUB= FAX SUBSET NO. (CARD IMAGE FORMAT)                    
C     WHERE IFLAB= FRONT LABEL INSET NO. (CARD IMAGE FORMAT)            
C     WHERE INSET= REAL INSET NO. (CARD IMAGE FORMAT)                   
C     WHERE IRLAB= REAR LABEL INSET NO. (CARD IMAGE FORMAT)             
C     WHERE ISCHED= FAX/VARIAN SCHEDULE CONTROL                         
C                                                                       
C     CALL SEQUENCE VARIABLES                                           
C     IAREA= ARRAY FOR ALL FAX/VARIAN AREAS                             
C     IA,JA= I,J INDEX FOR IAREA ARRAY                                  
C     INDEX= SPECIFIC IAREA INDEX TO BE USED FOR THIS CUT               
C     INCR1= OUTPUT SCAN LINE J FOR SPECIFIC AREA USED                  
C            (NEEDED FOR LABEL ARRAY LINKAGE TO CNTR)                   
C                                                                       
C                                                                       
C     INTERNAL SUBROUTINE VARIABLES                                     
C                                                                       
C     WHERE IFAXT=0  VARIAN MAP                                         
C                "1  WHOLE FAX MAP(NO REAL INSET)                       
C                =2  WHOLE FAX MAP(REAL INSET(S))                       
C                =3 LEFT PANEL MAP                                      
C                =4  RIGHT PANEL MAP                                    
C                =5  REAL INSET                                         
C                                                                       
C     WHERE INUML=0  NO STRIP LABELS                                    
C                =1  FRONT LABEL ONLY OR FRONT OR REAR LABEL            
C                =2  FRONT AND REAR LABEL                               
C     INITIALIZE FOR SPECIAL MAP TYPE                                   
      print *,' ENTERING FAXSCH'
      NNN=NNN+1
      IPANY=0
      IVARY=0
      IVAR=LAND(ISLOTA,MASK2)
      IF(IVAR.EQ.MAPT7) IVARY=1
      IF((IPANA.NE.IISUB(8)).OR.(IPANA.NE.IISUB(9)).OR.
     1   (IPANA.NE.IISUB(10)).OR.(IPANA.NE.IBCHK)) IPANY=1
      IFAXT=0
      IF(IVARY.EQ.1) IFAXT=0
      IF((IPANA.EQ.IBCHK).AND.(INSET.LT.1).AND.(IVARY.EQ.0)) IFAXT=1
      IF((IPANA.EQ.IBCHK).AND.(INSET.GE.1).AND.(IVARY.EQ.0)) IFAXT=2
      IF((IPANA.EQ.IISUB(2)).OR.(IPANA.EQ.IISUB(4)).OR.
     1   (IPANA.EQ.IISUB(6))) IFAXT=3
      IF((IPANA.EQ.IISUB(3)).OR.(IPANA.EQ.IISUB(5)).OR.
     1   (IPANA.EQ.IISUB(7)).OR.(IPANA.EQ.IISUB(11))) IFAXT=4
      IF((IPANA.EQ.IISUB(8)).OR.(IPANA.EQ.IISUB(9)).OR.
     1   (IPANA.EQ.IISUB(10))) IFAXT=5
      INUML=0
      IF((IFLAB.LT.1).AND.(IRLAB.LT.1)) INUML=0
      IF((IFLAB.GE.1).AND.(IRLAB.LT.1)) INUML=1
      IF((IFLAB.GE.1).AND.(IRLAB.GE.1)) INUML=2
      IF(INCR1.NE.0) GO TO 15
      JLAST=ITBSE-INCR
   15 CONTINUE
      ISCHED(1,NNN)=ISUB
      ISCHED(2,NNN)=IAREA(1,INDEX)
      ISCHED(3,NNN)=0
      ISCHED(4,NNN)=IAREA(2,INDEX)
      JSCHED(9,NNN)=char(IAREA(3,INDEX)    )
      call j2i(9,nnn)
      JSCHED(10,NNN)=char(IAREA(4,INDEX)   )
      JSCHED(11,NNN)=char(IAREA(5,INDEX)        )
      call j2i(11,nnn)
      call j2i(10,nnn)
      IF((IFAXT.EQ.0).OR.((IFAXT.EQ.1).AND.(INUML.EQ.0))) GO TO 1426
      GO TO 1427
C                                                                       
C     FOUND VARIAN MAP OR WHOLE FAX MAP(NO REAL INSET)                  
C     AND NO STRIP LABELS                                               
C                                                                       
 1426 CONTINUE
       print *, 'j2i call after 1426'
      JSCHED(12,NNN)=char(0)
      call j2i(12,nnn)
      ISCHED(7,NNN)= 0
      ISCHED(8,NNN)=0
      GO TO 1450
 1427 CONTINUE
      GO TO (1428,1428,1432,1434,1436),IFAXT
C                                                                       
C     FOUND WHOLE MAP (NO REAL INSET) OR                                
C     FOUND WHOLE MAP (REAL INSET(S))                                   
 1428 CONTINUE
      IF((IFAXT.EQ.2).AND.(INUML.EQ.0)) GO TO 1430
C                                                                       
C     PREPARE FOR FRONT STRIP LABEL                                     
C                                                                       
       print *, 'j2i call  2 after front strip'
      JSCHED(12,NNN)=char(0)
      call j2i(12,nnn)
      ISCHED(7,NNN)=-30
      ISCHED(8,NNN)=IFLAB
      JLAST=JLAST+INCR
      JXL=JLAST
      NNN=NNN+1
      ISCHED(1,NNN)=IFLAB
      ISCHED(2,NNN)=JXL
      ISCHED(3,NNN)=0
      ISCHED(4,NNN)=INCR
      IFLW2=216
      IF(INUML.EQ.2) GO TO 1429
C                                                                       
C     FOUND WHOLE MAP FRONT LABEL ONLY                                  
C                                                                       
      JSCHED(11,NNN)=char(IFLW2)
      call j2i(11,nnn)
      ISCHED(5,NNN)=0
      JSCHED(12,NNN)=char(0 )
       print *, 'j2i call  3 '
      call j2i(12,nnn)
      ISCHED(7,NNN)=0
      ISCHED(8,NNN)=0
      GO TO 1450
C                                                                       
C     FOUND FRONT AND REAR STRIP LABEL                                  
C                                                                       
 1429 CONTINUE
      IFLW3=1
      IF(IFAXT.EQ.2) GO TO 14292
      JSCHED(9,NNN)=char(IFLW3       )
      JSCHED(10,NNN)=char(0)
      JSCHED(11,NNN)=char(IFLW2)
      JSCHED(12,NNN)=char(0 )
       print *, 'j2i call  4 '
      call j2i(10,nnn)
      call j2i(11,nnn)
      call j2i(12,nnn)
      call j2i(9,nnn)
      ISCHED(7,NNN)=IAREA(2,INDEX)
      ISCHED(8,NNN)=IRLAB
      IWORK1=IRLAB
      JLAST=JLAST+INCR
      JXL=JLAST
14291 CONTINUE
      NNN=NNN+1
      ISCHED(1,NNN)=IWORK1
      ISCHED(2,NNN)=JXL
      ISCHED(3,NNN)=0
      ISCHED(4,NNN)=INCR
      IRLW2=216
      JSCHED(11,NNN)=char(IRLW2)
      call j2i(11,nnn)
      ISCHED(5,NNN)=0
      JSCHED(12,NNN)=char(0)
       print *, 'j2i call  5 '
      call j2i(12,nnn)
      ISCHED(7,NNN)=0
      ISCHED(8,NNN)=0
      GO TO 1450
C                                                                       
C     FOUND REAL INSET                                                  
C                                                                       
14292 CONTINUE
      JSCHED(9,NNN)=char(IFLW3)
      JSCHED(10,NNN)=char(0)
      JSCHED(11,NNN)=char(IFLW2)
      JSCHED(12,NNN)=char(IAREA(6,INDEX)    )
       print *, 'j2i call  6 '
      call j2i(10,nnn)
      call j2i(11,nnn)
      call j2i(12,nnn)
      call j2i(9,nnn)
      ISCHED(7,NNN)=IAREA(7,INDEX)
      ISCHED(8,NNN)=INSET
      GO TO 1450
C                                                                       
C     FOUND WHOLE MAP (REAL INSET(S)) BUT NO STRIP LABELS               
C                                                                       
 1430 CONTINUE
      GO TO 1450
C                                                                       
C     FOUND LEFT PANEL MAP                                              
C                                                                       
 1432 CONTINUE
       print *, 'j2i call  7 '
      JSCHED(12,NNN)=char(0)
      call j2i(12,nnn)
      ISCHED(7,NNN)=-30
      ISCHED(8,NNN)=IFLAB
      JLAST=JLAST+INCR
      JXL=JLAST
      NNN=NNN+1
      ISCHED(1,NNN)=IFLAB
      ISCHED(2,NNN)=JXL
      ISCHED(3,NNN)=0
      ISCHED(4,NNN)=INCR
      JSCHED(9,NNN)=char(1)
      JSCHED(10,NNN)=char(0)
      JSCHED(11,NNN)=char(216 )
      JSCHED(12,NNN)=char(IAREA(5,INDEX)   )
       print *, 'j2i call  8 '
      call j2i(10,nnn)
      call j2i(11,nnn)
      call j2i(12,nnn)
      call j2i(9,nnn)
      ISCHED(7,NNN)=0
      ISCHED(8,NNN)=INSET
      IWORK1=IRLAB
      JLAST=JLAST+INCR
      JXL=JLAST
      GO TO 14291
C                                                                       
C     FOUND RIGHT PANEL MAP                                             
C                                                                       
 1434 CONTINUE
      JSCHED(9,NNN)=char(69)
      call j2i(9,nnn)
      IF(IPANA.EQ.IISUB(11)) JSCHED(9,NNN)= char(65)
      ISCHED(7,NNN)=IAREA(2,INDEX)
      JSCHED(12,NNN)=char(0)
       print *, 'j2i call  9 '
      call j2i(12,nnn)
      ISCHED(8,NNN)=IFLAB
      GO TO 1450
C                                                                       
C     FOUND REAL INSET                                                  
C                                                                       
 1436 CONTINUE
      JSCHED(12,NNN)=char(0)
       print *, 'j2i call  a '
      call j2i(12,nnn)
      ISCHED(7,NNN)=IAREA(8,INDEX)
      ISCHED(8,NNN)=IFLAB
      IWORK1=IFLAB
      JLAST=JLAST+INCR
      JXL=JLAST
      GO TO 14291
 1450 CONTINUE
      PRINT 250,IFAXT,INUML,INDEX,INCR1
  250 FORMAT('0TYPE MAP=  ',I5,'  TYPE LABELS  ',I5,'  AREA INDEX=  ',
     1   I5,'  LABEL ARRAY VALUE=  ',I5)
      RETURN
      END
