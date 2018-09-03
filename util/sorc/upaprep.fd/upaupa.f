C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                          
C                .      .    .                                       .          
C MAIN PROGRAM: GRAPH_UPAUPA
C   PRGMMR: SAGER            ORG: NP12        DATE: 2000-01-18
C                                                                               
C ABSTRACT: UPAUPA READS IN OBSERVATIONAL RADIOSONDE UPPER AIR                  
C   REPORTS AND REFORMATS THE MANDATORY LEVEL DATA INTO A SIMPLE 
C   FLAT FILE FORMAT FOR INPUT INTO GRAPHICS CODES
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   99-07-27  LARRY SAGER   CONVERTED TO THE IBM SP
C   96-08-27  LARRY SAGER                                                       
C                                                                               
C USAGE:                                                                        
C   INPUT FILES:                                                                
C     FT10F001 - PREPBUFR UPPER AIR DATA FILE                                   
C     FT11F001 - 12HR OLD UPPER AIR DATA FILE(GRAPHICS FORMAT)                 
C                                                                               
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)                                    
C     FT55F001 - MANDATORY UPPER AIR DATA IN GRAPHICS FORMAT                    
C                                                                               
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)                
C     UNIQUE:    - REDHGT BRDHGT CONTRL CONAFT REDUPA FNDHGT 
C		   FORUPA       
C     LIBRARY:                                                                  
C       COMMON   - OPENBF UFBINT READSB READMG SBYTES                          
C       W3LIB    - W3UTCDAT                                                     
C       GRAPHICS - BIN2CH                                                       
C                                                                               
C   EXIT STATES:                                                                
C     COND =   0 - SUCCESSFUL RUN                                               
C                                                                               
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION                     
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY4                                            
C                                                                               
C$$$                                                                            
C
      CHARACTER*8   COUT (512)
      CHARACTER*8   CTUT (512)
      CHARACTER*8   CAFT (512)
      CHARACTER*8   CWND (512)
      CHARACTER*8   SRE, ERE
      CHARACTER*8   CTEMP, CDATE
      CHARACTER*40  ENDMSG
C
      INTEGER*8     IARR (300)
      INTEGER*8     ITME (8)
      DIMENSION     HDR (10)
C
      REAL          ARR (10,255)
      REAL*8        RHGTS (800,23)
      REAL*8        RLVLS(23)
      REAL*8        RLIMS(4)
      REAL*8        RLIMA(4)
C 
      EQUIVALENCE (CTEMP,ITEMP)
      EQUIVALENCE (RTEMP,ITEMP)
C
      DATA RHGTS   /18400*99999./
      DATA IREP    /0/
      DATA IUNU    /55/
      DATA IUNA    /56/
      DATA IUNW    /57/
      DATA IUNS    /58/
      DATA  IMISS /Z'7FFFF000'/  
      DATA FZER    /-.1/
      DATA FMAX    /24./
C
      CALL W3TAGB('GRAPH_UPAUPA',2000,0018,0082,'NP12')
C
C     CONTRL READS IN THE CONTROL CARDS AND FORMS THE HEADER 
C     RECORD FOR UPAUPA
C
      CALL CONTRL (COUT, SRE, ERE, RLIMS, ISWR, ICHG, IRET)
      PRINT *,' IBM SP VERSION   JULY 24, 1999'                  
C     PRINT *,' LIMITS: ',RLIMS(1),RLIMS(2),RLIMS(3),RLIMS(4)
      IF (IRET .NE. 0) THEN
  	PRINT *,' CONTROL CARDS ARE BAD--JOB STOPPED',IRET
	STOP
      END IF
      CALL CONAFT (CAFT, RLIMA, IRET)
C     PRINT 106,(CAFT(KKK),KKK=1,20)
 106  FORMAT(' AIRCR ',8a8,/,12a8)
      IF (IRET .NE. 0) THEN
  	PRINT *,' CONAFT CARDS ARE BAD-JOB STOPPED',IRET
	STOP
      END IF
      CALL CONWND (CWND, RLIMA, IRET)
      PRINT 107,(CWND(KKK),KKK=1,20)
 107  FORMAT(' SATW  ',8a8,/,12a8)
      IF (IRET .NE. 0) THEN
  	PRINT *,' CONWND CARDS ARE BAD-JOB STOPPED',IRET
	STOP
      END IF
C
C     GET THE 12-HR OLD HEIGHTS
C
      print *,'ichg is ',ichg
      IF(ICHG .EQ. 1) THEN
          CALL REDHGT (CTUT, IARR, RHGTS, IREP, IREH)
      END IF
C
C     SET THE INITIAL VARIABLES
C
      CTEMP = COUT(5)
      KNEXT = ITEMP
      CTEMP = CAFT(5)
      KNAXT = ITEMP
      CTEMP = CWND(5)
      KNWXT = ITEMP
      KNSXT = ITEMP
      READ(5,100) CDATE     
 100  FORMAT(2x,A8)    
      PRINT *,' date read is ',CDATE
      COUT(3) = CDATE
      CAFT(3) = CDATE
      CWND(3) = CDATE
      READ(CDATE(7:8),101) IHR
 101  FORMAT(I2)
C
C     READ THE CLOCK TO SET THE TIME
C
      CALL W3UTCDAT(ITME)      
      RTM = ITME(6)                     
      ITM = ITME(5) + RTM*1.66667
      CALL BIN2CH(ITM,CTEMP,4,'A99')
C     PRINT *,'  TIME FROM UTC  is ',CTEMP
      COUT(4) = CTEMP
      CAFT(4) = CTEMP
      CWND(4) = CTEMP
      IRET = 0
      IREPTS = 0
      IRAFT = 0
      IRWFT = 0
      IRSFT = 0
C
C     READ AND PROCESS THE UPPER AIR FILE IN BUFR
C
      ISTOP = 1
      DO WHILE (IRET .EQ. 0)
         DO K = 1,300
            ARR(K,1) = 99999.
         END DO
         IFLAG = 0
         CALL REDUPA (SRE, ERE, RLIMS, ISWR, HDR, ARR,
     1      ITYP,  NRET1, ITIR, IFLAG, IRET)
         IF (IRET .EQ. 0) THEN
           IF(ITYP .EQ. 1) THEN
C
C              UPPER AIR RADIOSONDE PROCESSING
C
  	       IST = 1
	       DO  K = 1,300
		   IARR(K) = IMISS  
 	       END DO
C
C	       FIND THE 12HR OLD DATA FOR THIS STATION
C
               DO I = 1,23
                  RLVLS(I) = 99999.
               END DO
               HDR(8) = IHR
               IF(ICHG .EQ. 1) THEN
 	         CALL FNDHGT (HDR, RHGTS, IREP, RLVLS, IREH)
               END IF
               KRET = 0
 	       CALL FORUPA (HDR, ARR, NRET1, IARR, KRET,
     1                RLVLS, IFLAG, IRET1)
	       IF (KRET .GT. 40 ) CALL DOSBYT (IARR, KRET,
     1            IUNU, COUT, KNEXT, IREPTS, ISTOP)
          END IF
          IF(ITYP .EQ. 2) THEN
	       DO  K = 1,300
		   IARR(K) = 0     
   	       END DO
C
C	       FORMAT THE AIRCRAFT DATA
C
               KRET = 0
               HDR(8) = IHR + HDR(4)
               IF(HDR(8) .LT. FZER) HDR(8) = FMAX + HDR(8)
               IF(HDR(8) .GT. FMAX) HDR(8) = HDR(8) - FMAX  
               CALL FORAFT(HDR, ARR, IARR, KRET, IRET1)
               IF (KRET .GT. 40 ) CALL DOSBYT (IARR, KRET,
     1            IUNA, CAFT, KNAXT, IRAFT, ISTOP)
          END IF 
          IF(ITYP .EQ. 3) THEN
	       DO  K = 1,300
		   IARR(K) = IMISS
   	       END DO
C
C	       FORMAT THE SATWND   DATA
C
               KRET = 0
               HDR(8) = IHR + HDR(4)
               IF(HDR(8) .LT. FZER) HDR(8) = FMAX + HDR(8)
               IF(HDR(8) .GT. FMAX) HDR(8) = HDR(8) - FMAX  
               CALL FORWND(HDR, ARR, IARR, KRET, IRET1)
               IF (KRET .GT. 40 ) CALL DOSBYT (IARR, KRET,
     1            IUNW, CWND, KNWXT, IRWFT, ISTOP)
           END IF 
         END IF
      END DO
C
C     DONE.  OUTPUT LAST BLOCK AND ISSUE CONSOLE MSGS
C
      ISTOP = 2
      CALL DOSBYT (IARR, KRET, IUNU, COUT, KNEXT, IREPTS, ISTOP)
      CALL DOSBYT (IARR, KRET, IUNA, CAFT, KNAXT, IRAFT , ISTOP)
      CALL DOSBYT (IARR, KRET, IUNW, CWND, KNWXT, IRWFT , ISTOP)
C
C----------------------------------------------------------------------|
C     THE END-OF-FILE.                                                 |
C----------------------------------------------------------------------|
C
      PRINT *,' '
      PRINT *,' TOTAL RADIOSONDE REPORTS:  ',IREPTS
      PRINT *,' TOTAL AIRCRAFT REPORTS  :  ',IRAFT
      PRINT *,' TOTAL SATWIND  REPORTS  :  ',IRWFT 
      PRINT *,' '
      CALL BIN2CH (IREPTS,CTEMP,4,'A99')
      ENDMSG = ' UPAPREP WRITES '//CTEMP//' REPORTS:' 
C     CALL CONSOL(ENDMSG)
      PRINT *, ' '
      PRINT *, ' NORMAL END OF JOB EXECUTED'
      ENDMSG = 'UPAPREP'        
      ISTOP = 0
      CALL W3TAGE('GRAPH_UPAUPA')
C
      STOP  
      END
