C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    CONSAT      READ PROGRAM CONTROL CARDS                         
C   PRGMMR: LARRY SAGER      ORG: W/NMCXX    DATE: 96-08-27                     
C                                                                               
C ABSTRACT: CONTRL READS THE PROGRAM CONTROL CARD FILE FOR THE  
C           REGION TO DUMP, PARAMETERS TO PROCESS AND MANDATORY
C           LEVELS TO PROCESS.
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   96-08-16  LARRY SAGER                                                       
C                                                                               
C USAGE:    CALL CONTRL  (COUT, RLIMS, IRET)                
C   INPUT ARGUMENT LIST:                                                        
C                                                                               
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)                          
C     COUT     - ARRAY TO HOLD FILE HEADER                                      
C     RLIMS    - LIMITS OF DATA
C     IRET     - SWITCH  IRET = 0 DO TIROS
C                        IRET = 1 DELETE TIROS
C                                                                               
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)                     
C     FTR24F001 - CONTROL CARD FILE                                              
C                                                                               
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)                    
C                                                                               
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION                     
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: CRAY CFT77 FORTRAN                                                
C   MACHINE:  CRAY4                                                             
C                                                                               
C$$$                                                                            
      SUBROUTINE CONSAT (COUT, RLIMS, IRET)          
C     
C     This subroutine reads the control cards and forms the
C     header record for UPAUPA
C
      CHARACTER*8   COUT (512)
      CHARACTER*8   SRE,ERE
      CHARACTER*8   CBUFR(50)
      CHARACTER*8   CLVLS(25)
      CHARACTER*8   CSHD, CEHD, CEBF, CSLV, CSBF  
      CHARACTER*8   CELV, CENB, CSTR 
      CHARACTER*8   CFLN
      CHARACTER*8   CTEMP
      CHARACTER*2   CTIR 
C
      REAL*8        RLIMS(4)
C
      EQUIVALENCE (CTEMP,ITEMP)
C
      DATA INP     /24/
      DATA CSHD    /'STR_HEAD'/
      DATA CEHD    /'END_HEAD'/
      DATA CSBF    /'STR_BUFR'/
      DATA CEBF    /'END_BUFR'/
      DATA CSLV    /'STR_LVLS'/
      DATA CELV    /'END_LVLS'/
      DATA CENB    /'ENDBLOCK'/
      DATA CSTR    /'STR_REPO'/
C
C     READ IN THE FIRST CONTROL CARD: FILE TYPE 
C
      IRET = 0
      READ(INP,100,END=80) CFLN,CTIR    
 100  FORMAT(A8,15X,A2)
      IF (CTIR .NE. 'ON') IRET = 1
      COUT(1) = CSHD
      COUT(2) = CFLN
      COUT(3) = '99999999'
      COUT(4) = '99999999'
      COUT(5) = '99999999'
      COUT(6) = CEHD
      COUT(7) = CSBF
C
C     READ IN THE SECOND C.C.:  REGION TO PROCESS           
C
      READ(INP,102,ERR=33,END=80) SRE, ERE, RLIMS
 102  FORMAT(2A8,4F5.0)    
 33   ISWR = 0
      CTEMP = SRE
      ITEMP = ISHFT(ISHFT(ITEMP,48),-48)
      SRE = CTEMP
      CTEMP = ERE
      ITEMP = ISHFT(ISHFT(ITEMP,48),-48)
      ERE = CTEMP
       PRINT *,'SRE ERE ',SRE,ERE
      PRINT *,' RLIMS ',RLIMS(1),RLIMS(2),RLIMS(3),RLIMS(4)
C	
C     READ IN THE BUFR PARAMETERS TO PROCESS
C
      K = 0
      IPAR = 1
      DO WHILE (K .EQ. 0)
	READ(INP,100) CBUFR(IPAR)
        IF (CBUFR(IPAR) .EQ. '********') THEN
   	   K = 1
   	   IPAR = IPAR - 1
	ELSE
	   COUT (7+IPAR) = CBUFR(IPAR)
	   IPAR = IPAR + 1
	END IF
      END DO
      IF ( IPAR .EQ. 0 ) THEN
  	PRINT *,' NO BUFR PARAMETERS FOUND-JOB STOPPED'
  	IRET = -1
  	RETURN
      END IF
      IND = 8 + IPAR
      COUT (IND) = CEBF
      IND = IND + 1
      COUT (IND) = CSLV 
C
C     READ IN THE LEVELS TO PROCESS
C
      K = 0
      ILVL = 1
      DO WHILE (K .EQ. 0)
	READ(INP,100) CLVLS(ILVL)
        IF (CLVLS(ILVL) .EQ. '********') THEN
   	   K = 1
   	   ILVL = ILVL - 1
	ELSE
	   COUT(IND+ILVL) = CLVLS(ILVL)	
	   ILVL = ILVL + 1
	END IF
      END DO
      IF ( ILVL .EQ. 0 ) THEN
  	PRINT *,' NO LVLS FOUND-JOB STOPPED'
  	IRET = -1
  	RETURN
      END IF
      IND = IND + ILVL + 1
      COUT (IND) = CELV
      IND = IND + 1
      COUT (IND) = CENB
      IND = IND + 1
C
C     STORE THE START OF DATA POINTER
C 
      ITEMP = IND
      COUT (5) = CTEMP
      RETURN
C
C     ERROR -- END OF FILE ENCOUNTERED WHERE DATA CARD 
C     SHOULD HAVE BEEN.
C
 80   PRINT *,'  ERROR READING CONTROL CARDS'
      IRET = -1
      RETURN
      END
