C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    CONWND      READ SATWND CONTROL CARDS                         
C   PRGMMR: LARRY SAGER      ORG: W/NMCXX    DATE: 96-08-27                     
C                                                                               
C ABSTRACT: CONWND READS THE PROGRAM CONTROL CARD FILE FOR THE  
C           REGION TO DUMP, PARAMETERS TO PROCESS AND MANDATORY
C           LEVELS TO PROCESS.
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   96-08-16  LARRY SAGER                                                       
C                                                                               
C USAGE:    CALL CONWND  (COUT, RIMS, IRET)                 
C   INPUT ARGUMENT LIST:                                                        
C                                                                               
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)                          
C     COUT     - ARRAY TO HOLD FILE HEADER                                      
C     RIMS     - GEOGRAPHIC AREA TO DUMP
C     IRET     - RETRUN CODE
C                                                                               
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)                     
C     FT22F001 - CONTROL CARD FILE                                              
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
      SUBROUTINE CONWND (COUT, RIMS, IRET)
C     
C     This subroutine reads the control cards and forms the
C     header record for SATWND data
C
      CHARACTER*8   COUT (512)
      CHARACTER*8   CBUFR(50)
      CHARACTER*8   CLVLS(25)
      CHARACTER*8   CSHD, CEHD, CEBF, CSLV  
      CHARACTER*8   CELV, CENB, CSTR, CSUA, CSAR 
      CHARACTER*8   CFLN
      CHARACTER*8   CTEMP
C
      REAL*8	    RIMS (4)
C
      EQUIVALENCE (CTEMP,ITEMP)
C
      DATA INP     /23/
      DATA CSHD    /'STR_HEAD'/
      DATA CEHD    /'END_HEAD'/
      DATA CSUA    /'STR_BUFR'/
      DATA CSAR    /'STR_ARCF'/
      DATA CEBF    /'END_BUFR'/
      DATA CSLV    /'STR_LVLS'/
      DATA CELV    /'END_LVLS'/
      DATA CENB    /'ENDBLOCK'/
      DATA CSTR    /'STR_REPO'/
C
C     READ IN THE FIRST CONTROL CARD: FILE TYPE 
C
      RIMS (1) = 0.
      RIMS (2) = 0.
      RIMS (3) = 0.
      RIMS (4) = 0.
      IRET = 0
      READ(INP,100,END=80) CFLN
 100  FORMAT(A8)
      COUT(1) = CSHD
      COUT(2) = CFLN
      COUT(3) = '99999999'
      COUT(4) = '99999999'
      COUT(5) = '99999999'
      COUT(6) = CEHD
      COUT(7) = CSUA
      IPAR = 1
      K = 0
C
C     READ IN THE SECOND C.C.:  LAT/LON AREA TO PROCESS
C    1- MOST N LAT  2- MOST S LAT  3- MOST W LON 4- MOST E LON
C
      IND = 8
      READ(INP,104,END=80) RIMS
 104  FORMAT(4F6.0)   
      PRINT *,' LIMITS: ',RIMS
      DO WHILE (K .EQ. 0)
	    READ(INP,100) CBUFR(IPAR)
            IF (CBUFR(IPAR) .EQ. '********') THEN
   	       K = 1
   	       IPAR = IPAR - 1
	    ELSE
               IND = IND + 1
	       COUT (IND) = CBUFR(IPAR)
               IPAR = IPAR  + 1
	    END IF
      END DO
      IF ( IPAR .EQ. 0 ) THEN
  	    PRINT *,' NO BUFR PARAMETERS FOUND-JOB STOPPED'
  	    IRET = -1
  	    RETURN
      END IF
      IND = IND + 1
      COUT (IND) = CEBF
      IND = IND + 1
      COUT (IND) = CENB
      IND = IND + 1
C
C        STORE THE START OF DATA POINTER
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
