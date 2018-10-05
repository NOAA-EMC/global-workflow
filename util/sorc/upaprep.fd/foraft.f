C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    FORAFT      FORM THE GRAPHICS FORMAT.                          
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-09-10                     
C                                                                               
C ABSTRACT: FORUPA TAKES UNPACKED BUFR FORMAT AND CONVERTS IT 	                 
C   INTO A SIMPLE FORMAT FOR USE IN GRAPHICS PROGRAMS.                          
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   96-09-10  LARRY SAGER                                                       
C                                                                               
C USAGE:    CALL FORUPA  (HDR, ARR, IARR, KRET, IRETT1)            
C   INPUT ARGUMENT LIST:                                                        
C     HDR      - UNPACKED BUFR HEADING INFORMATION (ID, LAT/LON ...)            
C     ARR      - UNPACKED BUFR MANDATORY LEVEL DATA            
C                                                                               
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)                          
C     IARR     - OUTPUT IN GRAPHICS FORMAT.                                     
C     KRET     - POINTER TO STARTING LOCATION OF NEXT REPORT                    
C     IRET1    - RETURN CODE                                                    
C                                                                               
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION                     
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: CRAY CFT77 FORTRAN                      
C   MACHINE:  CRAY4                                              
C                                                                               
C$$$                                                                            
      SUBROUTINE FORAFT (HDR, ARR,  IARR, KRET, IRET1)
C
C     THIS SUBROUTINE CONVERTS THE BUFR DATA AND STORES INTO
C       A FORMATED ARRAY.
C     
      REAL*8        ARR(10,255)
C
      CHARACTER*8   CTEMP
C
      INTEGER*8     IARR (300)
      DIMENSION     HDR (10)
C
      EQUIVALENCE   (CTEMP,ITEMP)
      EQUIVALENCE   (RTEMP,ITEMP)
C
      DATA  SHFL12/4096./
      DATA  IMISS /Z'7FFFF000'/
      DATA  KMISS /Z'20202020'/
C
C----------------------------------------------------------------------|
C     START BY SETTING BUFR TYPE AND SUBTYPE                          |
C----------------------------------------------------------------------|
C
      IRET1 = 0
      IARR (9) = 41*SHFL12
      IARR (10)= 0        
      RTEMP = HDR (1)
      IARR(11) = ISHFT(ITEMP,-32)
      IARR(12) = ISHFT(ISHFT(ITEMP,32),-32 )              
      IARR(1)=NINT(HDR(3)*SHFL12)
      IARR(2)=NINT((360.0-HDR(2))*SHFL12)
      IARR(7)=NINT(HDR(5)*SHFL12)
      IARR(8)=NINT(HDR(6)*SHFL12)
C
C     STORE THE HOUR OF THIS REPORT
C
      IARR(4) = NINT(HDR(8)*SHFL12)
      J = 0
      IDX = 35
      IARR(15) = 2 * SHFL12
      IARR(16) = IDX*SHFL12
      J = 1
C
C     START WITH PRESSURE ALTITUDE                
C
      IF (ARR(6,J).LT.99999.) THEN
	   IARR(IDX)=NINT(ARR(6,J)*SHFL12 )
      ELSE
           IARR(IDX) = IMISS
      END IF
C
C     PRESSURE...
C
      IF (ARR(1,J) .LT. 99999.) THEN
           IARR(IDX+6) = NINT(ARR(1,J)*SHFL12)
      ELSE
           IARR(IDX+6) = IMISS
      END IF
C
C     TEMPERATURE AND DEW POINT         
C
      IF (ARR(2,J).LT.99999.) THEN
	   IARR(IDX+1)=NINT(ARR(2,J)*SHFL12)
      ELSE
           IARR(IDX+1)=IMISS
      END IF
      IF (ARR(3,J).LT.99999.) THEN
	   IARR(IDX+2)=NINT(ARR(3,J)*SHFL12)
      ELSE 
	   IARR(IDX+2)=IMISS
      END IF
C
C     STORE WIND SPEED AND DIRECTION
C
      IF (ARR(4,J).LT.99999.) THEN
	   IARR(IDX+3) = NINT(ARR(4,J) * SHFL12)
      ELSE
	   IARR(IDX+3) = IMISS
      END IF
      IF (ARR(5,J).LT.99999.) THEN
	   SPED = ARR(5,J)
           IARR(IDX+4) = NINT(SPED*SHFL12)
      ELSE
	   IARR(IDX+4) = IMISS
      END IF
C
C     QUALITY MARKS
C
      IF (ARR(7,J).LT.99999.) THEN
	   IARR(IDX+5)=NINT(ARR(7,J)*SHFL12)
      ELSE 
	   IARR(IDX+5)=IMISS
      END IF
C
C     UPDATE THE POINTER TO THE END OF THE REPORT
C
      IDX = IDX + 8
C
C     LOAD THE POINTER TO THE END OF THIS REPORT
C
      KRET = IDX
C
      RETURN
      END
