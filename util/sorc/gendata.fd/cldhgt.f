C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    CLDHGT      FORM THE GRAPHICS FORMAT.                          
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-09-27                     
C                                                                               
C ABSTRACT: CLDHGT INPUTS THE CLOUD HGT IN METERS AND RETURNS
C           THE WMO CODE FIGURE
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   96-09-27  LARRY SAGER                                                       
C                                                                               
C USAGE:    CALL CLDHGT  (ARR, IN)                                       
C   INPUT ARGUMENT LIST:                                                        
C     ARR      - CLOUD HGT IN METERS                           
C                                                                               
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)                          
C     IN       - WMO CLOUD HGT CODE FIGURE.                                     
C                                                                               
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION                     
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP                                              
C                                                                               
C$$$                                                                            
      SUBROUTINE CLDHGT (ARR, IN)
C
      IHGT = ARR
      IN = 9
      IF (IHGT .LT. 50) THEN
         IN = 0
      ELSE IF (IHGT .LT. 100) THEN
         IN = 1
      ELSE IF (IHGT .LT. 200) THEN
         IN = 2
      ELSE IF (IHGT .LT. 300) THEN
         IN = 3
      ELSE IF (IHGT .LT. 600) THEN
         IN = 4
      ELSE IF (IHGT .LT. 1000) THEN
         IN = 5
      ELSE IF (IHGT .LT. 1500) THEN
         IN = 6
      ELSE IF (IHGT .LT. 2000) THEN
         IN = 7
      ELSE IF (IHGT .LT. 2500) THEN
         IN = 8
      END IF             
C     
      RETURN
      END
