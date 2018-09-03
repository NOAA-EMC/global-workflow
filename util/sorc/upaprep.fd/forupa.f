C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    FORUPA      FORM THE GRAPHICS FORMAT.                          
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-08-27                     
C                                                                               
C ABSTRACT: FORUPA TAKES UNPACKED BUFR FORMAT AND CONVERTS IT 	                 
C   INTO A SIMPLE FORMAT FOR USE IN GRAPHICS PROGRAMS.                          
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   96-08-27  LARRY SAGER                                                       
C                                                                               
C USAGE:    CALL FORUPA  (HDR, ARR, NRET, IARR, KRET, RLVLS, IRETT1)            
C   INPUT ARGUMENT LIST:                                                        
C     HDR      - UNPACKED BUFR HEADING INFORMATION (ID, LAT/LON ...)            
C     ARR      - UNPACKED BUFR MANDATORY LEVEL DATA            
C     NRET     - NUMBER OF LEVELS IN THE BUFR REPORT.
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
      SUBROUTINE FORUPA (HDR, ARR,  NRET, IARR, KRET,
     1             RLVLS, IFLAG, IRET1)
C
C     THIS SUBROUTINE CONVERTS THE BUFR DATA AND STORES INTO
C       A FORMATED ARRAY.
C     
      REAL*8        ARR(10,255)
      REAL*8        RLVLS(23)
C
      CHARACTER*8   CTEMP
C
      INTEGER*8     IARR (300)
      INTEGER*8     ILEV (21)  
      DIMENSION     HDR (10)
C
      EQUIVALENCE   (CTEMP,ITEMP)
      EQUIVALENCE   (RTEMP,ITEMP)
C
      DATA  SHFL12/4096./
      DATA  IMISS /Z'7FFFF000'/
      DATA  KMISS /Z'20202020'/
      DATA  ILEV  / 1000, 925, 850, 700, 500, 400, 300, 250, 200,
     1             150, 100, 70, 50, 30, 20, 10, 7, 5, 3, 2, 1 /
C
C----------------------------------------------------------------------|
C     START BY SETTING DATA TYPE 
C----------------------------------------------------------------------|
C
      IRET1 = 0
      IARR (9) = 11*SHFL12
      IF(IFLAG .EQ. 1) IARR(9) = 31*SHFL12
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
      IDR = NINT(HDR(8) - HDR(4))
      IF(IDR .LT. 0) IDR = 24 + IDR     
      IARR(4) = IDR*SHFL12
          J = 0
          IDX = 35
          MIDX = IDX
	  IARR(14) = IDX*SHFL12
	  NLEV = 0
          MLEV = 0
          DO WHILE (J .LT. NRET)
             J = J + 1
C
C            MANDATORY LEVEL DATA BELOW 100. MB 
C
	     IF (ARR(1,J) .GT. 00. ) THEN
                  IPLV = NINT(ARR(1,J))
                  DO K = 1,21
                    IF(IPLV .EQ. ILEV(K)) THEN
                       IDX = 35 + (K-1)*7
                       NLEV = K - 1
                       GOTO 10
                    END IF
                  END DO
                  GO TO 30
C
C	          START WITH HEIGHTS AND 12HR HGT DIFFERENCES
C
  10		  IF (ARR(2,J).LT.99999.) THEN
	             IARR(IDX)=NINT(ARR(2,J)*SHFL12)
		     IF (RLVLS(NLEV+3) .LT. 99999.0) THEN
		        RDIF = ARR(2,J) - RLVLS(NLEV+3)
			IARR(IDX+5) = NINT(RDIF*SHFL12)
		     ELSE
			IARR(IDX+5) = IMISS
		     END IF
		  ELSE
		     IARR(IDX) = IMISS
		     IARR(IDX+5) = IMISS
		  END IF
C
C	          TEMPERATURE AND DEW POINT         
C
		  IF (ARR(3,J).LT.99999.) THEN
	             IARR(IDX+1)=NINT(ARR(3,J)*SHFL12)
	 	  ELSE
		     IARR(IDX+1)=IMISS
		  END IF
 		  IF (ARR(4,J).LT.99999.) THEN
	             IARR(IDX+2)=NINT(ARR(4,J)*SHFL12)
		  ELSE 
		     IARR(IDX+2)=IMISS
		  END IF
C
C	          STORE WIND SPEED AND DIRECTION
C
		  IF (ARR(5,J).LT.99999.) THEN
	             IARR(IDX+3) = NINT(ARR(5,J) * SHFL12)
		  ELSE
		     IARR(IDX+3) = IMISS
		  END IF
		  IF (ARR(6,J).LT.99999.) THEN
		     SPED = ARR(6,J)
	             IARR(IDX+4) = NINT(SPED*SHFL12)
		  ELSE
		     IARR(IDX+4) = IMISS
		  END IF
C
C		  QUALITY MARKS
C
                  IA =  9
                  IB = 99
                  IC = 99
                  IF (ARR(7,J) .LT. 1000.) IA = ARR(7,J)
                  IF (ARR(8,J) .LT. 1000.) IB = ARR(8,J)
                  IF (ARR(9,J) .LT. 1000.) IC = ARR(9,J)
                
                  IARR(IDX+6) = (IA*10000 + IB*100 + IC)*SHFL12    
C                 PRINT *,' MARKS ',IARR(IDX+6)
C
           	  NLEV = NLEV + 1
                  IF(NLEV .GT. MLEV) MLEV = NLEV
                  IDX = IDX + 7
	          IF(IDX .GT. MIDX) MIDX  = IDX 
 30               CONTINUE
	    END IF
          END DO  
C
C         STORE THE NUMBER OF LEVELS IN THIS REPORT
C
	  KRET = MIDX
          IARR(13) = MLEV * SHFL12
C
      RETURN
      END
