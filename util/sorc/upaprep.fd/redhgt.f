C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    REDHGT      READ 12HR OLD HEIGHTS     	
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-08-27
C
C ABSTRACT: REDHGT READS THE 12-HR OLD UPPER AIR FILE        
C   AND EXTRACTS THE HEIGHT DATA FOR USE IN THE 12HR HEIGHT
C   CHANGE CALCULATION.
C
C PROGRAM HISTORY LOG:
C   96-08-27  LARRY SAGER
C
C USAGE:    CALL REDHGT  (COUT, ARR, RHGTS IREP, IRET)                
C   INPUT ARGUMENT LIST:
C     COUT     - ARRAY SPACE TO HOLD 12HR OLD UPPER AIR DATA        
C     ARR      - ARRAY SPACE TO HOLD UPPER AIR REPORT
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     RHGTS    - 12HR OLD MANDATORY LEVEL HEIGHT DATA
C     IREP     - NUMBER OF REPORTS RETURNED                  
C     IRET     - RETURN CODE
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY4
C
C$$$

      SUBROUTINE REDHGT(COUT, ARR, RHGTS, IREP, IRET)
C
C     This subroutine reads the 12HR old upper air file in      
C     graphics format and returns the mandatory height data 
C     in an array.  
C
      CHARACTER*8   COUT (512)
      CHARACTER*8   CAR
      CHARACTER*8   CSTA
C
      INTEGER*8     IHAF(512)
C
      REAL*8        RHGTS(800,23)
      REAL*8        ARR (300)
      REAL*8        RAR
C
      DATA  KLIM   /800/
      DATA  IMASK  /Z'FFFFFFFF00000000'/
      DATA  IUNO   /11/
C
      EQUIVALENCE (CAR,IAR)
      EQUIVALENCE (CAR,RAR)
C
      KNEXT = 1 
C
C     READ THE UPAUPA DATA AND RETURN THE HGTS
C
      ITOT = 0
      NOTEOF = 0 
      IREC = 0
      DO WHILE (NOTEOF.EQ.0)
         READ  (IUNO,END=20) COUT
         IREC = IREC +1
         IF ( IREC .EQ. 1 ) THEN  
 	    CAR = COUT(5)
             call byteswap(CAR, 8, 1)
            KNEXT = IAR - 1
            KNEXT = KNEXT + 1
         END IF
         NEXREP  = 0
         DO WHILE (NEXREP.EQ.0)
            IF(COUT(KNEXT).EQ.'STR_REPO') THEN
               KNEXT = KNEXT + 1
               IF (KNEXT .GT. 512) THEN
                  KNEXT = 1 
                  READ(IUNO) COUT
               END IF
            ELSE IF(COUT(KNEXT) .EQ. 'ENDOFILE') THEN
	       IRET = 10
	       RETURN
            ELSE
               PRINT *,' STR_REPO NOT FOUND REDHGT'
               RETURN
            END IF
            IF(KNEXT .LT. 512) THEN
               CAR  = COUT(KNEXT+1)
               call byteswap(CAR, 8, 1)
               KEND = ISHFT(IAR,-32)/4096
               IF(KEND .EQ. 1) KEND = -2
               KEND = KEND/2
            ELSE
               KEND = 0
            END IF
            IF(KEND.LT.KNEXT) THEN
               CALL BRDHGT(KNEXT, KEND, COUT, IUNO, RHGTS,
     1           IREP, ARR, IREB)
               IF(IREB.EQ.10) THEN
	          NOTEOF = 1
	          NEXREP = 1
               ENDIF
            ELSE 
               J = 1
               ITOT = ITOT + 1
               DO  K = KNEXT,KEND 
                  CAR = COUT(K)
                  call byteswap(CAR, 8, 1) 
	          IAR1 = ISHFT(IAR,-32)
	          IA = ISHFT(IAR1,-28)
	          IF (IA .GT. 0) IAR1=IAR1+IMASK
                  IAR2 = ISHFT(ISHFT(IAR,32),-32)
	          IA = ISHFT(IAR2,-28)
	          IF (IA .GT. 0) IAR2 = IAR2+IMASK
	          FAR =  IAR1
	          FAR =  (FAR/4096.)
	          FAR2 = IAR2
                  FAR2 = (FAR2/4096.)
	          ARR(J) = FAR
                  ARR(J+1) = FAR2
                  J = J + 2
               END DO
               CSTA = COUT(KNEXT+5)
               IA = ARR(13)
               IB = ARR(14)
               IE = KEND + 1
               IF(IE.GT.512) THEN
                  IE = 1
                  READ(IUNO) COUT
               END IF
               IF(COUT(IE).EQ.'END_REPO') THEN
                  KNEXT = IE + 1
                  IF(KNEXT .GT. 512) THEN
                     KNEXT = 1
                     READ(IUNO) COUT
                  END IF  
               ELSE
                  PRINT *,' END REPO NOT FOUND REDHGT'
                  RETURN
               END IF
C
C              STORE THE HEIGHT
C
	       IF ((IA .GT. 0) .AND. (IREP .LT. KLIM)) THEN
                  IREP = IREP + 1
	          CAR = CSTA
		  RHGTS(IREP,1) = RAR
		  RHGTS(IREP,2) = ARR(4)
                  IND = IB           
		  DO K = 1,IA
		     RHGTS(IREP,K+2) = ARR(IND)
 		     IND = IND + 7
		  END DO
               END IF
            END IF
         END DO      
      END DO
C      
      RETURN
C
  20  PRINT *,' NO FILE FOUND'
      END

