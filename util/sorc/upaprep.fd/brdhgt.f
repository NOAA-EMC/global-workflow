C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    BRDHGT      FORM THE GRAPHICS FORMAT.
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-08-27
C
C ABSTRACT: BRDHGT RETURNS A REPORT THAT SPANNED TWO RECORDS 
C   OF THE SIMPLE GRAPHICS OBSERVATIONAL FORMAT.       
C
C PROGRAM HISTORY LOG:
C   96-08-27  LARRY SAGER
C
C USAGE:    CALL FORUPA  (KNEXT, KEND, COUT, IUNO, RHGTS, IREP,
C                            ARR, IRET) 
C   INPUT ARGUMENT LIST:
C     KNEXT    - POINTER TO STARTING LOCATION OF THIS REPORT       
C     KEND     - POINTER TO ENDING LOCATION OF THIS REPORT
C     COUT     - MANDATORY LEVEL DATA                    
C     IUNO     - OBSERVATIONAL DATA FILE UNIT NUMBER    
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     RHGTS    - MANDATORY LEVEL HEIGHT DATA
C     IREP     - NUMBER OF REPORTS IN RHGTS                   
C     IRET     - RETURN CODE  0 = NORMAL RETURN
C                            10 = END OF FILE ENCOUNTERED
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY4
C
C$$$
      SUBROUTINE BRDHGT(KNEXT, KEND, COUT, IUNO, RHGTS, IREP,
     1         ARR, IRET)
C     
C     This subroutine returns a UPAUPA report that spanned two    
C     records.                         
C
      CHARACTER*8   COUT (512)
      CHARACTER*8   CAR
      CHARACTER*8   AID
C
      REAL*8        ARR(300)
      REAL*8        RHGTS(800,23)
C
C
      DATA  IMASK  /Z'FFFFFFFF00000000'/
C
      EQUIVALENCE (CAR,RAR)
      EQUIVALENCE (CAR,IAR)
C
      IRET = 0
      IOK = 1
      IF((KNEXT+5).LT.512) THEN
        AID = COUT(KNEXT+5)
        IOK = 0
      END IF
      PROCESS = 0
      J = 1
      PRINT *,' KNEXT, KEND ARE ',KNEXT,KEND
      IF(KNEXT .LT. 513) THEN
      DO  K = KNEXT,512 
         CAR = COUT(K)
         call byteswap(CAR, 8, 1)
	 IAR1 = ISHFT(IAR,-32)
	 IA = ISHFT(IAR1,-28)
	 IF (IA .GT. 0) IAR1=IAR1+IMASK
         IAR2 = ISHFT(ISHFT(IAR,32),-32)
         IF (J .EQ. 11) THEN
            IAR = IAR1
            ARR(J) = RAR
            IAR = IAR2 
            ARR(J+1) = RAR
            J = J + 2
         ELSE
	    IA = ISHFT(IAR2,-28)
	    IF (IA .GT. 0) IAR2 = IAR2+IMASK
	    FAR =  IAR1
	    FAR =  FAR/4096.
	    FAR2 = IAR2
            FAR2 = FAR2/4096.
	    ARR(J) = FAR
            ARR(J+1) = FAR2
            J = J + 2
         END IF
      END DO
      END IF
      READ  (IUNO) COUT
      IF (COUT(1).eq.'ENDOFILE') THEN
         IRET = 10   
         RETURN
      END IF
      IF (KEND .EQ. 0) THEN
         IA = 512 - KNEXT
         IF(KNEXT .EQ. 513) IA = 1
         CAR = COUT(IA+1)
         call byteswap(CAR, 8, 1)
         KEND = ISHFT(IAR,-32)/8192
      END IF
      IF(KEND .LT. 0) THEN
         KEND = 0
         GOTO 22
      END IF
      DO  K = 1,KEND
         CAR = COUT(K)
         call byteswap(CAR, 8, 1)
	 IAR1 = ISHFT(IAR,-32)
	 IA = ISHFT(IAR1,-28)
	 IF (IA .GT. 0) IAR1=IAR1+IMASK
         IAR2 = ISHFT(ISHFT(IAR,32),-32)
         IF (J .EQ. 11) THEN
            IAR = IAR1
            ARR(J) = RAR
            IAR = IAR2 
            ARR(J+1) = RAR
            J = J + 2
         ELSE
	    IA = ISHFT(IAR2,-28)
	    IF (IA .GT. 0) IAR2 = IAR2+IMASK
	    FAR =  IAR1
	    FAR =  FAR/4096.
	    FAR2 = IAR2
            FAR2 = FAR2/4096.
	    ARR(J) = FAR
            ARR(J+1) = FAR2
            J = J + 2
         END IF
      END DO
 22   IF ( IOK .EQ. 1 ) AID = COUT (512-KNEXT)
      IA = ARR(13)
      IB = ARR(14)
      IF ((IA .GT. 0) .AND. (IREP .LT. 200)) THEN
	IREP=IREP+1
	CAR = AID
 	RHGTS(IREP,1) = RAR
 	RHGTS(IREP,2) = ARR(4)
	IND = IB
	DO K = 1,IA
	   RHGTS(IREP,K+2) = ARR(IND)
           IND = IND + 7
	END DO
      ENDIF
      IA = KEND + 1
      IF(COUT(IA).NE.'END_REPO') THEN
        PRINT 119,IA,COUT(IA)
 119    FORMAT('  END_REPO NOT FOUND AT WORD ',i5,' INSTEAD',A8 )
      END IF
      KNEXT = IA + 1
C      
      RETURN
      END
