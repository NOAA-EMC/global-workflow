      SUBROUTINE CLOUDS(RRR, NRET, ARR) 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    CLOUDS      STORE THE CLOUD DATA       
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-12-17
C
C ABSTRACT: REDSFC STORES THE CLOUD GROUP DATA FROM BUFR TANKS
C           INTO A USABLE FORM FOR THE AFOS PLOTFILE MAKER.
C
C PROGRAM HISTORY LOG:
C   96-12-17  LARRY SAGER
C
C USAGE:    CALL CLOUDS  (ARR, NRET, IRET)    
C   INPUT ARGUMENT LIST:
C     RRR      - BUFR CLOUD DATA            
C     NRET     - PARAMETER RETURN CODE  
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     ARR      - ARRAY TO HOLD DATA FOR INPUT TO AFOS
C                SURFACE PLOTFILE MAKER.
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C
      REAL          ARR (*)
      REAL          RRR (10,255)
C
      DATA FMISS   /9999999./ 
C
C     SEARCH FOR LOW MIDDLE AND HIGH CLOUD
C          GROUPS (VSSO = 7,8,9)
C
      J = 15   
      DO JJ = 1,NRET      
          INDEX = NRET - JJ + 1
          RR = RRR(1,INDEX)
          IF (RR .EQ. 7.) THEN
              ARR(J) = RRR(2,INDEX)
              ARR(J+1) = RRR(3,INDEX)
              ARR(J+2) = RRR(4,INDEX)
          END IF
          IF (RR .EQ. 8.) THEN
              ARR(J+3) = RRR(2,INDEX)
              ARR(J+4) = RRR(3,INDEX)
              ARR(J+5) = RRR(4,INDEX)
          END IF
          IF (RR .EQ. 9.) THEN
              ARR(J+6) = RRR(2,INDEX)
              ARR(J+7) = RRR(3,INDEX)
              ARR(J+8) = RRR(4,INDEX)
          END IF
          IF (RR .EQ. 0.) THEN
              IF(RRR(3,INDEX) .LT. 99990.) THEN
                 ARR(J+1) = RRR(3,INDEX)
              END IF
              IF(RRR(4,INDEX) .LT. 40. ) THEN
                 ITYP = RRR(4,INDEX)
                 ITY  = ITYP / 10
                 ILV  = ITYP - ITY*10
                 IF((ITY .GE. 1).AND.(ITY.LE.3)) THEN
                    IXX = (3 - ITY)*3
                    IF(ILV .GT. 0) ARR(J+2+IXX) = ILV
                 END IF
              END IF
          END IF
C
C         DECODE LOW MIDDLE HIGH CLOUDS
C            (VSSO = 1,2,3) FROM METARS
C
          IF((RR .GE. 1.) .AND. (RR .LE. 3.)) THEN
              IN = 0
              IF(RRR(2,INDEX) .EQ. 8.) IN = 8
              IF(RRR(2,INDEX) .EQ. 11.) IN = 3
              IF(RRR(2,INDEX) .EQ. 12.) IN = 6
              IF(RRR(2,INDEX) .EQ. 13.) IN = 2
              IF(RRR(2,INDEX) .GT. 99990.) IN = 9
              K = 1
              IF (RRR(3,INDEX) .GT. 2000.) K = 4
              IF (RRR(3,INDEX) .GT. 5500.) K = 7 
              IF (RRR(3,INDEX) .LT. 99990.) THEN
                  ARR(J+K) = RRR(3,INDEX)
                  ARR(J+K-1) = IN
              END IF
              IF (ARR(J-1) .GT. 99990.) THEN
                  ARR(J-1) = IN*12
              ELSE
                  IF (ARR(J-1) .LT. IN*12) ARR(J-1) = IN*12
              END IF   
          END IF
      END DO
C
C              SET WORK AREA TO MISSING
C
      DO KK = 1,4
          DO JJ = 1,NRET     
              RRR(KK,JJ) = FMISS
          END DO
      END DO
C
      RETURN
      END
