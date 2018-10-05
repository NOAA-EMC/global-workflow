C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    THNSTN      THIN THE STATION LIST
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 97-01-10
C
C ABSTRACT: THNSTN SORTS THE DATA ACCORDING TO LOCATION AND
C   THEN DROPS OVERLAPPING STATIONS ACCORDING TO THE 
C   PRIORITY
C
C PROGRAM HISTORY LOG:
C
C   97-01-10  LARRY SAGER
C
C USAGE:    CALL THNSTN  (ITABL, KSTN)                  
C   INPUT ARGUMENT LIST:
C     ITABL    - STATION SORT TABLE        
C     KSTN     - NUMBER OF STATIONS IN SORT TABLE
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C    ITABL     - STATION SORT TABLE UPDATTED BY THINNING
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
      SUBROUTINE THNSTN (LL, ITABL, KSTN, CSTN)
C 
C     THIS SUBROUTINE SORTS THE STATION TABLE BY LOCATION
C     AND THEN THINS OVERLAPPING STATIONS
C
C
      INTEGER       ITABL(3,20000)
      INTEGER       ITN(20000)
      INTEGER       ITP(20000)
      INTEGER       ISORT(1,20000)
      INTEGER       IWORK(20000)
      INTEGER       IOVER(20000)
      INTEGER       IVR(50,20000)
      INTEGER       LL(6)
 
      CHARACTER*8   CSTN(20000)

      DATA IVR  /1000000*0/
      DATA IOVER/20000*0/
      DATA ITOL /45/
      DATA JTOL /43/   
C
      IMODE = 0
      IRECLTH = 1 
      IKEYLTH = 8
      IRADSIZ = 1
C     
C     REPLACE THE I AND J TOLERANCES IF NEW VALUES 
C       WERE READ IN
C
      IF(LL(5) .NE. 999) ITOL = LL(5)
      IF(LL(6) .NE. 999) JTOL = LL(6)
      PRINT *,' I J TOLERANCES ARE ',ITOL,JTOL

      DO K =1,KSTN
         ISORT(1,K) = ITABL(1,K)
      END DO  
C
C     SORT THE REPORTS USING THE SORTER  ORDERS
C

      CALL ORDERS ( IMODE, IWORK, ISORT, ITN, KSTN, IRECLTH, IKEYLTH,
     1              IRADSIZ )

      DO K =1,KSTN
         ISORT(1,K) = ITABL(3,K)
      END DO  

      CALL ORDERS ( IMODE, IWORK, ISORT, ITP, KSTN, IRECLTH, IKEYLTH,
     1              IRADSIZ )

C     DO K = 1,KSTN
C        ISORT(1,K) = ITN(K)                                 
C     END DO
C
C     THIN OVERLAPPING STATIONS BY COMPARING PRIORITY NUMBERS
C
      DO 20 KK = 1,KSTN-1        
C
C        START BY SKIPPING THIS STATION IF IT HAS ALREADY BEEN 
C           THINNED.
C
         IF(ITN(KK) .EQ. 0) GO TO 20
         K = ITN(KK)
C
C        SET THE OVERLAP LIMITS FOR THIS STATION
C
         IP = ITABL(1,K) + ITOL
         JM = ITABL(2,K) - JTOL
         JP = ITABL(2,K) + JTOL
C
C        LOOP ON STATIONS WHICH OVERLAP IN THE I-DIRECTION
C
         DO 10 JJ = KK+1,KSTN 
             IF(ITN(JJ) .EQ. 0) GO TO 10  
             J = ITN(JJ)
             IF(IP .LT. ITABL(1,J)) GO TO 20
C
C               SEE IF THESE STATIONS OVERLAP IN THE J-DIRECTION
C
                JTAB = ITABL(2,J)
                IF((JM .LE. JTAB) .AND. (JP .GE. JTAB))THEN
C
C                    THESE STATIONS OVERLAP:  DROP THE STATION
C                      WITH THE LOWER PRIORITY NUMBER
C 
                     IF(IOVER(K) .LT. 50) IOVER(K) = IOVER(K) + 1
                     IF(IOVER(J) .LT. 50) IOVER(J) = IOVER(J) + 1
                     IVR(IOVER(K),K) = J      
                     IVR(IOVER(J),J) = K
C                    IF (ITABL(3,K) .LT. ITABL(3,J)) THEN
C                       ITN(KK) = 0    
C                       ITABL(3,K) = -1
C                       GO TO 20
C                    END IF
C
C                    ERASE THE STATION           
C
C                    ITN(JJ) = 0
C                    ITABL(3,J) = -1
                END IF
  10      CONTINUE
  20  CONTINUE  
C
C     PASS # 2 THIN OUT STATIONS WITH THE SAME 
C       PRIORITY AS OTHERS BUT MORE OVERLAPS
C
C     DO K = 1,KSTN
C        KK = ITP(KSTN - K + 1)
C        PRINT 108,CSTN(KK),IOVER(KK),ITABL(3,KK)
C        IF(IOVER(KK).GT.0) THEN
C            DO J=1,IOVER(KK)
C               JJ = IVR(J,KK)
C               PRINT 107,CSTN(JJ),ITABL(3,JJ),IOVER(JJ)
C            END DO
C        END IF
C     END DO
C     PRINT *,'AAAAAAAAAAAAAAAAPASS = 2'
C
      DO 50 KR = 1,KSTN
      K = ITP(KSTN - KR + 1)
      IF(ITABL(3,K) .EQ. -1) THEN
C        PRINT *,CSTN(K),' PREVIOUSLY THINNED'
         GOTO 50
      END IF
      IF(IOVER(K).GT.0) THEN
        DO 40 J = 1,IOVER(K)
           KK = IVR(J,K)
           IF(ITABL(3,KK) .EQ. -1) THEN    
C             PRINT *,' ',CSTN(KK),' Olap station peviously thinned'
              GOTO 40
           END IF
C
C         DROP STATION OF EQUAL PRIORITY BUT WITH
C           MORE OVERLAPS
C
          IF(ITABL(3,K).EQ.ITABL(3,KK)) THEN
             IF(IOVER(K) .LT. IOVER(KK)) THEN
                ITABL(3,KK) = -1
C               PRINT *,CSTN(KK),' =PRIOR THINNED'
             ELSE
                ITABL(3,K) = -1
C               PRINT *,CSTN(K),' =PRIOR LOOP THINNED'
                GOTO 50
             END IF  
          END IF
 40     CONTINUE
      END IF
 107  FORMAT(' OVERLAPPING STATION ',A8,i5,' has ', i3,' olaps,')
 108  FORMAT(' STATION ',A8,' olaps: ',i4,' PRIOR ',i4)
 50   CONTINUE
C
C     PASS #3 THROUGH THE REPORTS:  DROP OVERLAPPING STATIONS
C        OF LOWER PRIORITY
C
C     PRINT *,'XXXXXXXXXXX START PASS 3       ****'
      DO 70 KR = 1,KSTN
      K = ITP(KSTN - KR + 1)
      IF(ITABL(3,K) .EQ. -1) THEN
C        PRINT *,CSTN(K),' THINNED BEFOR PRIOR DROP'
         GOTO 70
      END IF
C     PRINT 108,CSTN(K),IOVER(K),ITABL(3,K)
      IF(IOVER(K).GT.0) THEN
        DO 60 J = 1,IOVER(K)
           KK = IVR(J,K)
           IF(ITABL(3,KK) .EQ. -1) THEN   
C             PRINT *,CSTN(KK),' OVERLAP ALREADY THINNED'
              GOTO 60
           END IF
C         PRINT 107,CSTN(KK),ITABL(3,KK),IOVER(KK)
C
C         DROP STATION OF EQUAL PRIORITY BUT WITH
C           MORE OVERLAPS
C
          IF(ITABL(3,K) .GT. ITABL(3,KK)) THEN
                ITABL(3,KK) = -1
C               PRINT *,CSTN(KK),'  THINNED'
          END IF
 60     CONTINUE
      END IF
 70   CONTINUE
         DO JA=1,20000
            IOVER(JA) = 0
         END DO 
      PRINT *, 'KSTN = ', KSTN
      DO 90 KK = 1,KSTN-1        
C
C        THIS LOOP DOES A FINAL THINNING CLEANUP               
C
         K = ITN(KK)
         IF ( K .EQ. 0 ) GO TO 90
         IF(ITABL(3,K) .EQ. -1) GO TO 90
C
C        SET THE OVERLAP LIMITS FOR THIS STATION
C
         IP = ITABL(1,K) + ITOL
         JM = ITABL(2,K) - JTOL
         JP = ITABL(2,K) + JTOL
C
C        LOOP ON STATIONS WHICH OVERLAP IN THE I-DIRECTION
C
         DO 80 JJ = KK+1,KSTN 
             J = ITN(JJ)
             IF ( J .EQ. 0 ) GO TO 80
             IF(ITABL(3,J) .EQ. -1) GO TO 80  
             IF(IP .LT. ITABL(1,J)) GO TO 90
C
C               SEE IF THESE STATIONS OVERLAP IN THE J-DIRECTION
C
                JTAB = ITABL(2,J)
                IF((JM .LE. JTAB) .AND. (JP .GE. JTAB))THEN
C
C                    THESE STATIONS OVERLAP:  DROP THE STATION
C                      WITH THE LOWER PRIORITY NUMBER
C 
                     IF (ITABL(3,K) .LT. ITABL(3,J)) THEN
                        ITN(KK) = 0    
                        ITABL(3,K) = -1
C                       PRINT *,' DROPPING ',CSTN(K)
                        GO TO 90
                     END IF
C
C                    ERASE THE STATION           
C
                     ITN(JJ) = 0
                     ITABL(3,J) = -1
C                    PRINT *,' DRPPING ',CSTN(J)
                END IF
  80      CONTINUE
  90  CONTINUE  
C     DO K = 1,KSTN
C        KK = ITP(KSTN - K + 1)
C        PRINT 108,CSTN(KK),IOVER(KK),ITABL(3,KK)
C        IF(IOVER(KK).GT.0) THEN
C            DO J=1,IOVER(KK)
C               JJ = IVR(J,KK)
C               PRINT 107,CSTN(JJ),ITABL(3,JJ),IOVER(JJ)
C            END DO
C        END IF
C     END DO
C
      RETURN
      END
