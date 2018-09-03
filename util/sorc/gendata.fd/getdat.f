C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GETDAT      GET THE DATA             
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-12-10
C
C ABSTRACT: GETDAT EXTRACTS THE OBSERVATIONAL DATA FROM THE  
C   GRAPHICAL FORMATTED FILES AND STORES IT IN PLOTFILE 
C   FORMAT INTO THE AFOS SURFACE PLOTFILE.
C
C PROGRAM HISTORY LOG:
C   96-12-10  LARRY SAGER
C
C USAGE:    CALL GETDAT  (IARR, OLINE, ILAT, ILON, ILNG)               
C   INPUT ARGUMENT LIST:
C     ARR      - UNPACKED OBSERVATION IN GRAPHICS FORMAT
C     ILAT     - LATITUDE OF OBSERVATION
C     ILON     - LONGITUDE OF OBSERVATION
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     OLINE    - AFOS SURFACE PLOTFILE ARRAY
C     ILNG     - LENGTH (IN WORDS) OF THE AFOS ARRAY
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$

      SUBROUTINE GETDAT (IARR, OLINE, ILAT, ILON, ILNG)        
C 
C     THIS SUBROUTINE EXTRACTS AND STORES THE DATA 
C       INTO THE AFOS PLOTFILE ARRAY. 
C
      CHARACTER*8   CWORK
      CHARACTER*8   CTEMP
C
      CHARACTER*1   OWORK(8)
      CHARACTER*1   OLINE(*)
      CHARACTER*1   OCT(3)
C
      INTEGER       IEND    
      INTEGER       IARR(*)
C
      EQUIVALENCE   (CWORK,OWORK)
      EQUIVALENCE   (IWORK,OWORK)
C
      DATA          IEND /Z'3B0D0A0000000000'/
C
C     EXTRACT THE DATA NECESSARY FOR THE AFOS PLOT FILE.
C
      ILNG = 1
      ILST = 0
C
C     FRACTION OF THE CELESTIAL DOME
C
      OLINE(ILNG) = 'M'
      IF(IARR(14) .LT. 99990) THEN
         IFN = IARR(14) / 12
         CALL BIN2CH(IFN,CWORK,8,'A99')
         OLINE(ILNG) = OWORK(8)
         ILNG = ILNG + 1
         ILST = ILNG
      ELSE 
         ILNG = ILNG + 1
      END IF
      OLINE(ILNG) = ','
C
C     STORE THE WINDS  
C
      IF((IARR(3) .LT. 99990) .AND. (IARR(4) .LT. 99990)) THEN
           IWIND = IARR(3)*100
           IWIND = IWIND + IARR(4)
           CALL BIN2CH(IWIND,CWORK,8,'A99')
           DO K = 1,5
               ILNG = ILNG + 1
               OLINE(ILNG) = OWORK(K+3) 
           END DO
           ILST = ILNG + 1
      END IF
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
C
C     STORE THE MEAN SEA LEVEL PRESSURE
C
      IF(IARR(1) .LT. 99990) THEN
          CALL BIN2CH(IARR(1),CWORK,8,'A99')
          DO K = 6,8
	     ILNG = ILNG + 1
 	     OLINE(ILNG) = OWORK(K)
          END DO
          ILST = ILNG + 1
      END IF
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
C     
C     TEMPERATURE IN CELSIUS    
C
      IF (IARR(5) .LT. 99990) THEN
          RTMP = .1 * IARR(5) 
          ITMP = NINT(RTMP)
          IF (ITMP .LT. 0) THEN
              ILNG = ILNG + 1
              ITMP = -ITMP
              OLINE(ILNG) = '-'
          END IF
          CALL BIN2CH(ITMP,CWORK,8,'A99')
          IST =7 
          IF(OWORK(7) .EQ. '0') IST = 8
          DO K = IST,8
              ILNG = ILNG + 1
              OLINE(ILNG) = OWORK(K)
          END DO
          ILST = ILNG + 1
      END IF
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
C
C     DEW POINT TEMPERATURE  
C 
      IF (IARR(6) .LT. 99990) THEN
          RTMP = .1 * IARR(6) 
          IAR = NINT(RTMP)
          IF (IAR .LT. 0) THEN
              ILNG = ILNG + 1
              IAR = -IAR
              OLINE(ILNG) = '-'
          END IF
          CALL BIN2CH(IAR,CWORK,8,'A99')
          IST = 7
          IF (OWORK(7) .EQ. '0') IST = 8 
          DO K = IST,8
             ILNG = ILNG + 1
             OLINE(ILNG) = OWORK(K)
          END DO
          ILST = ILNG + 1
      END IF
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
C
C     PRESENT WEATHER
C
      IF (IARR(12) .LT. 99990) THEN
          CALL BIN2CH(IARR(12),CWORK,8,'A99')
          DO K = 7,8
             ILNG = ILNG + 1
             OLINE(ILNG) = OWORK(K)
          END DO
          ILST = ILNG + 1
      END IF
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
C
C     HORIZONTAL VISIBILITY (REMOVE FROM N.HEMI)
C
C     IF (IARR(11) .LE. 66) THEN
C         CALL BIN2CH(IARR(11),CWORK,8,'A99')
C         DO K = 7,8
C            ILNG = ILNG + 1
C            OLINE(ILNG) = OWORK(K)
C         END DO
C         ILST = ILNG + 1
C     END IF
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
C
C     PRESSURE TENDENCY AND CHARACTERISTIC
C
      IF (IARR(21) .LT. 99990) THEN
         IAA = IARR(20)
         IF(IAA .GT. 99990) THEN
            IAA = 2
            IF(IARR(21) .LT. 0) IAA = 6
            IF(IARR(21) .EQ. 0) IAA = 4
         END IF
         ILNG = ILNG + 1
         OLINE(ILNG) = '+'
         IF(IARR(21) .LT. 0) THEN
            IARR(21) = -IARR(21)
            OLINE(ILNG) = '-'
         END IF
         CALL BIN2CH(IARR(21),CWORK,8,'A99')
         IST = 8
         IF(IARR(21) .GE. 10) IST =7
         IF(IARR(21) .GE. 100) IST = 6
         DO K = IST,8
            ILNG = ILNG + 1
            OLINE(ILNG) = OWORK(K)
         END DO
         ILNG = ILNG + 1
         OLINE(ILNG) = ','
         CALL BIN2CH(IAA,CWORK,8,'A99')
         ILNG = ILNG + 1
         OLINE(ILNG) = OWORK(8)
         ILNG = ILNG + 1
         OLINE(ILNG) = ','
         ILST = ILNG
      ELSE
         ILNG = ILNG + 1
         OLINE(ILNG) = ','
         ILNG = ILNG + 1
         OLINE(ILNG) = ','
      END IF
C
C     CLOUD TYPES
C
      OCT(1) = '/'
      OCT(2) = '/'         
      OCT(3) = '/'
      IHC = 0 
      IF (IARR(16) .LT. 99990) THEN
          CALL BIN2CH(IARR(16),CWORK,8,'A99')
          IF(OWORK(8) .NE. '0') OCT(1) = OWORK(8)
          IHC = 1
      ENDIF
      IF (IARR(18) .LT. 99990) THEN
          CALL BIN2CH(IARR(18),CWORK,8,'A99')
          IF(OWORK(8) .NE. '0') OCT(2) = OWORK(8)
          IHC = 1
      ENDIF
      IF (IARR(19) .LT. 99990) THEN
          CALL BIN2CH(IARR(19),CWORK,8,'A99')
          IF(OWORK(8) .NE. '0') OCT(3) = OWORK(8)
          IHC = 1
      ENDIF
      IF (IHC .EQ. 1) THEN
          DO K = 1,3
  	     ILNG = ILNG + 1
             OLINE(ILNG) = OCT(K)
          END DO
          ILST = ILNG + 1
      END IF
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
C
C     6-HR PRECIP
C
      IF (IARR(22) .LT. 99990) THEN
         IF (IARR(22) .NE. 0) THEN
            IF (IARR(22) .LT. 0) THEN  
               ILNG = ILNG + 1
               OLINE(ILNG) = 'T'
               ILST = ILNG + 1
            ELSE
               APR = IARR(22)
               IPR = NINT(APR/2.54)
               CALL BIN2CH(IPR,CWORK,8,'A99')
               IF(IPR .GE. 100) THEN
                  ILNG = ILNG + 1
                  OLINE(ILNG) = OWORK(6)
               END IF
               ILNG = ILNG + 1
               OLINE(ILNG) = '.'
               DO K =7,8
                  ILNG = ILNG + 1
                  OLINE(ILNG) = OWORK(K)
               END DO
               ILST = ILNG + 1
            END IF
         END IF
      END IF
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
C
C     LAT, LONG
C
      CALL BIN2CH(ILAT,CWORK,8,'A99')
      DO K = 6,8
          ILNG = ILNG + 1
          OLINE(ILNG) = OWORK(K)
      END DO
      ILNG = ILNG + 1
      OLINE(ILNG) = 'N'
      IF(ILAT .LT. 0) OLINE(ILNG) = 'S'
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
C  
      IF(ILON .GT. 1800) THEN
          ILON = -(3600 - ILON)
      END IF
      CALL BIN2CH(ILON,CWORK,8,'A99')
      DO K = 5,8
          ILNG = ILNG + 1
          OLINE(ILNG) = OWORK(K)
      END DO
      ILNG = ILNG + 1
      OLINE(ILNG) = 'W'
      IF(ILON .LT. 0) OLINE(ILNG) = 'E'
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
      ILST = ILNG
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
C
C     SHIP'S DIRECTION AND SPEED
C
      IAD = 9        
      IAS = 9
      IF (IARR(32) .LT. 99990) IAD = IARR(32)
      IF (IARR(33) .LT. 99990) IAS = IARR(33)
      IAR = IAD*10 + IAS
      IF (IAR .LT. 99) THEN
          CALL BIN2CH(IAR,CWORK,8,'A99')
          DO K = 7,8
             ILNG = ILNG + 1
             OLINE(ILNG) = OWORK(K)
          END DO
          ILST = ILNG     
      END IF
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
C
C     WIND WAVE PERIOD AND HEIGHT
C
      CTEMP = '1XXXX   '
      IF (IARR(26) .LT. 0) THEN
         CTEMP(1:1) = '2'
         IARR(26) = -IARR(26)
      END IF
      IF (IARR(27) .LT. 0) THEN
         CTEMP(1:1) = '2'
         IARR(27) = -IARR(27)
      END IF
      IF (IARR(26) .LT. 99990)  THEN
         IAD = IARR(26)
         CALL BIN2CH(IAD,CWORK,8,'A99')
         CTEMP(2:3) = CWORK(7:8)
      END IF
      IF (IARR(27) .LT. 99990) THEN
         IAS = IARR(27)
         CALL BIN2CH(IAS,CWORK,8,'A99')
         CTEMP(4:5) = CWORK(7:8)
      END IF
      IF (CTEMP(2:8) .NE. 'XXXX   ') THEN
         CWORK = CTEMP
         DO K = 1,5
            ILNG = ILNG + 1
            OLINE(ILNG) = OWORK(K)
         END DO
         ILST = ILNG + 1
      END IF
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
C
C     SWELL WAVE DIRECTION PERIOD HEIGHT
C
      CTEMP = 'XXXXXX  '
      IF (IARR(28) .LT. 99990) THEN
         IAD = IARR(28)*.1
         CALL BIN2CH(IAD,CWORK,8,'A99')
         CTEMP(1:2) = CWORK(7:8)
      END IF
      IF (IARR(29) .LT. 99990) THEN
         IAD = IARR(29)
         CALL BIN2CH(IAD,CWORK,8,'A99')
         CTEMP(3:4) = CWORK(7:8)
      END IF
      IF (IARR(30) .LT. 99990) THEN
         IAD = IARR(30)
         CALL BIN2CH(IAD,CWORK,8,'A99')
         CTEMP(5:6) = CWORK(7:8)
      END IF
      IF (CTEMP .NE. 'XXXXXX  ') THEN
         CWORK = CTEMP
         DO K = 1,6
            ILNG = ILNG + 1
            OLINE(ILNG) = OWORK(K)
         END DO
         ILST = ILNG + 1
      END IF
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
C
C     WATER TEMPERATURE                
C
      IF (IARR(31) .LT. 99990) THEN
          RTMP = .1* IARR(31)  
          ITMP = NINT(RTMP)
          IF (ITMP .LT. 0) THEN
              ILNG = ILNG + 1
              ITMP = -ITMP
              OLINE(ILNG) = '-'
          END IF
          CALL BIN2CH(ITMP,CWORK,8,'A99')
          IST = 7
          IF (OWORK(6) .GT. '0') IST = 6
          DO K = IST,8
              ILNG = ILNG + 1
              OLINE(ILNG) = OWORK(K)
          END DO
          ILST = ILNG + 1    
      END IF
      ILNG = ILNG + 1
      OLINE(ILNG) = ','
      ILNG = ILST
C
C     STORE THE END OF REPORT INDICATOR
C
      IWORK = IEND
      call byteswap(iwork,8,1)
      DO K = 1,3
         OLINE(ILNG) = OWORK(K)
         ILNG = ILNG + 1
      END DO
      ILNG = ILNG - 1
C         
      RETURN
      END
