C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    HEAFOS      FORM THE REPORT HEADING 
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-12-10
C
C ABSTRACT: HEAFOS FORMS THE HEADING IN AFOS FORMAT FOR EACH 
C   OBSERVATION                                       
C
C PROGRAM HISTORY LOG:
C   96-12-10  LARRY SAGER
C
C USAGE:    CALL HEAFOS  (IARR, OHED, IAUTO, ILHD)                           
C   INPUT ARGUMENT LIST:
C     ARR      - UNPACKED REPORT IN GRAPHICAL FORMAT
C     IAUTO    - AUTOMATIC/MANUAL STATION FLAG
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     OHED     - HEADING ARRAY                
C     ILHD     - LENGTH (IN WORDS) OF HEADING ARRAY         
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
      SUBROUTINE HEAFOS (IARR, OHED, IAUTO, ILHD)       
C 
C     THIS SUBROUTINE FORMS THE HEADING FOR EACH    
C       REPORT IN THE AFOS SURFACE PLOTFILE 
C
      INTEGER       IARR(*)
C
      CHARACTER*8   CWORK
C
      CHARACTER*1   OWORK(8)
      CHARACTER*1   OHED(*)
      CHARACTER*1   PSOWDT(6)
C
      EQUIVALENCE (CWORK,OWORK)
      EQUIVALENCE (IWORK,CWORK)
C
      ILHD = 1
C
C     BEGIN BY GETTING THE I, J LOCATION FOR THIS
C       REPORT.
C
      ALAT = IARR(1)*.1
      ALONG = IARR(2)*.1
      KEIL = 7 
      CALL TRUIJ(ALAT,ALONG,XI,XJ,KEIL)
      XI = XI - 28.0
      XJ = XJ - 5.0
      IN = (XI - 1.)*37.5 + 0.5
      JN = (XJ - 1.)*37.5 + 0.5
C     PRINT 100,IARR(11),IN,JN
 100  FORMAT(' STATION ',A8,' i js are ',2i8)
      IF(IN .LT. 0) THEN
         OHED(ILHD) = '-'
         ILHD = ILHD + 1
         IN = -IN
      END IF
      CALL BIN2CH(IN,CWORK,4,'A99')
      IST = 4
      IF (IN .GE. 10) IST = 3 
      IF (IN .GE. 100) IST = 2
      IF (IN .GE. 1000) IST = 1
      DO K = IST,4
         OHED(ILHD) = OWORK(K)
         ILHD = ILHD + 1
      END DO      
      OHED(ILHD) = ','
      ILHD = ILHD + 1
      IF(JN .LT. 0) THEN
         OHED(ILHD) = '-'
         ILHD = ILHD + 1
         JN = -JN
      END IF
      CALL BIN2CH(JN,CWORK,4,'A99')
      IST = 4 
      IF (JN .GE. 10) IST = 3
      IF (JN .GE. 100) IST = 2
      IF (JN .GE. 1000) IST = 1
      DO K = IST,4
         OHED(ILHD) = OWORK(K)
         ILHD = ILHD + 1
      END DO
      OHED(ILHD) = ','
      ILHD = ILHD + 1 
C
C     AFZOOM CREATES PSOWDT
C     
      KTTYP = 1 
      CALL AFZOOM (IARR(11),KTTYP,PSOWDT) 
C
C     ADD FLAG FOR AUTOMATIC STATION
C
      IF(IAUTO .EQ. 1) PSOWDT(6) = '7'
C
C     ADD PSOWDT TO HEADER
C
      DO K = 1,6
         OHED(ILHD) = PSOWDT(K)
         ILHD = ILHD + 1
      END DO
      OHED(ILHD) = ','
      ILHD = ILHD + 1
C
C     OBSERVATION TIME OF THIS REPORT
C
      ITME = IARR(4)
      CALL BIN2CH(ITME,CWORK,8,'A99')
      DO K = 5,8
         OHED(ILHD) = OWORK(K) 
         ILHD = ILHD + 1
      END DO
      OHED(ILHD) = ','
C
C     ADD THE STATION NAME
C
      IWORK = IARR(11)
      DO K = 1,6
         IF(OWORK(K) .NE. ' ') THEN
            ILHD = ILHD + 1 
            OHED(ILHD) = OWORK(K) 
         END IF
      END DO
C     
      ILHD = ILHD + 1 
      OHED(ILHD) = ','
C
      RETURN
      END
