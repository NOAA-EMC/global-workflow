C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    FORSFC      FORM THE GRAPHICS FORMAT.                          
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-09-17                     
C                                                                               
C ABSTRACT: FORSFC TAKES UNPACKED BUFR FORMAT AND CONVERTS IT 	                 
C   INTO A SIMPLE FORMAT FOR USE IN GRAPHICS PROGRAMS.                          
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   96-09-17  LARRY SAGER                                                       
C                                                                               
C USAGE:    CALL FORSFC  (HDR, HDT, ARR, IARR, IRET1 )             
C   INPUT ARGUMENT LIST:  
C     HDR      - UNPACKED BUFR HEADING INFORMATION (ID, LAT/LON ...)            
C     HDT      - UNPACKED BUFR DAT/TIME INFORMATION                             
C     ARR      - UNPACKED BUFR MANDATORY LEVEL DATA            
C                                                                               
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)                          
C     IARR     - OUTPUT IN GRAPHICS FORMAT.                                     
C                                                                               
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION                     
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP                                              
C                                                                               
C$$$                                                                            
      SUBROUTINE FORSFC (HDR, HDT, ARR, IARR, IRET1 )        
C
C     THIS SUBROUTINE CONVERTS THE BUFR DATA AND STORES INTO
C       A FORMATED ARRAY.
C     
      REAL          ARR(*)     
C
      CHARACTER*8   CTEMP
C
      INTEGER       IARR (*)  
      INTEGER       IPWN (100)
      DIMENSION     HDR (*)
      DIMENSION     HDT (*)
C
      EQUIVALENCE   (CTEMP,ITEMP)
      EQUIVALENCE   (RTEMP,ITEMP)
C
      DATA  IMISS /999999/     
      DATA  KMISS /Z'20202020'/
      DATA  IPWN  /0,1,2,3,4,5,0,0,0,0,10,76,13,0,0,0,0,0,18,0,
     1            28,21,0,21,22,25,29,31,31,34,28,41,42,44,46,48,
     1            0,0,0,0,21,63,65,63,65,73,75,67,67,0,51,51,53,55,
     1            56,57,57,58,59,0,61,61,63,65,66,67,67,68,69,0,
     1            71,71,73,75,79,79,79,0,0,0,81,80,81,81,82,85,86,86,
     1            0,0,95,91,92,96,17,97,99,0,0,19/
C
C----------------------------------------------------------------------|
C     SET THE BUFR TYPE AND SUBTYPE-- FOR NOW ZEROES
C
      IARR (9) = 0       
      IARR (10)= 0       
C
C     STATION ID
C
      RTEMP = HDR (1)
      IARR(11) = ITEMP               
      IARR(12) = KMISS
      IARR(1) = NINT(HDR(2)*10.)
      IARR(2) = NINT(HDR(3)*10.)
      IARR(7) = NINT(HDR(4))
C
C     STORE THE HOUR OF THIS REPORT
C
      RMINS = HDT(5)*1.6666666
      RHRMN = HDT(4)*100. + RMINS
      IARR(4) = NINT(RHRMN)
      IDX = 34
      IARR(13) = 1 
      IARR(14) = IDX + 1
C
C     INITIALIZE THE DATA AREA TO MISSING
C
      DO K = 1,40
         IARR(IDX + K) = IMISS
      END DO
C
C     STORE THE DATA INTO THE OUTPUT ARRAY.       
C       START WITH MEAN SEA LEVEL PRESSURE
C       AND STATION PRESSURE
C
      IARR(IDX+1) = NINT(ARR(1)*.1)
      IARR(IDX+2) = NINT(ARR(2)*.1)
C     
C     WIND DIRECTION AND SPEED
C
      IARR(IDX+3) = NINT(ARR(3))
      IF (ARR(4) .LT. 99999.) THEN
         SPED = ARR(4)*1.94
	 IARR(IDX+4) = NINT(SPED)
      END IF
C
C     TEMPERATURE, DEW POINT, MAX/MIN
C
      IF (ARR(5).LT.99999.) THEN
         TEMP = ARR(5) - 273.16
	 IARR(IDX+5) = NINT(10*TEMP )
      END IF
      IF (ARR(6).LT.99999.) THEN
         TEMP = ARR(6) - 273.16
	 IARR(IDX+6) = NINT(10*TEMP )
      END IF
      IF (ARR(7).LT.99999.) THEN
         TEMP = ARR(7) - 273.16
	 IARR(IDX+7) = NINT(10*TEMP )
      END IF
      IF (ARR(8).LT.99999.) THEN
         TEMP = ARR(8) - 273.16
	 IARR(IDX+8) = NINT(10*TEMP )
      END IF
C
C     QUALITY MARKS
C 
      IARR(IDX+9) = NINT(ARR(9))
C
C     HORIZONTAL VISIBILITY
C 
      IF (ARR(11).LT.99999.) THEN
         IF (ARR(11).LE.50.) THEN
	    IARR(IDX+11) = 90 
         ELSE
            IVIS = ARR(11)*.001 + 0.5
            IF (IVIS.GT.5.) IVIS = IVIS + 50.
	    IARR(IDX+11) = IVIS
         END IF
      END IF
C
C     PRESENT AND PAST WEATHER
C
      IARR(IDX+12) = NINT(ARR(12))
      IW = IARR(IDX+12)
      IF((IW .GE. 100) .AND. (IW .LT. 500)) THEN 
         IW = IW - 99
         IF(IW .GT. 99) THEN 
            IARR(IDX+12) = 99999
         ELSE
            IARR(IDX+12) = IPWN(IW)
         END IF
      END IF
      IARR(IDX+13) = NINT(ARR(13))
C
C     FRACTION OF THE CELESTIAL DOME
C
      IARR(IDX+14) = NINT(ARR(14))
C
C     FRACTION OF THE LOW,MIDDLE CELESTIAL DOME
C
      IF(ARR(18) .LT. 99999.) IARR(IDX+15) = NINT(ARR(18))
      IF(ARR(15) .LT. 99999.) IARR(IDX+15) = NINT(ARR(15))
C
C     CLOUD TYPES  LOW, MIDDLE HIGH AND CLOUD HGT
C
      IARR(IDX+16) = ARR(17)
      IARR(IDX+18) = ARR(20)
      IARR(IDX+19) = ARR(23)
C
C     CLOUD HGT CONVERSION
C
      IHGT = 99999  
      IF (ARR(22) .LT. 99999.) IHGT = NINT(ARR(22))
      IF (ARR(19) .LT. 99999.) IHGT = NINT(ARR(19))
      IF (ARR(16) .LT. 99999.) IHGT = NINT(ARR(16))
      IF (IHGT .LT. 99999) THEN
         CALL CLDHGT(IHGT, IN)
	 IARR(IDX+17) = IN 
      END IF
C
C     CHARACTERISTIC OF PRESSURE TENDENCY AND
C     PRESSURE TENDENCY
C
      IF (ARR(25).LT.99999.) THEN 
         IARR(IDX+20) = NINT(ARR(24))
         IARR(IDX+21) = NINT(ARR(25)*.1) 
      ELSE  
C
C        IF NO 3-HR PRESSURE TENDENCY, TRY FOR 24-HR
C          PRESSURE TENDENCY
C
         IF(ARR(40) .LT. 99999.) THEN
            IARR(IDX+20) = 9
            IARR(IDX+21) = NINT(ARR(40)*.01)
         END IF
      END IF
C
C
C     PRECIPITATION AMOUNT, SNOW DEPTH, 24-HR PRECIP
C     # OF 6HR GROUPS 
C
      IARR(IDX+22) = NINT(10.*ARR(26))
      IARR(IDX+23) = NINT(ARR(27))
      IARR(IDX+24) = NINT(ARR(28))
C
C     ALTIMETER SETTING -- NOT REPORTED BY SHIPS
C
      IARR(IDX+25) = NINT(ARR(29))
C
C     WAVE PERIOD AND HEIGHT
C
      IARR(IDX+26) = NINT(ARR(30))
      IARR(IDX+27) = NINT(ARR(31))
C
C     IF WAVE GROUP IS NOT AVAILABLE USE THE 
C       WIND WAVE GROUP
C
      IF((IARR(IDX+26) .GT. 99990) .AND. (IARR(IDX+27) .GT. 99990))THEN
         IARR(IDX+26) = -NINT(ARR(32))
         IARR(IDX+27) = -NINT(ARR(33))
      END IF
C
C     SWELL WAVE HEIGHT, PERIOD, AND DIRECTION
C
      IARR(IDX+28) = NINT(ARR(34))
      IARR(IDX+29) = NINT(ARR(35))
      IARR(IDX+30) = NINT(ARR(36))
C
C     SEA SURFACE TEMPERATURE
C
      IF (ARR(37).LT.99999.) THEN
         TEMP = ARR(37) - 273.16
	 IARR(IDX+31) = NINT(10*TEMP )
      END IF
C
C     DIRECTION OF SHIP  
C
      IARR(IDX+32) = NINT(ARR(38))
C
C     SPEED OF SHIP  
C
      IARR(IDX+33) = NINT(ARR(39))
C
      RETURN
      END
