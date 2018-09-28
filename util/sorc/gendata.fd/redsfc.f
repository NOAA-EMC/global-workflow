      SUBROUTINE REDSFC(RLIMS, LUBFR, IX, HDR, HDT, ARR, 
     1            IAUTO, ITYP, IJMIN, IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    REDSFC      READ THE DATA FROM THE IBM SP
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-09-17
C
C ABSTRACT: REDSFC READS THE BUFR DATA FROM THE IBM SP TANKS AND
C           STORES THE RESULTS INTO AN ARRAY.
C
C PROGRAM HISTORY LOG:
C   96-09-17  LARRY SAGER
C
C USAGE:    CALL REDSFC  (RLIMS, LUBFR, IX, HDR, HDT, ARR, 
C             IAUTO, ITYP, IRET)    
C   INPUT ARGUMENT LIST:
C     RLIMS    - GEOGRAPHIC LIMITS TO DUMP.
C     LUBFR    - DATA INPUT UNIT NUMBER 
C     IX       - FIRST READ SWITCH
C     IAUTO    - MANUAL/AUTOMATIC STATION FLAG
C     ITYP     - TYPE OF DATA
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     HDR      - ARRAY HOLDING STATION NAME AND LOCATION INFO
C     HDT      - ARRAY HOLDING STATION DATE/TIME             
C     ARR      - OBSERVATIONAL DATA
C     IJMIN    - JULIAN MINUTE FOR THIS REPORT
C     IRET     - RETURN CODE
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C
      CHARACTER*8   INOUT
      CHARACTER*8   SUBSET
      CHARACTER*8   CWORK
C
      CHARACTER*40  STRING, CDATE           
      CHARACTER*40  CATAA, CATAB, CATAC, CATAD, CATAE
      CHARACTER*40  CATAF, CATAG, CATAH, CATAI, CATAJ 
      CHARACTER*40  CATAK, CATAL 
      CHARACTER*5   CATA(48)
C
      DIMENSION     HDR (10)
      DIMENSION     HDT (10)
C
      REAL          ARR (*)
      REAL          RRR (10,255)
      REAL	    RLIMS (4)
C
      EQUIVALENCE  (CWORK,RWORK)
C
      DATA STRING  /'RPID CLAT CLON SELV AUTO TOST '/      
      DATA CDATE   /'YEAR MNTH DAYS HOUR MINU '/
      DATA CATAA   /'PMSL '/                                         
      DATA CATAB   /'WDIR WSPD '/
      DATA CATAC   /'TMDB TMDP '/               
      DATA CATAD   /'HOVI '/
      DATA CATAE   /'PRWE '/
      DATA CATAF   /'TOCC '/
      DATA CATAG   /'VSSO CLAM HOCB CLTP '/        
      DATA CATAH   /'CHPT 3HPC 24PC '/
      DATA CATAI   /'TP06 TOSD TP24 '/
      DATA CATAJ   /'POWV HOWV POWW HOWW '/                 
      DATA CATAK   /'DOSW POSW HOSW '/                                
      DATA CATAL   /'SST1 TDMP ASMP '/
      DATA CATA    /'PMSL ','PRES ','WDIR ','WSPD ','TMDB ','TMDP ',
     1              'MXTM ','MITM ','QMRK ','QMR2 ','HOVI ','PRWE ',
     2              'PSW1 ','TOCC ','CLAM ','HOCB ','CTLO ','CAME ',
     2              'HOCB ','CTME ','CAHI ','HOCB ','CTHI ',
     3              'CHPT ','3HPC ','TP06 ','TOSD ','TP24 ','ALSE ',
     4              'POWV ','HOWV ','POWW ','HOWW ','DOSW ','POSW ',
     5              'HOSW ','SST1 ','TDMP ','ASMP ','24PC ',
     6              8*'    '/

      DATA FMISS   /9999999./ 

      SAVE
C
C----------------------------------------------------------------------|
C     OPEN THE FILE                                                    |
C----------------------------------------------------------------------| 
      IRET = 0
      IF( IX .EQ. 0) THEN
        IX = 1
        INOUT = 'IN'
        LUNDX = LUBFR
        CALL OPENBF( LUBFR, INOUT, LUNDX )
        CALL READMG(LUBFR, SUBSET, IDATE, IRET1)
        IF(IRET1 .NE. 0) GO TO 50
      ENDIF
C
C----------------------------------------------------------------------|
C
C     READ THE BUFR DATA TANKS
C
C----------------------------------------------------------------------|
C
 20   CALL READSB (LUBFR,IRET2)
      IF (IRET2 .LT. 0) THEN    
           CALL READMG(LUBFR, SUBSET, IDATE, IRET1)
           IF (IRET1 .LT. 0) GOTO 50
           GO TO 20
      END IF
C
C----------------------------------------------------------------------|
C     UNPACK THE STATION INFORMATION FROM THE BUFR MESSAGE       |
C----------------------------------------------------------------------|
C
      CALL UFBINT (LUBFR, HDR, 10, 1, NRET, STRING)
C     PRINT 103,(HDR(KK),KK=1,6) 
 103  FORMAT(' HDR IS ',A8,3f8.1,' AUTO/TOST =',2f9.1)
C
C     CONVERT THE LONGITUDE TO WEST LONGITUDE
C
      IF(HDR(3) .LT. 0.) THEN
          HDR(3) = -HDR(3)
      ELSE
          HDR(3) = 360. - HDR(3)
      END IF
C
C     DROP ALL NON-US METAR REPORTS
C
      IF(ITYP .EQ. 2) THEN
         RWORK = HDR(1)
         IF(CWORK(1:1) .NE. 'K') GOTO 20
      END IF
C
C----------------------------------------------------------------------|
C     CHECK TO SEE IF THIS REPORT IS WITHIN THE AREA 
C       WANTED.  IF NOT, GO GET THE NEXT REPORT
C----------------------------------------------------------------------|
C
      IF((HDR(2) .GT. RLIMS(1)) .OR. (HDR(2) .LT. RLIMS(2)))
     1      GO  TO 20
      IF((HDR(3) .LT. RLIMS(3)) .OR. (HDR(3) .GT. RLIMS(4)))
     1      GO TO 20

C     PRINT 103,(HDR(KK),KK=1,6) 
C
C     SAVE THE AUOMATIC/MANUAL FLAG
C
      IAUTO = 0
      IF(HDR(5) .LT. 99990.) IAUTO = 1
      IF(HDR(6) .EQ. 0)  IAUTO = 1
C
C----------------------------------------------------------------------|
C     UNPACK NEEDED PARAMETERS FROM THIS REPORT 
C----------------------------------------------------------------------|
C
      CALL UFBINT (LUBFR, HDT, 10, 1, NRET, CDATE)
C     PRINT 105,(HDT(KK),KK=1,5)
 105  FORMAT(' DATE/TIME OF THIS REPORT: ',5F8.0)
C
C     GET THE JULIAN DAY NUMBER FOR THIS REPORT
C
      IYR = HDT(1) 
      IMT = HDT(2)
      IDY = HDT(3)
      IJDN = IW3JDN(IYR, IMT, IDY)
C     PRINT *,' JULIAN DAY NUMBER FOR THIS REPORT :',IJDN
      IJMIN = IJDN*1440 + HDT(4)*60 + HDT(5)
C     PRINT *,' JULIAN MINUTE FOR THIS REPORT ',IJMIN
      DO KK = 1,16
         RRR(1,KK) = FMISS
      END DO
C     CALL UFBINT (LUBFR, RRR, 10,255 , NRRT1, 'RRSTG ')
C     PRINT 104,(RRR(1,KK),KK=1,16)
C104  FORMAT(8A8)
      DO K = 1,40
         ARR(K) = FMISS
      END DO
C
C     UNPACK THE MEAN SEA LEVEL PRESSURE
C
      CALL UFBINT (LUBFR, RRR, 10, 255, NRET, CATAA)
      IF(NRET .NE. 0) ARR(1) = RRR(1,1) 
C     
C     UNPACK THE WINDS
C 
      CALL UFBINT (LUBFR, RRR, 10, 255, NRET, CATAB)
      IF(NRET .NE. 0) THEN             
         ARR(3) = RRR(1,1) 
         ARR(4) = RRR(2,1) 
      END IF
C     
C     UNPACK THE TEMPERATURE AND DEW POINT TEMP
C 
      CALL UFBINT (LUBFR, RRR, 10, 255, NRET, CATAC)
      IF(NRET .NE. 0) THEN            
         ARR(5) = RRR(1,1) 
         ARR(6) = RRR(2,1) 
      END IF
C
C     UNPACK THE HORIZONTAL VISIBILITY   
C
      CALL UFBINT (LUBFR, RRR, 10, 255, NRET, CATAD)
      IF(NRET .NE. 0) ARR(11) = RRR(1,1)
C
C     UNPACK THE PRESENT WEATHER         
C
      CALL UFBINT (LUBFR, RRR, 10, 255, NRET, CATAE)
      IF(NRET .NE. 0) ARR(12) = RRR(1,1)  
C
C     UNPACK THE TOTAL CLOUD COVER       
C
      CALL UFBINT (LUBFR, RRR, 10, 255, NRET, CATAF)
      IF(NRET .NE. 0) ARR(14) = RRR(1,1)
C
C     UNPACK THE CLOUD GROUPS
C 
      CALL UFBINT (LUBFR, RRR, 10, 255, NRET, CATAG)
      IF (NRET .NE. 0) CALL CLOUDS(RRR, NRET, ARR)
C
C     PRESSURE TENDENCY          
C
      CALL UFBINT (LUBFR, RRR, 10, 255, NRET, CATAH)
      IF(NRET .NE. 0) THEN               
          ARR(24) = RRR(1,1) 
          ARR(25) = RRR(2,1) 
          ARR(40) = RRR(3,1) 
      END IF
C
C     UNPACK PRECIPITATION GROUPS
C
      CALL UFBINT (LUBFR, RRR, 10, 255, NRET, CATAI)
      IF(NRET .NE. 0) THEN             
          ARR(26) = RRR(1,1) 
          ARR(27) = RRR(2,1) 
          ARR(28) = RRR(3,1) 
      END IF
C
C     UNPACK WIND WAVE GROUPS     
C
      CALL UFBINT (LUBFR, RRR, 10, 255, NRET, CATAJ)
      IF(NRET .NE. 0) THEN             
          ARR(30) = RRR(1,1) 
          ARR(31) = RRR(2,1)*2. 
          ARR(32) = RRR(3,1) 
          ARR(33) = RRR(4,1)*2. 
      END IF
C
C     UNPACK SWELL WAVE GROUPS    
C
      CALL UFBINT (LUBFR, RRR, 10, 255, NRET, CATAK)
      IF(NRET .NE. 0) THEN 
          ARR(34) = RRR(1,1) 
          ARR(35) = RRR(2,1) 
          ARR(36) = RRR(3,1)*2 
      END IF
C
C     UNPACK WATER TEMP, SHIP'S SPEED AND DIRECTION
C
      CALL UFBINT (LUBFR, RRR, 10, 255, NRET, CATAL)
      IF(NRET .NE. 0) THEN 
          ARR(37) = RRR(1,1) 
          ARR(38) = RRR(2,1) 
          ARR(39) = RRR(3,1) 
      END IF
C
C     PRINT THE DATA READ IN
C
      INUMP = 40
C     PRINT 102,HDR(1)
 102  FORMAT(/,' ',a8)
C     PRINT 101,(CATA(KK),ARR(KK),KK=1,INUMP)
 101  FORMAT(6(a5,f8.1))
C
      RETURN
  50  IRET = -1
      RETURN
C
      END
