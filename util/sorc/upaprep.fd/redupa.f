C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    REDUPA      READ UPPER AIR BUFR FILE                           
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-08-27                     
C                                                                               
C ABSTRACT: REDUPA READS THE PREPBUFR OBSERVATIONAL FILE AND                    
C   RETURNS A SINGLE STATION IN AN ARRAY.                                       
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   96-08-27  LARRY SAGER                                                       
C                                                                               
C USAGE:    CALL REDUPA  (SRE, ERE, RLIMS,ISWR,HDR, ARR,
C                              ITYP, NRET1, IRET)                
C   INPUT ARGUMENT LIST:                                                        
C     SRE      - STARTING WMO BLOCK NUMBER TO DUMP                              
C     ERE      - ENDING WMO BLOCK NUMBER TO DUMP                                
C     RLIMS    - LAT/LONG LIMITS TO DUMP
C     ISWR     - BLOCK/ LATLONG THINING SWITCH
C                                                                               
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)                          
C     HDR      - HEADER RECORD FOR STATION.  CONTAINS                           
C              - ID, LONG, LAT, HOUR, ELEVATION AND TYPE.                       
C     ARR      - DATA ARRAY FOR MANDATORY LEVEL DATA                            
C                HEIGHT, TEMPERATURE, DEW PT, WIND SPEED 
C                WIND DIRECTION AND Q-MARKS
C     ITYP     - TYPE OF DATA   ITYP=1  ADPUPA
C                               ITYP=2  AIRCFT AIRCAR
C                                                                               
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)                     
C     FT55F001 - PREPBUFR UPPER AIR DATA FILE                                   
C                                                                               
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION                     
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: CRAY CFT77 FORTRAN                                                 
C   MACHINE:  CRAY4                                                             
C                                                                               
C$$$                                                                            
      SUBROUTINE REDUPA( SRE, ERE, RLIMS, ISWR, HDR, ARR,
     1           ITYP, NRET1, ITIR, IFLG , IRET)
C
C     REDUPA READS THE PREPBUFR FILE AND RETURNS AN UNPACKED      
C     BUFR REPORT.
C
      CHARACTER*8   INOUT
      CHARACTER*8   SUBSET
      CHARACTER*8   CTEMP
      CHARACTER*8   SRE, ERE
      CHARACTER*40  CATA, CATB, STRING, CTEM
      CHARACTER*40  CATC, CATD, CATG, CATH 
C
      DIMENSION     HDR (10)
      DIMENSION     RHDR (10)
      DIMENSION     HDA (10)
C
      REAL          ARR (10,255)
      REAL          BRR (6,255)
      REAL          CRR (5,255)
      REAL          DRR (6,255)
      REAL          TRR (4,255,20)
      REAL          RLIMS (4)
C 
      EQUIVALENCE   (CTEMP,ITEMP)
      EQUIVALENCE   (RTEMP,ITEMP)
      EQUIVALENCE   (C651,R651)
C
      DATA STRING  /'SID XOB YOB DHR ELV TYP SQN'/
      DATA CTEM    /'CAT=1 TOB TQM TPC POB '/
      DATA CATA    /'CAT=1 POB ZOB ZQM TOB TQM TDO '/
      DATA CATB    /'CAT=1 POB DDO FFO WQM '/
      DATA CATC    /'CAT=5 POB DDO FFO WQM '/
      DATA CATD    /'CAT=5 POB ZOB ZQM TOB TQM TDO '/
      DATA CATG    /'CAT=6 POB ZOB ZQM TOB TQM TDO '/
      DATA CATH    /'CAT=6 POB DDO FFO WQM'/
      DATA IX      /0/
      DATA RMISS   /9999999./
      DATA KSTA    /0/
      DATA IK      /0/
      DATA C651    /'74651   '/
C
C----------------------------------------------------------------------|
C   OPEN THE CRAY BUFR TANK FILE                                       |
C----------------------------------------------------------------------| 
C
      SAVE 
      IRET = 0
      IMASS = 0
      IWIND = 0
      LUBFR = 10
      IF( IX .EQ. 0) THEN
        IX = 1
        INOUT = 'IN'
        LUNDX = 10 
C       PRINT *,'about to open bufr file'
        CALL  OPENBF( LUBFR, INOUT, LUNDX )
 10     CALL READMG(LUBFR, SUBSET, IDATE, IRET1)
C          PRINT *,' SUBSET',SUBSET,' RET ',IRET1
        IF(IRET1 .NE. 0) GOTO 50
        ITYP = 0
        IF(SUBSET .EQ. 'ADPUPA' ) ITYP = 1                   
        IF(SUBSET .EQ. 'AIRCFT' ) ITYP = 2                   
        IF(SUBSET .EQ. 'AIRCAR' ) ITYP = 2                   
        IF(SUBSET .EQ. 'SATWND' ) ITYP = 3                   
        IF(ITYP .EQ. 0) GOTO 10
      ENDIF
      IF(IK .EQ. 1) THEN   
         IK = 0
         GOTO 28
      END IF
C
C----------------------------------------------------------------------|
C     READ THE BUFR DATA TANKS
C----------------------------------------------------------------------|
C
 20   CALL READSB (LUBFR,IRET2)
      IF (IRET2 .LT. 0) THEN    
 22        CALL READMG(LUBFR, SUBSET, IDATE, IRET1)
C          PRINT *,' SUBSET',SUBSET,' RET ',IRET1
           IF (IRET1 .LT. 0) GOTO 50
           ITYP = 0
           IF(SUBSET .EQ. 'ADPUPA' ) THEN
              ITYP = 1                   
              GOTO 20
           END IF
           IF(SUBSET .EQ. 'AIRCFT' ) THEN
              ITYP = 2                   
              GOTO 20  
           END IF
           IF(SUBSET .EQ. 'AIRCAR' ) THEN
              ITYP = 2                   
              GOTO 20  
           END IF
           IF(SUBSET .EQ. 'SATWND' ) THEN
              ITYP = 3                   
              GOTO 20  
           END IF
           IF(ITYP .EQ. 0) GOTO 22
      END IF
C
C----------------------------------------------------------------------|
C     READ IN THE STATION INFORMATION                                  |
C----------------------------------------------------------------------|
C
  28  LUBFA = LUBFR
      CALL UFBINT (LUBFR, HDR, 10, 1, NRET, STRING)
C     IF(ITYP .EQ. 3)  PRINT 119,ITYP,HDR(1),HDR(6)
 119  FORMAT(' FOUND TYPE ',i3,' STATION ',A8,' subtype ',F10.1)
C
C----------------------------------------------------------------------|
C     PROCESS AN UPPER AIR RADIOSONDE REPORT. START BY
C     MAKING SURE THIS REPORT IS THE CORRECT TYPE AND THAT
C       IT'S WITHIN THE AREA TO PROCESS.  IF NOT, GO READ
C       THE NEXT REPORT
C----------------------------------------------------------------------|
C
      IF (ITYP .EQ. 1) THEN
      IF ((HDR(6) .NE. 120.) .AND. (HDR(6) .NE. 220.) .AND.   
     1    (HDR(6) .NE. 132.) .AND. (HDR(6) .NE. 232.)) GOTO 20
      IF((HDR(6) .EQ. 132.) .OR. (HDR(6) .EQ. 232.)) IFLG = 1
      IF(ISWR .EQ. 0) THEN
         RTEMP = HDR (1)
         ITEMP = ISHFT(ITEMP,-48)
         IF((CTEMP .LT. SRE) .OR. (CTEMP .GT. ERE)) GO TO 20
      END IF
C
C     MAKE SURE THIS STATION IS WITHIN THE LAT/LONG LIMITS
C
      IF(ISWR .EQ. 1) THEN
        IF((HDR(3) .GT. RLIMS(1)) .OR. (HDR(3) .LT. RLIMS(2)))
     1    GOTO 20
        IF((HDR(2) .LT. RLIMS(3)) .OR. (HDR(2) .GT. RLIMS(4)))
     1    GOTO 20 
      END IF
C     PRINT 100,HDR(1),HDR(2),HDR(3),HDR(4),HDR(6),HDR(7)
      IF(HDR(1) .EQ. R651) GOTO 20
      IK = 0
      IF((IMASS .EQ. 1) .AND. (IWIND .EQ. 0)) THEN  
        IF(RHDR(1) .NE. HDR(1)) THEN
C       PRINT 109,HDR(1),RHDR(1),HDR(2),HDR(3),HDR(4),HDR(6),HDR(7)
 109  FORMAT(' MISM ',a8,' OLD ',a8, ' LL ',2f10.1,' HR ',f6.0,
     1     ' typ ',f6.0,' SEQ ',f10.0)
        IK = 1
        DO K = 1,10
          HDR(K) = RHDR(K)
        END DO
        GO TO 40
        END IF
      END IF
      DO K = 1,10
         RHDR(K) = HDR(K)
      END DO 
      KSTA = KSTA + 1
C
C----------------------------------------------------------------------|
C     UNPACK THE BUFR REPORT
C----------------------------------------------------------------------|
C
      IF (HDR(6) .LT. 200.) THEN
          CALL UFBINT (LUBFR, BRR, 6, 255, NRET1, CATA)
          IMSQ = NINT(HDR(7))
          IMASS = 1
C
C         GET THE OBSERVED TEMPERATURES BY FLIPPING BACK
C           THROUGH THE EVENTS
C
          CALL UFBEVN(LUBFR, TRR, 4, 255, 20, IRAT, CTEM)  
          DO 7 K = 1,IRAT
              DO M = 1,20
                  IF(NINT(TRR(3,K,M)) .EQ. 1) THEN
                     BRR(4,K) = TRR(1,K,M)
                     GOTO 7
                  END IF
   	      END DO
 7        CONTINUE
C
C         SAVE THE TROPOPAUSE IF IT OCCURS AT A MANDATORY 
C            LEVEL
C
          CALL UFBINT (LUBFR, DRR, 6, 255, NRET3, CATD)
          ITROP = 0
          IF(NRET3 .GT. 0) THEN
C            DO K = 1,NRET3
C               PRINT *,' TROP ',(DRR(KK,K),KK=1,6)
C            END DO
             IREM = NINT(DRR(1,1))
             ICOM = IREM/10       
             ICOM = ICOM*10
             IF(IREM .EQ. ICOM) THEN
                ITROP = 1
                NRET1 = NRET1 + 1
                BRR(1,NRET1) = DRR(1,1)
                BRR(2,NRET1) = DRR(2,1)
                BRR(3,NRET1) = DRR(3,1)
                BRR(4,NRET1) = DRR(4,1)
                BRR(5,NRET1) = DRR(5,1)
                BRR(6,NRET1) = DRR(6,1)
C
C               RETRIEVE THE TROP WINDS
C
                CALL UFBINT (LUBFR, DRR, 6, 255, NRET3, CATC)
             END IF
          ENDIF 
C            DO K = 1,NRET1
C               PRINT *,' PRES  ',(BRR(KK,K),KK=1,6)
C            END DO
          GOTO 20
      END IF
      IF (HDR(6) .GT. 200.) THEN 
          CALL UFBINT (LUBFR, CRR, 5, 255, NRET2, CATB)
          IWSQ = NINT(HDR(7))
C
C         RETRIEVE THE TROP WINDS
C
          IF(ITROP .EQ. 1) THEN
             CALL UFBINT (LUBFR, DRR, 6, 255, NRET3, CATC)
             NRET2 = NRET2 + 1
             CRR(1,NRET2) = DRR(1,1)
             CRR(2,NRET2) = DRR(2,1)
             CRR(3,NRET2) = DRR(3,1)
             CRR(4,NRET2) = DRR(4,1)
          END IF
C         DO K = 1,NRET2
C         PRINT *,' WINDS ',(CRR(KK,K),KK=1,4)
C         END DO
          IWIND = 1
      END IF 
      IF ((IMASS .EQ. 0) .OR. (IWIND .EQ. 0)) GO TO 20
C
C     STORE THE PIECES OF THE REPORT INTO AN ARRAY 
C
 40   DO K=1,NRET1
	 ARR (1,K) = BRR (1,K)
         ARR (2,K) = BRR (2,K)
         ARR (3,K) = BRR (4,K)
         ARR (4,K) = BRR (6,K)
         ARR (7,K) = BRR (3,K)
         ARR (8,K) = BRR (5,K)
         IF(IMSQ .NE. IWSQ) THEN
C          PRINT *,' SEQ #s DO NOT MATCH DROP WINDS'
           GOTO 32
         END IF
         DO J = 1,NRET2 
           IF(BRR(1,K) .EQ. CRR(1,J)) THEN
              ARR (5,K) = CRR (2,J)
              ARR (6,K) = CRR (3,J)
              ARR (9,K) = CRR (4,J)
              GOTO 32
           END IF
         END DO
 32      IF (ARR(7,K) .LT. 0.0) ARR(7,K) = 99.
         IF (ARR(8,K) .LT. 0.0) ARR(8,K) = 99.
         IF (ARR(9,K) .LT. 0.0) ARR(9,K) = 99.
         IF (ARR(7,K) .GT. 1000.) ARR(7,K) = 99.
          IF (ARR(8,K) .GT. 1000.) ARR(8,K) = 99.
         IF (ARR(9,K) .GT. 1000.) ARR(9,K) = 99.
C        PRINT *,' PRES ',ARR(1,K),' TMP ',ARR(3,K)
      END DO 
      IMASS = 0
      IWIND = 0
C   
C     SET THE WORK AREA TO MISSING AND RETURN
C
      DO K = 1,NRET1 
         BRR(1,K) = RMISS
         BRR(2,K) = RMISS
         BRR(4,K) = RMISS
         BRR(6,K) = RMISS
         CRR(2,K) = RMISS
         CRR(3,K) = RMISS
         BRR(3,K) = RMISS
         BRR(5,K) = RMISS
         CRR(4,K) = RMISS
      END DO
      RETURN
      END IF
      IF(ITYP .EQ. 2) THEN     
C
C        UNPACK AND STORE THE AIRCRAFT DATA
C
C----------------------------------------------------------------------|
C     GET THE RELEVANT PARTS OF THE UNPACKED BUFR MESSAGE        |
C----------------------------------------------------------------------|
      IF(HDR(4) .GT. 3.0) GOTO 20
      IF(HDR(4) .LT. -3.0) GOTO 20
      IFLAG = 0
C     PRINT 100,HDR(1),HDR(2),HDR(3),HDR(4),HDR(6)
      IF((HDR(3) .GT. RLIMS(1)) .OR. (HDR(3) .LT. RLIMS(2)))
     1    GOTO 20
      IF((HDR(2) .LT. RLIMS(3)) .OR. (HDR(2) .GT. RLIMS(4)))
     1    GOTO 20 
C     PRINT 100,HDR(1),HDR(2),HDR(3),HDR(4),HDR(6)
 100  FORMAT(a8,' LL ',2f10.1,' HR ',f6.0,' typ ',f6.0,' SEQ ',f10.0)
C
C     READ FROM THE BUFR TANKS
C
C     HEIGHTS 
C
      ARR(6,1) = HDR(5)
      IF ((HDR(6) .EQ. 130.) .OR. (HDR(6) .EQ. 131.) .OR.
     1           (HDR(6) .EQ. 133.)) THEN
          CALL UFBINT (LUBFR, BRR, 6, 255, NRET1, CATG)
          IMASS = 1
          ARR (1,1) = BRR(1,1)
          ARR (2,1) = BRR(4,1)
          ARR (3,1) = BRR(6,1)
          ARR (6,1) = BRR(2,1)
          ARR (7,1) = 0.
          IF(BRR(3,1) .LT. 99.) ARR(7,1) = BRR(3,1)*10000.
          IF(BRR(5,1) .LT. 99.)
     1          ARR(7,1) = ARR(7,1) + BRR(5,1)*100.
          GOTO 20
      END IF
C
C     WINDS
C
      IF ((HDR(6) .EQ. 230.) .OR. (HDR(6) .EQ. 231.) .OR.
     1         (HDR(6) .EQ. 233.)) THEN
         CALL UFBINT (LUBFR, CRR, 5, 255, NRET2, CATH)
         ARR (1,1) = CRR (1,1)
         ARR (4,1) = CRR (2,1)
         ARR (5,1) = CRR (3,1)
         IF(CRR(4,1) .LT. 99.)      
     1         ARR(7,1) = ARR(7,1) + CRR(4,1)
         IWIND = 1
      END IF
      IF((IWIND.EQ.0) .AND. (IMASS .EQ. 0)) GOTO 20
      END IF
C
C     READ THE SATWND DATA  
C
      IF (ITYP .EQ. 3) THEN
C
C     MAKE SURE THIS STATION IS WITHIN THE LAT/LONG LIMITS
C
      IF((HDR(3) .GT. RLIMS(1)) .OR. (HDR(3) .LT. RLIMS(2)))
     1    GOTO 20
      IF((HDR(2) .LT. RLIMS(3)) .OR. (HDR(2) .GT. RLIMS(4)))
     1    GOTO 20 
C
C----------------------------------------------------------------------|
C     UNPACK THE BUFR REPORT
C----------------------------------------------------------------------|
C
      ARR(6,1) = HDR(5)
      IF((HDR(6) .EQ. 144.) .OR. (HDR(6) .EQ. 140.))THEN
          CALL UFBINT (LUBFR, BRR, 6, 255, NRET1, CATG)
          IMASS = 1
C         PRINT 298,BRR(1,1),BRR(2,1) 
 298      FORMAT(' SAT MASS ',2f8.0)
          ARR (1,1) = BRR(1,1)
          ARR (2,1) = BRR(4,1)
          ARR (3,1) = BRR(6,1)
          ARR (6,1) = BRR(2,1)
          ARR (7,1) = 0.
          IF(BRR(3,1) .LT. 99.) ARR(7,1) = BRR(3,1)*10000.
          IF(BRR(5,1) .LT. 99.)
     1          ARR(7,1) = ARR(7,1) + BRR(5,1)*100.
          GOTO 20
      END IF
      IF((HDR(6) .eq. 245.) .OR. (HDR(6) .EQ. 253.) .OR.
     1    (HDR(6). eq. 243.) .or. (HDR(6) .EQ. 246.)) THEN 
          CALL UFBINT (LUBFR, CRR, 5, 255, NRET2, CATH)
C         PRINT 299,CRR(2,1),CRR(3,1)
 299      FORMAT(' SAT WINDS ',2f8.0)
         ARR (1,1) = CRR (1,1)
         ARR (4,1) = CRR (2,1)
         ARR (5,1) = CRR (3,1)
         IF(CRR(4,1) .LT. 99.)      
     1         ARR(7,1) = ARR(7,1) + CRR(4,1)
         IWIND = 1
      END IF
      IF(IWIND.EQ.0) GOTO 20
      END IF
C
C     PROCESS THE TIROS DATA
C
C     IF(ITYP .EQ. 4) THEN
C
C     MAKE SURE THIS STATION IS WITHIN THE LAT/LONG LIMITS
C
C     IF((HDR(3) .GT. RLIMS(1)) .OR. (HDR(3) .LT. RLIMS(2)))
C    1    GOTO 20
C     IF((HDR(2) .LT. RLIMS(3)) .OR. (HDR(2) .GT. RLIMS(4)))
C    1    GOTO 20 
C
C----------------------------------------------------------------------|
C     UNPACK THE BUFR REPORT
C----------------------------------------------------------------------|
C
C     IF((HDR(6) .EQ. 171.) .OR. (HDR(6) .EQ. 172.) .OR.
C    1   (HDR(6) .EQ. 173.)) THEN
C         CALL UFBINT (LUBFR, BRR, 6, 255, NRET1, CATA)
C         DO K=1,NRET1
C 	     ARR (1,K) = BRR (1,K)
C            ARR (2,K) = BRR (2,K)
C            ARR (3,K) = BRR (4,K)
C            ARR (4,K) = BRR (6,K)
C            ARR (7,K) = BRR (3,K)
C            ARR (8,K) = BRR (5,K)
C            IF (ARR(7,K) .LT. 0.0) ARR(7,K) = 99.
C            IF (ARR(8,K) .LT. 0.0) ARR(8,K) = 99.
C            IF (ARR(7,K) .GT. 1000.) ARR(7,K) = 99.
C            IF (ARR(8,K) .GT. 1000.) ARR(8,K) = 99.
C            ARR(9,K) = 99.
C         END DO 
C     END IF
C     END IF
      RETURN
C
  50  IRET = -1

      RETURN
      END
