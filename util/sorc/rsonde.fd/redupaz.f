C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    REDUPAZ      READ UPPER AIR BUFR FILE                           
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-08-27                     
C                                                                               
C ABSTRACT: REDUPA READS THE PREPBUFR OBSERVATIONAL FILE AND                    
C   RETURNS A SINGLE STATION IN AN ARRAY.                                       
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   96-08-27  LARRY SAGER                                                       
c   97-01-17  George Vandenberghe.  Added mandatory Z level logic
c             
C                                                                               
C USAGE:    CALL REDUPA  (SRE, ERE, HDR, ARR, NRET1, IRET)                
C   INPUT ARGUMENT LIST:                                                        
C     SRE      - STARTING WMO BLOCK NUMBER TO DUMP                              
C     ERE      - ENDING WMO BLOCK NUMBER TO DUMP                                
C                                                                               
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)                          
C     HDR      - HEADER RECORD FOR STATION.  CONTAINS                           
C              - ID, LONG, LAT, HOUR, ELEVATION AND TYPE.                       
C     ARR      - DATA ARRAY FOR MANDATORY LEVEL DATA                            
C                HEIGHT, TEMPERATURE, DEW PT, WIND SPEED 
C                WIND DIRECTION AND Q-MARKS
C                                                                               
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)                     
C     FT55F001 - PREPBUFR UPPER AIR DATA FILE                                   
C                                                                               
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION                     
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: CRAY F90 FORTRAN                                                 
C   MACHINE:  CRAY4                                                             
C                                                                               
C$$$                                                                            
      SUBROUTINE REDUPAZ( SRE, ERE, HDR, ARR, NRET1, IRET)
C
C     REDUPA READS THE PREPBUFR FILE AND RETURNS AN UNPACKED      
C     BUFR REPORT.
C
      common/dat/IDATE
      COMMON/dateln/LENDAT
      CHARACTER*8   INOUT
      CHARACTER*8   SUBSET
      CHARACTER*8   CTEMP
      CHARACTER*8   SRE, ERE
      CHARACTER*40  CATA, CATB, STRING
      character*40  catc,catd,cate
C
      DIMENSION     HDR (10)
C
      REAL          ARR (10,255)
      REAL          BRR (6,255),BRR1(6,255),brr2(4,255)
      REAL          CRR (5,255),crr1(5,255)
      INTEGER ITEMP
      REAL RTEMP
C 
      EQUIVALENCE   (CTEMP,ITEMP)
      EQUIVALENCE   (RTEMP,ITEMP)
C
      DATA STRING  /'SID XOB YOB DHR ELV TYP'/
c      DATA CATA    /'CAT=1 POB ZOB ZQM TOB TQM TDO '/
c      DATA CATB    /'CAT=1 POB DDO FFO WQM'/
      DATA CATA    /'CAT=2 POB ZOB ZQM TOB TQM TDO '/
      DATA CATB    /'CAT=2 POB DDO FFO WQM'/
      DATA CATC    /'CAT=1 POB ZOB ZQM TOB TQM TDO '/
      DATA CATD    /'CAT=1 POB DDO FFO WQM'/
      DATA CATE    /'CAT=4 ZOB DDO FFO WQM '/
      DATA IX      /0/
c  MUST SAVE LOCAL VARIABLES ACCROSS 
c  SUBROUTINE CALLS  SEVERE BUG IN
c   ORIGINAL REDUPA REPAIRED  WITH FOLLOWING
c   SAVE STATEMENT (George VandenBerghe 1/15/97) 
c**************
      SAVE 
c***************
C
C----------------------------------------------------------------------|
C  1. OPEN THE FILE                                                    |
C----------------------------------------------------------------------| 
C     OPENBF:   Opens the file for reading.                            |
C     INPUT:    LUBFR:  Specifies the logical unit containing the file |
C                       to be read from.                               |
C               INOUT:  Opens the file for reading when INOUT is set   |
C                       to IN.                                         |
C               LUNDX:  Specifies the logical unit containing the BUFR |
C                       tables.  If the tables are contained           | 
C                       in the BUFR messages, LUNDX = LUBFR.           |
C     OUTPUT:   NONE                                                   |
C----------------------------------------------------------------------|
C     Let us upen a file of BUFR messages containing PIBAL, RADIOSONDE,|
C     and DROPSONDE data for reading.  Since the BUFR TEMPLATE for     |
C     this type of data is not contained within the BUFR message itself|
C     itself, the BUFR message is on logical unit 10 while the BUFR    |
C     TEMPLATE for that data is on logical unit 20.                    |
C----------------------------------------------------------------------|
C
      IRET = 0
      IMASS = 0
      IWIND = 0
      ITEMP=LUBFR
      LUBFR = 11
      IF( IX .EQ. 0) THEN
        IX = 1
        INMG = 0
        INOUT = 'IN'
        LUNDX = 11 
        CALL  OPENBF( LUBFR, INOUT, LUNDX )
      ENDIF
C
C----------------------------------------------------------------------|
C
C     READ THE BUFR DATA TANKS
C
C
C----------------------------------------------------------------------|
C    3. ADVANCE THE POINTER TO THE NEXT BUFR MESSAGE IN THE FILE       |
C       AND READ THE BUFR MESSAGE INTO AN INTERNAL BUFFER              |
C----------------------------------------------------------------------|
C       READMG:   Advances the input message pointer to the next       |
C                 BUFR message in the file and reads the message,      |
C                 without change, into an internal buffer.             |
C       INPUT:    LUBFR:  Defined under OPENBF.                        |
C       OUTPUT:   SUBSET: Contains the table A mnemonic associated     |
C                         with the type of data in the BUFR message.   |
C                 IDATE:  Contains the year, month, day, and hour      |
C                         from Section 1 of the BUFR message.          |
C                 IRET1:  If IRET = 0, the read has been successful    |
C                         If IRET = -1, and end-of-file has been read. |
C----------------------------------------------------------------------|
C       Call READMG to advance the pointer to the next BUFR message    |
C       and read it.                                                   |
C----------------------------------------------------------------------|
C
 10   IF ( INMG .EQ. 0) THEN   
        LENDAT = 10
        CALL  READMG( LUBFR, SUBSET, IDATE, IRET1)
        IF (IRET1 .NE. 0) GO TO 50
        IF (SUBSET .NE. 'ADPUPA' ) GOTO 10
        INMG = 1
      END IF
C
C
C----------------------------------------------------------------------|
C      4. READ AND UNPACK THE BUFR MESSAGE                             |
C----------------------------------------------------------------------|
C         READSB:   Reads and unpacks the BUFR message placed in       |
C                   the internal buffer by READMG into an indexed      |
C                   and expanded internal buffer.                      |
C         INPUT:    LUBFR:  Defined under OPENBF                       |
C         OUTPUT:   IRET2:  Set to 0 (zero) when operation was         |
C                           successful and data is available for       |
C                           user access.  Set to -1 when all           |
C                           subsets have been processed, and           |
C                           another call to READMG is required to      |
C                           read the next BUFR message.                |
C----------------------------------------------------------------------|
C         INITIALIZE IRET2 TO 0 AND READ THIS UNPACKED BUFR MESSAGE    |
C         UNTIL SUBSETS HAVE BEEN READ.                                |
C----------------------------------------------------------------------|
C
 20     CALL READSB (LUBFR,IRET2)
        IF (IRET2 .LT. 0) THEN    
           INMG = 0
           GO TO 10
        END IF
C
C----------------------------------------------------------------------|
C        5. ACCESS RELEVANT PARTS OF THE UNPACKED BUFR MESSAGE         |
C----------------------------------------------------------------------|
C           UFBINT: Transfers data values either (a) from the internal |
C                   buffer produced by READSB to the user, or (b) from |
C                   the user to an internal buffer for subsequent      |
C                   writing out by WRITSB.  When using UFBINT for      |
C                   purpose (a), the following definitions apply:      |
C           INPUT:  LUBFR:  Defined under OPENBF.                      |
C                   I1:     The maximum first dimension of ARR.  This  |
C                           dimension must be large enough to hold     |
C                           the maximum number of parameters to be     |
C                           accessed and placed into array ARR.        |
C                   I2:     The maximum second dimension of ARR.  This |
C                           dimension must be large enough to hold     |
C                           the maximum number of repetitions of the   |
C                           set of parameters to be accessed and       |
C                           placed into array ARR.                     |
C                   STR:    String of mnemonics indicating which       |
C                           parameters to be placed into array ARR.    |
C                           Note that there cannot be more than I1     |
C                           mnemonics in STR.                          |
C           OUTPUT: ARR:    A two-dimensional array containing real    |
C                   NRET:   The number of sets of requested parameters |
C                           - those indicated by the mnemonics in      |
C                           STR - that were retrieved by UFBINT and    |
C                           placed into array ARR.                     |
C----------------------------------------------------------------------|
C       5c. THIRD, ACCESS THE WMO BLOCK AND STATION NUMBER FOR THIS   |
C           OBSERVATION.  Note that the block number (mnemonic =      |
C           WMOB) is returned in ARR(1,1) and the station number      |
C           (mnemonic = WMOS) in ARR(2,1).  They are combined into    |
C           one 5-digit number for printing.  This case is like the   |
C           in 5a.                                                    |
C---------------------------------------------------------------------|
C
      
      CALL UFBINT (LUBFR, HDR, 10, 1, NRET, STRING)
C
C     PROCESS ONLY THOSE REPORTS REQUESTED
C
      RTEMP = HDR (1)
      call byteswap(ITEMP, 8, 1)
      ITEMP = ISHFT(ITEMP,-48)
      call byteswap(ITEMP, 8, 1)
c      print * ,ctemp,sre
c1      IF((CTEMP .LT. SRE) .OR. (CTEMP .GT. ERE)) GO TO 20
c       print *,' a ',ctemp(1:1),' b ',ctemp(2:2),' c ',ctemp(3:3),
c     1 ' d ',ctemp(4:4),' e ',ctemp(5:5),' f ',ctemp(6:6),' g ',
c     1 ctemp(7:7),' h ',ctemp(8:8)
      if(ctemp(7:7) .ne. '7') go to 20
c      print  *,' no 20 ',ctemp,sre
c        print *,' HEADER(5) ',hdr(5)
cc         write(57,195) hdr(1) 
 195    format(' HEADERR  ',a9)
C---------------------------------------------------------------------|
C       5e. FIFTH, ACCESS THE PRESSURE (mnemonic - PRLC),             |
C           GEOPOTNETIAL (mnemonic = GP10                             |
C           TEMPERATATURE (mnemonic - TMDB), DEW POINT "(mnemonic =   |
C           TMDP), WIND DIRECTION (mnemonic = WDIR), WIND SPEED       |
C           (mnemonic = WSPD), and VERTICAL SOUNDING SIGNIFICANCE     |
C           (mnemonic - VSIG) for all pressure levels in this part.   |
C           Note that if the pressure is available, the geopotential  |
C           if given, will be in GP10.  If the pressure is not        |
C           available, the geopotential , if given, will be in GP07.  |
C           This example is a combination of examples 5a. and 5b.     |
C           The result is that the full set of parameters - for the   |
C           first pressure level - will be in ARR(1,1) - ARR(8,1),    |
C           respectively, the second set - for the second pressure    |
C           level - in ARR(1,2) - ARR(8,2), respectively, and so on   |
C           until the NRETth set is in ARR(1,NRET) - ARR(8,NRET),     |
C           respectively.                                             |
C---------------------------------------------------------------------|
C
         nret1=0
c         nretz=0
         nret1a=0
      IF (HDR(6) .EQ. 120.) THEN
c          CALL UFBINT (LUBFR, BRR, 6, 255, NRET1, CATA)
c          CALL UFBINT (LUBFR, BRR1, 6, 255, NRET1A, CATC)
          IMASS = 1
      END IF
      IF (HDR(6) .eq. 220.) THEN 
c          CALL UFBINT (LUBFR, CRR, 5, 255, NRET2, CATB)
c          CALL UFBINT (LUBFR, CRR1, 5, 255, NRET2, CATD)
          CALL UFBINT (LUBFR, BRR2, 4, 255, NRETZ, CATE)
c          if(NRETZ .ne. 0) then 
c          print *,' NONZERO GEOMETRIC RETURN ' 
c          do k=1,nretz
c          print *,' Z DATA ',brr2(1,k),brr2(2,k),brr2(3,k)
c     1  ,brr2(4,k)
c          end do
c          else
C           print *,' ZERO GEOMETRIC RETURN'
c          endif
          IWIND = 1
      END IF 
      IF ((IMASS .EQ. 0) .OR. (IWIND .EQ. 0)) GO TO 20
C
C     STORE THE PIECES OF THE REPORT INTO AN ARRAY 
C
c      DATA CATC    /'CAT=1 POB ZOB ZQM TOB TQM TDO '/
c      DATA CATD    /'CAT=1 POB DDO FFO WQM'/
c      DATA CATE    /'CAT=4 ZOB DDO FFO WQM '/
      DO K=1,NRETZ
          arr(1,k)=-7
         arr(2,k)=brr2(1,k)
         arr(5,k)=brr2(2,k)
         arr(6,k)=brr2(3,k)
         arr(9,k)=brr2(4,k)
         arr(10,k)=hdr(5)
      END DO 
      DO K=NRETZ+1,NRETZ+NRET1A
       km=(k-nretz)
	 ARR (1,K) = BRR1 (1,Km)
         ARR (2,K) = BRR1 (2,Km)
         ARR (3,K) = BRR1 (4,Km)
         ARR (4,K) = BRR1 (6,Km)
         ARR (5,K) = CRR1 (2,Km)
         ARR (6,K) = CRR1 (3,Km)
         ARR (7,K) = BRR1 (3,Km)
         ARR (8,K) = BRR1 (5,Km)
         ARR (9,K) = CRR1 (4,Km)
c  arr9 is wqm, arr8 is tqm and arr7 is zqm
         arr(10,k)= hdr(5)
      END DO 
      RETURN
C
  50  IRET = -1
      RETURN
C
      END
