       program RSONDPLT
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: RSONDPLT
C   PRGMMR: CARUSO           ORG: NP12        DATE: 2000-03-13
C
C ABSTRACT:      Generate formatted radiosonde                  
c                report.   Used downstream by WNDANLFV and 
c                also easily human readable for data debugging
c                purposes.
C
C PROGRAM HISTORY LOG:
C   97-01-17  VandenBerghe  first written
c   98-04-07  Caruso        modified to dump tropopause level obs. if
c                           indicated and renamed program to 
c                           rsondplt
c   98-05-05  caruso        consolidated this program with s/r's
c                           redupap and redupaz.  added a common
c                           block (/dateln/lendat) to both s/r's
c                           to tell bufr s/r readmg.f to return
c                           a 10 digit date (includes a 4 digit
c                           year).
c   99-06-07  caruso        convert to ibm.
c
C USAGE:
C   INPUT FILES:
c     fort.10   - bufr "prepbufr" file containing radiosonde
c     fort.5    - single word of text specifying "P", "T", or "Z"
c       (stdin)   directs program to extract pressure, tropopause
c                 pressure, or geopotential height
c                 from fort.10.  BUFRLIB bug prevents 
c                 concurrent ext of both (grumble grumble!!) 
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)     
c     fort.51   - formatted sounding report 
C
C   SUBPROGRAMS CALLED: 
C     UNIQUE:    - redupap redupaz
C     LIBRARY:   - UFBINT OPENBF READMG READSB
C       COMMON   - none
c       w3lib    - w3tagb w3tag3
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =NNNN - system library return codes .. no user
c                - return codes
C
C REMARKS:                                                  
C
C ATTRIBUTES:
c   language:  fortran 90.
C   MACHINE:  ibm
C$$$
      common/dateln/ lendat
      integer       lendat

      Character*8 sre,ere
      character*1 cflag
      common/stuff/pad(1000),hdr,pa2(1000),arr
      common/dat/idate
      dimension hdr(10) 
      character*80 chead
      equivalence (chead,hdr)
      dimension arr(10,255)
      integer(8) iexit

      save

      CALL W3TAGB('RSONDPLT',2000,0073,0077,'NP12')                   

      iexit = 0
      read(5,901) cflag
 901  format(a1)
      print*,' cflag = ',cflag
      sre = '      70'
      ere = '      78'
      inum = 1
      do k = 1,300000
        do l = 1,10
          do m = 1,255
            arr(l,m)=0
          enddo
        enddo
        do l = 1,10
          hdr(l) = 0
        enddo

        if(cflag .eq. 'P' .or. cflag .eq. 'T' )
     *      call redupap(cflag,sre,ere,hdr,arr,is,is2)

        if(cflag .eq. 'Z') call redupaz(sre,ere,hdr,arr,is,is2)

        if(cflag .ne. 'Z'.and.cflag .ne. 'P'.and.
     *      cflag .ne. 'T') then
          print *,' ERROR IN SPECIFYING PRESSURE/Z CHOICE'
          iexit = 240
          call  errexit (iexit) 
        endif
        if(is2 .ne. 0)  stop 'redupa '
c
c  save hdr(4) (dhr) into integer
c
        ihdr4 = nint(hdr(4))

        do l = 1,39
          do j = 1,10
            if(arr(j,l) .gt. 999999 .or. arr(j,l) .lt. -999999)
     1        arr(j,l) = -999999
          enddo
        enddo
        do 12 kl = 1,39
c
c  check wqm.  if bad, set wind dir and spd to miss.
c
          if(arr(9,kl) .gt. 2.0)  then   !wqm is bad
             arr(5,kl) = -999999
             arr(6,kl) = -999999
          endif
c
c  check all array elements. if all are 0, don't write out
c  this report.
c
          if(arr(1,kl).eq.0.0.and.arr(2,kl).eq.0.0.and.
     *       arr(3,kl).eq.0.0.and.arr(4,kl).eq.0.0.and.
     *       arr(5,kl).eq.0.0.and.arr(6,kl).eq.0.0.and.
     *       arr(7,kl).eq.0.0.and.arr(8,kl).eq.0.0.and.
     *       arr(9,kl).eq.0.0.and.arr(10,kl).eq.0.0) go to 12
c
c  check wind dir and spd.  if both are 0, set them to miss.
c
          if(arr(5,kl).eq.0.0.and.arr(6,kl).eq.0.0) then
             arr(5,kl) = -999999
             arr(6,kl) = -999999
          endif
c
c  check tqm.  if bad, set air temp and dew pt to miss.
c  idate is date read in from bufr message (has a 4 digit year).
c
          if(arr(8,kl) .gt. 2.0) arr(4,kl) = -999999  !tqm is bad  
          if(arr(8,kl) .gt. 2.0) arr(3,kl) = -999999    
          if(arr(1,kl) .le. 1200. .and. arr(1,kl) .ge. 0.) 
     1      Write(51,102)chead(1:8),hdr(2),hdr(3),(arr(n,kl),n=1,10),
     1         idate,ihdr4
 102        format(a8,1x,f6.2,1x,f6.2,1x,10(f10.2,1x),i10,i4,' SNDP')
          if(arr(1,kl) .gt. 1200. .or.  arr(1,kl) .lt. 0.) 
     1      Write(51,192)chead(1:8),hdr(2),hdr(3),(arr(n,kl),n=1,10),
     1         idate,ihdr4
 192        format(a8,1x,f6.2,1x,f6.2,1x,10(f10.2,1x),i10,i4,' SNDZ')
 12     continue
      enddo
      CALL W3TAGE('RSONDPLT') 
      call errexit(iexit)
      CALL W3TAGE('RSONDPLT') 
      stop
      end
c=====================================================================
      SUBROUTINE REDUPAP( CFLAG, SRE, ERE, HDR, ARR, NRET1, IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    REDUPAP      READ UPPER AIR BUFR FILE                           
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-08-27                     
C                                                                               
C ABSTRACT: REDUPAP READS THE PREPBUFR OBSERVATIONAL FILE AND                    
C   RETURNS A SINGLE STATION IN AN ARRAY.                                       
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   96-08-27  LARRY SAGER                                                       
c   97-01-17  George Vandenberghe.  Added mandatory Z level logic
C   98-03-26  CHRIS CARUSO - CHANGED FROM WIND DIR/WIND SPD TO
C             U AND V WINDS
C   98-03-30  CHRIS CARUSO - CHANGED BACK TO WIND DIR/SPD.
c   98-04-07  chris caruso - added cflag to input argument list
c             to determine whether we're getting regular
c             pressure obs or tropopause pressure obs.
c   98-05-05  chris caruso - added common/dateln/lendat to set 
c             length of idate returned by readmg.  if lendat=8, 
c             2 digit year is returned.  if lendat=10, 4 digit 
c             year is returned.  consolidated this s/r with
c             redupaz.f and rsondplt.f into one file.
C                                                                               
C USAGE:    CALL REDUPAP(CFLAG, SRE, ERE, HDR, ARR, NRET1, IRET)                
C   INPUT ARGUMENT LIST:
C     CFLAG    - CHAR*1 FLAG INDICATING WHETHER TO GET REGULAR
C                PRESSURE OBS OR TROPOPAUSE PRESS. OBS.                                                        
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
C     Fort.55  - PREPBUFR UPPER AIR DATA FILE                                   
C                                                                               
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION                     
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  ibm                                                             
C                                                                               
C$$$                                                                            
C
C     REDUPA READS THE PREPBUFR FILE AND RETURNS AN UNPACKED      
C     BUFR REPORT.
C
      common/dat/ IDATE
      common/dateln/ lendat
      integer       lendat

      CHARACTER*8   INOUT
      CHARACTER*8   SUBSET
      CHARACTER*8   CTEMP
      CHARACTER*8   SRE, ERE
      CHARACTER*1   CFLAG
      CHARACTER*40  STRING
      character*40  CATA, CATB, catc, catd
C
      DIMENSION     HDR (10)
C
      REAL          ARR (10,255)
      REAL          BRR (6,255),BRR1(6,255),brr2(4,255)
      REAL          CRR (5,255),crr1(5,255)
C 
      EQUIVALENCE   (CTEMP,ITEMP)
      EQUIVALENCE   (RTEMP,ITEMP)
C
      DATA STRING  /'SID XOB YOB DHR ELV TYP'/
      DATA CATA    /'CAT=5 POB ZOB ZQM TOB TQM TDO '/
      DATA CATB    /'CAT=5 POB DDO FFO WQM'/
      DATA CATC    /'CAT=1 POB ZOB ZQM TOB TQM TDO '/
      DATA CATD    /'CAT=1 POB DDO FFO WQM'/
      DATA IX      /0/
c
c  MUST SAVE LOCAL VARIABLES ACCROSS 
c  SUBROUTINE CALLS.  SEVERE BUG IN
c  ORIGINAL REDUPA REPAIRED  WITH FOLLOWING
c  SAVE STATEMENT (George VandenBerghe 1/15/97) 
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
      IRET  = 0
      IMASS = 0
      IWIND = 0
      ITEMP = LUBFR
      LUBFR = 10
      IF( IX .EQ. 0) THEN
        IX = 1
        INMG = 0
        INOUT = 'IN'
        LUNDX = 10 
        CALL  OPENBF( LUBFR, INOUT, LUNDX )
      ENDIF
C----------------------------------------------------------------------|
C
C     READ THE BUFR DATA TANKS
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
C                 IRET1:  If IRET1 = 0, the read has been successful   |
C                         If IRET1 = -1, and end-of-file has been read.|
C----------------------------------------------------------------------|
C       Call READMG to advance the pointer to the next BUFR message    |
C       and read it.                                                   |
c
c       set lendat = 10 here, since openbf calls s/r bfrini, which
c       sets lendat = 8.
C----------------------------------------------------------------------|
 10   IF( INMG .EQ. 0) THEN   
        lendat = 10
        CALL  READMG( LUBFR, SUBSET, IDATE, IRET1)
        IF (IRET1 .NE. 0) GO TO 50
        IF (SUBSET .NE. 'ADPUPA' ) GO TO 10
        INMG = 1
      ENDIF
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
 20     CALL READSB (LUBFR,IRET2)
        IF(IRET2 .LT. 0) THEN    
           INMG = 0
           GO TO 10
        ENDIF
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
      
      CALL UFBINT (LUBFR, HDR, 10, 1, NRET, STRING)
C
C     PROCESS ONLY THOSE REPORTS REQUESTED
C
      RTEMP = HDR (1)
      call byteswap(ITEMP, 8, 1)
      ITEMP = ISHFT(ITEMP,-48)
      call byteswap(ITEMP, 8, 1)
      if(ctemp(7:7) .ne. '7') go to 20
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
      nret2  = 0
      nret1  = 0
      nretz  = 0
c
c hdr(6) is type
c
      IF(HDR(6) .EQ. 120.) THEN
        IF(CFLAG.EQ.'P') THEN
          CALL UFBINT (LUBFR, BRR1, 6, 255, NRET1A, CATC)
        ELSE                    !TROPOPAUSE
          CALL UFBINT (LUBFR, BRR1, 6, 255, NRET1A, CATA)
c          print*,' nret1a = ',nret1a
        ENDIF
        IMASS = 1
      ENDIF
      IF(HDR(6) .eq. 220.) THEN 
        IF(CFLAG.EQ.'P') THEN
          CALL UFBINT (LUBFR, CRR1, 5, 255, NRET2, CATD)
        ELSE                    !TROPOPAUSE
          CALL UFBINT (LUBFR, CRR1, 5, 255, NRET2, CATB)
c          print*,' nret2 = ',nret2
        ENDIF
        IWIND = 1
      ENDIF 
c      print*,' imass = ',imass,' iwind = ',iwind
      IF((IMASS .EQ. 0) .OR. (IWIND .EQ. 0)) GO TO 20
C
C     STORE THE PIECES OF THE REPORT INTO AN ARRAY 
C
      DO K = NRETZ + 1, NRETZ + NRET1A
         km = (k - nretz)
c         print*,' km = ',km
c         print 968,crr1(1,km),brr1(1,km)
c 968     format(' crr1 = ',f10.2,' brr1 = ',f10.2)
c        search for pressure level in CRR1 which matches
c        brr1(1,km).   Assumption that CRR1(1,km)=BRR1(1,km)
c        is not valid!!
         kms = 0
         do 87 ks = nretz + 1, nretz + nret1a
           kmd = (ks - nretz)
           if(crr1(1,kmd) .eq. brr1(1,km)) then
              kms = kmd
              go to 89
           endif
           kms = 0
 87      continue
 89      continue
c
c  arr1 is pressure, arr2 is height, arr3 is temp.,
c  arr4 is dew point, arr7 is zqm and arr8 is tqm.
c
         ARR (1,K) = BRR1 (1,Km)
         ARR (2,K) = BRR1 (2,Km)
         ARR (3,K) = BRR1 (4,Km)
         ARR (4,K) = BRR1 (6,Km)
         ARR (7,K) = BRR1 (3,Km)
         ARR (8,K) = BRR1 (5,Km)
C
c        assign winds
C
         if(kms .gt. 0) then
           ARR (5,K) = CRR1 (2,Kms)
           ARR (6,K) = CRR1 (3,Kms)
           ARR (9,K) = CRR1 (4,Kms)
         endif

         arr(10,k) = hdr(5)
C
c  arr5 is wind dir, arr6 is wind spd,
c  arr9 is wqm, arr10 is elevation.
c
c         print 988,arr(1,k),arr(2,k),arr(3,k),arr(4,k),arr(5,k)
c 988     format(' arr1 = ',f10.2,' arr2 = ',f10.2,
c     *          ' arr3 = ',f10.2,' arr4 = ',f10.2,' arr5 = ',f10.2)
c         print 987,arr(6,k),arr(7,k),arr(8,k),arr(9,k),arr(10,k)
c 987     format(' arr6 = ',f10.2,' arr7 = ',f10.2,
c     *          ' arr8 = ',f10.2,' arr9 = ',f10.2,' arr10 = ',f10.2)
      ENDDO

      RETURN
C
  50  IRET = -1
      RETURN
      END
c=====================================================================
      SUBROUTINE REDUPAZ( SRE, ERE, HDR, ARR, NRET1, IRET)
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
c   98-05-05  chris caruso - added common/dateln/lendat to set
c             length of idate returned by readmg.  if lendat=8,
c             2 digit year is returned.  if lendat=10, 4 digit
c             year is returned.  consolidated this s/r with
c             redupap.f and rsondplt.f into one file.
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
C     Fort.55  - PREPBUFR UPPER AIR DATA FILE                                   
C                                                                               
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION                     
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: FORTRAN 90                                                 
C   MACHINE:  ibm                                                     
C                                                                               
C$$$                                                                            
C
C     REDUPA READS THE PREPBUFR FILE AND RETURNS AN UNPACKED      
C     BUFR REPORT.
C
      common/dat/ IDATE
      common/dateln/ lendat
      integer       lendat

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
C 
      EQUIVALENCE   (CTEMP,ITEMP)
      EQUIVALENCE   (RTEMP,ITEMP)
C
      DATA STRING  /'SID XOB YOB DHR ELV TYP'/
      DATA CATE    /'CAT=4 ZOB DDO FFO WQM '/
      DATA IX      /0/
c
c  MUST SAVE LOCAL VARIABLES ACCROSS 
c  SUBROUTINE CALLS.  SEVERE BUG IN
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
      IRET  = 0
      IMASS = 0
      IWIND = 0
      ITEMP = LUBFR
      LUBFR = 10
      IF( IX .EQ. 0) THEN
        IX = 1
        INMG = 0
        INOUT = 'IN'
        LUNDX = 10 
        CALL  OPENBF( LUBFR, INOUT, LUNDX )
      ENDIF
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
c
c       set lendat = 10 here, since openbf calls s/r bfrini, which
c       sets lendat = 8.
C----------------------------------------------------------------------|
 10   IF( INMG .EQ. 0) THEN   
        lendat = 10
        CALL  READMG( LUBFR, SUBSET, IDATE, IRET1)
        IF (IRET1 .NE. 0) GO TO 50
        IF (SUBSET .NE. 'ADPUPA' ) GO TO 10
        INMG = 1
      ENDIF
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
 20     CALL READSB (LUBFR,IRET2)
        IF(IRET2 .LT. 0) THEN    
           INMG = 0
           GO TO 10
        ENDIF
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
      CALL UFBINT (LUBFR, HDR, 10, 1, NRET, STRING)
C
C     PROCESS ONLY THOSE REPORTS REQUESTED
C
      RTEMP = HDR (1)
      call byteswap(ITEMP, 8, 1)
      ITEMP = ISHFT(ITEMP,-48)
      call byteswap(ITEMP, 8, 1)
      if(ctemp(7:7) .ne. '7') go to 20
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
      nret1 = 0
      nret1a = 0
      IF(HDR(6) .EQ. 120.) THEN
          IMASS = 1
      ENDIF
      IF(HDR(6) .eq. 220.) THEN 
          CALL UFBINT (LUBFR, BRR2, 4, 255, NRETZ, CATE)
          IWIND = 1
      ENDIF 
      IF((IMASS .EQ. 0) .OR. (IWIND .EQ. 0)) GO TO 20
C
C     STORE THE PIECES OF THE REPORT INTO AN ARRAY 
C
c      print*,' nretz = ',nretz
      DO K = 1,NRETZ
         arr(1,k)  = -7
         arr(2,k)  = brr2(1,k)
         arr(5,k)  = brr2(2,k)
         arr(6,k)  = brr2(3,k)
         arr(9,k)  = brr2(4,k)
         arr(10,k) = hdr(5)
      ENDDO 
      RETURN
C
  50  IRET = -1
      RETURN
      END
