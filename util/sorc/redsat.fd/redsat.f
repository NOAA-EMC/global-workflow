      program REDSATW  
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: REDSAT       EXTRACTS LOW LEVEL SAT WINDS FROM BUFR 
C   PRGMMR: KRISHNA KUMAR        ORG: W/NP12      DATE: 1998-12-28
C   
C ABSTRACT:  read avn prepbufr file, extract low level sat.
C              winds and write latitude, longitude, wind, and
C              pressure level to formatted output file for
C              downstream use.
C
C PROGRAM HISTORY LOG:
C       97-06-01   George VandenBerghe.  New program
C       98-07-23   Krishna Kumar converted to f90 with Y2K
C                  compliance
C     1998-12-28   Krishna Kumar modified to run on IBM-SP 6000 
C     2013-03-12   Krishna Kumar ported this code to WCOSS 
C     2017-10-16   Boi Vuong removed wind missing data from 
C                  upper air PREPBUFR file. 
C
C USAGE:
C   INPUT FILES:
C      fort.11   - GFS prepbufr input.
C   
C   OUTPUT FILES:
C      fort.78    - output formatted satellite winds 
C
C   SUBPROGRAMS  CALLED
C     UNIQUE      - RSAT
C     LIBRARY     - OPENBF READMG READSB  UFBINT (bufr library)
C     W3LIB       -W3LOG
C    
C   EXIT STATES:  
C     COND =   0 - SUCCESSFUL RUN
C          =NNNN - system errors only
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C   BUFR library requirement makes this Cray specific.
C   This program was written to contain this cray specific
C   code in a separate small module.  The formatted output
C   is easy to ingest on other machines. It would be logically
C   simpler to run this as a subroutine within a code that needed
C   sat winds but that would require that TRPSFCMV 
C   run on  a machine with robust bufr libraries, TRPSFCMV
C   is now more portable.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM,
C
C$$$
       character*11 envvar           ! for ibm_sp_6000  
       character*80 filei,fileo      ! for ibm_sp_6000
c
       dimension ary(6) ,rlims(4) 
       data rlims/50.,-50.,-360.,360/
c
       CALL W3TAGB('REDSAT  ',1998,0204,0081,'NP12   ') 

c
c****** added for ibm_sp_6000
c
       in_unit=11
       envvar='FORT   '
       write(envvar(9:10),fmt='(I2)') in_unit
       call getenv(envvar,filei)
       call baopen(in_unit,filei,iret)
c
       io_unit=78
       envvar='FORT   '
       write(envvar(9:10),fmt='(I2)') io_unit
       call getenv(envvar,fileo)
       call baopen(io_unit,fileo,iret)
c
c******
c
       do 10,k=1,9999999
       call rsat(rlims,in_unit,ary,iret)
       if(iret .ne. 0) go to 99
c
c  Removed wind missing data from upper air PREPBUFR file 
c  to prevent bad wind speed data in output satwinds file.
       if (ary(3).gt.850. .and. abs(ary(5)).lt.500.0) 
     &     write (io_unit,101) ary
 101   format('SATWND',6f10.4) 
 10    continue
 99    continue
c
       CALL W3TAGE('REDSAT  ') 
c
       stop
       end
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .  
C SUBPROGRAM:    REDSAT      READ UPPER AIR BUFR FILE                   
C   PRGMMR: KRISHNA KUMAR    ORG: W/NP12   DATE: 1998-12-28             
C                                                                       
C ABSTRACT: REDSAT READS THE PREPBUFR OBSERVATIONAL FILE AND            
C   RETURNS A SINGLE SATWND REPORT IN ARRAY DATA                        
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   97-06-03  LARRY SAGER                                               
C 1998-12-28  KRISHNA KUMAR  CONVERTED THIS CODE FROM CRAY TO
C                            IBM RS/6000 SP SYSTEM.
C 2013-03-12  KRISHNA KUMAR  PORTED THIS CODE FROM IBM RS/6000 
C                            TO WCOSS SYSTEM
C
C USAGE:    CALL REDSAT  (RLIMS, IUNS, DATA, IRET)
C   INPUT ARGUMENT LIST:                                                
C     RLIMS    - LAT/LONG LIMITS TO DUMP
C     IUNS     - UNIT NUMBER OF PREPBUFR FILE
C                                                                       
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)                  
C     DATA     - THE RETURNED SATWND REPORT.                            
C              - LAT, LONG, PRESALT, WDIR, WSP, WQM                     
C     IRET     - FLAG:   IRET=0  NORMAL RETURN
C                        IRET=-1 END OF FILE    
C                                                                       
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)             
C     FT55F001 - PREPBUFR UPPER AIR DATA FILE                           
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                       
C   MACHINE:  IBM                                                      
C                                                                       
C$$$                                                                    
      SUBROUTINE RSAT(RLIMS, IUNS, DATA, IRET)
C
C     REDUPA READS THE PREPBUFR FILE AND RETURNS AN UNPACKED      
C     SATWND REPORT.
C
      CHARACTER*8   INOUT
      CHARACTER*8   SUBSET
      CHARACTER*8   CTEMP
      CHARACTER*40  CATH, STRING        
C
      DIMENSION     HDR (10)
C
      REAL          CRR (5,255)
      REAL          RLIMS (4)
      REAL          DATA(6)
C 
      EQUIVALENCE   (CTEMP,ITEMP)
      EQUIVALENCE   (RTEMP,ITEMP)
C
      DATA STRING  /'SID XOB YOB DHR ELV TYP SQN'/
      DATA CATH    /'CAT=6 POB DDO FFO WQM'/
      DATA IX      /0/
C
C----------------------------------------------------------------------|
C   OPEN THE CRAY BUFR TANK FILE                                       |
C----------------------------------------------------------------------|
C
      SAVE 
      IRET = 0
      IMASS = 0
      IWIND = 0
      LUBFR = IUNS
      IF( IX .EQ. 0) THEN
        IX = 1
        INOUT = 'IN'
ckumar
        LUNDX = IUNS 
ckumar
        CALL  OPENBF( LUBFR, INOUT, LUNDX )
 10     CALL READMG(LUBFR, SUBSET, IDATE, IRET1)
        IF(IRET1 .NE. 0) GOTO 50
        ITYP = 0
        IF(SUBSET .NE. 'SATWND' ) GOTO 10                    
      ENDIF
C
C----------------------------------------------------------------------|
C     READ THE BUFR DATA TANKS
C----------------------------------------------------------------------|
C
 20   CALL READSB (LUBFR,IRET2)
      IF (IRET2 .LT. 0) THEN    
 22        CALL READMG(LUBFR, SUBSET, IDATE, IRET1)
           IF (IRET1 .LT. 0) GOTO 50
           IF(SUBSET .EQ. 'SATWND' ) GOTO 20
           GOTO 22
      END IF
C
C----------------------------------------------------------------------|
C     READ IN THE STATION INFORMATION                                  |
C----------------------------------------------------------------------|
C
      CALL UFBINT (LUBFR, HDR, 10, 1, NRET, STRING)
C     PRINT 119,HDR(1),HDR(6)
 119  FORMAT(' STATION ',A8,' subtype ',F10.1)
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
      IF((HDR(6) .GE. 240.) .AND. (HDR(6) .LE. 246.))THEN 
          CALL UFBINT (LUBFR, CRR, 5, 255, NRET2, CATH)
C         PRINT *,'SATWNDS ',CRR(2,1),CRR(3,1)
          DATA(1) = HDR(3)    
          DATA(2) = HDR(2)    
          DATA(3) = CRR(1,1)  
          DATA(4) = CRR(2,1)
          DATA(5) = CRR(3,1)
          DATA(6) = CRR(4,1)
C         PRINT *,' DATA ',(DATA(KK),KK=1,6)            
      ELSE
          GOTO 20
      END IF
      RETURN
C
  50  IRET = -1

      RETURN
      END
