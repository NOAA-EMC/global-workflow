C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
C                                                                       
C MAIN PROGRAM: BULLS_NAVYBULL
C   PRGMMR: VUONG            ORG: NP11        DATE: 2000-01-03
C                                                                       
C ABSTRACT: COMPUTES A TRANSMISSION FILE FROM SHIP REPORTS ???          
C   OWNER IS UNKNOWN, WHO THE OUTPUT GOES TO IS UNKNOWN.                
C                                                                       
C PROGRAM HISTORY LOG:
C   97-03-12  L.L. MORONE  CRAY VERSION
C   98-07-10  VUONG        REPLACED CALLS TO IW3GAD WITH CALLS TO Y2K
C                          COMPLIANT ROUTINE IW3UNP29 AND REMOVED CALLS
C                          TO W3LOG AND CONVERTED TO FORTRAN 90
C   99-12-20  VUONG        CONVERTED TO RUN ON IBM RS/6000 SP
C 2016-03-01  VUONG        REMOVED OPEN STATEMENT THAT CONFLICTS WITH IW3UNP29
C                                                                       
C USAGE:                                                                
C                                                                       
C   INPUT FILES:                                                        
C     FT11F001   - SFC SHIP FILE                                        
C     FT12F001   - PARM FIELD DENOTING WHAT RUN CYCLE   
C     FT15F001   - FILE CONTAINING DATES AND TIMES OF
C                  PREVIOUS RUNS                                        
C                                                                       
C   OUTPUT FILES:                                                       
C     FT06F001   - STANDARD FORTRAN OUTPUT PRINT FILE                   
C     FT15F001   - FILE CONTAINING DATES AND TIMES OF
C                  PREVIOUS RUNS                                        
C     FT51F001   - TRANSMISSION FILE                                    
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C     UNIQUE:    - BULHDR                                               
C                                                                       
C     LIBRARY:                                                          
C       W3LIB    - W3TAGB W3TAGE IW3UNP29 W3AI19 
C                                                                       
C   EXIT STATES:                                                        
C     COND =   0 - SUCCESSFUL RUN                                       
C          =  99 - ERROR IN PARM FIELD                                  
C                                                                       
C   REMARKS: JOB IS IJP'D IN WW1NV8W2 , WW1NV2P2 , WW1NV0D2 , WW1NV6K2  
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90
C                                                                       
C$$$                                                                    
      PROGRAM NAVYBULL
      CHARACTER*2560 NBLK
      DIMENSION NSHIP(1000),RSHIP(5280)
      CHARACTER*8 XID
      CHARACTER*8 RCPTID
      CHARACTER*1 PHR(2),P0,P1,P2,P6,P8
      CHARACTER*1 DIRLAT(2),DIRLON(2)
      CHARACTER*1 CCORS,CSPD
      CHARACTER*1 IETX,LF
      CHARACTER*4 IEOL
      CHARACTER*4 NAMA,NAMB,IRCPT2,IRCPT1
      CHARACTER*3 CLAT
      CHARACTER*4 CLON
      CHARACTER*2 CDAY,CHR
      CHARACTER*10 CDATE(4)

      DATA IEOL/'<<@ '/
      DATA DIRLAT/'N','S'/
      DATA DIRLON/'W','E'/
      DATA P0/'0'/,P1/'1'/,P2/'2'/,P6/'6'/,P8/'8'/
      DATA XID/'LASTRTIM'/
      DATA IETX/'%'/
      EQUIVALENCE (NSHIP(1),RSHIP(1),CDATE(1))
      EQUIVALENCE (RSHIP(11),NAMA)
      EQUIVALENCE (RSHIP(12),NAMB)
      EQUIVALENCE (RSHIP(5),IRCPT1)
      EQUIVALENCE (RSHIP(6),IRCPT2)
      CALL W3TAGB('BULLS_NAVYBULL',2000,0003,0069,'NP11')
      N = 0
      NXTBLK=0
      NSIZBK=1280
      N2560 = 2560
      LF = CHAR(10)
      NSTA = 0
      IUNIT = 51
      OPEN(IUNIT,ACCESS='DIRECT',RECL=1281)
      IREC=1
C                                                                       
C **************** READ POINTER FOR CORRECT HOUR ********************** 
C *** READ IN CONTENTS OF PARM FIELD THAT DENOTES RUN CYCLE *********** 
                                                                  
      READ(12,22,END=23) PHR
      WRITE(6,444) PHR
  444 FORMAT(' PHR',3X,2A1)
      IF (PHR(1) .EQ. P0 .AND. PHR(2) .EQ. P0) GO TO 7
      IF (PHR(1) .EQ. P0 .AND. PHR(2) .EQ. P6) GO TO 6
      IF (PHR(1) .EQ. P1 .AND. PHR(2) .EQ. P2) GO TO 12
      IF (PHR(1) .EQ. P1 .AND. PHR(2) .EQ. P8) GO TO 18
      CALL W3TAGE('BULLS_NAVYBULL') 
      CALL ERREXIT (99)
    7 READ (15,13,END=23) RCPTID,LTIME1,LDATE1,N1,N2,N3,N4,N5,N6
      NHOUR=1
      GO TO 25
    6 READ (15,13,END=23) RCPTID,N1,N2,LTIME1,LDATE1,N3,N4,N5,N6
      NHOUR=2
      GO TO 25
   12 READ (15,13,END=23) RCPTID,N1,N2,N3,N4,LTIME1,LDATE1,N5,N6
      NHOUR=3
      GO TO 25
   18 READ (15,13,END=23) RCPTID,N1,N2,N3,N4,N5,N6,LTIME1,LDATE1
      NHOUR=4
   13 FORMAT(A8,4(I4,I2))
   22 FORMAT(2A1)
C                       ***                                             
C                                                                       
C ******* IF POINTER IS MISSING OR INCORRECT, LIST ALL SHIPS *********  
C                                                                       
   25 IF(RCPTID .EQ. XID) GO TO 41
   23 LTIME1=8888
      LDATE1=88
      WRITE(6,31)
   31 FORMAT(' BAD TIME CHECK ID - LIST ALL SHIPS, INCLU. DUPLICATES')
      WRITE(6,88) NHOUR,RCPTID, XID
   88 FORMAT(' NHOUR, RCPTID, XID ',I2,2(2X,A8))
C                         ***                                           
C                                                                       
C *** INITIALIZE POINTER OUT (LAST RECEIPT TIME FROM THIS RUN) TO ***   
C ***       POINTER IN (LAST RECEIPT TIME FROM PREVIOUS RUN)      ***   
C                                                                       
   41 LTIME2=LTIME1
      LDATE2=LDATE1
      WRITE(6,560) LTIME1,LTIME2,LDATE1,LDATE2
  560 FORMAT(' LTIME1,LTIME2,LDATE1,LDATE2 ',4I6)
C                          ***                                          
      NUNIT = 11
C     OPEN (UNIT=NUNIT, FILE='SFCSHP', FORM='UNFORMATTED',
C    X ACCESS='SEQUENTIAL',STATUS='OLD', IOSTAT=IERR)
C     IF(IERR.NE.0) THEN
C       PRINT 290, NUNIT
C       GO TO 500
C     ENDIF
C 290 FORMAT(' I/O ERROR OPENING SFCSHP FILE ON UNIT',I3)

    5 IF(IW3UNP29(NUNIT,RSHIP,IER).EQ.0) GO TO 50
        GO TO (10,20,30,40), IER
   10   CONTINUE
C
C  READ IN NMC DATE WORD FROM HEADER
C
          READ(CDATE(2),301) IYR,IMO,IDAY,ITME
  301     FORMAT(I4,3I2.2)

          WRITE(6,333) IYR,IMO,IDAY,ITME
  333     FORMAT(' YEAR, MONTH, DAY, TIME OF SFCSHP FILE: ',I4,
     C           3(1X,I2.2))
C  READ IN DUMP TIME
          READ(CDATE(3),302) IRTM
  302     FORMAT(I4)

          ITME=ITME*100
C                          ***                                          
C ***** WRITE FIRST BULLETIN HEADER ***           
          CALL BULHDR (ITME,IDAY,NBLK,NXTBLK)
          GO TO 5
C                          ***                                          
   20   CONTINUE
C
C  LOGICAL END OF FILE REACHED
C
          IF(NSTA.GT.0) THEN
            ISTATS = 0
             GO TO 210
          ELSE
            PRINT 320
  320       FORMAT(' LOGICAL END OF FILE REACHED WITH NO USABLE DATA')
            ISTATS = 1
            GO TO 210
          ENDIF
   30   CONTINUE
C
C  PHYSICAL END OF FILE REACHED
C
          IF(NSTA.GT.0) THEN
            ISTATS = 0
            GO TO 210
          ELSE
            PRINT 330
  330       FORMAT(' PHYSICAL END OF FILE REACHED WITH NO USABLE DATA')
            ISTATS = 1
            GO TO 210
          ENDIF
   40   CONTINUE
C
C  I/O ERROR READING RECORD BLOCK
C
          PRINT 340
  340     FORMAT(' I/O ERROR READING RECORD BLOCK')
          ISTATS = 1
          GO TO 5
   50   CONTINUE

C                                                                       
C ********************** READ A REPORT *********************************
C                                                                       
C **************** SKIP DATA OTHER THAN SHIPS **************************
      IF (NSHIP(9) .LT. 521 .OR. NSHIP(9) .GT. 523) GO TO 5
      NSTA = NSTA + 1
C                                                                       
C **** BUILD INTEGER RECEIPT TIME FROM EBCDIC WORDS 5 AND 6 ************
      READ(IRCPT2,56) IRHR
      READ(IRCPT1,56) IHNDHR
   56 FORMAT(I2)
      IRTIME = IRHR*100+IHNDHR
C                         ***                                           
C                                                                       
C ************** CORRECT DATE FOR REPORTS FROM PREVIOUS DAY **********  
      IRDAY=IDAY
      ITIME = RSHIP(4)
C     WRITE(6,555) IRTIME, ITIME, IRTM, IRDAY,LDATE1,LTIME1
C 555 FORMAT(' IRTIME, ITIME, IRTM, IRDAY, LDATE1, LTIME1',6I5)
      IF (IRTIME .LE. IRTM .AND. ITIME .LE. IRTM) GO TO 145
      IRDAY=IRDAY-1
      IF (IRDAY .GT. 0 ) GO TO 145
      IRDAY=31
      IF (IMO .EQ. 5 .OR. IMO .EQ. 7 .OR. IMO .EQ. 10 .OR. IMO . EQ. 12)
     * IRDAY=30
      IF (IMO .EQ. 3) IRDAY=28
      IF (IMO .EQ. 3 .AND. MOD(IYR,4) .EQ. 0) IRDAY=29
C                  ***                                                  
C                                                                       
C *** CHECK DATE AND TIME AGAINST POINTER TO ELIMINATE DUPLICATES ***   
C                                                                       
  145 CONTINUE
C     WRITE(6,557) IRTIME, ITIME, IRTM, IRDAY,LDATE1,LTIME1
C 557 FORMAT(' AT 145,IRTIME, ITIME, IRTM, IRDAY, LDATE1, LTIME1',6I5)
      IF (IRDAY .LT. LDATE1 .AND. LDATE1-IRDAY .LT. 25) GO TO 5
      IF (IRDAY .GT. LDATE1 .OR. LDATE1-IRDAY .GE. 25)  GO TO 146
      IF(IRTIME .LE. LTIME1 .AND. LTIME1 .LE. 2400) GO TO 5
C                         ***                                           
C                                                                       
C ******** DETERMINE DIRECTION OF LAT AND LON **************************
  146 XLAT=RSHIP(1)/100.
      XLON=RSHIP(2)/100.
      IDLT=1
      IF (XLAT .GE. 0.00) GO TO 150
      IDLT=2
      XLAT=ABS(XLAT)
  150 IDLN=1
      IF(XLON .LE. 180.00) GO TO 160
      IDLN=2
      XLON=360.00-XLON
  160 ILAT=XLAT*10.
      ILON=XLON*10.
C ***** INITIALIZE COURSE AND SPEED IN CASE NONE ARE REPORTED *********
      ICORS = 11
      ISPD = 11
C     FIND LOCATION OF CAT 52 DATA
      ILOC = NSHIP(32)
      ICORS = NSHIP(ILOC+12)
      ISPD = NSHIP(ILOC+13)
C                                                                       
C *************************** BUILD BULLETIN ***************************
      CALL W3AI19(NAMA,4,NBLK,N2560,NXTBLK)
      CALL W3AI19(NAMB,3,NBLK,N2560,NXTBLK)
      WRITE(UNIT=CLAT(1:3),FMT='(I3.3)') ILAT
      WRITE(UNIT=CLON(1:4),FMT='(I4.4)') ILON
      CALL W3AI19(CLAT,3,NBLK,N2560,NXTBLK)
      CALL W3AI19(DIRLAT(IDLT),1,NBLK,N2560,NXTBLK)
      NXTBLK = NXTBLK + 1
      CALL W3AI19(CLON,4,NBLK,N2560,NXTBLK)
      CALL W3AI19(DIRLON(IDLN),1,NBLK,N2560,NXTBLK)
      IF (ICORS .GE. 10 .OR. ICORS .LT. 0) ICORS = 9
      WRITE(UNIT=CCORS(1:1),FMT='(I1)') ICORS
      NXTBLK = NXTBLK + 1
      CALL W3AI19(CCORS,1,NBLK,N2560,NXTBLK)
      IF (ISPD .GE. 10 .OR. ISPD .LT. 0) THEN
        NXTBLK = NXTBLK + 3
        GO TO 155
      ENDIF
      WRITE(UNIT=CSPD(1:1),FMT='(I1)') ISPD
      NXTBLK = NXTBLK + 1
      CALL W3AI19(CSPD,1,NBLK,N2560,NXTBLK)
      NXTBLK = NXTBLK + 1
  155 WRITE(UNIT=CDAY(1:2),FMT='(I2.2)') IRDAY
      CALL W3AI19(CDAY,2,NBLK,N2560,NXTBLK)
  162 ITIME = ITIME/100
      WRITE(UNIT=CHR(1:2),FMT='(I2.2)') ITIME
      CALL W3AI19(CHR,2,NBLK,N2560,NXTBLK)
      CALL W3AI19(IEOL,3,NBLK,N2560,NXTBLK)
      PRINT 199, NSTA, NXTBLK
  199 FORMAT( ' NSTA, NBLK ',2I6)
C                                 ***                                   
C ********* CHECK FOR END OF BLOCK OR END OF TRANSMISSION***************
      IF (NXTBLK .GE. 2530) THEN
        CALL W3AI19(IETX,1,NBLK,N2560,NXTBLK)
C
        WRITE(IUNIT,REC=IREC) NBLK(1:1280),LF  
        IREC=IREC+1
        IF(NXTBLK.GT.1280) THEN
          WRITE(IUNIT,REC=IREC) NBLK(1281:2560),LF
          IREC=IREC+1
          NXTBLK = 0
          CALL BULHDR(ITME,IDAY,NBLK,NXTBLK)
        ENDIF
      ENDIF
C                       ***                                             
C ***** IF DATE-TIME OF REPORT IS LATEST THUS FAR, UPDATE POINTER ***** 
  200 IF (((IRDAY .LT. LDATE2) .AND. ((LDATE2 - IRDAY) .LT. 15)) .OR.
     *((IRTIME .LE. LTIME2) .AND. (IRDAY .EQ. LDATE2)) .OR.
     *(IRTIME .GT. 2400) .OR. (IRDAY .GT. 31)) GO TO 5
      LTIME2=IRTIME
      LDATE2=IRDAY
      GO TO 5
C                       ***                                             
C ********** FINISHED; WRITE OUT NEW POINTER FOR CORRECT HOUR **********
C ********** BUT FIRST, WRITE OUT LAST GROUP OF OBS           **********
C ********** PRESUMABLY SHORTER THAN 2530 CHARACTERS          **********
  210 CONTINUE
      PRINT 211, NSTA, NXTBLK
  211 FORMAT( ' AFTER 210, NSTA, NBLK ',2I6)
      IF (NXTBLK .GT. 62) THEN
        CALL W3AI19(IETX,1,NBLK,N2560,NXTBLK)
C
        WRITE(IUNIT,REC=IREC) NBLK(1:1280),LF  
        IREC=IREC+1

        IF(NXTBLK.GT.1280) THEN
          WRITE(IUNIT,REC=IREC) NBLK(1281:2560),LF
          IREC=IREC+1
        ENDIF
      ENDIF
C
      REWIND 15
      GO TO (300,306,312,318),NHOUR
  300 WRITE(15,220) LTIME2,LDATE2,N1,N2,N3,N4,N5,N6
      GO TO 212
  306 WRITE(15,220) N1,N2,LTIME2,LDATE2,N3,N4,N5,N6
      GO TO 212
  312 WRITE(15,220) N1,N2,N3,N4,LTIME2,LDATE2,N5,N6
      GO TO 212
  318 WRITE(15,220) N1,N2,N3,N4,N5,N6,LTIME2,LDATE2
  212 L=N+1
C                  ***                                                  
C *************** FINISH OFF LAST TRANSMISSION ************************ 
  220 FORMAT('LASTRTIM',4(I4,I2))
      CALL W3TAGE('BULLS_NAVYBULL') 
  500 STOP
      END
