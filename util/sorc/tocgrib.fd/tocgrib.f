      PROGRAM tocgrib
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: tocgrib
C   PRGMMR: GILBERT          ORG: NP11        DATE: 2003-03-28
C
C ABSTRACT: Program reads selected GRIB fields from a file, adds a TOC
C           Flag Field separator block and WMO Header in front of each GRIB 
C           field, and writes them out to a new file.  The output file
C           is in the format required for TOC's FTP Input Service, which
C           can be used to disseminate the GRIB bulletins.
C           This service is described at http://weather.gov/tg/ftpingest.html.
C
C PROGRAM HISTORY LOG:
C 2003-03-28  Gilbert
C 2012-10-22  VUONG   CHANGED VARIABLE ENVVAR TO CHARACTER*6
C 2016-10-15  VUONG   Increased length of file name to 200 Characters
C
C USAGE:
C   INPUT FILES:
C      5       - LIST OF GRIB FIELDS AND ASSOCIATED WMO HEADERS.
C     11       - INPUT GRIB FILE.
C     31       - CORRESPONDING INPUT GRIB INDEX FILE.
C     PARM     - PASS IN 4 CHARACTERS 'KWBX' WITH PARM FIELD
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C      6       - STANDARD FORTRAN PRINT FILE
C     51       - OUTPUT GRIB BULLETIN FILE IN TOC FORMAT
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - MAKWMO
C     LIBRARY:
C       W3LIB    - W3TAGB W3UTCDAT IW3PDS W3FP11
C                  W3TAGE W3AS00 GETGBP
C       BACIO    - BAREAD BAOPENR BAOPENW BACLOSE
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C             10 - Error opening input GRIB data file 
C             20 - Error opening output GRIB transmission file 
C             19 - ERROR READING CONTROL CARD FILE - All Bulletins missing
C             30 - Some BULLETINS ARE MISSING
C
C REMARKS: None
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C
      PARAMETER (MXSIZ3=1000000)
C
      INTEGER         DUM
      INTEGER         JGDS(200)
      INTEGER         MPDS(200)
      INTEGER,dimension(8):: ITIME=(/0,0,0,-500,0,0,0,0/)
      INTEGER         KGDS(200)
      INTEGER         KPDS(200)
      INTEGER         MAPNUM
      INTEGER         NBITS
      INTEGER         NBUL
      INTEGER         NPARM
      INTEGER         PUNUM
      INTEGER,dimension(28) ::   HEXPDS
c     INTEGER    ::   HEXPDS(28)/28*0/
C
      CHARACTER * 6   BULHED
      CHARACTER * 100 CPARM
      CHARACTER * 20  DESC
      CHARACTER * 3   EOML
      CHARACTER * 1   GRIB(MXSIZ3)
      CHARACTER * 200 fileb,filei,fileo
      CHARACTER * 6   envvar
      CHARACTER * 4   KWBX
      CHARACTER * 1   PDS(28)
      CHARACTER * 1   PDSL(28)
      CHARACTER * 1   CSEP(80)
      CHARACTER * 132 TITLE
      integer,parameter :: lenhead=21
      CHARACTER * 1   WMOHDR(lenhead)
C
      LOGICAL         IW3PDS
C
      HEXPDS=0
c     CALL W3TAGB('tocgrib',2002,0916,0083,'NP11')                  
      LUGB=11
      LUGI=31
      LUGO=51
C
C     GET PARM FIELD WITH UP TO 100 CHARACTERS
C     Parm field should contain the originating center part of
C     the WMO Header.
C
      CPARM = '    '
      KWBX  = 'KWBC'
      CALL W3AS00(NPARM,CPARM,IER)
      IF (IER.EQ.0) THEN
        IF (NPARM.EQ.0.OR.CPARM(1:4).EQ.'    ') THEN
          PRINT *,'THERE IS A PARM FIELD BUT IT IS EMPTY'
          PRINT *,'OR BLANK, I WILL USE THE DEFAULT KWBC'
        ELSE
          KWBX(1:4) = CPARM(1:4)
        END IF
      ELSE IF (IER.EQ.2.OR.IER.EQ.3) THEN
        PRINT *,'W3AS00 ERROR = ',IER
        PRINT *,'THERE IS NO PARM FIELD, I USED DEFAULT KWBC'
      ELSE
        PRINT *,'W3AS00 ERROR = ',IER
      END IF
      PRINT *,'NPARM = ',NPARM
      PRINT *,'CPARM = ',CPARM(1:4)
      PRINT *,'KWBX  = ',KWBX(1:4)
C
C        Read GRIB data and index file names from the FORTnn
C        environment variables, and open the files.
C
      envvar='FORT  '
      write(envvar(5:6),fmt='(I2)') lugb
      call getenv(envvar,fileb)
      write(envvar(5:6),fmt='(I2)') lugi
      call getenv(envvar,filei)

      call baopenr(lugb,fileb,iret1)
      if ( iret1  .ne. 0 ) then
        write(6,fmt='(" Error opening GRIB file: ",A200)') fileb
        write(6,fmt='(" baopenr error = ",I5)') iret1
        stop 10
      endif
C
C         Open GRIB index file.  If doesn't open, use just the data
C         file.
C
      call baopenr(lugi,filei,iret2)
      if ( iret2  .ne. 0 ) then
        lugi=0
      endif

C
C        Read output GRIB bulletin file name from FORTnn
C        environment variable, and open file.
C
      write(envvar(5:6),fmt='(I2)') lugo
      call getenv(envvar,fileo)
      call baopenw(lugo,fileo,iret1)
      if ( iret1  .ne. 0 ) then
        write(6,fmt='(" Error opening GRIB file: ",A200)') fileo
        write(6,fmt='(" baopenw error = ",I5)') iret1
        stop 20
      endif

      IRET   = 0
      iopt=2
      insize=19
      NBUL   = 0
C
C        loop through input control records.
C
      nrec = 0
      foreachelement: do

        READ (*,66,iostat=ios) (HEXPDS(J),J=1,12),
     &        (HEXPDS(J),J=17,20), PUNUM, DESC
 66     FORMAT(3(2X,4Z2.2),3X,4Z2.2,6X,I3,1X,A20)
        if ( ios .ne. 0 ) then
          write(6,fmt='(" Error reading PDS from input file. iostat = "
     *          ,i5)') ios
          exit
        endif
        PDS=char(HEXPDS)
C
C        exit loop, if no more bulletins in input cards
C
        IF ( mova2i(pds(1)) .EQ. 255) exit
C
        nrec = nrec + 1
        WRITE(6,FMT='(/,''***********************************'',
     &      ''********************************************'')')
        print *,'Start new record no. = ',nrec
        WRITE (6,FMT='('' INPUT PDS, PUNUM'',
     &        '' & DESC...DESIRED GRIB MAPS LISTED ON FOLLOWING '',
     &        ''LINES...'',/,4X,3(2X,4z2.2),3X,4z2.2,6X,I3,1X,
     &        A20)') (HEXPDS(J),J=1,12),
     &        (HEXPDS(J),J=17,20), PUNUM, DESC

C
C        Read WNO Header associated with this element
C
        READ (*,iostat=ios,FMT='(4X,I3,2X,I2,2X,A6,1X,I3,24X,A3)')
     &        MAPNUM,NBITS, BULHED, DUM, EOML
        WRITE (6,FMT='(4X,I3,2X,I2,2X,A6,1X,I3,24X,A3)')
     &         MAPNUM,NBITS, BULHED, DUM, EOML
        if ( ios .ne. 0 ) then
          write(6,fmt='(" Error reading PDS from input file. iostat ="
     *          ,i6)') ios
        endif
C
C        Set up 25 word PDS array of GRIB field to read
C
        JREW    = 0
        JGDS    = -1
        MPDS    = -1
        MPDS(3) = mova2i(PDS(7))
        MPDS(5) = mova2i(PDS(9))
        MPDS(6) = mova2i(PDS(10))
        MPDS(7) = mova2i(PDS(11)) * 256 + mova2i(PDS(12))
        MPDS(14) = mova2i(PDS(19))
        MPDS(15) = mova2i(PDS(20))
C
C        Read and return packed GRIB field
C
        CALL GETGBP(LUGB,LUGI,MXSIZ3,JREW,MPDS,JGDS,
     &      itot,KREW,KPDS,KGDS,GRIB,IRET)
        IF (IRET.NE.0) THEN
          IF (IRET.LT.96) PRINT *,'GETGB-W3FI63: ERROR = ',IRET
          IF (IRET.EQ.96) PRINT *,'GETGB: ERROR READING INDEX FILE'
          IF (IRET.EQ.97) PRINT *,'GETGB: ERROR READING GRIB FILE'
          IF (IRET.EQ.98) THEN
            PRINT *,'GETGB ERROR: NUM. OF DATA POINTS GREATER THAN JF'
          END IF
          IF (IRET.EQ.99) PRINT *,'GETGB ERROR: REQUEST NOT FOUND'
          IF (IRET.GT.99) PRINT *,'GETGB ERROR = ',IRET
          cycle
        END IF
        PRINT *,'RECORD NO. OF GRIB RECORD IN INPUT FILE = ',KREW
C
C     COMPARE RECORD (GRIB) TO CONTROL CARD (PDS), THEY SHOULD MATCH
C
        PDSL(1:28)=GRIB(9:36)
        KEY = 2
        IF (.NOT.IW3PDS(PDSL,PDS,KEY)) THEN
          PRINT 2900, nrec,(mova2i(PDSL(j)),j=1,28),
     &                (mova2i(PDS(j)),j=1,28)
2900      FORMAT ( 1X,I4,' (PDS) IN RECORD DOES NOT MATCH (PDS) IN ',
     &    'CONTROL CARD ',/,7(1X,4Z2.2), /,7(1X,4Z2.2))
          cycle
        END IF
C
C        Print PDS
C
        PRINT 2, (mova2i(PDSL(J)),J=1,28)
 2      FORMAT (' PDS = ',7(4Z2.2,1X))
C
C        Construct and print Description of GRIB field
C
        CALL W3FP11 (GRIB,PDSL,TITLE,IER)
        IF (IER.NE.0) PRINT *,'W3FP11 ERROR = ',IER
        PRINT *,TITLE(1:86)
C
C
        PRINT *,' Size of GRIB Field = ',ITOT
C
C        MAKE Flag Field Separator block
C
          call mkfldsep(csep,iopt,insize,itot+lenhead,lenout)
C
C        MAKE WMO HEADER
C
C        Get system date and time
C         call w3utcdat(itime)
          CALL MAKWMO (BULHED,KPDS(10),KPDS(11),KWBX,WMOHDR)
C
C        write out Separator block, Abbreviated WMO Heading,
C        and GRIB field to output file.
C
          call wryte(lugo,lenout,csep)
          call wryte(lugo,lenhead,WMOHDR)
          call wryte(lugo,itot,grib)
          nbul=nbul+1
C
      enddo foreachelement
C
C*     CLOSING SECTION
C
      IF (NBUL .EQ. 0 ) THEN
        WRITE (6,FMT='('' SOMETHING WRONG WITH DATA CARDS...'',
     &         ''NOTHING WAS PROCESSED'')')
c       CALL W3TAGE('tocgrib')                                       
        stop 19 
      ELSE
        CALL BACLOSE (LUGB,iret)
        CALL BACLOSE (LUGI,iret)
        CALL BACLOSE (LUGO,iret)
        WRITE (6,FMT='(//,'' ******** RECAP OF THIS EXECUTION '',
     &    ''********'',/,5X,''READ  '',I6,'' INDIVIDUAL IDS'',
     &    /,5X,''WROTE '',I6,'' BULLETINS OUT FOR TRANSMISSION'',
     &    //)') nrec, NBUL
      ENDIF
C
C         TEST TO SEE IF ANY BULLETINS MISSING
C
      MBUL = nrec - NBUL
      IF (MBUL.NE.0) THEN
        PRINT *,'BULLETINS MISSING = ',MBUL
c       CALL W3TAGE('tocgrib')                                     
        stop 30
      END IF
C
c     CALL W3TAGE('tocgrib')                                       
      STOP
      END
